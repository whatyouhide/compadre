defmodule Compadre.CombinatorsTest do
  use ExUnit.Case, async: true

  doctest Compadre.Combinators

  import Compadre.TestHelper
  import Compadre
  import Compadre.Combinators

  alias Compadre.Parsers, as: Ps

  ## Core combinators ##

  test "bind/2: the first parser is successful" do
    parser = bind(Ps.advance(1), fn nil -> Ps.advance(1) end)
    assert_parse_result parser, "bar", {:ok, nil, "r"}
  end

  test "bind/2: the first parser is not successful" do
    parser = bind(Ps.flunk("error"), fn _ -> Ps.fixed(:success) end)
    assert_parse_result parser, "bar", {:error, "error", "bar"}
  end

  test "bind/2: raises if the function doesn't return a parser" do
    parser = bind(Ps.fixed(:ok), fn :ok -> :not_a_parser end)

    msg = "the second argument passed to bind/2 must be a function that" <>
          " returns a %Compadre.Parser{}, got: :not_a_parser"
    assert_raise ArgumentError, msg, fn ->
      parse_test(parser, "foo")
    end
  end

  test "plus/2" do
    parser = plus(Ps.Binary.binary("bar"), Ps.Binary.binary("baz"))

    assert_parse_result parser, "bar rest", {:ok, "bar", " rest"}
    assert_parse_result parser, "baz rest", {:ok, "baz", " rest"}
  end

  test "look_ahead/1" do
    parser = look_ahead(Ps.Binary.binary("foo"))
    assert_parse_result parser, "foobar", {:ok, "foo", "foobar"}
  end

  test "with_consumed_input/1" do
    parser = with_consumed_input(Ps.advance(3))
    assert_parse_result parser, "foorest", {:ok, {nil, "foo"}, "rest"}
  end

  test "followed_by/2: both parsers succeed" do
    parser = followed_by(Ps.Binary.take_bytes(1), Ps.Binary.take_bytes(2))
    assert_parse_result parser, "foorest", {:ok, "f", "rest"}
  end

  test "followed_by/2: one of the parsers fails" do
    # First one
    parser = followed_by(Ps.Binary.binary("foo"), Ps.Binary.binary("bar"))
    assert_parse_result parser, "fobar", {:error, _, "fobar"}

    # Second one
    parser = followed_by(Ps.Binary.binary("foo"), Ps.flunk(:oops))
    assert_parse_result parser, "foobar", {:error, :oops, "foobar"}
  end

  test "label/2" do
    parser = label(Ps.Binary.binary("foo"), "foo parser")

    # Let's avoid assert_parse_result here as we need to match on the message.
    assert {:error, msg, "bar"} = parse(parser, "bar")
    assert msg =~ "parser 'foo parser' failed: "
  end

  ## Combinators that are built upon the "core" combinators ##

  test "seq/2: the first parser succeeds" do
    parser = seq(Ps.Binary.take_bytes(1), Ps.Binary.take_bytes(2))
    assert_parse_result parser, "foorest", {:ok, "oo", "rest"}
  end

  test "seq/2: one of the parsers fails" do
    # First one
    parser = seq(Ps.flunk(:oops), Ps.advance(3))
    assert_parse_result parser, "baz", {:error, :oops, "baz"}

    # Second one
    parser = seq(Ps.Binary.take_bytes(3), Ps.flunk(:oops))
    assert_parse_result parser, "baz", {:error, :oops, ""}
  end

  test "transform/2" do
    parser = transform(Ps.fixed("foo"), &(&1 <> "bar"))
    assert_parse_result parser, "rest", {:ok, "foobar", "rest"}

    parser = transform(Ps.flunk("error"), &(&1 <> "bar"))
    assert_parse_result parser, "rest", {:error, "error", "rest"}
  end

  test "defaulting_to/2" do
    parser = defaulting_to(Ps.Binary.binary("foo"), :default)
    assert_parse_result parser, "foo rest", {:ok, "foo", " rest"}
    assert_parse_result parser, "bar", {:ok, :default, "bar"}
  end

  test "one_of/1" do
    parser = one_of(Enum.map(~w(foo bar baz), &Ps.Binary.binary/1))
    assert_parse_result parser, "foo rest", {:ok, "foo", " rest"}
    assert_parse_result parser, "bar rest", {:ok, "bar", " rest"}
    assert_parse_result parser, "baz rest", {:ok, "baz", " rest"}
  end

  test "many_until/2" do
    parser = many_until(Ps.Binary.take_byte(), Ps.Binary.binary("-stop"))
    assert_parse_result parser, "abc-stop-rest", {:ok, 'abc', "-rest"}

    parser = many_until(Ps.Binary.binary("foo"), Ps.Binary.binary("-stop"))
    assert_parse_result parser, "bar-stop", {:ok, [], "bar-stop"}
  end

  test "many/1" do
    parser = many(Ps.Binary.binary("foo"))

    assert_parse_result parser, "foofoobar", {:ok, ["foo", "foo"], "bar"}
    assert_parse_result parser, "bar", {:ok, [], "bar"}
  end

  test "one_or_more/1" do
    parser = one_or_more(Ps.Binary.binary("foo"))

    assert_parse_result parser, "foofoobar", {:ok, ["foo", "foo"], "bar"}
    assert_parse_result parser, "bar", {:error, _, "bar"}
  end

  test "sequence/1: all the sequence successfully goes through" do
    parser = sequence([Ps.Binary.binary("foo"), Ps.Binary.binary("bar")])
    assert_parse_result parser, "foobar rest", {:ok, ["foo", "bar"], " rest"}
  end

  test "sequence/1: the sequence is stopped on the first failing parser" do
    parser = sequence([Ps.Binary.binary("foo"), Ps.Binary.binary("bar"), Ps.Binary.binary("baz")])
    assert_parse_result parser, "foobaz", {:error, _, _}
  end

  test "count/1" do
    parser = count(Ps.Binary.binary("foo"), 3)
    assert_parse_result parser, "foofoofoo rest", {:ok, ["foo", "foo", "foo"], " rest"}
  end

  test "sep_by_at_least_one/2" do
    parser = sep_by_at_least_one(Ps.Binary.take_bytes(3), Ps.Binary.binary(", "))

    assert_parse_result parser, "foo, bar, baz-rest", {:ok, ~w(foo bar baz), "-rest"}
    assert_parse_result parser, {"fo", :eoi}, {:error, _, "fo"}

    parser = sep_by_at_least_one(Ps.Text.integer(), Ps.Binary.binary(", "))
    assert_parse_result parser, {"1", :eoi}, {:ok, [1], ""}
  end

  test "sep_by/2" do
    parser = sep_by(Ps.Binary.take_bytes(3), Ps.Binary.binary(", "))

    assert_parse_result parser, "foo, bar, baz-rest", {:ok, ~w(foo bar baz), "-rest"}
    assert_parse_result parser, {"fo", :eoi}, {:ok, [], "fo"}
  end
end
