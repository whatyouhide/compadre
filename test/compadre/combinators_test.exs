defmodule Compadre.CombinatorsTest do
  use ExUnit.Case, async: true

  import Compadre.TestHelper
  import Compadre
  import Compadre.Combinators

  alias Compadre.Parsers, as: Ps

  ## Core combinators ##

  test "bind/2: the first parser is successful" do
    parser = bind Ps.advance(1), fn nil ->
      Ps.advance(1)
    end

    assert {:ok, nil, <<3>>} = parse(parser, <<1, 2, 3>>)
  end

  test "bind/2: the first parser is not successful" do
    parser = bind Ps.flunk("error"), fn _ ->
      Ps.fixed(:success)
    end

    assert {:error, "error", "baz"} = parse_test(parser, "heybaz", pos: 3)
  end

  test "plus/2" do
    parser = plus(Ps.binary("bar"), Ps.binary("baz"))

    assert {:ok, "bar", " rest"} = parse_test(parser, "bar rest")
    assert {:ok, "baz", " rest"} = parse_test(parser, "baz rest")

    assert {:ok, "baz", " rest"} =
      parser
      |> parse_test("foob", pos: 3)
      |> Compadre.feed("a")
      |> Compadre.feed("z rest")
  end

  test "with_consumed_input/1" do
    parser = with_consumed_input(Ps.advance(3))
    assert {:ok, {nil, "foo"}, "rest"} = Compadre.parse(parser, "foorest")

    assert {:ok, {nil, "foo"}, "rest"} =
      parser
      |> Compadre.parse("f")
      |> Compadre.feed("o")
      |> Compadre.feed("orest")
  end

  test "followed_by/2: both parsers succeed" do
    parser = followed_by(Ps.take_bytes(1), Ps.take_bytes(2))
    assert {:ok, <<1>>, "rest"} = parse(parser, <<1, 2, 3, "rest">>)
  end

  test "followed_by/2: one of the parsers fails" do
    # First one
    parser = followed_by(Ps.binary("foo"), Ps.binary("bar"))
    assert {:error, _, "fobar"} = parse_test(parser, "fobar")

    # Second one
    parser = followed_by(Ps.binary("foo"), Ps.flunk(:oops))
    assert {:error, :oops, "foobar"} = parse_test(parser, "foobar")
  end

  test "label/2" do
    parser = label(Ps.binary("foo"), "foo parser")
    assert {:error, msg, "bar"} = parse_test(parser, "_bar", pos: 1)
    assert msg =~ "parser 'foo parser' failed: "
  end

  ## Combinators that are built upon the "core" combinators ##

  test "look_ahead/1" do
    parser = look_ahead(Ps.binary("foo"))
    assert {:ok, "foo", "foobar"} = parse_test(parser, "_foobar", pos: 1)
  end

  test "seq/2: the first parser succeeds" do
    parser = seq(Ps.take_bytes(1), Ps.take_bytes(2))
    assert {:ok, <<2, 3>>, "rest"} = parse(parser, <<1, 2, 3, "rest">>)
  end

  test "seq/2: one of the parsers fails" do
    # First one
    parser = seq(Ps.flunk(:oops), Ps.advance(3))
    assert {:error, :oops, "baz"} = parse_test(parser, "baz")

    # Second one
    parser = seq(Ps.take_bytes(3), Ps.flunk(:oops))
    assert {:error, :oops, ""} = parse_test(parser, "baz")
  end

  test "transform/2" do
    parser = transform(Ps.fixed("foo"), &(&1 <> "bar"))
    assert {:ok, "foobar", "rest"} = parse_test(parser, "rest")

    parser = transform(Ps.flunk("error"), &(&1 <> "bar"))
    assert {:error, "error", "rest"} = parse_test(parser, "rest")
  end

  test "defaulting_to/2" do
    parser = defaulting_to(Ps.binary("foo"), :default)
    assert {:ok, "foo", " rest"} = parse_test(parser, "_foo rest", pos: 1)
    assert {:ok, :default, "bar"} = parse_test(parser, "_bar", pos: 1)
  end

  test "one_of/1" do
    parser = one_of(Enum.map(~w(foo bar baz), &Ps.binary/1))
    assert {:ok, "foo", " rest"} = parse_test(parser, "_foo rest", pos: 1)
    assert {:ok, "bar", " rest"} = parse_test(parser, "_bar rest", pos: 1)
    assert {:ok, "baz", " rest"} = parse_test(parser, "_baz rest", pos: 1)
  end

  test "many_until/2" do
    parser = many_until(Ps.take_bytes(1), Ps.binary("-stop"))
    assert {:ok, ~w(a b c), "-rest"} = parse_test(parser, "abc-stop-rest")

    parser = many_until(Ps.binary("foo"), Ps.binary("-stop"))
    assert {:ok, [], "bar-stop"} = parse_test(parser, "bar-stop")
  end

  test "take_until/2" do
    parser = take_until(Ps.binary("foo"))
    assert {:ok, "before-", "-rest"} = parse_test(parser, "before-foo-rest")
  end

  test "many/1" do
    parser = many(Ps.binary("foo"))

    assert {:ok, ["foo", "foo"], "bar"} =
      parser
      |> parse_test("foo")
      |> Compadre.feed("fo")
      |> Compadre.feed("obar")

    assert {:ok, [], "bar"} = parse_test(parser, "bar")
  end

  test "one_or_more/1" do
    parser = one_or_more(Ps.binary("foo"))

    assert {:ok, ["foo", "foo"], "bar"} =
      parser
      |> parse_test("_foo", pos: 1)
      |> Compadre.feed("fo")
      |> Compadre.feed("obar")

    assert {:error, _, _} = parse_test(parser, "bar")
  end

  test "sequence/1: all the sequence successfully goes through" do
    parser = sequence([Ps.binary("foo"), Ps.binary("bar")])
    assert {:ok, ["foo", "bar"], " rest"} =
      parser
      |> parse_test("_f", pos: 1)
      |> Compadre.feed("oob")
      |> Compadre.feed("ar rest")
  end

  test "sequence/1: the sequence is stopped on the first failing parser" do
    parser = sequence([Ps.binary("foo"), Ps.binary("bar"), Ps.binary("baz")])
    assert {:error, _, _} =
      parser
      |> parse_test("_f", pos: 1)
      |> Compadre.feed("oobaz")
  end

  test "count/1" do
    parser = count(Ps.binary("foo"), 3)
    assert {:ok, ["foo", "foo", "foo"], " rest"} =
      parser
      |> parse_test("_foo", pos: 1)
      |> Compadre.feed("foof")
      |> Compadre.feed("oo rest")
  end

  test "sep_by_at_least_one/2" do
    parser = sep_by_at_least_one(Ps.take_bytes(3), Ps.binary(", "))

    assert {:ok, ["foo", "bar", "baz"], "-rest"} =
      parse_test(parser, "foo, bar, baz-rest")

    assert {:error, _, "fo"} = parse_test(parser, "fo") |> Compadre.eoi()
  end

  test "sep_by/2" do
    parser = sep_by(Ps.take_bytes(3), Ps.binary(", "))

    assert {:ok, ["foo", "bar", "baz"], "-rest"} =
      parse_test(parser, "foo, bar, baz-rest")

    assert {:ok, [], "fo"} = parse_test(parser, "fo") |> Compadre.eoi()
  end
end
