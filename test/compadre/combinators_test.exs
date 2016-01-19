defmodule Compadre.CombinatorsTest do
  use ExUnit.Case

  import Compadre.TestHelper
  import Compadre
  import Compadre.Combinators
  alias Compadre.Parsers, as: Ps

  test "bind/2: the first parser is successful" do
    parser = bind Ps.take_bytes(1), fn b ->
      send(self, b)
      Ps.take_bytes(1)
    end

    assert {:ok, <<2>>, <<3>>} = parse(parser, <<1, 2, 3>>)
    assert_received <<1>>
  end

  test "bind/2: the first parser is not successful" do
    parser = bind Ps.binary("foo"), fn "foo" ->
      send(self, :success)
      Ps.binary("bar")
    end

    assert {:error, _, "baz"} = parse_test(parser, "heybaz", pos: 3)
  end

  test "seq/2: the first parser succeeds" do
    parser = seq(Ps.take_bytes(1), Ps.take_bytes(1))
    assert {:ok, <<2>>, ""} = parse(parser, <<1, 2>>)
  end

  test "seq/2: the first parser doesn't succeed" do
    parser = seq(Ps.binary("foo"), Ps.binary("bar"))
    assert {:error, _, "baz"} = parse_test(parser, "baz")
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

  test "one_of/1" do
    parser = one_of(Enum.map(~w(foo bar baz), &Ps.binary/1))
    assert {:ok, "foo", " rest"} = parse_test(parser, "_foo rest", pos: 1)
    assert {:ok, "bar", " rest"} = parse_test(parser, "_bar rest", pos: 1)
    assert {:ok, "baz", " rest"} = parse_test(parser, "_baz rest", pos: 1)
  end

  test "defaulting_to/2" do
    parser = defaulting_to(Ps.binary("foo"), :default)
    assert {:ok, "foo", " rest"} = parse_test(parser, "_foo rest", pos: 1)
    assert {:ok, :default, "bar"} = parse_test(parser, "_bar", pos: 1)
  end

  test "look_ahead/1" do
    parser = look_ahead(Ps.binary("foo"))
    assert {:ok, "foo", "foobar"} = parse_test(parser, "_foobar", pos: 1)
  end

  test "count/1" do
    parser = count(Ps.binary("foo"), 3)
    assert {:ok, ["foo", "foo", "foo"], " rest"} =
      parser
      |> parse_test("_foo", pos: 1)
      |> Compadre.feed("foof")
      |> Compadre.feed("oo rest")
  end

  test "label/2" do
    parser = label(Ps.binary("foo"), "foo parser")
    assert {:error, msg, "bar"} = parse_test(parser, "_bar", pos: 1)
    assert msg =~ "parser 'foo parser' failed: "
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
end
