defmodule Compadre.ParsersTest do
  use ExUnit.Case

  import Compadre.TestHelper
  import Compadre.Parsers

  test "fixed/1" do
    p = fixed(:hello_world)
    assert Compadre.parse(p, "foo") == {:ok, :hello_world, "foo"}
    assert Compadre.parse(p, "") == {:ok, :hello_world, ""}
  end

  test "demand_input/0 when there is available input" do
    assert Compadre.parse(demand_input(), "foo") == {:ok, nil, "foo"}
  end

  test "demand_input/0 when we wait for new input" do
    # Works both if we're at the very beginning of the input...
    assert {:partial, _} = res = Compadre.parse(demand_input(), "")
    assert Compadre.feed(res, "foo") == {:ok, nil, "foo"}

    # ...as well as in the middle of it.
    assert {:partial, _} = res = parse_test(demand_input(), "a", pos: 1)
    assert Compadre.feed(res, "foo") == {:ok, nil, "foo"}
  end

  test "demand_input/0 when we reach eoi" do
    assert {:error, "unexpected end of input", ""} =
      demand_input()
      |> parse_test("skipped", pos: 7)
      |> Compadre.eoi()
  end

  test "advance/1" do
    assert Compadre.parse(advance(1), "foo") == {:ok, nil, "oo"}

    assert {:ok, nil, "world"} =
      advance(6)
      |> parse_test("foohe", pos: 3)
      |> Compadre.feed("ll")
      |> Compadre.feed("o world")

    assert {:error, "expected to have 3 bytes available, only got 2", "ba"} =
      advance(3)
      |> parse_test("fooba", pos: 3)
      |> Compadre.eoi()
  end

  test "take_bytes/1" do
    assert {:ok, "hello", " world!"} =
      take_bytes(5)
      |> parse_test("ignoreme he", pos: byte_size("ignoreme "))
      |> Compadre.feed("ll")
      |> Compadre.feed("o world!")

    assert {:error, _, _} =
      take_bytes(5)
      |> parse_test("foobar", pos: 3)
      |> Compadre.eoi()
  end

  test "peek_bytes/1" do
    assert {:ok, "hello", "hello world!"} =
      peek_bytes(5)
      |> parse_test("foohel", pos: 3)
      |> Compadre.feed("lo world!")
  end

  test "peek_byte/0" do
    assert {:ok, 1, <<1, 2, 3>>} =
      peek_byte()
      |> parse_test(<<0>>, pos: 1)
      |> Compadre.feed(<<1, 2, 3>>)

    assert {:error, _, ""} =
      peek_byte()
      |> parse_test(<<>>)
      |> Compadre.eoi()
  end

  test "take_byte/0" do
    assert {:ok, 1, <<2, 3>>} =
      take_byte()
      |> parse_test(<<0>>, pos: 1)
      |> Compadre.feed(<<1, 2, 3>>)

    assert {:error, _, ""} =
      peek_byte()
      |> parse_test(<<>>)
      |> Compadre.eoi()
  end

  test "at_end?/0" do
    assert {:ok, true, ""} =
      at_end?()
      |> parse_test("foo", pos: 3)
      |> Compadre.eoi()

    assert {:ok, false, "bar"} =
      at_end?()
      |> parse_test("foo", pos: 3)
      |> Compadre.feed("bar")
  end

  test "eoi/0" do
    assert {:ok, _, ""} =
      eoi()
      |> parse_test("foo", pos: 3)
      |> Compadre.eoi()

    assert {:error, "expected end of input", "bar"} =
      eoi()
      |> parse_test("foo", pos: 3)
      |> Compadre.feed("bar")
  end

  test "binary/1" do
    parser = binary("foo")

    assert {:ok, "foo", "bar"} =
      parser
      |> parse_test("hey", pos: 3)
      |> Compadre.feed("fo")
      |> Compadre.feed("obar")

    msg = ~s(expected "foo", found "bar")
    assert {:error, ^msg, "barbaz"} = parse_test(parser, "heybarbaz", pos: 3)

    msg = ~s(expected "foo", found "fba")
    assert {:error, ^msg, "fbar"} = parse_test(parser, "fbar")
  end
end
