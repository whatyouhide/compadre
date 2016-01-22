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
    assert_parse_result advance(5), "hello world", {:ok, nil, " world"}

    msg = "expected to have 5 bytes available, only got 3"
    assert_parse_result advance(5), {"foo", :eoi}, {:error, ^msg, "foo"}
  end

  test "take_bytes/1" do
    assert_parse_result take_bytes(5), "hello world!", {:ok, "hello", " world!"}

    msg = "expected to have 5 bytes available, only got 3"
    assert_parse_result take_bytes(5), {"foo", :eoi}, {:error, ^msg, "foo"}
  end

  test "peek_bytes/1" do
    assert_parse_result peek_bytes(5), "hello world", {:ok, "hello", "hello world"}
  end

  test "peek_byte/0" do
    assert_parse_result peek_byte(), "foo", {:ok, ?f, "foo"}
    assert_parse_result peek_byte(), {"", :eoi}, {:error, _, ""}
  end

  test "take_byte/0" do
    assert_parse_result take_byte(), "foo", {:ok, ?f, "oo"}
    assert_parse_result take_byte(), {"", :eoi}, {:error, _, ""}
  end

  test "at_end?/0" do
    assert_parse_result at_end?(), {"", :eoi}, {:ok, true, ""}
    assert_parse_result at_end?(), "foo", {:ok, false, "foo"}
  end

  test "eoi/0" do
    assert_parse_result eoi(), {"", :eoi}, {:ok, _, ""}
    assert_parse_result eoi(), "bar", {:error, "expected end of input", "bar"}
  end

  test "binary/1" do
    parser = binary("foo")

    assert_parse_result parser, "foobar", {:ok, "foo", "bar"}

    # We'll leave these two tests as is because with randomly crafted inputs we
    # can't be sure about the error message (e.g., if the input is "bar" we can
    # say 'found "bar"', if it's "ba" we have to say 'found "ba"').

    msg = ~s(expected "foo", found "bar")
    assert {:error, ^msg, "barbaz"} = parse_test(parser, "heybarbaz", pos: 3)

    msg = ~s(expected "foo", found "fba")
    assert {:error, ^msg, "fbar"} = parse_test(parser, "fbar")
  end
end
