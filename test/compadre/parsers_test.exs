defmodule Compadre.ParsersTest do
  use ExUnit.Case

  doctest Compadre.Parsers

  import Compadre.TestHelper
  import Compadre.Parsers

  test "fixed/1" do
    p = fixed(:hello_world)
    assert Compadre.parse(p, "foo") == {:ok, :hello_world, "foo"}
    assert Compadre.parse(p, "") == {:ok, :hello_world, ""}
  end

  test "at_end?/0" do
    assert {:ok, true, ""} = parse_test(at_end?(), "", complete?: true)
    assert_parse_result at_end?(), {"", :eoi}, {:ok, true, ""}
    assert_parse_result at_end?(), "foo", {:ok, false, "foo"}
  end

  test "eoi/0" do
    assert_parse_result eoi(), {"", :eoi}, {:ok, _, ""}
    assert_parse_result eoi(), "bar", {:error, "expected end of input", "bar"}
  end

  ## `@doc false` parsers

  test "flunk/1" do
    # Qualified name because of ExUnit.Assertions.flunk/1.
    assert_parse_result Compadre.Parsers.flunk(:err), "foo", {:error, :err, "foo"}
  end

  test "demand_input/0 when there is available input" do
    assert Compadre.parse(demand_input(), "foo") == {:ok, nil, "foo"}
  end

  test "demand_input/0 when we wait for new input" do
    assert {:partial, _} = res = parse_test(demand_input(), "a", pos: 1)
    assert Compadre.feed(res, "foo") == {:ok, nil, "foo"}
  end

  test "demand_input/0 when we reach eoi" do
    assert {:error, "unexpected end of input", ""} =
      demand_input()
      |> parse_test("skipped", pos: 7)
      |> Compadre.eoi()

    # When we already are at eoi
    assert {:error, "unexpected end of input", ""} =
      demand_input()
      |> parse_test("", complete?: true)
  end

  test "advance/1" do
    assert_parse_result advance(5), "hello world", {:ok, nil, " world"}

    msg = "expected to have 5 bytes available, only got 3"
    assert_parse_result advance(5), {"foo", :eoi}, {:error, ^msg, "foo"}
  end
end
