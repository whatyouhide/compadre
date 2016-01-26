defmodule Compadre.Parsers.BinaryTest do
  use ExUnit.Case, async: true

  doctest Compadre.Parsers.Binary

  import Compadre.TestHelper
  import Compadre.Parsers.Binary

  test "peek_byte/0" do
    assert_parse_result peek_byte(), "foo", {:ok, {:ok, ?f}, "foo"}
    assert_parse_result peek_byte(), {"", :eoi}, {:ok, nil, ""}
  end

  test "peek_byte!/0" do
    assert_parse_result peek_byte!(), "foo", {:ok, ?f, "foo"}
    assert_parse_result peek_byte!(), {"", :eoi} , {:error, "unexpected end of input", ""}
  end

  test "take_byte/0" do
    assert_parse_result take_byte(), "foo", {:ok, ?f, "oo"}
    assert_parse_result take_byte(), {"", :eoi}, {:error, "unexpected end of input", ""}
  end

  test "byte/1" do
    assert_parse_result byte(?a), "abc", {:ok, ?a, "bc"}
    assert_parse_result byte(?a), "bc", {:error, "expected byte 97, found 98", "bc"}
    assert_parse_result byte(?a), {"", :eoi}, {:error, "unexpected end of input", ""}
  end

  test "satisfy/1" do
    assert_parse_result satisfy(&(&1 in ?a..?z)), "abc1", {:ok, ?a, "bc1"}
    msg = "expected byte #{?1} to satisfy predicate"
    assert_parse_result satisfy(&(&1 in ?a..?z)), "100", {:error, ^msg, "100"}
    assert_parse_result satisfy(&(&1 in ?a..?z)), {"", :eoi}, {:error, "unexpected end of input", ""}
  end

  test "take_bytes/1" do
    assert_parse_result take_bytes(5), "hello world!", {:ok, "hello", " world!"}

    msg = "expected to have 5 bytes available, only got 3"
    assert_parse_result take_bytes(5), {"foo", :eoi}, {:error, ^msg, "foo"}
  end

  test "peek_bytes/1" do
    assert_parse_result peek_bytes(5), "hello world", {:ok, {:ok, "hello"}, "hello world"}
    assert_parse_result peek_bytes(4), {"foo", :eoi}, {:ok, nil, "foo"}
  end

  test "peek_bytes!/1" do
    assert_parse_result peek_bytes!(5), "hello world!", {:ok, "hello", "hello world!"}

    msg = "expected to have 5 bytes available, only got 3"
    assert_parse_result peek_bytes!(5), {"foo", :eoi}, {:error, ^msg, "foo"}
  end

  test "binary/1" do
    parser = binary("foo")

    assert_parse_result parser, "foobar", {:ok, "foo", "bar"}

    # We'll leave these tests as is because with randomly crafted inputs we
    # can't be sure about the error message (e.g., if the input is "bar" we can
    # say 'found "bar"', if it's "ba" we have to say 'found "ba"').

    msg = ~s(expected "foo", found "bar")
    assert {:error, ^msg, "barbaz"} = parse_test(parser, "heybarbaz", pos: 3)

    msg = ~s(expected "foo", found "fba")
    assert {:error, ^msg, "fbar"} = parse_test(parser, "fbar")

    assert {:error, ~s(expected "foo", found "fo"), "fo"} =
      parser
      |> parse_test("f")
      |> Compadre.feed("o")
      |> Compadre.eoi()

    assert {:error, ~s(expected "foo", found ""), _} =
      parser |> parse_test("", complete?: true)
  end

  test "until_binary/1" do
    parser = until_binary("stop")

    assert_parse_result parser, "foo-stop", {:ok, "foo-", "stop"}
    assert_parse_result parser, {"foo-st", :eoi}, {:error, "unexpected end of input", "st"}
  end
end
