defmodule Compadre.Parsers.TextTest do
  use ExUnit.Case, async: true

  doctest Compadre.Parsers.Text

  import Compadre.Parsers.Text
  import Compadre.TestHelper

  test "integer/0" do
    assert_parse_result integer(), "123rest", {:ok, 123, "rest"}
    assert_parse_result integer(), "-123rest", {:ok, -123, "rest"}
    assert_parse_result integer(), "+123rest", {:ok, 123, "rest"}

    assert_parse_result integer(), "foo", {:error, "expected integer", "foo"}

    # Some eoi/empty input cases
    assert_parse_result integer(), {"123", :eoi}, {:ok, 123, ""}
    assert_parse_result integer(), {"", :eoi}, {:error, "unexpected end of input", ""}
  end

  test "unsigned_integer/0" do
    assert_parse_result unsigned_integer(), "123rest", {:ok, 123, "rest"}
    assert_parse_result unsigned_integer(), "-123" , {:error, "unexpected sign (-)", "-123"}
    assert_parse_result unsigned_integer(), "+123" , {:error, "unexpected sign (+)", "+123"}

    # Some eoi/empty input cases
    assert_parse_result unsigned_integer(), {"123", :eoi}, {:ok, 123, ""}
    assert_parse_result unsigned_integer(), "foo", {:error, "expected integer", "foo"}
    assert_parse_result unsigned_integer(), {"", :eoi}, {:error, "unexpected end of input", ""}
  end
end
