defmodule Compadre.ParserTest do
  use ExUnit.Case, async: true

  alias Compadre.Parser

  test "implementation of the Inspect protocol" do
    res = inspect(Parser.new(fn _, _, _ -> nil end))
    assert res =~ ~r/#Compadre\.Parser<\d+>/
  end

  test "new/1" do
    assert %Parser{} = Parser.new(fn _, _, _ -> nil end)

    msg = "a parser is a function that takes 3 arguments but the provided one takes 2"
    assert_raise ArgumentError, msg, fn ->
      Parser.new(fn _, _ -> nil end)
    end

    assert_raise FunctionClauseError, fn -> Parser.new(:foo) end
  end
end
