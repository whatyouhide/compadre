defmodule CompadreTest do
  use ExUnit.Case, async: true

  doctest Compadre

  test "parse/2 vs parse_in_one_shot/2" do
    parser = Compadre.Parsers.Text.integer()

    assert {:partial, _} = Compadre.parse(parser, "123")
    assert {:ok, 123, ""} = Compadre.parse_in_one_shot(parser, "123")
  end
end
