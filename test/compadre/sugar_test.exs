defmodule Compadre.SugarTest do
  use ExUnit.Case

  import Compadre.Sugar, only: [combine: 1]

  test "using the sugar syntax" do
    import Compadre.Parsers
    import Compadre.Parsers.{Binary, Text}

    parser = combine do
      year <- integer()
      _ = if year < 0 do
        raise "BC is not supported"
      end
      byte(?-)
      month <- integer()
      byte(?-)
      day <- integer()
      result = {year, month, day}
      fixed(result)
    end

    assert %Compadre.Parser{} = parser

    assert Compadre.parse(parser, "1991-09-06 or so") == {:ok, {1991, 09, 06}, " or so"}

    assert_raise RuntimeError, "BC is not supported", fn ->
      Compadre.parse(parser, "-200-09-09 is BC")
    end
  end

  test "the RHS of <- is checked for being a parser" do
    parser = combine do
      string <- Compadre.Parsers.fixed("not_a_parser")
      _ <- String.to_atom(string)
      Compadre.Parsers.fixed(:ok)
    end

    message = "expressions on the right of a <- in a combine block must be parsers, got: :not_a_parser"
    assert_raise ArgumentError, message, fn ->
      Compadre.parse(parser, "foo")
    end
  end

  test "statements are checked for being parsers" do
    parser = combine do
      :ok <- Compadre.Parsers.fixed(:ok)
      :not_a_parser
      Compadre.Parsers.fixed(:ok)
      # TODO if :not_a_parser is the last statement, then the error is different
    end

    message = "statements in a combine block must be parsers, got: :not_a_parser"
    assert_raise ArgumentError, message, fn ->
      Compadre.parse(parser, "foo")
    end
  end
end
