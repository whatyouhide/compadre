defmodule Compadre.CombinatorsTest do
  use ExUnit.Case

  import Compadre
  import Compadre.Combinators
  alias Compadre.Parsers

  test "bind/2" do
    parser = bind Parsers.take_bytes(1), fn b ->
      send(self, b)
      Parsers.take_bytes(1)
    end

    assert {:ok, <<2>>, <<3>>} = parse(parser, <<1, 2, 3>>)
    assert_received <<1>>
  end

  test "seq/2" do
    parser = seq(Parsers.take_bytes(1), Parsers.take_bytes(1))
    assert {:ok, <<2>>, ""} = parse(parser, <<1, 2>>)
  end
end
