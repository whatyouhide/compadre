defmodule Compadre.HelpersTest do
  use ExUnit.Case

  import Compadre.Helpers
  alias Compadre.State

  test "from_position_to_end/2" do
    assert from_position_to_end(%State{input: "foobar", pos: 3}) == "bar"
  end
end
