defmodule Compadre.HelpersTest do
  use ExUnit.Case

  import Compadre.Helpers

  test "from_position_to_end/2" do
    assert from_position_to_end("foobar", 3) == "bar"

    msg = "position 10 is outside the given binary: \"foo\""
    assert_raise ArgumentError, msg, fn ->
      from_position_to_end("foo", 10)
    end
  end
end
