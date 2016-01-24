defmodule Compadre.HelpersTest do
  use ExUnit.Case, async: true

  import Compadre.Helpers

  alias Compadre.State
  alias Compadre.Partial

  test "from_position_to_end/1-2" do
    assert from_position_to_end(%State{input: "foobar", pos: 3}) == "bar"
    assert from_position_to_end(%State{input: "foobar", pos: 3}, 6) == "bar"
  end

  test "prompt/3" do
    failf = fn(nil, nstate) ->
      assert nstate.complete?
      assert nstate.input == "foo"
    end

    succf = fn(nil, nstate) ->
      refute nstate.complete?
      assert nstate.input == "foobar"
    end

    state = %State{input: "foo", pos: 3, complete?: false}

    assert %Partial{cont: cont} = prompt(state, failf, succf)
    cont.("bar")
    cont.("")
  end

  test "advance_pos/2" do
    assert %State{pos: 5} = advance_pos(%State{pos: 0}, 5)
    assert_raise FunctionClauseError, fn -> advance_pos(%State{pos: 0}, -10) end
  end
end
