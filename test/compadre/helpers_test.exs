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
      send self(), {:failf, nstate}
      assert nstate.complete?
      assert nstate.input == "foo"
    end

    succf = fn(nil, nstate) ->
      send self(), {:succf, nstate}
      refute nstate.complete?
      assert nstate.input == "foobar"
    end

    state = %State{input: "foo", pos: 3, complete?: false}
    assert %Partial{cont: cont} = prompt(state, failf, succf)

    # Success (more input)
    cont.("bar")
    assert_received {:succf, %State{complete?: false, input: "foobar"}}
    refute_received {:failf, _}
    # Failure (eoi)
    cont.("")
    refute_received {:succf, _}
    assert_received {:failf, %State{complete?: true, input: "foo"}}
  end

  test "prompt_or_fail_if_complete/3" do
    failf = fn(nil, nstate) -> nstate end
    succf = fn(_, _) -> nil end
    state = %State{input: "foo", pos: 3, complete?: false}

    assert %Partial{} = prompt_or_fail_if_complete(state, failf, succf)

    # We don't return a continuation when the state is already complete.
    complete_state = %{state | complete?: true}
    assert prompt_or_fail_if_complete(complete_state, failf, succf) == complete_state
  end

  test "advance_pos/2" do
    assert %State{pos: 5} = advance_pos(%State{pos: 0}, 5)
    assert_raise FunctionClauseError, fn -> advance_pos(%State{pos: 0}, -10) end
  end
end
