defmodule Compadre.Helpers do
  @moduledoc false

  alias Compadre.State
  alias Compadre.Partial

  @doc """
  Returns the portion of the binary in `state` from the current position to the
  end.
  """
  @spec from_position_to_end(State.t) :: binary
  def from_position_to_end(%State{input: input, pos: pos}) do
    :binary.part(input, pos, byte_size(input) - pos)
  end

  @doc """
  Same as `from_position_to_end/1`, but takes the byte size of the input as an
  argument so that if it has already been computed it's not computed again.
  """
  @spec from_position_to_end(State.t, non_neg_integer) :: binary
  def from_position_to_end(%State{input: input, pos: pos}, size) do
    :binary.part(input, pos, size - pos)
  end

  @doc """
  Returns a partial result that immediately asks for new input.

  If the next given input is empty (i.e., we reached the final eoi), then
  `failf` is called with `:eoi` and the new state as its arguments. If the new
  input is not empty, then `succf` is called with the new input and the new
  state as arguments.
  """
  @spec prompt(State.t, (nil, State.t -> failt), (nil, State.t -> succt)) ::
    Partial.t(failt | succt)
    when failt: any, succt: any
  def prompt(%State{} = state, failf, succf)
      when is_function(failf, 2) and is_function(succf, 2) do
    Partial.new fn
      "" ->
        failf.(nil, %{state | complete?: true})
      data ->
        succf.(nil, %{state | input: state.input <> data, complete?: false})
    end
  end

  @doc """
  Advances the position in state by the given amount `n`.
  """
  @spec advance_pos(State.t, pos_integer) :: State.t
  def advance_pos(%State{pos: pos} = state, n) when n > 0 do
    %{state | pos: pos + n}
  end
end
