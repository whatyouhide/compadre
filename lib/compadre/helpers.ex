defmodule Compadre.Helpers do
  @moduledoc false

  alias Compadre.State

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
end
