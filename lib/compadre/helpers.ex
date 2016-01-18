defmodule Compadre.Helpers do
  @moduledoc false

  @doc """
  TODO
  """
  @spec from_position_to_end(binary, non_neg_integer) :: binary
  def from_position_to_end(binary, position)

  def from_position_to_end(bin, pos) when pos <= byte_size(bin) do
    :binary.part(bin, pos, byte_size(bin) - pos)
  end

  def from_position_to_end(bin, pos) do
    raise ArgumentError, "position #{pos} is outside the given binary: #{inspect bin}"
  end
end
