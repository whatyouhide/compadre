defmodule Compadre do
  alias Compadre.Helpers
  alias Compadre.State
  alias Compadre.Partial
  alias Compadre.Parser

  @doc """
  TODO
  """
  # TODO spec
  def parse(parser, input) do
    failf0 = terminal_failf()
    succf0 = terminal_succf()
    state0 = %State{input: input, pos: 0, complete?: false}

    case Parser.apply(parser, state0, failf0, succf0) do
      %Partial{cont: cont} -> {:partial, cont}
      other                -> other
    end
  end

  @doc """
  Feeds more input to the result of a parser.

  If the result is a successful result, that result will be returned with
  `input` appended to its `rest`. The same happens for failure results. If
  result is a partial result (a continuation), then that continuation will be
  called with `input` as its input.
  """
  # TODO spec
  def feed(result, input)

  def feed({:ok, result, rest}, input),
    do: {:ok, result, rest <> input}
  def feed({:error, reason, rest}, input),
    do: {:error, reason, rest <> input}
  def feed({:partial, cont}, input),
    do: (case cont.(input) do
           %Partial{cont: cont} -> {:partial, cont}
           other                -> other
         end)

  # TODO spec
  def eoi({:partial, cont}) do
    case cont.("") do
      %Partial{} -> raise "a parser returned a :partial even on eoi"
      other      -> other
    end
  end

  def eoi(result) do
    result
  end

  defp terminal_succf() do
    fn(result, %State{} = state) ->
      {:ok, result, remaining_input(state)}
    end
  end

  defp terminal_failf() do
    fn(reason, %State{} = state) ->
      {:error, reason, remaining_input(state)}
    end
  end

  defp remaining_input(%State{input: input, pos: pos}) do
    Helpers.from_position_to_end(input, pos)
  end
end
