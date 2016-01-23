defmodule Compadre do
  alias Compadre.Helpers
  alias Compadre.State
  alias Compadre.Partial
  alias Compadre.Parser

  @type parse_result(failt, succt) ::
    {:ok, succt, binary}
    | {:error, failt, binary}
    | {:partial, (binary -> parse_result(failt, succt))}

  @spec parse(Parser.t(failt, succt), binary) ::
    parse_result(failt, succt)
    when failt: any, succt: any
  def parse(parser, input) do
    state0 = %State{input: input, pos: 0, complete?: false}

    case Parser.apply(parser, state0, terminal_failf(), terminal_succf()) do
      %Partial{cont: cont} -> {:partial, cont}
      other                -> other
    end
  end

  @spec parse_in_one_shot(Parser.t(failt, succt), binary) ::
    parse_result(failt, succt)
    when failt: any, succt: any
  def parse_in_one_shot(parser, input) do
    state0 = %State{input: input, pos: 0, complete?: true}
    case Parser.apply(parser, state0, terminal_failf(), terminal_succf()) do
      %Partial{} -> raise "this should never happen!"
      other      -> other
    end
  end

  @doc """
  Feeds more input to the result of a parser.

  If the result is a successful result, that result will be returned with
  `input` appended to its `rest`. The same happens for failure results. If
  result is a partial result (a continuation), then that continuation will be
  called with `input` as its input.
  """
  @spec feed(parse_result(failt, succt), binary) :: parse_result(failt, succt)
    when failt: any, succt: any
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

  @spec eoi({:partial, (binary -> val)}) :: val when val: any
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
      {:ok, result, Helpers.from_position_to_end(state)}
    end
  end

  defp terminal_failf() do
    fn(reason, %State{} = state) ->
      {:error, reason, Helpers.from_position_to_end(state)}
    end
  end
end
