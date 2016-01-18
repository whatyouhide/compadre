defmodule Compadre do
  alias Compadre.Core
  alias Compadre.Helpers
  alias Compadre.Core.{Success, Failure, Partial}
  alias Compadre.Parser

  @doc """
  TODO
  """
  # TODO fix this spec
  @spec parse(Parser.t, binary) :: {:ok, any, binary}
                                 | {:error, any, binary}
                                 | {:partial, (... -> any)}
  def parse(parser, input) do
    case Parser.apply(parser, input, 0, terminal_failf(), terminal_succf()) do
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
  @spec feed(Core.result(t1, t2), binary) :: Core.result(t1, t2) when t1: any, t2: any
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

  defp terminal_succf() do
    fn(%Success{} = succ, input, pos) ->
      {:ok, succ.result, Helpers.from_position_to_end(input, pos)}
    end
  end

  defp terminal_failf() do
    fn(%Failure{} = fail, input, pos) ->
      {:error, fail.reason, Helpers.from_position_to_end(input, pos)}
    end
  end

  defmacro compose(do: block) do
    case block do
      {:__block__, _meta, actions} ->
        expand_actions(actions)
      action ->
        expand_actions([action])
    end
    |> (fn q -> IO.puts(Macro.to_string(q)); q end).()
  end

  defp expand_actions([{op, _, _} = code]) when op in [:=, :<-] do
    raise ArgumentError, "the last action in a compose block cannot be an" <>
                         " assignment (<- or =) as it must be a parser," <>
                         " got: #{Macro.to_string(code)}"
  end

  defp expand_actions([action]) do
    action
  end

  defp expand_actions([{:<-, _meta, [var, parser]}|rest]) do
    quote do
      Compadre.bind unquote(parser), fn(unquote(var)) ->
        unquote(expand_actions(rest))
      end
    end
  end

  defp expand_actions([{:=, _, _} = code|rest]) do
    quote do
      unquote(code)
      unquote(expand_actions(rest))
    end
  end

  defp expand_actions([action|rest]) do
    quote do
      Compadre.seq(unquote(action), unquote(expand_actions(rest)))
    end
  end
end
