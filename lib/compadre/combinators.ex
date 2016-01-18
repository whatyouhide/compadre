defmodule Compadre.Combinators do
  alias Compadre.Parser
  alias Compadre.Core.Failure

  @doc """
  TODO
  """
  @spec bind(Parser.t, (any -> Parser.t)) :: Parser.t
  def bind(parser, bindf) when is_function(bindf, 1) do
    Parser.new fn input, pos, failf, succf ->
      # We apply the original parser with an update success
      # continuation: this continuation (but the same failure continuation).
      Parser.apply parser, input, pos, failf, fn(succ, new_input, new_pos) ->
        # We create the new parser, which is returned by `bindf`...
        new_parser = ensure_parser(bindf.(succ.result))
        # ...and we apply it with the rest of the input and the original
        # success/failure continuation.
        Parser.apply(new_parser, new_input, new_pos, failf, succf)
      end
    end
  end

  defp ensure_parser(%Parser{} = new_parser) do
    new_parser
  end

  defp ensure_parser(other) do
    raise ArgumentError, "the second argument passed to bind/2" <>
                         " must be a function that returns a" <>
                         " %Compadre.Parser{}, got: " <> (inspect other)
  end

  @doc """
  TODO
  """
  @spec seq(Parser.t, Parser.t) :: Parser.t
  def seq(parser1, parser2) do
    bind(parser1, fn _ -> parser2 end)
  end

  @doc """
  TODO
  """
  @spec plus(Parser.t, Parser.t) :: Parser.t
  def plus(parser1, parser2) do
    Parser.new fn input, pos, failf, succf ->
      new_failf = fn %Failure{} = _ ->
        Parser.apply(parser2, input, failf, succf)
      end
      Parser.apply(parser1, input, new_failf, succf)
    end
  end
end
