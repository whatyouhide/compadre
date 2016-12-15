defmodule Compadre.Sugar do
  @moduledoc """
  Provides some isolated syntactic sugar to create and combine parsers.

  The entry point for the syntactic sugar is the `combine/1` macro.
  """

  @doc ~S"""
  Provides syntactic sugar to create a new parser by combining parsers.

  This macro takes a block of "instructions" and returns a parser. In such
  block, you can use three main constructs:

    * `pattern <- parser`: with this construct you can match on the result of
      the parser on the right side. The parser will be automatically run on the
      input and `pattern` will be matched with the successful result of
      `parser`. If `parser` errors out, the execution of the given block stops
      with this line and the error is returned.

    * `parser`: with this construct you can declare a parser to run. It behaves
      like `_ <- parser` but is optimized.

    * `pattern = expression`: a regular pattern matching expression that
      supports binding of variables.

  ## Examples

  In the example below, we're showing a simplicistic version of a parser for HTTP headers capable of parsing lists of headers like this:

      Content-Type: application/json
      Content-Length: 231

  First, we import a few functions so that the example reads nicely:

      import Compadre.Sugar
      import Compadre.Parsers, only: [fixed: 1]
      import Compadre.Parsers.Binary, only: [binary: 1, until_binary: 1, take_while: 1]
      alias Combine.Combinators

  Then, we define a parser that parses a single HTTP header:

      http_header_parser = combine do
        key <- take_while(fn byte -> byte != ?: end)
        binary(": ")
        value <- until_binary("\n")
        binary("\n")
        fixed({key, value})
      end

  This parser "returns" `{key, value}` when successful. To parse multiple
  headers, we only need to make a parser that applies our header parser multiple
  times:

      headers_parser = Combinators.many(http_header_parser)

  Now, we can use our header parser:

      headers_parser
      |> Combine.parse("Content-Type: application/json\n")
      |> Combine.eoi()
      #=> {:ok, [{"Content-Type", "application/json"}]}

  """
  defmacro combine(do: block) do
    case block do
      {:__block__, _meta, actions} when is_list(actions) ->
        expand_actions(actions)
      action ->
        expand_actions([action])
    end
  end

  defp expand_actions([action]) do
    expand_last_action(action)
  end

  defp expand_actions([{:<-, _meta, [var, parser]} | rest]) do
    quote do
      case unquote(parser) do
        %Compadre.Parser{} = parser ->
          Compadre.Combinators.bind(parser, fn(unquote(var)) ->
            unquote(expand_actions(rest))
          end)
        other ->
          raise ArgumentError, "expressions on the right of a <- in a combine " <>
                               "block must be parsers, got: #{inspect(other)}"
      end
    end
  end

  defp expand_actions([{:=, _, _} = code | rest]) do
    quote do
      unquote(code)
      unquote(expand_actions(rest))
    end
  end

  defp expand_actions([action | rest]) do
    quote do
      case unquote(action) do
        %Compadre.Parser{} = parser ->
          Compadre.Combinators.seq(parser, unquote(expand_actions(rest)))
        other ->
          raise ArgumentError, "statements in a combine block must be parsers, " <>
                               "got: #{inspect(other)}"
      end
    end
  end

  # Expands the last action in the `combine` block. If that action is a <- or =
  # assignment, raise an error (as a parser must be returned from the block).
  defp expand_last_action({op, _, _} = code) when op in [:=, :<-],
    do: raise(ArgumentError, "the last action in a combine block cannot be an" <>
                             " assignment (<- or =) as it must be a parser," <>
                             " got: #{Macro.to_string(code)}")
  defp expand_last_action(action), do: action
end
