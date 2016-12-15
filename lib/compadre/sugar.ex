defmodule Compadre.Sugar do
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
