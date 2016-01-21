defmodule Compadre.Parser do
  alias __MODULE__
  alias Compadre.State

  defstruct [:f]

  @doc """
  Creates a new `Parser` struct from the given `fun`.
  """
  # TODO spec
  def new(fun)

  def new(fun) when is_function(fun, 3),
    do: %Parser{f: fun}
  def new(fun) when is_function(fun),
    do: raise(ArgumentError, "a parser is a function that takes 3 arguments" <>
                             " but the provided one takes #{arity(fun)}")

  @doc """
  Applies the given parser to the arguments it expects.
  """
  # TODO spec
  def apply(%Parser{f: f}, %State{} = state, failf, succf)
      when is_function(failf, 2) and is_function(succf, 2) do
    f.(state, failf, succf)
  end

  defp arity(fun) when is_function(fun) do
    :erlang.fun_info(fun)[:arity]
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(p, opts) do
      uniq = :erlang.fun_info(p.f)[:uniq]
      concat ["#Compadre.Parser<", to_doc(uniq, opts), ">"]
    end
  end
end
