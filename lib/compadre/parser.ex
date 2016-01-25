defmodule Compadre.Parser do
  @moduledoc """
  A struct that represents a parser.
  """

  alias __MODULE__
  alias Compadre.State

  @typep failf(t) :: (term, State.t -> t)
  @typep succf(t) :: (term, State.t -> t)

  @typep parser_fun(failt, succt) :: (State.t, failf(failt), succf(succt) -> failt | succt)

  @typedoc """
  A parser, the basic unit of parsing in Compadre.

  The internal representation is not to be used by users, as it may change
  without notice.
  """
  @type t(failt, succt) :: %__MODULE__{
    f: parser_fun(failt, succt),
  }

  defstruct [:f]

  @doc """
  Creates a new `Parser` struct from the given `fun`.
  """
  @spec new(parser_fun(failt, succt)) :: t(failt, succt) when failt: any, succt: any
  def new(fun)

  def new(fun) when is_function(fun, 3),
    do: %Parser{f: fun}
  def new(fun) when is_function(fun),
    do: raise(ArgumentError, "a parser is a function that takes 3 arguments" <>
                             " but the provided one takes #{arity(fun)}")

  @doc """
  Applies the given parser to the arguments it expects.
  """
  @spec apply(t(failt, succt), State.t, failf(failt), succf(succt)) ::
    failt | succt | Partial.t(any)
    when failt: any, succt: any
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
