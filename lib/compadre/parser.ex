defmodule Compadre.Parser do
  alias __MODULE__
  alias Compadre.Core

  # The position in the input binary.
  @typep position :: non_neg_integer

  # Success and failure continuations.
  @typep succf(val) :: (Success.t(any), binary, position -> val)
  @typep failf(val) :: (Failure.t(any), binary, position -> val)

  # The actual parsing function.
  @typep parser_fun(fail, succ) ::
    (binary, position, failf(fail), succf(succ) -> Core.result(succ, fail))

  # The %Parser{} struct.
  @opaque t(succ, fail) :: %__MODULE__{f: parser_fun(succ, fail)}
  defstruct [:f]

  @doc """
  Creates a new `Parser` struct from the given `fun`.
  """
  @spec new(parser_fun(succ, fail)) :: t(succ, fail) when succ: any, fail: any
  def new(fun)

  def new(fun) when is_function(fun, 4),
    do: %Parser{f: fun}
  def new(fun) when is_function(fun),
    do: raise(ArgumentError, "a parser is a function that takes 4 arguments" <>
                             " but the provided one takes #{arity(fun)}")


  @doc """
  Applies the given parser to the arguments it expects.
  """
  @spec apply(t(succ, fail), binary, position, failf(fail), succf(succ)) ::
    succ | fail
    when succ: any, fail: any
  def apply(%Parser{f: f}, input, pos, failf, succf)
      when is_binary(input) and is_integer(pos) and pos >= 0 and
           is_function(failf, 3) and is_function(succf, 3) do
    f.(input, pos, failf, succf)
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
