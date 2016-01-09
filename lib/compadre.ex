defmodule Compadre do
  defmodule Cont do
    @moduledoc """
    Opaque representation of a continuation.

    You can use this struct in pattern matches:

        case parse(data) do
          %Compadre.Cont{} -> :continuation
          {:ok, _, _}      -> :result
        end

    but you're not supposed to use the field of the struct (as they may change
    in the future).
    """

    defstruct fun: nil

    defimpl Inspect do
      import Inspect.Algebra

      def inspect(%Cont{fun: fun}, opts) do
        concat ["#Compadre.Cont<", to_doc(fun, opts), ">"]
      end
    end
  end

  @type parser_result ::
    {:ok, any, binary} | %Cont{fun: (binary -> parser_result)}

  @type continuation_fun ::
    (binary -> parser_result)

  @type continuation :: %Cont{fun: continuation_fun}

  @compile {:inline, mkcont: 1}

  @doc """
  Creates a continuation from the given `fun`.

  As the type signature says, `fun` should be a continuation function, i.e., a
  function that takes a binary and returns a `Compadre.parser_result`.
  """
  @spec mkcont(continuation_fun) :: Cont.t
  def mkcont(fun) when is_function(fun, 1) do
    %Cont{fun: fun}
  end

  @doc """
  Tries to resolve a continuation and runs `ok_fun` if it manages to.

  This function takes the result of a parsing attempt (so either `{:ok, value,
  rest}` or a continuation).

    * If `parser_result` is `{:ok, value, rest}`, then `ok_fun.(value, rest)` is
      called.
    * If `parser_result` is a continuation, then a wrapper continuation (with
      the same `ok_fun`) is returned.

  ## Examples

      iex> my_parser = fn
      ...>   ":"          -> Compadre.mkcont(fn(")" <> rest) -> {:ok, :smile, rest} end)
      ...>   ":)" <> rest -> {:ok, :smile, rest}
      ...> end
      iex> ok_fun = fn(:smile, _rest) -> :smile_from_ok_fun end
      iex> %Compadre.Cont{} = cont = Compadre.resolve_cont_then(my_parser.(":"), ok_fun)
      iex> Compadre.apply_cont(cont, ")")
      :smile_from_ok_fun

  """
  @spec resolve_cont_then(parser_result, (any, binary -> result)) :: result when result: any
  def resolve_cont_then(parser_result, ok_fun)

  def resolve_cont_then({:ok, val, rest}, ok_fun) when is_function(ok_fun, 2) do
    ok_fun.(val, rest)
  end

  def resolve_cont_then(%Cont{fun: cont}, ok_fun) when is_function(ok_fun, 2) do
    mkcont fn new_data ->
      resolve_cont_then(cont.(new_data), ok_fun)
    end
  end

  def resolve_cont_then(what, ok_fun) when is_function(ok_fun, 2) do
    msg = "expected {:ok, value, rest} or a continuation, got: #{inspect what}"
    raise ArgumentError, msg
  end

  @doc """
  Applies the given continuation to the given `data`.

  ## Examples

      iex> %Compadre.Cont{} = cont = Compadre.mkcont(fn data -> byte_size(data) end)
      iex> Compadre.apply_cont(cont, "foo")
      3

  """
  def apply_cont(continuation, data)

  def apply_cont(%Cont{fun: cont}, data) when is_bitstring(data),
    do: cont.(data)
  def apply_cont(_cont, term),
    do: raise(ArgumentError, "continuations can only be applied to bitstrings, got: #{inspect term}")
end
