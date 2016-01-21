defmodule Compadre.Bare do
  # @doc """
  # Feeds more input to the result of a parser.

  # If the result is a successful result, that result will be returned with
  # `input` appended to its `rest`. The same happens for failure results. If
  # result is a partial result (a continuation), then that continuation will be
  # called with `input` as its input.
  # """
  # @spec feed(result(t1, t2), binary) :: result(t1, t2) when t1: any, t2: any
  # def feed(result, input)

  # def feed(%Success{rest: rest} = succ, input),
  #   do: %{succ | rest: rest <> input}
  # def feed(%Failure{rest: rest} = fail, input),
  #   do: %{fail | rest: rest <> input}
  # def feed(%Partial{cont: cont}, input),
  #   do: cont.(input)

  # @doc """
  # Wraps a partial result in a new continuation or calls the callback on it.

  # If `result` is a partial result, then this function will return a new partial
  # result that "wraps" the old partial result by calling the old continuation in
  # the new continuation. If `result` is a success/failure result, then
  # `when_finished` is called with `result` as its input.
  # """
  # @spec wrap(result(succ, fail), (Success.t(succ) | Failure.t(fail) -> value))
  #   :: value | Partial.t(result(succ, fail))
  #      when succ: any,
  #           fail: any,
  #           value: any
  # def wrap(result, when_finished)

  # def wrap(%{__struct__: s} = result, when_finished)
  #   when s in [Success, Failure] and is_function(when_finished, 1),
  #   do: when_finished.(result)
  # def wrap(%Partial{cont: cont}, when_finished)
  #   when is_function(when_finished, 1),
  #   do: %Partial{cont: fn(data) -> wrap(cont.(data), when_finished) end}

  # @doc """
  # "Binds" the given result to the given `ok_fun`.

  # "Binding" is intended as in monadic binding (e.g. in Haskell): if `result` is
  # a partial result, then that result is wrapped exactly as in `wrap/2`. If
  # `result` is a successful result, then `ok_fun` is called with that result as
  # its input. If `result` is a failure result, then it's returned as is.

  # Informally, this function only "binds" `ok_fun` to the result if it's a
  # successful result. Chaining calls to `bind` will have the effect of stopping
  # the chain as soon as a failure is encountered (as all subsequent binds will
  # just return that failure).

  #     %Success{}
  #     |> bind(&(&1))
  #     |> bind(fn _ -> %Failure{} end)
  #     |> bind(fn _ -> IO.puts "foo" end)
  #     #=> %Failure{}

  # """
  # @spec bind(result(succ, fail), (Success.t(succ) -> value)) :: value
  #       when succ: any, fail: any, value: any
  # def bind(result, ok_fun) when is_function(ok_fun, 1) do
  #   wrap result, fn
  #     %Success{} = success -> ok_fun.(success)
  #     %Failure{} = failure -> failure
  #   end
  # end
end
