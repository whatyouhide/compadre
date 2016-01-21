defmodule Compadre.Partial do
  @opaque t(result) :: %__MODULE__{
    cont: (binary -> result)
  }

  defstruct [:cont]

  @doc """
  Creates a new partial result from the given `continuation`. `continuation`
  must be a function of arity 1, otherwise an `ArgumentError` exception is
  raised.
  """
  @spec new((binary -> result)) :: t(result) when result: any
  def new(continuation)

  def new(continuation) when is_function(continuation, 1) do
    %__MODULE__{cont: continuation}
  end

  def new(continuation) when is_function(continuation) do
    raise ArgumentError, "continuations must be function with arity 1"
  end
end

defmodule Compadre.State do
  @opaque t :: %__MODULE__{
    input: binary,
    pos: non_neg_integer,
    complete?: boolean,
  }

  defstruct [:input, :pos, :complete?]
end
