defmodule Compadre.Parsers do
  alias Compadre.Parser
  alias Compadre.Combinators, as: Combs
  alias Compadre.Core.{Success, Partial}

  @doc """
  A parser that always returns the given `value`, without consuming input.

  ## Examples

      Compadre.parse(Compadre.Parsers.fixed(:foo), "hello")
      #=> {:ok, :foo, "hello"}

  """
  @spec fixed(value) :: Parser.t(value, any) when value: any
  def fixed(value) do
    Parser.new fn input, pos, _failf, succf ->
      succf.(%Success{result: value, bytes_consumed: 0}, input, pos)
    end
  end

  # This parser simply demands input immediately if there's no input, otherwise
  # just returns `nil`.
  # Made public for testing.
  @doc false
  @spec demand_input() :: Parser.t(nil, any)
  def demand_input() do
    Parser.new(&do_demand_input/4)
  end

  defp do_demand_input(<<>>, pos, failf, succf),
    do: %Partial{cont: &do_demand_input(&1, pos, failf, succf)}
  defp do_demand_input(input, pos, _failf, succf),
    do: succf.(%Success{result: nil, bytes_consumed: 0}, input, pos)

  # This parser simply advances by `nbytes` bytes. It never fails: if the input
  # doesn't have enough bytes, it simply returns a continuation.
  # Made public for testing (as it's quite an important building block).
  @doc false
  @spec advance(non_neg_integer) :: Parser.t(nil, any)
  def advance(nbytes) do
    Parser.new(&do_advance(&1, &2, &3, &4, nbytes))
  end

  defp do_advance(input, pos, failf, succf, nbytes) do
    case input do
      <<_ :: size(pos)-bytes, _ :: size(nbytes)-bytes, _ :: binary>> ->
        succf.(%Success{result: nil, bytes_consumed: nbytes}, input, pos + nbytes)
      _ ->
        %Partial{cont: &do_advance(input <> &1, pos, failf, succf, nbytes)}
    end
  end

  @doc """
  Parses exactly like `parser` but returns both `parser`'s result and the
  consumed input.

  This function just wraps a parser, creating a new parser that parses like the
  original parser but returns a tuple with the original parser's result as the
  first element and the input that the original parser returned as the second
  element.

  ## Examples

  Say `integer()` is a parser that parses an integer from the input (i.e.,
  `Compadre.parse(integer(), "32rest") == {:ok, 32, "rest"}`). Then:

      p = Compadre.Parsers.with_consumed_input(integer())
      Compadre.parse(p, "32rest")
      #=> {:ok, {32, "32"}, "rest"}

  """
  @spec with_consumed_input(Parser.t(succ, fail)) :: Parser.t({succ, binary}, fail)
        when succ: any, fail: any
  def with_consumed_input(parser) do
    Parser.new fn input, pos, failf, succf ->
      Parser.apply parser, input, pos, failf, fn(succ, new_input, new_pos) ->
        consumed = :binary.part(new_input, pos, new_pos - pos)
        succf.(%{succ | result: {succ.result, consumed}}, new_input, new_pos)
      end
    end
  end

  @doc """
  Takes the next `nbytes` bytes from the input.

  This parser never fails; it returns a partial result if the input is smaller
  than `nbytes` long.
  """
  @spec take_bytes(non_neg_integer) :: Parser.t(binary, any)
  def take_bytes(nbytes) do
    Parser.new fn input, pos, failf, succf ->
      Parser.apply peek_bytes(nbytes), input, pos, failf, fn(succ, new_input, ^pos) ->
        succf.(%{succ | bytes_consumed: nbytes}, new_input, pos + nbytes)
      end
    end
  end

  @doc """
  """
  @spec peek_bytes(non_neg_integer) :: Parser.t(binary, any)
  def peek_bytes(nbytes) do
    Parser.new(&do_peek_bytes(&1, &2, &3, &4, nbytes))
  end

  defp do_peek_bytes(input, pos, failf, succf, nbytes) do
    case input do
      <<_before :: size(pos)-bytes, result :: size(nbytes)-bytes, _ :: binary>> ->
        succf.(%Success{result: result, bytes_consumed: 0}, input, pos)
      _ ->
        %Partial{cont: &do_peek_bytes(input <> &1, pos, failf, succf, nbytes)}
    end
  end

  @doc """
  """
  @spec peek_byte() :: Parser.t(byte, any)
  def peek_byte() do
    parser = Parser.new fn input, pos, _failf, succf ->
      success = %Success{result: :binary.first(input), bytes_consumed: 1}
      succf.(success, input, pos + 1)
    end

    Combs.seq(demand_input(), parser)
  end
end
