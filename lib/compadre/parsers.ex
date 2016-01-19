defmodule Compadre.Parsers do
  alias Compadre.Parser
  alias Compadre.Combinators, as: Combs
  alias Compadre.Core.{Success, Failure, Partial}

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

  defp do_demand_input(input, pos, failf, succf) do
    if byte_size(input) == pos do
      %Partial{cont: &do_demand_input(input <> &1, pos, failf, succf)}
    else
      succf.(%Success{result: nil, bytes_consumed: 0}, input, pos)
    end
  end

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
  TODO
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
  TODO
  """
  @spec peek_byte() :: Parser.t(byte, any)
  def peek_byte() do
    parser = Parser.new fn input, pos, _failf, succf ->
      success = %Success{result: :binary.at(input, pos), bytes_consumed: 0}
      succf.(success, input, pos)
    end

    Combs.seq(demand_input(), parser)
  end

  @doc """
  TODO
  """
  def binary(target) do
    do_binary(target, target)
  end

  defp do_binary(orig_target, "") do
    fixed(orig_target)
  end

  defp do_binary(orig_target, <<first, rest_of_target :: binary>> = target) do
    Combs.bind peek_byte(), fn
      ^first ->
        Combs.seq(advance(1), do_binary(orig_target, rest_of_target))
      _other_byte ->
        Parser.new fn input, pos, failf, _succf ->
          consumed = byte_size(orig_target) - byte_size(target)
          original_start = pos - consumed
          remaining_input_size = byte_size(input) - original_start
          failing_input = :binary.part(input,
                                       original_start,
                                       min(remaining_input_size, byte_size(orig_target)))

          msg = "expected #{inspect orig_target}, found #{inspect failing_input}"
          failf.(%Failure{reason: msg}, input, original_start)
        end
    end
  end

  @doc """
  TODO
  """
  # TODO spec
  def satisfy_after_transforming(transformation, pred) do
    Combs.bind peek_byte(), fn b ->
      transformed = transformation.(b)
      if pred.(transformed) do
        Combs.seq(advance(1), fixed(transformed))
      else
        Parser.new fn input, pos, failf, _succf ->
          reason = "expected byte #{b} to satisfy the given predicate (after" <>
                   " being transformed), but it didn't"
          failf.(%Failure{reason: reason}, input, pos)
        end
      end
    end
  end

  @doc """
  TODO
  """
  # TODO spec
  def satisfy(pred) do
    satisfy_after_transforming(&(&1), pred)
  end
end
