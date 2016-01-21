defmodule Compadre.Parsers do
  alias Compadre.Parser
  alias Compadre.State
  alias Compadre.Partial
  alias Compadre.Combinators, as: Combs

  ## Core parsers ##
  # These parsers do not depend on any parsers or combinators.

  def fixed(value) do
    Parser.new fn state, _failf, succf ->
      succf.(value, state)
    end
  end

  def flunk(error) do
    Parser.new fn state, failf, _succf ->
      failf.(error, state)
    end
  end

  # This parser simply demands input immediately if there's no input, otherwise
  # just returns `nil`. Fails if we reach eoi.
  # Made public for testing.
  def demand_input() do
    Parser.new fn state, failf, succf ->
      if byte_size(state.input) == state.pos do
        nfailf = fn nil, nstate -> failf.("unexpected end of input", nstate) end
        prompt(state, nfailf, succf)
      else
        succf.(nil, state)
      end
    end
  end

  # This parser simply advances by `nbytes` bytes. It doesn't fail if the input
  # doesn't have enough bytes, it simply returns a continuation. If we reach
  # eoi, it fails.
  @spec advance(non_neg_integer) :: Parser.t(nil, any)
  def advance(nbytes) do
    Parser.new(&do_advance(&1, &2, &3, nbytes))
  end

  defp do_advance(%State{input: input, pos: pos} = state, failf, succf, nbytes) do
    case input do
      <<_ :: size(pos)-bytes, _ :: size(nbytes)-bytes, _ :: binary>> ->
        succf.(nil, %{state | pos: pos + nbytes})
      _ ->
        nfailf = fn nil, nstate ->
          avail_bytes = byte_size(nstate.input) - nstate.pos
          msg  = "expected to have #{nbytes} bytes available, only got #{avail_bytes}"
          failf.(msg, nstate)
        end
        nsuccf = fn nil, nstate ->
          do_advance(nstate, failf, succf, nbytes)
        end
        prompt(state, nfailf, nsuccf)
    end
  end

  ## Parsers that depend on other parsers/combinators ##

  def take_bytes(nbytes) do
    advance(nbytes)
    |> Combs.with_consumed_input()
    |> Combs.transform(fn {nil, consumed} -> consumed end)
  end

  def peek_bytes(nbytes) do
    Combs.look_ahead(take_bytes(nbytes))
  end

  def peek_byte() do
    Combs.transform(peek_bytes(1), fn <<b>> -> b end)
  end

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
        Parser.new fn state, failf, _succf ->
          consumed = byte_size(orig_target) - byte_size(target)
          original_start = state.pos - consumed
          remaining_input_size = byte_size(state.input) - original_start
          failing_input = :binary.part(state.input,
                                       original_start,
                                       min(remaining_input_size, byte_size(orig_target)))

          msg = "expected #{inspect orig_target}, found #{inspect failing_input}"
          failf.(msg, %{state | pos: original_start})
        end
    end
  end

  # Returns a partial result that immediately asks for new input, and calls
  # `failf` is the next given input is empty (i.e., we reached the final eoi) or
  # `succf` if the new input is not empty.
  defp prompt(%State{} = state, failf, succf) do
    Partial.new fn
      ""   -> failf.(nil, %{state | complete?: true})
      data -> succf.(nil, %{state | input: state.input <> data, complete?: false})
    end
  end
end
