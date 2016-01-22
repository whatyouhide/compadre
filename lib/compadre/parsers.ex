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

  def take_byte() do
    Combs.followed_by(peek_byte(), advance(1))
  end

  def until_binary(target) do
    advance_until_binary(target)
    |> Combs.with_consumed_input()
    |> Combs.transform(fn {_, bin} -> :binary.part(bin, 0, byte_size(bin) - byte_size(target)) end)
  end

  def at_end?() do
    Combs.transform(input_available?(), &Kernel.not/1)
  end

  def eoi() do
    Parser.new fn state, failf, succf ->
      Parser.apply at_end?(), state, failf, fn end?, nstate ->
        if end? do
          succf.(nil, nstate)
        else
          failf.("expected end of input", nstate)
        end
      end
    end
  end

  ## Parsers implemented "natively" for performance ##

  def binary(target) do
    # Could be implemented using `peek_byte()` and comparing each byte with the
    # corresponding byte in `target`, but that performs terribly when the binary
    # we expect is already there (which shouldn't be a rare case, at all!).
    Parser.new(&do_binary(&1, &2, &3, target, byte_size(target)))
  end

  defp do_binary(state, failf, succf, target, target_size) do
    pos          = state.pos
    segment_size = min(target_size, byte_size(state.input) - pos)
    segment      = :binary.part(state.input, pos, segment_size)
    lc_prefix    = :binary.longest_common_prefix([segment, target])

    cond do
      lc_prefix == target_size ->
        succf.(target, %{state | pos: pos + target_size})
      segment_size < target_size and lc_prefix == segment_size ->
        prompt state, failf, fn nil, nstate ->
          do_binary(nstate, failf, succf, target, target_size)
        end
      true ->
        failf.("expected #{inspect target}, found #{inspect segment}", state)
    end
  end

  def advance_until_binary(target) do
    Parser.new(&do_advance_until_binary(&1, &2, &3, target, byte_size(target)))
  end

  def do_advance_until_binary(state, failf, succf, target, target_size) do
    pos = state.pos

    case state.input do
      <<_ :: size(pos)-bytes, ^target :: size(target_size)-bytes, _ :: binary>> ->
        succf.(nil, %{state | pos: pos + target_size})
      _ ->
        Parser.apply demand_input(), state, failf, fn nil, nstate ->
          nstate = %{nstate | pos: pos + 1}
          do_advance_until_binary(nstate, failf, succf, target, target_size)
        end
    end
  end

  ## Helpers

  # Returns a partial result that immediately asks for new input, and calls
  # `failf` is the next given input is empty (i.e., we reached the final eoi) or
  # `succf` if the new input is not empty.
  defp prompt(%State{} = state, failf, succf) do
    if state.complete? do
      failf.(nil, state)
    else
      Partial.new fn
        ""   -> failf.(nil, %{state | complete?: true})
        data -> succf.(nil, %{state | input: state.input <> data, complete?: false})
      end
    end
  end

  # This parser returns `false` if we reached eoi, otherwise (if input is
  # available or it can be in the future) `true`. This parser always succeeds.
  defp input_available?() do
    Parser.new fn state, _failf, succf ->
      if byte_size(state.input) == state.pos do
        pfailf = fn nil, nstate -> succf.(false, nstate) end
        psuccf = fn nil, nstate -> succf.(true, nstate) end
        prompt state, pfailf, psuccf
      else
        succf.(true, state)
      end
    end
  end
end
