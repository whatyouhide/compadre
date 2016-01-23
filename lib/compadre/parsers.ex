defmodule Compadre.Parsers do
  alias Compadre.Parser
  alias Compadre.State
  alias Compadre.Partial
  alias Compadre.Helpers
  alias Compadre.Combinators, as: Combs

  ## Core parsers ##
  # These parsers do not depend on any parsers or combinators.

  @spec fixed(val) :: Parser.t(any, val) when val: any
  def fixed(value) do
    Parser.new fn state, _failf, succf ->
      succf.(value, state)
    end
  end

  @spec flunk(val) :: Parser.t(val, any) when val: any
  def flunk(error) do
    Parser.new fn state, failf, _succf ->
      failf.(error, state)
    end
  end

  # This parser simply demands input immediately if there's no input, otherwise
  # just returns `nil`. Fails if we reach eoi.
  # Made public for testing.
  @spec demand_input() :: Parser.t(any, any)
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
  @spec advance(non_neg_integer) :: Parser.t(any, nil)
  def advance(nbytes) when is_integer(nbytes) and nbytes >= 0 do
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

  @spec take_bytes(non_neg_integer) :: Parser.t(any, binary)
  def take_bytes(nbytes) when is_integer(nbytes) and nbytes >= 0 do
    advance(nbytes)
    |> Combs.with_consumed_input()
    |> Combs.transform(fn {nil, consumed} -> consumed end)
  end

  @spec peek_bytes(non_neg_integer) :: Parser.t(any, binary)
  def peek_bytes(nbytes) when is_integer(nbytes) and nbytes >= 0 do
    Combs.look_ahead(take_bytes(nbytes))
  end

  @spec peek_byte() :: Parser.t(any, byte)
  def peek_byte() do
    Combs.transform(peek_bytes(1), fn <<b>> -> b end)
  end

  @spec take_byte() :: Parser.t(any, byte)
  def take_byte() do
    Combs.followed_by(peek_byte(), advance(1))
  end

  @spec at_end?() :: Parser.t(any, boolean)
  def at_end?() do
    Combs.transform(input_available?(), &Kernel.not/1)
  end

  @spec eoi() :: Parser.t(binary, nil)
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

  def take_bytes_while(pred) when is_function(pred, 1) do
    Combs.bind(peek_byte(), fn b ->
      if pred.(b) do
        Combs.seq(advance(1), take_bytes_while(pred))
      else
        fixed(:ok)
      end
    end)
    |> Combs.with_consumed_input()
    |> Combs.transform(fn {_, bin} -> bin end)
  end

  ## Parsers implemented "natively" for performance ##

  @spec binary(binary) :: Parser.t(any, binary)
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

  @spec until_binary(binary) :: Parser.t(any, binary)
  def until_binary(target) do
    Parser.new(&do_until_binary(&1, &2, &3, target, byte_size(target), demand_input()))
  end

  def do_until_binary(state, failf, succf, target, target_size, di, orig_pos \\ nil) do
    orig_pos = orig_pos || state.pos
    pos      = state.pos

    case state.input do
      <<_ :: size(pos)-bytes, ^target :: size(target_size)-bytes, _ :: binary>> ->
        result = :binary.part(state.input, orig_pos, pos - orig_pos)
        succf.(result, %{state | pos: pos + target_size})
      _ ->
        Parser.apply di, state, failf, fn _, nstate ->
          nstate = %{nstate | pos: pos + 1}
          do_until_binary(nstate, failf, succf, target, target_size, di, orig_pos)
        end
    end
  end

  @spec int() :: Parser.t(any, integer)
  def int() do
    Compadre.Parser.new(&do_int/3)
  end

  defp do_int(state, failf, succf) do
    input_size = byte_size(state.input)
    target     = Helpers.from_position_to_end(state, input_size)

    case Integer.parse(target) do
      {i, ""} ->
        prompt state,
               fn(_, nstate) -> succf.(i, nstate) end,
               fn(_, nstate) -> do_int(nstate, failf, succf) end
      {i, rest} ->
        succf.(i, %{state | pos: input_size - byte_size(rest)})
      :error ->
        failf.("expected integer", state)
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
