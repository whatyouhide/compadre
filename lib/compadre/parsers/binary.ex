defmodule Compadre.Parsers.Binary do
  @moduledoc """
  Parsers to work in a byte-oriented fashion.
  """

  alias Compadre.Parser
  alias Compadre.State
  alias Compadre.Helpers
  alias Compadre.Parsers
  alias Compadre.Combinators, as: Combs

  @doc """
  Returns the next byte, without consuming any input.

  If we have reached end of input, then this parser will return `nil`;
  otherwise, it will return `{:ok, byte}`. This parser never fails.

  ## Examples

      iex> import Compadre.Parsers.Binary
      iex> Compadre.parse(peek_byte(), "foo")
      {:ok, {:ok, ?f}, "foo"}
      iex> Compadre.parse(peek_byte(), "") |> Compadre.eoi()
      {:ok, nil, ""}

  """
  @spec peek_byte() :: Parser.t(any, {:ok, byte} | nil)
  def peek_byte() do
    Parser.new fn(state, _failf, succf) ->
      nfailf = fn(_, nstate) -> succf.(nil, nstate) end
      nsuccf = fn(_, nstate) -> succf.({:ok, :binary.at(nstate.input, nstate.pos)}, nstate) end
      Parser.apply(Parsers.demand_input(), state, nfailf, nsuccf)
    end
  end

  @doc """
  Returns the next byte or fails if we reached end of input, without consuming
  any input.

  ## Examples

      iex> import Compadre.Parsers.Binary
      iex> Compadre.parse(peek_byte!(), "foo")
      {:ok, ?f, "foo"}
      iex> Compadre.parse(peek_byte!(), "") |> Compadre.eoi()
      {:error, "unexpected end of input", ""}

  """
  @spec peek_byte!() :: Parser.t(any, byte)
  def peek_byte!() do
    Parsers.demand_input()
    |> Combs.seq(peek_byte)
    |> Combs.transform(fn {:ok, b} -> b end)
  end

  @doc """
  Takes a byte from the input.

  This parser fails if we reached end of input.

  ## Examples

      iex> import Compadre.Parsers.Binary
      iex> Compadre.parse(take_byte(), "foo")
      {:ok, ?f, "oo"}

  """
  @spec take_byte() :: Parser.t(any, byte)
  def take_byte() do
    Combs.bind peek_byte!(), fn b ->
      Combs.seq(Parsers.advance(1), Parsers.fixed(b))
    end
  end

  @doc """
  Matches the given byte at the start of the input.

  If it succeeds, it consumes and returns that byte. Fails if end of input is
  reached.

  ## Examples

      iex> import Compadre.Parsers.Binary
      iex> Compadre.parse(byte(?+), "+100")
      {:ok, ?+, "100"}
      iex> Compadre.parse(byte(?+), "") |> Compadre.eoi()
      {:error, "unexpected end of input", ""}

  """
  @spec byte(byte) :: Parser.t(any, byte)
  def byte(target) when is_integer(target) and target in 0..255 do
    # Implemented "natively" for performance.
    Parser.new(&do_byte(&1, &2, &3, target))
  end

  defp do_byte(%State{pos: pos, input: input} = state, failf, succf, target) do
    if byte_size(input) > pos do
      case :binary.at(input, pos) do
        ^target -> succf.(target, %{state | pos: pos + 1})
        b       -> failf.("expected byte #{target}, found #{b}", state)
      end
    else
      Parser.apply Parsers.demand_input(), state, failf, fn(_, nstate) ->
        do_byte(nstate, failf, succf, target)
      end
    end
  end

  @doc """
  Matches any byte that satisfies the given predicate.

  Fails if we're at the end of input or if the next byte doesn't satisfy `pred`.

  ## Examples

      iex> import Compadre.Parsers.Binary
      iex> Compadre.parse(satisfy(fn b -> b in ?a..?z end), "foo")
      {:ok, ?f, "oo"}

  """
  @spec satisfy((byte -> boolean)) :: Parser.t(any, byte)
  def satisfy(pred) when is_function(pred, 1) do
    Combs.bind peek_byte!(), fn b ->
      if pred.(b) do
        Combs.seq(Parsers.advance(1), Parsers.fixed(b))
      else
        Parsers.flunk("expected byte #{b} to satisfy predicate")
      end
    end
  end

  @doc """
  Takes the next `nbytes` bytes from the input.

  This parser fails if we reach end of input and there are less than `nbytes`
  bytes available. `nbytes` must be strictly greater than zero.

  ## Examples

      iex> import Compadre.Parsers.Binary
      iex> Compadre.parse(take_bytes(3), "foobar")
      {:ok, "foo", "bar"}
      iex> Compadre.parse(take_bytes(3), "fo") |> Compadre.eoi()
      {:error, "expected to have 3 bytes available, only got 2", "fo"}

  """
  @spec take_bytes(pos_integer) :: Parser.t(any, binary)
  def take_bytes(nbytes) when is_integer(nbytes) and nbytes > 0 do
    Parsers.advance(nbytes)
    |> Combs.with_consumed_input()
    |> Combs.transform(fn {_, consumed} -> consumed end)
  end

  @doc """
  Peeks the next `nbytes` bytes from the input, without consuming any input.

  If we reach end of input and there are less than `nbytes` bytes available,
  this parser returns `nil`; otherwise, it returns `{:ok, bytes}`. This parser
  never fails.

  ## Examples

      iex> import Compadre.Parsers.Binary
      iex> Compadre.parse(peek_bytes(3), "foobar")
      {:ok, {:ok, "foo"}, "foobar"}
      iex> Compadre.parse(peek_bytes(3), "fo") |> Compadre.eoi()
      {:ok, nil, "fo"}

  """
  @spec peek_bytes(pos_integer) :: Parser.t(any, {:ok, binary} | nil)
  def peek_bytes(nbytes) when is_integer(nbytes) and nbytes > 0 do
    take_bytes(nbytes)
    |> Combs.look_ahead()
    |> Combs.transform(fn bs -> {:ok, bs} end)
    |> Combs.defaulting_to(nil)
  end

  @doc """
  Works like `peek_bytes/1`, but fails if we reach end of input with less than
  `nbytes` left.

  ## Examples

      iex> import Compadre.Parsers.Binary
      iex> Compadre.parse(peek_bytes!(3), "foobar")
      {:ok, "foo", "foobar"}
      iex> Compadre.parse(peek_bytes!(3), "fo") |> Compadre.eoi()
      {:error, "expected to have 3 bytes available, only got 2", "fo"}

  """
  @spec peek_bytes!(pos_integer) :: Parser.t(any, binary)
  def peek_bytes!(nbytes) when is_integer(nbytes) and nbytes > 0 do
    Combs.look_ahead(take_bytes(nbytes))
  end

  @doc ~S"""
  Matches the given binary exactly.

  Fails if the input doesn't start with `target`. Returns `target` if it
  matches.

  ## Examples

      iex> import Compadre.Parsers.Binary
      iex> Compadre.parse(binary("foo"), "foobar")
      {:ok, "foo", "bar"}
      iex> Compadre.parse(binary("foo"), "forza")
      {:error, "expected \"foo\", found \"for\"", "forza"}

  """
  @spec binary(binary) :: Parser.t(any, binary)
  def binary(target) do
    # Could be implemented using `peek_byte()` and comparing each byte with the
    # corresponding byte in `target`, but that performs terribly when the binary
    # we expect is already there (which shouldn't be a rare case, at all!).
    Parser.new(&do_binary(&1, &2, &3, target, byte_size(target)))
  end

  defp do_binary(%State{input: input, pos: pos} = state, failf, succf, target, target_size) do
    # `segment` is a binary that starts from the current position and is as long
    # as `target`. If the available bytes are less than the size of the target,
    # then `segment` is all the remaining input.

    segment_size = min(target_size, byte_size(input) - pos)
    segment      = :binary.part(input, pos, segment_size)
    lc_prefix    = :binary.longest_common_prefix([segment, target])

    # If the LCP is the target size, this means `segment` contains `target`. If
    # `segment` is shorter than `target` but the LCP is as long as `segment`,
    # that means that `segment` is a prefix of `target` (we return a
    # continuation). Otherwise, they differ and we error.
    cond do
      lc_prefix == target_size ->
        succf.(target, %{state | pos: pos + target_size})
      segment_size < target_size and lc_prefix == segment_size ->
        Helpers.prompt state, failf, fn(_, nstate) ->
          do_binary(nstate, failf, succf, target, target_size)
        end
      true ->
        failf.("expected #{inspect target}, found #{inspect segment}", state)
    end
  end
end
