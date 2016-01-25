defmodule Compadre.Parsers do
  alias Compadre.Parser
  alias Compadre.State
  alias Compadre.Partial
  alias Compadre.Helpers
  alias Compadre.Combinators, as: Combs

  @doc """
  Always returns the given value, without consuming any input.

  ## Examples

      iex> import Compadre.Parsers
      iex> Compadre.parse(fixed(:foo), "whatever")
      {:ok, :foo, "whatever"}

  """
  @spec fixed(val) :: Parser.t(any, val) when val: any
  def fixed(value) do
    Parser.new fn(state, _failf, succf) ->
      succf.(value, state)
    end
  end

  # Made public because it's used as a building block for many other parsers,
  # also outside this module.
  @doc false
  @spec flunk(val) :: Parser.t(val, any) when val: any
  def flunk(error) do
    Parser.new fn(state, failf, _succf) ->
      failf.(error, state)
    end
  end

  # This parser simply demands input immediately if there's no input, otherwise
  # just returns `nil`. Fails if we reach eoi.
  # Made public for testing.
  @doc false
  @spec demand_input() :: Parser.t(any, any)
  def demand_input() do
    Parser.new fn(%State{input: input, pos: pos} = state, failf, succf) ->
      error_msg = "unexpected end of input"
      cond do
        state.complete? ->
          failf.(error_msg, state)
        byte_size(input) == pos ->
          nfailf = fn nil, nstate -> failf.(error_msg, nstate) end
          Helpers.prompt(state, nfailf, succf)
        true ->
          succf.(nil, state)
      end
    end
  end

  # This parser simply advances by `nbytes` bytes. It doesn't fail if the input
  # doesn't have enough bytes, it simply returns a continuation. If we reach
  # eoi, it fails.
  # Made public for testing.
  @doc false
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
          msg = "expected to have #{nbytes} bytes available, only got #{avail_bytes}"
          failf.(msg, nstate)
        end
        nsuccf = fn nil, nstate ->
          do_advance(nstate, failf, succf, nbytes)
        end
        Helpers.prompt(state, nfailf, nsuccf)
    end
  end

  @doc """
  A parser that returns `true` if we reached end of input, `false` otherwise.

  If there's no more input in the current state, but we have not reached end of
  input yet (e.g., through `Compadre.eoi/1`), then this parser returns a partial
  result.

  ## Examples

      iex> import Compadre.Parsers
      iex> Compadre.parse(at_end?(), "foo")
      {:ok, false, "foo"}
      iex> Compadre.parse(at_end?(), "") |> Compadre.eoi()
      {:ok, true, ""}

  """
  @spec at_end?() :: Parser.t(any, boolean)
  def at_end?() do
    Parser.new fn state, _failf, succf ->
      cond do
        state.complete? ->
          succf.(true, state)
        byte_size(state.input) == state.pos ->
          pfailf = fn nil, nstate -> succf.(true, nstate) end
          psuccf = fn nil, nstate -> succf.(false, nstate) end
          Helpers.prompt(state, pfailf, psuccf)
        true ->
          succf.(false, state)
      end
    end
  end

  @doc """
  A parser that only succeeds if we reached end of input, and fails otherwise.

  When the parser succeeds, it just returns `nil` (as its return value is not
  important, just the fact that it fails or succeeds is).

  ## Examples

      iex> import Compadre.Parsers
      iex> Compadre.parse(eoi(), "foo")
      {:error, "expected end of input", "foo"}
      iex> {:partial, _} = result = Compadre.parse(eoi(), "")
      iex> Compadre.eoi(result)
      {:ok, nil, ""}

  """
  @spec eoi() :: Parser.t(binary, nil)
  def eoi() do
    Parser.new fn(state, failf, succf) ->
      Parser.apply at_end?(), state, failf, fn
        true, nstate  -> succf.(nil, nstate)
        false, nstate -> failf.("expected end of input", nstate)
      end
    end
  end
end
