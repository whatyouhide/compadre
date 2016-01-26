defmodule Compadre.Parsers.Text do
  @moduledoc """
  Parsers to work with "text" - things humans understand (like integers, words,
  and son on).
  """

  alias Compadre.Helpers
  alias Compadre.Parser
  alias Compadre.Parsers
  alias Compadre.Combinators, as: Combs

  @doc """
  Parses an integer with an optional sign.

  ## Examples

      iex> import Compadre.Parsers.Text
      iex> Compadre.parse(integer(), "123rest")
      {:ok, 123, "rest"}
      iex> Compadre.parse(integer(), "-55rest")
      {:ok, -55, "rest"}

  """
  @spec integer() :: Parser.t(any, integer)
  def integer() do
    parser = Parser.new(&do_integer/3)
    Combs.seq(Parsers.demand_input(), parser)
  end

  @doc """
  Parses an integer without a sign.

  If a sign is encountered, this parser fails.

  ## Examples

      iex> import Compadre.Parsers.Text
      iex> Compadre.parse(unsigned_integer(), "123rest")
      {:ok, 123, "rest"}
      iex> Compadre.parse(unsigned_integer(), "+55rest")
      {:error, "unexpected sign (+)", "+55rest"}

  """
  @spec unsigned_integer() :: Parser.t(any, non_neg_integer)
  def unsigned_integer() do
    Combs.bind Parsers.Binary.peek_byte!(), fn
      b when b in '+-' -> Parsers.flunk("unexpected sign (#{<<b>>})")
      _                -> integer()
    end
  end

  # This always parses with an optional sign (as it uses Integer.parse/1 in the
  # background). We took care of the sign before calling this.
  defp do_integer(state, failf, succf) do
    # We're sure we have something in the input here, as we called
    # demand_input() before calling this.

    input_size = byte_size(state.input)
    target     = Helpers.from_position_to_end(state, input_size)

    case Integer.parse(target) do
      {i, ""} ->
        nfailf = fn(_, nstate) ->
          # We return `i` and advance to the end of the input (as we reached the
          # eoi); we can use `input_size` instead of the size of `nstate`'s
          # input because if we're here in the failf, it means we reached eoi so
          # state and nstate have the same input.
          succf.(i, %{nstate | pos: input_size})
        end
        nsuccf = fn(_, nstate) ->
          do_integer(nstate, failf, succf)
        end
        Helpers.prompt_or_fail_if_complete(state, nfailf, nsuccf)
      {i, rest} ->
        succf.(i, %{state | pos: input_size - byte_size(rest)})
      :error when target == "+" or target == "-" ->
        Parser.apply Parsers.demand_input(), Helpers.advance_pos(state, 1), failf, fn _, nstate ->
          do_integer(%{nstate | pos: nstate.pos - 1}, failf, succf)
        end
      :error ->
        failf.("expected integer", state)
    end
  end

  @doc """
  Parses a float from the input.

  The float is parsed using `Float.parse/1`, so rules that apply to that
  function also apply to this function. For example, floats with a sign will be
  parsed as well as floats without sign.

  As the documentation for `Float.parse/1` mentions, if the size of the floats
  exceeds the maximum size of `1.7976931348623157e+308` then this parser will
  fail.

  ## Examples

      iex> import Compadre.Parsers.Text
      iex> Compadre.parse(float(), "1.0e-10...")
      {:ok, 1.0e-10, "..."}

  """
  def float() do
    Combs.seq(Parsers.demand_input(), Parser.new(&do_float/3))
  end

  defp do_float(state, failf, succf) do
    input_size = byte_size(state.input)
    target     = Helpers.from_position_to_end(state, input_size)

    case safe_parse_float(target) do
      {:error, :too_large} ->
        failf.("tried to parse float, but it was too large", state)
      {f, bin} when bin in ["", ".", "e", "e+", "e-", ".e", ".e+", ".e-"] ->
        nfailf = fn(_, nstate) -> succf.(f, %{nstate | pos: input_size}) end
        nsuccf = fn(_, nstate) -> do_float(nstate, failf, succf) end
        Helpers.prompt_or_fail_if_complete(state, nfailf, nsuccf)
      {f, rest} ->
        succf.(f, %{state | pos: input_size - byte_size(rest)})
      :error when target == "+" or target == "-" ->
        Parser.apply Parsers.demand_input(), Helpers.advance_pos(state, 1), failf, fn(_, nstate) ->
          do_float(%{nstate | pos: nstate.pos - 1}, failf, succf)
        end
      :error ->
        failf.("expected float", state)
    end
  end

  defp safe_parse_float(target) do
    try do
      Float.parse(target)
    rescue
      ArgumentError -> {:error, :too_large}
    end
  end
end
