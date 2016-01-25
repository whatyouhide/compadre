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
        Helpers.prompt(state, nfailf, nsuccf)
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
end
