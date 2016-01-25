defmodule Compadre do
  @moduledoc """
  Compadre is a parsing combinators library built with network binary protocol
  in mind.
  """

  alias Compadre.Helpers
  alias Compadre.State
  alias Compadre.Partial
  alias Compadre.Parser

  @type parse_result(failt, succt) ::
    {:ok, succt, binary}
    | {:error, failt, binary}
    | {:partial, (binary -> parse_result(failt, succt))}

  @doc """
  Parses the given `input` with the parser `parser`.

  If parsing is successful, returns `{:ok, result, rest}` where `result` is the
  result of the parsing and `rest` is the non-parsed input.

  If parsing fails, returns `{:error, reason, rest}` where `reason` is the
  reason the parsing failed and `rest` is the non-parsed input.

  If the input is not enough for `parser` to decide if it has to succeed or
  fail, a `{:partial, continuation}` result is returned: `continuation` is a
  function that takes additional input and resumes parsing from where it
  stopped. You could call the `continuation` directly, but the `feed/1` function
  is provided to do just that; see its documentation for more information.

  ## Examples

      iex> parser = Compadre.Parsers.Binary.take_bytes(5)
      iex> Compadre.parse(parser, "hello world")
      {:ok, "hello", " world"}
      iex> {:partial, cont} = Compadre.parse(parser, "hel")
      iex> cont.("lo world")
      {:ok, "hello", " world"}

  """
  @spec parse(Parser.t(failt, succt), binary) ::
    parse_result(failt, succt)
    when failt: any, succt: any
  def parse(parser, input) do
    state0 = %State{input: input, pos: 0, complete?: false}

    case Parser.apply(parser, state0, terminal_failf(), terminal_succf()) do
      %Partial{cont: cont} -> {:partial, cont}
      other                -> other
    end
  end

  @doc """
  Works like `parse/2`, but doesn't allow `parser` to return a partial result.

  ## Examples

      iex> parser = Compadre.Parsers.Binary.take_bytes(5)
      iex> Compadre.parse_in_one_shot(parser, "hello")
      {:ok, "hello", ""}
      iex> Compadre.parse_in_one_shot(parser, "foo")
      {:error, "expected to have 5 bytes available, only got 3", "foo"}

  """
  @spec parse_in_one_shot(Parser.t(failt, succt), binary) ::
    parse_result(failt, succt)
    when failt: any, succt: any
  def parse_in_one_shot(parser, input) do
    state0 = %State{input: input, pos: 0, complete?: true}
    case Parser.apply(parser, state0, terminal_failf(), terminal_succf()) do
      %Partial{} -> raise "this should never happen!"
      other      -> other
    end
  end

  @doc """
  Feeds more input to the result of a parser.

  If the result is a successful result, that result will be returned with
  `input` appended to its `rest`. The same happens for failure results.

  If `result` is a partial result (a continuation in the form `{:partial,
  function}`), then that continuation will be called with `input` as its input.
  Be careful when feeding an empty binary (`""`) to a partial result as that is
  a signal for end of input; read more about this in the documentation for
  `eoi/1`.

  ## Examples

      iex> parser = Compadre.Parsers.Binary.take_bytes(5)
      iex> parser |> Compadre.parse("hel") |> Compadre.feed("lo")
      {:ok, "hello", ""}

  """
  @spec feed(parse_result(failt, succt), binary) :: parse_result(failt, succt)
    when failt: any, succt: any
  def feed(result, input)

  def feed({:ok, result, rest}, input),
    do: {:ok, result, rest <> input}
  def feed({:error, reason, rest}, input),
    do: {:error, reason, rest <> input}
  def feed({:partial, cont}, input),
    do: (case cont.(input) do
           %Partial{cont: cont} -> {:partial, cont}
           other                -> other
         end)

  @doc """
  Signals that the end of the input has been reached to the given `result`.

  If `result` (which is a parser result) is a successful or failing result, then
  this function does nothing. However, if `result` is a partial result
  (`{:partial, continuation}`), then the continuation is called with an empty
  input (`""`). Note that calling `eoi(result)` is analogous to calling
  `feed(result, "")`, but `eoi/1` makes the purpose much clearer.
  """
  @spec eoi(parse_result(failt, succt)) :: parse_result(failt, succt)
    when failt: any, succt: any
  def eoi(result)

  def eoi({:partial, cont}) do
    case cont.("") do
      %Partial{} -> raise "a parser returned a :partial even on eoi"
      other      -> other
    end
  end

  def eoi(result) do
    result
  end

  defp terminal_succf() do
    fn(result, %State{} = state) ->
      {:ok, result, Helpers.from_position_to_end(state)}
    end
  end

  defp terminal_failf() do
    fn(reason, %State{} = state) ->
      {:error, reason, Helpers.from_position_to_end(state)}
    end
  end
end
