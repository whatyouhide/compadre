defmodule Compadre.Combinators do
  @moduledoc """
  Collection of combinators to combine parsers and build new parsers.
  """

  alias Compadre.Parser
  alias Compadre.Parsers

  @doc """
  Combines `parser` and the parser returned by `bindf`.

  This function takes a `parser` and returns a new parser that first parses with
  `parser`, then calls `bindf` (which must return a parser) with the result of
  this parsing, then parses with the parser returned by the call to `bindf`.

  In other words, this combinator (which is a core one) gives the chance to
  *combine* two parsers to create a parser that depends on the result of the
  first parser. If the first parser fails, that failure is returned.

  ## Examples

  Say we want to parse a string like `"3foo"`, where the first byte is a number
  (in `0..9`) that tells us how many bytes are ahead. We can use just `bind/2`
  and `Compadre.Parsers.Binary.take_bytes/1` for this: we first parse the number
  of bytes (`"3"` in the example), then we return a parser that parses that
  number of bytes (that we converted to an integer with `String.to_integer/1`).

      iex> import Compadre.Combinators
      iex> nbytes_parser = Compadre.Parsers.Binary.take_bytes(1)
      iex> parser = bind nbytes_parser, fn nbytes ->
      ...>   Compadre.Parsers.Binary.take_bytes(String.to_integer(nbytes))
      ...> end
      iex> Compadre.parse(parser, "3foo...")
      {:ok, "foo", "..."}

  """
  @spec bind(Parser.t(failt, succt), (succt -> Parser.t(nfailt, nsucct))) ::
    Parser.t(failt | nfailt, nsucct)
    when succt: any, failt: any, nfailt: any, nsucct: any
  def bind(parser, bindf) when is_function(bindf, 1) do
    Parser.new fn state, failf, succf ->
      # We apply the original parser with an update success
      # continuation: this continuation (but the same failure continuation).
      Parser.apply parser, state, failf, fn(result, new_state) ->
        # We create the new parser, which is returned by `bindf`...
        new_parser = ensure_parser(bindf.(result))
        # ...and we apply it with the rest of the input and the original
        # success/failure continuation.
        Parser.apply(new_parser, new_state, failf, succf)
      end
    end
  end

  @doc """
  Returns a parser that parses with `parser1` or, if it fails, with `parser2`.

  If `parser1` succeeds, than its result is returned; otherwise, the input is
  parsed with `parser2` and the final result is `parser2`'s final result
  (whether it's a success or an error).

  ## Examples

      iex> import Compadre.Combinators
      iex> parser = plus(Compadre.Parsers.Binary.binary("foo"),
      ...>               Compadre.Parsers.Binary.binary("bar"))
      iex> Compadre.parse(parser, "foo...")
      {:ok, "foo", "..."}
      iex> Compadre.parse(parser, "bar...")
      {:ok, "bar", "..."}

  """
  @spec plus(Parser.t(any, succt1), Parser.t(failt, succt2)) ::
    Parser.t(failt, succt1 | succt2)
    when failt: any, succt1: any, succt2: any
  def plus(parser1, parser2) do
    # TODO make the error message mention both parsers?

    Parser.new fn state, failf, succf ->
      # The new failf ignores the failing reason, then applies `parser2` with
      # the new state but using the old position (basically retrying from where
      # we left).
      new_failf = fn(_reason, new_state) ->
        Parser.apply(parser2, %{new_state | pos: state.pos}, failf, succf)
      end

      Parser.apply(parser1, state, new_failf, succf)
    end
  end

  @doc """
  Tries to run `parser` without consuming any input.

  Returns the result of `parser` (weather it's a success or an error), but
  doesn't consume any input.

  ## Examples

      iex> import Compadre.Combinators
      iex> Compadre.parse(look_ahead(Compadre.Parsers.Binary.take_bytes(3)), "foobar")
      {:ok, "foo", "foobar"}

  """
  @spec look_ahead(Parser.t(failt, succt)) :: Parser.t(failt, succt)
    when failt: any, succt: any
  def look_ahead(parser) do
    Parser.new fn state, failf, succf ->
      Parser.apply parser, state, failf, fn(res, new_state) ->
        succf.(res, %{new_state | pos: state.pos})
      end
    end
  end

  @doc """
  Runs `parser` and returns its result alongside the input it consumed.

  ## Examples

      iex> import Compadre.Combinators
      iex> parser = with_consumed_input(Compadre.Parsers.Text.integer())
      iex> Compadre.parse(parser, "123...")
      {:ok, {123, "123"}, "..."}

  """
  @spec with_consumed_input(Parser.t(failt, succt)) ::
    Parser.t(failt, {succt, binary})
    when failt: any, succt: any
  def with_consumed_input(parser) do
    Parser.new fn state, failf, succf ->
      Parser.apply parser, state, failf, fn(res, new_state) ->
        pos      = state.pos
        new_pos  = new_state.pos
        consumed = :binary.part(new_state.input, pos, new_pos - pos)
        succf.({res, consumed}, new_state)
      end
    end
  end

  @doc """
  Runs `parser1` and then `parser2`, returning the result of `parser1`.

  The combined parser fails if any of the two combined parsers fail.

  ## Examples

      iex> import Compadre.Combinators
      iex> parser = followed_by(Compadre.Parsers.Text.integer(),
      ...>                      Compadre.Parsers.Binary.take_bytes(3))
      iex> Compadre.parse(parser, "123...rest")
      {:ok, 123, "rest"}

  """
  @spec followed_by(Parser.t(failt1, succt), Parser.t(failt2, any)) ::
    Parser.t(failt1 | failt2, succt)
    when failt1: any, failt2: any, succt: any
  def followed_by(parser1, parser2) do
    Parser.new fn state, failf, succf ->
      Parser.apply parser1, state, failf, fn res1, state1 ->
        failf2 = fn(err, state2) -> failf.(err, %{state2 | pos: state.pos}) end
        succf2 = fn(_res2, state2) -> succf.(res1, state2) end
        Parser.apply(parser2, state1, failf2, succf2)
      end
    end
  end

  @doc """
  Labels the `parser` to make its possible error message clearer.

  ## Examples

      iex> import Compadre.Combinators
      iex> parser = label(Compadre.Parsers.Text.integer(), "my parser")
      iex> Compadre.parse(parser, "foo")
      {:error, "parser 'my parser' failed: expected integer", "foo"}

  """
  @spec label(Parser.t(any, succt), binary) :: Parser.t(binary, succt)
    when succt: any
  def label(parser, name) when is_binary(name) do
    Parser.new fn state, failf, succf ->
      new_failf = fn(reason, new_state) ->
        new_reason = "parser '#{name}' failed: #{to_string(reason)}"
        failf.(new_reason, new_state)
      end

      Parser.apply(parser, state, new_failf, succf)
    end
  end

  @doc """
  Applies `parser1` and `parser2` in sequence, returning the result of
  `parser2`.

  If `parser1` fails, that failure is returned and `parser2` is not called at
  all.

  ## Examples

      iex> import Compadre.Combinators
      iex> parser = seq(Compadre.Parsers.Text.integer(),
      ...>              Compadre.Parsers.Binary.take_bytes(3))
      iex> Compadre.parse(parser, "123...rest")
      {:ok, "...", "rest"}

  """
  @spec seq(Parser.t(failt1, any), Parser.t(failt2, succt)) ::
    Parser.t(failt1 | failt2, succt)
    when failt1: any, failt2: any, succt: any
  def seq(parser1, parser2) do
    bind(parser1, fn _ -> parser2 end)
  end

  @doc """
  Applies `parser` and, if successful, returns the result of applying `fun` to
  its result. This can be achieved easily with `bind/2` and
  `Compadre.Parsers.fixed/1`, but it's a common pattern, hence it has its own
  function.

  ## Examples

      iex> import Compadre.Combinators
      iex> parser = transform(Compadre.Parsers.Binary.take_bytes(1), &String.to_integer/1)
      iex> Compadre.parse(parser, "1")
      {:ok, 1, ""}

  """
  @spec transform(Parser.t(failt, succt), (succt -> result)) ::
    Parser.t(failt, result)
    when failt: any, succt: any, result: any
  def transform(parser, fun) do
    Parser.new fn(state, failf, succf) ->
      Parser.apply parser, state, failf, fn(res, nstate) ->
        succf.(fun.(res), nstate)
      end
    end
  end

  @doc """
  Applies parser and returns its result in case of success, otherwise returns
  `default`.

  ## Examples

      iex> import Compadre.Combinators
      iex> parser = Compadre.Parsers.Binary.binary("foo") |> defaulting_to(nil)
      iex> Compadre.parse(parser, "foo")
      {:ok, "foo", ""}
      iex> Compadre.parse(parser, "bar")
      {:ok, nil, "bar"}

  """
  @spec defaulting_to(Parser.t(any, succt), val) :: Parser.t(any, succt | val)
    when succt: any, val: any
  def defaulting_to(parser, default) do
    plus(parser, Parsers.fixed(default))
  end

  @doc """
  Tries the parsers in `parsers` one by one, stopping as soon as one succeeds.

  The return value is the return value of the first parser that succeeds.

  Raises a `FunctionClauseError` exception if `parsers` is an empty list.
  """
  # TODO should we return the error of the last parser if all error out?
  @spec one_of([Parser.t(any, any)]) :: Parser.t(any, any)
  def one_of(parsers) when is_list(parsers) and parsers != [] do
    Enum.reduce(parsers, &plus(&2, &1))
  end

  @doc """
  Parses repeatedly with `parser` and returns the results in a list.

  This combinator returns a parser that never fails: if `parser` never succeeds,
  it simply returns an empty list.

  Never fails.

  ## Examples

      iex> import Compadre.Combinators
      iex> parser = many(Compadre.Parsers.Binary.binary("foo"))
      iex> Compadre.parse(parser, "foofoo...")
      {:ok, ["foo", "foo"], "..."}
      iex> Compadre.parse(parser, "bar")
      {:ok, [], "bar"}

  """
  @spec many(Parser.t(any, succt)) :: Parser.t(any, [succt]) when succt: any
  def many(parser) do
    Parser.new(&do_many(&1, &2, &3, parser, [])) |> transform(&Enum.reverse/1)
  end

  defp do_many(state, failf, succf, parser, acc) do
    # This "native" implementation should be quite faster than its combined
    # alternative.
    nfailf = fn(_, nstate) -> succf.(acc, nstate) end
    nsuccf = fn(res, nstate) -> do_many(nstate, failf, succf, parser, [res|acc]) end
    Parser.apply(parser, state, nfailf, nsuccf)
  end

  @doc """
  Works exactly like `many/1`, but fails if `parser` never succeeds.
  """
  @spec one_or_more(Parser.t(any, succt)) :: Parser.t(any, [succt, ...])
    when succt: any
  def one_or_more(parser) do
    # There has to be one, so we first parse with `parser`...
    bind parser, fn res ->
      # ...and if it succeeds, we parse with `many(parser)` and then add the
      # result of the first parse.
      many(parser) |> transform(fn rest -> [res|rest] end)
    end
  end

  @doc """
  Parses zero or more times with `parser`, stopping when `end_parser` succeeds.

  **Warning**: this combinator tends to be slow as it tries `end_parser` every
  time after each parsing with `parser`.

  The resulting parser stops also when `parser` fails. If `parser` never
  succeeds, this parser just returns an empty list instead of failing.

  Never fails.

  ## Examples

      iex> import Compadre.Combinators
      iex> parser = many_until(Compadre.Parsers.Binary.binary("foo"),
      ...>                     Compadre.Parsers.Binary.binary("stop"))
      iex> Compadre.parse(parser, "foofoo...")
      {:ok, ["foo", "foo"], "..."}
      iex> Compadre.parse(parser, "bar")
      {:ok, [], "bar"}

  """
  @spec many_until(Parser.t(any, succt), Parser.t(any, any)) ::
    Parser.t(any, [succt])
    when succt: any
  def many_until(parser, end_parser) do
    do_many_until(parser, end_parser, []) |> transform(&Enum.reverse/1)
  end

  defp do_many_until(parser, end_parser, acc) do
    # `p` is a parser that calls do_many_until recursively and prepends its
    # result to the result of the recursive parse.
    p = bind(parser, &do_many_until(parser, end_parser, [&1|acc]))
    pacc = Parsers.fixed(acc)

    # One of these happens
    #  * `end_parser` succeeds and we return the accumulator
    #  * `end_parser` fails, and we try `p`: if `p` fails, we return the
    #    accumulator, otherwise we recurse
    seq(end_parser, pacc)
    |> plus(p)
    |> plus(pacc)
  end

  @doc """
  Parses zero or more times with `parser`, separating the results with
  `separator`.

  The results of each parse with `separator` are simply discarded.
  If `parser` never succeeds, then an empty list is returned.

  Never fails.

  ## Examples

      iex> import Compadre.Combinators
      iex> parser = sep_by(Compadre.Parsers.Text.integer(),
      ...>                 Compadre.Parsers.Binary.binary(", "))
      iex> Compadre.parse(parser, "1, 2, 3...")
      {:ok, [1, 2, 3], "..."}
      iex> Compadre.parse(parser, "1...")
      {:ok, [1], "..."}
      iex> Compadre.parse(parser, "foo")
      {:ok, [], "foo"}

  """
  @spec sep_by(Parser.t(any, succt), Parser.t(any, any)) ::
    Parser.t(any, [succt, ...])
    when succt: any
  def sep_by(parser, separator) do
    plus(sep_by_at_least_one(parser, separator), Parsers.fixed([]))
  end

  @doc """
  Works exactly like `sep_by/2`, but `parser` must succeed at least once.
  """
  @spec sep_by_at_least_one(Parser.t(any, succt), Parser.t(any, any)) ::
    Parser.t(any, [succt, ...])
    when succt: any
  def sep_by_at_least_one(parser, separator) do
    bind parser, fn first_res ->
      sep_then_p = seq(separator, parser)
      transform many(sep_then_p), &[first_res|&1]
    end
  end

  @doc """
  Parses `n` occurrences of `parser` and returns the results in a list.

  If `parser` fails before being applied `n` times, this parser fails.

  ## Examples

      iex> import Compadre.Combinators
      iex> Compadre.parse(count(Compadre.Parsers.Binary.binary("foo"), 2), "foofoo")
      {:ok, ["foo", "foo"], ""}

  """
  @spec count(Parser.t(any, succt), pos_integer) :: Parser.t(any, [succt, ...])
    when succt: any
  def count(parser, n) when is_integer(n) and n >= 0 do
    parser |> List.duplicate(n) |> sequence()
  end

  @doc false
  @spec sequence([Parser.t(any, succt)]) :: Parser.t(any, [any])
    when succt: any
  def sequence(parsers) do
    parsers
    |> reduce(Parsers.fixed([]), &[&1|&2])
    |> transform(&Enum.reverse/1)
  end

  # Reduces over a list of parsers. If any of the parsers fail, the failing
  # result is returned right away. Otherwise, `fun` is called with the result of
  # each parser and the accumulator (which starts at `acc0`).
  #
  #     p = reduce([binary("foo"), binary("bar")], "", fn str, acc -> str <> acc end)
  #     Compadre.parse(p, "foobar")
  #     #=> "foobar"
  defp reduce(parsers, acc0, fun) do
    Enum.reduce parsers, acc0, fn parser, acc ->
      bind acc, fn acc_res ->
        bind parser, fn parser_res ->
          Parsers.fixed(fun.(parser_res, acc_res))
        end
      end
    end
  end

  # Ensure that the argument is a parser; if it is, returns it, otherwise raises
  # an `ArgumentError`.
  defp ensure_parser(%Parser{} = new_parser) do
    new_parser
  end

  defp ensure_parser(other) do
    raise ArgumentError, "the second argument passed to bind/2" <>
                         " must be a function that returns a" <>
                         " %Compadre.Parser{}, got: " <> (inspect other)
  end
end
