defmodule Compadre.Combinators do
  alias Compadre.Parser
  alias Compadre.Parsers

  ## Core parser combinators ##

  # These combinators do not depend on other combinators; they usually create a
  # new parser from scratch, so you'll probably see them having a call to
  # `Parser.new/1`.

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

  @spec plus(Parser.t(any, succt1), Parser.t(failt, succt2)) ::
    Parser.t(failt, succt1 | succt2)
    when failt: any, succt1: any, succt2: any
  def plus(parser1, parser2) do
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

  @spec look_ahead(Parser.t(failt, succt)) :: Parser.t(failt, succt)
    when failt: any, succt: any
  def look_ahead(parser) do
    Parser.new fn state, failf, succf ->
      Parser.apply parser, state, failf, fn(res, new_state) ->
        succf.(res, %{new_state | pos: state.pos})
      end
    end
  end

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

  @spec followed_by(Parser.t(failt1, succt), Parser.t(failt2, any)) ::
    Parser.t(failt1 | failt2, succt)
    when failt1: any, failt2: any, succt: any
  def followed_by(parser1, parser2) do
    Parser.new fn state, failf, succf ->
      Parser.apply parser1, state, failf, fn res1, state1 ->
        failf2 = fn err, state2 ->
          failf.(err, %{state2 | pos: state.pos})
        end
        succf2 = fn _res2, state2 ->
          succf.(res1, state2)
        end
        Parser.apply(parser2, state1, failf2, succf2)
      end
    end
  end

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

  @spec seq(Parser.t(failt1, any), Parser.t(failt2, succt)) ::
    Parser.t(failt1 | failt2, succt)
    when failt1: any, failt2: any, succt: any
  def seq(parser1, parser2) do
    bind(parser1, fn _ -> parser2 end)
  end

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

  @spec defaulting_to(Parser.t(any, succt), val) :: Parser.t(any, succt | val)
    when succt: any, val: any
  def defaulting_to(parser, default) do
    plus(parser, Parsers.fixed(default))
  end

  @spec one_of([Parser.t(any, any)]) :: Parser.t(any, any)
  def one_of(parsers) do
    Enum.reduce(parsers, &plus(&2, &1))
  end

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

  @spec many(Parser.t(any, succt)) :: Parser.t(any, [succt]) when succt: any
  def many(parser) do
    Parser.new(&do_many(&1, &2, &3, parser, [])) |> transform(&Enum.reverse/1)
  end

  defp do_many(state, failf, succf, parser, acc) do
    nfailf = fn(_, nstate) -> succf.(acc, nstate) end
    nsuccf = fn(res, nstate) -> do_many(nstate, failf, succf, parser, [res|acc]) end
    Parser.apply(parser, state, nfailf, nsuccf)
  end

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

  @spec sep_by_at_least_one(Parser.t(any, succt), Parser.t(any, any)) ::
    Parser.t(any, [succt, ...])
    when succt: any
  def sep_by_at_least_one(parser, separator) do
    bind parser, fn first_res ->
      sep_then_p = seq(separator, parser)
      bind many(sep_then_p), fn other_results ->
        Parsers.fixed([first_res|other_results])
      end
    end
  end

  @spec sep_by(Parser.t(any, succt), Parser.t(any, any)) ::
    Parser.t(any, [succt, ...])
    when succt: any
  def sep_by(parser, separator) do
    plus(sep_by_at_least_one(parser, separator), Parsers.fixed([]))
  end

  @spec count(Parser.t(any, succt), pos_integer) :: Parser.t(any, [succt, ...])
    when succt: any
  def count(parser, n) when is_integer(n) and n > 0 do
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
