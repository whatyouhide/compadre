defmodule Compadre.Combinators do
  alias Compadre.Parser
  alias Compadre.Parsers

  ## Core parser combinators ##

  # These combinators do not depend on other combinators; they usually create a
  # new parser from scratch, so you'll probably see them having a call to
  # `Parser.new/1`.

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

  def look_ahead(parser) do
    Parser.new fn state, failf, succf ->
      Parser.apply parser, state, failf, fn(res, new_state) ->
        succf.(res, %{new_state | pos: state.pos})
      end
    end
  end

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

  def label(parser, name) do
    Parser.new fn state, failf, succf ->
      new_failf = fn(reason, new_state) ->
        new_reason = "parser '#{name}' failed: #{reason}"
        failf.(new_reason, new_state)
      end

      Parser.apply(parser, state, new_failf, succf)
    end
  end

  ## Combinators that are built upon the "core" combinators ##

  def seq(parser1, parser2) do
    bind(parser1, fn _ -> parser2 end)
  end

  def transform(parser, fun) do
    bind(parser, fn res -> Parsers.fixed(fun.(res)) end)
  end

  def defaulting_to(parser, default) do
    plus(parser, Parsers.fixed(default))
  end

  def one_of(parsers) do
    Enum.reduce(parsers, &plus(&2, &1))
  end

  def many_until(parser, end_parser) do
    do_many_until(parser, end_parser, []) |> transform(&Enum.reverse/1)
  end

  def do_many_until(parser, end_parser, acc \\ []) do
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

  def many(parser) do
    many_until(parser, Parsers.flunk(:never_reached))
  end

  def take_until(end_parser) do
    take_byte = followed_by(Parsers.peek_byte(), Parsers.advance(1))
    many_until(take_byte, end_parser) |> transform(&List.to_string/1)
  end

  def one_or_more(parser) do
    # There has to be one, so we first parse with `parser`...
    bind parser, fn res ->
      # ...and if it succeeds, we parse with `many(parser)` and then add the
      # result of the first parse.
      bind(many(parser), fn rest -> Parsers.fixed([res|rest]) end)
    end
  end

  def sep_by_at_least_one(parser, separator) do
    bind parser, fn first_res ->
      sep_then_p = seq(separator, parser)
      bind many(sep_then_p), fn other_results ->
        Parsers.fixed([first_res|other_results])
      end
    end
  end

  def sep_by(parser, separator) do
    plus(sep_by_at_least_one(parser, separator), Parsers.fixed([]))
  end

  def count(parser, n) when n > 0 do
    parser |> List.duplicate(n) |> sequence()
  end

  @doc false
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
