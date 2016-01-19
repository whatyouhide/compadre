defmodule Compadre.Combinators do
  alias Compadre.Parser
  alias Compadre.Parsers
  alias Compadre.Core.Failure

  @doc """
  TODO
  """
  # TODO better spec
  @spec bind(Parser.t, (any -> Parser.t)) :: Parser.t
  def bind(parser, bindf) when is_function(bindf, 1) do
    Parser.new fn input, pos, failf, succf ->
      # We apply the original parser with an update success
      # continuation: this continuation (but the same failure continuation).
      Parser.apply parser, input, pos, failf, fn(succ, new_input, new_pos) ->
        # We create the new parser, which is returned by `bindf`...
        new_parser = ensure_parser(bindf.(succ.result))
        # ...and we apply it with the rest of the input and the original
        # success/failure continuation.
        Parser.apply(new_parser, new_input, new_pos, failf, succf)
      end
    end
  end

  defp ensure_parser(%Parser{} = new_parser) do
    new_parser
  end

  defp ensure_parser(other) do
    raise ArgumentError, "the second argument passed to bind/2" <>
                         " must be a function that returns a" <>
                         " %Compadre.Parser{}, got: " <> (inspect other)
  end

  @doc """
  TODO
  """
  # TODO better spec
  @spec seq(Parser.t, Parser.t) :: Parser.t
  def seq(parser1, parser2) do
    bind(parser1, fn _ -> parser2 end)
  end

  @doc """
  TODO
  """
  # TODO spec
  # TODO test this
  def rseq(parser1, parser2) do
    bind(parser1, fn res -> seq(parser2, Parsers.fixed(res)) end)
  end

  @doc """
  TODO
  """
  # TODO better spec
  @spec plus(Parser.t, Parser.t) :: Parser.t
  def plus(parser1, parser2) do
    Parser.new fn input, pos, failf, succf ->
      new_failf = fn(_fail, new_input, _fpos) ->
        Parser.apply(parser2, new_input, pos, failf, succf)
      end

      Parser.apply(parser1, input, pos, new_failf, succf)
    end
  end

  @doc """
  TODO
  """
  @spec one_of([Parser.t]) :: Parser.t
  def one_of(parsers) do
    Enum.reduce(parsers, &plus(&2, &1))
  end

  @doc """
  TODO
  """
  # TODO spec
  def defaulting_to(parser, default) do
    plus(parser, Parsers.fixed(default))
  end

  @doc """
  TODO
  """
  # TODO spec
  def look_ahead(parser) do
    Parser.new fn input, pos, failf, succf ->
      Parser.apply parser, input, pos, failf, fn(succ, new_input, _new_pos) ->
        succf.(succ, new_input, pos)
      end
    end
  end

  @doc """
  TODO
  """
  # TODO spec
  def count(parser, n) when n > 0 do
    parser |> List.duplicate(n) |> sequence()
  end

  @doc """
  TODO
  """
  # TODO spec
  def many(parser) do
    do_many(parser, []) |> bind(&Parsers.fixed(Enum.reverse(&1)))
  end

  defp do_many(parser, acc) do
    p = bind parser, fn res -> do_many(parser, [res|acc]) end
    plus(p, Parsers.fixed(acc))
  end

  @doc """
  TODO
  """
  # TODO spec
  def one_or_more(parser) do
    bind parser, fn res ->
      bind(many(parser), fn rest -> Parsers.fixed([res|rest]) end)
    end
  end

  @doc false
  def sequence(parsers) do
    # This is a parser that returns the result we're interested in, but
    # reversed.
    p = reduce(parsers, Parsers.fixed([]), &[&1|&2])
    bind(p, fn results -> Parsers.fixed(Enum.reverse(results)) end)
  end

  @doc """
  TODO
  """
  # TODO spec
  def label(parser, name) do
    Parser.new fn input, pos, failf, succf ->
      new_failf = fn(%Failure{reason: reason} = fail, input, pos) ->
        new_reason = "parser '#{name}' failed: #{reason}"
        failf.(%{fail | reason: new_reason}, input, pos)
      end

      Parser.apply(parser, input, pos, new_failf, succf)
    end
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

end
