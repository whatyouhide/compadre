ExUnit.start()

defmodule Compadre.TestHelper do
  alias Compadre.Parser
  alias Compadre.State

  def parse_test(%Parser{} = p, input, opts \\ []) when is_list(opts) do
    parser = Parser.new fn state, failf, succf ->
      state = %State{pos: opts[:pos] || state.pos,
                     complete?: opts[:complete?] || false,
                     input: state.input}
      Parser.apply(p, state, failf, succf)
    end

    Compadre.parse(parser, input)
  end

  defmacro assert_parse_result(parser, input, result) do
    quote do
      parser = unquote(parser)
      res_code = unquote(Macro.to_string(result))

      {input, eoi?} =
        case unquote(input) do
          {i, :eoi} -> {i, true}
          i         -> {i, false}
        end

      for {inp, seq} <- Compadre.TestHelper.possible_parse_seqs(parser, input, eoi?) do
        case seq do
          unquote(result) ->
            :ok
          res ->
            raise """
            Match failed with input fed as #{inspect inp}.
            Expected: #{res_code}
            To match: #{inspect res}
            """
        end
      end
    end
  end

  # Returns a list of `{input, result}` where input is a splitting combination
  # of the `input` arg, and `result` is the result of parsing that combination
  # with `parser`. If `eoi?` is true, also adds eoi to each result.
  def possible_parse_seqs(parser, input, eoi?) do
    :random.seed(:os.timestamp)
    res =
      input
      |> split_combinations()
      |> Enum.flat_map(&parse_split_input(parser, &1))

    if eoi? do
      after_eoiing = Enum.map(res, fn {inp, res} -> {inp, Compadre.eoi(res)} end)
      # We add a call where the input is complete before even starting to parse.
      already_complete = {{:already_complete, input},
                          parse_test(parser, input, complete?: true)}
      after_eoiing ++ [already_complete]
    else
      res
    end
  end

  # Parses the given split input (a list of chunks) with the given parser.
  # Returns `[{input, pos}, {input, nopos}]`, where `pos` is the parsing when
  # the first chunk isn't the start of the input (i.e., pos != 0) and `nopos` is
  # when the first chunk is the start of the input.
  defp parse_split_input(parser, input) do
    input = if input == [], do: [""], else: input
    [first_chunk|rest] = input
    nopos = parse_test(parser, first_chunk)
    pos   = parse_test(parser, "start " <> first_chunk, pos: 6)
    [{input, feed_pieces(nopos, rest)}, {input, feed_pieces(pos, rest)}]
  end

  # Calls Compadre.feed/1 multiple times over `parse_res`, each time with one of
  # the given `pieces`.
  defp feed_pieces(parse_res, pieces) do
    Enum.reduce(pieces, parse_res, &Compadre.feed(&2, &1))
  end

  # Returns the given input, split in many different ways (using
  # random_subsequences/1).
  defp split_combinations(input) do
    1..200
    |> Enum.map(fn _ -> random_subsequences(input) end)
    |> Enum.uniq()
  end

  # Returns subsequences of `str`, each with a random length (e.g. "foo" ->
  # ["fo", "o"]).
  defp random_subsequences("") do
    []
  end

  defp random_subsequences(string) do
    string_size = byte_size(string)
    bytes_to_take = Enum.random(1..string_size)
    first = :binary.part(string, 0, bytes_to_take)
    rest = :binary.part(string, bytes_to_take, string_size - bytes_to_take)
    [first | random_subsequences(rest)]
  end
end
