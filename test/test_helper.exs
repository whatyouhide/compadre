ExUnit.start()

defmodule Compadre.TestHelper do
  alias Compadre.Parser

  def parse_test(%Parser{} = p, input, opts \\ []) when is_list(opts) do
    position = opts[:pos] || 0

    parser = Parser.new fn input, _pos, failf, succf ->
      Parser.apply(p, input, position, failf, succf)
    end

    Compadre.parse(parser, input)
  end
end
