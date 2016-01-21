ExUnit.start()

defmodule Compadre.TestHelper do
  alias Compadre.Parser
  alias Compadre.State

  def parse_test(%Parser{} = p, input, opts \\ []) when is_list(opts) do
    parser = Parser.new fn state, failf, succf ->
      state = %State{pos: opts[:pos] || state.pos,
                     complete?: opts[:complete?] || state.complete?,
                     input: state.input}
      Parser.apply(p, state, failf, succf)
    end

    Compadre.parse(parser, input)
  end
end
