defmodule Compadre.ParsersTest do
  use ExUnit.Case

  import Compadre.Parsers

  test "fixed/1" do
    p = fixed(:hello_world)
    assert Compadre.parse(p, "foo") == {:ok, :hello_world, "foo"}
  end

  test "demand_input/0" do
    assert Compadre.parse(demand_input(), "foo") == {:ok, nil, "foo"}

    assert {:partial, _} = res = Compadre.parse(demand_input(), "")
    assert Compadre.feed(res, "foo") == {:ok, nil, "foo"}
  end

  test "advance/1" do
    assert Compadre.parse(advance(1), "foo") == {:ok, nil, "oo"}
    assert {:ok, nil, "world"} =
      advance(6)
      |> Compadre.parse("he")
      |> Compadre.feed("ll")
      |> Compadre.feed("o world")
  end

  test "with_consumed_input/1" do
    parser = with_consumed_input(advance(3))
    assert {:ok, {nil, "foo"}, "rest"} = Compadre.parse(parser, "foorest")

    assert {:ok, {nil, "foo"}, "rest"} =
      parser
      |> Compadre.parse("f")
      |> Compadre.feed("o")
      |> Compadre.feed("orest")
  end

  test "take_bytes/1" do
    assert {:ok, "hello", " world!"} =
      take_bytes(5)
      |> Compadre.parse("he")
      |> Compadre.feed("ll")
      |> Compadre.feed("o world!")
  end

  test "peek_bytes/1" do
    assert {:ok, "hello", "hello world!"} =
      peek_bytes(5)
      |> Compadre.parse("hel")
      |> Compadre.feed("lo world!")
  end

  test "peek_byte/0" do
    assert {:ok, 1, <<2, 3>>} = Compadre.parse(peek_byte(), <<1, 2, 3>>)
  end
end
