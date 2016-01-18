defmodule Compadre.CoreTest do
  use ExUnit.Case

  import Compadre.Core
  alias Compadre.Core.{Success, Failure, Partial}

  test "feed/2: success results" do
    success = %Success{result: :successful, rest: "foo"}
    assert %Success{result: :successful, rest: "foobar"} = feed(success, "bar")
  end

  test "feed/2: failure results" do
    failure = %Failure{reason: :failed, rest: "foo"}
    assert %Failure{reason: :failed, rest: "foobar"} = feed(failure, "bar")
  end

  test "feed/2: partial results" do
    partial = %Partial{cont: fn(data) -> byte_size(data) end}
    assert feed(partial, "foø") == 4
  end

  test "wrap/2: finished result" do
    assert wrap(%Success{result: "foo"}, &byte_size(&1.result)) == 3
    assert wrap(%Failure{reason: "oops"}, &byte_size(&1.reason)) == 4
  end

  test "wrap/2: partial result" do
    partial = %Partial{cont: fn(data) -> %Success{result: data} end}
    assert %Partial{} = new_partial = wrap(partial, &String.to_atom(&1.result))
    assert feed(new_partial, "inception") == :inception
  end

  test "bind/2: finished result" do
    res =
      %Success{}
      |> bind(fn s -> send(self, :success); s end)
      |> bind(fn _ -> send(self, :failure); %Failure{} end)
      |> bind(fn _ -> send(self, :after_failure) end)

    assert %Failure{} = res
    assert_received :success
    assert_received :failure
    refute_received :after_failure
  end

  test "bind/2: partial result, where it behaves like wrap/2" do
    partial = %Partial{cont: fn(_) -> %Failure{} end}
    assert %Partial{} = new_partial = bind(partial, fn _ -> send(self, :foo) end)
    assert %Failure{} = feed(new_partial, "finish")
    refute_received :foo
  end
end
