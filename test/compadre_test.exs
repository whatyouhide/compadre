defmodule CompadreTest do
  use ExUnit.Case, async: true
  doctest Compadre

  test "Inspect protocol for Compadre.Cont" do
    cont = Compadre.mkcont(&{:ok, &1, ""})
    assert String.starts_with?(inspect(cont), "#Compadre.Cont<#Function<")
  end
end
