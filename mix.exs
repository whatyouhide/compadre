defmodule Compadre.Mixfile do
  use Mix.Project

  def project do
    [app: :compadre,
     version: "0.0.1",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: []]
  end

  def application do
    [applications: [:logger]]
  end
end
