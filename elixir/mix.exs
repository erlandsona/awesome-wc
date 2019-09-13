defmodule Elixir.MixProject do
  use Mix.Project

  def project do
    [
      app: :elixir,
      version: "0.1.0",
      elixir: "~> 1.8",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:algae, "~> 1.2"},
      {:destructure, "~> 0.2.3"},
      {:dialyxir, "~> 1.0.0-rc.6", only: [:dev], runtime: false},
      {:flow, "~> 0.14"},
      {:type_class, "~> 1.1"},
      {:witchcraft, "~> 1.0"}
    ]
  end
end
