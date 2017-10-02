defmodule OpencensusErlangPrometheus.Mixfile do
  use Mix.Project

  def project do
    [app: :opencensus_erlang_prometheus,
     version: "0.0.1",
     elixir: "~> 1.4",
     deps: deps(),
     description: description(),
     package: package(),
     erlc_options: [:warnings_as_errors],
     elixirc_options: [warnings_as_errors: true],
     test_coverage: [tool: ExCoveralls],
     preferred_cli_env: ["coveralls": :test, "coveralls.html": :test]]
  end

  defp description do
    """
    opencensus_erlang_prometheus
    """
  end

  defp package do
    [maintainers: ["Ilya Khaprov"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/deadtrickster/opencensus_erlang_prometheus"},
     files: ["priv", "src", "README.md", "rebar.config"]]
  end

  defp deps do
    [{:prometheus, "~> 3.4"},
     {:credo, "~> 0.8.7", only: [:dev, :test]},
     {:excoveralls, "~> 0.7.3", only: [:test]}]
  end
end
