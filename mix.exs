defmodule OpencensusErlangPrometheus.Mixfile do
  use Mix.Project

  def project do
    [app: :opencensus_erlang_prometheus,
     version: "0.1.0",
     elixir: "~> 1.4",
     deps: deps(),
     description: description(),
     package: package(),
     erlc_options: [:warnings_as_errors,
                    :warn_export_vars,
                    :warn_shadow_vars,
                    :warn_obsolete_guard],
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
     links: %{"GitHub" => "https://github.com/deadtrickster/opencensus-erlang-prometheus"},
     files: ["priv", "src", "README.md", "rebar.config"]]
  end

  defp deps do
    [{:prometheus, "~> 4.1"},
     {:opencensus, "~> 0.3"},

     # test
     {:credo, "~> 0.8.7", only: [:dev, :test]}]
  end
end
