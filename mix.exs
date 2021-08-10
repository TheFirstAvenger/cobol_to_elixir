defmodule CobolToElixir.MixProject do
  use Mix.Project

  def project do
    [
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test,
        coverage_report: :test
      ],
      app: :cobol_to_elixir,
      version: "0.1.0",
      elixir: "~> 1.12",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases(),
      source_url: "https://github.com/TheFirstAvenger/cobol_to_elixir",
      package: package()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:excoveralls, "~> 0.14.0", only: :test},
      {:mix_test_watch, "~> 1.0", only: :dev, runtime: false},
      {:ex_doc, "~> 0.25.1"}
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end

  defp aliases do
    [
      coverage_report: [&coverage_report/1]
    ]
  end

  defp package do
    [
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/TheFirstAvenger/cobol_to_elixir"}
    ]
  end

  defp coverage_report(_) do
    Mix.Task.run("coveralls.html")

    open_cmd =
      case :os.type() do
        {:win32, _} ->
          "start"

        {:unix, :darwin} ->
          "open"

        {:unix, _} ->
          "xdg-open"
      end

    System.cmd(open_cmd, ["cover/excoveralls.html"])
  end
end
