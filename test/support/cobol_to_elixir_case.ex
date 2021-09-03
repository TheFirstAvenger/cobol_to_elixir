defmodule CobolToElixirCase do
  import ExUnit.CaptureIO

  alias CobolToElixir.Util

  require ExUnit.Assertions
  require Logger

  defmacro __using__([]) do
    quote do
      use ExUnit.Case
      import CobolToElixirCase
    end
  end

  @doc """
  Compiles the cobol code and ensures there are no errors/warnings
  """
  def validate_cobol_code(cobol) do
    tmp_folder = Util.generate_tmp_folder()

    try do
      case Util.compile_cobol(cobol, tmp_folder) do
        {"", 0} -> :ok
        {output, 1} -> raise "Error compiling cobol:\n#{output}"
      end
    after
      File.rm_rf!(tmp_folder)
    end
  end

  @doc """
  Compiles and then runs the given cobol code with the optional list of inputs being sent.
  Input is a list of timeout/command values, e.g. [{1000, "John"}, {500, "Mike"}]
  would send "John" after 1 second, then "Mike" after another half a second.

  Returns map containing program output and any new files.
  """
  def execute_cobol_code!(cobol, input \\ []) do
    Util.execute_cobol_code!(cobol, input)
  end

  @doc """
  Compiles and loads the given code into memory, runs the module, and unloads the module.
  Accepts list of input same as `execute_cobol_code!/2`.

  Returns map containing program output and any new files.
  """
  def execute_elixir_code(str, module, input, tmp_folder \\ nil) do
    log =
      capture_io(:stderr, fn ->
        Code.compile_string(str)
        {:module, ^module} = Code.ensure_loaded(module)
      end)

    if log != "" do
      Logger.info("compiler warning compiling Elixir: #{log}")
    end

    io =
      capture_io(fn ->
        Enum.each(input, &send(self(), {:input, elem(&1, 1)}))
        apply(module, :main, [])
      end)

    true = :code.delete(module)
    :code.purge(module)

    files =
      case tmp_folder do
        nil -> nil
        _ -> Util.get_new_files([], File.ls!(tmp_folder), tmp_folder)
      end

    %{output: io, files: files}
  end

  @doc """
  This is the one-stop-shop for ensuring cobol text acts the same as the converted elixir version.
  Given cobol text, a module name, expected output, and a list of input, it will
  1) Compile and execute the cobol code, sending the specified inputs
  2) Run CobolToElixir and convert the cobol code to Elixir
  3) Load and run the Elixir code, sending the specified inputs
  4) Assert that the output of both the cobol and Elixir programs matches the specififed output
  """
  def assert_output_equal(cobol_text, module, opts \\ []) when is_list(opts) do
    output = Keyword.get(opts, :output, "")
    input = Keyword.get(opts, :input, [])
    %{output: cobol_output, files: cobol_files} = execute_cobol_code!(cobol_text, input)

    if !is_nil(output) do
      ExUnit.Assertions.assert(cobol_output == output)
    end

    tmp_folder = Util.generate_tmp_folder()
    {:ok, elixir_text} = CobolToElixir.convert(cobol_text, accept_via_message: true, io_dir: tmp_folder)

    try do
      %{output: elixir_output, files: elixir_files} = execute_elixir_code(elixir_text, module, input, tmp_folder)

      ExUnit.Assertions.assert(cobol_output == elixir_output)
      ExUnit.Assertions.assert(cobol_files == elixir_files)
    after
      File.rm_rf!(tmp_folder)
    end
  end
end
