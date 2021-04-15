defmodule CobolToElixirCase do
  import ExUnit.CaptureIO

  require ExUnit.Assertions

  defmacro __using__([]) do
    quote do
      use ExUnit.Case
      import CobolToElixirCase
    end
  end

  def execute_cobol_code(cobol, input \\ []) do
    CobolToElixir.Util.execute_cobol_code(cobol, input)
  end

  def execute_elixir_code(str, module, input) do
    Code.compile_string(str)

    {:module, ^module} = Code.ensure_loaded(module)

    io =
      capture_io(fn ->
        Enum.each(input, &send(self(), {:input, elem(&1, 1)}))
        apply(module, :main, [])
      end)

    :code.delete(module)
    :code.purge(module)

    io
  end

  def assert_output_equal(cobol_text, module, output \\ "", input \\ []) do
    cobol_output = execute_cobol_code(cobol_text, input)
    {:ok, elixir_text} = CobolToElixir.convert(cobol_text, accept_via_message: true)
    elixir_output = execute_elixir_code(elixir_text, module, input)
    ExUnit.Assertions.assert(cobol_output == elixir_output)

    if !is_nil(output) do
      ExUnit.Assertions.assert(cobol_output == output)
    end
  end
end
