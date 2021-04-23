defmodule CobolToElixir.Util do
  def execute_cobol_code!(cobol, input \\ []) do
    tmp_folder = generate_tmp_folder()

    try do
      case compile_cobol(cobol, tmp_folder) do
        {"", 0} -> :ok
        {output, 1} -> raise "Error compiling cobol:\n#{output}"
      end

      port = Port.open({:spawn, "#{tmp_folder}/cobol"}, [:binary, :exit_status, :stderr_to_stdout])

      send_cobol_input(port, input)

      # :timer.sleep(1000)
      # Port.close(port)
      output = get_cobol_output(port) |> Enum.reverse() |> Enum.join("")
      # {output, 0} = System.cmd("./#{tmp_folder}/cobol", [], stderr_to_stdout: true)
      output
    after
      File.rm_rf!(tmp_folder)
    end
  end

  def compile_cobol(code, tmp_folder) do
    cobol_path = Path.join(tmp_folder, "cobol.cob")
    cobol_executable_path = Path.join(tmp_folder, "cobol")
    File.write(cobol_path, code)
    System.cmd("cobc", ["-x", "-o", cobol_executable_path, cobol_path], stderr_to_stdout: true)
  end

  def generate_tmp_folder do
    tmp_folder = Path.relative_to_cwd("test/temp_delete_contents/#{Enum.random(1000..1_000_000_000_000)}")
    File.mkdir_p!(tmp_folder)
    tmp_folder
  end

  defp get_cobol_output(port, acc \\ []) do
    # coveralls-ignore-start
    receive do
      {^port, {:data, output}} -> get_cobol_output(port, [output | acc])
      {^port, {:closed}} -> acc
      {^port, {:exit_status, 0}} -> acc
      other -> raise "got unexpected port response: #{inspect(other)}"
    end

    # coveralls-ignore-end
  end

  defp send_cobol_input(_port, []), do: :ok

  defp send_cobol_input(port, [{timeout, input} | tail]) do
    :timer.sleep(timeout)
    Port.command(port, "#{input}\n")
    send_cobol_input(port, tail)
  end
end
