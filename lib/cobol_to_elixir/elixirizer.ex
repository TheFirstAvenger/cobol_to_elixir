defmodule CobolToElixir.Elixirizer do
  alias CobolToElixir.Parsed
  alias CobolToElixir.Parsed.Variable

  require Logger

  def elixirize(%Parsed{program_id: program_id} = parsed, opts \\ []) do
    namespace = Keyword.get(opts, :namespace, "ElixirFromCobol")
    accept_via_message = Keyword.get(opts, :accept_via_message, false)
    module_name = program_id_to_module_name(program_id)

    lines =
      [
        ~s|defmodule #{namespace}.#{module_name} do|,
        ~s|  @moduledoc """|,
        ~s|  author: #{parsed.author || "n/a"}|,
        ~s|  date written: #{parsed.date_written || "n/a"}|,
        ~s|  """|,
        ~s||,
        ~s|  def main do|,
        ~s|    try do|,
        ~s|      do_main()|,
        ~s|    catch|,
        ~s|      :stop_run -> :stop_run|,
        ~s|    end|,
        ~s|  end|,
        ~s||
      ] ++
        do_main_function(parsed) ++
        do_paragraphs(parsed) ++
        [
          ~s||,
          ~s|  defp do_accept() do|
        ] ++
        accept_lines(accept_via_message) ++
        [
          ~s|  end|,
          ~s||,
          ~s/  def accept({:str, _, _} = pic), do: do_accept() |> format(pic)/,
          ~s||,
          ~s|  def accept(_), do: do_accept()|,
          ~s||,
          ~s/  def format(str, {:str, _, length}), do: str |> String.slice(0..(length - 1)) |> String.pad_trailing(length)/,
          ~s||,
          ~s|end|
        ]

    {:ok, Enum.join(lines, "\n")}
  end

  defp accept_lines(false), do: [~s/      "" |> IO.gets() |> String.trim_trailing("\\n")/]

  defp accept_lines(true) do
    [
      ~s/      receive do/,
      ~s/        {:input, input} -> input/,
      ~s/      end/
    ]
  end

  defp program_id_to_module_name(program_id) do
    String.capitalize(program_id)
  end

  def do_main_function(%Parsed{} = parsed) do
    [
      ~s|  def do_main do|
    ] ++
      variables(parsed) ++
      procedure(parsed) ++
      [
        ~s|  end|
      ]
  end

  def do_paragraphs(%Parsed{paragraphs: paragraphs} = parsed) do
    Enum.flat_map(paragraphs, &do_paragraph(&1, parsed))
  end

  defp do_paragraph({name, lines}, parsed) do
    [~s||, ~s|  def paragraph_#{name} do|] ++ procedure(%Parsed{parsed | procedure: lines}) ++ [~s|  end|]
  end

  def variables(%Parsed{variables: variables}) do
    Enum.flat_map(variables, &variable_to_line/1)
  end

  def procedure(%Parsed{procedure: procedure} = parsed) do
    Enum.flat_map(procedure, &procedure_to_line(&1, parsed))
  end

  def variable_to_line(%Variable{pic: pic, value: value} = variable) do
    [
      ~s|    # pic: #{pic_str(pic)}|,
      ~s|    #{variable_name(variable)} = #{maybe_parens(value, pic)}|
    ]
  end

  defp maybe_parens(val, {:str, _, _}), do: ~s|"#{val}"|
  defp maybe_parens(val, _), do: val

  defp pic_str(nil), do: "none"
  defp pic_str(pic), do: elem(pic, 1)

  defp variable_name(%Variable{name: name}), do: variable_name(name)
  defp variable_name(name), do: "var_#{name}"

  defp procedure_to_line({display_type, display}, _)
       when display_type in [:display_no_advancing, :display] do
    io_type =
      case display_type do
        :display -> "puts"
        :display_no_advancing -> "write"
      end

    to_display =
      display
      |> display_vars_and_strings()
      |> Enum.join(" <> ")

    [~s|    IO.#{io_type} #{to_display}|]
  end

  defp procedure_to_line({:accept, var}, %Parsed{variable_map: m}),
    do: [~s|    #{variable_name(m[var])} = accept(#{inspect(m[var].pic)})|]

  defp procedure_to_line({:stop, :run}, _), do: [~s|    throw :stop_run|]

  defp procedure_to_line({:move_line, [val, "TO", var]}, %Parsed{variable_map: m}),
    do: [~s|    #{variable_name(var)} = format(#{val}, #{inspect(m[var].pic)})|]

  defp procedure_to_line({:perform_paragraph, paragraph_name}, _),
    do: [~s|    paragraph_#{paragraph_name}()|]

  defp procedure_to_line({:perform, {:repeat, x, paragraph_name}}, _),
    do: List.duplicate(~s|    paragraph_#{paragraph_name}()|, x)

  defp procedure_to_line(other, _) do
    # coveralls-ignore-start
    Logger.warn("No procedure_to_line for #{inspect(other)}")
    []
    # coveralls-ignore-end
  end

  defp display_vars_and_strings([{:variable, var} | rest]),
    do: [variable_name(var) | display_vars_and_strings(rest)]

  defp display_vars_and_strings([{:string, str} | rest]),
    do: [~s|"#{str}"| | display_vars_and_strings(rest)]

  defp display_vars_and_strings([]), do: []
end
