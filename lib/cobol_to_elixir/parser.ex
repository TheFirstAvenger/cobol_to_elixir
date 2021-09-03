defmodule CobolToElixir.Parser do
  alias CobolToElixir.Parsed
  alias CobolToElixir.Parsed.Variable

  require Logger

  def parse(tokenized) do
    divisions = parse_divisions(tokenized)

    parsed =
      Enum.reduce(divisions, %Parsed{}, fn {name, division}, parsed ->
        parse_division(name, division, parsed)
      end)

    {:ok, parsed}
  end

  def parse_divisions(tokenized) do
    do_parse_divisions(tokenized, nil, %{})
  end

  defp do_parse_divisions([:source_format_free | tail], nil, %{}),
    do: do_parse_divisions(tail, nil, %{})

  defp do_parse_divisions([{:division, division} | tail], _current, divisions),
    do: do_parse_divisions(tail, division, Map.put_new(divisions, division, []))

  defp do_parse_divisions([other | tail], current, divisions),
    do: do_parse_divisions(tail, current, update_in(divisions, [current], &(&1 ++ [other])))

  defp do_parse_divisions([], _, divisions), do: divisions

  def parse_sections(tokenized) do
    do_parse_sections(tokenized, nil, %{})
  end

  defp do_parse_sections([{:section, section} | tail], _current, sections),
    do: do_parse_sections(tail, section, Map.put_new(sections, section, []))

  defp do_parse_sections([other | tail], current, sections),
    do: do_parse_sections(tail, current, update_in(sections, [current], &(&1 ++ [other])))

  defp do_parse_sections([], _, sections), do: sections

  defp parse_division("IDENTIFICATION", identification, %Parsed{} = parsed) do
    if is_nil(identification), do: raise("No identification division")
    {program_id, identification} = Keyword.pop!(identification, :program_id)
    {author, identification} = Keyword.pop(identification, :author)
    {date_written, identification} = Keyword.pop(identification, :date_written)

    # coveralls-ignore-start
    case identification do
      [] -> :ok
      [{:not_tokenized, _}] -> :ok
      _ -> Logger.warn("Unparsed identification: #{inspect(Keyword.keys(identification))}")
    end

    # coveralls-ignore-end

    %Parsed{
      parsed
      | program_id: program_id,
        author: author,
        date_written: date_written
    }
  end

  defp parse_division("DATA", data, parsed) do
    data
    |> parse_sections()
    |> Enum.reduce(parsed, fn {name, section}, parsed ->
      parse_data_section(name, section, parsed)
    end)
  end

  defp parse_division("PROCEDURE", procedure, %Parsed{} = parsed) do
    {procedure, paragraphs, remaining} =
      Enum.reduce(procedure, {[], %{}, nil}, fn
        {:paragraph, paragraph_name}, {procedure, paragraphs, nil} ->
          if Map.has_key?(paragraphs, paragraph_name),
            do: raise("Multiple paragraphs found named #{paragraph_name}")

          {[{:perform_paragraph, paragraph_name} | procedure], paragraphs, {paragraph_name, []}}

        {:paragraph, new_paragraph_name}, {procedure, paragraphs, {paragraph_name, lines}} ->
          if Map.has_key?(paragraphs, new_paragraph_name),
            do: raise("Multiple paragraphs found named #{new_paragraph_name}")

          {[{:perform_paragraph, new_paragraph_name} | procedure],
           Map.put(paragraphs, paragraph_name, Enum.reverse(lines)), {new_paragraph_name, []}}

        line, {procedure, paragraphs, nil} ->
          {[line | procedure], paragraphs, nil}

        line, {procedure, paragraphs, {paragraph_name, lines}} ->
          {procedure, paragraphs, {paragraph_name, [line | lines]}}
      end)

    paragraphs =
      case remaining do
        nil -> paragraphs
        {paragraph_name, lines} -> Map.put(paragraphs, paragraph_name, Enum.reverse(lines))
      end

    %Parsed{parsed | procedure: Enum.reverse(procedure), paragraphs: paragraphs}
  end

  defp parse_division("ENVIRONMENT", environment, %Parsed{} = parsed) do
    environment
    |> parse_sections()
    |> Enum.reduce(parsed, fn {name, section}, %Parsed{} = parsed ->
      parse_environment_section(name, section, parsed)
    end)
  end

  defp parse_division(name, _contents, %Parsed{} = parsed) do
    Logger.warn("No parser for division #{name}")
    parsed
  end

  defp parse_data_section("WORKING-STORAGE", contents, %Parsed{} = parsed) do
    {variables, variable_map} = parse_variables(contents)
    variable_map = Map.merge(parsed.variable_map || %{}, variable_map)
    %Parsed{parsed | variables: variables, variable_map: variable_map}
  end

  defp parse_data_section("FILE", contents, %Parsed{} = parsed) do
    file_variables = parse_individual_files(contents, nil, %{})

    variable_map =
      file_variables
      |> Map.values()
      |> Enum.map(&elem(&1, 1))
      |> Enum.reduce(%{}, &Map.merge/2)
      |> Map.merge(parsed.variable_map)

    %Parsed{parsed | file_variables: file_variables, variable_map: variable_map}
  end

  defp parse_environment_section("INPUT-OUTPUT", contents, %Parsed{} = parsed) do
    case hd(contents) do
      :file_control ->
        [{:select, file_var_name, :assign, :to, file_name} | tl] = tl(contents)

        if tl != [{:organization, :line_sequential}, {:access, :sequential}] do
          Logger.warn("file control options differ from assumed")
        end

        %Parsed{parsed | file_control: [%{var_name: file_var_name, file_name: file_name}]}
    end
  end

  defp parse_individual_files([{:fd, file_name} | tl], _current_file_name, files),
    do: parse_individual_files(tl, file_name, Map.put(files, file_name, []))

  defp parse_individual_files([{:variable_line, _} = line | tl], current_file_name, files) do
    files = Map.put(files, current_file_name, [line | files[current_file_name]])
    parse_individual_files(tl, current_file_name, files)
  end

  defp parse_individual_files([], _, files) do
    files
    |> Enum.map(fn {name, lines} -> {name, lines |> Enum.reverse() |> parse_variables()} end)
    |> Enum.into(%{})
  end

  defp parse_variables(contents) do
    {variable_lines, other} = Enum.split_with(contents, &(elem(&1, 0) == :variable_line))

    if other != [] do
      Logger.warn("Unknown lines in variable section: #{inspect(other)}")
    end

    all_variables =
      Enum.map(variable_lines, fn {:variable_line, line} ->
        variable_line_to_variable(line)
      end)

    {variables, variable_map, _depth_list} =
      Enum.reduce(all_variables, {[], %{}, []}, fn %Variable{depth: depth} = variable,
                                                   {variables, variable_map, depth_list} ->
        depth_list = prune_depth_list(depth_list, depth)

        depth_vars = Enum.map(depth_list, &elem(&1, 0))

        case {depth_list, variable.type} do
          {[], :single} ->
            {[variable | variables], Map.put(variable_map, variable.name, variable), []}

          {[], :map} ->
            {[%Variable{variable | value: %{}} | variables], Map.put(variable_map, variable.name, variable),
             [{variable.name, depth}]}

          {[_], :single} ->
            parent = hd(variables)

            parent = %Variable{
              parent
              | value: Map.put(parent.value, variable.name, variable.value),
                children: parent.children ++ [depth_vars ++ [variable.name]]
            }

            child = %Variable{variable | value: depth_vars, type: :map_child}
            {[parent | tl(variables)], Map.put(variable_map, child.name, child), depth_list}

          {[_], :map} ->
            parent = hd(variables)

            parent = %Variable{
              parent
              | value: Map.put(parent.value, variable.name, variable.value)
            }

            child = %Variable{variable | value: depth_vars, type: :map_child_map}

            {[parent | tl(variables)], Map.put(variable_map, child.name, child), depth_list ++ [{variable.name, depth}]}

          {[_ | path], type} ->
            child_type =
              if type == :single do
                :map_child
              else
                :map_child_map
              end

            child = %Variable{variable | value: depth_vars, type: child_type}

            parent = hd(variables)

            children =
              case type do
                :map -> parent.children
                :single -> parent.children ++ [depth_vars ++ [variable.name]]
              end

            parent = %Variable{
              parent
              | value: put_in(parent.value, Enum.map(path, &elem(&1, 0)) ++ [variable.name], variable.value),
                children: children
            }

            {[parent | tl(variables)], Map.put(variable_map, child.name, child), depth_list}

          {_, _} ->
            {variables, variable_map, depth_list}
        end
      end)

    variable_map =
      Enum.reduce(variables, variable_map, fn %{name: name} = var, variable_map ->
        Map.put(variable_map, name, var)
      end)

    {Enum.reverse(variables), variable_map}
  end

  defp prune_depth_list([], _), do: []

  defp prune_depth_list(list, depth) do
    case List.pop_at(list, -1) do
      {{_, d}, rest} when d >= depth -> prune_depth_list(rest, depth)
      _ -> list
    end
  end

  defp variable_line_to_variable([depth, name | rest] = line) do
    {pic, rest} = parse_var_field(rest, :pic)
    pic = parse_pic(pic)
    {value, rest} = parse_var_field(rest, :value)
    constant = :constant in rest

    if rest != [] do
      # coveralls-ignore-start
      Logger.warn("Variable contained unexpected values: #{inspect(rest)}. Full variable: #{inspect(line)}")

      # coveralls-ignore-end
    end

    value =
      case {value, pic} do
        {value, {:str, _, length}} when value in [nil, :spaces] ->
          String.duplicate(" ", length)

        {value, {:str, _, length}} ->
          value |> trim_quotes() |> String.slice(0..(length - 1)) |> String.pad_trailing(length)

        {value, {:int, _, _}} when value in [nil, :zeros] ->
          0

        {value, {:int, pic_str, length}} ->
          if !is_nil(value) and String.length(value) > length do
            raise "Variable #{name} has value #{value} which is larger than pic (#{pic_str}) allows"
          end

          value

        {nil, nil} ->
          nil

        _ ->
          Logger.warn(label: "unexpected variable_line_to_variable: #{inspect(line)}")
          value
      end

    %Variable{
      depth: depth,
      name: name,
      type:
        if is_nil(pic) and !constant and is_nil(value) do
          :map
        else
          :single
        end,
      pic: pic,
      value: value || constant || %{},
      constant: constant
    }
  end

  defp trim_quotes(str) do
    cond do
      String.starts_with?(str, "\"") && String.ends_with?(str, "\"") ->
        String.slice(str, 1..(String.length(str) - 2))

      String.starts_with?(str, "'") && String.ends_with?(str, "'") ->
        String.slice(str, 1..(String.length(str) - 2))

      true ->
        raise "String variable #{str} did not start and end with quotes"
    end
  end

  defp parse_var_field(var, field) do
    var
    |> Enum.split_with(&match?({^field, _}, &1))
    |> case do
      {[{^field, val}], rest} -> {val, rest}
      {[], rest} -> {nil, rest}
    end
  end

  defp parse_pic(nil), do: nil

  defp parse_pic(pic) do
    pic =
      case Regex.run(~r/^([9|X])\((\d+)\)/, pic) do
        [_, type, count] -> String.duplicate(type, String.to_integer(count))
        nil -> pic
      end

    split = String.split(pic, "", trim: true)

    cond do
      Enum.all?(split, &(&1 == "X")) -> {:str, pic, String.length(pic)}
      Enum.all?(split, &(&1 == "9")) -> {:int, pic, String.length(pic)}
      true -> {:other, pic}
    end
  end
end
