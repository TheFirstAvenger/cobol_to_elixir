defmodule CobolToElixir.Tokenizer do
  require Logger
  @integers_as_strings ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
  @two_integers_as_strings for a <- @integers_as_strings,
                               b <- @integers_as_strings,
                               do: a <> b

  def tokenize(program) do
    tokens =
      program
      |> String.split("\n")
      |> Enum.map(&String.trim(&1, " "))
      |> Enum.map(&String.trim_trailing(&1, "."))
      |> Enum.reject(&(&1 == ""))
      |> Enum.map(&tokenize_line/1)

    {:ok, tokens}
  end

  def tokenize_line("*>" <> comment), do: {:comment, String.trim_leading(comment, " ")}
  def tokenize_line("*" <> comment), do: {:comment, String.trim_leading(comment, " ")}
  def tokenize_line(">>SOURCE FORMAT FREE"), do: :source_format_free
  def tokenize_line(~s|$ SET SOURCEFORMAT"FREE"|), do: :source_format_free

  def tokenize_line(line) do
    cond do
      division = match_division(line) -> {:division, division}
      section = match_section(line) -> {:section, section}
      variable_line = match_variable_line(line) -> {:variable_line, variable_line}
      move_line = match_move_line(line) -> {:move_line, move_line}
      simple = match_simple(line) -> simple
      complex = match_complex(String.split(line)) -> complex
      paragraph = match_paragraph(line) -> {:paragraph, paragraph}
      true -> {:not_tokenized, warn_not_tokenized(line)}
    end
  end

  defp warn_not_tokenized(line) do
    Logger.warn("Unable to tokenize line: #{line}")
    line
  end

  defp match_division(line) do
    case String.split(line, " ") do
      [division, "DIVISION"] -> division
      _ -> false
    end
  end

  defp match_section(line) do
    case String.split(line, " ") do
      [section, "SECTION"] -> section
      _ -> false
    end
  end

  def match_variable_line(line) do
    case split_vars_and_strings(line) do
      [level, variable] when level in @two_integers_as_strings ->
        [tokenize_level(level), variable]

      [level, variable, "PIC", picture_clause] when level in @two_integers_as_strings ->
        [tokenize_level(level), variable, {:pic, picture_clause}]

      [level, variable, "PIC", picture_clause, "VALUE", value] when level in @two_integers_as_strings ->
        [tokenize_level(level), variable, {:pic, picture_clause}, {:value, maybe_to_zeros_or_spaces(value)}]

      [level, variable, "CONSTANT", "AS", value] when level in @two_integers_as_strings ->
        [tokenize_level(level), variable, :constant, {:value, value}]

      _ ->
        false
    end
  end

  defp tokenize_level(level) do
    {:level, String.to_integer(level)}
  end

  defp match_paragraph(line) do
    if Regex.match?(~r/^[a-z\-A-Z0-9]+$/, line) do
      line
    else
      false
    end
  end

  defp maybe_to_zeros_or_spaces("ZERO"), do: :zeros
  defp maybe_to_zeros_or_spaces("ZEROS"), do: :zeros
  defp maybe_to_zeros_or_spaces("SPACE"), do: :spaces
  defp maybe_to_zeros_or_spaces("SPACES"), do: :spaces
  defp maybe_to_zeros_or_spaces(other), do: other

  def match_move_line("MOVE " <> move_line), do: split_vars_and_strings(move_line)
  def match_move_line(_), do: false

  defp split_vars_and_strings(line) do
    ~r("[^"]*"|'[^']*'|[^"\s]+)
    |> Regex.scan(line)
    |> List.flatten()
    |> Enum.map(&single_quotes_to_double/1)
  end

  defp single_quotes_to_double("'" <> rest) do
    ~s|"#{String.trim_trailing(rest, "'")}"|
  end

  defp single_quotes_to_double(other), do: other

  def to_vars_and_strings(["\"" <> string | tail]),
    do: [{:string, String.trim_trailing(string, "\"")} | to_vars_and_strings(tail)]

  def to_vars_and_strings(["'" <> string | tail]),
    do: [{:string, String.trim_trailing(string, "'")} | to_vars_and_strings(tail)]

  def to_vars_and_strings([var | tail]), do: [{:variable, var} | to_vars_and_strings(tail)]
  def to_vars_and_strings([]), do: []

  defp match_simple("PROGRAM-ID." <> id), do: {:program_id, String.trim(id)}
  defp match_simple("AUTHOR." <> author), do: {:author, String.trim(author)}
  defp match_simple("DATE-WRITTEN." <> date), do: {:date_written, String.trim(date)}
  defp match_simple("DISPLAY " <> display), do: parse_display(display)
  defp match_simple("ACCEPT " <> accept), do: {:accept, accept}
  defp match_simple("COMPUTE " <> compute), do: {:compute, compute}
  defp match_simple("PERFORM " <> perform), do: {:perform, parse_perform(perform)}
  defp match_simple("STOP RUN"), do: {:stop, :run}
  defp match_simple("FILE-CONTROL"), do: :file_control
  defp match_simple("ORGANIZATION IS LINE SEQUENTIAL"), do: {:organization, :line_sequential}
  defp match_simple("ACCESS IS SEQUENTIAL"), do: {:access, :sequential}
  defp match_simple("END-WRITE"), do: :end_write
  defp match_simple(_), do: false

  defp match_complex(["FD", file_descriptor_name]), do: {:fd, file_descriptor_name}
  defp match_complex(["OPEN", "OUTPUT", file_descriptor_name]), do: {:open, :output, file_descriptor_name}
  defp match_complex(["WRITE", file_descriptor_name]), do: {:write, file_descriptor_name}
  defp match_complex(["CLOSE", file_descriptor_name]), do: {:close, file_descriptor_name}

  defp match_complex(["SELECT", file_var_name, "ASSIGN", "TO", file_name]),
    do: {:select, file_var_name, :assign, :to, file_name}

  defp match_complex(_), do: false

  defp parse_perform(perform) do
    case String.split(perform, " ") do
      [name] -> {:repeat, 1, name}
      [name, x, "TIMES"] -> {:repeat, String.to_integer(x), name}
    end
  end

  defp parse_display(display) do
    {advancing, display} = do_parse_display([], split_vars_and_strings(display))

    if advancing do
      {:display, to_vars_and_strings(display)}
    else
      {:display_no_advancing, to_vars_and_strings(display)}
    end
  end

  defp do_parse_display(acc, ["WITH", "NO", "ADVANCING"]), do: {false, Enum.reverse(acc)}
  defp do_parse_display(acc, [a | rest]), do: do_parse_display([a | acc], rest)
  defp do_parse_display(acc, []), do: {true, Enum.reverse(acc)}
end
