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

  def tokenize_line(line) do
    cond do
      division = match_division(line) -> {:division, division}
      section = match_section(line) -> {:section, section}
      variable_line = match_variable_line(line) -> {:variable_line, variable_line}
      move_line = match_move_line(line) -> {:move_line, move_line}
      simple = match_simple(line) -> simple
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
        [level, variable]

      [level, variable, "PIC", picture_clause] when level in @two_integers_as_strings ->
        [level, variable, :pic, picture_clause]

      [level, variable, "PIC", picture_clause, "VALUE", value] when level in @two_integers_as_strings ->
        [level, variable, :pic, picture_clause, :value, maybe_to_zeros_or_spaces(value)]

      [level, variable, "CONSTANT", "AS", value] when level in @two_integers_as_strings ->
        [level, variable, :constant, value]

      _ ->
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
    ~r([^"^\s]+|"[^"]*")
    |> Regex.scan(line)
    |> List.flatten()
  end

  def to_vars_and_strings(str) when is_binary(str), do: str |> split_vars_and_strings() |> to_vars_and_strings()

  def to_vars_and_strings(["\"" <> string | tail]),
    do: [{:string, String.trim_trailing(string, "\"")} | to_vars_and_strings(tail)]

  def to_vars_and_strings([var | tail]), do: [{:variable, var} | to_vars_and_strings(tail)]
  def to_vars_and_strings([]), do: []

  defp match_simple("PROGRAM-ID." <> id), do: {:program_id, String.trim(id)}
  defp match_simple("AUTHOR." <> author), do: {:author, String.trim(author)}
  defp match_simple("DATE-WRITTEN." <> date), do: {:date_written, String.trim(date)}
  defp match_simple("DISPLAY " <> display), do: parse_display(display)
  defp match_simple("ACCEPT " <> accept), do: {:accept, accept}
  defp match_simple("COMPUTE " <> compute), do: {:compute, compute}
  defp match_simple("STOP RUN"), do: {:stop, :run}
  defp match_simple(_), do: false

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
