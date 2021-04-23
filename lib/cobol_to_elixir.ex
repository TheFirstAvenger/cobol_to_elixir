defmodule CobolToElixir do
  alias CobolToElixir.Tokenizer
  alias CobolToElixir.Parser
  alias CobolToElixir.Elixirizer

  def convert_file!(cobol_file, output_file) do
    contents = File.read!(cobol_file)
    {:ok, elixir} = convert(contents)
    File.write!(output_file, elixir)
  end

  def convert!(contents, opts \\ []) do
    {:ok, elixir} = convert(contents, opts)
    elixir
  end

  def convert(contents, opts \\ []) do
    {:ok, tokenized} = Tokenizer.tokenize(contents)
    # IO.inspect(tokenized)
    {:ok, parsed} = Parser.parse(tokenized)
    # IO.inspect(parsed)
    {:ok, elixir} = Elixirizer.elixirize(parsed, opts)
    # IO.puts(elixir)
    {:ok, elixir}
  end
end
