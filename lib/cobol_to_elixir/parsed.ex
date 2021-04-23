defmodule CobolToElixir.Parsed do
  defstruct program_id: nil,
            author: nil,
            date_written: nil,
            divisions: [],
            procedure: [],
            paragraphs: %{},
            variables: [],
            variable_map: %{}
end
