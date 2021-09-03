defmodule CobolToElixir.Parsed do
  defstruct program_id: nil,
            author: nil,
            date_written: nil,
            divisions: [],
            file_control: [],
            file_variables: %{},
            procedure: [],
            paragraphs: %{},
            variables: [],
            variable_map: %{}
end
