defmodule CobolToElixir.Parsed do
  defstruct program_id: nil,
            author: nil,
            date_written: nil,
            divisions: nil,
            procedure: nil,
            variables: [],
            variable_map: %{}
end
