defmodule CobolToElixir.Parsed do
  defstruct program_id: nil,
            author: nil,
            date_written: nil,
            divisions: [],
            procedure: [],
            variables: [],
            variable_map: %{}
end
