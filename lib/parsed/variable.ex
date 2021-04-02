defmodule CobolToElixir.Parsed.Variable do
  defstruct depth: nil, name: nil, type: :single, pic: nil, value: nil, children: [], constant: nil
end
