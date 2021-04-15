# CobolToElixir

Roadmap:

- Features
  - Identification Division
    - [x] Program ID -> Module Name
    - [x] Author -> Note in Moduledoc
    - [x] Date-Written -> Note in Moduledoc
  - Working-Storage Section
    - [x] Parse String Variable
    - [x] Parse Simple Number Variable
    - [ ] Parse Complex Number Variable
    - [ ] Parse Data Structure (nested maps)
  - File Access
    - [ ] Parse file-control
    - [ ] Parse file section
    - [ ] Write (Open Output)
    - [ ] Read (Open Input)
  - Procedure Division
    - [x] Initialize Variables
    - [x] Display
    - [x] Accept
    - [x] Move
    - [ ] Compute
    - [ ] If/Else
    - [ ] Internal Subroutines (Perform)
    - [ ] External Subroutines (Call)
- Testing Framework
  - [x] Compile and execute COBOL code
  - [x] Compile and execute Elixir code
  - [x] Support specifying input (stdio)
  - [x] Support comparing output (stdio)
  - [ ] Support specifying external files for input
  - [ ] Support comparing external output files on completion

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `cobol_to_elixir` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:cobol_to_elixir, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/cobol_to_elixir](https://hexdocs.pm/cobol_to_elixir).
