# CobolToElixir
[![Run in Livebook](https://livebook.dev/badge/v1/blue.svg)](https://livebook.dev/run?url=https%3A%2F%2Fgithub.com%2FTheFirstAvenger%2Fcobol_to_elixir%2Fblob%2Fmaster%2Flivebook_examples.livemd)

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
    - [x] Parse Group Items (nested maps)
  - File Access
    - [ ] Parse file-control
    - [ ] Parse file section
    - [ ] Write (Open Output)
    - [ ] Read (Open Input)
  - Procedure Division
    - [x] Initialize Variables
    - [x] Display
      - [x] String
      - [x] Simple Variable
      - [x] Group Item (nested map)
    - [x] Accept
    - [ ] Move
      - [x] Into Simple Variable
      - [x] Into Group Items
    - [ ] Compute
    - [ ] If/Else
    - [x] Internal Subroutines (Perform)
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
