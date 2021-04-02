defmodule CobolToElixirTest do
  use ExUnit.Case
  import CobolToElixirCase

  @moduletag timeout: 5000

  test "validate testing framework" do
    cobol_text = """
           >>SOURCE FORMAT FREE
    IDENTIFICATION DIVISION.
    PROGRAM-ID. test1.
    AUTHOR. Mike Binns.
    DATE-WRITTEN.March 19th 2021
    DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 NAME PIC X(5).
    PROCEDURE DIVISION.
    MOVE "Mike" TO NAME
    DISPLAY "Hello " NAME
    ACCEPT NAME
    DISPLAY "Hello " NAME
    DISPLAY "Hello " NAME WITH NO ADVANCING
    STOP RUN.

    """

    input = [{1000, "John"}]
    expected_output = "Hello Mike \nHello John \nHello John "
    # First verify our cobol code runs and returns the right value
    assert expected_output == execute_cobol_code(cobol_text, input)
    # Next run our converter to generate Elixir code
    assert {:ok, elixir_code} = CobolToElixir.convert(cobol_text, accept_via_message: true)
    IO.puts(elixir_code)
    # Now run that Elixir code, and ensure the outtput matches our expected output
    assert expected_output == execute_elixir_code(elixir_code, ElixirFromCobol.Test1, input)

    # The below code is a one liner version of all of the above commands. Lets make sure that works too.
    assert_output_equal(cobol_text, ElixirFromCobol.Test1, expected_output, input)
  end
end
