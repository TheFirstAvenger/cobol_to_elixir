defmodule CobolToElixir.UtilTest do
  use CobolToElixirCase

  test "execute_cobol_code!/2 succeeds" do
    cobol = """
           >>SOURCE FORMAT FREE
    IDENTIFICATION DIVISION.
    PROGRAM-ID. test1.
    AUTHOR. Mike Binns.
    DATE-WRITTEN.March 19th 2021
    PROCEDURE DIVISION.
    DISPLAY "Hello" " " "World"
    STOP RUN.

    """

    validate_cobol_code(cobol)

    assert CobolToElixir.Util.execute_cobol_code!(cobol) == %{output: "Hello World\n", files: %{}}
  end

  test "execute_cobol_code!/2 raises on error" do
    cobol = """
           >>SOURCE FORMAT FREE
    THIS IS NOT A VALID COBOL FILE
    """

    assert_raise RuntimeError, ~r/^Error compiling cobol/, fn -> CobolToElixir.Util.execute_cobol_code!(cobol) end
  end
end
