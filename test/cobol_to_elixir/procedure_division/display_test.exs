defmodule CobolToElixir.ProcedureDivision.DisplayTest do
  use CobolToElixirCase

  test "displays simple string" do
    cobol = """
              >>SOURCE FORMAT FREE
    IDENTIFICATION DIVISION.
    PROGRAM-ID. foo123.
    PROCEDURE DIVISION.
    DISPLAY "Hello".
    """

    validate_cobol_code(cobol)

    assert CobolToElixir.convert!(cobol) =~ ~s|IO.puts "Hello"|
  end

  test "displays variable" do
    cobol = """
              >>SOURCE FORMAT FREE
    IDENTIFICATION DIVISION.
    PROGRAM-ID. foo123.
    DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 NAME PIC X(4) VALUE "JOE".
    PROCEDURE DIVISION.
    DISPLAY NAME.
    """

    validate_cobol_code(cobol)

    assert CobolToElixir.convert!(cobol) =~ ~s|IO.puts var_NAME|
  end

  test "displays with no advancing" do
    cobol = """
              >>SOURCE FORMAT FREE
    IDENTIFICATION DIVISION.
    PROGRAM-ID. foo123.
    DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 NAME PIC X(4) VALUE "JOE".
    PROCEDURE DIVISION.
    DISPLAY NAME WITH NO ADVANCING.
    """

    validate_cobol_code(cobol)

    assert CobolToElixir.convert!(cobol) =~ ~s|IO.write var_NAME|
  end
end
