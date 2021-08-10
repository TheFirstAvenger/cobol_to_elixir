defmodule CobolToElixir.CSISCOBOLExamples.IterifTest do
  use CobolToElixirCase

  # Need to add support for inline perform, currently only support `perform [procedure_name]`
  @tag :skip
  test "iterif.cbl" do
    cobol = """
           >> SOURCE FORMAT FREE
          *> replaced below line with above line to get it to compile
          *>      $ SET SOURCEFORMAT"FREE"
    IDENTIFICATION DIVISION.
    PROGRAM-ID.  Iteration-If.
    AUTHOR.  Michael Coughlan.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
    01  Num1           PIC 9  VALUE ZEROS.
    01  Num2           PIC 9  VALUE ZEROS.
    01  Result         PIC 99 VALUE ZEROS.
    01  Operator       PIC X  VALUE SPACE.

    PROCEDURE DIVISION.
    Calculator.
        PERFORM 3 TIMES
          DISPLAY "Enter First Number      : " WITH NO ADVANCING
          ACCEPT Num1
          DISPLAY "Enter Second Number     : " WITH NO ADVANCING
          ACCEPT Num2
          DISPLAY "Enter operator (+ or *) : " WITH NO ADVANCING
          ACCEPT Operator
          IF Operator = "+" THEN
              ADD Num1, Num2 GIVING Result
          END-IF
          IF Operator = "*" THEN
              MULTIPLY Num1 BY Num2 GIVING Result
          END-IF
          DISPLAY "Result is = ", Result
        END-PERFORM.
        STOP RUN.
    """

    validate_cobol_code(cobol)

    input = [{100, 5}, {100, 6}, {100, "+"}, {100, 5}, {100, 6}, {100, "+"}, {100, 5}, {100, 6}, {100, "+"}]

    expected_output =
      "Enter First Number      : Enter Second Number     : Enter operator (+ or *) : Result is = 11\nEnter First Number      : Enter Second Number     : Enter operator (+ or *) : Result is = 11\nEnter First Number      : Enter Second Number     : Enter operator (+ or *) : Result is = 11\n"

    assert_output_equal(cobol, ElixirFromCobol.Test1, expected_output, input)
  end
end
