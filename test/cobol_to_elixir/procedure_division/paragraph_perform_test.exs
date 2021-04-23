defmodule CobolToElixir.ProcedureDivision.ParagraphPerformTest do
  use CobolToElixirCase

  test "handles paragraphs/performs" do
    cobol = """
          >>SOURCE FORMAT FREE
    IDENTIFICATION DIVISION.
    PROGRAM-ID. proceduretest.
    AUTHOR. Mike Binns.
    DATE-WRITTEN.March 19th 2021.
    PROCEDURE DIVISION.
    SubOne.
            DISPLAY "In Paragraph 1"
            PERFORM SubTwo
            DISPLAY "Returned to Paragraph 1"
            PERFORM SubFour 2 TIMES.
            STOP RUN.

    SubThree.
        DISPLAY "In Paragraph 3".

    SubTwo.
        DISPLAY "In Paragraph 2"
          PERFORM SubThree
          DISPLAY "Returned to Paragraph 2".

    SubFour.
        DISPLAY "Repeat".

    STOP RUN.

    """

    validate_cobol_code(cobol)

    expected =
      "In Paragraph 1\nIn Paragraph 2\nIn Paragraph 3\nReturned to Paragraph 2\nReturned to Paragraph 1\nRepeat\n"

    assert_output_equal(cobol, ElixirFromCobol.Proceduretest, expected)
  end
end
