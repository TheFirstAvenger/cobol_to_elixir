defmodule CobolToElixir.FileTest do
  use CobolToElixirCase

  test "file writing" do
    cobol = """
            >>SOURCE FORMAT FREE
    IDENTIFICATION DIVISION.
    PROGRAM-ID. foo123.
    ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
    FILE-CONTROL.
      SELECT CustomerFile ASSIGN TO "Customer.dat"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS IS SEQUENTIAL.
    DATA DIVISION.
    FILE SECTION.
    FD CustomerFile.
    01 CustomerData.
      02 IDNum PIC 9(5).
      02 CustName.
        03 FirstName PIC X(15).
        03 LastName PIC X(15).
    WORKING-STORAGE SECTION.
    01 WSCustomerData.
      02 WSIDNum PIC 9(5).
      02 WSCustName.
        03 WSFirstName PIC X(15).
        03 WSLastName PIC X(15).

    PROCEDURE DIVISION.
    OPEN OUTPUT CustomerFile.
      MOVE 00001 TO IDNum.
      MOVE 'Doug' TO FirstName.
      MOVE "Thomas" TO LastName.
      WRITE CustomerData
      END-WRITE.
    CLOSE CustomerFile.
    STOP RUN.
    """

    validate_cobol_code(cobol)

    assert %{output: "", files: %{"Customer.dat" => "00001Doug           Thomas\n"}} = execute_cobol_code!(cobol)

    assert_output_equal(cobol, ElixirFromCobol.Foo123)
  end
end
