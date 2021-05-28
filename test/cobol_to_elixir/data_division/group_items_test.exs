defmodule CobolToElixir.DataDivision.GroupItemsTest do
  use CobolToElixirCase

  alias CobolToElixir.Parsed
  alias CobolToElixir.Parser
  alias CobolToElixir.Parsed.Variable
  alias CobolToElixir.Tokenizer

  test "Parses a group item" do
    cobol = """
            >>SOURCE FORMAT FREE
    IDENTIFICATION DIVISION.
    PROGRAM-ID. test1.
    DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 Customer.
      02 IDENT PIC 9(3).
      02 CustName PIC X(5).
      02 DateOfBirth.
          03 MOB PIC 99.
          03 DOB PIC 99.
          03 YOB PIC 9(4).
    """

    validate_cobol_code(cobol)

    {:ok, tokenized} = Tokenizer.tokenize(cobol)
    {:ok, parsed} = Parser.parse(tokenized)

    assert %Parsed{
             variables: [
               %Variable{
                 name: "Customer",
                 type: :map,
                 value: %{
                   "IDENT" => 0,
                   "CustName" => "     ",
                   "DateOfBirth" => %{
                     "MOB" => 0,
                     "DOB" => 0,
                     "YOB" => 0
                   }
                 }
               }
             ],
             variable_map: %{
               "Customer" => %Variable{name: "Customer", type: :map},
               "IDENT" => %Variable{name: "IDENT", type: :map_child, value: ["Customer"]},
               "CustName" => %Variable{name: "CustName", type: :map_child, value: ["Customer"]},
               "DateOfBirth" => %Variable{name: "DateOfBirth", type: :map_child_map, value: ["Customer"]},
               "MOB" => %Variable{name: "MOB", type: :map_child, value: ["Customer", "DateOfBirth"]},
               "DOB" => %Variable{name: "DOB", type: :map_child, value: ["Customer", "DateOfBirth"]},
               "YOB" => %Variable{name: "YOB", type: :map_child, value: ["Customer", "DateOfBirth"]}
             }
           } = parsed
  end

  test "renders a group item" do
    cobol = """
            >>SOURCE FORMAT FREE
    IDENTIFICATION DIVISION.
    PROGRAM-ID. test1.
    DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 Customer.
      02 IDENT PIC 9(3).
      02 CustName PIC X(5).
      02 DateOfBirth.
          03 MOB PIC 99.
          03 DOB PIC 99.
          03 YOB PIC 9(4).
    """

    validate_cobol_code(cobol)

    elixir = CobolToElixir.convert!(cobol)

    assert elixir =~
             ~s|var_Customer = %{"CustName" => "     ", "DateOfBirth" => %{"DOB" => 0, "MOB" => 0, "YOB" => 0}, "IDENT" => 0}|
  end

  test "renders a group item followed by a single item" do
    cobol = """
            >>SOURCE FORMAT FREE
    IDENTIFICATION DIVISION.
    PROGRAM-ID. test1.
    DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 Customer.
      02 IDENT PIC 9(3).
      02 CustName PIC X(5).
      02 DateOfBirth.
          03 MOB PIC 99.
          03 DOB PIC 99.
          03 YOB PIC 9(4).
      02 Title PIC X(4).
    01 Note PIC X(4).
    """

    validate_cobol_code(cobol)

    elixir = CobolToElixir.convert!(cobol)

    assert elixir =~
             ~s|var_Customer = %{"CustName" => "     ", "DateOfBirth" => | <>
               ~s|%{"DOB" => 0, "MOB" => 0, "YOB" => 0}, "IDENT" => 0, "Title" => "    "}|

    assert elixir =~ ~s|var_Note = "    "|
  end

  test "displays a group item" do
    cobol = """
            >>SOURCE FORMAT FREE
    IDENTIFICATION DIVISION.
    PROGRAM-ID. test1.
    DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 Customer.
      02 IDENT PIC 9(3) VALUE 042.
      02 CustName PIC X(5) VALUE "MIKEB".
      02 DateOfBirth.
          03 MOB PIC 99 VALUE 1.
          03 DOB PIC 99 VALUE 2.
          03 YOB PIC 9(4) VALUE 1982.
      02 Title PIC X(3) VALUE "SSE".
    PROCEDURE DIVISION.
    DISPLAY "Customer: " Customer.
    """

    expected_output = "Customer: 042MIKEB01021982SSE\n"

    assert_output_equal(cobol, ElixirFromCobol.Test1, expected_output)
  end

  test "renders a move into a map child" do
    cobol = """
            >>SOURCE FORMAT FREE
    IDENTIFICATION DIVISION.
    PROGRAM-ID. test1.
    DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 Customer.
      02 IDENT PIC 9(3) VALUE 042.
      02 CustName PIC X(5) VALUE "MIKEB".
      02 DateOfBirth.
          03 MOB PIC 99 VALUE 1.
          03 DOB PIC 99 VALUE 2.
          03 YOB PIC 9(4) VALUE 1982.
      02 Title PIC X(3) VALUE "SSE".
    PROCEDURE DIVISION.
    DISPLAY "Customer: " Customer.
    MOVE "JOHND" TO CustName
    MOVE 1945 TO YOB
    DISPLAY "Customer: " Customer.
    """

    validate_cobol_code(cobol)

    expected_output = "Customer: 042MIKEB01021982SSE\nCustomer: 042JOHND01021945SSE\n"

    assert_output_equal(cobol, ElixirFromCobol.Test1, expected_output)
  end
end
