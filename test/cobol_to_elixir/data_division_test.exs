defmodule CobolToElixir.DataDivisionTest do
  use CobolToElixirCase

  test "handles no DATA section" do
    cobol = """
              >>SOURCE FORMAT FREE
    IDENTIFICATION DIVISION.
    PROGRAM-ID. test1.
    """

    validate_cobol_code(cobol)

    refute CobolToElixir.convert!(cobol) =~ ~s|var_|
  end

  test "raises on bad variable value" do
    cobol = """
              >>SOURCE FORMAT FREE
    IDENTIFICATION DIVISION.
    PROGRAM-ID. test1.
    DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 NAME PIC X(5) VALUE NO_QUOTES.
    """

    assert_raise RuntimeError, "String variable NO_QUOTES did not start and end with quotes", fn ->
      CobolToElixir.convert!(cobol)
    end
  end

  describe "Simple String Variables" do
    test "Handles X(5) notation" do
      cobol = """
              >>SOURCE FORMAT FREE
      IDENTIFICATION DIVISION.
      PROGRAM-ID. test1.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 NAME PIC X(5).
      """

      validate_cobol_code(cobol)

      assert CobolToElixir.convert!(cobol) =~ ~s|# pic: XXXXX\n    var_NAME = "     "|
    end

    test "Handles XXXXXX notation" do
      cobol = """
              >>SOURCE FORMAT FREE
      IDENTIFICATION DIVISION.
      PROGRAM-ID. test1.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 NAME PIC XXXXX.
      """

      validate_cobol_code(cobol)

      assert CobolToElixir.convert!(cobol) =~ ~s|# pic: XXXXX\n    var_NAME = "     "|
    end

    test "Populates values" do
      cobol = """
              >>SOURCE FORMAT FREE
      IDENTIFICATION DIVISION.
      PROGRAM-ID. test1.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 NAME PIC XXXXX VALUE "Mikey".
      """

      validate_cobol_code(cobol)

      assert CobolToElixir.convert!(cobol) =~ ~s|# pic: XXXXX\n    var_NAME = "Mikey"|
    end

    test "Populates values with single quotes" do
      cobol = """
              >>SOURCE FORMAT FREE
      IDENTIFICATION DIVISION.
      PROGRAM-ID. test1.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 NAME PIC XXXXX VALUE 'Mikey'.
      """

      validate_cobol_code(cobol)

      assert CobolToElixir.convert!(cobol) =~ ~s|# pic: XXXXX\n    var_NAME = "Mikey"|
    end

    test "Populates spaces" do
      cobol = """
              >>SOURCE FORMAT FREE
      IDENTIFICATION DIVISION.
      PROGRAM-ID. test1.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 NAME PIC XXXXX VALUE SPACES.
      """

      validate_cobol_code(cobol)

      assert CobolToElixir.convert!(cobol) =~ ~s|# pic: XXXXX\n    var_NAME = "     "|
    end

    test "Trims values" do
      cobol = """
              >>SOURCE FORMAT FREE
      IDENTIFICATION DIVISION.
      PROGRAM-ID. test1.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 NAME PIC XXXX VALUE "Mikey".
      """

      assert CobolToElixir.convert!(cobol) =~ ~s|# pic: XXXX\n    var_NAME = "Mike"|
    end

    test "Pads values" do
      cobol = """
              >>SOURCE FORMAT FREE
      IDENTIFICATION DIVISION.
      PROGRAM-ID. test1.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 NAME PIC XXXXXX VALUE "Mikey".
      """

      validate_cobol_code(cobol)

      assert CobolToElixir.convert!(cobol) =~ ~s|# pic: XXXXXX\n    var_NAME = "Mikey "|
    end
  end

  describe "Simple numeric values" do
    test "Handles 9(4) notation" do
      cobol = """
              >>SOURCE FORMAT FREE
      IDENTIFICATION DIVISION.
      PROGRAM-ID. test1.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 YEAR PIC 9(4).
      """

      validate_cobol_code(cobol)

      assert CobolToElixir.convert!(cobol) =~ ~s|# pic: 9999\n    var_YEAR = 0|
    end

    test "Handles 9999 notation" do
      cobol = """
              >>SOURCE FORMAT FREE
      IDENTIFICATION DIVISION.
      PROGRAM-ID. test1.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 YEAR PIC 9999.
      """

      validate_cobol_code(cobol)

      assert CobolToElixir.convert!(cobol) =~ ~s|# pic: 9999\n    var_YEAR = 0|
    end

    test "Populates values" do
      cobol = """
              >>SOURCE FORMAT FREE
      IDENTIFICATION DIVISION.
      PROGRAM-ID. test1.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 YEAR PIC 9999 VALUE 1234.
      """

      validate_cobol_code(cobol)

      assert CobolToElixir.convert!(cobol) =~ ~s|# pic: 9999\n    var_YEAR = 1234|
    end

    test "Populates zeroes" do
      cobol = """
              >>SOURCE FORMAT FREE
      IDENTIFICATION DIVISION.
      PROGRAM-ID. test1.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 YEAR PIC 9999 VALUE ZEROS.
      """

      validate_cobol_code(cobol)

      assert CobolToElixir.convert!(cobol) =~ ~s|# pic: 9999\n    var_YEAR = 0|
    end

    test "Raises on overflow" do
      cobol = """
              >>SOURCE FORMAT FREE
      IDENTIFICATION DIVISION.
      PROGRAM-ID. test1.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 YEAR PIC 9999 VALUE 12345.
      """

      assert_raise RuntimeError, "Variable YEAR has value 12345 which is larger than pic (9999) allows", fn ->
        CobolToElixir.convert!(cobol)
      end
    end
  end
end
