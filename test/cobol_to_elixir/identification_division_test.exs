defmodule CobolToElixir.IdentificationDivisionTest do
  use CobolToElixirCase

  describe "Program-ID" do
    test "Sets module name correctly" do
      cobol = """
              >>SOURCE FORMAT FREE
      IDENTIFICATION DIVISION.
      PROGRAM-ID. foo123.
      """

      validate_cobol_code(cobol)

      assert CobolToElixir.convert!(cobol) =~ "defmodule ElixirFromCobol.Foo123 do"
    end

    test "Overrides Namespace" do
      cobol = """
              >>SOURCE FORMAT FREE
      IDENTIFICATION DIVISION.
      PROGRAM-ID. foo123.
      """

      validate_cobol_code(cobol)

      assert CobolToElixir.convert!(cobol, namespace: "MyModule") =~ "defmodule MyModule.Foo123 do"
    end
  end

  describe "Author" do
    test "Renders in moduledoc" do
      cobol = """
              >>SOURCE FORMAT FREE
      IDENTIFICATION DIVISION.
      PROGRAM-ID. foo123.
      AUTHOR. John Doe.
      """

      validate_cobol_code(cobol)

      assert CobolToElixir.convert!(cobol) =~ "@moduledoc \"\"\"\n  author: John Doe\n"
    end

    test "n/a when missing" do
      cobol = """
              >>SOURCE FORMAT FREE
      IDENTIFICATION DIVISION.
      PROGRAM-ID. foo123.
      """

      validate_cobol_code(cobol)

      assert CobolToElixir.convert!(cobol) =~ "@moduledoc \"\"\"\n  author: n/a\n"
    end
  end

  describe "Date Written" do
    test "Renders in moduledoc" do
      cobol = """
              >>SOURCE FORMAT FREE
      IDENTIFICATION DIVISION.
      PROGRAM-ID. foo123.
      DATE-WRITTEN. January 1st, 2021.
      """

      validate_cobol_code(cobol)

      assert CobolToElixir.convert!(cobol) =~ "@moduledoc \"\"\"\n  author: n/a\n  date written: January 1st, 2021\n"
    end

    test "n/a when missing" do
      cobol = """
              >>SOURCE FORMAT FREE
      IDENTIFICATION DIVISION.
      PROGRAM-ID. foo123.
      """

      validate_cobol_code(cobol)

      assert CobolToElixir.convert!(cobol) =~ "@moduledoc \"\"\"\n  author: n/a\n  date written: n/a\n"
    end
  end
end
