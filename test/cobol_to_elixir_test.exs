defmodule CobolToElixirTest do
  use CobolToElixirCase

  @moduletag timeout: 10000

  test "convert_file!/2" do
    tmp_folder = Path.relative_to_cwd("test/temp_delete_contents/#{Enum.random(1000..1_000_000_000_000)}")

    try do
      File.mkdir_p!(tmp_folder)

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
      STOP RUN.
      """

      cobol_file = Path.join(tmp_folder, "cobol.cob")
      elixir_file = Path.join(tmp_folder, "module.ex")
      File.write!(cobol_file, cobol_text)
      CobolToElixir.convert_file!(cobol_file, elixir_file)
      elixir_text = File.read!(elixir_file)
      assert elixir_text =~ "defmodule ElixirFromCobol.Test1"
    after
      File.rm_rf!(tmp_folder)
    end
  end

  test "validate testing framework" do
    cobol = """
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

    validate_cobol_code(cobol)

    input = [{1000, "John"}]
    expected_output = "Hello Mike \nHello John \nHello John "
    # First verify our cobol code runs and returns the right value
    assert %{output: expected_output, files: %{}} == execute_cobol_code!(cobol, input)
    # Next run our converter to generate Elixir code
    assert {:ok, elixir_code} = CobolToElixir.convert(cobol, accept_via_message: true)
    # Now run that Elixir code, and ensure the output matches our expected output
    assert %{output: expected_output, files: nil} == execute_elixir_code(elixir_code, ElixirFromCobol.Test1, input)

    # The below code is a one liner version of all of the above commands. Lets make sure that works too.
    assert_output_equal(cobol, ElixirFromCobol.Test1, output: expected_output, input: input)

    # make sure validate_cobol_code raises on bad cobol
    assert_raise RuntimeError, ~r/^Error compiling cobol:/, fn -> validate_cobol_code("some bad code") end
  end
end
