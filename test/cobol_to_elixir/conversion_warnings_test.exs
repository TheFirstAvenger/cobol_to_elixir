defmodule CobolToElixir.ConversionWarningsTest do
  use CobolToElixirCase

  import ExUnit.CaptureLog

  test "warns on untokenizable lines" do
    cobol = """
    IDENTIFICATION DIVISION.
    PROGRAM-ID. test1.
    WHATISTHISLINE ITS NOT COBOL.
    PROCEDURE DIVISION.
    """

    log = capture_log(fn -> CobolToElixir.convert!(cobol) end)
    assert log =~ "[warn]  Unable to tokenize line: WHATISTHISLINE ITS NOT COBOL\n"
  end

  test "warns on unknown division" do
    cobol = """
    IDENTIFICATION DIVISION.
    PROGRAM-ID. test1.
    MULTIPLICATION DIVISION.
    PROCEDURE DIVISION.
    """

    log = capture_log(fn -> CobolToElixir.convert!(cobol) end)
    assert log =~ "[warn]  No parser for division MULTIPLICATION\n"
  end
end
