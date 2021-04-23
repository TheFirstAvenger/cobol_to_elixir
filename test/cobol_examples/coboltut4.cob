       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. coboltut.
AUTHOR. Mike Binns.
DATE-WRITTEN.March 19th 2021
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SPECIAL-NAMES.
DATA DIVISION.
FILE SECTION.
WORKING-STORAGE SECTION.


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
