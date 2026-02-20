       IDENTIFICATION DIVISION.
       PROGRAM-ID. GIBBERISH-TO-DISK.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GIB-FILE ASSIGN TO "gibberish.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  GIB-FILE.
       01  GIB-RECORD         PIC X(80).
       WORKING-STORAGE SECTION.
       01  WS-TIME             PIC 9(8).
       01  WS-SEED             PIC 9(18).
       01  WS-CHARS            PIC X(62) VALUE
           "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnop
      -    "qrstuvwxyz0123456789".
       01  WS-LINE             PIC X(80).
       01  WS-LINE-COUNT       PIC 9(4) VALUE 0.
       01  WS-CHAR-COUNT       PIC 9(2) VALUE 0.
       01  WS-RAND             PIC 9(18).
       01  WS-IDX              PIC 9(2).
       01  WS-TEMP             PIC 9(18).
       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-TIME FROM TIME.
           MOVE WS-TIME TO WS-SEED.
           OPEN OUTPUT GIB-FILE.
           PERFORM GENERATE-LINES
               VARYING WS-LINE-COUNT FROM 1 BY 1
               UNTIL WS-LINE-COUNT > 330.
           CLOSE GIB-FILE.
           DISPLAY "CHAOS COMMITTED TO DISK: gibberish.txt".
           STOP RUN.
       GENERATE-LINES.
           MOVE SPACES TO WS-LINE.
           PERFORM GENERATE-CHAR
               VARYING WS-CHAR-COUNT FROM 1 BY 1
               UNTIL WS-CHAR-COUNT > 80.
           WRITE GIB-RECORD FROM WS-LINE.
       GENERATE-CHAR.
           MULTIPLY WS-SEED BY 1103515245
               GIVING WS-TEMP.
           ADD 12345 TO WS-TEMP.
           COMPUTE WS-RAND =
               FUNCTION MOD(WS-TEMP, 2147483648).
           MOVE WS-RAND TO WS-SEED.
           COMPUTE WS-IDX =
               FUNCTION MOD(WS-RAND, 62) + 1.
           MOVE WS-CHARS(WS-IDX:1)
               TO WS-LINE(WS-CHAR-COUNT:1).
