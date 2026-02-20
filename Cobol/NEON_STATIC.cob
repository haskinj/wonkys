       IDENTIFICATION DIVISION.
       PROGRAM-ID. NEON-STATIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ROW             PIC 99.
       01  WS-COL             PIC 99.
       01  WS-RAND            PIC V9(4).
       01  WS-CHAR-PICK       PIC 9.
       01  WS-COLOR-PICK      PIC 9.
       01  WS-OUTPUT-LINE     PIC X(800).
       01  WS-LINE-POS        PIC 999.
       01  WS-FRAME           PIC 9(4).
       01  WS-ANSI-ESC        PIC X VALUE X"1B".
       01  WS-CHARS           PIC X(8) VALUE "X#%@*+=~".
       01  WS-ONE-CHAR        PIC X.
       01  WS-SEED            PIC 9(8).
       01  WS-TEMP-NUM        PIC 9(4).
       01  WS-COLOR-NUM       PIC 99.
       01  WS-COLOR-STR       PIC X(2).
       01  WS-COLORS.
           05  FILLER         PIC 99 VALUE 91.
           05  FILLER         PIC 99 VALUE 92.
           05  FILLER         PIC 99 VALUE 93.
           05  FILLER         PIC 99 VALUE 95.
           05  FILLER         PIC 99 VALUE 96.
           05  FILLER         PIC 99 VALUE 97.
       01  WS-COLOR-TBL REDEFINES WS-COLORS.
           05  WS-CLR         PIC 99 OCCURS 6.
       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE FUNCTION CURRENT-DATE(9:8) TO WS-SEED
           COMPUTE WS-RAND = FUNCTION RANDOM(WS-SEED)
           DISPLAY WS-ANSI-ESC "[2J"
           DISPLAY WS-ANSI-ESC "[H"
           PERFORM VARYING WS-FRAME FROM 1 BY 1
               UNTIL WS-FRAME > 60
               DISPLAY WS-ANSI-ESC "[H"
               PERFORM VARYING WS-ROW FROM 1 BY 1
                   UNTIL WS-ROW > 24
                   MOVE SPACES TO WS-OUTPUT-LINE
                   MOVE 1 TO WS-LINE-POS
                   PERFORM VARYING WS-COL FROM 1 BY 1
                       UNTIL WS-COL > 80
                       COMPUTE WS-RAND =
                           FUNCTION RANDOM
                       COMPUTE WS-TEMP-NUM =
                           FUNCTION INTEGER(WS-RAND * 1000)
                       COMPUTE WS-COLOR-PICK =
                           FUNCTION MOD(WS-TEMP-NUM, 6) + 1
                       MOVE WS-CLR(WS-COLOR-PICK)
                           TO WS-COLOR-NUM
                       COMPUTE WS-RAND =
                           FUNCTION RANDOM
                       COMPUTE WS-TEMP-NUM =
                           FUNCTION INTEGER(WS-RAND * 1000)
                       COMPUTE WS-CHAR-PICK =
                           FUNCTION MOD(WS-TEMP-NUM, 8) + 1
                       MOVE WS-CHARS(WS-CHAR-PICK:1)
                           TO WS-ONE-CHAR
                       MOVE WS-COLOR-NUM TO WS-COLOR-STR
                       STRING
                           WS-ANSI-ESC DELIMITED SIZE
                           "[" DELIMITED SIZE
                           WS-COLOR-STR DELIMITED SPACES
                           "m" DELIMITED SIZE
                           WS-ONE-CHAR DELIMITED SIZE
                           INTO WS-OUTPUT-LINE
                           WITH POINTER WS-LINE-POS
                       END-STRING
                   END-PERFORM
                   STRING
                       WS-ANSI-ESC DELIMITED SIZE
                       "[0m" DELIMITED SIZE
                       INTO WS-OUTPUT-LINE
                       WITH POINTER WS-LINE-POS
                   END-STRING
                   DISPLAY WS-OUTPUT-LINE(1:WS-LINE-POS)
               END-PERFORM
           END-PERFORM
           DISPLAY WS-ANSI-ESC "[0m"
           STOP RUN.
