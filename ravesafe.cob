       IDENTIFICATION DIVISION.
       PROGRAM-ID. RAVE-SAFE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TIP-COUNT        PIC 9(2) VALUE 0.
       01  WS-COLOR-R          PIC 9(3).
       01  WS-COLOR-G          PIC 9(3).
       01  WS-COLOR-B          PIC 9(3).
       01  WS-HUE              PIC 9(3) VALUE 0.
       01  WS-CHAR-IDX         PIC 9(3).
       01  WS-CHAR-LEN         PIC 9(3).
       01  WS-ONE-CHAR         PIC X(1).
       01  WS-ESC              PIC X(1) VALUE X"1B".
       01  WS-PRINT-BUF        PIC X(60).
       01  WS-TIPS.
           05 WS-TIP-TABLE.
               10 PIC X(60) VALUE
               "==============================================".
               10 PIC X(60) VALUE
               "  RAVE SAFE: A COBOL HARM REDUCTION GUIDE".
               10 PIC X(60) VALUE
               "  Compiled for your safety.  Stay alive.".
               10 PIC X(60) VALUE
               "==============================================".
               10 PIC X(60) VALUE
               " ".
               10 PIC X(60) VALUE
               "1. HYDRATE. Water is your best friend.".
               10 PIC X(60) VALUE
               "   Sip regularly. Do not chug. Steady intake.".
               10 PIC X(60) VALUE
               " ".
               10 PIC X(60) VALUE
               "2. BUDDY SYSTEM. Never rave alone.".
               10 PIC X(60) VALUE
               "   Arrive together. Leave together. Always.".
               10 PIC X(60) VALUE
               "   Check in every 30 minutes. Every time.".
               10 PIC X(60) VALUE
               " ".
               10 PIC X(60) VALUE
               "3. DO NOT INGEST UNKNOWN SUBSTANCES.".
               10 PIC X(60) VALUE
               "   You did not bring it? Do not consume it.".
               10 PIC X(60) VALUE
               " ".
               10 PIC X(60) VALUE
               "4. TEST YOUR STUFF. Test kits save lives.".
               10 PIC X(60) VALUE
               "   Fentanyl strips are cheap. Use them.".
               10 PIC X(60) VALUE
               " ".
               10 PIC X(60) VALUE
               "5. KNOW OVERHEATING SIGNS. No sweat = danger.".
               10 PIC X(60) VALUE
               "   Cool area. Water on wrists and neck. Help.".
               10 PIC X(60) VALUE
               " ".
               10 PIC X(60) VALUE
               "6. PROTECT YOUR HEARING. Earplugs are cool.".
               10 PIC X(60) VALUE
               "   Tinnitus is permanent. Good plugs help.".
               10 PIC X(60) VALUE
               " ".
               10 PIC X(60) VALUE
               "7. TAKE BREAKS. Sit down. Cool off. Breathe.".
               10 PIC X(60) VALUE
               "   Your body is not infinite. Rest is ok.".
               10 PIC X(60) VALUE
               " ".
               10 PIC X(60) VALUE
               "8. LOOK OUT FOR OTHERS. Someone struggling?".
               10 PIC X(60) VALUE
               "   Ask if ok. Get help. Stay with them.".
               10 PIC X(60) VALUE
               " ".
               10 PIC X(60) VALUE
               "9. HAVE AN EXIT PLAN. Know where exits are.".
               10 PIC X(60) VALUE
               "   Know how you get home BEFORE you go out.".
               10 PIC X(60) VALUE
               " ".
               10 PIC X(60) VALUE
               "10. CONSENT IS EVERYTHING. Always.".
               10 PIC X(60) VALUE
               "    No one is entitled to your space. Ever.".
               10 PIC X(60) VALUE
               " ".
               10 PIC X(60) VALUE
               "==============================================".
               10 PIC X(60) VALUE
               "  Be decent to all entities regardless. ><^".
               10 PIC X(60) VALUE
               "  GNU TERRY PRATCHETT".
               10 PIC X(60) VALUE
               "==============================================".
           05 WS-TIP-ARRAY REDEFINES WS-TIP-TABLE.
               10 WS-TIP-ENTRY PIC X(60)
                   OCCURS 38 TIMES.
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY " ".
           MOVE 0 TO WS-HUE.
           PERFORM VARYING WS-TIP-COUNT FROM 1 BY 1
               UNTIL WS-TIP-COUNT > 38
               MOVE WS-TIP-ENTRY(WS-TIP-COUNT)
                   TO WS-PRINT-BUF
               PERFORM PRINT-RAINBOW-LINE
               ADD 8 TO WS-HUE
               IF WS-HUE > 359
                   SUBTRACT 360 FROM WS-HUE
               END-IF
           END-PERFORM.
           DISPLAY " ".
           DISPLAY WS-ESC "[0m" WITH NO ADVANCING.
           STOP RUN.
       PRINT-RAINBOW-LINE.
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-PRINT-BUF))
               TO WS-CHAR-LEN.
           IF WS-CHAR-LEN < 2
               DISPLAY " "
           ELSE
               PERFORM VARYING WS-CHAR-IDX
                   FROM 1 BY 1
                   UNTIL WS-CHAR-IDX > WS-CHAR-LEN
                   PERFORM CALC-RAINBOW-COLOR
                   MOVE WS-PRINT-BUF(WS-CHAR-IDX:1)
                       TO WS-ONE-CHAR
                   DISPLAY WS-ESC "[38;2;"
                       WS-COLOR-R ";"
                       WS-COLOR-G ";"
                       WS-COLOR-B "m"
                       WS-ONE-CHAR
                       WITH NO ADVANCING
                   ADD 4 TO WS-HUE
                   IF WS-HUE > 359
                       SUBTRACT 360 FROM WS-HUE
                   END-IF
               END-PERFORM
               DISPLAY " "
           END-IF.
       CALC-RAINBOW-COLOR.
           EVALUATE TRUE
               WHEN WS-HUE < 60
                   MOVE 255 TO WS-COLOR-R
                   COMPUTE WS-COLOR-G =
                       (WS-HUE * 255) / 60
                   MOVE 0 TO WS-COLOR-B
               WHEN WS-HUE < 120
                   COMPUTE WS-COLOR-R =
                       ((120 - WS-HUE) * 255) / 60
                   MOVE 255 TO WS-COLOR-G
                   MOVE 0 TO WS-COLOR-B
               WHEN WS-HUE < 180
                   MOVE 0 TO WS-COLOR-R
                   MOVE 255 TO WS-COLOR-G
                   COMPUTE WS-COLOR-B =
                       ((WS-HUE - 120) * 255) / 60
               WHEN WS-HUE < 240
                   MOVE 0 TO WS-COLOR-R
                   COMPUTE WS-COLOR-G =
                       ((240 - WS-HUE) * 255) / 60
                   MOVE 255 TO WS-COLOR-B
               WHEN WS-HUE < 300
                   COMPUTE WS-COLOR-R =
                       ((WS-HUE - 240) * 255) / 60
                   MOVE 0 TO WS-COLOR-G
                   MOVE 255 TO WS-COLOR-B
               WHEN OTHER
                   MOVE 255 TO WS-COLOR-R
                   MOVE 0 TO WS-COLOR-G
                   COMPUTE WS-COLOR-B =
                       ((360 - WS-HUE) * 255) / 60
           END-EVALUATE.
