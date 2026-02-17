       IDENTIFICATION DIVISION.
       PROGRAM-ID. DOOM-COBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MSG          PIC X(30)
           VALUE "DOOM COBOL - ENGINE STARTING".
       01 WS-ESC          PIC X VALUE X"1B".
       01 WS-ANSI-HOME    PIC X(4).
       01 WS-ANSI-CLEAR   PIC X(7).
       01 WS-ANSI-RESET   PIC X(4).
       01 WS-ANSI-RED     PIC X(5).
       01 WS-ANSI-GREEN   PIC X(5).
       01 WS-ANSI-YELLOW  PIC X(5).
       01 WS-ANSI-BLUE    PIC X(5).
       01 WS-ANSI-GRAY    PIC X(5).
       01 WS-ANSI-WHITE   PIC X(5).
       01 WS-ANSI-BYELLOW PIC X(5).
       01 WS-ANSI-BGREEN  PIC X(5).
       01 WS-OUTPUT-LINE  PIC X(600).
       01 WS-KEY-CHAR     PIC X.
       01 WS-KEY-CODE     PIC S9(4) COMP.
       01 WS-STTY-RAW     PIC X(20) VALUE "stty raw -echo".
       01 WS-STTY-SANE    PIC X(10) VALUE "stty sane".

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INIT-ANSI
           DISPLAY WS-ANSI-CLEAR
           CALL "SYSTEM" USING WS-STTY-RAW
           DISPLAY "Press keys (q to quit):"
           MOVE SPACE TO WS-KEY-CHAR
           PERFORM TEST-INPUT UNTIL WS-KEY-CHAR = "q"
           PERFORM SAFE-EXIT.

       TEST-INPUT.
           CALL "getchar" RETURNING WS-KEY-CODE
           IF WS-KEY-CODE = -1
               MOVE "q" TO WS-KEY-CHAR
           ELSE
               MOVE FUNCTION CHAR(WS-KEY-CODE + 1)
                   TO WS-KEY-CHAR
           END-IF
           DISPLAY "Key: " WS-KEY-CHAR.

       SAFE-EXIT.
           CALL "SYSTEM" USING WS-STTY-SANE
           DISPLAY " "
           DISPLAY "Terminal restored. Done."
           STOP RUN.

       INIT-ANSI.
           STRING WS-ESC "[H" DELIMITED BY SIZE
               INTO WS-ANSI-HOME
           STRING WS-ESC "[2J" WS-ESC "[H" DELIMITED BY SIZE
               INTO WS-ANSI-CLEAR
           STRING WS-ESC "[0m" DELIMITED BY SIZE
               INTO WS-ANSI-RESET
           STRING WS-ESC "[31m" DELIMITED BY SIZE
               INTO WS-ANSI-RED
           STRING WS-ESC "[32m" DELIMITED BY SIZE
               INTO WS-ANSI-GREEN
           STRING WS-ESC "[33m" DELIMITED BY SIZE
               INTO WS-ANSI-YELLOW
           STRING WS-ESC "[34m" DELIMITED BY SIZE
               INTO WS-ANSI-BLUE
           STRING WS-ESC "[90m" DELIMITED BY SIZE
               INTO WS-ANSI-GRAY
           STRING WS-ESC "[97m" DELIMITED BY SIZE
               INTO WS-ANSI-WHITE
           STRING WS-ESC "[93m" DELIMITED BY SIZE
               INTO WS-ANSI-BYELLOW
           STRING WS-ESC "[92m" DELIMITED BY SIZE
               INTO WS-ANSI-BGREEN.
