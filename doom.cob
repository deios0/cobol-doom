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

       01 WS-PI            PIC 9V9(8) VALUE 3.14159265.
       01 WS-RADIANS       PIC S9(3)V9(8).
       01 WS-TRIG-IDX      PIC 9(3).

       01 WS-SIN-TABLE.
          05 WS-SIN-VAL    PIC S9(1)V9(6) OCCURS 360 TIMES.
       01 WS-COS-TABLE.
          05 WS-COS-VAL    PIC S9(1)V9(6) OCCURS 360 TIMES.

       01 WS-ANGLE-WORK   PIC S9(5).
       01 WS-ANGLE-LOOKUP PIC 9(3).
       01 WS-RAY-SIN      PIC S9(1)V9(6).
       01 WS-RAY-COS      PIC S9(1)V9(6).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INIT-ANSI
           PERFORM INIT-TRIG
           PERFORM VERIFY-TRIG
      *> Test angle wrapping
           MOVE -45 TO WS-ANGLE-WORK
           PERFORM GET-SIN
           PERFORM GET-COS
           DISPLAY "sin(-45)=" WS-RAY-SIN " cos(-45)=" WS-RAY-COS
           MOVE 405 TO WS-ANGLE-WORK
           PERFORM GET-SIN
           PERFORM GET-COS
           DISPLAY "sin(405)=" WS-RAY-SIN " cos(405)=" WS-RAY-COS
           STOP RUN.

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

       INIT-TRIG.
           PERFORM VARYING WS-TRIG-IDX FROM 1 BY 1
               UNTIL WS-TRIG-IDX > 360
               COMPUTE WS-RADIANS =
                   (WS-TRIG-IDX - 1) * WS-PI / 180
               COMPUTE WS-SIN-VAL(WS-TRIG-IDX) =
                   FUNCTION SIN(WS-RADIANS)
               COMPUTE WS-COS-VAL(WS-TRIG-IDX) =
                   FUNCTION COS(WS-RADIANS)
           END-PERFORM.

       VERIFY-TRIG.
      *> sin(0)=0, cos(0)=1
           DISPLAY "sin(0)=" WS-SIN-VAL(1)
               " cos(0)=" WS-COS-VAL(1)
      *> sin(30)=0.5, cos(30)=0.866
           DISPLAY "sin(30)=" WS-SIN-VAL(31)
               " cos(30)=" WS-COS-VAL(31)
      *> sin(90)=1, cos(90)=0
           DISPLAY "sin(90)=" WS-SIN-VAL(91)
               " cos(90)=" WS-COS-VAL(91)
      *> sin(180)=0, cos(180)=-1
           DISPLAY "sin(180)=" WS-SIN-VAL(181)
               " cos(180)=" WS-COS-VAL(181).

       GET-SIN.
      *> Input: WS-ANGLE-WORK (any integer angle, can be negative)
      *> Output: WS-RAY-SIN
           COMPUTE WS-ANGLE-LOOKUP =
               FUNCTION MOD(WS-ANGLE-WORK + 3600, 360) + 1
           MOVE WS-SIN-VAL(WS-ANGLE-LOOKUP) TO WS-RAY-SIN.

       GET-COS.
      *> Input: WS-ANGLE-WORK (any integer angle)
      *> Output: WS-RAY-COS
           COMPUTE WS-ANGLE-LOOKUP =
               FUNCTION MOD(WS-ANGLE-WORK + 3600, 360) + 1
           MOVE WS-COS-VAL(WS-ANGLE-LOOKUP) TO WS-RAY-COS.
