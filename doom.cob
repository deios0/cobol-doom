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

       01 WS-MAP-SIZE     PIC 9(2)   VALUE 16.
       01 WS-I             PIC 9(3).
       01 WS-J             PIC 9(3).

       01 WS-MAP.
          05 WS-MAP-ROW OCCURS 16 TIMES.
             10 WS-MAP-CELL PIC 9 OCCURS 16 TIMES.

       01 WS-MAP-DATA.
          05 FILLER PIC X(16) VALUE "1111111111111111".
          05 FILLER PIC X(16) VALUE "1000000100000001".
          05 FILLER PIC X(16) VALUE "1010100100101001".
          05 FILLER PIC X(16) VALUE "1010100000101001".
          05 FILLER PIC X(16) VALUE "1000100100001001".
          05 FILLER PIC X(16) VALUE "1110100111101001".
          05 FILLER PIC X(16) VALUE "1000100000001001".
          05 FILLER PIC X(16) VALUE "1011110101111001".
          05 FILLER PIC X(16) VALUE "1000000100000001".
          05 FILLER PIC X(16) VALUE "1010111100011101".
          05 FILLER PIC X(16) VALUE "1010000200010001".
          05 FILLER PIC X(16) VALUE "1010101111010101".
          05 FILLER PIC X(16) VALUE "1000100000010101".
          05 FILLER PIC X(16) VALUE "1011100101000101".
          05 FILLER PIC X(16) VALUE "1000000100000091".
          05 FILLER PIC X(16) VALUE "1111111111111111".

       01 WS-MAP-DATA-R REDEFINES WS-MAP-DATA.
          05 WS-MDR-ROW OCCURS 16 TIMES.
             10 WS-MDR-CELL PIC 9 OCCURS 16 TIMES.

       01 WS-PLAYER.
          05 WS-PX         PIC S9(3)V9(4).
          05 WS-PY         PIC S9(3)V9(4).
          05 WS-PA         PIC S9(5).
          05 WS-HEALTH     PIC 9(3).
          05 WS-AMMO       PIC 9(3).

       01 WS-GAME-OVER    PIC 9      VALUE 0.
       01 WS-GAME-WON     PIC 9      VALUE 0.
       01 WS-KILLS        PIC 9(2)   VALUE 0.
       01 WS-TOTAL-ENEMIES PIC 9(2)  VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INIT-ANSI
           PERFORM INIT-TRIG
           PERFORM INIT-MAP
           PERFORM INIT-PLAYER
           DISPLAY WS-ANSI-CLEAR
           PERFORM DEBUG-MAP
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

       INIT-MAP.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 16
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > 16
                   MOVE WS-MDR-CELL(WS-I, WS-J)
                       TO WS-MAP-CELL(WS-I, WS-J)
               END-PERFORM
           END-PERFORM.

       INIT-PLAYER.
           MOVE +001.5000 TO WS-PX
           MOVE +001.5000 TO WS-PY
           MOVE 0         TO WS-PA
           MOVE 100       TO WS-HEALTH
           MOVE 25        TO WS-AMMO.

       DEBUG-MAP.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 16
               MOVE SPACES TO WS-OUTPUT-LINE
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > 16
                   EVALUATE WS-MAP-CELL(WS-I, WS-J)
                       WHEN 1 MOVE "#" TO
                           WS-OUTPUT-LINE(WS-J:1)
                       WHEN 0 MOVE "." TO
                           WS-OUTPUT-LINE(WS-J:1)
                       WHEN 2 MOVE "E" TO
                           WS-OUTPUT-LINE(WS-J:1)
                       WHEN 9 MOVE "X" TO
                           WS-OUTPUT-LINE(WS-J:1)
                   END-EVALUATE
               END-PERFORM
               DISPLAY WS-OUTPUT-LINE(1:16)
           END-PERFORM
           DISPLAY "Player at: " WS-PX " , " WS-PY
               " angle: " WS-PA.
