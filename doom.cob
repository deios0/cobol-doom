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

       01 WS-SCREEN-W     PIC 9(3)   VALUE 120.
       01 WS-SCREEN-H     PIC 9(2)   VALUE 40.
       01 WS-FOV          PIC 9(2)   VALUE 60.
       01 WS-HALF-FOV     PIC 9(2)   VALUE 30.

       01 WS-RAY-ANGLE    PIC S9(5).
       01 WS-RAY-DIR-X    PIC S9(3)V9(6).
       01 WS-RAY-DIR-Y    PIC S9(3)V9(6).

       01 WS-MAP-X        PIC S9(3).
       01 WS-MAP-Y        PIC S9(3).
       01 WS-STEP-X       PIC S9(1).
       01 WS-STEP-Y       PIC S9(1).

       01 WS-SIDE-DIST-X  PIC S9(5)V9(6).
       01 WS-SIDE-DIST-Y  PIC S9(5)V9(6).
       01 WS-DELTA-DIST-X PIC S9(5)V9(6).
       01 WS-DELTA-DIST-Y PIC S9(5)V9(6).

       01 WS-HIT          PIC 9.
       01 WS-SIDE         PIC 9.
       01 WS-PERP-DIST    PIC S9(5)V9(6).
       01 WS-WALL-H       PIC S9(3).
       01 WS-WALL-TOP     PIC S9(3).
       01 WS-WALL-BOT     PIC S9(3).
       01 WS-MAX-STEPS    PIC 9(3)   VALUE 064.
       01 WS-STEP-COUNT   PIC 9(3).
       01 WS-CUR-COL      PIC 9(3).
       01 WS-ROW          PIC 9(3).

       01 WS-DEPTH-BUF.
          05 WS-DEPTH-VAL PIC S9(5)V9(4) OCCURS 120 TIMES.
       01 WS-WALL-H-BUF.
          05 WS-WALLH-VAL PIC S9(3) OCCURS 120 TIMES.
       01 WS-WALL-TOP-BUF.
          05 WS-WALLT-VAL PIC S9(3) OCCURS 120 TIMES.
       01 WS-WALL-BOT-BUF.
          05 WS-WALLB-VAL PIC S9(3) OCCURS 120 TIMES.
       01 WS-HIT-CELL-BUF.
          05 WS-HITC-VAL  PIC 9 OCCURS 120 TIMES.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INIT-ANSI
           PERFORM INIT-TRIG
           PERFORM INIT-MAP
           PERFORM INIT-PLAYER
           PERFORM CAST-ALL-RAYS
           DISPLAY "Raycasting complete."
           DISPLAY "Col 1  dist: " WS-DEPTH-VAL(1)
               " height: " WS-WALLH-VAL(1)
               " top: " WS-WALLT-VAL(1)
               " bot: " WS-WALLB-VAL(1)
           DISPLAY "Col 60 dist: " WS-DEPTH-VAL(60)
               " height: " WS-WALLH-VAL(60)
               " top: " WS-WALLT-VAL(60)
               " bot: " WS-WALLB-VAL(60)
           DISPLAY "Col 120 dist: " WS-DEPTH-VAL(120)
               " height: " WS-WALLH-VAL(120)
               " top: " WS-WALLT-VAL(120)
               " bot: " WS-WALLB-VAL(120)
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

       CAST-ALL-RAYS.
           PERFORM VARYING WS-CUR-COL FROM 1 BY 1
               UNTIL WS-CUR-COL > WS-SCREEN-W
               PERFORM CAST-ONE-RAY
           END-PERFORM.

       CAST-ONE-RAY.
      *> Calculate ray angle for this column
           COMPUTE WS-RAY-ANGLE =
               WS-PA - WS-HALF-FOV +
               WS-CUR-COL * WS-FOV / WS-SCREEN-W

      *> Get sin/cos for ray direction
           MOVE WS-RAY-ANGLE TO WS-ANGLE-WORK
           PERFORM GET-SIN
           PERFORM GET-COS
           MOVE WS-RAY-SIN TO WS-RAY-DIR-Y
           MOVE WS-RAY-COS TO WS-RAY-DIR-X

      *> Starting map cell (0-based position -> 1-based array)
           COMPUTE WS-MAP-X =
               FUNCTION INTEGER-PART(WS-PX) + 1
           COMPUTE WS-MAP-Y =
               FUNCTION INTEGER-PART(WS-PY) + 1

      *> Calculate delta distances
           IF WS-RAY-DIR-X NOT = 0
               COMPUTE WS-DELTA-DIST-X =
                   FUNCTION ABS(1.0 / WS-RAY-DIR-X)
           ELSE
               MOVE 999.0 TO WS-DELTA-DIST-X
           END-IF
           IF WS-RAY-DIR-Y NOT = 0
               COMPUTE WS-DELTA-DIST-Y =
                   FUNCTION ABS(1.0 / WS-RAY-DIR-Y)
           ELSE
               MOVE 999.0 TO WS-DELTA-DIST-Y
           END-IF

      *> Step direction and initial side distances
           IF WS-RAY-DIR-X < 0
               MOVE -1 TO WS-STEP-X
               COMPUTE WS-SIDE-DIST-X =
                   (WS-PX - FUNCTION INTEGER-PART(WS-PX))
                   * WS-DELTA-DIST-X
           ELSE
               MOVE 1 TO WS-STEP-X
               COMPUTE WS-SIDE-DIST-X =
                   (FUNCTION INTEGER-PART(WS-PX) + 1.0 - WS-PX)
                   * WS-DELTA-DIST-X
           END-IF
           IF WS-RAY-DIR-Y < 0
               MOVE -1 TO WS-STEP-Y
               COMPUTE WS-SIDE-DIST-Y =
                   (WS-PY - FUNCTION INTEGER-PART(WS-PY))
                   * WS-DELTA-DIST-Y
           ELSE
               MOVE 1 TO WS-STEP-Y
               COMPUTE WS-SIDE-DIST-Y =
                   (FUNCTION INTEGER-PART(WS-PY) + 1.0 - WS-PY)
                   * WS-DELTA-DIST-Y
           END-IF

      *> DDA loop
           MOVE 0 TO WS-HIT
           MOVE 0 TO WS-STEP-COUNT
           PERFORM UNTIL WS-HIT = 1
               OR WS-STEP-COUNT > WS-MAX-STEPS
               IF WS-SIDE-DIST-X < WS-SIDE-DIST-Y
                   ADD WS-DELTA-DIST-X TO WS-SIDE-DIST-X
                   ADD WS-STEP-X TO WS-MAP-X
                   MOVE 0 TO WS-SIDE
               ELSE
                   ADD WS-DELTA-DIST-Y TO WS-SIDE-DIST-Y
                   ADD WS-STEP-Y TO WS-MAP-Y
                   MOVE 1 TO WS-SIDE
               END-IF
               ADD 1 TO WS-STEP-COUNT
      *> Check bounds (1-based: valid range 1-16)
               IF WS-MAP-X >= 1 AND WS-MAP-X <= 16
                   AND WS-MAP-Y >= 1 AND WS-MAP-Y <= 16
                   IF WS-MAP-CELL(WS-MAP-Y, WS-MAP-X) >= 1
                       MOVE 1 TO WS-HIT
                       MOVE WS-MAP-CELL(WS-MAP-Y, WS-MAP-X)
                           TO WS-HITC-VAL(WS-CUR-COL)
                   END-IF
               ELSE
                   MOVE 1 TO WS-HIT
                   MOVE 1 TO WS-HITC-VAL(WS-CUR-COL)
               END-IF
           END-PERFORM

      *> Perpendicular distance (fixes fisheye)
           IF WS-SIDE = 0
               COMPUTE WS-PERP-DIST =
                   WS-SIDE-DIST-X - WS-DELTA-DIST-X
           ELSE
               COMPUTE WS-PERP-DIST =
                   WS-SIDE-DIST-Y - WS-DELTA-DIST-Y
           END-IF
           IF WS-PERP-DIST < 0.1
               MOVE 0.1 TO WS-PERP-DIST
           END-IF

      *> Wall height on screen
           COMPUTE WS-WALL-H =
               WS-SCREEN-H / WS-PERP-DIST
           IF WS-WALL-H > WS-SCREEN-H
               MOVE WS-SCREEN-H TO WS-WALL-H
           END-IF

      *> Wall top/bottom rows
           COMPUTE WS-WALL-TOP =
               (WS-SCREEN-H / 2) - (WS-WALL-H / 2) + 1
           IF WS-WALL-TOP < 1
               MOVE 1 TO WS-WALL-TOP
           END-IF
           COMPUTE WS-WALL-BOT =
               (WS-SCREEN-H / 2) + (WS-WALL-H / 2)
           IF WS-WALL-BOT > WS-SCREEN-H
               MOVE WS-SCREEN-H TO WS-WALL-BOT
           END-IF

      *> Store in buffers
           MOVE WS-PERP-DIST TO WS-DEPTH-VAL(WS-CUR-COL)
           MOVE WS-WALL-H TO WS-WALLH-VAL(WS-CUR-COL)
           MOVE WS-WALL-TOP TO WS-WALLT-VAL(WS-CUR-COL)
           MOVE WS-WALL-BOT TO WS-WALLB-VAL(WS-CUR-COL).

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
