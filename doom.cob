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
       01 WS-OUTPUT-LINE  PIC X(1200).
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

       01 WS-FRAME.
          05 WS-FRAME-ROW OCCURS 40 TIMES.
             10 WS-FRAME-CELL PIC X OCCURS 120 TIMES.
       01 WS-SHADE-CHAR   PIC X.
       01 WS-WALL-COLOR   PIC 9.

       01 WS-COLOR-BUF.
          05 WS-COLOR-ROW OCCURS 40 TIMES.
             10 WS-COLOR-CELL PIC 9 OCCURS 120 TIMES.
      *> Color codes: 0=reset, 1=blue(ceiling), 2=white(wall close),
      *> 3=gray(wall far), 4=yellow(floor), 5=red(enemy),
      *> 6=green(HUD), 7=byellow(crosshair), 8=bgreen(exit)
       01 WS-PREV-COLOR   PIC 9.
       01 WS-OUT-POS      PIC 9(4).
       01 WS-ANSI-CURSOR  PIC X(12).

       01 WS-MOVE-SPEED   PIC 9V9(4) VALUE 0.3000.
       01 WS-TURN-SPEED   PIC 9(2)   VALUE 10.
       01 WS-NEW-X        PIC S9(3)V9(4).
       01 WS-NEW-Y        PIC S9(3)V9(4).
       01 WS-CHK-X        PIC S9(3).
       01 WS-CHK-Y        PIC S9(3).

       01 WS-ENEMIES.
          05 WS-ENEMY OCCURS 10 TIMES.
             10 WS-EX       PIC S9(3)V9(4).
             10 WS-EY       PIC S9(3)V9(4).
             10 WS-EALIVE   PIC 9.
             10 WS-EHEALTH  PIC 9(2).
             10 WS-ESPEED   PIC 9V9(2).

       01 WS-ENEMY-IDX    PIC 9(2).
       01 WS-ENEMY-DX     PIC S9(3)V9(4).
       01 WS-ENEMY-DY     PIC S9(3)V9(4).
       01 WS-ENEMY-ANGLE  PIC S9(5)V9(4).
       01 WS-ENEMY-DIST   PIC S9(5)V9(4).
       01 WS-ENEMY-COL    PIC S9(5).
       01 WS-ENEMY-SIZE   PIC S9(3).
       01 WS-ANGLE-DIFF   PIC S9(5)V9(4).
       01 WS-ATAN-RESULT  PIC S9(3)V9(6).
       01 WS-ABS-DX       PIC S9(3)V9(4).
       01 WS-ABS-DY       PIC S9(3)V9(4).
       01 WS-TEMP         PIC S9(5)V9(4).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INIT-ANSI
           PERFORM INIT-TRIG
           PERFORM INIT-MAP
           PERFORM INIT-PLAYER
           PERFORM SPAWN-ENEMIES
           CALL "SYSTEM" USING WS-STTY-RAW
           DISPLAY WS-ANSI-CLEAR
           MOVE 0 TO WS-GAME-OVER
           PERFORM GAME-LOOP UNTIL WS-GAME-OVER = 1
           CALL "SYSTEM" USING WS-STTY-SANE
           DISPLAY WS-ANSI-CLEAR
           DISPLAY "Thanks for playing DOOM COBOL!"
           STOP RUN.

       GAME-LOOP.
           PERFORM CAST-ALL-RAYS
           PERFORM RENDER-FRAME
           PERFORM RENDER-ENEMIES
           PERFORM RENDER-CROSSHAIR
           PERFORM DRAW-FRAME
           PERFORM READ-INPUT
           PERFORM PROCESS-INPUT
           PERFORM MOVE-ENEMIES
           PERFORM CHECK-ENEMY-ATTACKS.

       READ-INPUT.
           CALL "getchar" RETURNING WS-KEY-CODE
           IF WS-KEY-CODE = -1
               MOVE "q" TO WS-KEY-CHAR
           ELSE
               MOVE FUNCTION CHAR(WS-KEY-CODE + 1)
                   TO WS-KEY-CHAR
           END-IF.

       PROCESS-INPUT.
           EVALUATE WS-KEY-CHAR
      *> Forward
               WHEN "w"
               WHEN "W"
                   MOVE WS-PA TO WS-ANGLE-WORK
                   PERFORM GET-SIN
                   PERFORM GET-COS
                   COMPUTE WS-NEW-X =
                       WS-PX + WS-RAY-COS * WS-MOVE-SPEED
                   COMPUTE WS-NEW-Y =
                       WS-PY + WS-RAY-SIN * WS-MOVE-SPEED
                   PERFORM CHECK-COLLISION
      *> Backward
               WHEN "s"
               WHEN "S"
                   MOVE WS-PA TO WS-ANGLE-WORK
                   PERFORM GET-SIN
                   PERFORM GET-COS
                   COMPUTE WS-NEW-X =
                       WS-PX - WS-RAY-COS * WS-MOVE-SPEED
                   COMPUTE WS-NEW-Y =
                       WS-PY - WS-RAY-SIN * WS-MOVE-SPEED
                   PERFORM CHECK-COLLISION
      *> Rotate left
               WHEN "a"
               WHEN "A"
                   SUBTRACT WS-TURN-SPEED FROM WS-PA
                   IF WS-PA < 0
                       ADD 360 TO WS-PA
                   END-IF
      *> Rotate right
               WHEN "d"
               WHEN "D"
                   ADD WS-TURN-SPEED TO WS-PA
                   IF WS-PA >= 360
                       SUBTRACT 360 FROM WS-PA
                   END-IF
      *> Shoot
               WHEN " "
                   PERFORM SHOOT
      *> Quit
               WHEN "q"
               WHEN "Q"
                   MOVE 1 TO WS-GAME-OVER
           END-EVALUATE.

       CHECK-COLLISION.
      *> Check X movement
           COMPUTE WS-CHK-X =
               FUNCTION INTEGER-PART(WS-NEW-X) + 1
           COMPUTE WS-CHK-Y =
               FUNCTION INTEGER-PART(WS-PY) + 1
           IF WS-CHK-X >= 1 AND WS-CHK-X <= 16
               AND WS-CHK-Y >= 1 AND WS-CHK-Y <= 16
               IF WS-MAP-CELL(WS-CHK-Y, WS-CHK-X) = 0
                   OR WS-MAP-CELL(WS-CHK-Y, WS-CHK-X) = 2
                   OR WS-MAP-CELL(WS-CHK-Y, WS-CHK-X) = 9
                   MOVE WS-NEW-X TO WS-PX
               END-IF
           END-IF
      *> Check Y movement
           COMPUTE WS-CHK-X =
               FUNCTION INTEGER-PART(WS-PX) + 1
           COMPUTE WS-CHK-Y =
               FUNCTION INTEGER-PART(WS-NEW-Y) + 1
           IF WS-CHK-X >= 1 AND WS-CHK-X <= 16
               AND WS-CHK-Y >= 1 AND WS-CHK-Y <= 16
               IF WS-MAP-CELL(WS-CHK-Y, WS-CHK-X) = 0
                   OR WS-MAP-CELL(WS-CHK-Y, WS-CHK-X) = 2
                   OR WS-MAP-CELL(WS-CHK-Y, WS-CHK-X) = 9
                   MOVE WS-NEW-Y TO WS-PY
               END-IF
           END-IF.

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

       RENDER-FRAME.
           PERFORM VARYING WS-CUR-COL FROM 1 BY 1
               UNTIL WS-CUR-COL > WS-SCREEN-W
               PERFORM RENDER-COLUMN
           END-PERFORM.

       RENDER-COLUMN.
      *> Determine wall shade character by distance
           EVALUATE TRUE
               WHEN WS-DEPTH-VAL(WS-CUR-COL) < 3
                   MOVE "@" TO WS-SHADE-CHAR
               WHEN WS-DEPTH-VAL(WS-CUR-COL) < 5
                   MOVE "#" TO WS-SHADE-CHAR
               WHEN WS-DEPTH-VAL(WS-CUR-COL) < 8
                   MOVE "=" TO WS-SHADE-CHAR
               WHEN WS-DEPTH-VAL(WS-CUR-COL) < 12
                   MOVE "-" TO WS-SHADE-CHAR
               WHEN OTHER
                   MOVE "." TO WS-SHADE-CHAR
           END-EVALUATE

      *> Determine wall color by distance and type
           IF WS-HITC-VAL(WS-CUR-COL) = 9
               MOVE 8 TO WS-WALL-COLOR
           ELSE IF WS-DEPTH-VAL(WS-CUR-COL) < 5
               MOVE 2 TO WS-WALL-COLOR
           ELSE
               MOVE 3 TO WS-WALL-COLOR
           END-IF END-IF

      *> Fill this column row by row
           PERFORM VARYING WS-ROW FROM 1 BY 1
               UNTIL WS-ROW > WS-SCREEN-H
               EVALUATE TRUE
      *> Ceiling
                   WHEN WS-ROW < WS-WALLT-VAL(WS-CUR-COL)
                       MOVE " " TO
                           WS-FRAME-CELL(WS-ROW, WS-CUR-COL)
                       MOVE 1 TO
                           WS-COLOR-CELL(WS-ROW, WS-CUR-COL)
      *> Wall
                   WHEN WS-ROW >= WS-WALLT-VAL(WS-CUR-COL)
                       AND WS-ROW <= WS-WALLB-VAL(WS-CUR-COL)
                       MOVE WS-SHADE-CHAR TO
                           WS-FRAME-CELL(WS-ROW, WS-CUR-COL)
                       MOVE WS-WALL-COLOR TO
                           WS-COLOR-CELL(WS-ROW, WS-CUR-COL)
      *> Floor
                   WHEN WS-ROW > WS-WALLB-VAL(WS-CUR-COL)
                       MOVE "," TO
                           WS-FRAME-CELL(WS-ROW, WS-CUR-COL)
                       MOVE 4 TO
                           WS-COLOR-CELL(WS-ROW, WS-CUR-COL)
               END-EVALUATE
           END-PERFORM.

       RENDER-CROSSHAIR.
           MOVE "+" TO WS-FRAME-CELL(20, 60)
           MOVE 7  TO WS-COLOR-CELL(20, 60).

       DRAW-FRAME.
           DISPLAY WS-ANSI-HOME WITH NO ADVANCING
           PERFORM VARYING WS-ROW FROM 1 BY 1
               UNTIL WS-ROW > WS-SCREEN-H
               PERFORM DRAW-ROW
           END-PERFORM.

       DRAW-ROW.
           MOVE SPACES TO WS-OUTPUT-LINE
           MOVE 0 TO WS-PREV-COLOR
           MOVE 1 TO WS-OUT-POS
           PERFORM VARYING WS-CUR-COL FROM 1 BY 1
               UNTIL WS-CUR-COL > WS-SCREEN-W
      *> Insert color code if color changed
               IF WS-COLOR-CELL(WS-ROW, WS-CUR-COL)
                   NOT = WS-PREV-COLOR
                   PERFORM INSERT-COLOR-CODE
                   MOVE WS-COLOR-CELL(WS-ROW, WS-CUR-COL)
                       TO WS-PREV-COLOR
               END-IF
      *> Insert character
               MOVE WS-FRAME-CELL(WS-ROW, WS-CUR-COL)
                   TO WS-OUTPUT-LINE(WS-OUT-POS:1)
               ADD 1 TO WS-OUT-POS
           END-PERFORM
      *> Reset color at end of row
           MOVE WS-ANSI-RESET TO
               WS-OUTPUT-LINE(WS-OUT-POS:4)
           ADD 3 TO WS-OUT-POS
           DISPLAY WS-OUTPUT-LINE(1:WS-OUT-POS).

       INSERT-COLOR-CODE.
           EVALUATE WS-COLOR-CELL(WS-ROW, WS-CUR-COL)
               WHEN 1
                   MOVE WS-ANSI-BLUE TO
                       WS-OUTPUT-LINE(WS-OUT-POS:5)
                   ADD 5 TO WS-OUT-POS
               WHEN 2
                   MOVE WS-ANSI-WHITE TO
                       WS-OUTPUT-LINE(WS-OUT-POS:5)
                   ADD 5 TO WS-OUT-POS
               WHEN 3
                   MOVE WS-ANSI-GRAY TO
                       WS-OUTPUT-LINE(WS-OUT-POS:5)
                   ADD 5 TO WS-OUT-POS
               WHEN 4
                   MOVE WS-ANSI-YELLOW TO
                       WS-OUTPUT-LINE(WS-OUT-POS:5)
                   ADD 5 TO WS-OUT-POS
               WHEN 5
                   MOVE WS-ANSI-RED TO
                       WS-OUTPUT-LINE(WS-OUT-POS:5)
                   ADD 5 TO WS-OUT-POS
               WHEN 6
                   MOVE WS-ANSI-GREEN TO
                       WS-OUTPUT-LINE(WS-OUT-POS:5)
                   ADD 5 TO WS-OUT-POS
               WHEN 7
                   MOVE WS-ANSI-BYELLOW TO
                       WS-OUTPUT-LINE(WS-OUT-POS:5)
                   ADD 5 TO WS-OUT-POS
               WHEN 8
                   MOVE WS-ANSI-BGREEN TO
                       WS-OUTPUT-LINE(WS-OUT-POS:5)
                   ADD 5 TO WS-OUT-POS
               WHEN 0
                   MOVE WS-ANSI-RESET TO
                       WS-OUTPUT-LINE(WS-OUT-POS:4)
                   ADD 4 TO WS-OUT-POS
           END-EVALUATE.

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

       SPAWN-ENEMIES.
           MOVE 0 TO WS-TOTAL-ENEMIES
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 16
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > 16
                   IF WS-MAP-CELL(WS-I, WS-J) = 2
                       ADD 1 TO WS-TOTAL-ENEMIES
                       COMPUTE WS-EX(WS-TOTAL-ENEMIES) =
                           WS-J - 1 + 0.5
                       COMPUTE WS-EY(WS-TOTAL-ENEMIES) =
                           WS-I - 1 + 0.5
                       MOVE 1  TO WS-EALIVE(WS-TOTAL-ENEMIES)
                       MOVE 10 TO WS-EHEALTH(WS-TOTAL-ENEMIES)
                       MOVE 0.40 TO
                           WS-ESPEED(WS-TOTAL-ENEMIES)
      *> Clear spawn marker from map
                       MOVE 0 TO WS-MAP-CELL(WS-I, WS-J)
                   END-IF
               END-PERFORM
           END-PERFORM.

       MOVE-ENEMIES.
           PERFORM VARYING WS-ENEMY-IDX FROM 1 BY 1
               UNTIL WS-ENEMY-IDX > WS-TOTAL-ENEMIES
               IF WS-EALIVE(WS-ENEMY-IDX) = 1
                   PERFORM MOVE-ONE-ENEMY
               END-IF
           END-PERFORM.

       MOVE-ONE-ENEMY.
      *> Direction to player
           COMPUTE WS-ENEMY-DX =
               WS-PX - WS-EX(WS-ENEMY-IDX)
           COMPUTE WS-ENEMY-DY =
               WS-PY - WS-EY(WS-ENEMY-IDX)
           COMPUTE WS-ABS-DX =
               FUNCTION ABS(WS-ENEMY-DX)
           COMPUTE WS-ABS-DY =
               FUNCTION ABS(WS-ENEMY-DY)

      *> Move along axis with larger difference
           IF WS-ABS-DX > WS-ABS-DY
               IF WS-ENEMY-DX > 0
                   COMPUTE WS-NEW-X =
                       WS-EX(WS-ENEMY-IDX) +
                       WS-ESPEED(WS-ENEMY-IDX)
               ELSE
                   COMPUTE WS-NEW-X =
                       WS-EX(WS-ENEMY-IDX) -
                       WS-ESPEED(WS-ENEMY-IDX)
               END-IF
               MOVE WS-EY(WS-ENEMY-IDX) TO WS-NEW-Y
           ELSE
               MOVE WS-EX(WS-ENEMY-IDX) TO WS-NEW-X
               IF WS-ENEMY-DY > 0
                   COMPUTE WS-NEW-Y =
                       WS-EY(WS-ENEMY-IDX) +
                       WS-ESPEED(WS-ENEMY-IDX)
               ELSE
                   COMPUTE WS-NEW-Y =
                       WS-EY(WS-ENEMY-IDX) -
                       WS-ESPEED(WS-ENEMY-IDX)
               END-IF
           END-IF

      *> Check wall collision for enemy
           COMPUTE WS-CHK-X =
               FUNCTION INTEGER-PART(WS-NEW-X) + 1
           COMPUTE WS-CHK-Y =
               FUNCTION INTEGER-PART(WS-NEW-Y) + 1
           IF WS-CHK-X >= 1 AND WS-CHK-X <= 16
               AND WS-CHK-Y >= 1 AND WS-CHK-Y <= 16
               IF WS-MAP-CELL(WS-CHK-Y, WS-CHK-X) = 0
                   OR WS-MAP-CELL(WS-CHK-Y, WS-CHK-X) = 2
                   MOVE WS-NEW-X TO WS-EX(WS-ENEMY-IDX)
                   MOVE WS-NEW-Y TO WS-EY(WS-ENEMY-IDX)
               END-IF
           END-IF.

       CHECK-ENEMY-ATTACKS.
           PERFORM VARYING WS-ENEMY-IDX FROM 1 BY 1
               UNTIL WS-ENEMY-IDX > WS-TOTAL-ENEMIES
               IF WS-EALIVE(WS-ENEMY-IDX) = 1
                   COMPUTE WS-ENEMY-DIST =
                       FUNCTION SQRT(
                           (WS-PX - WS-EX(WS-ENEMY-IDX)) ** 2
                           + (WS-PY - WS-EY(WS-ENEMY-IDX)) ** 2
                       )
                   IF WS-ENEMY-DIST < 1.5
                       SUBTRACT 5 FROM WS-HEALTH
                       IF WS-HEALTH <= 0
                           MOVE 0 TO WS-HEALTH
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

       SHOOT.
           IF WS-AMMO <= 0
               EXIT PARAGRAPH
           END-IF
           SUBTRACT 1 FROM WS-AMMO

      *> Cast a ray from player position along facing angle
      *> Check each enemy: is it close to the ray and closer
      *> than the nearest wall?
           MOVE WS-PA TO WS-ANGLE-WORK
           PERFORM GET-SIN
           PERFORM GET-COS

      *> Check center column depth for max range
           MOVE WS-DEPTH-VAL(60) TO WS-TEMP

           PERFORM VARYING WS-ENEMY-IDX FROM 1 BY 1
               UNTIL WS-ENEMY-IDX > WS-TOTAL-ENEMIES
               IF WS-EALIVE(WS-ENEMY-IDX) = 1
      *> Calculate angle to this enemy
                   COMPUTE WS-ENEMY-DX =
                       WS-EX(WS-ENEMY-IDX) - WS-PX
                   COMPUTE WS-ENEMY-DY =
                       WS-EY(WS-ENEMY-IDX) - WS-PY
                   COMPUTE WS-ENEMY-DIST =
                       FUNCTION SQRT(
                           WS-ENEMY-DX ** 2 +
                           WS-ENEMY-DY ** 2)
      *> Simple hit check: enemy within 5 degrees of
      *> center and closer than wall
                   IF WS-ENEMY-DX NOT = 0
                       COMPUTE WS-ATAN-RESULT =
                           FUNCTION ATAN(
                               WS-ENEMY-DY / WS-ENEMY-DX)
                       COMPUTE WS-ENEMY-ANGLE =
                           WS-ATAN-RESULT * 180 / WS-PI
                       IF WS-ENEMY-DX < 0
                           ADD 180 TO WS-ENEMY-ANGLE
                       END-IF
                       IF WS-ENEMY-ANGLE < 0
                           ADD 360 TO WS-ENEMY-ANGLE
                       END-IF
                       COMPUTE WS-ANGLE-DIFF =
                           WS-ENEMY-ANGLE - WS-PA
                       IF WS-ANGLE-DIFF > 180
                           SUBTRACT 360 FROM WS-ANGLE-DIFF
                       END-IF
                       IF WS-ANGLE-DIFF < -180
                           ADD 360 TO WS-ANGLE-DIFF
                       END-IF
      *> Hit if within 5 degrees and closer than wall
                       IF FUNCTION ABS(WS-ANGLE-DIFF) < 5
                           AND WS-ENEMY-DIST < WS-TEMP
                           SUBTRACT 25 FROM
                               WS-EHEALTH(WS-ENEMY-IDX)
                           IF WS-EHEALTH(WS-ENEMY-IDX) <= 0
                               MOVE 0 TO
                                   WS-EALIVE(WS-ENEMY-IDX)
                               ADD 1 TO WS-KILLS
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

       RENDER-ENEMIES.
           PERFORM VARYING WS-ENEMY-IDX FROM 1 BY 1
               UNTIL WS-ENEMY-IDX > WS-TOTAL-ENEMIES
               IF WS-EALIVE(WS-ENEMY-IDX) = 1
                   PERFORM RENDER-ONE-ENEMY
               END-IF
           END-PERFORM.

       RENDER-ONE-ENEMY.
      *> Direction to enemy
           COMPUTE WS-ENEMY-DX =
               WS-EX(WS-ENEMY-IDX) - WS-PX
           COMPUTE WS-ENEMY-DY =
               WS-EY(WS-ENEMY-IDX) - WS-PY

      *> Distance to enemy
           COMPUTE WS-ENEMY-DIST =
               FUNCTION SQRT(
                   WS-ENEMY-DX * WS-ENEMY-DX +
                   WS-ENEMY-DY * WS-ENEMY-DY)
           IF WS-ENEMY-DIST < 0.1
               MOVE 0.1 TO WS-ENEMY-DIST
           END-IF

      *> Angle to enemy using ATAN
      *> We need atan2(dy, dx). COBOL only has ATAN.
      *> Handle quadrants manually.
           IF WS-ENEMY-DX NOT = 0
               COMPUTE WS-ATAN-RESULT =
                   FUNCTION ATAN(
                       WS-ENEMY-DY / WS-ENEMY-DX)
               COMPUTE WS-ENEMY-ANGLE =
                   WS-ATAN-RESULT * 180 / WS-PI
      *> Adjust for quadrant (atan gives -90 to +90)
               IF WS-ENEMY-DX < 0
                   ADD 180 TO WS-ENEMY-ANGLE
               END-IF
           ELSE
               IF WS-ENEMY-DY > 0
                   MOVE 90 TO WS-ENEMY-ANGLE
               ELSE
                   MOVE 270 TO WS-ENEMY-ANGLE
               END-IF
           END-IF
           IF WS-ENEMY-ANGLE < 0
               ADD 360 TO WS-ENEMY-ANGLE
           END-IF

      *> Angle difference from player facing
           COMPUTE WS-ANGLE-DIFF =
               WS-ENEMY-ANGLE - WS-PA
           IF WS-ANGLE-DIFF > 180
               SUBTRACT 360 FROM WS-ANGLE-DIFF
           END-IF
           IF WS-ANGLE-DIFF < -180
               ADD 360 TO WS-ANGLE-DIFF
           END-IF

      *> Check if within FOV (-30 to +30)
           IF WS-ANGLE-DIFF >= -30 AND WS-ANGLE-DIFF <= 30
      *> Map to screen column (1-based)
               COMPUTE WS-ENEMY-COL =
                   (WS-ANGLE-DIFF + 30) * WS-SCREEN-W / 60 + 1
               IF WS-ENEMY-COL >= 1
                   AND WS-ENEMY-COL <= WS-SCREEN-W
      *> Depth check: only draw if closer than wall
                   IF WS-ENEMY-DIST <
                       WS-DEPTH-VAL(WS-ENEMY-COL)
      *> Calculate sprite size
                       COMPUTE WS-ENEMY-SIZE =
                           WS-SCREEN-H / WS-ENEMY-DIST / 3
                       IF WS-ENEMY-SIZE > 10
                           MOVE 10 TO WS-ENEMY-SIZE
                       END-IF
                       IF WS-ENEMY-SIZE < 1
                           MOVE 1 TO WS-ENEMY-SIZE
                       END-IF
      *> Draw sprite
                       PERFORM DRAW-ENEMY-SPRITE
                   END-IF
               END-IF
           END-IF.

       DRAW-ENEMY-SPRITE.
      *> Draw "M" vertically centered at WS-ENEMY-COL
           COMPUTE WS-WALL-TOP =
               (WS-SCREEN-H / 2) - WS-ENEMY-SIZE
           COMPUTE WS-WALL-BOT =
               (WS-SCREEN-H / 2) + WS-ENEMY-SIZE
           IF WS-WALL-TOP < 1
               MOVE 1 TO WS-WALL-TOP
           END-IF
           IF WS-WALL-BOT > WS-SCREEN-H
               MOVE WS-SCREEN-H TO WS-WALL-BOT
           END-IF
           PERFORM VARYING WS-ROW FROM WS-WALL-TOP BY 1
               UNTIL WS-ROW > WS-WALL-BOT
               MOVE "M" TO
                   WS-FRAME-CELL(WS-ROW, WS-ENEMY-COL)
               MOVE 5 TO
                   WS-COLOR-CELL(WS-ROW, WS-ENEMY-COL)
           END-PERFORM.
