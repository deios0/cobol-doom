# COBOL Doom Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build a playable pseudo-3D first-person shooter in pure GnuCOBOL with ASCII raycasting, ANSI colors, one level, enemies with chase AI, and shooting.

**Architecture:** Single COBOL source file (`doom.cob`) compiled with GnuCOBOL free-format. DDA raycasting engine with pre-computed trig lookup tables. Keypress-driven game loop (each key = one tick). ANSI escape codes for color and cursor control. Terminal set to raw mode via `stty` for single-key input.

**Tech Stack:** GnuCOBOL 3.x (`cobc`), ANSI terminal, `stty` for raw input, no external libraries

---

### Task 1: Install GnuCOBOL + Hello World

**Files:**
- Create: `doom.cob`

**Step 1: Install GnuCOBOL**

Run: `sudo apt-get install -y gnucobol`
Run: `cobc --version` — verify version 3.x+

**Step 2: Write minimal COBOL program**

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DOOM-COBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MSG PIC X(30) VALUE "DOOM COBOL - ENGINE STARTING".

       PROCEDURE DIVISION.
           DISPLAY WS-MSG
           STOP RUN.
```

**Step 3: Compile and run**

Run: `cd /mnt/c/OneDrive/CCode/Private/Cobol-Test && cobc -x -free doom.cob -o doom`
Run: `./doom`
Expected: prints "DOOM COBOL - ENGINE STARTING"

**Step 4: Commit**

```
feat: scaffold COBOL Doom project with hello world
```

---

### Task 2: ANSI terminal control

**Files:**
- Modify: `doom.cob`

Verify that GnuCOBOL can output ANSI escape codes for cursor positioning and colors. This is foundational — if ANSI doesn't work, we need a different rendering approach.

**Step 1: Add ANSI constants to WORKING-STORAGE**

```cobol
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
```

**Step 2: Initialize ANSI strings in PROCEDURE DIVISION**

Use `STRING` to build ANSI codes since VALUE can't concatenate with X"1B":

```cobol
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
```

**Step 3: Test colored output**

Replace the main paragraph with:

```cobol
       MAIN-PROGRAM.
           PERFORM INIT-ANSI
           DISPLAY WS-ANSI-CLEAR
           DISPLAY WS-ANSI-RED "RED TEXT" WS-ANSI-RESET
           DISPLAY WS-ANSI-GREEN "GREEN TEXT" WS-ANSI-RESET
           DISPLAY WS-ANSI-BLUE "BLUE TEXT" WS-ANSI-RESET
           DISPLAY WS-ANSI-YELLOW "YELLOW TEXT" WS-ANSI-RESET
           STOP RUN.
```

**Step 4: Compile and run**

Run: `cobc -x -free doom.cob -o doom && ./doom`
Expected: colored text on cleared screen. If colors don't show, check terminal supports ANSI.

**Step 5: Commit**

```
feat: add ANSI escape code support for terminal colors and cursor
```

---

### Task 3: Single-key input handling

**Files:**
- Modify: `doom.cob`

This is the most uncertain part. COBOL's ACCEPT is line-buffered by default. We need single-key input without pressing Enter.

**Step 1: Add input variables**

```cobol
       01 WS-KEY-CHAR     PIC X.
       01 WS-KEY-CODE     PIC S9(4) COMP.
       01 WS-STTY-RAW     PIC X(20) VALUE "stty raw -echo".
       01 WS-STTY-SANE    PIC X(10) VALUE "stty sane".
```

**Step 2: Test approach — `stty raw` + ACCEPT**

Set terminal to raw mode, then ACCEPT reads one character immediately:

```cobol
       MAIN-PROGRAM.
           PERFORM INIT-ANSI
           DISPLAY WS-ANSI-CLEAR
           CALL "SYSTEM" USING WS-STTY-RAW
           DISPLAY "Press keys (q to quit):"
           PERFORM TEST-INPUT UNTIL WS-KEY-CHAR = "q"
           CALL "SYSTEM" USING WS-STTY-SANE
           DISPLAY " "
           DISPLAY "Done."
           STOP RUN.

       TEST-INPUT.
           ACCEPT WS-KEY-CHAR FROM CONSOLE
           DISPLAY "Key: " WS-KEY-CHAR.
```

**Step 3: Compile and test**

Run: `cobc -x -free doom.cob -o doom && ./doom`
Expected: each keypress immediately shows "Key: X" without needing Enter. Press q to exit.

**Step 4: If ACCEPT doesn't work in raw mode**, try fallback:

```cobol
       TEST-INPUT.
           CALL "getchar" RETURNING WS-KEY-CODE
           MOVE FUNCTION CHAR(WS-KEY-CODE) TO WS-KEY-CHAR
           DISPLAY "Key: " WS-KEY-CHAR.
```

Test which approach works. Use the one that reads single characters.

**Step 5: Add safety — always restore terminal on exit**

Ensure `stty sane` runs even if program crashes. Add a wrapper paragraph:

```cobol
       SAFE-EXIT.
           CALL "SYSTEM" USING WS-STTY-SANE
           STOP RUN.
```

**Step 6: Commit**

```
feat: add single-key input handling via stty raw mode
```

---

### Task 4: Trig tables + math verification

**Files:**
- Modify: `doom.cob`

GnuCOBOL has FUNCTION SIN and FUNCTION COS intrinsic functions. We compute lookup tables at init instead of hardcoding 720 values.

**Step 1: Add trig table data structures**

```cobol
       01 WS-PI            PIC 9V9(8) VALUE 3.14159265.
       01 WS-RADIANS       PIC S9(3)V9(8).
       01 WS-TRIG-IDX      PIC 9(3).

       01 WS-SIN-TABLE.
          05 WS-SIN-VAL    PIC S9(1)V9(6) OCCURS 360 TIMES.
       01 WS-COS-TABLE.
          05 WS-COS-VAL    PIC S9(1)V9(6) OCCURS 360 TIMES.
```

**Step 2: Write INIT-TRIG paragraph**

```cobol
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
```

**Step 3: Write verification — print known values**

```cobol
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
```

**Step 4: Test**

Run: `cobc -x -free doom.cob -o doom && ./doom`
Expected output:
```
sin(0)=+0.000000 cos(0)=+1.000000
sin(30)=+0.500000 cos(30)=+0.866025
sin(90)=+1.000000 cos(90)=+0.000000
sin(180)=+0.000000 cos(180)=-1.000000
```

Verify values are correct (small rounding errors OK).

**Step 5: Add helper to get sin/cos by angle (handles 0-359 wrapping)**

```cobol
       01 WS-ANGLE-WORK   PIC S9(5).
       01 WS-ANGLE-LOOKUP PIC 9(3).

       GET-SIN.
      *> Input: WS-ANGLE-WORK (any integer angle)
      *> Output: WS-RAY-SIN
           COMPUTE WS-ANGLE-LOOKUP =
               FUNCTION MOD(WS-ANGLE-WORK + 3600, 360) + 1
           MOVE WS-SIN-VAL(WS-ANGLE-LOOKUP) TO WS-RAY-SIN.

       GET-COS.
           COMPUTE WS-ANGLE-LOOKUP =
               FUNCTION MOD(WS-ANGLE-WORK + 3600, 360) + 1
           MOVE WS-COS-VAL(WS-ANGLE-LOOKUP) TO WS-RAY-COS.
```

**Step 6: Commit**

```
feat: add trig lookup tables with FUNCTION SIN/COS
```

---

### Task 5: Map data + player state initialization

**Files:**
- Modify: `doom.cob`

**Step 1: Add map and player data structures**

```cobol
       01 WS-MAP-SIZE     PIC 9(2)   VALUE 16.

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
```

**Step 2: Write INIT-MAP to load map data + INIT-PLAYER**

```cobol
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
```

**Step 3: Write DEBUG-MAP to display top-down view (verify map loaded)**

```cobol
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
```

**Step 4: Test**

Update MAIN-PROGRAM to call INIT-MAP, INIT-PLAYER, DEBUG-MAP.
Run: `cobc -x -free doom.cob -o doom && ./doom`
Expected: top-down map displayed with #=wall, .=floor, E=enemy spawn, X=exit.

**Step 5: Commit**

```
feat: add 16x16 level map and player state initialization
```

---

### Task 6: DDA raycasting engine

**Files:**
- Modify: `doom.cob`

The core algorithm. Cast a single ray from the player's position and find where it hits a wall.

**Step 1: Add raycasting work variables**

```cobol
       01 WS-SCREEN-W     PIC 9(3)   VALUE 120.
       01 WS-SCREEN-H     PIC 9(2)   VALUE 40.
       01 WS-FOV          PIC 9(2)   VALUE 60.
       01 WS-HALF-FOV     PIC 9(2)   VALUE 30.

       01 WS-RAY-ANGLE    PIC S9(5).
       01 WS-RAY-SIN      PIC S9(1)V9(6).
       01 WS-RAY-COS      PIC S9(1)V9(6).
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
       01 WS-MAX-STEPS    PIC 9(3)   VALUE 64.
       01 WS-STEP-COUNT   PIC 9(3).
       01 WS-CUR-COL      PIC 9(3).

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
```

**Step 2: Implement CAST-ONE-RAY paragraph**

DDA algorithm: given WS-CUR-COL, compute wall distance and store in buffers.

```cobol
       CAST-ONE-RAY.
      *> Calculate ray angle for this column
           COMPUTE WS-RAY-ANGLE =
               WS-PA - WS-HALF-FOV +
               WS-CUR-COL * WS-FOV / WS-SCREEN-W

      *> Get sin/cos for ray
           MOVE WS-RAY-ANGLE TO WS-ANGLE-WORK
           PERFORM GET-SIN
           PERFORM GET-COS
           MOVE WS-RAY-SIN TO WS-RAY-DIR-Y
           MOVE WS-RAY-COS TO WS-RAY-DIR-X

      *> Starting map cell
           COMPUTE WS-MAP-X =
               FUNCTION INTEGER-PART(WS-PX)
           COMPUTE WS-MAP-Y =
               FUNCTION INTEGER-PART(WS-PY)

      *> Calculate delta distances (distance between grid lines)
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

      *> Calculate step direction and initial side distances
           IF WS-RAY-DIR-X < 0
               MOVE -1 TO WS-STEP-X
               COMPUTE WS-SIDE-DIST-X =
                   (WS-PX - WS-MAP-X) * WS-DELTA-DIST-X
           ELSE
               MOVE 1 TO WS-STEP-X
               COMPUTE WS-SIDE-DIST-X =
                   (WS-MAP-X + 1.0 - WS-PX)
                       * WS-DELTA-DIST-X
           END-IF
           IF WS-RAY-DIR-Y < 0
               MOVE -1 TO WS-STEP-Y
               COMPUTE WS-SIDE-DIST-Y =
                   (WS-PY - WS-MAP-Y) * WS-DELTA-DIST-Y
           ELSE
               MOVE 1 TO WS-STEP-Y
               COMPUTE WS-SIDE-DIST-Y =
                   (WS-MAP-Y + 1.0 - WS-PY)
                       * WS-DELTA-DIST-Y
           END-IF

      *> DDA loop — step until wall hit
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
      *> Check bounds
               IF WS-MAP-X >= 1 AND WS-MAP-X <= 16
                   AND WS-MAP-Y >= 1 AND WS-MAP-Y <= 16
                   IF WS-MAP-CELL(WS-MAP-Y, WS-MAP-X) > 0
                       MOVE 1 TO WS-HIT
                       MOVE WS-MAP-CELL(WS-MAP-Y, WS-MAP-X)
                           TO WS-HITC-VAL(WS-CUR-COL)
                   END-IF
               ELSE
                   MOVE 1 TO WS-HIT
                   MOVE 1 TO WS-HITC-VAL(WS-CUR-COL)
               END-IF
           END-PERFORM

      *> Calculate perpendicular distance (fixes fisheye)
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

      *> Calculate wall height on screen
           COMPUTE WS-WALL-H =
               WS-SCREEN-H / WS-PERP-DIST
           IF WS-WALL-H > WS-SCREEN-H
               MOVE WS-SCREEN-H TO WS-WALL-H
           END-IF

      *> Calculate wall top/bottom rows
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
```

**Step 3: Implement CAST-ALL-RAYS**

```cobol
       CAST-ALL-RAYS.
           PERFORM VARYING WS-CUR-COL FROM 1 BY 1
               UNTIL WS-CUR-COL > WS-SCREEN-W
               PERFORM CAST-ONE-RAY
           END-PERFORM.
```

**Step 4: Test with debug output**

Cast all rays and print wall distances for verification:

```cobol
       DEBUG-RAYS.
           PERFORM INIT-TRIG
           PERFORM INIT-MAP
           PERFORM INIT-PLAYER
           PERFORM CAST-ALL-RAYS
           DISPLAY "Col 1 dist: " WS-DEPTH-VAL(1)
               " height: " WS-WALLH-VAL(1)
           DISPLAY "Col 60 dist: " WS-DEPTH-VAL(60)
               " height: " WS-WALLH-VAL(60)
           DISPLAY "Col 120 dist: " WS-DEPTH-VAL(120)
               " height: " WS-WALLH-VAL(120).
```

Run: `cobc -x -free doom.cob -o doom && ./doom`
Expected: reasonable distances (player at 1.5,1.5 facing east — col 60 should see walls ahead).

**Step 5: Commit**

```
feat: implement DDA raycasting engine with fisheye correction
```

---

### Task 7: Frame buffer rendering

**Files:**
- Modify: `doom.cob`

Build the visual frame from raycasting results — ceiling, walls, floor.

**Step 1: Add frame buffer**

```cobol
       01 WS-FRAME.
          05 WS-FRAME-ROW OCCURS 40 TIMES.
             10 WS-FRAME-CELL PIC X OCCURS 120 TIMES.
       01 WS-SHADE-CHAR   PIC X.
```

**Step 2: Implement RENDER-FRAME**

For each column, fill ceiling/wall/floor based on raycasting buffers:

```cobol
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

      *> Fill this column row by row
           PERFORM VARYING WS-ROW FROM 1 BY 1
               UNTIL WS-ROW > WS-SCREEN-H
               EVALUATE TRUE
      *> Ceiling
                   WHEN WS-ROW < WS-WALLT-VAL(WS-CUR-COL)
                       MOVE " " TO
                           WS-FRAME-CELL(WS-ROW, WS-CUR-COL)
      *> Wall
                   WHEN WS-ROW >= WS-WALLT-VAL(WS-CUR-COL)
                       AND WS-ROW <= WS-WALLB-VAL(WS-CUR-COL)
                       MOVE WS-SHADE-CHAR TO
                           WS-FRAME-CELL(WS-ROW, WS-CUR-COL)
      *> Floor
                   WHEN WS-ROW > WS-WALLB-VAL(WS-CUR-COL)
                       MOVE "," TO
                           WS-FRAME-CELL(WS-ROW, WS-CUR-COL)
               END-EVALUATE
           END-PERFORM.
```

**Step 3: Implement DRAW-FRAME (output to terminal)**

```cobol
       DRAW-FRAME.
           DISPLAY WS-ANSI-HOME WITH NO ADVANCING
           PERFORM VARYING WS-ROW FROM 1 BY 1
               UNTIL WS-ROW > WS-SCREEN-H
               DISPLAY WS-FRAME-ROW(WS-ROW)
           END-PERFORM.
```

**Step 4: Test — render one static frame**

```cobol
       MAIN-PROGRAM.
           PERFORM INIT-ANSI
           PERFORM INIT-TRIG
           PERFORM INIT-MAP
           PERFORM INIT-PLAYER
           DISPLAY WS-ANSI-CLEAR
           PERFORM CAST-ALL-RAYS
           PERFORM RENDER-FRAME
           PERFORM DRAW-FRAME
           ACCEPT WS-KEY-CHAR
           STOP RUN.
```

Run: `cobc -x -free doom.cob -o doom && ./doom`
Expected: a pseudo-3D ASCII view of the corridor! Walls should appear as vertical bars of ASCII characters. Press any key to exit.

**Step 5: Commit**

```
feat: add frame buffer rendering with ASCII wall shading
```

---

### Task 8: Player movement + collision detection

**Files:**
- Modify: `doom.cob`

**Step 1: Add movement constants**

```cobol
       01 WS-MOVE-SPEED   PIC 9V9(4) VALUE 0.3000.
       01 WS-TURN-SPEED   PIC 9(2)   VALUE 10.
       01 WS-NEW-X        PIC S9(3)V9(4).
       01 WS-NEW-Y        PIC S9(3)V9(4).
       01 WS-CHK-X        PIC S9(3).
       01 WS-CHK-Y        PIC S9(3).
```

**Step 2: Implement PROCESS-INPUT**

```cobol
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
      *> Quit
               WHEN "q"
               WHEN "Q"
                   MOVE 1 TO WS-GAME-OVER
           END-EVALUATE.
```

**Step 3: Implement CHECK-COLLISION**

Wall collision with margin to prevent getting stuck:

```cobol
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
```

**Step 4: Implement basic GAME-LOOP**

```cobol
       MAIN-PROGRAM.
           PERFORM INIT-ANSI
           PERFORM INIT-TRIG
           PERFORM INIT-MAP
           PERFORM INIT-PLAYER
           CALL "SYSTEM" USING WS-STTY-RAW
           DISPLAY WS-ANSI-CLEAR
           PERFORM GAME-LOOP UNTIL WS-GAME-OVER = 1
           CALL "SYSTEM" USING WS-STTY-SANE
           DISPLAY WS-ANSI-CLEAR
           DISPLAY "Thanks for playing DOOM COBOL!"
           STOP RUN.

       GAME-LOOP.
           PERFORM CAST-ALL-RAYS
           PERFORM RENDER-FRAME
           PERFORM DRAW-FRAME
           ACCEPT WS-KEY-CHAR FROM CONSOLE
           PERFORM PROCESS-INPUT.
```

**Step 5: Test — walk around the level**

Run: `cobc -x -free doom.cob -o doom && ./doom`
Expected: pseudo-3D view, WASD to move/rotate, Q to quit. Walls should change as you move. Cannot walk through walls.

**Step 6: Commit**

```
feat: add player movement with WASD and wall collision
```

---

### Task 9: Color rendering

**Files:**
- Modify: `doom.cob`

Replace plain DISPLAY with color-coded output. Since we need per-character colors, we build each row as a string with embedded ANSI codes.

**Step 1: Add color frame buffer**

Instead of a character buffer, track the color type per cell:

```cobol
       01 WS-COLOR-BUF.
          05 WS-COLOR-ROW OCCURS 40 TIMES.
             10 WS-COLOR-CELL PIC 9 OCCURS 120 TIMES.
      *> Color codes: 0=reset, 1=blue(ceiling), 2=white(wall close),
      *> 3=gray(wall far), 4=yellow(floor), 5=red(enemy),
      *> 6=green(HUD), 7=byellow(crosshair), 8=bgreen(exit)
```

**Step 2: Update RENDER-COLUMN to set colors**

```cobol
       RENDER-COLUMN.
      *> Determine wall shade + color by distance
           EVALUATE TRUE
               WHEN WS-DEPTH-VAL(WS-CUR-COL) < 3
                   MOVE "@" TO WS-SHADE-CHAR
                   MOVE 2 TO WS-WALL-COLOR
               WHEN WS-DEPTH-VAL(WS-CUR-COL) < 5
                   MOVE "#" TO WS-SHADE-CHAR
                   MOVE 2 TO WS-WALL-COLOR
               WHEN WS-DEPTH-VAL(WS-CUR-COL) < 8
                   MOVE "=" TO WS-SHADE-CHAR
                   MOVE 3 TO WS-WALL-COLOR
               WHEN WS-DEPTH-VAL(WS-CUR-COL) < 12
                   MOVE "-" TO WS-SHADE-CHAR
                   MOVE 3 TO WS-WALL-COLOR
               WHEN OTHER
                   MOVE "." TO WS-SHADE-CHAR
                   MOVE 3 TO WS-WALL-COLOR
           END-EVALUATE

      *> Special color for exit walls
           IF WS-HITC-VAL(WS-CUR-COL) = 9
               MOVE 8 TO WS-WALL-COLOR
           END-IF

           PERFORM VARYING WS-ROW FROM 1 BY 1
               UNTIL WS-ROW > WS-SCREEN-H
               EVALUATE TRUE
                   WHEN WS-ROW < WS-WALLT-VAL(WS-CUR-COL)
                       MOVE " " TO
                           WS-FRAME-CELL(WS-ROW, WS-CUR-COL)
                       MOVE 1 TO
                           WS-COLOR-CELL(WS-ROW, WS-CUR-COL)
                   WHEN WS-ROW >= WS-WALLT-VAL(WS-CUR-COL)
                       AND WS-ROW <= WS-WALLB-VAL(WS-CUR-COL)
                       MOVE WS-SHADE-CHAR TO
                           WS-FRAME-CELL(WS-ROW, WS-CUR-COL)
                       MOVE WS-WALL-COLOR TO
                           WS-COLOR-CELL(WS-ROW, WS-CUR-COL)
                   WHEN WS-ROW > WS-WALLB-VAL(WS-CUR-COL)
                       MOVE "," TO
                           WS-FRAME-CELL(WS-ROW, WS-CUR-COL)
                       MOVE 4 TO
                           WS-COLOR-CELL(WS-ROW, WS-CUR-COL)
               END-EVALUATE
           END-PERFORM.
```

**Step 3: Update DRAW-FRAME with color output**

Build each row with ANSI color codes. Track previous color to minimize escape code output:

```cobol
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
      *> Reset color at end of line
           MOVE WS-ANSI-RESET TO
               WS-OUTPUT-LINE(WS-OUT-POS:4)
           ADD 4 TO WS-OUT-POS
           DISPLAY WS-OUTPUT-LINE(1:WS-OUT-POS).
```

The `INSERT-COLOR-CODE` paragraph copies the appropriate ANSI code into WS-OUTPUT-LINE at WS-OUT-POS and advances the position.

**Step 4: Add crosshair at center**

```cobol
       RENDER-CROSSHAIR.
           MOVE "+" TO WS-FRAME-CELL(20, 60)
           MOVE 7  TO WS-COLOR-CELL(20, 60).
```

Call after RENDER-FRAME.

**Step 5: Test**

Run: `cobc -x -free doom.cob -o doom && ./doom`
Expected: colored output — blue ceiling (or dark space), white/gray walls, yellow floor, yellow crosshair at center.

**Step 6: Commit**

```
feat: add ANSI color rendering with distance-based wall shading
```

---

### Task 10: Enemy system (spawn + render)

**Files:**
- Modify: `doom.cob`

**Step 1: Add enemy data structures**

```cobol
       01 WS-ENEMIES.
          05 WS-ENEMY OCCURS 10 TIMES.
             10 WS-EX       PIC S9(3)V9(4).
             10 WS-EY       PIC S9(3)V9(4).
             10 WS-EALIVE   PIC 9.
             10 WS-EHEALTH  PIC 9(2).
             10 WS-ESPEED   PIC 9V9(2) VALUE 0.40.

       01 WS-ENEMY-IDX    PIC 9(2).
       01 WS-ENEMY-DX     PIC S9(3)V9(4).
       01 WS-ENEMY-DY     PIC S9(3)V9(4).
       01 WS-ENEMY-ANGLE  PIC S9(5)V9(4).
       01 WS-ENEMY-DIST   PIC S9(5)V9(4).
       01 WS-ENEMY-COL    PIC S9(5).
       01 WS-ENEMY-SIZE   PIC S9(3).
       01 WS-ANGLE-DIFF   PIC S9(5)V9(4).
       01 WS-ATAN-RESULT  PIC S9(3)V9(6).
```

**Step 2: Implement SPAWN-ENEMIES**

Scan map for cell value 2, create enemy at that position:

```cobol
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
```

**Step 3: Implement RENDER-ENEMIES**

Project each alive enemy onto the screen. Calculate angle from player to enemy, check if in FOV, determine screen column, check depth buffer.

```cobol
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

      *> Angle to enemy (using ATAN2 via ATAN)
           COMPUTE WS-ATAN-RESULT =
               FUNCTION ATAN(WS-ENEMY-DY / WS-ENEMY-DX)
           COMPUTE WS-ENEMY-ANGLE =
               WS-ATAN-RESULT * 180 / WS-PI
      *> Adjust for quadrant
           IF WS-ENEMY-DX < 0
               ADD 180 TO WS-ENEMY-ANGLE
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
      *> Map to screen column
               COMPUTE WS-ENEMY-COL =
                   (WS-ANGLE-DIFF + 30) * WS-SCREEN-W / 60
               IF WS-ENEMY-COL >= 1
                   AND WS-ENEMY-COL <= WS-SCREEN-W
      *> Depth check: only draw if closer than wall
                   IF WS-ENEMY-DIST <
                       WS-DEPTH-VAL(WS-ENEMY-COL)
      *> Calculate enemy size on screen
                       COMPUTE WS-ENEMY-SIZE =
                           WS-SCREEN-H / WS-ENEMY-DIST / 2
                       IF WS-ENEMY-SIZE > 10
                           MOVE 10 TO WS-ENEMY-SIZE
                       END-IF
                       IF WS-ENEMY-SIZE < 1
                           MOVE 1 TO WS-ENEMY-SIZE
                       END-IF
      *> Draw enemy sprite (vertical bar of "M")
                       PERFORM DRAW-ENEMY-SPRITE
                   END-IF
               END-IF
           END-IF.

       DRAW-ENEMY-SPRITE.
      *> Draw "M" characters vertically centered
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
```

**Step 4: Test — enemies visible in the level**

Add SPAWN-ENEMIES to initialization. Walk toward enemy spawn point (cell 2 at row 11, col 6 in the map). Expect to see red "M" characters.

Run: `cobc -x -free doom.cob -o doom && ./doom`

**Step 5: Commit**

```
feat: add enemy spawning and rendering with depth-checked projection
```

---

### Task 11: Enemy AI + shooting + combat

**Files:**
- Modify: `doom.cob`

**Step 1: Implement MOVE-ENEMIES (chase AI)**

```cobol
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
```

**Step 2: Implement CHECK-ENEMY-ATTACKS**

```cobol
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
```

**Step 3: Implement SHOOT (hitscan)**

Cast a ray along the player's facing direction. Check if any alive enemy is near the ray path:

```cobol
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
```

**Step 4: Add space bar to PROCESS-INPUT**

```cobol
               WHEN " "
                   PERFORM SHOOT
```

**Step 5: Update GAME-LOOP to include enemy logic**

```cobol
       GAME-LOOP.
           PERFORM CAST-ALL-RAYS
           PERFORM RENDER-FRAME
           PERFORM RENDER-ENEMIES
           PERFORM RENDER-CROSSHAIR
           PERFORM DRAW-FRAME
           ACCEPT WS-KEY-CHAR FROM CONSOLE
           PERFORM PROCESS-INPUT
           PERFORM MOVE-ENEMIES
           PERFORM CHECK-ENEMY-ATTACKS.
```

**Step 6: Test — shoot enemies**

Run game, navigate to enemy, aim with crosshair, press space.
Expected: enemy disappears when shot, kill count increments.

**Step 7: Commit**

```
feat: add enemy chase AI, hitscan shooting, and damage system
```

---

### Task 12: HUD + win/lose conditions + final integration

**Files:**
- Modify: `doom.cob`

**Step 1: Implement RENDER-HUD**

Use the last row (row 40) for HUD:

```cobol
       RENDER-HUD.
      *> Clear HUD row
           MOVE SPACES TO WS-FRAME-ROW(WS-SCREEN-H)
      *> Build HUD string
           MOVE SPACES TO WS-HUD-TEXT
           STRING
               "HP:" DELIMITED BY SIZE
               WS-HEALTH DELIMITED BY SIZE
               " | AMMO:" DELIMITED BY SIZE
               WS-AMMO DELIMITED BY SIZE
               " | KILLS:" DELIMITED BY SIZE
               WS-KILLS DELIMITED BY SIZE
               "/" DELIMITED BY SIZE
               WS-TOTAL-ENEMIES DELIMITED BY SIZE
               " | DOOM COBOL" DELIMITED BY SIZE
               INTO WS-HUD-TEXT
           END-STRING
      *> Copy to frame buffer
           MOVE WS-HUD-TEXT TO WS-FRAME-ROW(WS-SCREEN-H)
      *> Set HUD color (green)
           PERFORM VARYING WS-CUR-COL FROM 1 BY 1
               UNTIL WS-CUR-COL > WS-SCREEN-W
               MOVE 6 TO
                   WS-COLOR-CELL(WS-SCREEN-H, WS-CUR-COL)
           END-PERFORM.
```

Add `01 WS-HUD-TEXT PIC X(120).` to WORKING-STORAGE.

**Step 2: Implement CHECK-WIN-CONDITION**

```cobol
       CHECK-WIN-CONDITION.
           COMPUTE WS-CHK-X =
               FUNCTION INTEGER-PART(WS-PX) + 1
           COMPUTE WS-CHK-Y =
               FUNCTION INTEGER-PART(WS-PY) + 1
           IF WS-CHK-X >= 1 AND WS-CHK-X <= 16
               AND WS-CHK-Y >= 1 AND WS-CHK-Y <= 16
               IF WS-MAP-CELL(WS-CHK-Y, WS-CHK-X) = 9
                   MOVE 1 TO WS-GAME-WON
                   MOVE 1 TO WS-GAME-OVER
               END-IF
           END-IF.

       CHECK-DEATH.
           IF WS-HEALTH <= 0
               MOVE 1 TO WS-GAME-OVER
           END-IF.
```

**Step 3: Implement SHOW-END-SCREEN**

```cobol
       SHOW-END-SCREEN.
           DISPLAY WS-ANSI-CLEAR
           IF WS-GAME-WON = 1
               DISPLAY WS-ANSI-BGREEN
               DISPLAY " "
               DISPLAY "  =========================="
               DISPLAY "  = LEVEL COMPLETE!        ="
               DISPLAY "  = YOU SURVIVED!          ="
               DISPLAY "  =========================="
           ELSE
               DISPLAY WS-ANSI-RED
               DISPLAY " "
               DISPLAY "  =========================="
               DISPLAY "  = YOU DIED!              ="
               DISPLAY "  =========================="
           END-IF
           DISPLAY WS-ANSI-RESET
           DISPLAY " "
           DISPLAY "  Kills: " WS-KILLS "/" WS-TOTAL-ENEMIES
           DISPLAY "  Health: " WS-HEALTH
           DISPLAY "  Ammo remaining: " WS-AMMO
           DISPLAY " "
           DISPLAY "  Press any key to exit..."
           ACCEPT WS-KEY-CHAR FROM CONSOLE.
```

**Step 4: Update GAME-LOOP with all systems**

```cobol
       GAME-LOOP.
           PERFORM CAST-ALL-RAYS
           PERFORM RENDER-FRAME
           PERFORM RENDER-ENEMIES
           PERFORM RENDER-CROSSHAIR
           PERFORM RENDER-HUD
           PERFORM DRAW-FRAME
           ACCEPT WS-KEY-CHAR FROM CONSOLE
           PERFORM PROCESS-INPUT
           IF WS-GAME-OVER = 0
               PERFORM MOVE-ENEMIES
               PERFORM CHECK-ENEMY-ATTACKS
               PERFORM CHECK-WIN-CONDITION
               PERFORM CHECK-DEATH
           END-IF.
```

**Step 5: Update MAIN-PROGRAM**

```cobol
       MAIN-PROGRAM.
           PERFORM INIT-ANSI
           PERFORM INIT-TRIG
           PERFORM INIT-MAP
           PERFORM INIT-PLAYER
           PERFORM SPAWN-ENEMIES
           CALL "SYSTEM" USING WS-STTY-RAW
           DISPLAY WS-ANSI-CLEAR
           PERFORM GAME-LOOP UNTIL WS-GAME-OVER = 1
           CALL "SYSTEM" USING WS-STTY-SANE
           PERFORM SHOW-END-SCREEN
           STOP RUN.
```

**Step 6: Full integration test**

Run: `cobc -x -free doom.cob -o doom && ./doom`

Verify:
- [x] Walk around with WASD
- [x] Walls render in pseudo-3D with color shading
- [x] Enemies visible as red M characters
- [x] Enemies move toward player each turn
- [x] Shooting works (space bar)
- [x] HUD shows health/ammo/kills
- [x] Getting close to enemies causes damage
- [x] Reaching exit (bottom-right corner) shows win screen
- [x] Dying shows death screen
- [x] Q quits cleanly
- [x] Terminal restored properly on exit

**Step 7: Commit**

```
feat: add HUD, win/lose conditions — game complete
```

---

## Execution Order

Tasks are strictly sequential — each builds on the previous:

```
1 → 2 → 3 → 4 → 5 → 6 → 7 → 8 → 9 → 10 → 11 → 12
```

## Known Risks

1. **Input handling (Task 3)**: `stty raw` + ACCEPT may not work as expected. Fallback: `CALL "getchar"`. If neither works, use SCREEN SECTION with ncurses.
2. **Performance**: 120 rays × DDA steps per frame may be slow in COBOL. If so, reduce to 80 columns or increase step size.
3. **ANSI color output**: String building with embedded escape codes may need debugging. The WS-OUTPUT-LINE buffer (600 chars) should be large enough for 120 chars + color codes.
4. **ATAN edge cases**: Division by zero when enemy is directly north/south. Guard with `IF WS-ENEMY-DX NOT = 0`.
5. **GnuCOBOL version**: Some intrinsic functions (ATAN, SQRT) require GnuCOBOL 3.x+. The apt package should have this.
