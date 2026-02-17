      *> ============================================================
      *> hud-proc.cpy — HUD (heads-up display) rendering procedures
      *> Draws status bar with health, ammo, and face indicator.
      *> Call RENDER-HUD after wall/sprite rendering each frame.
      *> ============================================================

      *> ============================================================
      *> RENDER-HUD: Main entry point — draws the full status bar
      *> ============================================================
       RENDER-HUD.
      *>   Only draw HUD when playing or dead
           IF WS-GAME-STATE < 1
               EXIT PARAGRAPH
           END-IF

      *>   1. Draw status bar background (dark gray, rows 168-199)
           PERFORM DRAW-HUD-BACKGROUND

      *>   2. Draw border line at row 167
           PERFORM DRAW-HUD-BORDER

      *>   3. Draw "HEALTH" label at left side
           MOVE WS-HUD-TXT-R TO WS-HUD-CUR-R
           MOVE WS-HUD-TXT-G TO WS-HUD-CUR-G
           MOVE WS-HUD-TXT-B TO WS-HUD-CUR-B
           MOVE WS-HUD-HLBL-X TO WS-HUD-LBL-X
           PERFORM VARYING WS-HUD-LBL-I FROM 1 BY 1
               UNTIL WS-HUD-LBL-I > 6
               MOVE WS-HUD-HLTH-CI(WS-HUD-LBL-I)
                   TO WS-HUD-CHR-IDX
               MOVE WS-HUD-LBL-X TO WS-HUD-CHR-X
               MOVE WS-HUD-HLBL-Y TO WS-HUD-CHR-Y
               PERFORM DRAW-HUD-CHAR
               ADD 6 TO WS-HUD-LBL-X
           END-PERFORM

      *>   4. Determine health number color
           IF WS-HEALTH > 50
               MOVE WS-HUD-HP-HI-R TO WS-HUD-CUR-R
               MOVE WS-HUD-HP-HI-G TO WS-HUD-CUR-G
               MOVE WS-HUD-HP-HI-B TO WS-HUD-CUR-B
           ELSE
               IF WS-HEALTH > 25
                   MOVE WS-HUD-HP-MID-R TO WS-HUD-CUR-R
                   MOVE WS-HUD-HP-MID-G TO WS-HUD-CUR-G
                   MOVE WS-HUD-HP-MID-B TO WS-HUD-CUR-B
               ELSE
                   MOVE WS-HUD-HP-LO-R TO WS-HUD-CUR-R
                   MOVE WS-HUD-HP-LO-G TO WS-HUD-CUR-G
                   MOVE WS-HUD-HP-LO-B TO WS-HUD-CUR-B
               END-IF
           END-IF

      *>   5. Draw health number
           MOVE WS-HEALTH TO WS-HUD-NUM-VAL
           MOVE WS-HUD-HNUM-X TO WS-HUD-NUM-X
           MOVE WS-HUD-HNUM-Y TO WS-HUD-NUM-Y
           PERFORM DRAW-HUD-NUMBER

      *>   6. Draw "%" after health number
      *>      NUM-CUR-X holds the next X position after digits
           MOVE 19 TO WS-HUD-CHR-IDX
           MOVE WS-HUD-NUM-CUR-X TO WS-HUD-CHR-X
           MOVE WS-HUD-HNUM-Y TO WS-HUD-CHR-Y
           PERFORM DRAW-HUD-CHAR

      *>   7. Draw face indicator in center
           PERFORM DRAW-HUD-FACE

      *>   8. Draw "AMMO" label at right side
           MOVE WS-HUD-TXT-R TO WS-HUD-CUR-R
           MOVE WS-HUD-TXT-G TO WS-HUD-CUR-G
           MOVE WS-HUD-TXT-B TO WS-HUD-CUR-B
           MOVE WS-HUD-ALBL-X TO WS-HUD-LBL-X
           PERFORM VARYING WS-HUD-LBL-I FROM 1 BY 1
               UNTIL WS-HUD-LBL-I > 4
               MOVE WS-HUD-AMMO-CI(WS-HUD-LBL-I)
                   TO WS-HUD-CHR-IDX
               MOVE WS-HUD-LBL-X TO WS-HUD-CHR-X
               MOVE WS-HUD-ALBL-Y TO WS-HUD-CHR-Y
               PERFORM DRAW-HUD-CHAR
               ADD 6 TO WS-HUD-LBL-X
           END-PERFORM

      *>   9. Draw ammo number (yellow text color)
           MOVE WS-HUD-TXT-R TO WS-HUD-CUR-R
           MOVE WS-HUD-TXT-G TO WS-HUD-CUR-G
           MOVE WS-HUD-TXT-B TO WS-HUD-CUR-B
           MOVE WS-AMMO TO WS-HUD-NUM-VAL
           MOVE WS-HUD-ANUM-X TO WS-HUD-NUM-X
           MOVE WS-HUD-ANUM-Y TO WS-HUD-NUM-Y
           PERFORM DRAW-HUD-NUMBER
           .

      *> ============================================================
      *> DRAW-HUD-BACKGROUND: Fill rows 168-199 with dark gray RGBA
      *> 320 pixels x 32 rows = 10240 pixels, 4 bytes each
      *> ============================================================
       DRAW-HUD-BACKGROUND.
           PERFORM VARYING WS-HUD-DRW-Y
               FROM WS-HUD-BAR-Y BY 1
               UNTIL WS-HUD-DRW-Y > 199
               PERFORM VARYING WS-HUD-DRW-X
                   FROM 1 BY 1
                   UNTIL WS-HUD-DRW-X > 320
                   COMPUTE WS-HUD-FB-IDX =
                       ((WS-HUD-DRW-Y - 1) * 320
                       + (WS-HUD-DRW-X - 1)) * 4 + 1
                   MOVE WS-HUD-BG-R
                       TO WS-FB-BYTE(WS-HUD-FB-IDX)
                   ADD 1 TO WS-HUD-FB-IDX
                   MOVE WS-HUD-BG-G
                       TO WS-FB-BYTE(WS-HUD-FB-IDX)
                   ADD 1 TO WS-HUD-FB-IDX
                   MOVE WS-HUD-BG-B
                       TO WS-FB-BYTE(WS-HUD-FB-IDX)
                   ADD 1 TO WS-HUD-FB-IDX
                   MOVE X"FF"
                       TO WS-FB-BYTE(WS-HUD-FB-IDX)
               END-PERFORM
           END-PERFORM
           .

      *> ============================================================
      *> DRAW-HUD-BORDER: 1-pixel line at row 167, lighter gray
      *> ============================================================
       DRAW-HUD-BORDER.
           PERFORM VARYING WS-HUD-DRW-X
               FROM 1 BY 1
               UNTIL WS-HUD-DRW-X > 320
               COMPUTE WS-HUD-FB-IDX =
                   ((WS-HUD-BORDER-Y - 1) * 320
                   + (WS-HUD-DRW-X - 1)) * 4 + 1
               MOVE WS-HUD-BORD-R
                   TO WS-FB-BYTE(WS-HUD-FB-IDX)
               ADD 1 TO WS-HUD-FB-IDX
               MOVE WS-HUD-BORD-G
                   TO WS-FB-BYTE(WS-HUD-FB-IDX)
               ADD 1 TO WS-HUD-FB-IDX
               MOVE WS-HUD-BORD-B
                   TO WS-FB-BYTE(WS-HUD-FB-IDX)
               ADD 1 TO WS-HUD-FB-IDX
               MOVE X"FF"
                   TO WS-FB-BYTE(WS-HUD-FB-IDX)
           END-PERFORM
           .

      *> ============================================================
      *> DRAW-HUD-NUMBER: Draw a 1-3 digit number at (NUM-X, NUM-Y)
      *> Input: WS-HUD-NUM-VAL, WS-HUD-NUM-X/Y
      *>        WS-HUD-CUR-R/G/B = drawing color (raw bytes)
      *> Output: WS-HUD-NUM-CUR-X = next X after last digit
      *> Each digit is 5px wide + 1px spacing = 6px per digit.
      *> Leading zeros suppressed except for ones digit.
      *> ============================================================
       DRAW-HUD-NUMBER.
      *>   Decompose into digits
           DIVIDE WS-HUD-NUM-VAL BY 100
               GIVING WS-HUD-NUM-HUND
               REMAINDER WS-HUD-TEMP
           DIVIDE WS-HUD-TEMP BY 10
               GIVING WS-HUD-NUM-TENS
               REMAINDER WS-HUD-NUM-ONES

           MOVE WS-HUD-NUM-X TO WS-HUD-NUM-CUR-X
           MOVE 0 TO WS-HUD-NUM-LEAD

      *>   Hundreds digit (suppress leading zero)
           IF WS-HUD-NUM-HUND > 0
               COMPUTE WS-HUD-CHR-IDX =
                   WS-HUD-NUM-HUND + 1
               MOVE WS-HUD-NUM-CUR-X TO WS-HUD-CHR-X
               MOVE WS-HUD-NUM-Y TO WS-HUD-CHR-Y
               PERFORM DRAW-HUD-CHAR
               ADD 6 TO WS-HUD-NUM-CUR-X
               MOVE 1 TO WS-HUD-NUM-LEAD
           END-IF

      *>   Tens digit (suppress if zero and no hundreds)
           IF WS-HUD-NUM-TENS > 0
               OR WS-HUD-NUM-LEAD = 1
               COMPUTE WS-HUD-CHR-IDX =
                   WS-HUD-NUM-TENS + 1
               MOVE WS-HUD-NUM-CUR-X TO WS-HUD-CHR-X
               MOVE WS-HUD-NUM-Y TO WS-HUD-CHR-Y
               PERFORM DRAW-HUD-CHAR
               ADD 6 TO WS-HUD-NUM-CUR-X
           END-IF

      *>   Ones digit (always drawn)
           COMPUTE WS-HUD-CHR-IDX =
               WS-HUD-NUM-ONES + 1
           MOVE WS-HUD-NUM-CUR-X TO WS-HUD-CHR-X
           MOVE WS-HUD-NUM-Y TO WS-HUD-CHR-Y
           PERFORM DRAW-HUD-CHAR
           ADD 6 TO WS-HUD-NUM-CUR-X
           .

      *> ============================================================
      *> DRAW-HUD-CHAR: Draw one 5x7 bitmap character to FB
      *> Input: WS-HUD-CHR-IDX = font table index (1-19)
      *>        WS-HUD-CHR-X/Y = top-left pixel position (1-based)
      *>        WS-HUD-CUR-R/G/B = foreground color (raw bytes)
      *> Only writes pixels where bitmap has "1"; background is
      *> transparent (whatever was drawn before shows through).
      *> ============================================================
       DRAW-HUD-CHAR.
      *>   Validate char index
           IF WS-HUD-CHR-IDX < 1
               OR WS-HUD-CHR-IDX > WS-HUD-CHAR-COUNT
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-HUD-CHR-ROW FROM 0 BY 1
               UNTIL WS-HUD-CHR-ROW > 6
               PERFORM VARYING WS-HUD-CHR-COL FROM 0 BY 1
                   UNTIL WS-HUD-CHR-COL > 4

      *>           Bit position in the 35-char bitmap
                   COMPUTE WS-HUD-CHR-BIT =
                       WS-HUD-CHR-ROW * 5
                       + WS-HUD-CHR-COL + 1

                   MOVE WS-HUD-CHAR-BMP(
                       WS-HUD-CHR-IDX)
                       (WS-HUD-CHR-BIT:1)
                       TO WS-HUD-CHR-PIX

                   IF WS-HUD-CHR-PIX = "1"
      *>               Compute screen pixel position
                       COMPUTE WS-HUD-DRW-X =
                           WS-HUD-CHR-X
                           + WS-HUD-CHR-COL
                       COMPUTE WS-HUD-DRW-Y =
                           WS-HUD-CHR-Y
                           + WS-HUD-CHR-ROW

      *>               Bounds check
                       IF WS-HUD-DRW-X >= 1
                           AND WS-HUD-DRW-X <= 320
                           AND WS-HUD-DRW-Y >= 1
                           AND WS-HUD-DRW-Y <= 200

      *>                   FB offset for (x,y)
                           COMPUTE WS-HUD-FB-IDX =
                               ((WS-HUD-DRW-Y - 1)
                               * 320
                               + (WS-HUD-DRW-X - 1))
                               * 4 + 1
                           MOVE WS-HUD-CUR-R
                               TO WS-FB-BYTE(
                               WS-HUD-FB-IDX)
                           ADD 1 TO WS-HUD-FB-IDX
                           MOVE WS-HUD-CUR-G
                               TO WS-FB-BYTE(
                               WS-HUD-FB-IDX)
                           ADD 1 TO WS-HUD-FB-IDX
                           MOVE WS-HUD-CUR-B
                               TO WS-FB-BYTE(
                               WS-HUD-FB-IDX)
                           ADD 1 TO WS-HUD-FB-IDX
                           MOVE X"FF"
                               TO WS-FB-BYTE(
                               WS-HUD-FB-IDX)
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
           .

      *> ============================================================
      *> DRAW-HUD-RECT: Fill a rectangle with a solid color
      *> Input: WS-HUD-RECT-X/Y = top-left (1-based)
      *>        WS-HUD-RECT-W/H = width/height in pixels
      *>        WS-HUD-CUR-R/G/B = fill color (raw bytes)
      *> ============================================================
       DRAW-HUD-RECT.
           PERFORM VARYING WS-HUD-RECT-RY FROM 0 BY 1
               UNTIL WS-HUD-RECT-RY >= WS-HUD-RECT-H
               COMPUTE WS-HUD-RECT-PY =
                   WS-HUD-RECT-Y + WS-HUD-RECT-RY
               IF WS-HUD-RECT-PY >= 1
                   AND WS-HUD-RECT-PY <= 200
                   PERFORM VARYING WS-HUD-RECT-RX
                       FROM 0 BY 1
                       UNTIL WS-HUD-RECT-RX
                           >= WS-HUD-RECT-W
                       COMPUTE WS-HUD-RECT-PX =
                           WS-HUD-RECT-X
                           + WS-HUD-RECT-RX
                       IF WS-HUD-RECT-PX >= 1
                           AND WS-HUD-RECT-PX <= 320
                           COMPUTE WS-HUD-FB-IDX =
                               ((WS-HUD-RECT-PY - 1)
                               * 320
                               + (WS-HUD-RECT-PX - 1))
                               * 4 + 1
                           MOVE WS-HUD-CUR-R
                               TO WS-FB-BYTE(
                               WS-HUD-FB-IDX)
                           ADD 1 TO WS-HUD-FB-IDX
                           MOVE WS-HUD-CUR-G
                               TO WS-FB-BYTE(
                               WS-HUD-FB-IDX)
                           ADD 1 TO WS-HUD-FB-IDX
                           MOVE WS-HUD-CUR-B
                               TO WS-FB-BYTE(
                               WS-HUD-FB-IDX)
                           ADD 1 TO WS-HUD-FB-IDX
                           MOVE X"FF"
                               TO WS-FB-BYTE(
                               WS-HUD-FB-IDX)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> DRAW-HUD-FACE: Draw the face indicator (colored square)
      *> Color based on health: green > 66, yellow 34-66, red < 34
      *> Surrounded by a 1-pixel dark border.
      *> ============================================================
       DRAW-HUD-FACE.
      *>   Draw dark border rectangle (full face area)
           MOVE WS-HUD-FACE-BRD-R TO WS-HUD-CUR-R
           MOVE WS-HUD-FACE-BRD-G TO WS-HUD-CUR-G
           MOVE WS-HUD-FACE-BRD-B TO WS-HUD-CUR-B
           MOVE WS-HUD-FACE-X TO WS-HUD-RECT-X
           MOVE WS-HUD-FACE-Y TO WS-HUD-RECT-Y
           MOVE WS-HUD-FACE-SZ TO WS-HUD-RECT-W
           MOVE WS-HUD-FACE-SZ TO WS-HUD-RECT-H
           PERFORM DRAW-HUD-RECT

      *>   Pick face color based on health level
           IF WS-HEALTH > 66
               MOVE WS-HUD-FACE-HI-R TO WS-HUD-CUR-R
               MOVE WS-HUD-FACE-HI-G TO WS-HUD-CUR-G
               MOVE WS-HUD-FACE-HI-B TO WS-HUD-CUR-B
           ELSE
               IF WS-HEALTH > 33
                   MOVE WS-HUD-FACE-MID-R
                       TO WS-HUD-CUR-R
                   MOVE WS-HUD-FACE-MID-G
                       TO WS-HUD-CUR-G
                   MOVE WS-HUD-FACE-MID-B
                       TO WS-HUD-CUR-B
               ELSE
                   MOVE WS-HUD-FACE-LO-R
                       TO WS-HUD-CUR-R
                   MOVE WS-HUD-FACE-LO-G
                       TO WS-HUD-CUR-G
                   MOVE WS-HUD-FACE-LO-B
                       TO WS-HUD-CUR-B
               END-IF
           END-IF

      *>   Draw the filled interior (inset 1px from border)
           COMPUTE WS-HUD-RECT-X = WS-HUD-FACE-X + 1
           COMPUTE WS-HUD-RECT-Y = WS-HUD-FACE-Y + 1
           COMPUTE WS-HUD-RECT-W = WS-HUD-FACE-SZ - 2
           COMPUTE WS-HUD-RECT-H = WS-HUD-FACE-SZ - 2
           PERFORM DRAW-HUD-RECT
           .

      *> ============================================================
      *> SHOW-TITLE-SCREEN: Display title until SPACE pressed
      *> Draws "COBOL DOOM 2" at 3x scale, subtitle at 1x scale
      *> ============================================================
       SHOW-TITLE-SCREEN.
      *>   Clear framebuffer to black
           MOVE LOW-VALUES TO WS-FRAMEBUFFER

      *>   --- Draw "COBOL DOOM 2" centered at 3x scale ---
      *>   Each char is 5*3=15 wide + 3px space = 18px per char
      *>   12 chars => 12*18 - 3 = 213 pixels total
      *>   Start X = (320-213)/2 = 53
           MOVE 53 TO WS-TTL-CUR-X
           MOVE 60 TO WS-TTL-CUR-Y
           PERFORM VARYING WS-TTL-I FROM 1 BY 1
               UNTIL WS-TTL-I > WS-TTL-TITLE-LEN
               IF WS-TTL-TITLE-CI(WS-TTL-I) = 0
      *>           Space character: just advance X
                   ADD 18 TO WS-TTL-CUR-X
               ELSE
      *>           Draw scaled character
                   MOVE WS-TTL-TITLE-CI(WS-TTL-I)
                       TO WS-HUD-CHR-IDX
                   IF WS-HUD-CHR-IDX >= 1
                       AND WS-HUD-CHR-IDX
                       <= WS-HUD-CHAR-COUNT
                       PERFORM DRAW-TITLE-CHAR-3X
                   END-IF
                   ADD 18 TO WS-TTL-CUR-X
               END-IF
           END-PERFORM

      *>   --- Draw "PRESS SPACE TO START" at 1x ---
      *>   20 chars => 20*6 - 1 = 119 pixels total
      *>   Start X = (320-119)/2 = 100
           MOVE 100 TO WS-TTL-CUR-X
           MOVE 120 TO WS-TTL-CUR-Y
           MOVE WS-TTL-SUB-R TO WS-HUD-CUR-R
           MOVE WS-TTL-SUB-G TO WS-HUD-CUR-G
           MOVE WS-TTL-SUB-B TO WS-HUD-CUR-B
           PERFORM VARYING WS-TTL-I FROM 1 BY 1
               UNTIL WS-TTL-I > WS-TTL-SUB-LEN
               IF WS-TTL-SUB-CI(WS-TTL-I) = 0
                   ADD 6 TO WS-TTL-CUR-X
               ELSE
                   MOVE WS-TTL-SUB-CI(WS-TTL-I)
                       TO WS-HUD-CHR-IDX
                   MOVE WS-TTL-CUR-X TO WS-HUD-CHR-X
                   MOVE WS-TTL-CUR-Y TO WS-HUD-CHR-Y
                   IF WS-HUD-CHR-IDX >= 1
                       AND WS-HUD-CHR-IDX
                       <= WS-HUD-CHAR-COUNT
                       PERFORM DRAW-HUD-CHAR
                   END-IF
                   ADD 6 TO WS-TTL-CUR-X
               END-IF
           END-PERFORM

      *>   Display title frame
           CALL "sdl_frame" USING WS-FRAMEBUFFER
                                  WS-SCREEN-W
                                  WS-SCREEN-H

      *>   Wait for SPACE key (WS-KEY(6))
           PERFORM UNTIL WS-KEY(6) NOT = 0
               OR WS-KEY(1) = -1
               CALL "sdl_input" USING WS-KEYS
           END-PERFORM

      *>   Check if user quit during title
           IF WS-KEY(1) = -1
               MOVE 0 TO WS-RUNNING
           END-IF
           .

      *> ============================================================
      *> DRAW-TITLE-CHAR-3X: Draw one font char at 3x scale
      *> Uses WS-HUD-CHR-IDX, WS-TTL-CUR-X/Y
      *> Color: WS-TTL-R/G/B (red)
      *> ============================================================
       DRAW-TITLE-CHAR-3X.
           IF WS-HUD-CHR-IDX < 1
               OR WS-HUD-CHR-IDX > WS-HUD-CHAR-COUNT
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-TTL-ROW FROM 0 BY 1
               UNTIL WS-TTL-ROW > 6
               PERFORM VARYING WS-TTL-COL FROM 0 BY 1
                   UNTIL WS-TTL-COL > 4

                   COMPUTE WS-TTL-BIT =
                       WS-TTL-ROW * 5
                       + WS-TTL-COL + 1

                   MOVE WS-HUD-CHAR-BMP(
                       WS-HUD-CHR-IDX)
                       (WS-TTL-BIT:1)
                       TO WS-TTL-PIX

                   IF WS-TTL-PIX = "1"
      *>               Draw a 3x3 block for this pixel
                       PERFORM VARYING WS-TTL-CHR-SY
                           FROM 0 BY 1
                           UNTIL WS-TTL-CHR-SY >= 3
                           PERFORM VARYING
                               WS-TTL-CHR-SX
                               FROM 0 BY 1
                               UNTIL WS-TTL-CHR-SX
                               >= 3
                               COMPUTE WS-TTL-PIX-X =
                                   WS-TTL-CUR-X
                                   + WS-TTL-COL * 3
                                   + WS-TTL-CHR-SX
                               COMPUTE WS-TTL-PIX-Y =
                                   WS-TTL-CUR-Y
                                   + WS-TTL-ROW * 3
                                   + WS-TTL-CHR-SY
                               IF WS-TTL-PIX-X >= 1
                                   AND WS-TTL-PIX-X
                                   <= 320
                                   AND WS-TTL-PIX-Y
                                   >= 1
                                   AND WS-TTL-PIX-Y
                                   <= 200
                                   COMPUTE
                                   WS-TTL-FB-IDX =
                                   ((WS-TTL-PIX-Y
                                   - 1) * 320
                                   + (WS-TTL-PIX-X
                                   - 1)) * 4 + 1
                                   MOVE WS-TTL-R
                                   TO WS-FB-BYTE(
                                   WS-TTL-FB-IDX)
                                   ADD 1 TO
                                   WS-TTL-FB-IDX
                                   MOVE WS-TTL-G
                                   TO WS-FB-BYTE(
                                   WS-TTL-FB-IDX)
                                   ADD 1 TO
                                   WS-TTL-FB-IDX
                                   MOVE WS-TTL-B
                                   TO WS-FB-BYTE(
                                   WS-TTL-FB-IDX)
                                   ADD 1 TO
                                   WS-TTL-FB-IDX
                                   MOVE X"FF"
                                   TO WS-FB-BYTE(
                                   WS-TTL-FB-IDX)
                               END-IF
                           END-PERFORM
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-PERFORM
           .

      *> ============================================================
      *> CHECK-LEVEL-EXIT: Check if player reached exit area
      *> Exit if player is at far edge: row < 5, col > 120
      *> Sets WS-GAME-STATE = 2 (won) and shows message
      *> ============================================================
       CHECK-LEVEL-EXIT.
      *>   Compute player grid position
           COMPUTE WS-EXIT-CHK-ROW =
               FUNCTION INTEGER-PART(WS-PY) + 1
           COMPUTE WS-EXIT-CHK-COL =
               FUNCTION INTEGER-PART(WS-PX) + 1

      *>   Check exit area: far edge of map
           IF WS-EXIT-CHK-ROW >= 1
               AND WS-EXIT-CHK-ROW < 5
               AND WS-EXIT-CHK-COL > 120
               AND WS-EXIT-CHK-COL <= WS-MAP-SIZE
               MOVE 2 TO WS-GAME-STATE
               MOVE 0 TO WS-RUNNING
           END-IF
           .
