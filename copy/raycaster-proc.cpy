      *> ============================================================
      *> raycaster-proc.cpy — Textured wall raycaster procedures
      *> Uses map-loader grid: MC-TYPE(row,col), MC-TEX-ID(row,col)
      *> ============================================================

      *> ============================================================
      *> INIT-TRIG: Build sin/cos tables (3600 entries, 0.1 deg)
      *> ============================================================
       INIT-TRIG.
           PERFORM VARYING WS-TRIG-IDX FROM 1 BY 1
               UNTIL WS-TRIG-IDX > 3600
               COMPUTE WS-TRIG-RADIANS =
                   (WS-TRIG-IDX - 1) * WS-PI / 1800
               COMPUTE WS-SIN-VAL(WS-TRIG-IDX) =
                   FUNCTION SIN(WS-TRIG-RADIANS)
               COMPUTE WS-COS-VAL(WS-TRIG-IDX) =
                   FUNCTION COS(WS-TRIG-RADIANS)
           END-PERFORM
           .

      *> ============================================================
      *> INIT-MAP: Populate map grid from hardcoded 16x16 test data
      *> Sets WS-MAP-SIZE = 16 for the fallback map
      *> ============================================================
       INIT-MAP.
           MOVE 16 TO WS-MAP-SIZE
      *>   Clear entire grid
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 128
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > 128
                   MOVE 0 TO MC-TYPE(WS-I, WS-J)
                   MOVE 0 TO MC-TEX-ID(WS-I, WS-J)
               END-PERFORM
           END-PERFORM
      *>   Copy 16x16 hardcoded data into grid
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 16
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > 16
                   IF WS-MIDR-CELL(WS-I, WS-J) > 0
                       MOVE 1 TO MC-TYPE(WS-I, WS-J)
                       MOVE WS-MIDR-CELL(WS-I, WS-J)
                           TO MC-TEX-ID(WS-I, WS-J)
                   END-IF
               END-PERFORM
           END-PERFORM
           .

      *> ============================================================
      *> INIT-PLAYER: Set spawn position and initial state
      *> Uses WAD spawn point if available, else hardcoded
      *> ============================================================
       INIT-PLAYER.
           IF WS-USE-WAD-MAP = 1
               AND WS-SPAWN-X NOT = 0
               MOVE WS-SPAWN-X TO WS-PX
               MOVE WS-SPAWN-Y TO WS-PY
               MOVE WS-SPAWN-ANGLE TO WS-PA
           ELSE
               MOVE +00002.5000 TO WS-PX
               MOVE +00002.5000 TO WS-PY
               MOVE 0 TO WS-PA
           END-IF
           MOVE 100 TO WS-HEALTH
           MOVE 50 TO WS-AMMO
           .

      *> ============================================================
      *> CLEAR-FRAMEBUFFER: Fill with zeroes (black)
      *> ============================================================
       CLEAR-FRAMEBUFFER.
           MOVE LOW-VALUES TO WS-FRAMEBUFFER
           .

      *> ============================================================
      *> CAST-ALL-RAYS: Loop 320 screen columns
      *> ============================================================
       CAST-ALL-RAYS.
           PERFORM VARYING WS-CUR-COL FROM 1 BY 1
               UNTIL WS-CUR-COL > WS-SCREEN-W
               PERFORM CAST-ONE-RAY
           END-PERFORM
           .

      *> ============================================================
      *> CAST-ONE-RAY: DDA raycasting with textured wall rendering
      *> Floor/ceiling drawn by RENDER-FLOOR-CEILING (called first)
      *> ============================================================
       CAST-ONE-RAY.
      *>   --- Calculate ray angle for this column ---
           COMPUTE WS-RAY-ANGLE-F =
               WS-PA - WS-HALF-FOV
               + (WS-CUR-COL - 1) * WS-FOV / WS-SCREEN-W
           COMPUTE WS-RAY-ANGLE =
               WS-RAY-ANGLE-F * 10

      *>   --- Normalize angle to 0..3599 ---
           COMPUTE WS-ANGLE-LOOKUP =
               FUNCTION MOD(WS-RAY-ANGLE + 36000, 3600) + 1
           MOVE WS-COS-VAL(WS-ANGLE-LOOKUP)
               TO WS-RAY-DIR-X
           MOVE WS-SIN-VAL(WS-ANGLE-LOOKUP)
               TO WS-RAY-DIR-Y

      *>   --- Starting map cell (1-based) ---
           COMPUTE WS-MAP-X =
               FUNCTION INTEGER-PART(WS-PX) + 1
           COMPUTE WS-MAP-Y =
               FUNCTION INTEGER-PART(WS-PY) + 1

      *>   --- Delta distances ---
           IF WS-RAY-DIR-X NOT = 0
               COMPUTE WS-DELTA-DIST-X =
                   FUNCTION ABS(1.0 / WS-RAY-DIR-X)
           ELSE
               MOVE 999999.0 TO WS-DELTA-DIST-X
           END-IF
           IF WS-RAY-DIR-Y NOT = 0
               COMPUTE WS-DELTA-DIST-Y =
                   FUNCTION ABS(1.0 / WS-RAY-DIR-Y)
           ELSE
               MOVE 999999.0 TO WS-DELTA-DIST-Y
           END-IF

      *>   --- Step direction and initial side distances ---
           IF WS-RAY-DIR-X < 0
               MOVE -1 TO WS-STEP-X
               COMPUTE WS-SIDE-DIST-X =
                   (WS-PX - FUNCTION INTEGER-PART(WS-PX))
                   * WS-DELTA-DIST-X
           ELSE
               MOVE 1 TO WS-STEP-X
               COMPUTE WS-SIDE-DIST-X =
                   (FUNCTION INTEGER-PART(WS-PX) + 1.0
                   - WS-PX)
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
                   (FUNCTION INTEGER-PART(WS-PY) + 1.0
                   - WS-PY)
                   * WS-DELTA-DIST-Y
           END-IF

      *>   --- DDA loop ---
           MOVE 0 TO WS-HIT
           MOVE 0 TO WS-STEP-COUNT
           MOVE 0 TO WS-HIT-TEX-ID
           PERFORM UNTIL WS-HIT = 1
               OR WS-STEP-COUNT >= WS-MAX-STEPS
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
      *>       Bounds check
               IF WS-MAP-X >= 1 AND WS-MAP-X <= WS-MAP-SIZE
                   AND WS-MAP-Y >= 1
                   AND WS-MAP-Y <= WS-MAP-SIZE
                   IF MC-TYPE(WS-MAP-Y, WS-MAP-X) > 0
                       MOVE 1 TO WS-HIT
                       MOVE MC-TEX-ID(WS-MAP-Y, WS-MAP-X)
                           TO WS-HIT-TEX-ID
                   END-IF
               ELSE
                   MOVE 1 TO WS-HIT
                   MOVE 1 TO WS-HIT-TEX-ID
               END-IF
           END-PERFORM

      *>   --- Perpendicular distance (fisheye correction) ---
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

      *>   --- Store depth ---
           MOVE WS-PERP-DIST TO WS-DEPTH-VAL(WS-CUR-COL)

      *>   --- Wall height on screen ---
           COMPUTE WS-WALL-H-FULL =
               WS-SCREEN-H / WS-PERP-DIST
           COMPUTE WS-WALL-H = WS-WALL-H-FULL

      *>   --- Wall top/bottom rows ---
           COMPUTE WS-HALF-H = WS-SCREEN-H / 2
           COMPUTE WS-WALL-TOP =
               WS-HALF-H - (WS-WALL-H / 2) + 1
           IF WS-WALL-TOP < 1
               MOVE 1 TO WS-WALL-TOP
           END-IF
           COMPUTE WS-WALL-BOT =
               WS-HALF-H + (WS-WALL-H / 2)
           IF WS-WALL-BOT > WS-SCREEN-H
               MOVE WS-SCREEN-H TO WS-WALL-BOT
           END-IF

      *>   --- Texture U coordinate ---
           IF WS-SIDE = 0
               COMPUTE WS-WALL-HIT-POS =
                   WS-PY + WS-PERP-DIST * WS-RAY-DIR-Y
           ELSE
               COMPUTE WS-WALL-HIT-POS =
                   WS-PX + WS-PERP-DIST * WS-RAY-DIR-X
           END-IF
           COMPUTE WS-WALL-HIT-POS =
               WS-WALL-HIT-POS
               - FUNCTION INTEGER-PART(WS-WALL-HIT-POS)

      *>   Clamp texture ID
           IF WS-HIT-TEX-ID > WS-TEX-COUNT
               COMPUTE WS-HIT-TEX-ID =
                   FUNCTION MOD(WS-HIT-TEX-ID - 1,
                   WS-TEX-COUNT) + 1
           END-IF
           IF WS-HIT-TEX-ID < 1
               MOVE 1 TO WS-HIT-TEX-ID
           END-IF

      *>   Map hit position to texture column (U)
           COMPUTE WS-TEX-U =
               WS-WALL-HIT-POS
               * WT-WIDTH(WS-HIT-TEX-ID)
           COMPUTE WS-TEX-U =
               FUNCTION MOD(WS-TEX-U,
               WT-WIDTH(WS-HIT-TEX-ID))

      *>   --- Lighting from distance ---
           COMPUTE WS-LIGHT-LEVEL =
               WS-PERP-DIST * 2
           IF WS-LIGHT-LEVEL < 0
               MOVE 0 TO WS-LIGHT-LEVEL
           END-IF
           IF WS-LIGHT-LEVEL > 31
               MOVE 31 TO WS-LIGHT-LEVEL
           END-IF
           IF WS-SIDE = 1
               ADD 2 TO WS-LIGHT-LEVEL
               IF WS-LIGHT-LEVEL > 31
                   MOVE 31 TO WS-LIGHT-LEVEL
               END-IF
           END-IF
           COMPUTE WS-LIGHT-TABLE = WS-LIGHT-LEVEL + 1

      *>   --- Draw textured wall pixels ---
           PERFORM VARYING WS-ROW FROM WS-WALL-TOP BY 1
               UNTIL WS-ROW > WS-WALL-BOT
      *>       V coordinate
               IF WS-WALL-H > 0
                   COMPUTE WS-V-FRAC =
                       (WS-ROW - WS-WALL-TOP)
                       * WT-HEIGHT(WS-HIT-TEX-ID)
                       / WS-WALL-H-FULL
                   COMPUTE WS-TEX-V =
                       FUNCTION INTEGER-PART(WS-V-FRAC)
               ELSE
                   MOVE 0 TO WS-TEX-V
               END-IF
               IF WS-TEX-V >= WT-HEIGHT(WS-HIT-TEX-ID)
                   COMPUTE WS-TEX-V =
                       WT-HEIGHT(WS-HIT-TEX-ID) - 1
               END-IF

      *>       Column-major pixel offset
               COMPUTE WS-TEX-PIX-OFF =
                   WS-TEX-U
                   * WT-HEIGHT(WS-HIT-TEX-ID)
                   + WS-TEX-V + 1
               IF WS-TEX-PIX-OFF < 1
                   MOVE 1 TO WS-TEX-PIX-OFF
               END-IF
               IF WS-TEX-PIX-OFF > 16384
                   MOVE 1 TO WS-TEX-PIX-OFF
               END-IF

      *>       Get palette index from texture
               MOVE WT-PIX(WS-HIT-TEX-ID,
                   WS-TEX-PIX-OFF)
                   TO WS-BIN-BUF1
               MOVE WS-BIN-BYTE TO WS-PAL-IDX
               ADD 1 TO WS-PAL-IDX
               IF WS-PAL-IDX > 256
                   MOVE 1 TO WS-PAL-IDX
               END-IF

      *>       Apply lighting via colormap
               MOVE WS-CMAP-ENTRY(WS-LIGHT-TABLE,
                   WS-PAL-IDX)
                   TO WS-BIN-BUF1
               MOVE WS-BIN-BYTE TO WS-LIT-PAL-IDX
               ADD 1 TO WS-LIT-PAL-IDX
               IF WS-LIT-PAL-IDX > 256
                   MOVE 1 TO WS-LIT-PAL-IDX
               END-IF

      *>       Get RGB from palette
               MOVE WS-PAL-R(WS-LIT-PAL-IDX) TO WS-PIX-R
               MOVE WS-PAL-G(WS-LIT-PAL-IDX) TO WS-PIX-G
               MOVE WS-PAL-B(WS-LIT-PAL-IDX) TO WS-PIX-B

      *>       Write RGBA to framebuffer
               COMPUTE WS-PIX-IDX =
                   ((WS-ROW - 1) * 320
                   + (WS-CUR-COL - 1)) * 4 + 1
               MOVE WS-PIX-R TO WS-FB-BYTE(WS-PIX-IDX)
               ADD 1 TO WS-PIX-IDX
               MOVE WS-PIX-G TO WS-FB-BYTE(WS-PIX-IDX)
               ADD 1 TO WS-PIX-IDX
               MOVE WS-PIX-B TO WS-FB-BYTE(WS-PIX-IDX)
               ADD 1 TO WS-PIX-IDX
               MOVE X"FF" TO WS-FB-BYTE(WS-PIX-IDX)
           END-PERFORM
           .

      *> ============================================================
      *> PROCESS-MOVEMENT: Handle WASD input
      *> WS-KEY(2)=W, WS-KEY(3)=S, WS-KEY(4)=A, WS-KEY(5)=D
      *> ============================================================
       PROCESS-MOVEMENT.
      *>   Rotate left (A)
           IF WS-KEY(4) NOT = 0
               SUBTRACT WS-TURN-SPEED FROM WS-PA
               IF WS-PA < 0
                   ADD 360 TO WS-PA
               END-IF
           END-IF

      *>   Rotate right (D)
           IF WS-KEY(5) NOT = 0
               ADD WS-TURN-SPEED TO WS-PA
               IF WS-PA >= 360
                   SUBTRACT 360 FROM WS-PA
               END-IF
           END-IF

      *>   Forward (W)
           IF WS-KEY(2) NOT = 0
               COMPUTE WS-ANGLE-LOOKUP =
                   FUNCTION MOD(WS-PA * 10 + 36000, 3600)
                   + 1
               COMPUTE WS-NEW-X =
                   WS-PX + WS-COS-VAL(WS-ANGLE-LOOKUP)
                   * WS-MOVE-SPEED
               COMPUTE WS-NEW-Y =
                   WS-PY + WS-SIN-VAL(WS-ANGLE-LOOKUP)
                   * WS-MOVE-SPEED
               PERFORM CHECK-WALL-COLLISION
           END-IF

      *>   Backward (S)
           IF WS-KEY(3) NOT = 0
               COMPUTE WS-ANGLE-LOOKUP =
                   FUNCTION MOD(WS-PA * 10 + 36000, 3600)
                   + 1
               COMPUTE WS-NEW-X =
                   WS-PX - WS-COS-VAL(WS-ANGLE-LOOKUP)
                   * WS-MOVE-SPEED
               COMPUTE WS-NEW-Y =
                   WS-PY - WS-SIN-VAL(WS-ANGLE-LOOKUP)
                   * WS-MOVE-SPEED
               PERFORM CHECK-WALL-COLLISION
           END-IF
           .

      *> ============================================================
      *> CHECK-WALL-COLLISION: Slide-style X/Y separate checking
      *> Uses MC-TYPE for wall detection
      *> ============================================================
       CHECK-WALL-COLLISION.
      *>   Check X axis
           IF WS-NEW-X > WS-PX
               COMPUTE WS-CHK-X =
                   FUNCTION INTEGER-PART(
                   WS-NEW-X + WS-COLLISION-PAD) + 1
           ELSE
               COMPUTE WS-CHK-X =
                   FUNCTION INTEGER-PART(
                   WS-NEW-X - WS-COLLISION-PAD) + 1
           END-IF
           COMPUTE WS-CHK-Y =
               FUNCTION INTEGER-PART(WS-PY) + 1
           IF WS-CHK-X >= 1 AND WS-CHK-X <= WS-MAP-SIZE
               AND WS-CHK-Y >= 1 AND WS-CHK-Y <= WS-MAP-SIZE
               IF MC-TYPE(WS-CHK-Y, WS-CHK-X) = 0
                   MOVE WS-NEW-X TO WS-PX
               END-IF
           END-IF

      *>   Check Y axis
           COMPUTE WS-CHK-X =
               FUNCTION INTEGER-PART(WS-PX) + 1
           IF WS-NEW-Y > WS-PY
               COMPUTE WS-CHK-Y =
                   FUNCTION INTEGER-PART(
                   WS-NEW-Y + WS-COLLISION-PAD) + 1
           ELSE
               COMPUTE WS-CHK-Y =
                   FUNCTION INTEGER-PART(
                   WS-NEW-Y - WS-COLLISION-PAD) + 1
           END-IF
           IF WS-CHK-X >= 1 AND WS-CHK-X <= WS-MAP-SIZE
               AND WS-CHK-Y >= 1 AND WS-CHK-Y <= WS-MAP-SIZE
               IF MC-TYPE(WS-CHK-Y, WS-CHK-X) = 0
                   MOVE WS-NEW-Y TO WS-PY
               END-IF
           END-IF
           .
