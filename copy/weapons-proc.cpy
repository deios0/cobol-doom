      *> ============================================================
      *> weapons-proc.cpy -- Weapon system procedures
      *> INIT-WEAPONS:     Set up default weapon and load sprite
      *> PROCESS-FIRE:     Per-frame fire logic with input tracking
      *> FIRE-HITSCAN:     Ray check against enemy sprites
      *> RENDER-WEAPON:    Draw weapon sprite overlay on framebuffer
      *> RENDER-CROSSHAIR: Draw small cross at screen center
      *> ============================================================

      *> ============================================================
      *> INIT-WEAPONS: Set current weapon to pistol (1), state to
      *> ready (0), and load the pistol idle sprite "PISGA0  ".
      *> ============================================================
       INIT-WEAPONS.
           MOVE 1 TO WS-WP-CURRENT
           MOVE 0 TO WS-WP-STATE
           MOVE 0 TO WS-WP-FIRE-TIMER
           MOVE 0 TO WS-WP-COOLDOWN-TIMER
           MOVE 0 TO WS-WP-ANIM-FRAME
           MOVE 0 TO WS-WP-FIRE-PREV
           MOVE 0 TO WS-WP-FIRE-EDGE

      *>   Load pistol idle sprite from WAD
           MOVE "PISGA0  " TO WS-FIND-NAME
           PERFORM FIND-LUMP
           IF WS-FOUND = 1
               MOVE WS-FOUND-OFFSET TO WS-WAD-OFFSET
               MOVE WS-FOUND-SIZE TO WS-WAD-RD-SIZE
               IF WS-WAD-RD-SIZE > 16384
                   MOVE 16384 TO WS-WAD-RD-SIZE
               END-IF
               CALL "wad_read" USING WS-WAD-OFFSET
                                     WS-WAD-RD-SIZE
                                     WS-WP-PATCH-BUF
      *>       Parse patch header
               MOVE WS-WP-PATCH-BUF(1:2) TO WS-BIN-BUF2
               MOVE WS-BIN-INT16 TO WS-WP-PATCH-W
               MOVE WS-WP-PATCH-BUF(3:2) TO WS-BIN-BUF2
               MOVE WS-BIN-INT16 TO WS-WP-PATCH-H
               MOVE WS-WP-PATCH-BUF(5:2) TO WS-BIN-BUF2
               MOVE WS-BIN-INT16 TO WS-WP-PATCH-LEFT
               MOVE WS-WP-PATCH-BUF(7:2) TO WS-BIN-BUF2
               MOVE WS-BIN-INT16 TO WS-WP-PATCH-TOP
               DISPLAY "Weapon loaded: PISGA0 "
                   WS-WP-PATCH-W "x" WS-WP-PATCH-H
           ELSE
               DISPLAY "WARNING: PISGA0 not found in WAD"
               MOVE 0 TO WS-WP-PATCH-W
               MOVE 0 TO WS-WP-PATCH-H
           END-IF
           .

      *> ============================================================
      *> LOAD-WEAPON-FRAME: Load a weapon sprite lump by name.
      *> Input: WS-WP-LUMP-NAME (8 chars, space-padded)
      *> Output: WS-WP-PATCH-BUF, WS-WP-PATCH-W/H/LEFT/TOP
      *> ============================================================
       LOAD-WEAPON-FRAME.
           MOVE WS-WP-LUMP-NAME TO WS-FIND-NAME
           PERFORM FIND-LUMP
           IF WS-FOUND = 1
               MOVE WS-FOUND-OFFSET TO WS-WAD-OFFSET
               MOVE WS-FOUND-SIZE TO WS-WAD-RD-SIZE
               IF WS-WAD-RD-SIZE > 16384
                   MOVE 16384 TO WS-WAD-RD-SIZE
               END-IF
               CALL "wad_read" USING WS-WAD-OFFSET
                                     WS-WAD-RD-SIZE
                                     WS-WP-PATCH-BUF
      *>       Parse patch header (8 bytes)
               MOVE WS-WP-PATCH-BUF(1:2) TO WS-BIN-BUF2
               MOVE WS-BIN-INT16 TO WS-WP-PATCH-W
               MOVE WS-WP-PATCH-BUF(3:2) TO WS-BIN-BUF2
               MOVE WS-BIN-INT16 TO WS-WP-PATCH-H
               MOVE WS-WP-PATCH-BUF(5:2) TO WS-BIN-BUF2
               MOVE WS-BIN-INT16 TO WS-WP-PATCH-LEFT
               MOVE WS-WP-PATCH-BUF(7:2) TO WS-BIN-BUF2
               MOVE WS-BIN-INT16 TO WS-WP-PATCH-TOP
           ELSE
               MOVE 0 TO WS-WP-PATCH-W
               MOVE 0 TO WS-WP-PATCH-H
           END-IF
      *>   Sanity check dimensions
           IF WS-WP-PATCH-W < 1 OR WS-WP-PATCH-W > 320
               MOVE 0 TO WS-WP-PATCH-W
           END-IF
           IF WS-WP-PATCH-H < 1 OR WS-WP-PATCH-H > 200
               MOVE 0 TO WS-WP-PATCH-H
           END-IF
           .

      *> ============================================================
      *> PROCESS-FIRE: Called each frame. Handles fire input,
      *> weapon state machine, animation frame progression,
      *> and weapon switching via keys 1/2 (WS-KEY(9)/WS-KEY(10)).
      *> ============================================================
       PROCESS-FIRE.
      *>   --- Detect fire key rising edge ---
      *>   WS-KEY(6) = SPACE key
           IF WS-KEY(6) NOT = 0
               IF WS-WP-FIRE-PREV = 0
                   MOVE 1 TO WS-WP-FIRE-EDGE
               ELSE
                   MOVE 0 TO WS-WP-FIRE-EDGE
               END-IF
               MOVE 1 TO WS-WP-FIRE-PREV
           ELSE
               MOVE 0 TO WS-WP-FIRE-PREV
               MOVE 0 TO WS-WP-FIRE-EDGE
           END-IF

      *>   --- Handle weapon switch requests ---
      *>   Key 1 = fist (WS-KEY(9)), Key 2 = pistol (WS-KEY(10))
           IF WS-KEY(9) NOT = 0
               AND WS-WP-STATE = 0
               AND WS-WP-CURRENT NOT = 1
               MOVE 1 TO WS-WP-CURRENT
      *>       Load pistol idle sprite
               MOVE "PISGA0  " TO WS-WP-LUMP-NAME
               PERFORM LOAD-WEAPON-FRAME
           END-IF
           IF WS-KEY(10) NOT = 0
               AND WS-WP-STATE = 0
               AND WS-WP-CURRENT NOT = 2
               MOVE 2 TO WS-WP-CURRENT
      *>       Load shotgun idle sprite
               MOVE "SHTGA0  " TO WS-WP-LUMP-NAME
               PERFORM LOAD-WEAPON-FRAME
           END-IF

      *>   --- Weapon state machine ---
           EVALUATE WS-WP-STATE

      *>       --- READY: check for fire input ---
               WHEN 0
                   IF WS-WP-FIRE-EDGE = 1
                       AND WS-AMMO > 0
      *>               Begin firing
                       MOVE 1 TO WS-WP-STATE
                       MOVE 1 TO WS-WP-ANIM-FRAME
                       MOVE WS-WP-FIRE-DURATION
                           TO WS-WP-FIRE-TIMER
      *>               Consume ammo
                       SUBTRACT 1 FROM WS-AMMO
      *>               Load fire frame B
                       IF WS-WP-CURRENT = 1
                           MOVE "PISGB0  "
                               TO WS-WP-LUMP-NAME
                       ELSE
                           MOVE "SHTGB0  "
                               TO WS-WP-LUMP-NAME
                       END-IF
                       PERFORM LOAD-WEAPON-FRAME
      *>               Do hitscan damage
                       PERFORM FIRE-HITSCAN
                   END-IF

      *>       --- FIRING: animate, then go to cooldown ---
               WHEN 1
                   IF WS-WP-FIRE-TIMER > 0
                       SUBTRACT 1 FROM WS-WP-FIRE-TIMER
      *>               Progress through fire animation frames
      *>               8 total frames: B for 2, C for 2,
      *>                               D for 2, E for 2
                       EVALUATE TRUE
                           WHEN WS-WP-FIRE-TIMER > 5
                               IF WS-WP-ANIM-FRAME NOT = 1
                                   MOVE 1 TO WS-WP-ANIM-FRAME
                                   IF WS-WP-CURRENT = 1
                                       MOVE "PISGB0  "
                                         TO WS-WP-LUMP-NAME
                                   ELSE
                                       MOVE "SHTGB0  "
                                         TO WS-WP-LUMP-NAME
                                   END-IF
                                   PERFORM LOAD-WEAPON-FRAME
                               END-IF
                           WHEN WS-WP-FIRE-TIMER > 3
                               IF WS-WP-ANIM-FRAME NOT = 2
                                   MOVE 2 TO WS-WP-ANIM-FRAME
                                   IF WS-WP-CURRENT = 1
                                       MOVE "PISGC0  "
                                         TO WS-WP-LUMP-NAME
                                   ELSE
                                       MOVE "SHTGC0  "
                                         TO WS-WP-LUMP-NAME
                                   END-IF
                                   PERFORM LOAD-WEAPON-FRAME
                               END-IF
                           WHEN WS-WP-FIRE-TIMER > 1
                               IF WS-WP-ANIM-FRAME NOT = 3
                                   MOVE 3 TO WS-WP-ANIM-FRAME
                                   IF WS-WP-CURRENT = 1
                                       MOVE "PISGD0  "
                                         TO WS-WP-LUMP-NAME
                                   ELSE
                                       MOVE "SHTGD0  "
                                         TO WS-WP-LUMP-NAME
                                   END-IF
                                   PERFORM LOAD-WEAPON-FRAME
                               END-IF
                           WHEN OTHER
                               IF WS-WP-ANIM-FRAME NOT = 4
                                   MOVE 4 TO WS-WP-ANIM-FRAME
                                   IF WS-WP-CURRENT = 1
                                       MOVE "PISGE0  "
                                         TO WS-WP-LUMP-NAME
                                   ELSE
                                       MOVE "SHTGE0  "
                                         TO WS-WP-LUMP-NAME
                                   END-IF
                                   PERFORM LOAD-WEAPON-FRAME
                               END-IF
                       END-EVALUATE
                   ELSE
      *>               Fire animation done, enter cooldown
                       MOVE 2 TO WS-WP-STATE
                       MOVE WS-WP-COOLDOWN-DURATION
                           TO WS-WP-COOLDOWN-TIMER
      *>               Load idle frame
                       MOVE 0 TO WS-WP-ANIM-FRAME
                       IF WS-WP-CURRENT = 1
                           MOVE "PISGA0  "
                               TO WS-WP-LUMP-NAME
                       ELSE
                           MOVE "SHTGA0  "
                               TO WS-WP-LUMP-NAME
                       END-IF
                       PERFORM LOAD-WEAPON-FRAME
                   END-IF

      *>       --- COOLDOWN: wait, then return to ready ---
               WHEN 2
                   IF WS-WP-COOLDOWN-TIMER > 0
                       SUBTRACT 1 FROM WS-WP-COOLDOWN-TIMER
                   ELSE
                       MOVE 0 TO WS-WP-STATE
                   END-IF

           END-EVALUATE
           .

      *> ============================================================
      *> FIRE-HITSCAN: Check if any enemy sprite is hit.
      *> Pistol: single ray at screen center (column 160).
      *> Shotgun: 7 pellets spread across ~5 degrees (~30px).
      *> Each ray checks all alive enemies by view-space
      *> projection; hits must be closer than the wall depth
      *> at center column and within ~10 pixels of aim point.
      *> ============================================================
       FIRE-HITSCAN.
      *>   Compute view vectors from player angle
           COMPUTE WS-WP-TRIG-IDX =
               FUNCTION MOD(WS-PA * 10 + 36000, 3600) + 1
           IF WS-WP-TRIG-IDX < 1
               MOVE 1 TO WS-WP-TRIG-IDX
           END-IF
           IF WS-WP-TRIG-IDX > 3600
               MOVE 3600 TO WS-WP-TRIG-IDX
           END-IF
           MOVE WS-COS-VAL(WS-WP-TRIG-IDX)
               TO WS-WP-VIEW-DX
           MOVE WS-SIN-VAL(WS-WP-TRIG-IDX)
               TO WS-WP-VIEW-DY
      *>   Strafe vector (perpendicular right)
           COMPUTE WS-WP-STRAFE-DX =
               0 - WS-WP-VIEW-DY
           MOVE WS-WP-VIEW-DX TO WS-WP-STRAFE-DY

           IF WS-WP-CURRENT = 2
      *>       Shotgun: 7 pellets spread -15 to +15 pixels
               PERFORM VARYING WS-WP-SG-PELLET
                   FROM 1 BY 1
                   UNTIL WS-WP-SG-PELLET
                       > WS-WP-SG-PELLET-MAX
      *>           Compute pixel offset: -15 to +15
      *>           pellet 1=-15, 2=-10, 3=-5, 4=0,
      *>           5=+5, 6=+10, 7=+15
                   COMPUTE WS-WP-SG-OFFSET =
                       (WS-WP-SG-PELLET - 4) * 5
                   PERFORM HITSCAN-ONE-RAY
               END-PERFORM
           ELSE
      *>       Pistol: single center ray
               MOVE 0 TO WS-WP-SG-OFFSET
               PERFORM HITSCAN-ONE-RAY
           END-IF
           .

      *> ============================================================
      *> HITSCAN-ONE-RAY: Test one hitscan ray against all alive
      *> enemy sprites. WS-WP-SG-OFFSET = pixel offset from
      *> screen center (0 for pistol, -15..+15 for shotgun).
      *> Finds closest hit enemy, applies damage.
      *> ============================================================
       HITSCAN-ONE-RAY.
           MOVE 0 TO WS-WP-HS-HIT
           MOVE 9999 TO WS-WP-HS-BEST-DIST
           MOVE 0 TO WS-WP-HS-BEST-IDX

           PERFORM VARYING WS-WP-HS-I FROM 1 BY 1
               UNTIL WS-WP-HS-I > WS-SP-COUNT

      *>       Skip dead or dying sprites
               IF WS-SP-STATE(WS-WP-HS-I) = 0

      *>           Compute vector from player to enemy
                   COMPUTE WS-WP-HS-DX =
                       WS-SP-WORLD-X(WS-WP-HS-I) - WS-PX
                   COMPUTE WS-WP-HS-DY =
                       WS-SP-WORLD-Y(WS-WP-HS-I) - WS-PY

      *>           Project into view space
                   COMPUTE WS-WP-HS-DEPTH =
                       WS-WP-HS-DX * WS-WP-VIEW-DX
                       + WS-WP-HS-DY * WS-WP-VIEW-DY
                   COMPUTE WS-WP-HS-LATERAL =
                       WS-WP-HS-DX * WS-WP-STRAFE-DX
                       + WS-WP-HS-DY * WS-WP-STRAFE-DY

      *>           Must be in front of player
                   IF WS-WP-HS-DEPTH > 0.2

      *>               Compute screen-X of enemy center
                       COMPUTE WS-WP-HS-SCREEN-X =
                           160 + WS-WP-HS-LATERAL
                           * 160 / WS-WP-HS-DEPTH

      *>               Check if within ~10 pixels of aim
      *>               (aim = 160 + offset)
                       COMPUTE WS-WP-HS-SCREEN-X =
                           WS-WP-HS-SCREEN-X
                           - 160 - WS-WP-SG-OFFSET
      *>               Take absolute value
                       IF WS-WP-HS-SCREEN-X < 0
                           COMPUTE WS-WP-HS-SCREEN-X =
                               0 - WS-WP-HS-SCREEN-X
                       END-IF

      *>               Within 10 pixel tolerance?
                       IF WS-WP-HS-SCREEN-X <= 10

      *>                   Check depth vs wall at center
      *>                   column (160)
                           IF WS-WP-HS-DEPTH <
                               WS-DEPTH-VAL(160)

      *>                       This enemy is hittable; is
      *>                       it the closest?
                               IF WS-WP-HS-DEPTH <
                                   WS-WP-HS-BEST-DIST
                                   MOVE WS-WP-HS-DEPTH
                                     TO WS-WP-HS-BEST-DIST
                                   MOVE WS-WP-HS-I
                                     TO WS-WP-HS-BEST-IDX
                                   MOVE 1
                                     TO WS-WP-HS-HIT
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

      *>   If we hit something, apply damage
           IF WS-WP-HS-HIT = 1
               AND WS-WP-HS-BEST-IDX >= 1
               AND WS-WP-HS-BEST-IDX <= 50

      *>       Random damage 7-15 for pistol pellet
      *>       Shotgun pellets also do 7-15 each (x7 total)
               COMPUTE WS-WP-HS-DMG =
                   7 + FUNCTION INTEGER-PART(
                       FUNCTION RANDOM * 9)
               IF WS-WP-HS-DMG < 7
                   MOVE 7 TO WS-WP-HS-DMG
               END-IF
               IF WS-WP-HS-DMG > 15
                   MOVE 15 TO WS-WP-HS-DMG
               END-IF

      *>       Apply damage via enemy system interface
               MOVE WS-WP-HS-BEST-IDX TO WS-EN-DMG-IDX
               MOVE WS-WP-HS-DMG TO WS-EN-DMG-AMT
               PERFORM EN-APPLY-DAMAGE
           END-IF
           .

      *> ============================================================
      *> RENDER-WEAPON: Draw the weapon sprite at bottom-center
      *> of the screen. Called after all 3D rendering.
      *> Uses column-post format from WS-WP-PATCH-BUF.
      *> Full brightness (colormap table 1, no distance fade).
      *> ============================================================
       RENDER-WEAPON.
      *>   Skip if no valid patch loaded
           IF WS-WP-PATCH-W < 1 OR WS-WP-PATCH-H < 1
               EXIT PARAGRAPH
           END-IF

      *>   Compute draw position: centered horizontally,
      *>   bottom of screen (above HUD at row 167).
      *>   Use patch left/top offsets for proper positioning.
           COMPUTE WS-WP-DRAW-W = WS-WP-PATCH-W
           COMPUTE WS-WP-DRAW-H = WS-WP-PATCH-H

      *>   Center horizontally using left-offset
           COMPUTE WS-WP-DRAW-LEFT =
               160 - WS-WP-PATCH-LEFT
           COMPUTE WS-WP-DRAW-RIGHT =
               WS-WP-DRAW-LEFT + WS-WP-DRAW-W - 1

      *>   Position vertically using top-offset
      *>   Doom weapon sprites are positioned relative to
      *>   a 320x200 virtual screen with origin at top-left
           COMPUTE WS-WP-DRAW-TOP =
               200 - WS-WP-PATCH-TOP
           COMPUTE WS-WP-DRAW-BOT =
               WS-WP-DRAW-TOP + WS-WP-DRAW-H - 1

      *>   Draw each column of the weapon patch
           PERFORM VARYING WS-WP-COL
               FROM WS-WP-DRAW-LEFT BY 1
               UNTIL WS-WP-COL > WS-WP-DRAW-RIGHT

      *>       Only draw columns that are on screen
               IF WS-WP-COL >= 1 AND WS-WP-COL <= 320
                   PERFORM RENDER-WEAPON-COLUMN
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> RENDER-WEAPON-COLUMN: Draw one column of the weapon
      *> sprite from WS-WP-PATCH-BUF. Same column-post format
      *> as world sprites. Uses colormap table 1 (full bright).
      *> ============================================================
       RENDER-WEAPON-COLUMN.
      *>   Map screen column to texture column
           COMPUTE WS-WP-TEX-COL =
               WS-WP-COL - WS-WP-DRAW-LEFT
      *>   Clamp texture column
           IF WS-WP-TEX-COL < 0
               MOVE 0 TO WS-WP-TEX-COL
           END-IF
           IF WS-WP-TEX-COL >= WS-WP-PATCH-W
               COMPUTE WS-WP-TEX-COL =
                   WS-WP-PATCH-W - 1
           END-IF

      *>   Get column data offset from column offset table
      *>   Column offsets at byte 9 (after 8-byte header),
      *>   each a 4-byte int32
           COMPUTE WS-WP-COL-OFF =
               WS-WP-TEX-COL * 4 + 9
           IF WS-WP-COL-OFF < 9
               MOVE 9 TO WS-WP-COL-OFF
           END-IF
           IF WS-WP-COL-OFF > 16380
               EXIT PARAGRAPH
           END-IF
           MOVE WS-WP-PATCH-BUF(WS-WP-COL-OFF:4)
               TO WS-BIN-BUF4
           MOVE WS-BIN-INT32 TO WS-WP-POST-POS
      *>   Convert to 1-based
           ADD 1 TO WS-WP-POST-POS
      *>   Sanity check
           IF WS-WP-POST-POS < 1
               OR WS-WP-POST-POS > 16380
               EXIT PARAGRAPH
           END-IF

      *>   Parse column posts until topdelta = 0xFF
           MOVE 0 TO WS-WP-POST-EXIT
           PERFORM UNTIL WS-WP-POST-EXIT = 1
               IF WS-WP-POST-POS > 16384
                   MOVE 1 TO WS-WP-POST-EXIT
               ELSE
      *>           Read topdelta byte
                   MOVE WS-WP-PATCH-BUF(
                       WS-WP-POST-POS:1)
                       TO WS-BIN-BUF1
                   MOVE WS-BIN-BYTE TO WS-WP-POST-TOP
                   IF WS-WP-POST-TOP = 255
                       MOVE 1 TO WS-WP-POST-EXIT
                   ELSE
                       ADD 1 TO WS-WP-POST-POS
      *>               Read length byte
                       IF WS-WP-POST-POS > 16384
                           MOVE 1 TO WS-WP-POST-EXIT
                       ELSE
                         MOVE WS-WP-PATCH-BUF(
                             WS-WP-POST-POS:1)
                             TO WS-BIN-BUF1
                         MOVE WS-BIN-BYTE
                             TO WS-WP-POST-LEN
                         ADD 1 TO WS-WP-POST-POS
      *>                 Skip pre-padding byte
                         ADD 1 TO WS-WP-POST-POS
      *>                 Draw each pixel in this post
                         PERFORM VARYING WS-WP-PIX-ROW
                             FROM 0 BY 1
                             UNTIL WS-WP-PIX-ROW
                                 >= WS-WP-POST-LEN
      *>                     Map texture row to screen row
                             COMPUTE WS-WP-SCREEN-ROW =
                                 WS-WP-DRAW-TOP
                                 + WS-WP-POST-TOP
                                 + WS-WP-PIX-ROW
      *>                     Only draw if on screen and
      *>                     above HUD (row <= 167)
                             IF WS-WP-SCREEN-ROW >= 1
                                 AND WS-WP-SCREEN-ROW
                                     <= 167
      *>                         Read palette index
                                 IF WS-WP-POST-POS >= 1
                                     AND WS-WP-POST-POS
                                         <= 16384
                                   MOVE
                                     WS-WP-PATCH-BUF(
                                     WS-WP-POST-POS:1)
                                     TO WS-BIN-BUF1
                                   MOVE WS-BIN-BYTE
                                     TO WS-WP-PAL-IDX
      *>                             Skip transparent
                                   IF WS-WP-PAL-IDX
                                       NOT = 255
      *>                               1-based palette
                                     ADD 1 TO
                                       WS-WP-PAL-IDX
                                     IF WS-WP-PAL-IDX
                                         > 256
                                       MOVE 1 TO
                                         WS-WP-PAL-IDX
                                     END-IF
      *>                               Apply colormap
      *>                               table 1 = full
      *>                               brightness
                                     MOVE
                                       WS-CMAP-ENTRY(
                                       1
                                       WS-WP-PAL-IDX)
                                       TO WS-BIN-BUF1
                                     MOVE WS-BIN-BYTE
                                       TO WS-WP-LIT-IDX
                                     ADD 1 TO
                                       WS-WP-LIT-IDX
                                     IF WS-WP-LIT-IDX
                                         > 256
                                       MOVE 1 TO
                                         WS-WP-LIT-IDX
                                     END-IF
      *>                               Write RGBA to FB
                                     COMPUTE
                                       WS-WP-FB-IDX =
                                       (WS-WP-SCREEN-ROW
                                       - 1) * 1280
                                       + (WS-WP-COL
                                       - 1) * 4 + 1
                                     IF WS-WP-FB-IDX
                                         >= 1
                                         AND
                                         WS-WP-FB-IDX
                                         <= 255997
                                       MOVE
                                         WS-PAL-R(
                                         WS-WP-LIT-IDX)
                                         TO
                                         WS-FB-BYTE(
                                         WS-WP-FB-IDX)
                                       ADD 1 TO
                                         WS-WP-FB-IDX
                                       MOVE
                                         WS-PAL-G(
                                         WS-WP-LIT-IDX)
                                         TO
                                         WS-FB-BYTE(
                                         WS-WP-FB-IDX)
                                       ADD 1 TO
                                         WS-WP-FB-IDX
                                       MOVE
                                         WS-PAL-B(
                                         WS-WP-LIT-IDX)
                                         TO
                                         WS-FB-BYTE(
                                         WS-WP-FB-IDX)
                                       ADD 1 TO
                                         WS-WP-FB-IDX
                                       MOVE X"FF"
                                         TO
                                         WS-FB-BYTE(
                                         WS-WP-FB-IDX)
                                     END-IF
                                   END-IF
                                 END-IF
                             END-IF
                             ADD 1 TO WS-WP-POST-POS
                         END-PERFORM
      *>                 Skip post-padding byte
                         ADD 1 TO WS-WP-POST-POS
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> RENDER-CROSSHAIR: Draw a small 3x3 cross in bright green
      *> at screen center (column 160, row 100). Writes 5 pixels
      *> directly to the framebuffer: center + 4 arms.
      *> Green = R:0 G:255 B:0 A:255
      *> ============================================================
       RENDER-CROSSHAIR.
      *>   Center pixel (160, 100)
           COMPUTE WS-WP-CROSS-FB =
               (99 * 320 + 159) * 4 + 1
           IF WS-WP-CROSS-FB >= 1
               AND WS-WP-CROSS-FB <= 255997
               MOVE X"00" TO WS-FB-BYTE(WS-WP-CROSS-FB)
               ADD 1 TO WS-WP-CROSS-FB
               MOVE X"FF" TO WS-FB-BYTE(WS-WP-CROSS-FB)
               ADD 1 TO WS-WP-CROSS-FB
               MOVE X"00" TO WS-FB-BYTE(WS-WP-CROSS-FB)
               ADD 1 TO WS-WP-CROSS-FB
               MOVE X"FF" TO WS-FB-BYTE(WS-WP-CROSS-FB)
           END-IF

      *>   Top pixel (160, 99)
           COMPUTE WS-WP-CROSS-FB =
               (98 * 320 + 159) * 4 + 1
           IF WS-WP-CROSS-FB >= 1
               AND WS-WP-CROSS-FB <= 255997
               MOVE X"00" TO WS-FB-BYTE(WS-WP-CROSS-FB)
               ADD 1 TO WS-WP-CROSS-FB
               MOVE X"FF" TO WS-FB-BYTE(WS-WP-CROSS-FB)
               ADD 1 TO WS-WP-CROSS-FB
               MOVE X"00" TO WS-FB-BYTE(WS-WP-CROSS-FB)
               ADD 1 TO WS-WP-CROSS-FB
               MOVE X"FF" TO WS-FB-BYTE(WS-WP-CROSS-FB)
           END-IF

      *>   Bottom pixel (160, 101)
           COMPUTE WS-WP-CROSS-FB =
               (100 * 320 + 159) * 4 + 1
           IF WS-WP-CROSS-FB >= 1
               AND WS-WP-CROSS-FB <= 255997
               MOVE X"00" TO WS-FB-BYTE(WS-WP-CROSS-FB)
               ADD 1 TO WS-WP-CROSS-FB
               MOVE X"FF" TO WS-FB-BYTE(WS-WP-CROSS-FB)
               ADD 1 TO WS-WP-CROSS-FB
               MOVE X"00" TO WS-FB-BYTE(WS-WP-CROSS-FB)
               ADD 1 TO WS-WP-CROSS-FB
               MOVE X"FF" TO WS-FB-BYTE(WS-WP-CROSS-FB)
           END-IF

      *>   Left pixel (159, 100)
           COMPUTE WS-WP-CROSS-FB =
               (99 * 320 + 158) * 4 + 1
           IF WS-WP-CROSS-FB >= 1
               AND WS-WP-CROSS-FB <= 255997
               MOVE X"00" TO WS-FB-BYTE(WS-WP-CROSS-FB)
               ADD 1 TO WS-WP-CROSS-FB
               MOVE X"FF" TO WS-FB-BYTE(WS-WP-CROSS-FB)
               ADD 1 TO WS-WP-CROSS-FB
               MOVE X"00" TO WS-FB-BYTE(WS-WP-CROSS-FB)
               ADD 1 TO WS-WP-CROSS-FB
               MOVE X"FF" TO WS-FB-BYTE(WS-WP-CROSS-FB)
           END-IF

      *>   Right pixel (161, 100)
           COMPUTE WS-WP-CROSS-FB =
               (99 * 320 + 160) * 4 + 1
           IF WS-WP-CROSS-FB >= 1
               AND WS-WP-CROSS-FB <= 255997
               MOVE X"00" TO WS-FB-BYTE(WS-WP-CROSS-FB)
               ADD 1 TO WS-WP-CROSS-FB
               MOVE X"FF" TO WS-FB-BYTE(WS-WP-CROSS-FB)
               ADD 1 TO WS-WP-CROSS-FB
               MOVE X"00" TO WS-FB-BYTE(WS-WP-CROSS-FB)
               ADD 1 TO WS-WP-CROSS-FB
               MOVE X"FF" TO WS-FB-BYTE(WS-WP-CROSS-FB)
           END-IF
           .
