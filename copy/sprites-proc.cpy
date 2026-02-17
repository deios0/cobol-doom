      *> ============================================================
      *> sprites-proc.cpy — Sprite rendering procedures
      *> INIT-SPRITES: populate active sprite table from THINGS
      *> RENDER-ALL-SPRITES: project, sort, draw all active sprites
      *> Uses view-space projection (no atan2 needed).
      *> ============================================================

      *> ============================================================
      *> INIT-SPRITES: Scan THINGS for enemies, build sprite table
      *> Enemy types: 3004=zombieman, 3001=imp, 3002=demon,
      *>              9=shotgun guy, 65=chaingunner
      *> ============================================================
       INIT-SPRITES.
           MOVE 0 TO WS-SP-COUNT
           PERFORM VARYING WS-SP-I FROM 1 BY 1
               UNTIL WS-SP-I > WS-MAP-THING-COUNT
               OR WS-SP-COUNT >= 50
      *>       Check if this thing type is an enemy
               PERFORM VARYING WS-SP-J FROM 1 BY 1
                   UNTIL WS-SP-J > WS-SP-MAP-COUNT
                   IF MT-TYPE(WS-SP-I) =
                       WS-SP-TM-TYPE(WS-SP-J)
      *>               Found enemy — add to sprite table
                       ADD 1 TO WS-SP-COUNT
      *>               Convert map coords to grid coords
      *>               grid = (map - min) / scale + 2
                       COMPUTE WS-SP-GRID-X =
                           (MT-X(WS-SP-I)
                           - WS-MAP-MIN-X)
                           / WS-MAP-SCALE + 2
                       COMPUTE WS-SP-GRID-Y =
                           (MT-Y(WS-SP-I)
                           - WS-MAP-MIN-Y)
                           / WS-MAP-SCALE + 2
      *>               Offset to center of cell
                       COMPUTE WS-SP-WORLD-X(WS-SP-COUNT)
                           = WS-SP-GRID-X - 0.5
                       COMPUTE WS-SP-WORLD-Y(WS-SP-COUNT)
                           = WS-SP-GRID-Y - 0.5
                       MOVE MT-TYPE(WS-SP-I)
                           TO WS-SP-THING-TYPE(WS-SP-COUNT)
                       MOVE 0
                           TO WS-SP-STATE(WS-SP-COUNT)
                       MOVE WS-SP-TM-HEALTH(WS-SP-J)
                           TO WS-SP-HEALTH(WS-SP-COUNT)
                       MOVE 0
                           TO WS-SP-FRAME-TMR(WS-SP-COUNT)
      *>               Build lump name: BASE + "A0  "
                       MOVE WS-SP-TM-BASE(WS-SP-J)
                           TO WS-SP-BASE-NAME
                       STRING WS-SP-BASE-NAME
                              "A0  "
                              DELIMITED BY SIZE
                              INTO WS-SP-LUMP-NAME(
                                  WS-SP-COUNT)
                   END-IF
               END-PERFORM
           END-PERFORM
           DISPLAY "Sprites: " WS-SP-COUNT " enemies"
           .

      *> ============================================================
      *> RENDER-ALL-SPRITES: Project, sort, and draw all sprites
      *> Called after wall raycasting so depth buffer is filled.
      *> ============================================================
       RENDER-ALL-SPRITES.
           IF WS-SP-COUNT < 1
               EXIT PARAGRAPH
           END-IF

      *>   Compute player view vectors from angle
           COMPUTE WS-SP-TRIG-IDX =
               FUNCTION MOD(WS-PA * 10 + 36000, 3600) + 1
           MOVE WS-COS-VAL(WS-SP-TRIG-IDX)
               TO WS-SP-VIEW-DX
           MOVE WS-SIN-VAL(WS-SP-TRIG-IDX)
               TO WS-SP-VIEW-DY
      *>   Strafe = perpendicular to view (right vector)
      *>   strafe_dx = -sin(angle), strafe_dy = cos(angle)
           COMPUTE WS-SP-STRAFE-DX =
               0 - WS-SP-VIEW-DY
           MOVE WS-SP-VIEW-DX TO WS-SP-STRAFE-DY

      *>   Calculate distance for each sprite and init order
           PERFORM VARYING WS-SP-I FROM 1 BY 1
               UNTIL WS-SP-I > WS-SP-COUNT
               MOVE WS-SP-I TO WS-SP-ORD-IDX(WS-SP-I)
      *>       Skip dead sprites
               IF WS-SP-STATE(WS-SP-I) = 2
                   MOVE 0 TO WS-SP-DIST-VAL(WS-SP-I)
               ELSE
                   COMPUTE WS-SP-DX =
                       WS-SP-WORLD-X(WS-SP-I) - WS-PX
                   COMPUTE WS-SP-DY =
                       WS-SP-WORLD-Y(WS-SP-I) - WS-PY
                   COMPUTE WS-SP-DIST-VAL(WS-SP-I) =
                       WS-SP-DX * WS-SP-DX
                       + WS-SP-DY * WS-SP-DY
               END-IF
           END-PERFORM

      *>   Sort sprites by distance (furthest first)
           PERFORM SORT-SPRITES

      *>   Render each sprite in sorted order
           PERFORM VARYING WS-SP-I FROM 1 BY 1
               UNTIL WS-SP-I > WS-SP-COUNT
               MOVE WS-SP-ORD-IDX(WS-SP-I) TO WS-SP-CUR
      *>       Skip dead sprites
               IF WS-SP-STATE(WS-SP-CUR) NOT = 2
                   PERFORM RENDER-ONE-SPRITE
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> SORT-SPRITES: Insertion sort by distance, furthest first
      *> ============================================================
       SORT-SPRITES.
           PERFORM VARYING WS-SP-I FROM 2 BY 1
               UNTIL WS-SP-I > WS-SP-COUNT
               MOVE WS-SP-DIST-VAL(
                   WS-SP-ORD-IDX(WS-SP-I))
                   TO WS-SP-SORT-KEY
               MOVE WS-SP-ORD-IDX(WS-SP-I)
                   TO WS-SP-SORT-IDX
               MOVE WS-SP-I TO WS-SP-SORT-J
               PERFORM UNTIL WS-SP-SORT-J < 2
                   OR WS-SP-DIST-VAL(
                       WS-SP-ORD-IDX(WS-SP-SORT-J - 1))
                       >= WS-SP-SORT-KEY
                   MOVE WS-SP-ORD-IDX(
                       WS-SP-SORT-J - 1)
                       TO WS-SP-ORD-IDX(WS-SP-SORT-J)
                   SUBTRACT 1 FROM WS-SP-SORT-J
               END-PERFORM
               MOVE WS-SP-SORT-IDX
                   TO WS-SP-ORD-IDX(WS-SP-SORT-J)
           END-PERFORM
           .

      *> ============================================================
      *> RENDER-ONE-SPRITE: Project and draw one sprite
      *> WS-SP-CUR = index into sprite table
      *> ============================================================
       RENDER-ONE-SPRITE.
      *>   Compute vector from player to sprite
           COMPUTE WS-SP-DX =
               WS-SP-WORLD-X(WS-SP-CUR) - WS-PX
           COMPUTE WS-SP-DY =
               WS-SP-WORLD-Y(WS-SP-CUR) - WS-PY

      *>   Project into view space
      *>   depth  = dx * view_dx + dy * view_dy  (forward)
      *>   lateral = dx * strafe_dx + dy * strafe_dy  (side)
           COMPUTE WS-SP-DEPTH =
               WS-SP-DX * WS-SP-VIEW-DX
               + WS-SP-DY * WS-SP-VIEW-DY
           COMPUTE WS-SP-LATERAL =
               WS-SP-DX * WS-SP-STRAFE-DX
               + WS-SP-DY * WS-SP-STRAFE-DY

      *>   Skip if behind player
           IF WS-SP-DEPTH < 0.2
               EXIT PARAGRAPH
           END-IF

      *>   Perspective projection
      *>   screen_x = 160 + lateral * 160 / depth
      *>   sprite_h = 200 / depth (scale factor)
           COMPUTE WS-SP-INV-DEPTH = 1.0 / WS-SP-DEPTH
           COMPUTE WS-SP-SCREEN-X =
               160 + WS-SP-LATERAL * 160
               * WS-SP-INV-DEPTH

      *>   Load sprite patch data
           PERFORM LOAD-SPRITE-FRAME
      *>   If patch load failed (width=0), skip
           IF WS-SP-PATCH-W < 1
               EXIT PARAGRAPH
           END-IF

      *>   Compute screen dimensions of sprite
      *>   Scale = screen_h / depth  (how many screen pixels
      *>   per world unit at this distance)
           COMPUTE WS-SP-SCALE =
               200.0 * WS-SP-INV-DEPTH
      *>   Screen pixel dimensions
           COMPUTE WS-SP-SCREEN-H =
               WS-SP-PATCH-H * WS-SP-SCALE
               / WS-SP-PATCH-H
      *>   Simplify: screen_h = scale
           MOVE WS-SP-SCALE TO WS-SP-SCREEN-H
           COMPUTE WS-SP-SCREEN-W =
               WS-SP-PATCH-W * WS-SP-SCREEN-H
               / WS-SP-PATCH-H
      *>   Clamp screen dimensions
           IF WS-SP-SCREEN-H < 1
               MOVE 1 TO WS-SP-SCREEN-H
           END-IF
           IF WS-SP-SCREEN-H > 400
               MOVE 400 TO WS-SP-SCREEN-H
           END-IF
           IF WS-SP-SCREEN-W < 1
               MOVE 1 TO WS-SP-SCREEN-W
           END-IF
           IF WS-SP-SCREEN-W > 640
               MOVE 640 TO WS-SP-SCREEN-W
           END-IF

      *>   Compute draw bounds using left/top offset
      *>   The patch header offsets say where the "origin"
      *>   of the sprite is (usually bottom-center)
           COMPUTE WS-SP-DRAW-LEFT =
               WS-SP-SCREEN-X
               - (WS-SP-PATCH-LEFT * WS-SP-SCREEN-W
                  / WS-SP-PATCH-W)
           COMPUTE WS-SP-DRAW-RIGHT =
               WS-SP-DRAW-LEFT + WS-SP-SCREEN-W - 1
           COMPUTE WS-SP-DRAW-TOP =
               WS-SP-HALF-H
               - (WS-SP-PATCH-TOP * WS-SP-SCREEN-H
                  / WS-SP-PATCH-H)
           COMPUTE WS-SP-DRAW-BOT =
               WS-SP-DRAW-TOP + WS-SP-SCREEN-H - 1

      *>   Quick reject: entirely off screen
           IF WS-SP-DRAW-RIGHT < 1
               EXIT PARAGRAPH
           END-IF
           IF WS-SP-DRAW-LEFT > 320
               EXIT PARAGRAPH
           END-IF
           IF WS-SP-DRAW-BOT < 1
               EXIT PARAGRAPH
           END-IF
           IF WS-SP-DRAW-TOP > 200
               EXIT PARAGRAPH
           END-IF

      *>   Lighting based on distance
           COMPUTE WS-SP-LIGHT =
               WS-SP-DEPTH * 2
           IF WS-SP-LIGHT < 0
               MOVE 0 TO WS-SP-LIGHT
           END-IF
           IF WS-SP-LIGHT > 31
               MOVE 31 TO WS-SP-LIGHT
           END-IF
           COMPUTE WS-SP-CMAP-TBL = WS-SP-LIGHT + 1

      *>   Draw each column of the sprite
           PERFORM VARYING WS-SP-COL
               FROM WS-SP-DRAW-LEFT BY 1
               UNTIL WS-SP-COL > WS-SP-DRAW-RIGHT
      *>       Skip columns off screen
               IF WS-SP-COL >= 1 AND WS-SP-COL <= 320
      *>           Depth test: skip if wall is closer
                   IF WS-SP-DEPTH <
                       WS-DEPTH-VAL(WS-SP-COL)
                       PERFORM RENDER-SPRITE-COLUMN
                   END-IF
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> LOAD-SPRITE-FRAME: Find and read a sprite patch from WAD
      *> Input: WS-SP-LUMP-NAME(WS-SP-CUR)
      *> Output: WS-SP-PATCH-BUF, WS-SP-PATCH-W/H/LEFT/TOP
      *> ============================================================
       LOAD-SPRITE-FRAME.
           MOVE 0 TO WS-SP-PATCH-W
           MOVE 0 TO WS-SP-PATCH-H
           MOVE 0 TO WS-SP-PATCH-LEFT
           MOVE 0 TO WS-SP-PATCH-TOP

      *>   Look up the sprite lump in the WAD
           MOVE WS-SP-LUMP-NAME(WS-SP-CUR)
               TO WS-FIND-NAME
           PERFORM FIND-LUMP
           IF WS-FOUND NOT = 1
               EXIT PARAGRAPH
           END-IF

      *>   Read patch data into sprite buffer
           MOVE WS-FOUND-OFFSET TO WS-WAD-OFFSET
           MOVE WS-FOUND-SIZE TO WS-WAD-RD-SIZE
           IF WS-WAD-RD-SIZE > 65536
               MOVE 65536 TO WS-WAD-RD-SIZE
           END-IF
           CALL "wad_read" USING WS-WAD-OFFSET
                                 WS-WAD-RD-SIZE
                                 WS-SP-PATCH-BUF

      *>   Parse patch header (8 bytes):
      *>   width (int16), height (int16),
      *>   left_offset (int16), top_offset (int16)
           MOVE WS-SP-PATCH-BUF(1:2) TO WS-BIN-BUF2
           MOVE WS-BIN-INT16 TO WS-SP-PATCH-W
           MOVE WS-SP-PATCH-BUF(3:2) TO WS-BIN-BUF2
           MOVE WS-BIN-INT16 TO WS-SP-PATCH-H
           MOVE WS-SP-PATCH-BUF(5:2) TO WS-BIN-BUF2
           MOVE WS-BIN-INT16 TO WS-SP-PATCH-LEFT
           MOVE WS-SP-PATCH-BUF(7:2) TO WS-BIN-BUF2
           MOVE WS-BIN-INT16 TO WS-SP-PATCH-TOP

      *>   Sanity check dimensions
           IF WS-SP-PATCH-W < 1 OR WS-SP-PATCH-W > 320
               MOVE 0 TO WS-SP-PATCH-W
           END-IF
           IF WS-SP-PATCH-H < 1 OR WS-SP-PATCH-H > 200
               MOVE 0 TO WS-SP-PATCH-H
           END-IF
           .

      *> ============================================================
      *> RENDER-SPRITE-COLUMN: Draw one column of a sprite
      *> WS-SP-COL = screen column (1..320)
      *> Reads column post data from WS-SP-PATCH-BUF
      *> ============================================================
       RENDER-SPRITE-COLUMN.
      *>   Map screen column back to texture column
           IF WS-SP-SCREEN-W > 0
               COMPUTE WS-SP-TEX-COL =
                   (WS-SP-COL - WS-SP-DRAW-LEFT)
                   * WS-SP-PATCH-W
                   / WS-SP-SCREEN-W
           ELSE
               MOVE 0 TO WS-SP-TEX-COL
           END-IF
      *>   Clamp texture column
           IF WS-SP-TEX-COL < 0
               MOVE 0 TO WS-SP-TEX-COL
           END-IF
           IF WS-SP-TEX-COL >= WS-SP-PATCH-W
               COMPUTE WS-SP-TEX-COL =
                   WS-SP-PATCH-W - 1
           END-IF

      *>   Get column data offset from column offset table
      *>   Column offsets start at byte 9 (after 8-byte header)
      *>   Each is a 4-byte int32
           COMPUTE WS-SP-COL-OFF =
               WS-SP-TEX-COL * 4 + 9
           IF WS-SP-COL-OFF < 9
               MOVE 9 TO WS-SP-COL-OFF
           END-IF
           IF WS-SP-COL-OFF > 65532
               EXIT PARAGRAPH
           END-IF
           MOVE WS-SP-PATCH-BUF(WS-SP-COL-OFF:4)
               TO WS-BIN-BUF4
           MOVE WS-BIN-INT32 TO WS-SP-POST-POS
      *>   Convert to 1-based buffer position
           ADD 1 TO WS-SP-POST-POS
      *>   Sanity check
           IF WS-SP-POST-POS < 1
               OR WS-SP-POST-POS > 65530
               EXIT PARAGRAPH
           END-IF

      *>   Parse column posts until topdelta = 0xFF
           MOVE 0 TO WS-SP-POST-EXIT
           PERFORM UNTIL WS-SP-POST-EXIT = 1
      *>       Read topdelta byte
               IF WS-SP-POST-POS > 65535
                   MOVE 1 TO WS-SP-POST-EXIT
               ELSE
                   MOVE WS-SP-PATCH-BUF(
                       WS-SP-POST-POS:1)
                       TO WS-BIN-BUF1
                   MOVE WS-BIN-BYTE TO WS-SP-POST-TOP
                   IF WS-SP-POST-TOP = 255
                       MOVE 1 TO WS-SP-POST-EXIT
                   ELSE
                       ADD 1 TO WS-SP-POST-POS
      *>               Read length byte
                       MOVE WS-SP-PATCH-BUF(
                           WS-SP-POST-POS:1)
                           TO WS-BIN-BUF1
                       MOVE WS-BIN-BYTE
                           TO WS-SP-POST-LEN
                       ADD 1 TO WS-SP-POST-POS
      *>               Skip pre-padding byte
                       ADD 1 TO WS-SP-POST-POS
      *>               Draw each pixel in this post
                       PERFORM VARYING WS-SP-PIX-ROW
                           FROM 0 BY 1
                           UNTIL WS-SP-PIX-ROW
                               >= WS-SP-POST-LEN
      *>                   Map texture row to screen row
      *>                   tex_y = post_top + pix_row
      *>                   screen_y = draw_top +
      *>                     tex_y * screen_h / patch_h
                           COMPUTE WS-SP-TEX-ROW =
                               WS-SP-POST-TOP
                               + WS-SP-PIX-ROW
                           IF WS-SP-PATCH-H > 0
                               COMPUTE
                                   WS-SP-SCREEN-ROW =
                                   WS-SP-DRAW-TOP
                                   + WS-SP-TEX-ROW
                                   * WS-SP-SCREEN-H
                                   / WS-SP-PATCH-H
                           ELSE
                               COMPUTE
                                   WS-SP-SCREEN-ROW =
                                   WS-SP-DRAW-TOP
                                   + WS-SP-PIX-ROW
                           END-IF
      *>                   Only draw if on screen
                           IF WS-SP-SCREEN-ROW >= 1
                               AND WS-SP-SCREEN-ROW
                                   <= 200
      *>                       Read palette index
                               IF WS-SP-POST-POS >= 1
                                   AND WS-SP-POST-POS
                                       <= 65536
                                   MOVE
                                     WS-SP-PATCH-BUF(
                                     WS-SP-POST-POS:1)
                                     TO WS-BIN-BUF1
                                   MOVE WS-BIN-BYTE
                                     TO WS-SP-PAL-IDX
      *>                           Skip transparent px
                                   IF WS-SP-PAL-IDX
                                       NOT = 255
      *>                               1-based palette
                                       ADD 1 TO
                                         WS-SP-PAL-IDX
                                       IF WS-SP-PAL-IDX
                                           > 256
                                           MOVE 1 TO
                                           WS-SP-PAL-IDX
                                       END-IF
      *>                               Apply colormap
                                       MOVE
                                         WS-CMAP-ENTRY(
                                         WS-SP-CMAP-TBL
                                         WS-SP-PAL-IDX)
                                         TO WS-BIN-BUF1
                                       MOVE WS-BIN-BYTE
                                         TO
                                         WS-SP-LIT-IDX
                                       ADD 1 TO
                                         WS-SP-LIT-IDX
                                       IF WS-SP-LIT-IDX
                                           > 256
                                           MOVE 1 TO
                                           WS-SP-LIT-IDX
                                       END-IF
      *>                               Write RGBA to FB
                                       COMPUTE
                                         WS-SP-FB-IDX =
                                         (WS-SP-SCREEN-ROW
                                         - 1) * 1280
                                         + (WS-SP-COL
                                         - 1) * 4 + 1
                                       IF WS-SP-FB-IDX
                                           >= 1
                                           AND
                                           WS-SP-FB-IDX
                                           <= 255997
                                         MOVE
                                           WS-PAL-R(
                                           WS-SP-LIT-IDX)
                                           TO
                                           WS-FB-BYTE(
                                           WS-SP-FB-IDX)
                                         ADD 1 TO
                                           WS-SP-FB-IDX
                                         MOVE
                                           WS-PAL-G(
                                           WS-SP-LIT-IDX)
                                           TO
                                           WS-FB-BYTE(
                                           WS-SP-FB-IDX)
                                         ADD 1 TO
                                           WS-SP-FB-IDX
                                         MOVE
                                           WS-PAL-B(
                                           WS-SP-LIT-IDX)
                                           TO
                                           WS-FB-BYTE(
                                           WS-SP-FB-IDX)
                                         ADD 1 TO
                                           WS-SP-FB-IDX
                                         MOVE X"FF"
                                           TO
                                           WS-FB-BYTE(
                                           WS-SP-FB-IDX)
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                           ADD 1 TO WS-SP-POST-POS
                       END-PERFORM
      *>               Skip post-padding byte
                       ADD 1 TO WS-SP-POST-POS
                   END-IF
               END-IF
           END-PERFORM
           .
