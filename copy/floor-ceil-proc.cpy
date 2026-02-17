      *> ============================================================
      *> floor-ceil-proc.cpy — Textured floor/ceiling renderer
      *> Call RENDER-FLOOR-CEILING before wall raycaster so walls
      *> overdraw the floor/ceiling pixels.
      *> ============================================================

       RENDER-FLOOR-CEILING.
      *>   Validate flats loaded
           IF WS-FLAT-COUNT < 1
               EXIT PARAGRAPH
           END-IF
      *>   Clamp flat IDs
           IF WS-FC-FLOOR-FLAT < 1
               OR WS-FC-FLOOR-FLAT > WS-FLAT-COUNT
               MOVE 1 TO WS-FC-FLOOR-FLAT
           END-IF
           IF WS-FC-CEIL-FLAT < 1
               OR WS-FC-CEIL-FLAT > WS-FLAT-COUNT
               MOVE 1 TO WS-FC-CEIL-FLAT
           END-IF

      *>   Compute ray directions at screen edges
           PERFORM FC-COMPUTE-RAY-DIRS

      *>   Render floor rows (center+1 to bottom)
      *>   and ceiling rows (mirrored above center)
           PERFORM VARYING WS-FC-ROW
               FROM 101 BY 1
               UNTIL WS-FC-ROW > 200

      *>       Row distance = half_h / (row - half_h)
               COMPUTE WS-FC-DENOM =
                   WS-FC-ROW - WS-FC-HALF-H
               IF WS-FC-DENOM < 1
                   MOVE 1 TO WS-FC-DENOM
               END-IF
               COMPUTE WS-FC-ROW-DIST =
                   WS-FC-HALF-H / WS-FC-DENOM

      *>       Floor step per column
               COMPUTE WS-FC-STEP-X =
                   WS-FC-ROW-DIST
                   * (WS-FC-RDIR-RX - WS-FC-RDIR-LX)
                   / 320
               COMPUTE WS-FC-STEP-Y =
                   WS-FC-ROW-DIST
                   * (WS-FC-RDIR-RY - WS-FC-RDIR-LY)
                   / 320

      *>       Starting floor world position
               COMPUTE WS-FC-FLOOR-X =
                   WS-PX + WS-FC-ROW-DIST * WS-FC-RDIR-LX
               COMPUTE WS-FC-FLOOR-Y =
                   WS-PY + WS-FC-ROW-DIST * WS-FC-RDIR-LY

      *>       Draw floor scanline
               MOVE WS-FC-FLOOR-FLAT TO WS-FC-CUR-FLAT
               PERFORM FC-DRAW-FLOOR-SCANLINE

      *>       Draw mirrored ceiling scanline
               COMPUTE WS-FC-CEIL-ROW = 200 - WS-FC-ROW + 1

      *>       Recompute floor position for ceiling
               COMPUTE WS-FC-FLOOR-X =
                   WS-PX + WS-FC-ROW-DIST * WS-FC-RDIR-LX
               COMPUTE WS-FC-FLOOR-Y =
                   WS-PY + WS-FC-ROW-DIST * WS-FC-RDIR-LY

               MOVE WS-FC-CEIL-FLAT TO WS-FC-CUR-FLAT
               PERFORM FC-DRAW-CEIL-SCANLINE

           END-PERFORM
           .

      *> ============================================================
      *> FC-COMPUTE-RAY-DIRS: Left/right edge ray directions
      *> ============================================================
       FC-COMPUTE-RAY-DIRS.
      *>   Left ray angle = PA - half_fov
           COMPUTE WS-FC-ANG-L = WS-PA - WS-FC-HALF-FOV
           IF WS-FC-ANG-L < 0
               ADD 360 TO WS-FC-ANG-L
           END-IF
           IF WS-FC-ANG-L >= 360
               SUBTRACT 360 FROM WS-FC-ANG-L
           END-IF
           COMPUTE WS-FC-TRIG-IDX =
               WS-FC-ANG-L * 10 + 1
           IF WS-FC-TRIG-IDX < 1
               MOVE 1 TO WS-FC-TRIG-IDX
           END-IF
           IF WS-FC-TRIG-IDX > 3600
               MOVE 3600 TO WS-FC-TRIG-IDX
           END-IF
           MOVE WS-COS-VAL(WS-FC-TRIG-IDX)
               TO WS-FC-RDIR-LX
           MOVE WS-SIN-VAL(WS-FC-TRIG-IDX)
               TO WS-FC-RDIR-LY

      *>   Right ray angle = PA + half_fov
           COMPUTE WS-FC-ANG-R = WS-PA + WS-FC-HALF-FOV
           IF WS-FC-ANG-R < 0
               ADD 360 TO WS-FC-ANG-R
           END-IF
           IF WS-FC-ANG-R >= 360
               SUBTRACT 360 FROM WS-FC-ANG-R
           END-IF
           COMPUTE WS-FC-TRIG-IDX =
               WS-FC-ANG-R * 10 + 1
           IF WS-FC-TRIG-IDX < 1
               MOVE 1 TO WS-FC-TRIG-IDX
           END-IF
           IF WS-FC-TRIG-IDX > 3600
               MOVE 3600 TO WS-FC-TRIG-IDX
           END-IF
           MOVE WS-COS-VAL(WS-FC-TRIG-IDX)
               TO WS-FC-RDIR-RX
           MOVE WS-SIN-VAL(WS-FC-TRIG-IDX)
               TO WS-FC-RDIR-RY
           .

      *> ============================================================
      *> FC-DRAW-FLOOR-SCANLINE: One floor row (below center)
      *> ============================================================
       FC-DRAW-FLOOR-SCANLINE.
           COMPUTE WS-FC-FB-IDX =
               (WS-FC-ROW - 1) * 1280 + 1

           PERFORM VARYING WS-FC-COL FROM 1 BY 1
               UNTIL WS-FC-COL > 320

      *>       Texture coordinates
               COMPUTE WS-FC-SCALED-X =
                   WS-FC-FLOOR-X * 64
               COMPUTE WS-FC-SCALED-Y =
                   WS-FC-FLOOR-Y * 64

               COMPUTE WS-FC-TEX-X =
                   FUNCTION MOD(
                       FUNCTION INTEGER-PART(WS-FC-SCALED-X)
                       64)
               IF WS-FC-TEX-X < 0
                   ADD 64 TO WS-FC-TEX-X
               END-IF
               COMPUTE WS-FC-TEX-Y =
                   FUNCTION MOD(
                       FUNCTION INTEGER-PART(WS-FC-SCALED-Y)
                       64)
               IF WS-FC-TEX-Y < 0
                   ADD 64 TO WS-FC-TEX-Y
               END-IF

      *>       Flat pixel offset = tex_y * 64 + tex_x + 1
               COMPUTE WS-FC-FLAT-OFF =
                   WS-FC-TEX-Y * 64 + WS-FC-TEX-X + 1
               IF WS-FC-FLAT-OFF < 1
                   MOVE 1 TO WS-FC-FLAT-OFF
               END-IF
               IF WS-FC-FLAT-OFF > 4096
                   MOVE 4096 TO WS-FC-FLAT-OFF
               END-IF

      *>       Get palette index
               MOVE WF-PIX(WS-FC-CUR-FLAT
                   WS-FC-FLAT-OFF) TO WS-BIN-BUF1
               MOVE WS-BIN-BYTE TO WS-FC-PAL-IDX
               ADD 1 TO WS-FC-PAL-IDX
               IF WS-FC-PAL-IDX > 256
                   MOVE 1 TO WS-FC-PAL-IDX
               END-IF

      *>       Distance lighting via colormap
               COMPUTE WS-FC-LIGHT =
                   WS-FC-ROW-DIST * 2
               IF WS-FC-LIGHT > 31
                   MOVE 31 TO WS-FC-LIGHT
               END-IF
               IF WS-FC-LIGHT < 0
                   MOVE 0 TO WS-FC-LIGHT
               END-IF
               COMPUTE WS-FC-CMAP-TBL =
                   WS-FC-LIGHT + 1

               MOVE WS-CMAP-ENTRY(WS-FC-CMAP-TBL
                   WS-FC-PAL-IDX)
                   TO WS-BIN-BUF1
               MOVE WS-BIN-BYTE TO WS-FC-LIT-IDX
               ADD 1 TO WS-FC-LIT-IDX
               IF WS-FC-LIT-IDX > 256
                   MOVE 1 TO WS-FC-LIT-IDX
               END-IF

      *>       Write RGBA
               MOVE WS-PAL-R(WS-FC-LIT-IDX)
                   TO WS-FB-BYTE(WS-FC-FB-IDX)
               ADD 1 TO WS-FC-FB-IDX
               MOVE WS-PAL-G(WS-FC-LIT-IDX)
                   TO WS-FB-BYTE(WS-FC-FB-IDX)
               ADD 1 TO WS-FC-FB-IDX
               MOVE WS-PAL-B(WS-FC-LIT-IDX)
                   TO WS-FB-BYTE(WS-FC-FB-IDX)
               ADD 1 TO WS-FC-FB-IDX
               MOVE X"FF"
                   TO WS-FB-BYTE(WS-FC-FB-IDX)
               ADD 1 TO WS-FC-FB-IDX

      *>       Advance floor position
               ADD WS-FC-STEP-X TO WS-FC-FLOOR-X
               ADD WS-FC-STEP-Y TO WS-FC-FLOOR-Y

           END-PERFORM
           .

      *> ============================================================
      *> FC-DRAW-CEIL-SCANLINE: One ceiling row (above center)
      *> ============================================================
       FC-DRAW-CEIL-SCANLINE.
           IF WS-FC-CEIL-ROW < 1
               OR WS-FC-CEIL-ROW > WS-FC-HALF-H
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-FC-CEIL-FB-IDX =
               (WS-FC-CEIL-ROW - 1) * 1280 + 1

           PERFORM VARYING WS-FC-COL FROM 1 BY 1
               UNTIL WS-FC-COL > 320

               COMPUTE WS-FC-SCALED-X =
                   WS-FC-FLOOR-X * 64
               COMPUTE WS-FC-SCALED-Y =
                   WS-FC-FLOOR-Y * 64

               COMPUTE WS-FC-TEX-X =
                   FUNCTION MOD(
                       FUNCTION INTEGER-PART(WS-FC-SCALED-X)
                       64)
               IF WS-FC-TEX-X < 0
                   ADD 64 TO WS-FC-TEX-X
               END-IF
               COMPUTE WS-FC-TEX-Y =
                   FUNCTION MOD(
                       FUNCTION INTEGER-PART(WS-FC-SCALED-Y)
                       64)
               IF WS-FC-TEX-Y < 0
                   ADD 64 TO WS-FC-TEX-Y
               END-IF

               COMPUTE WS-FC-FLAT-OFF =
                   WS-FC-TEX-Y * 64 + WS-FC-TEX-X + 1
               IF WS-FC-FLAT-OFF < 1
                   MOVE 1 TO WS-FC-FLAT-OFF
               END-IF
               IF WS-FC-FLAT-OFF > 4096
                   MOVE 4096 TO WS-FC-FLAT-OFF
               END-IF

               MOVE WF-PIX(WS-FC-CUR-FLAT
                   WS-FC-FLAT-OFF) TO WS-BIN-BUF1
               MOVE WS-BIN-BYTE TO WS-FC-PAL-IDX
               ADD 1 TO WS-FC-PAL-IDX
               IF WS-FC-PAL-IDX > 256
                   MOVE 1 TO WS-FC-PAL-IDX
               END-IF

               COMPUTE WS-FC-LIGHT =
                   WS-FC-ROW-DIST * 2
               IF WS-FC-LIGHT > 31
                   MOVE 31 TO WS-FC-LIGHT
               END-IF
               IF WS-FC-LIGHT < 0
                   MOVE 0 TO WS-FC-LIGHT
               END-IF
               COMPUTE WS-FC-CMAP-TBL =
                   WS-FC-LIGHT + 1

               MOVE WS-CMAP-ENTRY(WS-FC-CMAP-TBL
                   WS-FC-PAL-IDX)
                   TO WS-BIN-BUF1
               MOVE WS-BIN-BYTE TO WS-FC-LIT-IDX
               ADD 1 TO WS-FC-LIT-IDX
               IF WS-FC-LIT-IDX > 256
                   MOVE 1 TO WS-FC-LIT-IDX
               END-IF

               MOVE WS-PAL-R(WS-FC-LIT-IDX)
                   TO WS-FB-BYTE(WS-FC-CEIL-FB-IDX)
               ADD 1 TO WS-FC-CEIL-FB-IDX
               MOVE WS-PAL-G(WS-FC-LIT-IDX)
                   TO WS-FB-BYTE(WS-FC-CEIL-FB-IDX)
               ADD 1 TO WS-FC-CEIL-FB-IDX
               MOVE WS-PAL-B(WS-FC-LIT-IDX)
                   TO WS-FB-BYTE(WS-FC-CEIL-FB-IDX)
               ADD 1 TO WS-FC-CEIL-FB-IDX
               MOVE X"FF"
                   TO WS-FB-BYTE(WS-FC-CEIL-FB-IDX)
               ADD 1 TO WS-FC-CEIL-FB-IDX

               ADD WS-FC-STEP-X TO WS-FC-FLOOR-X
               ADD WS-FC-STEP-Y TO WS-FC-FLOOR-Y

           END-PERFORM
           .
