      *> ============================================================
      *> map-loader-proc.cpy — MAP01 loader procedures
      *> Reads VERTEXES, LINEDEFS, SIDEDEFS, SECTORS, THINGS from
      *> WAD and rasterizes vector geometry to 128x128 grid.
      *> ============================================================

       LOAD-MAP.
      *>   Find "MAP01" marker lump
           MOVE "MAP01   " TO WS-FIND-NAME
           PERFORM FIND-LUMP
           IF WS-FOUND NOT = 1
               DISPLAY "WARN: MAP01 not found in WAD"
               EXIT PARAGRAPH
           END-IF
           MOVE WS-FIND-IDX TO WS-MAP-LUMP-IDX
           DISPLAY "MAP01 found at lump index " WS-MAP-LUMP-IDX

      *>   Read VERTEXES (marker + 4)
           COMPUTE WS-ML-I = WS-MAP-LUMP-IDX + 4
           MOVE WL-OFFSET(WS-ML-I) TO WS-WAD-OFFSET
           MOVE WL-SIZE(WS-ML-I) TO WS-WAD-RD-SIZE
           IF WS-WAD-RD-SIZE > 8000
               MOVE 8000 TO WS-WAD-RD-SIZE
           END-IF
           CALL "wad_read" USING WS-WAD-OFFSET
                                 WS-WAD-RD-SIZE
                                 WS-VERT-BUF
           COMPUTE WS-VERT-COUNT = WL-SIZE(WS-ML-I) / 4
           IF WS-VERT-COUNT > 2000
               MOVE 2000 TO WS-VERT-COUNT
           END-IF
           DISPLAY "  VERTEXES: " WS-VERT-COUNT

      *>   Read LINEDEFS (marker + 2)
           COMPUTE WS-ML-I = WS-MAP-LUMP-IDX + 2
           MOVE WL-OFFSET(WS-ML-I) TO WS-WAD-OFFSET
           MOVE WL-SIZE(WS-ML-I) TO WS-WAD-RD-SIZE
           IF WS-WAD-RD-SIZE > 21000
               MOVE 21000 TO WS-WAD-RD-SIZE
           END-IF
           CALL "wad_read" USING WS-WAD-OFFSET
                                 WS-WAD-RD-SIZE
                                 WS-LINE-BUF
           COMPUTE WS-LINE-COUNT = WL-SIZE(WS-ML-I) / 14
           IF WS-LINE-COUNT > 1500
               MOVE 1500 TO WS-LINE-COUNT
           END-IF
           DISPLAY "  LINEDEFS: " WS-LINE-COUNT

      *>   Read SIDEDEFS (marker + 3)
           COMPUTE WS-ML-I = WS-MAP-LUMP-IDX + 3
           MOVE WL-OFFSET(WS-ML-I) TO WS-WAD-OFFSET
           MOVE WL-SIZE(WS-ML-I) TO WS-WAD-RD-SIZE
           IF WS-WAD-RD-SIZE > 60000
               MOVE 60000 TO WS-WAD-RD-SIZE
           END-IF
           CALL "wad_read" USING WS-WAD-OFFSET
                                 WS-WAD-RD-SIZE
                                 WS-SIDE-BUF
           COMPUTE WS-SIDE-COUNT = WL-SIZE(WS-ML-I) / 30
           IF WS-SIDE-COUNT > 2000
               MOVE 2000 TO WS-SIDE-COUNT
           END-IF
           DISPLAY "  SIDEDEFS: " WS-SIDE-COUNT

      *>   Read SECTORS (marker + 8)
           COMPUTE WS-ML-I = WS-MAP-LUMP-IDX + 8
           MOVE WL-OFFSET(WS-ML-I) TO WS-WAD-OFFSET
           MOVE WL-SIZE(WS-ML-I) TO WS-WAD-RD-SIZE
           IF WS-WAD-RD-SIZE > 5200
               MOVE 5200 TO WS-WAD-RD-SIZE
           END-IF
           CALL "wad_read" USING WS-WAD-OFFSET
                                 WS-WAD-RD-SIZE
                                 WS-SECT-BUF
           COMPUTE WS-SECT-COUNT = WL-SIZE(WS-ML-I) / 26
           IF WS-SECT-COUNT > 200
               MOVE 200 TO WS-SECT-COUNT
           END-IF
           DISPLAY "  SECTORS: " WS-SECT-COUNT

      *>   Read THINGS (marker + 1)
           COMPUTE WS-ML-I = WS-MAP-LUMP-IDX + 1
           MOVE WL-OFFSET(WS-ML-I) TO WS-WAD-OFFSET
           MOVE WL-SIZE(WS-ML-I) TO WS-WAD-RD-SIZE
           IF WS-WAD-RD-SIZE > 3000
               MOVE 3000 TO WS-WAD-RD-SIZE
           END-IF
           CALL "wad_read" USING WS-WAD-OFFSET
                                 WS-WAD-RD-SIZE
                                 WS-THING-BUF
           COMPUTE WS-THING-RAW-COUNT = WL-SIZE(WS-ML-I) / 10
           IF WS-THING-RAW-COUNT > 300
               MOVE 300 TO WS-THING-RAW-COUNT
           END-IF
           DISPLAY "  THINGS: " WS-THING-RAW-COUNT

      *>   Parse and build
           PERFORM PARSE-VERTEXES
           PERFORM BUILD-MAP-GRID
           PERFORM LOAD-THINGS

      *>   Mark WAD map as active
           MOVE 1 TO WS-USE-WAD-MAP
           MOVE 128 TO WS-MAP-SIZE

           DISPLAY "MAP01 loaded successfully."
           DISPLAY "  Player spawn: "
               WS-SPAWN-X " , " WS-SPAWN-Y
               " angle=" WS-SPAWN-ANGLE
           .

       PARSE-VERTEXES.
           PERFORM VARYING WS-ML-I FROM 1 BY 1
               UNTIL WS-ML-I > WS-VERT-COUNT
               COMPUTE WS-ML-OFF =
                   (WS-ML-I - 1) * 4 + 1
               MOVE WS-VERT-BUF(WS-ML-OFF:2)
                   TO WS-BIN-BUF2
               MOVE WS-BIN-INT16 TO WS-VTX-X(WS-ML-I)
               ADD 2 TO WS-ML-OFF
               MOVE WS-VERT-BUF(WS-ML-OFF:2)
                   TO WS-BIN-BUF2
               MOVE WS-BIN-INT16 TO WS-VTX-Y(WS-ML-I)
           END-PERFORM
           .

       BUILD-MAP-GRID.
      *>   Bounding box
           MOVE +32767 TO WS-MAP-MIN-X
           MOVE -32767 TO WS-MAP-MAX-X
           MOVE +32767 TO WS-MAP-MIN-Y
           MOVE -32767 TO WS-MAP-MAX-Y

           PERFORM VARYING WS-ML-I FROM 1 BY 1
               UNTIL WS-ML-I > WS-VERT-COUNT
               IF WS-VTX-X(WS-ML-I) < WS-MAP-MIN-X
                   MOVE WS-VTX-X(WS-ML-I) TO WS-MAP-MIN-X
               END-IF
               IF WS-VTX-X(WS-ML-I) > WS-MAP-MAX-X
                   MOVE WS-VTX-X(WS-ML-I) TO WS-MAP-MAX-X
               END-IF
               IF WS-VTX-Y(WS-ML-I) < WS-MAP-MIN-Y
                   MOVE WS-VTX-Y(WS-ML-I) TO WS-MAP-MIN-Y
               END-IF
               IF WS-VTX-Y(WS-ML-I) > WS-MAP-MAX-Y
                   MOVE WS-VTX-Y(WS-ML-I) TO WS-MAP-MAX-Y
               END-IF
           END-PERFORM

           DISPLAY "  Bounds: x=" WS-MAP-MIN-X ".."
               WS-MAP-MAX-X " y=" WS-MAP-MIN-Y ".."
               WS-MAP-MAX-Y

      *>   Compute scale
           COMPUTE WS-MAP-RANGE-X =
               WS-MAP-MAX-X - WS-MAP-MIN-X
           COMPUTE WS-MAP-RANGE-Y =
               WS-MAP-MAX-Y - WS-MAP-MIN-Y
           IF WS-MAP-RANGE-X > WS-MAP-RANGE-Y
               COMPUTE WS-MAP-SCALE =
                   (WS-MAP-RANGE-X + 125) / 126
           ELSE
               COMPUTE WS-MAP-SCALE =
                   (WS-MAP-RANGE-Y + 125) / 126
           END-IF
           IF WS-MAP-SCALE < 1
               MOVE 1 TO WS-MAP-SCALE
           END-IF
           DISPLAY "  Scale: " WS-MAP-SCALE " map units/cell"

      *>   Initialize grid as empty
           PERFORM VARYING WS-ML-I FROM 1 BY 1
               UNTIL WS-ML-I > 128
               PERFORM VARYING WS-ML-J FROM 1 BY 1
                   UNTIL WS-ML-J > 128
                   MOVE 0 TO MC-TYPE(WS-ML-I, WS-ML-J)
                   MOVE 0 TO MC-TEX-ID(WS-ML-I, WS-ML-J)
                   MOVE WS-DEF-FLOOR-H
                       TO MC-FLOOR-H(WS-ML-I, WS-ML-J)
                   MOVE WS-DEF-CEIL-H
                       TO MC-CEIL-H(WS-ML-I, WS-ML-J)
                   MOVE WS-DEF-FLOOR-T
                       TO MC-FLOOR-TEX(WS-ML-I, WS-ML-J)
                   MOVE WS-DEF-CEIL-T
                       TO MC-CEIL-TEX(WS-ML-I, WS-ML-J)
                   MOVE WS-DEF-LIGHT
                       TO MC-LIGHT(WS-ML-I, WS-ML-J)
               END-PERFORM
           END-PERFORM

      *>   Border walls
           PERFORM VARYING WS-ML-I FROM 1 BY 1
               UNTIL WS-ML-I > 128
               MOVE 1 TO MC-TYPE(1, WS-ML-I)
               MOVE 1 TO MC-TEX-ID(1, WS-ML-I)
               MOVE 1 TO MC-TYPE(128, WS-ML-I)
               MOVE 1 TO MC-TEX-ID(128, WS-ML-I)
               MOVE 1 TO MC-TYPE(WS-ML-I, 1)
               MOVE 1 TO MC-TEX-ID(WS-ML-I, 1)
               MOVE 1 TO MC-TYPE(WS-ML-I, 128)
               MOVE 1 TO MC-TEX-ID(WS-ML-I, 128)
           END-PERFORM

      *>   Rasterize linedefs onto grid
           PERFORM VARYING WS-ML-I FROM 1 BY 1
               UNTIL WS-ML-I > WS-LINE-COUNT
               PERFORM PROCESS-ONE-LINEDEF
           END-PERFORM

      *>   Apply sector properties
           PERFORM APPLY-SECTOR-DEFAULTS

           DISPLAY "  Grid built. Rasterized "
               WS-LINE-COUNT " linedefs."
           .

       PROCESS-ONE-LINEDEF.
           COMPUTE WS-ML-OFF =
               (WS-ML-I - 1) * 14 + 1

      *>   start_vertex
           MOVE WS-LINE-BUF(WS-ML-OFF:2) TO WS-BIN-BUF2
           IF WS-BIN-INT16 < 0
               COMPUTE WS-LD-V1 = WS-BIN-INT16 + 65536
           ELSE
               MOVE WS-BIN-INT16 TO WS-LD-V1
           END-IF
           ADD 1 TO WS-LD-V1

      *>   end_vertex
           ADD 2 TO WS-ML-OFF
           MOVE WS-LINE-BUF(WS-ML-OFF:2) TO WS-BIN-BUF2
           IF WS-BIN-INT16 < 0
               COMPUTE WS-LD-V2 = WS-BIN-INT16 + 65536
           ELSE
               MOVE WS-BIN-INT16 TO WS-LD-V2
           END-IF
           ADD 1 TO WS-LD-V2

      *>   flags
           ADD 2 TO WS-ML-OFF
           MOVE WS-LINE-BUF(WS-ML-OFF:2) TO WS-BIN-BUF2
           IF WS-BIN-INT16 < 0
               COMPUTE WS-LD-FLAGS = WS-BIN-INT16 + 65536
           ELSE
               MOVE WS-BIN-INT16 TO WS-LD-FLAGS
           END-IF

      *>   special_type
           ADD 2 TO WS-ML-OFF
           MOVE WS-LINE-BUF(WS-ML-OFF:2) TO WS-BIN-BUF2
           IF WS-BIN-INT16 < 0
               COMPUTE WS-LD-SPECIAL = WS-BIN-INT16 + 65536
           ELSE
               MOVE WS-BIN-INT16 TO WS-LD-SPECIAL
           END-IF

      *>   sector_tag
           ADD 2 TO WS-ML-OFF
           MOVE WS-LINE-BUF(WS-ML-OFF:2) TO WS-BIN-BUF2
           IF WS-BIN-INT16 < 0
               COMPUTE WS-LD-TAG = WS-BIN-INT16 + 65536
           ELSE
               MOVE WS-BIN-INT16 TO WS-LD-TAG
           END-IF

      *>   right_sidedef
           ADD 2 TO WS-ML-OFF
           MOVE WS-LINE-BUF(WS-ML-OFF:2) TO WS-BIN-BUF2
           IF WS-BIN-INT16 < 0
               COMPUTE WS-LD-RSDEF = WS-BIN-INT16 + 65536
           ELSE
               MOVE WS-BIN-INT16 TO WS-LD-RSDEF
           END-IF

      *>   left_sidedef
           ADD 2 TO WS-ML-OFF
           MOVE WS-LINE-BUF(WS-ML-OFF:2) TO WS-BIN-BUF2
           IF WS-BIN-INT16 < 0
               COMPUTE WS-LD-LSDEF = WS-BIN-INT16 + 65536
           ELSE
               MOVE WS-BIN-INT16 TO WS-LD-LSDEF
           END-IF

      *>   Two-sided check
           COMPUTE WS-LD-TWOSIDED =
               FUNCTION MOD(WS-LD-FLAGS / 4, 2)

      *>   Skip if no right sidedef
           IF WS-LD-RSDEF = 65535
               EXIT PARAGRAPH
           END-IF

      *>   Check blocking or one-sided
           IF FUNCTION MOD(WS-LD-FLAGS, 2) = 0
               AND WS-LD-LSDEF NOT = 65535
      *>       Two-sided non-blocking: check door specials
               IF WS-LD-SPECIAL = 1 OR WS-LD-SPECIAL = 26
                   OR WS-LD-SPECIAL = 27 OR WS-LD-SPECIAL = 28
                   OR WS-LD-SPECIAL = 31 OR WS-LD-SPECIAL = 32
                   OR WS-LD-SPECIAL = 33 OR WS-LD-SPECIAL = 34
                   OR WS-LD-SPECIAL = 46
                   OR WS-LD-SPECIAL = 117
                   OR WS-LD-SPECIAL = 118
                   MOVE 2 TO WS-GR-TYPE
               ELSE
                   EXIT PARAGRAPH
               END-IF
           ELSE
               MOVE 1 TO WS-GR-TYPE
           END-IF

      *>   Validate vertices
           IF WS-LD-V1 < 1 OR WS-LD-V1 > WS-VERT-COUNT
               EXIT PARAGRAPH
           END-IF
           IF WS-LD-V2 < 1 OR WS-LD-V2 > WS-VERT-COUNT
               EXIT PARAGRAPH
           END-IF

      *>   Get vertex coordinates
           MOVE WS-VTX-X(WS-LD-V1) TO WS-LN-X1
           MOVE WS-VTX-Y(WS-LD-V1) TO WS-LN-Y1
           MOVE WS-VTX-X(WS-LD-V2) TO WS-LN-X2
           MOVE WS-VTX-Y(WS-LD-V2) TO WS-LN-Y2

      *>   Get texture from right sidedef
           MOVE 1 TO WS-GR-TEX
           IF WS-LD-RSDEF < WS-SIDE-COUNT
               PERFORM EXTRACT-SIDEDEF-TEX
           END-IF

      *>   Get sector properties
           MOVE WS-DEF-FLOOR-H TO WS-GR-FLOOR-H
           MOVE WS-DEF-CEIL-H TO WS-GR-CEIL-H
           MOVE WS-DEF-FLOOR-T TO WS-GR-FLOOR-T
           MOVE WS-DEF-CEIL-T TO WS-GR-CEIL-T
           MOVE WS-DEF-LIGHT TO WS-GR-LIGHT
           IF WS-LD-RSDEF < WS-SIDE-COUNT
               PERFORM EXTRACT-SECTOR-PROPS
           END-IF

      *>   Convert vertex coords to grid coords
           COMPUTE WS-BH-X0 =
               (WS-LN-X1 - WS-MAP-MIN-X) / WS-MAP-SCALE + 2
           COMPUTE WS-BH-Y0 =
               (WS-LN-Y1 - WS-MAP-MIN-Y) / WS-MAP-SCALE + 2
           COMPUTE WS-BH-X1 =
               (WS-LN-X2 - WS-MAP-MIN-X) / WS-MAP-SCALE + 2
           COMPUTE WS-BH-Y1 =
               (WS-LN-Y2 - WS-MAP-MIN-Y) / WS-MAP-SCALE + 2

      *>   Clamp to grid bounds
           IF WS-BH-X0 < 2
               MOVE 2 TO WS-BH-X0
           END-IF
           IF WS-BH-X0 > 127
               MOVE 127 TO WS-BH-X0
           END-IF
           IF WS-BH-Y0 < 2
               MOVE 2 TO WS-BH-Y0
           END-IF
           IF WS-BH-Y0 > 127
               MOVE 127 TO WS-BH-Y0
           END-IF
           IF WS-BH-X1 < 2
               MOVE 2 TO WS-BH-X1
           END-IF
           IF WS-BH-X1 > 127
               MOVE 127 TO WS-BH-X1
           END-IF
           IF WS-BH-Y1 < 2
               MOVE 2 TO WS-BH-Y1
           END-IF
           IF WS-BH-Y1 > 127
               MOVE 127 TO WS-BH-Y1
           END-IF

           PERFORM RASTERIZE-LINE
           .

       EXTRACT-SIDEDEF-TEX.
           COMPUTE WS-ML-OFF =
               WS-LD-RSDEF * 30 + 1

      *>   Middle texture at offset +20
           ADD 20 TO WS-ML-OFF
           MOVE WS-SIDE-BUF(WS-ML-OFF:8)
               TO WS-SD-MIDDLE
           INSPECT WS-SD-MIDDLE
               REPLACING ALL X"00" BY SPACE

           IF WS-SD-MIDDLE = "-       "
               OR WS-SD-MIDDLE = SPACES
      *>       Try upper texture
               COMPUTE WS-ML-OFF =
                   WS-LD-RSDEF * 30 + 1 + 4
               MOVE WS-SIDE-BUF(WS-ML-OFF:8)
                   TO WS-SD-UPPER
               INSPECT WS-SD-UPPER
                   REPLACING ALL X"00" BY SPACE
               IF WS-SD-UPPER NOT = "-       "
                   AND WS-SD-UPPER NOT = SPACES
                   MOVE WS-SD-UPPER TO WS-SEARCH-NAME
                   PERFORM FIND-TEX-BY-NAME
                   IF WS-SEARCH-RESULT > 0
                       MOVE WS-SEARCH-RESULT TO WS-GR-TEX
                   END-IF
               END-IF
           ELSE
               MOVE WS-SD-MIDDLE TO WS-SEARCH-NAME
               PERFORM FIND-TEX-BY-NAME
               IF WS-SEARCH-RESULT > 0
                   MOVE WS-SEARCH-RESULT TO WS-GR-TEX
               END-IF
           END-IF
           .

       EXTRACT-SECTOR-PROPS.
      *>   sector_num from sidedef (uint16 at +28)
           COMPUTE WS-ML-OFF =
               WS-LD-RSDEF * 30 + 1 + 28
           MOVE WS-SIDE-BUF(WS-ML-OFF:2) TO WS-BIN-BUF2
           IF WS-BIN-INT16 < 0
               COMPUTE WS-SD-SECTOR = WS-BIN-INT16 + 65536
           ELSE
               MOVE WS-BIN-INT16 TO WS-SD-SECTOR
           END-IF

           IF WS-SD-SECTOR >= WS-SECT-COUNT
               EXIT PARAGRAPH
           END-IF

      *>   Read sector data (26 bytes per sector)
           COMPUTE WS-ML-OFF =
               WS-SD-SECTOR * 26 + 1

      *>   Floor height
           MOVE WS-SECT-BUF(WS-ML-OFF:2) TO WS-BIN-BUF2
           MOVE WS-BIN-INT16 TO WS-SC-FLOOR-H
           IF WS-SC-FLOOR-H > 999
               MOVE 999 TO WS-SC-FLOOR-H
           END-IF
           IF WS-SC-FLOOR-H < -999
               MOVE -999 TO WS-SC-FLOOR-H
           END-IF
           MOVE WS-SC-FLOOR-H TO WS-GR-FLOOR-H

      *>   Ceiling height
           ADD 2 TO WS-ML-OFF
           MOVE WS-SECT-BUF(WS-ML-OFF:2) TO WS-BIN-BUF2
           MOVE WS-BIN-INT16 TO WS-SC-CEIL-H
           IF WS-SC-CEIL-H > 999
               MOVE 999 TO WS-SC-CEIL-H
           END-IF
           IF WS-SC-CEIL-H < -999
               MOVE -999 TO WS-SC-CEIL-H
           END-IF
           MOVE WS-SC-CEIL-H TO WS-GR-CEIL-H

      *>   Floor texture
           ADD 2 TO WS-ML-OFF
           MOVE WS-SECT-BUF(WS-ML-OFF:8)
               TO WS-SC-FLOOR-TEX
           INSPECT WS-SC-FLOOR-TEX
               REPLACING ALL X"00" BY SPACE
           MOVE WS-SC-FLOOR-TEX TO WS-SEARCH-NAME
           PERFORM FIND-FLAT-BY-NAME
           IF WS-SEARCH-RESULT > 0
               MOVE WS-SEARCH-RESULT TO WS-GR-FLOOR-T
           END-IF

      *>   Ceiling texture
           ADD 8 TO WS-ML-OFF
           MOVE WS-SECT-BUF(WS-ML-OFF:8)
               TO WS-SC-CEIL-TEX
           INSPECT WS-SC-CEIL-TEX
               REPLACING ALL X"00" BY SPACE
           MOVE WS-SC-CEIL-TEX TO WS-SEARCH-NAME
           PERFORM FIND-FLAT-BY-NAME
           IF WS-SEARCH-RESULT > 0
               MOVE WS-SEARCH-RESULT TO WS-GR-CEIL-T
           END-IF

      *>   Light level
           ADD 8 TO WS-ML-OFF
           MOVE WS-SECT-BUF(WS-ML-OFF:2) TO WS-BIN-BUF2
           IF WS-BIN-INT16 < 0
               COMPUTE WS-SC-LIGHT = WS-BIN-INT16 + 65536
           ELSE
               MOVE WS-BIN-INT16 TO WS-SC-LIGHT
           END-IF
           IF WS-SC-LIGHT > 255
               MOVE 255 TO WS-SC-LIGHT
           END-IF
           MOVE WS-SC-LIGHT TO WS-GR-LIGHT
           .

       RASTERIZE-LINE.
           COMPUTE WS-BH-DX =
               FUNCTION ABS(WS-BH-X1 - WS-BH-X0)
           COMPUTE WS-BH-DY =
               FUNCTION ABS(WS-BH-Y1 - WS-BH-Y0)

           IF WS-BH-X0 < WS-BH-X1
               MOVE 1 TO WS-BH-SX
           ELSE
               MOVE -1 TO WS-BH-SX
           END-IF
           IF WS-BH-Y0 < WS-BH-Y1
               MOVE 1 TO WS-BH-SY
           ELSE
               MOVE -1 TO WS-BH-SY
           END-IF

           COMPUTE WS-BH-ERR = WS-BH-DX - WS-BH-DY

           MOVE WS-BH-X0 TO WS-BH-CUR-X
           MOVE WS-BH-Y0 TO WS-BH-CUR-Y
           MOVE 0 TO WS-BH-DONE

           PERFORM UNTIL WS-BH-DONE = 1
               MOVE WS-BH-CUR-Y TO WS-GR-ROW
               MOVE WS-BH-CUR-X TO WS-GR-COL

               IF WS-GR-ROW >= 1 AND WS-GR-ROW <= 128
                   AND WS-GR-COL >= 1 AND WS-GR-COL <= 128
                   IF MC-TYPE(WS-GR-ROW, WS-GR-COL) = 0
                       MOVE WS-GR-TYPE
                           TO MC-TYPE(WS-GR-ROW, WS-GR-COL)
                       MOVE WS-GR-TEX
                           TO MC-TEX-ID(WS-GR-ROW, WS-GR-COL)
                       MOVE WS-GR-FLOOR-H
                           TO MC-FLOOR-H(WS-GR-ROW, WS-GR-COL)
                       MOVE WS-GR-CEIL-H
                           TO MC-CEIL-H(WS-GR-ROW, WS-GR-COL)
                       MOVE WS-GR-FLOOR-T
                           TO MC-FLOOR-TEX(WS-GR-ROW,
                                           WS-GR-COL)
                       MOVE WS-GR-CEIL-T
                           TO MC-CEIL-TEX(WS-GR-ROW,
                                          WS-GR-COL)
                       MOVE WS-GR-LIGHT
                           TO MC-LIGHT(WS-GR-ROW, WS-GR-COL)
                   END-IF
               END-IF

               IF WS-BH-CUR-X = WS-BH-X1
                   AND WS-BH-CUR-Y = WS-BH-Y1
                   MOVE 1 TO WS-BH-DONE
               ELSE
                   COMPUTE WS-BH-E2 = WS-BH-ERR * 2
                   IF WS-BH-E2 > (0 - WS-BH-DY)
                       SUBTRACT WS-BH-DY FROM WS-BH-ERR
                       ADD WS-BH-SX TO WS-BH-CUR-X
                   END-IF
                   IF WS-BH-E2 < WS-BH-DX
                       ADD WS-BH-DX TO WS-BH-ERR
                       ADD WS-BH-SY TO WS-BH-CUR-Y
                   END-IF
               END-IF
           END-PERFORM
           .

       APPLY-SECTOR-DEFAULTS.
           PERFORM 3 TIMES
               PERFORM VARYING WS-ML-I FROM 2 BY 1
                   UNTIL WS-ML-I > 127
                   PERFORM VARYING WS-ML-J FROM 2 BY 1
                       UNTIL WS-ML-J > 127
                       IF MC-TYPE(WS-ML-I, WS-ML-J) = 0
                           AND MC-LIGHT(WS-ML-I, WS-ML-J)
                               = WS-DEF-LIGHT
                           PERFORM PROPAGATE-SECTOR-TO-CELL
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM
           .

       PROPAGATE-SECTOR-TO-CELL.
      *>   Check above
           COMPUTE WS-GR-ROW = WS-ML-I - 1
           IF WS-GR-ROW >= 1
               IF MC-LIGHT(WS-GR-ROW, WS-ML-J)
                   NOT = WS-DEF-LIGHT
                   PERFORM COPY-SECTOR-FROM-NEIGHBOR
                   EXIT PARAGRAPH
               END-IF
           END-IF
      *>   Check below
           COMPUTE WS-GR-ROW = WS-ML-I + 1
           IF WS-GR-ROW <= 128
               IF MC-LIGHT(WS-GR-ROW, WS-ML-J)
                   NOT = WS-DEF-LIGHT
                   PERFORM COPY-SECTOR-FROM-NEIGHBOR
                   EXIT PARAGRAPH
               END-IF
           END-IF
      *>   Check left
           COMPUTE WS-GR-COL = WS-ML-J - 1
           IF WS-GR-COL >= 1
               IF MC-LIGHT(WS-ML-I, WS-GR-COL)
                   NOT = WS-DEF-LIGHT
                   PERFORM COPY-SECTOR-FROM-LEFT
                   EXIT PARAGRAPH
               END-IF
           END-IF
      *>   Check right
           COMPUTE WS-GR-COL = WS-ML-J + 1
           IF WS-GR-COL <= 128
               IF MC-LIGHT(WS-ML-I, WS-GR-COL)
                   NOT = WS-DEF-LIGHT
                   PERFORM COPY-SECTOR-FROM-RIGHT
                   EXIT PARAGRAPH
               END-IF
           END-IF
           .

       COPY-SECTOR-FROM-NEIGHBOR.
           MOVE MC-FLOOR-H(WS-GR-ROW, WS-ML-J)
               TO MC-FLOOR-H(WS-ML-I, WS-ML-J)
           MOVE MC-CEIL-H(WS-GR-ROW, WS-ML-J)
               TO MC-CEIL-H(WS-ML-I, WS-ML-J)
           MOVE MC-FLOOR-TEX(WS-GR-ROW, WS-ML-J)
               TO MC-FLOOR-TEX(WS-ML-I, WS-ML-J)
           MOVE MC-CEIL-TEX(WS-GR-ROW, WS-ML-J)
               TO MC-CEIL-TEX(WS-ML-I, WS-ML-J)
           MOVE MC-LIGHT(WS-GR-ROW, WS-ML-J)
               TO MC-LIGHT(WS-ML-I, WS-ML-J)
           .

       COPY-SECTOR-FROM-LEFT.
           COMPUTE WS-GR-COL = WS-ML-J - 1
           MOVE MC-FLOOR-H(WS-ML-I, WS-GR-COL)
               TO MC-FLOOR-H(WS-ML-I, WS-ML-J)
           MOVE MC-CEIL-H(WS-ML-I, WS-GR-COL)
               TO MC-CEIL-H(WS-ML-I, WS-ML-J)
           MOVE MC-FLOOR-TEX(WS-ML-I, WS-GR-COL)
               TO MC-FLOOR-TEX(WS-ML-I, WS-ML-J)
           MOVE MC-CEIL-TEX(WS-ML-I, WS-GR-COL)
               TO MC-CEIL-TEX(WS-ML-I, WS-ML-J)
           MOVE MC-LIGHT(WS-ML-I, WS-GR-COL)
               TO MC-LIGHT(WS-ML-I, WS-ML-J)
           .

       COPY-SECTOR-FROM-RIGHT.
           COMPUTE WS-GR-COL = WS-ML-J + 1
           MOVE MC-FLOOR-H(WS-ML-I, WS-GR-COL)
               TO MC-FLOOR-H(WS-ML-I, WS-ML-J)
           MOVE MC-CEIL-H(WS-ML-I, WS-GR-COL)
               TO MC-CEIL-H(WS-ML-I, WS-ML-J)
           MOVE MC-FLOOR-TEX(WS-ML-I, WS-GR-COL)
               TO MC-FLOOR-TEX(WS-ML-I, WS-ML-J)
           MOVE MC-CEIL-TEX(WS-ML-I, WS-GR-COL)
               TO MC-CEIL-TEX(WS-ML-I, WS-ML-J)
           MOVE MC-LIGHT(WS-ML-I, WS-GR-COL)
               TO MC-LIGHT(WS-ML-I, WS-ML-J)
           .

       LOAD-THINGS.
           MOVE 0 TO WS-MAP-THING-COUNT
           MOVE 0 TO WS-SPAWN-X
           MOVE 0 TO WS-SPAWN-Y
           MOVE 0 TO WS-SPAWN-ANGLE

           PERFORM VARYING WS-ML-I FROM 1 BY 1
               UNTIL WS-ML-I > WS-THING-RAW-COUNT
               COMPUTE WS-ML-OFF =
                   (WS-ML-I - 1) * 10 + 1

      *>       x
               MOVE WS-THING-BUF(WS-ML-OFF:2)
                   TO WS-BIN-BUF2
               MOVE WS-BIN-INT16 TO WS-LN-X1

      *>       y
               ADD 2 TO WS-ML-OFF
               MOVE WS-THING-BUF(WS-ML-OFF:2)
                   TO WS-BIN-BUF2
               MOVE WS-BIN-INT16 TO WS-LN-Y1

      *>       angle
               ADD 2 TO WS-ML-OFF
               MOVE WS-THING-BUF(WS-ML-OFF:2)
                   TO WS-BIN-BUF2
               IF WS-BIN-INT16 < 0
                   COMPUTE WS-ML-UINT16 =
                       WS-BIN-INT16 + 65536
               ELSE
                   MOVE WS-BIN-INT16 TO WS-ML-UINT16
               END-IF

      *>       type
               ADD 2 TO WS-ML-OFF
               MOVE WS-THING-BUF(WS-ML-OFF:2)
                   TO WS-BIN-BUF2
               IF WS-BIN-INT16 < 0
                   COMPUTE WS-LD-SPECIAL =
                       WS-BIN-INT16 + 65536
               ELSE
                   MOVE WS-BIN-INT16 TO WS-LD-SPECIAL
               END-IF

      *>       flags
               ADD 2 TO WS-ML-OFF
               MOVE WS-THING-BUF(WS-ML-OFF:2)
                   TO WS-BIN-BUF2
               IF WS-BIN-INT16 < 0
                   COMPUTE WS-LD-FLAGS =
                       WS-BIN-INT16 + 65536
               ELSE
                   MOVE WS-BIN-INT16 TO WS-LD-FLAGS
               END-IF

      *>       Player 1 start (type = 1)
               IF WS-LD-SPECIAL = 1
                   COMPUTE WS-SPAWN-X =
                       (WS-LN-X1 - WS-MAP-MIN-X)
                       / WS-MAP-SCALE + 2 - 0.5
                   COMPUTE WS-SPAWN-Y =
                       (WS-LN-Y1 - WS-MAP-MIN-Y)
                       / WS-MAP-SCALE + 2 - 0.5
                   MOVE WS-ML-UINT16 TO WS-SPAWN-ANGLE
                   COMPUTE WS-SPAWN-ANGLE =
                       FUNCTION MOD(WS-SPAWN-ANGLE, 360)
               END-IF

      *>       Store thing
               IF WS-MAP-THING-COUNT < 200
                   ADD 1 TO WS-MAP-THING-COUNT
                   MOVE WS-LN-X1
                       TO MT-X(WS-MAP-THING-COUNT)
                   MOVE WS-LN-Y1
                       TO MT-Y(WS-MAP-THING-COUNT)
                   MOVE WS-ML-UINT16
                       TO MT-ANGLE(WS-MAP-THING-COUNT)
                   MOVE WS-LD-SPECIAL
                       TO MT-TYPE(WS-MAP-THING-COUNT)
                   MOVE WS-LD-FLAGS
                       TO MT-FLAGS(WS-MAP-THING-COUNT)
               END-IF
           END-PERFORM

           DISPLAY "  Things loaded: " WS-MAP-THING-COUNT
           .

       FIND-TEX-BY-NAME.
           MOVE 0 TO WS-SEARCH-RESULT
           PERFORM VARYING WS-ML-J FROM 1 BY 1
               UNTIL WS-ML-J > WS-TEX-COUNT
               OR WS-SEARCH-RESULT > 0
               IF WT-NAME(WS-ML-J) = WS-SEARCH-NAME
                   MOVE WS-ML-J TO WS-SEARCH-RESULT
               END-IF
           END-PERFORM
           .

       FIND-FLAT-BY-NAME.
           MOVE 0 TO WS-SEARCH-RESULT
           PERFORM VARYING WS-ML-J FROM 1 BY 1
               UNTIL WS-ML-J > WS-FLAT-COUNT
               OR WS-SEARCH-RESULT > 0
               IF WF-NAME(WS-ML-J) = WS-SEARCH-NAME
                   MOVE WS-ML-J TO WS-SEARCH-RESULT
               END-IF
           END-PERFORM
           .
