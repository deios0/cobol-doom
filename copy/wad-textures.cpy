      *> ============================================================
      *> WAD texture and flat loading procedures
      *> ============================================================

       LOAD-PNAMES.
           MOVE "PNAMES  " TO WS-FIND-NAME
           PERFORM FIND-LUMP
           IF WS-FOUND NOT = 1
               DISPLAY "WARN: PNAMES not found"
               EXIT PARAGRAPH
           END-IF
           MOVE WS-FOUND-OFFSET TO WS-WAD-OFFSET
           MOVE WS-FOUND-SIZE TO WS-WAD-RD-SIZE
           IF WS-WAD-RD-SIZE > 65536
               MOVE 65536 TO WS-WAD-RD-SIZE
           END-IF
           CALL "wad_read" USING WS-WAD-OFFSET
                                 WS-WAD-RD-SIZE
                                 WS-LUMP-BUF
      *>   Count (int32 at bytes 1-4)
           MOVE WS-LUMP-BUF(1:4) TO WS-BIN-BUF4
           MOVE WS-BIN-INT32 TO WS-PNAME-COUNT
           IF WS-PNAME-COUNT > 500
               MOVE 500 TO WS-PNAME-COUNT
           END-IF
      *>   8-byte names starting at byte 5
           PERFORM VARYING WS-TEX-I FROM 1 BY 1
               UNTIL WS-TEX-I > WS-PNAME-COUNT
               COMPUTE WS-TEX-OFFSET =
                   (WS-TEX-I - 1) * 8 + 5
               MOVE WS-LUMP-BUF(WS-TEX-OFFSET:8)
                   TO WS-PNAME(WS-TEX-I)
               INSPECT WS-PNAME(WS-TEX-I)
                   REPLACING ALL X"00" BY SPACE
           END-PERFORM
           DISPLAY "PNAMES: " WS-PNAME-COUNT " patches"
           .

       LOAD-TEXTURE-DEFS.
           MOVE 0 TO WS-TEX-COUNT
           PERFORM LOAD-TEXTURE1
           PERFORM LOAD-TEXTURE2
           DISPLAY "Textures: " WS-TEX-COUNT " loaded"
           .

       LOAD-TEXTURE1.
           MOVE "TEXTURE1" TO WS-FIND-NAME
           PERFORM FIND-LUMP
           IF WS-FOUND NOT = 1
               EXIT PARAGRAPH
           END-IF
           PERFORM PARSE-TEXTURE-LUMP
           .

       LOAD-TEXTURE2.
           MOVE "TEXTURE2" TO WS-FIND-NAME
           PERFORM FIND-LUMP
           IF WS-FOUND NOT = 1
               EXIT PARAGRAPH
           END-IF
           PERFORM PARSE-TEXTURE-LUMP
           .

       PARSE-TEXTURE-LUMP.
      *>   Read entire TEXTUREx lump into buffer
           MOVE WS-FOUND-OFFSET TO WS-WAD-OFFSET
           MOVE WS-FOUND-SIZE TO WS-WAD-RD-SIZE
           IF WS-WAD-RD-SIZE > 65536
               MOVE 65536 TO WS-WAD-RD-SIZE
           END-IF
           CALL "wad_read" USING WS-WAD-OFFSET
                                 WS-WAD-RD-SIZE
                                 WS-LUMP-BUF
      *>   Texture count (int32 at bytes 1-4)
           MOVE WS-LUMP-BUF(1:4) TO WS-BIN-BUF4
           MOVE WS-BIN-INT32 TO WS-TEXDEF-COUNT
      *>   Process each definition (limit to 100 for speed)
           PERFORM VARYING WS-TEX-I FROM 1 BY 1
               UNTIL WS-TEX-I > WS-TEXDEF-COUNT
               OR WS-TEX-COUNT >= 100
      *>       Get offset to this texture def
               COMPUTE WS-TEX-OFFSET =
                   (WS-TEX-I - 1) * 4 + 5
               MOVE WS-LUMP-BUF(WS-TEX-OFFSET:4)
                   TO WS-BIN-BUF4
               MOVE WS-BIN-INT32 TO WS-TEXDEF-OFFSET
      *>       Convert to 1-based buffer position
               ADD 1 TO WS-TEXDEF-OFFSET
      *>       Parse this texture definition
               PERFORM PARSE-ONE-TEXTURE
           END-PERFORM
           .

       PARSE-ONE-TEXTURE.
           ADD 1 TO WS-TEX-COUNT
           MOVE WS-TEX-COUNT TO WS-CUR-TEX
      *>   Name (8 bytes at def offset)
           MOVE WS-LUMP-BUF(WS-TEXDEF-OFFSET:8)
               TO WT-NAME(WS-CUR-TEX)
           INSPECT WT-NAME(WS-CUR-TEX)
               REPLACING ALL X"00" BY SPACE
      *>   Width (int16 at def+12: after 8 name + 4 masked)
           COMPUTE WS-TEX-OFFSET = WS-TEXDEF-OFFSET + 12
           MOVE WS-LUMP-BUF(WS-TEX-OFFSET:2)
               TO WS-BIN-BUF2
           MOVE WS-BIN-INT16 TO WT-WIDTH(WS-CUR-TEX)
      *>   Height (int16 at def+14)
           ADD 2 TO WS-TEX-OFFSET
           MOVE WS-LUMP-BUF(WS-TEX-OFFSET:2)
               TO WS-BIN-BUF2
           MOVE WS-BIN-INT16 TO WT-HEIGHT(WS-CUR-TEX)
      *>   Skip textures too large for our buffer (128x128 max)
           IF WT-WIDTH(WS-CUR-TEX) > 128
               OR WT-HEIGHT(WS-CUR-TEX) > 128
               OR WT-WIDTH(WS-CUR-TEX) = 0
               OR WT-HEIGHT(WS-CUR-TEX) = 0
               SUBTRACT 1 FROM WS-TEX-COUNT
               EXIT PARAGRAPH
           END-IF
      *>   Patch count (int16 at def+20)
           COMPUTE WS-TEX-OFFSET = WS-TEXDEF-OFFSET + 20
           MOVE WS-LUMP-BUF(WS-TEX-OFFSET:2)
               TO WS-BIN-BUF2
           MOVE WS-BIN-INT16 TO WS-PATCH-CNT
      *>   Initialize pixels to transparent (0xFF)
           MOVE ALL X"FF" TO WT-PIXEL-DATA(WS-CUR-TEX)
      *>   Composite each patch
           COMPUTE WS-TEX-OFFSET = WS-TEXDEF-OFFSET + 22
           PERFORM VARYING WS-TEX-J FROM 1 BY 1
               UNTIL WS-TEX-J > WS-PATCH-CNT
      *>       originx (int16)
               MOVE WS-LUMP-BUF(WS-TEX-OFFSET:2)
                   TO WS-BIN-BUF2
               MOVE WS-BIN-INT16 TO WS-PATCH-ORIG-X
      *>       originy (int16)
               ADD 2 TO WS-TEX-OFFSET
               MOVE WS-LUMP-BUF(WS-TEX-OFFSET:2)
                   TO WS-BIN-BUF2
               MOVE WS-BIN-INT16 TO WS-PATCH-ORIG-Y
      *>       patch index into PNAMES (int16)
               ADD 2 TO WS-TEX-OFFSET
               MOVE WS-LUMP-BUF(WS-TEX-OFFSET:2)
                   TO WS-BIN-BUF2
               MOVE WS-BIN-INT16 TO WS-PATCH-IDX
      *>       Skip stepdir + colormap (4 bytes)
               ADD 6 TO WS-TEX-OFFSET
      *>       Load and composite this patch
               ADD 1 TO WS-PATCH-IDX
               IF WS-PATCH-IDX >= 1
                   AND WS-PATCH-IDX <= WS-PNAME-COUNT
                   MOVE WS-PNAME(WS-PATCH-IDX)
                       TO WS-PATCH-NAME
                   PERFORM COMPOSITE-PATCH
               END-IF
           END-PERFORM
           .

       COMPOSITE-PATCH.
      *>   Find the patch lump in WAD directory
           MOVE WS-PATCH-NAME TO WS-FIND-NAME
           PERFORM FIND-LUMP
           IF WS-FOUND NOT = 1
               EXIT PARAGRAPH
           END-IF
      *>   Read patch lump into patch buffer
           MOVE WS-FOUND-OFFSET TO WS-WAD-OFFSET
           MOVE WS-FOUND-SIZE TO WS-WAD-RD-SIZE
           IF WS-WAD-RD-SIZE > 65536
               MOVE 65536 TO WS-WAD-RD-SIZE
           END-IF
           CALL "wad_read" USING WS-WAD-OFFSET
                                 WS-WAD-RD-SIZE
                                 WS-PATCH-BUF
      *>   Patch header: width (int16), height (int16)
           MOVE WS-PATCH-BUF(1:2) TO WS-BIN-BUF2
           MOVE WS-BIN-INT16 TO WS-PATCH-W
           MOVE WS-PATCH-BUF(3:2) TO WS-BIN-BUF2
           MOVE WS-BIN-INT16 TO WS-PATCH-H
      *>   Column offsets start at byte 9 (after 8-byte header)
      *>   For each column of the patch
           PERFORM VARYING WS-PATCH-COL FROM 0 BY 1
               UNTIL WS-PATCH-COL >= WS-PATCH-W
      *>       Get column offset (int32 at byte 9 + col*4)
               COMPUTE WS-TEX-OFFSET =
                   WS-PATCH-COL * 4 + 9
               MOVE WS-PATCH-BUF(WS-TEX-OFFSET:4)
                   TO WS-BIN-BUF4
               MOVE WS-BIN-INT32 TO WS-PATCH-POS
      *>       Convert to 1-based
               ADD 1 TO WS-PATCH-POS
      *>       Target texture column
               COMPUTE WS-TEX-COL =
                   WS-PATCH-ORIG-X + WS-PATCH-COL
               IF WS-TEX-COL >= 0
                   AND WS-TEX-COL <
                       WT-WIDTH(WS-CUR-TEX)
                   PERFORM READ-PATCH-COLUMN
               END-IF
           END-PERFORM
           .

       READ-PATCH-COLUMN.
      *>   Parse column posts until topdelta = 0xFF
           MOVE 0 TO WS-EXIT-FLAG
           PERFORM UNTIL WS-EXIT-FLAG = 1
      *>       Read topdelta byte
               MOVE WS-PATCH-BUF(WS-PATCH-POS:1)
                   TO WS-BIN-BUF1
               MOVE WS-BIN-BYTE TO WS-POST-TOP
               IF WS-POST-TOP = 255
                   MOVE 1 TO WS-EXIT-FLAG
               ELSE
                   ADD 1 TO WS-PATCH-POS
      *>           Read length byte
                   MOVE WS-PATCH-BUF(WS-PATCH-POS:1)
                       TO WS-BIN-BUF1
                   MOVE WS-BIN-BYTE TO WS-POST-LEN
                   ADD 1 TO WS-PATCH-POS
      *>           Skip pre-padding byte
                   ADD 1 TO WS-PATCH-POS
      *>           Copy pixel data
                   PERFORM VARYING WS-PATCH-ROW FROM 0 BY 1
                       UNTIL WS-PATCH-ROW >= WS-POST-LEN
      *>               Target row in texture
                       COMPUTE WS-TEX-ROW =
                           WS-PATCH-ORIG-Y + WS-POST-TOP
                           + WS-PATCH-ROW
                       IF WS-TEX-ROW >= 0
                           AND WS-TEX-ROW <
                               WT-HEIGHT(WS-CUR-TEX)
      *>                   Column-major pixel offset
                           COMPUTE WS-TEX-PIX-OFF =
                               WS-TEX-COL
                               * WT-HEIGHT(WS-CUR-TEX)
                               + WS-TEX-ROW + 1
                           IF WS-TEX-PIX-OFF >= 1
                               AND WS-TEX-PIX-OFF
                                   <= 16384
                               MOVE WS-PATCH-BUF
                                   (WS-PATCH-POS:1)
                                   TO WT-PIX(WS-CUR-TEX
                                       WS-TEX-PIX-OFF)
                           END-IF
                       END-IF
                       ADD 1 TO WS-PATCH-POS
                   END-PERFORM
      *>           Skip post-padding byte
                   ADD 1 TO WS-PATCH-POS
               END-IF
           END-PERFORM
           .

       LOAD-FLATS.
           MOVE 0 TO WS-FLAT-COUNT
      *>   Find F_START marker
           MOVE "F_START " TO WS-FIND-NAME
           PERFORM FIND-LUMP
           IF WS-FOUND NOT = 1
               MOVE "FF_START" TO WS-FIND-NAME
               PERFORM FIND-LUMP
               IF WS-FOUND NOT = 1
                   DISPLAY "WARN: No flat markers found"
                   EXIT PARAGRAPH
               END-IF
           END-IF
           MOVE WS-FIND-IDX TO WS-F-START-IDX
      *>   Find F_END marker
           MOVE "F_END   " TO WS-FIND-NAME
           PERFORM FIND-LUMP
           IF WS-FOUND NOT = 1
               MOVE "FF_END  " TO WS-FIND-NAME
               PERFORM FIND-LUMP
               IF WS-FOUND NOT = 1
                   DISPLAY "WARN: No flat end marker"
                   EXIT PARAGRAPH
               END-IF
           END-IF
           MOVE WS-FIND-IDX TO WS-F-END-IDX
      *>   Load 4096-byte lumps between markers
           PERFORM VARYING WS-WAD-I
               FROM WS-F-START-IDX BY 1
               UNTIL WS-WAD-I > WS-F-END-IDX
               OR WS-FLAT-COUNT >= 100
               IF WL-SIZE(WS-WAD-I) = 4096
                   ADD 1 TO WS-FLAT-COUNT
                   MOVE WL-NAME(WS-WAD-I)
                       TO WF-NAME(WS-FLAT-COUNT)
                   MOVE WL-OFFSET(WS-WAD-I)
                       TO WS-WAD-OFFSET
                   MOVE 4096 TO WS-WAD-RD-SIZE
                   CALL "wad_read" USING WS-WAD-OFFSET
                                         WS-WAD-RD-SIZE
                                         WF-PIXEL-DATA(
                                         WS-FLAT-COUNT)
               END-IF
           END-PERFORM
           DISPLAY "Flats: " WS-FLAT-COUNT " loaded"
           .
