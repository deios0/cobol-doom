      *> ============================================================
      *> WAD parser procedures
      *> ============================================================

       OPEN-WAD.
           STRING "freedoom2.wad" X"00" DELIMITED BY SIZE
               INTO WS-WAD-FILENAME
           CALL "wad_open" USING WS-WAD-FILENAME
               RETURNING WS-WAD-RC
           IF WS-WAD-RC NOT = 0
               DISPLAY "ERROR: Cannot open freedoom2.wad"
               CALL "sdl_quit"
               STOP RUN
           END-IF
      *>   Read 12-byte header
           MOVE 0 TO WS-WAD-OFFSET
           MOVE 12 TO WS-WAD-RD-SIZE
           CALL "wad_read" USING WS-WAD-OFFSET
                                 WS-WAD-RD-SIZE
                                 WS-WAD-HDR-BUF
           DISPLAY "WAD: " WH-ID
               " lumps=" WH-NUM-LUMPS
               " dir@" WH-DIR-OFFSET
           .

       READ-WAD-DIRECTORY.
           MOVE WH-NUM-LUMPS TO WS-WAD-LUMP-COUNT
           IF WS-WAD-LUMP-COUNT > 4000
               MOVE 4000 TO WS-WAD-LUMP-COUNT
           END-IF
      *>   Read all 16-byte directory entries at once
           MOVE WH-DIR-OFFSET TO WS-WAD-OFFSET
           COMPUTE WS-WAD-RD-SIZE =
               WS-WAD-LUMP-COUNT * 16
           CALL "wad_read" USING WS-WAD-OFFSET
                                 WS-WAD-RD-SIZE
                                 WS-WAD-LUMP-TABLE
      *>   Clean lump names: NUL -> space for comparison
           PERFORM VARYING WS-WAD-I FROM 1 BY 1
               UNTIL WS-WAD-I > WS-WAD-LUMP-COUNT
               INSPECT WL-NAME(WS-WAD-I)
                   REPLACING ALL X"00" BY SPACE
           END-PERFORM
           DISPLAY "Directory: " WS-WAD-LUMP-COUNT " lumps"
           DISPLAY "First lump: [" WL-NAME(1) "]"
           .

       FIND-LUMP.
      *>   Input:  WS-FIND-NAME (8 chars, space-padded)
      *>   Output: WS-FOUND, WS-FOUND-OFFSET, WS-FOUND-SIZE
           MOVE 0 TO WS-FOUND
           PERFORM VARYING WS-WAD-I FROM 1 BY 1
               UNTIL WS-WAD-I > WS-WAD-LUMP-COUNT
               OR WS-FOUND = 1
               IF WL-NAME(WS-WAD-I) = WS-FIND-NAME
                   MOVE 1 TO WS-FOUND
                   MOVE WL-OFFSET(WS-WAD-I)
                       TO WS-FOUND-OFFSET
                   MOVE WL-SIZE(WS-WAD-I)
                       TO WS-FOUND-SIZE
                   MOVE WS-WAD-I TO WS-FIND-IDX
               END-IF
           END-PERFORM
           .

       LOAD-PALETTE.
           MOVE "PLAYPAL " TO WS-FIND-NAME
           PERFORM FIND-LUMP
           IF WS-FOUND = 1
               MOVE WS-FOUND-OFFSET TO WS-WAD-OFFSET
               MOVE 768 TO WS-WAD-RD-SIZE
               CALL "wad_read" USING WS-WAD-OFFSET
                                     WS-WAD-RD-SIZE
                                     WS-PALETTE-RAW
               DISPLAY "Palette loaded (" WS-FOUND-SIZE
                   " bytes)"
           ELSE
               DISPLAY "ERROR: PLAYPAL not found!"
           END-IF
           .

       LOAD-COLORMAP.
           MOVE "COLORMAP" TO WS-FIND-NAME
           PERFORM FIND-LUMP
           IF WS-FOUND = 1
               MOVE WS-FOUND-OFFSET TO WS-WAD-OFFSET
               MOVE 8704 TO WS-WAD-RD-SIZE
               CALL "wad_read" USING WS-WAD-OFFSET
                                     WS-WAD-RD-SIZE
                                     WS-COLORMAP-RAW
               DISPLAY "Colormap loaded (" WS-FOUND-SIZE
                   " bytes)"
           ELSE
               DISPLAY "ERROR: COLORMAP not found!"
           END-IF
           .
