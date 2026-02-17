       IDENTIFICATION DIVISION.
       PROGRAM-ID. DOOM2-COBOL.
      *> ============================================================
      *> COBOL DOOM 2 — Software renderer with SDL2 display
      *> 320x200 RGBA framebuffer, all rendering in COBOL
      *> ============================================================

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *> --- Screen dimensions ---
       01 WS-SCREEN-W          PIC 9(4) COMP-5 VALUE 320.
       01 WS-SCREEN-H          PIC 9(4) COMP-5 VALUE 200.

      *> --- Framebuffer: 320 x 200 x 4 = 256000 bytes RGBA ---
       01 WS-FRAMEBUFFER       PIC X(256000).
       01 WS-FB REDEFINES WS-FRAMEBUFFER.
          05 WS-FB-BYTE         PIC X OCCURS 256000 TIMES.

      *> --- Input keys array (10 x 4-byte ints) ---
       01 WS-KEYS.
          05 WS-KEY             PIC S9(9) COMP-5
                                OCCURS 10 TIMES.

      *> --- Game state ---
       01 WS-RUNNING           PIC 9 VALUE 1.

      *> --- Rendering temp vars ---
       01 WS-PIX-X             PIC 9(4).
       01 WS-PIX-Y             PIC 9(4).
       01 WS-PIX-IDX           PIC 9(6).
       01 WS-COLOR-IDX         PIC 9(3).
       01 WS-CELL-X            PIC 9(3).
       01 WS-CELL-Y            PIC 9(3).

      *> --- WAD data structures ---
           COPY "wad-data.cpy".

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
      *>   Initialize SDL window
           CALL "sdl_init" USING WS-SCREEN-W WS-SCREEN-H

      *>   Load WAD data
           PERFORM OPEN-WAD
           PERFORM READ-WAD-DIRECTORY
           PERFORM LOAD-PALETTE
           PERFORM LOAD-COLORMAP

      *>   Render palette test pattern
           PERFORM RENDER-PALETTE-TEST

      *>   Main loop — display + handle input
           PERFORM UNTIL WS-RUNNING = 0
               CALL "sdl_frame" USING WS-FRAMEBUFFER
                                      WS-SCREEN-W
                                      WS-SCREEN-H
               CALL "sdl_input" USING WS-KEYS
               IF WS-KEY(1) = -1
                   MOVE 0 TO WS-RUNNING
               END-IF
           END-PERFORM

      *>   Cleanup
           CALL "wad_close"
           CALL "sdl_quit"
           STOP RUN.

      *> ============================================================
      *> RENDER-PALETTE-TEST: 16x16 grid of all 256 palette colors
      *> Each cell: 20px wide x 12px tall = 320x192 pixels
      *> ============================================================
       RENDER-PALETTE-TEST.
           MOVE LOW-VALUES TO WS-FRAMEBUFFER
           PERFORM VARYING WS-PIX-Y FROM 0 BY 1
               UNTIL WS-PIX-Y >= 192
               PERFORM VARYING WS-PIX-X FROM 0 BY 1
                   UNTIL WS-PIX-X >= 320
      *>           Which palette cell (0-15, 0-15)
                   COMPUTE WS-CELL-X = WS-PIX-X / 20
                   COMPUTE WS-CELL-Y = WS-PIX-Y / 12
      *>           Palette index (1-based)
                   COMPUTE WS-COLOR-IDX =
                       WS-CELL-Y * 16 + WS-CELL-X + 1
      *>           Framebuffer byte offset (1-based)
                   COMPUTE WS-PIX-IDX =
                       (WS-PIX-Y * 320 + WS-PIX-X) * 4 + 1
      *>           Write RGBA
                   MOVE WS-PAL-R(WS-COLOR-IDX)
                       TO WS-FB-BYTE(WS-PIX-IDX)
                   ADD 1 TO WS-PIX-IDX
                   MOVE WS-PAL-G(WS-COLOR-IDX)
                       TO WS-FB-BYTE(WS-PIX-IDX)
                   ADD 1 TO WS-PIX-IDX
                   MOVE WS-PAL-B(WS-COLOR-IDX)
                       TO WS-FB-BYTE(WS-PIX-IDX)
                   ADD 1 TO WS-PIX-IDX
                   MOVE X"FF"
                       TO WS-FB-BYTE(WS-PIX-IDX)
               END-PERFORM
           END-PERFORM
           .

      *> ============================================================
      *> WAD parser procedures
      *> ============================================================
           COPY "wad-parser.cpy".
