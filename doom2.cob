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
       01 WS-TEX-U             PIC 9(3).
       01 WS-TEX-V             PIC 9(3).

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

      *>   Load textures and flats
           PERFORM LOAD-PNAMES
           PERFORM LOAD-TEXTURE-DEFS
           PERFORM LOAD-FLATS

      *>   Render first texture as test
           PERFORM RENDER-TEXTURE-TEST

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
      *> RENDER-TEXTURE-TEST: display first loaded wall texture
      *> scaled to fill the screen (320x200)
      *> ============================================================
       RENDER-TEXTURE-TEST.
           IF WS-TEX-COUNT = 0
               DISPLAY "No textures to display"
               EXIT PARAGRAPH
           END-IF
           DISPLAY "Showing texture: " WT-NAME(1)
               " (" WT-WIDTH(1) "x" WT-HEIGHT(1) ")"
           MOVE LOW-VALUES TO WS-FRAMEBUFFER
           PERFORM VARYING WS-PIX-Y FROM 0 BY 1
               UNTIL WS-PIX-Y >= 200
               PERFORM VARYING WS-PIX-X FROM 0 BY 1
                   UNTIL WS-PIX-X >= 320
      *>           Texture coordinates
                   COMPUTE WS-TEX-U =
                       WS-PIX-X * WT-WIDTH(1) / 320
                   COMPUTE WS-TEX-V =
                       WS-PIX-Y * WT-HEIGHT(1) / 200
      *>           Column-major pixel offset
                   COMPUTE WS-TEX-PIX-OFF =
                       WS-TEX-U * WT-HEIGHT(1)
                       + WS-TEX-V + 1
                   IF WS-TEX-PIX-OFF < 1
                       OR WS-TEX-PIX-OFF > 16384
                       MOVE 1 TO WS-TEX-PIX-OFF
                   END-IF
      *>           Get palette index from texture
                   MOVE WT-PIX(1, WS-TEX-PIX-OFF)
                       TO WS-BIN-BUF1
                   MOVE WS-BIN-BYTE TO WS-COLOR-IDX
      *>           Convert to 1-based palette index
                   ADD 1 TO WS-COLOR-IDX
                   IF WS-COLOR-IDX > 256
                       MOVE 1 TO WS-COLOR-IDX
                   END-IF
      *>           Framebuffer offset
                   COMPUTE WS-PIX-IDX =
                       (WS-PIX-Y * 320 + WS-PIX-X) * 4 + 1
      *>           Write RGBA from palette
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

      *> ============================================================
      *> WAD texture/flat loading procedures
      *> ============================================================
           COPY "wad-textures.cpy".
