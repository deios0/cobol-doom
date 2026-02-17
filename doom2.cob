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

      *> --- Screen constants ---
       01 WS-SCREEN-W           PIC 9(4) COMP-5 VALUE 320.
       01 WS-SCREEN-H           PIC 9(4) COMP-5 VALUE 200.
       01 WS-SCREEN-PIXELS      PIC 9(6) VALUE 64000.

      *> --- Framebuffer: 320 * 200 * 4 = 256000 bytes RGBA ---
       01 WS-FRAMEBUFFER        PIC X(256000).
       01 WS-FB REDEFINES WS-FRAMEBUFFER.
          05 WS-FB-BYTE         PIC X OCCURS 256000 TIMES.

      *> --- Input keys array (10 ints) ---
       01 WS-KEYS.
          05 WS-KEY              PIC S9(9) COMP-5
                                 OCCURS 10 TIMES.

      *> --- Loop control ---
       01 WS-RUNNING            PIC 9 VALUE 1.
       01 WS-PIXEL-IDX          PIC 9(6).
       01 WS-X                  PIC 9(4).
       01 WS-Y                  PIC 9(4).
       01 WS-R                  PIC 9(3).
       01 WS-G                  PIC 9(3).
       01 WS-B                  PIC 9(3).
       01 WS-BYTE-VAL           PIC X.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
      *>   Initialize SDL
           CALL "sdl_init" USING WS-SCREEN-W WS-SCREEN-H

      *>   Fill framebuffer with gradient test pattern
           PERFORM FILL-TEST-PATTERN

      *>   Main loop
           PERFORM UNTIL WS-RUNNING = 0
      *>       Display frame
               CALL "sdl_frame" USING WS-FRAMEBUFFER
                                      WS-SCREEN-W
                                      WS-SCREEN-H
      *>       Get input
               CALL "sdl_input" USING WS-KEYS
      *>       Check quit
               IF WS-KEY(1) = -1
                   MOVE 0 TO WS-RUNNING
               END-IF
           END-PERFORM

      *>   Cleanup
           CALL "sdl_quit"
           STOP RUN.

      *> ============================================================
      *> FILL-TEST-PATTERN: gradient R=x, G=y, B=128
      *> ============================================================
       FILL-TEST-PATTERN.
           MOVE 1 TO WS-PIXEL-IDX
           PERFORM VARYING WS-Y FROM 0 BY 1
                   UNTIL WS-Y >= 200
               PERFORM VARYING WS-X FROM 0 BY 1
                       UNTIL WS-X >= 320
      *>           R = x mod 256
                   DIVIDE WS-X BY 256
                       GIVING WS-R REMAINDER WS-R
                   MOVE FUNCTION CHAR(WS-R + 1)
                       TO WS-FB-BYTE(WS-PIXEL-IDX)
                   ADD 1 TO WS-PIXEL-IDX
      *>           G = y mod 256
                   DIVIDE WS-Y BY 256
                       GIVING WS-G REMAINDER WS-G
                   MOVE FUNCTION CHAR(WS-G + 1)
                       TO WS-FB-BYTE(WS-PIXEL-IDX)
                   ADD 1 TO WS-PIXEL-IDX
      *>           B = 128
                   MOVE FUNCTION CHAR(129)
                       TO WS-FB-BYTE(WS-PIXEL-IDX)
                   ADD 1 TO WS-PIXEL-IDX
      *>           A = 255
                   MOVE FUNCTION CHAR(256)
                       TO WS-FB-BYTE(WS-PIXEL-IDX)
                   ADD 1 TO WS-PIXEL-IDX
               END-PERFORM
           END-PERFORM.
