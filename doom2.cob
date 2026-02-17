       IDENTIFICATION DIVISION.
       PROGRAM-ID. DOOM2-COBOL.
      *> ============================================================
      *> COBOL DOOM 2 — Software renderer with SDL2 display
      *> 320x200 RGBA framebuffer, all rendering in COBOL
      *> Textured walls, floor/ceiling, MAP01 from Freedoom WAD
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
       01 WS-GAME-STATE        PIC 9 VALUE 0.
      *>   0=loading, 1=playing, 2=won, 3=dead
       01 WS-USE-WAD-MAP       PIC 9 VALUE 0.

      *> --- WAD data structures ---
           COPY "wad-data.cpy".

      *> --- Map grid and loader data ---
           COPY "map-loader-data.cpy".

      *> --- Raycaster data (player, trig, depth buffer) ---
           COPY "raycaster-data.cpy".

      *> --- Floor/ceiling data ---
           COPY "floor-ceil-data.cpy".

      *> --- Sprite data ---
           COPY "sprites-data.cpy".

      *> --- Doors and pickups data ---
           COPY "doors-data.cpy".

      *> --- Enemy AI data ---
           COPY "enemies-data.cpy".

      *> --- Weapons data ---
           COPY "weapons-data.cpy".

      *> --- HUD data ---
           COPY "hud-data.cpy".

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
      *>   Initialize SDL window
           CALL "sdl_init" USING WS-SCREEN-W WS-SCREEN-H

      *>   Initialize trig tables
           PERFORM INIT-TRIG

      *>   Load WAD data
           PERFORM OPEN-WAD
           PERFORM READ-WAD-DIRECTORY
           PERFORM LOAD-PALETTE
           PERFORM LOAD-COLORMAP
           PERFORM LOAD-PNAMES
           PERFORM LOAD-TEXTURE-DEFS
           PERFORM LOAD-FLATS
           DISPLAY "WAD loading complete"

      *>   Try to load MAP01, fall back to hardcoded map
           PERFORM LOAD-MAP
           IF WS-USE-WAD-MAP = 1
               DISPLAY "Using MAP01 from WAD"
           ELSE
               DISPLAY "Using hardcoded test map"
               PERFORM INIT-MAP
           END-IF

      *>   Initialize player
           PERFORM INIT-PLAYER

      *>   Initialize sprites (enemies from THINGS)
           PERFORM INIT-SPRITES

      *>   Initialize enemy AI (after sprites loaded)
           PERFORM EN-INIT-AI

      *>   Initialize doors and pickups
           PERFORM INIT-DOORS
           PERFORM INIT-PICKUPS

      *>   Initialize weapons (load pistol sprite)
           PERFORM INIT-WEAPONS

      *>   Show title screen (wait for SPACE)
           PERFORM SHOW-TITLE-SCREEN

      *>   Game state = playing
           MOVE 1 TO WS-GAME-STATE

      *>   === MAIN GAME LOOP ===
           PERFORM UNTIL WS-RUNNING = 0

      *>       Clear framebuffer
               PERFORM CLEAR-FRAMEBUFFER

      *>       Render floor and ceiling first
               PERFORM RENDER-FLOOR-CEILING

      *>       Cast rays and render textured walls (overwrites)
               PERFORM CAST-ALL-RAYS

      *>       Render sprites (depth-sorted, after walls)
               PERFORM RENDER-ALL-SPRITES

      *>       Render weapon and crosshair (over 3D, under HUD)
               PERFORM RENDER-WEAPON
               PERFORM RENDER-CROSSHAIR

      *>       Render HUD (status bar, on top of everything)
               PERFORM RENDER-HUD

      *>       Display frame via SDL
               CALL "sdl_frame" USING WS-FRAMEBUFFER
                                      WS-SCREEN-W
                                      WS-SCREEN-H

      *>       Get input
               CALL "sdl_input" USING WS-KEYS

      *>       Check quit
               IF WS-KEY(1) = -1
                   MOVE 0 TO WS-RUNNING
               END-IF

      *>       Process movement
               PERFORM PROCESS-MOVEMENT

      *>       Enemy AI and combat
               PERFORM UPDATE-ENEMIES
               PERFORM PROCESS-FIRE

      *>       Door/pickup mechanics
               PERFORM CHECK-USE-KEY
               PERFORM UPDATE-DOORS
               PERFORM CHECK-PICKUPS

      *>       Check for level exit
               PERFORM CHECK-LEVEL-EXIT

           END-PERFORM

      *>   Cleanup
           CALL "wad_close"
           CALL "sdl_quit"
           STOP RUN.

      *> ============================================================
      *> WAD parser procedures
      *> ============================================================
           COPY "wad-parser.cpy".

      *> ============================================================
      *> WAD texture/flat loading procedures
      *> ============================================================
           COPY "wad-textures.cpy".

      *> ============================================================
      *> Map loader procedures
      *> ============================================================
           COPY "map-loader-proc.cpy".

      *> ============================================================
      *> Raycaster procedures
      *> ============================================================
           COPY "raycaster-proc.cpy".

      *> ============================================================
      *> Floor/ceiling procedures
      *> ============================================================
           COPY "floor-ceil-proc.cpy".

      *> ============================================================
      *> Sprite rendering procedures
      *> ============================================================
           COPY "sprites-proc.cpy".

      *> ============================================================
      *> Enemy AI procedures
      *> ============================================================
           COPY "enemies-proc.cpy".

      *> ============================================================
      *> Weapons procedures
      *> ============================================================
           COPY "weapons-proc.cpy".

      *> ============================================================
      *> Door and pickup procedures
      *> ============================================================
           COPY "doors-proc.cpy".

      *> ============================================================
      *> HUD procedures
      *> ============================================================
           COPY "hud-proc.cpy".
