      *> ============================================================
      *> raycaster-data.cpy — Data for textured wall raycaster
      *> Player state, trig tables, depth buffer, working vars.
      *> Map grid comes from map-loader-data.cpy (MC-TYPE/MC-TEX-ID).
      *> ============================================================

      *> --- Player state ---
       01 WS-PLAYER.
          05 WS-PX             PIC S9(5)V9(4).
          05 WS-PY             PIC S9(5)V9(4).
          05 WS-PA             PIC S9(5).
          05 WS-HEALTH         PIC 9(3).
          05 WS-AMMO           PIC 9(3).

      *> --- Trig tables: 3600 entries for 0.1-degree precision ---
       01 WS-SIN-TABLE.
          05 WS-SIN-VAL        PIC S9(1)V9(6)
                               OCCURS 3600 TIMES.
       01 WS-COS-TABLE.
          05 WS-COS-VAL        PIC S9(1)V9(6)
                               OCCURS 3600 TIMES.
       01 WS-TRIG-IDX          PIC 9(5).
       01 WS-TRIG-RADIANS      PIC S9(3)V9(8).

      *> --- Hardcoded 16x16 test map (fallback if WAD load fails) ---
      *> Texture IDs: 001-005 = wall textures, 000 = empty
       01 WS-MAP-INIT-DATA.
          05 FILLER PIC X(48) VALUE
             "001001001001001001001001001001001001001001001001".
          05 FILLER PIC X(48) VALUE
             "001000000000000002000000000000000000000000000001".
          05 FILLER PIC X(48) VALUE
             "001000003000003002000000003000003000000000000001".
          05 FILLER PIC X(48) VALUE
             "001000003000000000000000000000003000000000000001".
          05 FILLER PIC X(48) VALUE
             "001000003000000002000000000000000000000000000001".
          05 FILLER PIC X(48) VALUE
             "002002002000000002002002002002000000004004004001".
          05 FILLER PIC X(48) VALUE
             "001000000000000000000000000002000000000000000001".
          05 FILLER PIC X(48) VALUE
             "001000005005000005000005005002000000000000000001".
          05 FILLER PIC X(48) VALUE
             "001000000000000005000000000000000000000000000001".
          05 FILLER PIC X(48) VALUE
             "001000005005000005000000003003000000003003000001".
          05 FILLER PIC X(48) VALUE
             "001000000000000000000000000000000000000000000001".
          05 FILLER PIC X(48) VALUE
             "001000003000003003000003000000004000004000000001".
          05 FILLER PIC X(48) VALUE
             "001000003000000000000003000000000000000000000001".
          05 FILLER PIC X(48) VALUE
             "001000000000003000000003000000004000004000000001".
          05 FILLER PIC X(48) VALUE
             "001000000000000000000000000000000000000000000001".
          05 FILLER PIC X(48) VALUE
             "001001001001001001001001001001001001001001001001".

       01 WS-MAP-INIT-R REDEFINES WS-MAP-INIT-DATA.
          05 WS-MIDR-ROW OCCURS 16 TIMES.
             10 WS-MIDR-CELL    PIC 9(3) OCCURS 16 TIMES.

      *> --- Depth buffer: one entry per screen column ---
       01 WS-DEPTH-BUF.
          05 WS-DEPTH-VAL       PIC S9(5)V9(4)
                                OCCURS 320 TIMES.

      *> --- Constants ---
       01 WS-PI                 PIC 9V9(8) VALUE 3.14159265.
       01 WS-FOV                PIC 9(3)   VALUE 60.
       01 WS-HALF-FOV           PIC 9(3)   VALUE 30.
       01 WS-MOVE-SPEED         PIC 9V9(4) VALUE 0.1500.
       01 WS-TURN-SPEED         PIC 9(3)   VALUE 5.
       01 WS-MAX-STEPS          PIC 9(3)   VALUE 256.
       01 WS-COLLISION-PAD      PIC 9V9(4) VALUE 0.2000.

      *> --- Ray casting working variables ---
       01 WS-CUR-COL            PIC 9(4).
       01 WS-RAY-ANGLE          PIC S9(7).
       01 WS-RAY-ANGLE-F        PIC S9(5)V9(4).
       01 WS-RAY-DIR-X          PIC S9(3)V9(6).
       01 WS-RAY-DIR-Y          PIC S9(3)V9(6).
       01 WS-MAP-X              PIC S9(5).
       01 WS-MAP-Y              PIC S9(5).
       01 WS-STEP-X             PIC S9(1).
       01 WS-STEP-Y             PIC S9(1).
       01 WS-SIDE-DIST-X        PIC S9(5)V9(6).
       01 WS-SIDE-DIST-Y        PIC S9(5)V9(6).
       01 WS-DELTA-DIST-X       PIC S9(5)V9(6).
       01 WS-DELTA-DIST-Y       PIC S9(5)V9(6).
       01 WS-HIT                PIC 9.
       01 WS-SIDE               PIC 9.
       01 WS-PERP-DIST          PIC S9(5)V9(6).
       01 WS-STEP-COUNT         PIC 9(3).
       01 WS-HIT-TEX-ID         PIC 9(3).
       01 WS-ANGLE-LOOKUP       PIC 9(5).

      *> --- Wall rendering working variables ---
       01 WS-WALL-H             PIC S9(5).
       01 WS-WALL-TOP           PIC S9(5).
       01 WS-WALL-BOT           PIC S9(5).
       01 WS-WALL-HIT-POS       PIC S9(5)V9(6).
       01 WS-TEX-U              PIC 9(5).
       01 WS-TEX-V              PIC 9(5).
       01 WS-ROW                PIC S9(5).

      *> --- Lighting ---
       01 WS-LIGHT-LEVEL        PIC S9(5).
       01 WS-LIGHT-TABLE        PIC 9(3).

      *> --- Palette / pixel working vars ---
       01 WS-PAL-IDX            PIC 9(3).
       01 WS-LIT-PAL-IDX        PIC 9(3).
       01 WS-PIX-IDX            PIC 9(6).
       01 WS-PIX-R              PIC X.
       01 WS-PIX-G              PIC X.
       01 WS-PIX-B              PIC X.

      *> --- Movement working vars ---
       01 WS-NEW-X              PIC S9(5)V9(4).
       01 WS-NEW-Y              PIC S9(5)V9(4).
       01 WS-CHK-X              PIC S9(5).
       01 WS-CHK-Y              PIC S9(5).

      *> --- General loop vars ---
       01 WS-I                  PIC 9(5).
       01 WS-J                  PIC 9(5).
       01 WS-FB-I               PIC 9(6).

      *> --- Temp computation vars ---
       01 WS-TEMP-A             PIC S9(7)V9(6).
       01 WS-TEMP-B             PIC S9(7)V9(6).
       01 WS-WALL-H-FULL        PIC S9(7)V9(4).
       01 WS-HALF-H             PIC S9(5).
       01 WS-V-FRAC             PIC S9(5)V9(6).
