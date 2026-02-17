      *> ============================================================
      *> sprites-data.cpy — Data for sprite rendering system
      *> Active sprite table, sprite patch buffer, rendering temps.
      *> All variables prefixed WS-SP- to avoid collisions.
      *> ============================================================

      *> --- Active sprite table: up to 50 sprites ---
       01 WS-SP-COUNT             PIC 9(3) VALUE 0.
       01 WS-SP-TABLE.
          05 WS-SP-ENTRY OCCURS 50 TIMES.
             10 WS-SP-WORLD-X     PIC S9(5)V9(4).
             10 WS-SP-WORLD-Y     PIC S9(5)V9(4).
             10 WS-SP-THING-TYPE  PIC 9(5).
             10 WS-SP-STATE       PIC 9.
      *>       0=alive, 1=dying, 2=dead
             10 WS-SP-HEALTH      PIC 9(3).
             10 WS-SP-FRAME-TMR   PIC 9(3).
             10 WS-SP-LUMP-NAME   PIC X(8).

      *> --- Sort order array (indices sorted by distance) ---
       01 WS-SP-ORDER.
          05 WS-SP-ORD-IDX        PIC 9(3)
                                   OCCURS 50 TIMES.
       01 WS-SP-DISTANCES.
          05 WS-SP-DIST-VAL       PIC S9(7)V9(4)
                                   OCCURS 50 TIMES.

      *> --- Sprite patch buffer (64 KB for one sprite lump) ---
       01 WS-SP-PATCH-BUF         PIC X(65536).

      *> --- Sprite patch header fields ---
       01 WS-SP-PATCH-W           PIC S9(5).
       01 WS-SP-PATCH-H           PIC S9(5).
       01 WS-SP-PATCH-LEFT        PIC S9(5).
       01 WS-SP-PATCH-TOP         PIC S9(5).

      *> --- View-space projection variables ---
       01 WS-SP-VIEW-DX           PIC S9(3)V9(6).
       01 WS-SP-VIEW-DY           PIC S9(3)V9(6).
       01 WS-SP-STRAFE-DX         PIC S9(3)V9(6).
       01 WS-SP-STRAFE-DY         PIC S9(3)V9(6).
       01 WS-SP-DX                PIC S9(7)V9(4).
       01 WS-SP-DY                PIC S9(7)V9(4).
       01 WS-SP-DEPTH             PIC S9(7)V9(4).
       01 WS-SP-LATERAL           PIC S9(7)V9(4).

      *> --- Screen-space rendering variables ---
       01 WS-SP-SCREEN-X          PIC S9(5).
       01 WS-SP-SCREEN-H          PIC S9(5).
       01 WS-SP-SCREEN-W          PIC S9(5).
       01 WS-SP-DRAW-TOP          PIC S9(5).
       01 WS-SP-DRAW-BOT          PIC S9(5).
       01 WS-SP-DRAW-LEFT         PIC S9(5).
       01 WS-SP-DRAW-RIGHT        PIC S9(5).
       01 WS-SP-SCALE             PIC S9(5)V9(6).

      *> --- Column rendering variables ---
       01 WS-SP-COL               PIC S9(5).
       01 WS-SP-COL-OFF           PIC 9(6).
       01 WS-SP-POST-POS          PIC 9(6).
       01 WS-SP-POST-TOP          PIC 9(3).
       01 WS-SP-POST-LEN          PIC 9(3).
       01 WS-SP-POST-EXIT         PIC 9.
       01 WS-SP-TEX-COL           PIC S9(5).
       01 WS-SP-TEX-ROW           PIC S9(5).
       01 WS-SP-SCREEN-ROW        PIC S9(5).

      *> --- Pixel rendering temps ---
       01 WS-SP-PAL-IDX           PIC 9(3).
       01 WS-SP-LIT-IDX           PIC 9(3).
       01 WS-SP-LIGHT             PIC S9(5).
       01 WS-SP-CMAP-TBL          PIC 9(3).
       01 WS-SP-FB-IDX            PIC 9(6).

      *> --- V coordinate mapping ---
       01 WS-SP-V-FRAC            PIC S9(7)V9(6).
       01 WS-SP-V-INT             PIC S9(5).
       01 WS-SP-PIX-ROW           PIC S9(5).

      *> --- Loop indices ---
       01 WS-SP-I                 PIC 9(3).
       01 WS-SP-J                 PIC 9(3).
       01 WS-SP-CUR               PIC 9(3).

      *> --- Sort temps ---
       01 WS-SP-SORT-KEY          PIC S9(7)V9(4).
       01 WS-SP-SORT-IDX          PIC 9(3).
       01 WS-SP-SORT-J            PIC 9(3).

      *> --- Thing type to sprite name mapping ---
       01 WS-SP-MAP-COUNT         PIC 9(3) VALUE 5.
       01 WS-SP-TYPE-MAP.
          05 FILLER PIC X(13) VALUE "03004POSS0020".
          05 FILLER PIC X(13) VALUE "03001TROO0060".
          05 FILLER PIC X(13) VALUE "03002SARG0150".
          05 FILLER PIC X(13) VALUE "00009SPOS0030".
          05 FILLER PIC X(13) VALUE "00065CPOS0070".
       01 WS-SP-TYPE-MAP-R REDEFINES WS-SP-TYPE-MAP.
          05 WS-SP-TM-ENTRY OCCURS 5 TIMES.
             10 WS-SP-TM-TYPE     PIC 9(5).
             10 WS-SP-TM-BASE     PIC X(4).
             10 WS-SP-TM-HEALTH   PIC 9(3).
             10 WS-SP-TM-PAD      PIC X.

      *> --- Grid conversion temp ---
       01 WS-SP-GRID-X            PIC S9(5)V9(4).
       01 WS-SP-GRID-Y            PIC S9(5)V9(4).

      *> --- Sprite name build temp ---
       01 WS-SP-BASE-NAME         PIC X(4).

      *> --- Trig lookup temps ---
       01 WS-SP-TRIG-IDX          PIC 9(5).

      *> --- Inv depth for column scaling ---
       01 WS-SP-INV-DEPTH         PIC S9(5)V9(6).

      *> --- Half screen height constant ---
       01 WS-SP-HALF-H            PIC S9(5) VALUE 100.
