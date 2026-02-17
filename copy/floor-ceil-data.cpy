      *> ============================================================
      *> floor-ceil-data.cpy — Data for floor/ceiling renderer
      *> ============================================================

      *> --- Floor/ceiling configuration ---
       01 WS-FC-FLOOR-FLAT      PIC 9(3) VALUE 001.
       01 WS-FC-CEIL-FLAT       PIC 9(3) VALUE 002.
       01 WS-FC-FOV             PIC 9(3) VALUE 060.
       01 WS-FC-HALF-FOV        PIC 9(3) VALUE 030.

      *> --- Screen geometry ---
       01 WS-FC-HALF-H          PIC 9(3) VALUE 100.

      *> --- Ray direction vectors for left/right screen edges ---
       01 WS-FC-RDIR-LX         PIC S9(3)V9(6).
       01 WS-FC-RDIR-LY         PIC S9(3)V9(6).
       01 WS-FC-RDIR-RX         PIC S9(3)V9(6).
       01 WS-FC-RDIR-RY         PIC S9(3)V9(6).

      *> --- Per-scanline computation vars ---
       01 WS-FC-ROW             PIC S9(5).
       01 WS-FC-COL             PIC 9(5).
       01 WS-FC-ROW-DIST        PIC S9(5)V9(6).
       01 WS-FC-DENOM           PIC S9(5).

      *> --- Floor stepping variables ---
       01 WS-FC-STEP-X          PIC S9(5)V9(6).
       01 WS-FC-STEP-Y          PIC S9(5)V9(6).
       01 WS-FC-FLOOR-X         PIC S9(9)V9(6).
       01 WS-FC-FLOOR-Y         PIC S9(9)V9(6).

      *> --- Texture coordinate temps ---
       01 WS-FC-TEX-X           PIC S9(5).
       01 WS-FC-TEX-Y           PIC S9(5).
       01 WS-FC-FLAT-OFF        PIC 9(5).
       01 WS-FC-SCALED-X        PIC S9(9)V9(6).
       01 WS-FC-SCALED-Y        PIC S9(9)V9(6).

      *> --- Palette / colormap temps ---
       01 WS-FC-PAL-IDX         PIC 9(3).
       01 WS-FC-LIT-IDX         PIC 9(3).
       01 WS-FC-LIGHT           PIC 9(3).
       01 WS-FC-CMAP-TBL        PIC 9(3).

      *> --- Framebuffer write position ---
       01 WS-FC-FB-IDX          PIC 9(6).

      *> --- Angle computation temps ---
       01 WS-FC-ANG-L           PIC S9(5).
       01 WS-FC-ANG-R           PIC S9(5).
       01 WS-FC-TRIG-IDX        PIC 9(5).

      *> --- Current flat ID for inner loop ---
       01 WS-FC-CUR-FLAT        PIC 9(3).

      *> --- Ceiling mirror row ---
       01 WS-FC-CEIL-ROW        PIC S9(5).
       01 WS-FC-CEIL-FB-IDX     PIC 9(6).
