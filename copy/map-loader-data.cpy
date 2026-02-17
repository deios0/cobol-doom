      *> ============================================================
      *> map-loader-data.cpy — Data for MAP01 loader
      *> 128x128 grid map + raw WAD data buffers
      *> ============================================================

      *> --- Map grid: 128x128 cells ---
       01 WS-MAP-SIZE           PIC 9(3) VALUE 128.
       01 WS-MAP-GRID.
          05 WS-MAP-ROW OCCURS 128 TIMES.
             10 WS-MAP-CELL OCCURS 128 TIMES.
                15 MC-TYPE      PIC 9.
      *>          0=empty, 1=wall, 2=door
                15 MC-TEX-ID    PIC 9(3).
                15 MC-FLOOR-H   PIC S9(3).
                15 MC-CEIL-H    PIC S9(3).
                15 MC-FLOOR-TEX PIC 9(3).
                15 MC-CEIL-TEX  PIC 9(3).
                15 MC-LIGHT     PIC 9(3).

      *> --- Things table (enemies, pickups, player start) ---
       01 WS-MAP-THING-COUNT   PIC 9(3).
       01 WS-MAP-THINGS.
          05 WS-MAP-THING OCCURS 200 TIMES.
             10 MT-X            PIC S9(5).
             10 MT-Y            PIC S9(5).
             10 MT-ANGLE        PIC S9(5).
             10 MT-TYPE         PIC 9(5).
             10 MT-FLAGS        PIC 9(5).

      *> --- Player spawn from THINGS ---
       01 WS-SPAWN-X           PIC S9(5)V9(4).
       01 WS-SPAWN-Y           PIC S9(5)V9(4).
       01 WS-SPAWN-ANGLE       PIC S9(5).

      *> --- Raw WAD map data buffers ---
      *> VERTEXES: max 2000 verts x 4 bytes = 8000 bytes
       01 WS-VERT-COUNT        PIC 9(5).
       01 WS-VERT-BUF          PIC X(8000).

      *> LINEDEFS: max 1500 lines x 14 bytes = 21000 bytes
       01 WS-LINE-COUNT        PIC 9(5).
       01 WS-LINE-BUF          PIC X(21000).

      *> SIDEDEFS: max 2000 sides x 30 bytes = 60000 bytes
       01 WS-SIDE-COUNT        PIC 9(5).
       01 WS-SIDE-BUF          PIC X(60000).

      *> SECTORS: max 200 sectors x 26 bytes = 5200 bytes
       01 WS-SECT-COUNT        PIC 9(5).
       01 WS-SECT-BUF          PIC X(5200).

      *> THINGS: max 300 things x 10 bytes = 3000 bytes
       01 WS-THING-RAW-COUNT   PIC 9(5).
       01 WS-THING-BUF         PIC X(3000).

      *> --- Map lump index (position of MAP01 in directory) ---
       01 WS-MAP-LUMP-IDX      PIC 9(5).

      *> --- Bounding box for vertex coordinates ---
       01 WS-MAP-MIN-X         PIC S9(5).
       01 WS-MAP-MAX-X         PIC S9(5).
       01 WS-MAP-MIN-Y         PIC S9(5).
       01 WS-MAP-MAX-Y         PIC S9(5).
       01 WS-MAP-RANGE-X       PIC 9(5).
       01 WS-MAP-RANGE-Y       PIC 9(5).
       01 WS-MAP-SCALE         PIC 9(5).

      *> --- Extracted vertex coordinates ---
       01 WS-VTX-TABLE.
          05 WS-VTX OCCURS 2000 TIMES.
             10 WS-VTX-X       PIC S9(5).
             10 WS-VTX-Y       PIC S9(5).

      *> --- Temp variables for map loading ---
       01 WS-ML-I              PIC 9(5).
       01 WS-ML-J              PIC 9(5).
       01 WS-ML-OFF            PIC 9(6).
       01 WS-ML-SIZE           PIC 9(6).

      *> --- Linedef fields (extracted) ---
       01 WS-LD-V1             PIC 9(5).
       01 WS-LD-V2             PIC 9(5).
       01 WS-LD-FLAGS          PIC 9(5).
       01 WS-LD-SPECIAL        PIC 9(5).
       01 WS-LD-TAG            PIC 9(5).
       01 WS-LD-RSDEF          PIC 9(5).
       01 WS-LD-LSDEF          PIC 9(5).

      *> --- Sidedef fields (extracted) ---
       01 WS-SD-XOFF           PIC S9(5).
       01 WS-SD-YOFF           PIC S9(5).
       01 WS-SD-UPPER          PIC X(8).
       01 WS-SD-LOWER          PIC X(8).
       01 WS-SD-MIDDLE         PIC X(8).
       01 WS-SD-SECTOR         PIC 9(5).

      *> --- Sector fields (extracted) ---
       01 WS-SC-FLOOR-H        PIC S9(5).
       01 WS-SC-CEIL-H         PIC S9(5).
       01 WS-SC-FLOOR-TEX      PIC X(8).
       01 WS-SC-CEIL-TEX       PIC X(8).
       01 WS-SC-LIGHT          PIC 9(5).
       01 WS-SC-SPECIAL        PIC 9(5).
       01 WS-SC-TAG            PIC 9(5).

      *> --- Bresenham line rasterization variables ---
       01 WS-BH-X0             PIC S9(5).
       01 WS-BH-Y0             PIC S9(5).
       01 WS-BH-X1             PIC S9(5).
       01 WS-BH-Y1             PIC S9(5).
       01 WS-BH-DX             PIC S9(5).
       01 WS-BH-DY             PIC S9(5).
       01 WS-BH-SX             PIC S9(3).
       01 WS-BH-SY             PIC S9(3).
       01 WS-BH-ERR            PIC S9(5).
       01 WS-BH-E2             PIC S9(5).
       01 WS-BH-CUR-X          PIC S9(5).
       01 WS-BH-CUR-Y          PIC S9(5).
       01 WS-BH-DONE           PIC 9.

      *> --- Grid cell being written ---
       01 WS-GR-ROW            PIC S9(5).
       01 WS-GR-COL            PIC S9(5).
       01 WS-GR-TYPE           PIC 9.
       01 WS-GR-TEX            PIC 9(3).
       01 WS-GR-FLOOR-H        PIC S9(3).
       01 WS-GR-CEIL-H         PIC S9(3).
       01 WS-GR-FLOOR-T        PIC 9(3).
       01 WS-GR-CEIL-T         PIC 9(3).
       01 WS-GR-LIGHT          PIC 9(3).

      *> --- Texture/flat name search temp ---
       01 WS-SEARCH-NAME       PIC X(8).
       01 WS-SEARCH-RESULT     PIC 9(3).

      *> --- Unsigned 16-bit helper ---
       01 WS-ML-UINT16         PIC 9(5).

      *> --- Vertex coordinates for current line ---
       01 WS-LN-X1             PIC S9(5).
       01 WS-LN-Y1             PIC S9(5).
       01 WS-LN-X2             PIC S9(5).
       01 WS-LN-Y2             PIC S9(5).

      *> --- Two-sided flag ---
       01 WS-LD-TWOSIDED       PIC 9.

      *> --- Default sector properties for grid init ---
       01 WS-DEF-FLOOR-H       PIC S9(3) VALUE 0.
       01 WS-DEF-CEIL-H        PIC S9(3) VALUE 128.
       01 WS-DEF-FLOOR-T       PIC 9(3) VALUE 0.
       01 WS-DEF-CEIL-T        PIC 9(3) VALUE 0.
       01 WS-DEF-LIGHT         PIC 9(3) VALUE 160.
