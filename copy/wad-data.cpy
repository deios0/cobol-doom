      *> ============================================================
      *> WAD file data structures
      *> ============================================================

      *> WAD file name (null-terminated for C calls)
       01 WS-WAD-FILENAME      PIC X(256).
       01 WS-WAD-RC            PIC S9(9) COMP-5.

      *> WAD read parameters
       01 WS-WAD-OFFSET        PIC S9(9) COMP-5.
       01 WS-WAD-RD-SIZE       PIC S9(9) COMP-5.

      *> WAD header (12 bytes: 4 ID + 4 num_lumps + 4 dir_offset)
       01 WS-WAD-HDR-BUF       PIC X(12).
       01 WS-WAD-HDR REDEFINES WS-WAD-HDR-BUF.
          05 WH-ID              PIC X(4).
          05 WH-NUM-LUMPS       PIC S9(9) COMP-5.
          05 WH-DIR-OFFSET      PIC S9(9) COMP-5.

      *> Lump directory — max 4000 entries x 16 bytes each
       01 WS-WAD-LUMP-COUNT    PIC 9(5).
       01 WS-WAD-LUMP-TABLE.
          05 WS-WAD-LUMP OCCURS 4000 TIMES.
             10 WL-OFFSET       PIC S9(9) COMP-5.
             10 WL-SIZE          PIC S9(9) COMP-5.
             10 WL-NAME          PIC X(8).

      *> Lump search results
       01 WS-FIND-NAME         PIC X(8).
       01 WS-FIND-IDX          PIC 9(5).
       01 WS-FOUND             PIC 9 VALUE 0.
       01 WS-FOUND-OFFSET      PIC S9(9) COMP-5.
       01 WS-FOUND-SIZE        PIC S9(9) COMP-5.

      *> Palette — PLAYPAL: 256 RGB triplets (768 bytes)
       01 WS-PALETTE-RAW       PIC X(768).
       01 WS-PALETTE REDEFINES WS-PALETTE-RAW.
          05 WS-PAL-ENTRY OCCURS 256 TIMES.
             10 WS-PAL-R        PIC X.
             10 WS-PAL-G        PIC X.
             10 WS-PAL-B        PIC X.

      *> Colormap — 34 light tables x 256 bytes (8704 bytes)
       01 WS-COLORMAP-RAW      PIC X(8704).
       01 WS-COLORMAP REDEFINES WS-COLORMAP-RAW.
          05 WS-CMAP-TABLE OCCURS 34 TIMES.
             10 WS-CMAP-ENTRY   PIC X OCCURS 256 TIMES.

      *> --- Binary data extraction helpers ---
       01 WS-BIN-BUF4          PIC X(4).
       01 WS-BIN-INT32 REDEFINES WS-BIN-BUF4
                                PIC S9(9) COMP-5.
       01 WS-BIN-BUF2          PIC X(2).
       01 WS-BIN-INT16 REDEFINES WS-BIN-BUF2
                                PIC S9(4) COMP-5.
       01 WS-BIN-BUF1          PIC X.
       01 WS-BIN-BYTE REDEFINES WS-BIN-BUF1
                                PIC 9(2) COMP-5.

      *> --- Lump read buffers (64 KB each) ---
       01 WS-LUMP-BUF          PIC X(65536).
       01 WS-PATCH-BUF         PIC X(65536).

      *> --- Patch names (PNAMES) ---
       01 WS-PNAME-COUNT       PIC 9(5).
       01 WS-PNAME-TABLE.
          05 WS-PNAME OCCURS 500 TIMES PIC X(8).

      *> --- Wall textures: palette-indexed, column-major ---
       01 WS-TEX-COUNT         PIC 9(3).
       01 WS-TEXTURES.
          05 WS-TEX OCCURS 200 TIMES.
             10 WT-NAME         PIC X(8).
             10 WT-WIDTH        PIC 9(3).
             10 WT-HEIGHT       PIC 9(3).
             10 WT-PIXEL-DATA.
                15 WT-PIX       PIC X OCCURS 16384 TIMES.

      *> --- Flats (floor/ceiling): 64x64 palette-indexed ---
       01 WS-FLAT-COUNT        PIC 9(3).
       01 WS-FLATS.
          05 WS-FLAT OCCURS 100 TIMES.
             10 WF-NAME         PIC X(8).
             10 WF-PIXEL-DATA.
                15 WF-PIX       PIC X OCCURS 4096 TIMES.

      *> --- Texture loading temp vars ---
       01 WS-TEXDEF-COUNT      PIC 9(5).
       01 WS-TEXDEF-OFFSET     PIC 9(6).
       01 WS-TEX-I             PIC 9(5).
       01 WS-TEX-J             PIC 9(5).
       01 WS-TEX-OFFSET        PIC 9(6).
       01 WS-CUR-TEX           PIC 9(3).
       01 WS-PATCH-W           PIC 9(3).
       01 WS-PATCH-H           PIC 9(3).
       01 WS-PATCH-COL         PIC 9(3).
       01 WS-PATCH-ROW         PIC 9(3).
       01 WS-PATCH-POS         PIC 9(6).
       01 WS-POST-TOP          PIC 9(3).
       01 WS-POST-LEN          PIC 9(3).
       01 WS-TEX-COL           PIC S9(5).
       01 WS-TEX-ROW           PIC S9(5).
       01 WS-TEX-PIX-OFF       PIC 9(6).
       01 WS-PATCH-IDX         PIC 9(5).
       01 WS-PATCH-ORIG-X      PIC S9(5).
       01 WS-PATCH-ORIG-Y      PIC S9(5).
       01 WS-PATCH-CNT         PIC 9(3).
       01 WS-PATCH-NAME        PIC X(8).
       01 WS-EXIT-FLAG         PIC 9.
       01 WS-F-START-IDX       PIC 9(5).
       01 WS-F-END-IDX         PIC 9(5).

      *> WAD loop variable
       01 WS-WAD-I             PIC 9(5).
