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

      *> WAD loop variable
       01 WS-WAD-I             PIC 9(5).
