      *> ============================================================
      *> doors-data.cpy — Data for door mechanics and item pickups
      *> Door table, pickup table, and working variables.
      *> ============================================================

      *> --- Door table: up to 100 tracked doors ---
       01 WS-DR-COUNT              PIC 9(3) VALUE 0.
       01 WS-DR-TABLE.
          05 WS-DR-ENTRY OCCURS 100 TIMES.
             10 WS-DR-ROW          PIC 9(3).
             10 WS-DR-COL          PIC 9(3).
             10 WS-DR-STATE        PIC 9.
      *>       0=closed, 1=opening, 2=open, 3=closing
             10 WS-DR-TIMER        PIC 9(3).
             10 WS-DR-OPEN-DUR     PIC 9(3) VALUE 90.
             10 WS-DR-SPEED        PIC 9(3) VALUE 4.
             10 WS-DR-OFFSET       PIC 9(3).
      *>       0=fully closed, 64=fully open

      *> --- Door working variables ---
       01 WS-DR-IDX                PIC 9(3).
       01 WS-DR-FACE-X             PIC S9(5)V9(4).
       01 WS-DR-FACE-Y             PIC S9(5)V9(4).
       01 WS-DR-FACE-ROW           PIC S9(5).
       01 WS-DR-FACE-COL           PIC S9(5).
       01 WS-DR-ANGLE-LOOKUP       PIC 9(5).
       01 WS-DR-FOUND              PIC 9.
       01 WS-DR-SEARCH-IDX         PIC 9(3).

      *> --- Pickup table: up to 100 tracked pickups ---
       01 WS-PK-COUNT              PIC 9(3) VALUE 0.
       01 WS-PK-TABLE.
          05 WS-PK-ENTRY OCCURS 100 TIMES.
             10 WS-PK-X            PIC S9(5)V9(4).
             10 WS-PK-Y            PIC S9(5)V9(4).
             10 WS-PK-TYPE         PIC 9(5).
             10 WS-PK-ACTIVE       PIC 9.
      *>       1=active, 0=picked up

      *> --- Pickup working variables ---
       01 WS-PK-IDX                PIC 9(3).
       01 WS-PK-DIST-X             PIC S9(5)V9(4).
       01 WS-PK-DIST-Y             PIC S9(5)V9(4).
       01 WS-PK-DIST               PIC S9(5)V9(4).
       01 WS-PK-RADIUS             PIC 9V9(4) VALUE 0.5000.
       01 WS-PK-GRID-X             PIC S9(5)V9(4).
       01 WS-PK-GRID-Y             PIC S9(5)V9(4).
       01 WS-PK-THING-IDX          PIC 9(3).
