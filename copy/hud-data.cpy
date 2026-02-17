      *> ============================================================
      *> hud-data.cpy — Data for heads-up display (status bar)
      *> Health, ammo, face indicator drawn to framebuffer.
      *> All variables prefixed WS-HUD- to avoid collisions.
      *> ============================================================

      *> --- Status bar geometry ---
       01 WS-HUD-BAR-H            PIC 9(3) VALUE 032.
       01 WS-HUD-BAR-Y            PIC 9(3) VALUE 168.
       01 WS-HUD-BORDER-Y         PIC 9(3) VALUE 167.

      *> --- HUD colors stored as raw bytes for framebuffer ---
      *> Background: dark gray (R=48 G=48 B=48)
       01 WS-HUD-BG-RGB.
          05 WS-HUD-BG-R          PIC X VALUE X"30".
          05 WS-HUD-BG-G          PIC X VALUE X"30".
          05 WS-HUD-BG-B          PIC X VALUE X"30".

      *> Border: lighter gray (R=96 G=96 B=96)
       01 WS-HUD-BORD-RGB.
          05 WS-HUD-BORD-R        PIC X VALUE X"60".
          05 WS-HUD-BORD-G        PIC X VALUE X"60".
          05 WS-HUD-BORD-B        PIC X VALUE X"60".

      *> Text: bright yellow (R=255 G=255 B=0)
       01 WS-HUD-TXT-RGB.
          05 WS-HUD-TXT-R         PIC X VALUE X"FF".
          05 WS-HUD-TXT-G         PIC X VALUE X"FF".
          05 WS-HUD-TXT-B         PIC X VALUE X"00".

      *> --- Health number color presets (raw bytes) ---
      *> Green: health > 50  (R=0, G=255, B=0)
       01 WS-HUD-HP-HI-RGB.
          05 WS-HUD-HP-HI-R       PIC X VALUE X"00".
          05 WS-HUD-HP-HI-G       PIC X VALUE X"FF".
          05 WS-HUD-HP-HI-B       PIC X VALUE X"00".
      *> Yellow: health 26-50 (R=255, G=255, B=0)
       01 WS-HUD-HP-MID-RGB.
          05 WS-HUD-HP-MID-R      PIC X VALUE X"FF".
          05 WS-HUD-HP-MID-G      PIC X VALUE X"FF".
          05 WS-HUD-HP-MID-B      PIC X VALUE X"00".
      *> Red: health <= 25  (R=255, G=0, B=0)
       01 WS-HUD-HP-LO-RGB.
          05 WS-HUD-HP-LO-R       PIC X VALUE X"FF".
          05 WS-HUD-HP-LO-G       PIC X VALUE X"00".
          05 WS-HUD-HP-LO-B       PIC X VALUE X"00".

      *> --- Face indicator color presets (raw bytes) ---
      *> Green face: health > 66  (R=0, G=200, B=0)
       01 WS-HUD-FACE-HI-RGB.
          05 WS-HUD-FACE-HI-R     PIC X VALUE X"00".
          05 WS-HUD-FACE-HI-G     PIC X VALUE X"C8".
          05 WS-HUD-FACE-HI-B     PIC X VALUE X"00".
      *> Yellow face: health 34-66  (R=200, G=200, B=0)
       01 WS-HUD-FACE-MID-RGB.
          05 WS-HUD-FACE-MID-R    PIC X VALUE X"C8".
          05 WS-HUD-FACE-MID-G    PIC X VALUE X"C8".
          05 WS-HUD-FACE-MID-B    PIC X VALUE X"00".
      *> Red face: health < 34  (R=200, G=0, B=0)
       01 WS-HUD-FACE-LO-RGB.
          05 WS-HUD-FACE-LO-R     PIC X VALUE X"C8".
          05 WS-HUD-FACE-LO-G     PIC X VALUE X"00".
          05 WS-HUD-FACE-LO-B     PIC X VALUE X"00".

      *> --- Dark border for face indicator (R=32, G=32, B=32) ---
       01 WS-HUD-FACE-BRD-RGB.
          05 WS-HUD-FACE-BRD-R    PIC X VALUE X"20".
          05 WS-HUD-FACE-BRD-G    PIC X VALUE X"20".
          05 WS-HUD-FACE-BRD-B    PIC X VALUE X"20".

      *> --- Current draw color (raw bytes for FB writes) ---
       01 WS-HUD-CUR-RGB.
          05 WS-HUD-CUR-R         PIC X.
          05 WS-HUD-CUR-G         PIC X.
          05 WS-HUD-CUR-B         PIC X.

      *> --- Digit bitmap table: 5x7 pixels per character ---
      *> Characters: 0-9(idx 1-10), H(11), E(12), A(13), L(14),
      *>   T(15), P(16), M(17), O(18), %(19)
      *> Each char = 35 bytes: 7 rows x 5 cols, "1"=pixel "0"=off
      *> Row-major order: row1(5) row2(5) ... row7(5)
       01 WS-HUD-CHAR-COUNT        PIC 9(3) VALUE 019.
       01 WS-HUD-FONT-DATA.
      *>   Digit 0 (index 1):
      *>     .XXX.  X...X  X...X  X...X  X...X  X...X  .XXX.
          05 FILLER PIC X(35) VALUE
             "01110100011000110001100011000101110".
      *>   Digit 1:
      *>     ..X..  .XX..  ..X..  ..X..  ..X..  ..X..  .XXX.
          05 FILLER PIC X(35) VALUE
             "00100011000010000100001000010001110".
      *>   Digit 2:
      *>     .XXX.  X...X  ....X  ..XX.  .X...  X....  XXXXX
          05 FILLER PIC X(35) VALUE
             "01110100010000100110010001000011111".
      *>   Digit 3:
      *>     .XXX.  X...X  ....X  ..XX.  ....X  X...X  .XXX.
          05 FILLER PIC X(35) VALUE
             "01110100010000100110000011000101110".
      *>   Digit 4:
      *>     ...X.  ..XX.  .X.X.  X..X.  XXXXX  ...X.  ...X.
          05 FILLER PIC X(35) VALUE
             "00010001100101010010111110001000010".
      *>   Digit 5:
      *>     XXXXX  X....  XXXX.  ....X  ....X  X...X  .XXX.
          05 FILLER PIC X(35) VALUE
             "11111100001111000001000011000101110".
      *>   Digit 6:
      *>     .XXX.  X....  X....  XXXX.  X...X  X...X  .XXX.
          05 FILLER PIC X(35) VALUE
             "01110100001000011110100011000101110".
      *>   Digit 7:
      *>     XXXXX  ....X  ...X.  ..X..  .X...  .X...  .X...
          05 FILLER PIC X(35) VALUE
             "11111000010001000100010000100001000".
      *>   Digit 8:
      *>     .XXX.  X...X  X...X  .XXX.  X...X  X...X  .XXX.
          05 FILLER PIC X(35) VALUE
             "01110100011000101110100011000101110".
      *>   Digit 9:
      *>     .XXX.  X...X  X...X  .XXXX  ....X  ....X  .XXX.
          05 FILLER PIC X(35) VALUE
             "01110100011000101111000010000101110".
      *>   Letter H (index 11):
      *>     X...X  X...X  X...X  XXXXX  X...X  X...X  X...X
          05 FILLER PIC X(35) VALUE
             "10001100011000111111100011000110001".
      *>   Letter E (index 12):
      *>     XXXXX  X....  X....  XXXX.  X....  X....  XXXXX
          05 FILLER PIC X(35) VALUE
             "11111100001000011110100001000011111".
      *>   Letter A (index 13):
      *>     .XXX.  X...X  X...X  XXXXX  X...X  X...X  X...X
          05 FILLER PIC X(35) VALUE
             "01110100011000111111100011000110001".
      *>   Letter L (index 14):
      *>     X....  X....  X....  X....  X....  X....  XXXXX
          05 FILLER PIC X(35) VALUE
             "10000100001000010000100001000011111".
      *>   Letter T (index 15):
      *>     XXXXX  ..X..  ..X..  ..X..  ..X..  ..X..  ..X..
          05 FILLER PIC X(35) VALUE
             "11111001000010000100001000010000100".
      *>   Letter P (index 16):
      *>     XXXX.  X...X  X...X  XXXX.  X....  X....  X....
          05 FILLER PIC X(35) VALUE
             "11110100011000111110100001000010000".
      *>   Letter M (index 17):
      *>     X...X  XX.XX  X.X.X  X...X  X...X  X...X  X...X
          05 FILLER PIC X(35) VALUE
             "10001110111010110001100011000110001".
      *>   Letter O (index 18):
      *>     .XXX.  X...X  X...X  X...X  X...X  X...X  .XXX.
          05 FILLER PIC X(35) VALUE
             "01110100011000110001100011000101110".
      *>   Percent % (index 19):
      *>     X...X  ....X  ...X.  ..X..  .X...  X....  X...X
          05 FILLER PIC X(35) VALUE
             "10001000010001000100010001000010001".

       01 WS-HUD-FONT-R REDEFINES WS-HUD-FONT-DATA.
          05 WS-HUD-CHAR-BMP       PIC X(35)
                                    OCCURS 19 TIMES.

      *> --- Character map for "HEALTH" (indices into font) ---
      *> H=11 E=12 A=13 L=14 T=15 H=11
       01 WS-HUD-HLTH-IDX.
          05 FILLER PIC 9(2) VALUE 11.
          05 FILLER PIC 9(2) VALUE 12.
          05 FILLER PIC 9(2) VALUE 13.
          05 FILLER PIC 9(2) VALUE 14.
          05 FILLER PIC 9(2) VALUE 15.
          05 FILLER PIC 9(2) VALUE 11.
       01 WS-HUD-HLTH-IDX-R REDEFINES WS-HUD-HLTH-IDX.
          05 WS-HUD-HLTH-CI        PIC 9(2) OCCURS 6 TIMES.

      *> --- Character map for "AMMO" (indices into font) ---
      *> A=13 M=17 M=17 O=18
       01 WS-HUD-AMMO-IDX.
          05 FILLER PIC 9(2) VALUE 13.
          05 FILLER PIC 9(2) VALUE 17.
          05 FILLER PIC 9(2) VALUE 17.
          05 FILLER PIC 9(2) VALUE 18.
       01 WS-HUD-AMMO-IDX-R REDEFINES WS-HUD-AMMO-IDX.
          05 WS-HUD-AMMO-CI        PIC 9(2) OCCURS 4 TIMES.

      *> --- HUD layout positions ---
      *> Health label "HEALTH" at x=10, y=172
       01 WS-HUD-HLBL-X           PIC 9(3) VALUE 010.
       01 WS-HUD-HLBL-Y           PIC 9(3) VALUE 172.
      *> Health number at x=10, y=182
       01 WS-HUD-HNUM-X           PIC 9(3) VALUE 010.
       01 WS-HUD-HNUM-Y           PIC 9(3) VALUE 182.

      *> Face indicator: 20x20 square at x=150, y=173
       01 WS-HUD-FACE-X           PIC 9(3) VALUE 150.
       01 WS-HUD-FACE-Y           PIC 9(3) VALUE 173.
       01 WS-HUD-FACE-SZ          PIC 9(3) VALUE 020.

      *> Ammo label "AMMO" at x=230, y=172
       01 WS-HUD-ALBL-X           PIC 9(3) VALUE 230.
       01 WS-HUD-ALBL-Y           PIC 9(3) VALUE 172.
      *> Ammo number at x=230, y=182
       01 WS-HUD-ANUM-X           PIC 9(3) VALUE 230.
       01 WS-HUD-ANUM-Y           PIC 9(3) VALUE 182.

      *> --- Drawing temp variables ---
       01 WS-HUD-DRW-X            PIC 9(3).
       01 WS-HUD-DRW-Y            PIC 9(3).
       01 WS-HUD-FB-IDX           PIC 9(6).

      *> --- DRAW-HUD-CHAR parameters ---
       01 WS-HUD-CHR-IDX          PIC 9(3).
       01 WS-HUD-CHR-X            PIC 9(3).
       01 WS-HUD-CHR-Y            PIC 9(3).
       01 WS-HUD-CHR-ROW          PIC 9(3).
       01 WS-HUD-CHR-COL          PIC 9(3).
       01 WS-HUD-CHR-BIT          PIC 9(3).
       01 WS-HUD-CHR-PIX          PIC X.

      *> --- DRAW-HUD-NUMBER parameters ---
       01 WS-HUD-NUM-VAL          PIC 9(3).
       01 WS-HUD-NUM-X            PIC 9(3).
       01 WS-HUD-NUM-Y            PIC 9(3).
       01 WS-HUD-NUM-DIG          PIC 9(3).
       01 WS-HUD-NUM-HUND         PIC 9.
       01 WS-HUD-NUM-TENS         PIC 9.
       01 WS-HUD-NUM-ONES         PIC 9.
       01 WS-HUD-NUM-CUR-X        PIC 9(3).
       01 WS-HUD-NUM-LEAD         PIC 9 VALUE 0.

      *> --- DRAW-HUD-RECT parameters ---
       01 WS-HUD-RECT-X           PIC 9(3).
       01 WS-HUD-RECT-Y           PIC 9(3).
       01 WS-HUD-RECT-W           PIC 9(3).
       01 WS-HUD-RECT-H           PIC 9(3).
       01 WS-HUD-RECT-RY          PIC 9(3).
       01 WS-HUD-RECT-RX          PIC 9(3).
       01 WS-HUD-RECT-PX          PIC 9(3).
       01 WS-HUD-RECT-PY          PIC 9(3).

      *> --- Loop/temp variables ---
       01 WS-HUD-LBL-I            PIC 9(3).
       01 WS-HUD-LBL-X            PIC 9(3).
       01 WS-HUD-TEMP             PIC 9(3).
