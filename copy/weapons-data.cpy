      *> ============================================================
      *> weapons-data.cpy -- Data for weapon system
      *> Current weapon, fire state, sprite rendering, hitscan,
      *> crosshair, and input tracking variables.
      *> All variables prefixed WS-WP- to avoid collisions.
      *> ============================================================

      *> --- Current weapon and state ---
       01 WS-WP-CURRENT            PIC 9 VALUE 1.
      *>   0=fist, 1=pistol, 2=shotgun
       01 WS-WP-STATE              PIC 9 VALUE 0.
      *>   0=ready, 1=firing, 2=cooldown

      *> --- Fire timer and cooldown ---
       01 WS-WP-FIRE-TIMER         PIC 9(3) VALUE 0.
       01 WS-WP-COOLDOWN-TIMER     PIC 9(3) VALUE 0.
       01 WS-WP-FIRE-DURATION      PIC 9(3) VALUE 8.
      *>   Total animation frames for fire sequence
       01 WS-WP-COOLDOWN-DURATION  PIC 9(3) VALUE 6.
      *>   Frames to wait after firing before ready
       01 WS-WP-ANIM-FRAME         PIC 9 VALUE 0.
      *>   Current animation frame: 0=idle, 1-4=fire frames

      *> --- Weapon sprite screen position ---
       01 WS-WP-SCREEN-X           PIC S9(5) VALUE 120.
       01 WS-WP-SCREEN-Y           PIC S9(5) VALUE 100.
      *>   Top-left of where weapon is drawn on screen

      *> --- 16 KB weapon sprite patch buffer ---
       01 WS-WP-PATCH-BUF          PIC X(16384).

      *> --- Weapon patch header fields ---
       01 WS-WP-PATCH-W            PIC S9(5).
       01 WS-WP-PATCH-H            PIC S9(5).
       01 WS-WP-PATCH-LEFT         PIC S9(5).
       01 WS-WP-PATCH-TOP          PIC S9(5).

      *> --- Screen-space rendering vars for weapon overlay ---
       01 WS-WP-DRAW-LEFT          PIC S9(5).
       01 WS-WP-DRAW-RIGHT         PIC S9(5).
       01 WS-WP-DRAW-TOP           PIC S9(5).
       01 WS-WP-DRAW-BOT           PIC S9(5).
       01 WS-WP-DRAW-W             PIC S9(5).
       01 WS-WP-DRAW-H             PIC S9(5).

      *> --- Weapon column rendering variables ---
       01 WS-WP-COL                PIC S9(5).
       01 WS-WP-COL-OFF            PIC 9(6).
       01 WS-WP-POST-POS           PIC 9(6).
       01 WS-WP-POST-TOP           PIC 9(3).
       01 WS-WP-POST-LEN           PIC 9(3).
       01 WS-WP-POST-EXIT          PIC 9.
       01 WS-WP-TEX-COL            PIC S9(5).
       01 WS-WP-TEX-ROW            PIC S9(5).
       01 WS-WP-SCREEN-ROW         PIC S9(5).

      *> --- Pixel rendering temps ---
       01 WS-WP-PAL-IDX            PIC 9(3).
       01 WS-WP-LIT-IDX            PIC 9(3).
       01 WS-WP-FB-IDX             PIC 9(6).
       01 WS-WP-PIX-ROW            PIC S9(5).

      *> --- Fire input tracking (anti-autofire) ---
       01 WS-WP-FIRE-PREV          PIC 9 VALUE 0.
      *>   0=fire key was released, 1=fire key was held
       01 WS-WP-FIRE-EDGE          PIC 9 VALUE 0.
      *>   1=fire key just pressed this frame (rising edge)

      *> --- Hitscan working variables ---
       01 WS-WP-HS-I               PIC 9(3).
       01 WS-WP-HS-DX              PIC S9(7)V9(4).
       01 WS-WP-HS-DY              PIC S9(7)V9(4).
       01 WS-WP-HS-DEPTH           PIC S9(7)V9(4).
       01 WS-WP-HS-LATERAL         PIC S9(7)V9(4).
       01 WS-WP-HS-SCREEN-X        PIC S9(5).
       01 WS-WP-HS-HIT             PIC 9 VALUE 0.
       01 WS-WP-HS-BEST-DIST       PIC S9(7)V9(4).
       01 WS-WP-HS-BEST-IDX        PIC 9(3).
       01 WS-WP-HS-DMG             PIC 9(3).

      *> --- Shotgun pellet spread ---
       01 WS-WP-SG-PELLET          PIC 9(3).
       01 WS-WP-SG-PELLET-MAX      PIC 9(3) VALUE 7.
       01 WS-WP-SG-SPREAD          PIC S9(5)V9(4).
      *>   Lateral offset for each pellet (pixels from center)
       01 WS-WP-SG-OFFSET          PIC S9(5).
      *>   Screen-X offset per pellet: -15 to +15 pixels

      *> --- View-space projection temps for hitscan ---
       01 WS-WP-VIEW-DX            PIC S9(3)V9(6).
       01 WS-WP-VIEW-DY            PIC S9(3)V9(6).
       01 WS-WP-STRAFE-DX          PIC S9(3)V9(6).
       01 WS-WP-STRAFE-DY          PIC S9(3)V9(6).
       01 WS-WP-TRIG-IDX           PIC 9(5).

      *> --- Enemy damage interface: WS-EN-DMG-IDX and
      *>     WS-EN-DMG-AMT are defined in enemies-data.cpy ---

      *> --- Weapon lump name builder ---
       01 WS-WP-LUMP-NAME          PIC X(8).

      *> --- Crosshair vars ---
       01 WS-WP-CROSS-X            PIC 9(3) VALUE 160.
       01 WS-WP-CROSS-Y            PIC 9(3) VALUE 100.
       01 WS-WP-CROSS-FB           PIC 9(6).
       01 WS-WP-CROSS-I            PIC 9(3).

      *> --- Weapon switch tracking ---
       01 WS-WP-WANT-SWITCH        PIC 9 VALUE 0.
       01 WS-WP-NEXT-WEAPON        PIC 9 VALUE 0.
