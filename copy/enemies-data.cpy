      *> ============================================================
      *> enemies-data.cpy -- Data for enemy AI system
      *> AI state table, line-of-sight vars, movement temps.
      *> All variables prefixed WS-EN- to avoid collisions.
      *> Depends on: sprites-data.cpy (WS-SP-TABLE),
      *>   raycaster-data.cpy (WS-PX/PY/PA, trig tables),
      *>   map-loader-data.cpy (MC-TYPE grid, WS-MAP-SIZE).
      *> ============================================================

      *> --- AI state codes (used in WS-EN-STATE) ---
      *>   0=IDLE   1=ALERT   2=CHASE   3=ATTACK
      *>   4=PAIN   5=DYING   6=DEAD

      *> --- AI state table: one entry per sprite (50 max) ---
       01 WS-EN-AI-TABLE.
          05 WS-EN-AI-ENTRY OCCURS 50 TIMES.
             10 WS-EN-STATE        PIC 9.
      *>       Current AI state (0-6)
             10 WS-EN-ALERT-DIST   PIC S9(5)V9(4)
                                    VALUE +00015.0000.
      *>       Distance at which enemy notices player
             10 WS-EN-ATTACK-DIST  PIC S9(5)V9(4)
                                    VALUE +00003.0000.
      *>       Distance at which enemy can attack
             10 WS-EN-DAMAGE       PIC 9(3) VALUE 010.
      *>       Base damage per attack
             10 WS-EN-MOVE-SPEED   PIC S9(3)V9(4)
                                    VALUE +000.0500.
      *>       Grid units per tick
             10 WS-EN-FRAME-CTR    PIC 9(3) VALUE 0.
      *>       Frame counter for animation timing
             10 WS-EN-LOS-FLAG     PIC 9 VALUE 0.
      *>       1=has line of sight to player
             10 WS-EN-COOLDOWN     PIC 9(3) VALUE 0.
      *>       Attack cooldown timer (ticks)

      *> --- Current enemy index for processing ---
       01 WS-EN-IDX                PIC 9(3).

      *> --- Distance to player (computed per tick) ---
       01 WS-EN-DIST               PIC S9(7)V9(4).
       01 WS-EN-DIST-SQ            PIC S9(9)V9(4).
       01 WS-EN-DX                 PIC S9(7)V9(4).
       01 WS-EN-DY                 PIC S9(7)V9(4).

      *> --- LOS raycasting variables (DDA through MC-TYPE) ---
       01 WS-EN-LOS-RAY-X         PIC S9(3)V9(6).
       01 WS-EN-LOS-RAY-Y         PIC S9(3)V9(6).
       01 WS-EN-LOS-MAP-X         PIC S9(5).
       01 WS-EN-LOS-MAP-Y         PIC S9(5).
       01 WS-EN-LOS-STEP-X        PIC S9(1).
       01 WS-EN-LOS-STEP-Y        PIC S9(1).
       01 WS-EN-LOS-SIDE-X        PIC S9(5)V9(6).
       01 WS-EN-LOS-SIDE-Y        PIC S9(5)V9(6).
       01 WS-EN-LOS-DELTA-X       PIC S9(5)V9(6).
       01 WS-EN-LOS-DELTA-Y       PIC S9(5)V9(6).
       01 WS-EN-LOS-HIT           PIC 9.
       01 WS-EN-LOS-STEPS         PIC 9(3).
       01 WS-EN-LOS-MAX-STEPS     PIC 9(3) VALUE 128.
       01 WS-EN-LOS-TARGET-MX     PIC S9(5).
       01 WS-EN-LOS-TARGET-MY     PIC S9(5).

      *> --- Movement working variables ---
       01 WS-EN-MOVE-DX            PIC S9(5)V9(6).
       01 WS-EN-MOVE-DY            PIC S9(5)V9(6).
       01 WS-EN-NEW-X              PIC S9(5)V9(4).
       01 WS-EN-NEW-Y              PIC S9(5)V9(4).
       01 WS-EN-CHK-X              PIC S9(5).
       01 WS-EN-CHK-Y              PIC S9(5).
       01 WS-EN-COLLISION-PAD      PIC 9V9(4) VALUE 0.2000.

      *> --- Angle from enemy to player (for trig lookup) ---
       01 WS-EN-ANGLE              PIC S9(7).
       01 WS-EN-TRIG-IDX           PIC 9(5).

      *> --- Temp vars for distance/sqrt calculation ---
       01 WS-EN-TEMP-A             PIC S9(9)V9(6).
       01 WS-EN-TEMP-B             PIC S9(9)V9(6).
       01 WS-EN-TEMP-C             PIC S9(9)V9(6).

      *> --- Damage application interface ---
      *> Set these before calling EN-APPLY-DAMAGE
       01 WS-EN-DMG-IDX            PIC 9(3).
      *>   Sprite index to apply damage to (1..50)
       01 WS-EN-DMG-AMT            PIC 9(3).
      *>   Amount of damage to apply

      *> --- Random number for attack damage variance ---
       01 WS-EN-RAND-VAL           PIC 9(3).
       01 WS-EN-RAND-SEED          PIC 9(9) VALUE 12345.

      *> --- Death frame counter ---
       01 WS-EN-DEATH-FRAME        PIC 9(3).

      *> --- Sprite base name temp ---
       01 WS-EN-BASE-NAME          PIC X(4).
       01 WS-EN-LUMP-TEMP          PIC X(8).

      *> --- Alert distance squared (for fast compare) ---
       01 WS-EN-ALERT-DIST-SQ     PIC S9(9)V9(4).
       01 WS-EN-ATTACK-DIST-SQ    PIC S9(9)V9(4).

      *> --- Walk frame toggle (alternates 0/1) ---
       01 WS-EN-WALK-FRAME         PIC 9.
