      *> ============================================================
      *> enemies-proc.cpy -- Enemy AI procedures
      *> UPDATE-ENEMIES: main AI tick (call each frame)
      *> EN-CHECK-LOS: line-of-sight via DDA
      *> EN-MOVE-TOWARD-PLAYER: movement with collision
      *> EN-UPDATE-SPRITE-NAME: set lump name from AI state
      *> EN-APPLY-DAMAGE: damage interface for weapons
      *> Depends on: enemies-data.cpy, sprites-data.cpy,
      *>   raycaster-data.cpy, map-loader-data.cpy
      *> ============================================================

      *> ============================================================
      *> UPDATE-ENEMIES: Main AI tick, called each frame.
      *> Iterates all alive enemies, runs state machine.
      *> ============================================================
       UPDATE-ENEMIES.
           IF WS-SP-COUNT < 1
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-EN-IDX FROM 1 BY 1
               UNTIL WS-EN-IDX > WS-SP-COUNT
      *>       Skip dead sprites (renderer state 2 = dead)
               IF WS-SP-STATE(WS-EN-IDX) NOT = 2
      *>           Also skip AI-dead (state 6)
                   IF WS-EN-STATE(WS-EN-IDX) NOT = 6
                       PERFORM EN-TICK-ONE-ENEMY
                   END-IF
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> EN-TICK-ONE-ENEMY: Process AI for enemy WS-EN-IDX.
      *> ============================================================
       EN-TICK-ONE-ENEMY.
      *>   Decrement cooldown if active
           IF WS-EN-COOLDOWN(WS-EN-IDX) > 0
               SUBTRACT 1 FROM WS-EN-COOLDOWN(WS-EN-IDX)
           END-IF

      *>   Compute distance to player
           COMPUTE WS-EN-DX =
               WS-PX - WS-SP-WORLD-X(WS-EN-IDX)
           COMPUTE WS-EN-DY =
               WS-PY - WS-SP-WORLD-Y(WS-EN-IDX)
           COMPUTE WS-EN-DIST-SQ =
               WS-EN-DX * WS-EN-DX
               + WS-EN-DY * WS-EN-DY

      *>   Approximate distance via sqrt (Newton's method)
           IF WS-EN-DIST-SQ > 0
               PERFORM EN-APPROX-SQRT
           ELSE
               MOVE 0 TO WS-EN-DIST
           END-IF

      *>   Branch on current AI state
           EVALUATE WS-EN-STATE(WS-EN-IDX)
               WHEN 0
                   PERFORM EN-STATE-IDLE
               WHEN 1
                   PERFORM EN-STATE-ALERT
               WHEN 2
                   PERFORM EN-STATE-CHASE
               WHEN 3
                   PERFORM EN-STATE-ATTACK
               WHEN 4
                   PERFORM EN-STATE-PAIN
               WHEN 5
                   PERFORM EN-STATE-DYING
           END-EVALUATE

      *>   Update sprite lump name for renderer
           PERFORM EN-UPDATE-SPRITE-NAME
           .

      *> ============================================================
      *> EN-APPROX-SQRT: Newton's method sqrt of WS-EN-DIST-SQ.
      *> Result in WS-EN-DIST. 4 iterations for good accuracy.
      *> ============================================================
       EN-APPROX-SQRT.
      *>   Initial guess: half of input (clamped)
           COMPUTE WS-EN-TEMP-A = WS-EN-DIST-SQ / 2.0
           IF WS-EN-TEMP-A < 1
               MOVE 1 TO WS-EN-TEMP-A
           END-IF
      *>   4 Newton iterations: x = (x + n/x) / 2
           PERFORM 4 TIMES
               IF WS-EN-TEMP-A > 0
                   COMPUTE WS-EN-TEMP-A =
                       (WS-EN-TEMP-A
                       + WS-EN-DIST-SQ / WS-EN-TEMP-A)
                       / 2.0
               END-IF
           END-PERFORM
           MOVE WS-EN-TEMP-A TO WS-EN-DIST
           .

      *> ============================================================
      *> EN-STATE-IDLE (0): Check if player is within alert range
      *> and has line of sight. If so, transition to ALERT.
      *> ============================================================
       EN-STATE-IDLE.
      *>   Increment frame counter for idle animation
           ADD 1 TO WS-EN-FRAME-CTR(WS-EN-IDX)
           IF WS-EN-FRAME-CTR(WS-EN-IDX) > 999
               MOVE 0 TO WS-EN-FRAME-CTR(WS-EN-IDX)
           END-IF
      *>   Check alert distance (use squared for speed)
           COMPUTE WS-EN-ALERT-DIST-SQ =
               WS-EN-ALERT-DIST(WS-EN-IDX)
               * WS-EN-ALERT-DIST(WS-EN-IDX)
           IF WS-EN-DIST-SQ <=
               WS-EN-ALERT-DIST-SQ
      *>       Player is within alert range, check LOS
               PERFORM EN-CHECK-LOS
               IF WS-EN-LOS-FLAG(WS-EN-IDX) = 1
      *>           Player spotted -- go to ALERT
                   MOVE 1 TO WS-EN-STATE(WS-EN-IDX)
                   MOVE 15 TO WS-EN-FRAME-CTR(WS-EN-IDX)
               END-IF
           END-IF
           .

      *> ============================================================
      *> EN-STATE-ALERT (1): Play alert animation, then CHASE.
      *> ============================================================
       EN-STATE-ALERT.
      *>   Decrement alert timer
           IF WS-EN-FRAME-CTR(WS-EN-IDX) > 0
               SUBTRACT 1 FROM
                   WS-EN-FRAME-CTR(WS-EN-IDX)
           ELSE
      *>       Alert animation done, start chasing
               MOVE 2 TO WS-EN-STATE(WS-EN-IDX)
               MOVE 0 TO WS-EN-FRAME-CTR(WS-EN-IDX)
           END-IF
           .

      *> ============================================================
      *> EN-STATE-CHASE (2): Move toward player, check attack range.
      *> ============================================================
       EN-STATE-CHASE.
      *>   Move toward player
           PERFORM EN-MOVE-TOWARD-PLAYER

      *>   Increment frame counter for walk animation
           ADD 1 TO WS-EN-FRAME-CTR(WS-EN-IDX)
           IF WS-EN-FRAME-CTR(WS-EN-IDX) > 999
               MOVE 0 TO WS-EN-FRAME-CTR(WS-EN-IDX)
           END-IF

      *>   Check if within attack range
           COMPUTE WS-EN-ATTACK-DIST-SQ =
               WS-EN-ATTACK-DIST(WS-EN-IDX)
               * WS-EN-ATTACK-DIST(WS-EN-IDX)
           IF WS-EN-DIST-SQ <=
               WS-EN-ATTACK-DIST-SQ
      *>       Check LOS before attacking
               PERFORM EN-CHECK-LOS
               IF WS-EN-LOS-FLAG(WS-EN-IDX) = 1
                   IF WS-EN-COOLDOWN(WS-EN-IDX) = 0
      *>               In range with LOS and ready -- ATTACK
                       MOVE 3 TO
                           WS-EN-STATE(WS-EN-IDX)
                       MOVE 0 TO
                           WS-EN-FRAME-CTR(WS-EN-IDX)
                   END-IF
               END-IF
           END-IF
           .

      *> ============================================================
      *> EN-STATE-ATTACK (3): Fire at player, deal damage, cooldown.
      *> ============================================================
       EN-STATE-ATTACK.
      *>   Attack frame plays for a few ticks
           ADD 1 TO WS-EN-FRAME-CTR(WS-EN-IDX)

           IF WS-EN-FRAME-CTR(WS-EN-IDX) = 1
      *>       First tick of attack: compute and deal damage
      *>       Random damage between 3 and 15
               PERFORM EN-RANDOM-3-15
      *>       Apply damage to player health
               IF WS-HEALTH > WS-EN-RAND-VAL
                   SUBTRACT WS-EN-RAND-VAL FROM WS-HEALTH
               ELSE
                   MOVE 0 TO WS-HEALTH
               END-IF
           END-IF

      *>   After 10 ticks, return to CHASE with cooldown
           IF WS-EN-FRAME-CTR(WS-EN-IDX) >= 10
               MOVE 2 TO WS-EN-STATE(WS-EN-IDX)
               MOVE 0 TO WS-EN-FRAME-CTR(WS-EN-IDX)
               MOVE 30 TO WS-EN-COOLDOWN(WS-EN-IDX)
           END-IF
           .

      *> ============================================================
      *> EN-STATE-PAIN (4): Show pain frame for 5 ticks, then CHASE.
      *> ============================================================
       EN-STATE-PAIN.
           ADD 1 TO WS-EN-FRAME-CTR(WS-EN-IDX)
           IF WS-EN-FRAME-CTR(WS-EN-IDX) >= 5
      *>       Pain over, resume chasing
               MOVE 2 TO WS-EN-STATE(WS-EN-IDX)
               MOVE 0 TO WS-EN-FRAME-CTR(WS-EN-IDX)
           END-IF
           .

      *> ============================================================
      *> EN-STATE-DYING (5): Cycle death frames H-L, then DEAD.
      *> ============================================================
       EN-STATE-DYING.
           ADD 1 TO WS-EN-FRAME-CTR(WS-EN-IDX)

      *>   5 death frames (H,I,J,K,L), 5 ticks each = 25 total
           IF WS-EN-FRAME-CTR(WS-EN-IDX) >= 25
      *>       Death animation complete
      *>       Set sprite renderer state to 2 (dead)
               MOVE 2 TO WS-SP-STATE(WS-EN-IDX)
      *>       Set AI state to 6 (DEAD)
               MOVE 6 TO WS-EN-STATE(WS-EN-IDX)
           END-IF
           .

      *> ============================================================
      *> EN-CHECK-LOS: DDA ray from enemy to player through grid.
      *> Sets WS-EN-LOS-FLAG(WS-EN-IDX) = 1 if clear, 0 if blocked.
      *> ============================================================
       EN-CHECK-LOS.
           MOVE 0 TO WS-EN-LOS-FLAG(WS-EN-IDX)

      *>   Source: enemy position
      *>   Target: player position
      *>   Compute ray direction (not normalized, just dx/dy)
           IF WS-EN-DIST < 0.01
      *>       Enemy on top of player, trivially visible
               MOVE 1 TO WS-EN-LOS-FLAG(WS-EN-IDX)
               EXIT PARAGRAPH
           END-IF

      *>   Normalize direction
           COMPUTE WS-EN-LOS-RAY-X =
               WS-EN-DX / WS-EN-DIST
           COMPUTE WS-EN-LOS-RAY-Y =
               WS-EN-DY / WS-EN-DIST

      *>   Starting map cell for enemy (1-based grid)
           COMPUTE WS-EN-LOS-MAP-X =
               FUNCTION INTEGER-PART(
                   WS-SP-WORLD-X(WS-EN-IDX)) + 1
           COMPUTE WS-EN-LOS-MAP-Y =
               FUNCTION INTEGER-PART(
                   WS-SP-WORLD-Y(WS-EN-IDX)) + 1

      *>   Target map cell for player
           COMPUTE WS-EN-LOS-TARGET-MX =
               FUNCTION INTEGER-PART(WS-PX) + 1
           COMPUTE WS-EN-LOS-TARGET-MY =
               FUNCTION INTEGER-PART(WS-PY) + 1

      *>   Delta distances
           IF WS-EN-LOS-RAY-X NOT = 0
               COMPUTE WS-EN-LOS-DELTA-X =
                   FUNCTION ABS(1.0 / WS-EN-LOS-RAY-X)
           ELSE
               MOVE 999999.0 TO WS-EN-LOS-DELTA-X
           END-IF
           IF WS-EN-LOS-RAY-Y NOT = 0
               COMPUTE WS-EN-LOS-DELTA-Y =
                   FUNCTION ABS(1.0 / WS-EN-LOS-RAY-Y)
           ELSE
               MOVE 999999.0 TO WS-EN-LOS-DELTA-Y
           END-IF

      *>   Step direction and initial side distances
           IF WS-EN-LOS-RAY-X < 0
               MOVE -1 TO WS-EN-LOS-STEP-X
               COMPUTE WS-EN-LOS-SIDE-X =
                   (WS-SP-WORLD-X(WS-EN-IDX)
                   - FUNCTION INTEGER-PART(
                       WS-SP-WORLD-X(WS-EN-IDX)))
                   * WS-EN-LOS-DELTA-X
           ELSE
               MOVE 1 TO WS-EN-LOS-STEP-X
               COMPUTE WS-EN-LOS-SIDE-X =
                   (FUNCTION INTEGER-PART(
                       WS-SP-WORLD-X(WS-EN-IDX))
                   + 1.0
                   - WS-SP-WORLD-X(WS-EN-IDX))
                   * WS-EN-LOS-DELTA-X
           END-IF
           IF WS-EN-LOS-RAY-Y < 0
               MOVE -1 TO WS-EN-LOS-STEP-Y
               COMPUTE WS-EN-LOS-SIDE-Y =
                   (WS-SP-WORLD-Y(WS-EN-IDX)
                   - FUNCTION INTEGER-PART(
                       WS-SP-WORLD-Y(WS-EN-IDX)))
                   * WS-EN-LOS-DELTA-Y
           ELSE
               MOVE 1 TO WS-EN-LOS-STEP-Y
               COMPUTE WS-EN-LOS-SIDE-Y =
                   (FUNCTION INTEGER-PART(
                       WS-SP-WORLD-Y(WS-EN-IDX))
                   + 1.0
                   - WS-SP-WORLD-Y(WS-EN-IDX))
                   * WS-EN-LOS-DELTA-Y
           END-IF

      *>   DDA loop: step through grid cells
           MOVE 0 TO WS-EN-LOS-HIT
           MOVE 0 TO WS-EN-LOS-STEPS
           PERFORM UNTIL WS-EN-LOS-HIT = 1
               OR WS-EN-LOS-STEPS >=
                   WS-EN-LOS-MAX-STEPS
      *>       Step to next cell boundary
               IF WS-EN-LOS-SIDE-X <
                   WS-EN-LOS-SIDE-Y
                   ADD WS-EN-LOS-DELTA-X
                       TO WS-EN-LOS-SIDE-X
                   ADD WS-EN-LOS-STEP-X
                       TO WS-EN-LOS-MAP-X
               ELSE
                   ADD WS-EN-LOS-DELTA-Y
                       TO WS-EN-LOS-SIDE-Y
                   ADD WS-EN-LOS-STEP-Y
                       TO WS-EN-LOS-MAP-Y
               END-IF
               ADD 1 TO WS-EN-LOS-STEPS

      *>       Check if we reached player cell
               IF WS-EN-LOS-MAP-X =
                   WS-EN-LOS-TARGET-MX
                   AND WS-EN-LOS-MAP-Y =
                       WS-EN-LOS-TARGET-MY
      *>           Reached player with no wall hit
                   MOVE 1 TO
                       WS-EN-LOS-FLAG(WS-EN-IDX)
                   MOVE 1 TO WS-EN-LOS-HIT
               ELSE
      *>           Bounds check
                   IF WS-EN-LOS-MAP-X >= 1
                       AND WS-EN-LOS-MAP-X
                           <= WS-MAP-SIZE
                       AND WS-EN-LOS-MAP-Y >= 1
                       AND WS-EN-LOS-MAP-Y
                           <= WS-MAP-SIZE
      *>               Check for wall (MC-TYPE > 0)
                       IF MC-TYPE(WS-EN-LOS-MAP-Y,
                           WS-EN-LOS-MAP-X) > 0
      *>                   Hit a wall -- LOS blocked
                           MOVE 1 TO WS-EN-LOS-HIT
      *>                   LOS-FLAG stays 0
                       END-IF
                   ELSE
      *>               Out of bounds -- blocked
                       MOVE 1 TO WS-EN-LOS-HIT
                   END-IF
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> EN-MOVE-TOWARD-PLAYER: Move enemy WS-EN-IDX toward player.
      *> Checks wall collision before updating position.
      *> ============================================================
       EN-MOVE-TOWARD-PLAYER.
      *>   Compute direction to player
           IF WS-EN-DIST < 0.01
      *>       Already on top of player, skip
               EXIT PARAGRAPH
           END-IF

      *>   Normalized movement vector scaled by speed
           COMPUTE WS-EN-MOVE-DX =
               WS-EN-DX / WS-EN-DIST
               * WS-EN-MOVE-SPEED(WS-EN-IDX)
           COMPUTE WS-EN-MOVE-DY =
               WS-EN-DY / WS-EN-DIST
               * WS-EN-MOVE-SPEED(WS-EN-IDX)

      *>   Try X movement first
           COMPUTE WS-EN-NEW-X =
               WS-SP-WORLD-X(WS-EN-IDX)
               + WS-EN-MOVE-DX
           COMPUTE WS-EN-NEW-Y =
               WS-SP-WORLD-Y(WS-EN-IDX)

      *>   Check X collision with padding
           IF WS-EN-MOVE-DX > 0
               COMPUTE WS-EN-CHK-X =
                   FUNCTION INTEGER-PART(
                       WS-EN-NEW-X
                       + WS-EN-COLLISION-PAD) + 1
           ELSE
               COMPUTE WS-EN-CHK-X =
                   FUNCTION INTEGER-PART(
                       WS-EN-NEW-X
                       - WS-EN-COLLISION-PAD) + 1
           END-IF
           COMPUTE WS-EN-CHK-Y =
               FUNCTION INTEGER-PART(
                   WS-EN-NEW-Y) + 1

      *>   Bounds check and wall check for X
           IF WS-EN-CHK-X >= 1
               AND WS-EN-CHK-X <= WS-MAP-SIZE
               AND WS-EN-CHK-Y >= 1
               AND WS-EN-CHK-Y <= WS-MAP-SIZE
               IF MC-TYPE(WS-EN-CHK-Y,
                   WS-EN-CHK-X) = 0
      *>           No wall -- accept X move
                   MOVE WS-EN-NEW-X TO
                       WS-SP-WORLD-X(WS-EN-IDX)
               END-IF
           END-IF

      *>   Try Y movement
           COMPUTE WS-EN-NEW-X =
               WS-SP-WORLD-X(WS-EN-IDX)
           COMPUTE WS-EN-NEW-Y =
               WS-SP-WORLD-Y(WS-EN-IDX)
               + WS-EN-MOVE-DY

      *>   Check Y collision with padding
           COMPUTE WS-EN-CHK-X =
               FUNCTION INTEGER-PART(
                   WS-EN-NEW-X) + 1
           IF WS-EN-MOVE-DY > 0
               COMPUTE WS-EN-CHK-Y =
                   FUNCTION INTEGER-PART(
                       WS-EN-NEW-Y
                       + WS-EN-COLLISION-PAD) + 1
           ELSE
               COMPUTE WS-EN-CHK-Y =
                   FUNCTION INTEGER-PART(
                       WS-EN-NEW-Y
                       - WS-EN-COLLISION-PAD) + 1
           END-IF

      *>   Bounds check and wall check for Y
           IF WS-EN-CHK-X >= 1
               AND WS-EN-CHK-X <= WS-MAP-SIZE
               AND WS-EN-CHK-Y >= 1
               AND WS-EN-CHK-Y <= WS-MAP-SIZE
               IF MC-TYPE(WS-EN-CHK-Y,
                   WS-EN-CHK-X) = 0
      *>           No wall -- accept Y move
                   MOVE WS-EN-NEW-Y TO
                       WS-SP-WORLD-Y(WS-EN-IDX)
               END-IF
           END-IF
           .

      *> ============================================================
      *> EN-UPDATE-SPRITE-NAME: Set WS-SP-LUMP-NAME for current
      *> enemy based on AI state. Uses sprite type map for base name.
      *> ============================================================
       EN-UPDATE-SPRITE-NAME.
      *>   Find base name from type map
           MOVE SPACES TO WS-EN-BASE-NAME
           PERFORM VARYING WS-EN-WALK-FRAME
               FROM 1 BY 1
               UNTIL WS-EN-WALK-FRAME > WS-SP-MAP-COUNT
               IF WS-SP-THING-TYPE(WS-EN-IDX) =
                   WS-SP-TM-TYPE(WS-EN-WALK-FRAME)
                   MOVE WS-SP-TM-BASE(WS-EN-WALK-FRAME)
                       TO WS-EN-BASE-NAME
               END-IF
           END-PERFORM

      *>   If no base name found, skip
           IF WS-EN-BASE-NAME = SPACES
               EXIT PARAGRAPH
           END-IF

      *>   Build lump name based on AI state
           MOVE SPACES TO WS-EN-LUMP-TEMP

           EVALUATE WS-EN-STATE(WS-EN-IDX)
      *>       IDLE (0): alternate A0/B0 every 16 ticks
               WHEN 0
                   IF FUNCTION MOD(
                       WS-EN-FRAME-CTR(WS-EN-IDX),
                       16) < 8
                       STRING WS-EN-BASE-NAME "A0  "
                           DELIMITED BY SIZE
                           INTO WS-EN-LUMP-TEMP
                   ELSE
                       STRING WS-EN-BASE-NAME "B0  "
                           DELIMITED BY SIZE
                           INTO WS-EN-LUMP-TEMP
                   END-IF

      *>       ALERT (1): show alert frame B
               WHEN 1
                   STRING WS-EN-BASE-NAME "B0  "
                       DELIMITED BY SIZE
                       INTO WS-EN-LUMP-TEMP

      *>       CHASE (2): alternate A0/B0 every 8 ticks
               WHEN 2
                   IF FUNCTION MOD(
                       WS-EN-FRAME-CTR(WS-EN-IDX),
                       16) < 8
                       STRING WS-EN-BASE-NAME "A0  "
                           DELIMITED BY SIZE
                           INTO WS-EN-LUMP-TEMP
                   ELSE
                       STRING WS-EN-BASE-NAME "B0  "
                           DELIMITED BY SIZE
                           INTO WS-EN-LUMP-TEMP
                   END-IF

      *>       ATTACK (3): attack frame E0
               WHEN 3
                   STRING WS-EN-BASE-NAME "E0  "
                       DELIMITED BY SIZE
                       INTO WS-EN-LUMP-TEMP

      *>       PAIN (4): pain frame G0
               WHEN 4
                   STRING WS-EN-BASE-NAME "G0  "
                       DELIMITED BY SIZE
                       INTO WS-EN-LUMP-TEMP

      *>       DYING (5): death frames H0 through L0
      *>       5 frames, 5 ticks each
               WHEN 5
                   COMPUTE WS-EN-DEATH-FRAME =
                       WS-EN-FRAME-CTR(WS-EN-IDX) / 5
                   IF WS-EN-DEATH-FRAME > 4
                       MOVE 4 TO WS-EN-DEATH-FRAME
                   END-IF
                   EVALUATE WS-EN-DEATH-FRAME
                       WHEN 0
                           STRING WS-EN-BASE-NAME
                               "H0  "
                               DELIMITED BY SIZE
                               INTO WS-EN-LUMP-TEMP
                       WHEN 1
                           STRING WS-EN-BASE-NAME
                               "I0  "
                               DELIMITED BY SIZE
                               INTO WS-EN-LUMP-TEMP
                       WHEN 2
                           STRING WS-EN-BASE-NAME
                               "J0  "
                               DELIMITED BY SIZE
                               INTO WS-EN-LUMP-TEMP
                       WHEN 3
                           STRING WS-EN-BASE-NAME
                               "K0  "
                               DELIMITED BY SIZE
                               INTO WS-EN-LUMP-TEMP
                       WHEN 4
                           STRING WS-EN-BASE-NAME
                               "L0  "
                               DELIMITED BY SIZE
                               INTO WS-EN-LUMP-TEMP
                   END-EVALUATE

      *>       DEAD (6): keep last death frame
               WHEN 6
                   STRING WS-EN-BASE-NAME "L0  "
                       DELIMITED BY SIZE
                       INTO WS-EN-LUMP-TEMP
           END-EVALUATE

           MOVE WS-EN-LUMP-TEMP TO
               WS-SP-LUMP-NAME(WS-EN-IDX)
           .

      *> ============================================================
      *> EN-APPLY-DAMAGE: Apply damage to an enemy.
      *> Input: WS-EN-DMG-IDX (sprite index 1..50)
      *>        WS-EN-DMG-AMT (damage amount)
      *> Subtracts from WS-SP-HEALTH. If <= 0, set DYING.
      *> Otherwise set PAIN.
      *> ============================================================
       EN-APPLY-DAMAGE.
      *>   Validate index
           IF WS-EN-DMG-IDX < 1
               OR WS-EN-DMG-IDX > WS-SP-COUNT
               EXIT PARAGRAPH
           END-IF
      *>   Skip if already dead or dying
           IF WS-SP-STATE(WS-EN-DMG-IDX) = 2
               EXIT PARAGRAPH
           END-IF
           IF WS-EN-STATE(WS-EN-DMG-IDX) = 5
               OR WS-EN-STATE(WS-EN-DMG-IDX) = 6
               EXIT PARAGRAPH
           END-IF

      *>   Subtract damage from health
           IF WS-SP-HEALTH(WS-EN-DMG-IDX) >
               WS-EN-DMG-AMT
               SUBTRACT WS-EN-DMG-AMT FROM
                   WS-SP-HEALTH(WS-EN-DMG-IDX)
           ELSE
               MOVE 0 TO
                   WS-SP-HEALTH(WS-EN-DMG-IDX)
           END-IF

      *>   Check if enemy is dead
           IF WS-SP-HEALTH(WS-EN-DMG-IDX) = 0
      *>       Start dying animation
               MOVE 5 TO
                   WS-EN-STATE(WS-EN-DMG-IDX)
               MOVE 0 TO
                   WS-EN-FRAME-CTR(WS-EN-DMG-IDX)
      *>       Set sprite state to dying (1)
               MOVE 1 TO
                   WS-SP-STATE(WS-EN-DMG-IDX)
           ELSE
      *>       Pain reaction
               MOVE 4 TO
                   WS-EN-STATE(WS-EN-DMG-IDX)
               MOVE 0 TO
                   WS-EN-FRAME-CTR(WS-EN-DMG-IDX)
           END-IF
           .

      *> ============================================================
      *> EN-RANDOM-3-15: Generate pseudo-random number 3..15.
      *> Uses linear congruential generator. Result in WS-EN-RAND-VAL.
      *> ============================================================
       EN-RANDOM-3-15.
      *>   LCG: seed = seed * 1103515245 + 12345
      *>   We use a simpler multiply to stay within PIC 9(9)
           COMPUTE WS-EN-RAND-SEED =
               FUNCTION MOD(
                   WS-EN-RAND-SEED * 16807 + 1,
                   999999937)
      *>   Map to range 3..15 (13 values)
           COMPUTE WS-EN-RAND-VAL =
               FUNCTION MOD(WS-EN-RAND-SEED, 13) + 3
           .

      *> ============================================================
      *> EN-INIT-AI: Initialize AI state table after sprites loaded.
      *> Called once after INIT-SPRITES. Sets per-enemy-type stats.
      *> ============================================================
       EN-INIT-AI.
           IF WS-SP-COUNT < 1
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-EN-IDX FROM 1 BY 1
               UNTIL WS-EN-IDX > WS-SP-COUNT
      *>       Start all enemies in IDLE state
               MOVE 0 TO WS-EN-STATE(WS-EN-IDX)
               MOVE 0 TO WS-EN-FRAME-CTR(WS-EN-IDX)
               MOVE 0 TO WS-EN-COOLDOWN(WS-EN-IDX)
               MOVE 0 TO WS-EN-LOS-FLAG(WS-EN-IDX)

      *>       Set per-type stats based on thing type
               EVALUATE WS-SP-THING-TYPE(WS-EN-IDX)
      *>           Zombieman (3004): low damage, slow
                   WHEN 03004
                       MOVE +00015.0000 TO
                           WS-EN-ALERT-DIST(WS-EN-IDX)
                       MOVE +00003.0000 TO
                           WS-EN-ATTACK-DIST(WS-EN-IDX)
                       MOVE 010 TO
                           WS-EN-DAMAGE(WS-EN-IDX)
                       MOVE +000.0400 TO
                           WS-EN-MOVE-SPEED(WS-EN-IDX)

      *>           Imp (3001): medium damage, fireball range
                   WHEN 03001
                       MOVE +00020.0000 TO
                           WS-EN-ALERT-DIST(WS-EN-IDX)
                       MOVE +00005.0000 TO
                           WS-EN-ATTACK-DIST(WS-EN-IDX)
                       MOVE 015 TO
                           WS-EN-DAMAGE(WS-EN-IDX)
                       MOVE +000.0500 TO
                           WS-EN-MOVE-SPEED(WS-EN-IDX)

      *>           Demon (3002): high damage, melee only, fast
                   WHEN 03002
                       MOVE +00015.0000 TO
                           WS-EN-ALERT-DIST(WS-EN-IDX)
                       MOVE +00002.0000 TO
                           WS-EN-ATTACK-DIST(WS-EN-IDX)
                       MOVE 025 TO
                           WS-EN-DAMAGE(WS-EN-IDX)
                       MOVE +000.0800 TO
                           WS-EN-MOVE-SPEED(WS-EN-IDX)

      *>           Shotgun guy (9): burst damage, slow
                   WHEN 00009
                       MOVE +00015.0000 TO
                           WS-EN-ALERT-DIST(WS-EN-IDX)
                       MOVE +00004.0000 TO
                           WS-EN-ATTACK-DIST(WS-EN-IDX)
                       MOVE 020 TO
                           WS-EN-DAMAGE(WS-EN-IDX)
                       MOVE +000.0400 TO
                           WS-EN-MOVE-SPEED(WS-EN-IDX)

      *>           Chaingunner (65): rapid damage, medium
                   WHEN 00065
                       MOVE +00020.0000 TO
                           WS-EN-ALERT-DIST(WS-EN-IDX)
                       MOVE +00005.0000 TO
                           WS-EN-ATTACK-DIST(WS-EN-IDX)
                       MOVE 012 TO
                           WS-EN-DAMAGE(WS-EN-IDX)
                       MOVE +000.0500 TO
                           WS-EN-MOVE-SPEED(WS-EN-IDX)

      *>           Unknown type: use defaults
                   WHEN OTHER
                       MOVE +00015.0000 TO
                           WS-EN-ALERT-DIST(WS-EN-IDX)
                       MOVE +00003.0000 TO
                           WS-EN-ATTACK-DIST(WS-EN-IDX)
                       MOVE 010 TO
                           WS-EN-DAMAGE(WS-EN-IDX)
                       MOVE +000.0500 TO
                           WS-EN-MOVE-SPEED(WS-EN-IDX)
               END-EVALUATE
           END-PERFORM
           .
