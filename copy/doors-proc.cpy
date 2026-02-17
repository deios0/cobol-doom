      *> ============================================================
      *> doors-proc.cpy — Door mechanics and item pickup procedures
      *> Handles door opening/closing animation, use-key interaction,
      *> and item pickup detection with effect application.
      *> ============================================================

      *> ============================================================
      *> INIT-DOORS: Scan the map grid for MC-TYPE = 2 (door) cells
      *> and register each one in the door table as closed.
      *> ============================================================
       INIT-DOORS.
           MOVE 0 TO WS-DR-COUNT

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-MAP-SIZE
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > WS-MAP-SIZE
                   IF MC-TYPE(WS-I, WS-J) = 2
                       IF WS-DR-COUNT < 100
                           ADD 1 TO WS-DR-COUNT
                           MOVE WS-I
                               TO WS-DR-ROW(WS-DR-COUNT)
                           MOVE WS-J
                               TO WS-DR-COL(WS-DR-COUNT)
                           MOVE 0
                               TO WS-DR-STATE(WS-DR-COUNT)
                           MOVE 0
                               TO WS-DR-TIMER(WS-DR-COUNT)
                           MOVE 90
                               TO WS-DR-OPEN-DUR(WS-DR-COUNT)
                           MOVE 4
                               TO WS-DR-SPEED(WS-DR-COUNT)
                           MOVE 0
                               TO WS-DR-OFFSET(WS-DR-COUNT)
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM

           DISPLAY "Doors initialized: " WS-DR-COUNT
           .

      *> ============================================================
      *> UPDATE-DOORS: Animate all tracked doors each frame.
      *>  - opening: increase offset until 64 (fully open)
      *>  - open: count down timer, then start closing
      *>  - closing: decrease offset until 0 (fully closed)
      *> ============================================================
       UPDATE-DOORS.
           PERFORM VARYING WS-DR-IDX FROM 1 BY 1
               UNTIL WS-DR-IDX > WS-DR-COUNT
               EVALUATE WS-DR-STATE(WS-DR-IDX)

      *>           --- OPENING ---
                   WHEN 1
                       ADD WS-DR-SPEED(WS-DR-IDX)
                           TO WS-DR-OFFSET(WS-DR-IDX)
                       IF WS-DR-OFFSET(WS-DR-IDX) >= 64
                           MOVE 64
                               TO WS-DR-OFFSET(WS-DR-IDX)
                           MOVE 2
                               TO WS-DR-STATE(WS-DR-IDX)
                           MOVE WS-DR-OPEN-DUR(WS-DR-IDX)
                               TO WS-DR-TIMER(WS-DR-IDX)
      *>                   Door fully open: make passable
                           MOVE 0 TO MC-TYPE(
                               WS-DR-ROW(WS-DR-IDX),
                               WS-DR-COL(WS-DR-IDX))
                       END-IF

      *>           --- OPEN (waiting) ---
                   WHEN 2
                       IF WS-DR-TIMER(WS-DR-IDX) > 0
                           SUBTRACT 1
                               FROM WS-DR-TIMER(WS-DR-IDX)
                       ELSE
                           MOVE 3
                               TO WS-DR-STATE(WS-DR-IDX)
      *>                   Start closing: make blocking again
                           MOVE 2 TO MC-TYPE(
                               WS-DR-ROW(WS-DR-IDX),
                               WS-DR-COL(WS-DR-IDX))
                       END-IF

      *>           --- CLOSING ---
                   WHEN 3
                       IF WS-DR-OFFSET(WS-DR-IDX)
                           > WS-DR-SPEED(WS-DR-IDX)
                           SUBTRACT WS-DR-SPEED(WS-DR-IDX)
                               FROM WS-DR-OFFSET(WS-DR-IDX)
                       ELSE
                           MOVE 0
                               TO WS-DR-OFFSET(WS-DR-IDX)
                           MOVE 0
                               TO WS-DR-STATE(WS-DR-IDX)
      *>                   Fully closed: ensure blocking
                           MOVE 2 TO MC-TYPE(
                               WS-DR-ROW(WS-DR-IDX),
                               WS-DR-COL(WS-DR-IDX))
                       END-IF

               END-EVALUATE
           END-PERFORM
           .

      *> ============================================================
      *> CHECK-USE-KEY: When player presses Space (WS-KEY(6)) or
      *> E (WS-KEY(7)), find the cell the player is facing and
      *> activate any closed door there.
      *> facing_x = int(px + cos(pa) * 1.5) + 1
      *> facing_y = int(py + sin(pa) * 1.5) + 1
      *> ============================================================
       CHECK-USE-KEY.
      *>   Only proceed if Space or E is pressed
           IF WS-KEY(6) = 0 AND WS-KEY(7) = 0
               EXIT PARAGRAPH
           END-IF

      *>   Compute facing cell using player angle
           COMPUTE WS-DR-ANGLE-LOOKUP =
               FUNCTION MOD(WS-PA * 10 + 36000, 3600) + 1

           COMPUTE WS-DR-FACE-X =
               WS-PX + WS-COS-VAL(WS-DR-ANGLE-LOOKUP)
               * 1.5
           COMPUTE WS-DR-FACE-Y =
               WS-PY + WS-SIN-VAL(WS-DR-ANGLE-LOOKUP)
               * 1.5

           COMPUTE WS-DR-FACE-COL =
               FUNCTION INTEGER-PART(WS-DR-FACE-X) + 1
           COMPUTE WS-DR-FACE-ROW =
               FUNCTION INTEGER-PART(WS-DR-FACE-Y) + 1

      *>   Bounds check
           IF WS-DR-FACE-ROW < 1
               OR WS-DR-FACE-ROW > WS-MAP-SIZE
               OR WS-DR-FACE-COL < 1
               OR WS-DR-FACE-COL > WS-MAP-SIZE
               EXIT PARAGRAPH
           END-IF

      *>   Check if the facing cell is a door (MC-TYPE = 2)
           IF MC-TYPE(WS-DR-FACE-ROW, WS-DR-FACE-COL)
               NOT = 2
               EXIT PARAGRAPH
           END-IF

      *>   Find this door in the door table
           MOVE 0 TO WS-DR-FOUND
           PERFORM VARYING WS-DR-SEARCH-IDX FROM 1 BY 1
               UNTIL WS-DR-SEARCH-IDX > WS-DR-COUNT
               OR WS-DR-FOUND = 1
               IF WS-DR-ROW(WS-DR-SEARCH-IDX)
                   = WS-DR-FACE-ROW
                   AND WS-DR-COL(WS-DR-SEARCH-IDX)
                   = WS-DR-FACE-COL
                   MOVE 1 TO WS-DR-FOUND
      *>           Only open if currently closed
                   IF WS-DR-STATE(WS-DR-SEARCH-IDX)
                       = 0
                       MOVE 1
                           TO WS-DR-STATE(
                               WS-DR-SEARCH-IDX)
                       DISPLAY "Door opening at row="
                           WS-DR-FACE-ROW
                           " col=" WS-DR-FACE-COL
                   END-IF
               END-IF
           END-PERFORM
           .

      *> ============================================================
      *> INIT-PICKUPS: Scan WS-MAP-THINGS for pickup item types
      *> and register each one in the pickup table.
      *> Converts WAD map coords to grid coords:
      *>   grid_x = (map_x - WS-MAP-MIN-X) / WS-MAP-SCALE + 2
      *> ============================================================
       INIT-PICKUPS.
           MOVE 0 TO WS-PK-COUNT

           PERFORM VARYING WS-PK-THING-IDX FROM 1 BY 1
               UNTIL WS-PK-THING-IDX > WS-MAP-THING-COUNT

      *>       Check if this thing is a pickup type
               IF MT-TYPE(WS-PK-THING-IDX) = 2007
                   OR MT-TYPE(WS-PK-THING-IDX) = 2008
                   OR MT-TYPE(WS-PK-THING-IDX) = 2012
                   OR MT-TYPE(WS-PK-THING-IDX) = 2014
                   OR MT-TYPE(WS-PK-THING-IDX) = 2015
                   OR MT-TYPE(WS-PK-THING-IDX) = 2001
                   OR MT-TYPE(WS-PK-THING-IDX) = 2002

                   IF WS-PK-COUNT < 100
                       ADD 1 TO WS-PK-COUNT

      *>               Convert map coordinates to grid coords
                       COMPUTE WS-PK-GRID-X =
                           (MT-X(WS-PK-THING-IDX)
                           - WS-MAP-MIN-X)
                           / WS-MAP-SCALE + 2
                       COMPUTE WS-PK-GRID-Y =
                           (MT-Y(WS-PK-THING-IDX)
                           - WS-MAP-MIN-Y)
                           / WS-MAP-SCALE + 2

                       MOVE WS-PK-GRID-X
                           TO WS-PK-X(WS-PK-COUNT)
                       MOVE WS-PK-GRID-Y
                           TO WS-PK-Y(WS-PK-COUNT)
                       MOVE MT-TYPE(WS-PK-THING-IDX)
                           TO WS-PK-TYPE(WS-PK-COUNT)
                       MOVE 1
                           TO WS-PK-ACTIVE(WS-PK-COUNT)
                   END-IF
               END-IF
           END-PERFORM

           DISPLAY "Pickups initialized: " WS-PK-COUNT
           .

      *> ============================================================
      *> CHECK-PICKUPS: For each active pickup, check distance to
      *> player. If within pickup radius, apply effect and deactivate.
      *>   2007 = clip (ammo +10)
      *>   2008 = shells (ammo +4)
      *>   2012 = medikit (health +25, max 100)
      *>   2014 = health bonus (health +1, max 200)
      *>   2015 = armor bonus (+1, ignored — no armor var)
      *>   2001 = shotgun (ammo +8)
      *>   2002 = chaingun (ammo +20)
      *> ============================================================
       CHECK-PICKUPS.
           PERFORM VARYING WS-PK-IDX FROM 1 BY 1
               UNTIL WS-PK-IDX > WS-PK-COUNT

               IF WS-PK-ACTIVE(WS-PK-IDX) = 1

      *>           Compute distance to player (Manhattan approx
      *>           with max component for speed)
                   COMPUTE WS-PK-DIST-X =
                       FUNCTION ABS(
                       WS-PX - WS-PK-X(WS-PK-IDX))
                   COMPUTE WS-PK-DIST-Y =
                       FUNCTION ABS(
                       WS-PY - WS-PK-Y(WS-PK-IDX))

      *>           Use max of dx, dy as quick radius check
                   IF WS-PK-DIST-X > WS-PK-DIST-Y
                       MOVE WS-PK-DIST-X TO WS-PK-DIST
                   ELSE
                       MOVE WS-PK-DIST-Y TO WS-PK-DIST
                   END-IF

      *>           Within pickup radius?
                   IF WS-PK-DIST <= WS-PK-RADIUS

      *>               Apply effect based on type
                       EVALUATE WS-PK-TYPE(WS-PK-IDX)

      *>                   Clip: ammo +10
                           WHEN 02007
                               ADD 10 TO WS-AMMO
                               IF WS-AMMO > 999
                                   MOVE 999 TO WS-AMMO
                               END-IF
                               DISPLAY "Picked up a clip."

      *>                   Shells: ammo +4
                           WHEN 02008
                               ADD 4 TO WS-AMMO
                               IF WS-AMMO > 999
                                   MOVE 999 TO WS-AMMO
                               END-IF
                               DISPLAY "Picked up shells."

      *>                   Medikit: health +25, max 100
                           WHEN 02012
                               ADD 25 TO WS-HEALTH
                               IF WS-HEALTH > 100
                                   MOVE 100 TO WS-HEALTH
                               END-IF
                               DISPLAY
                                   "Picked up a medikit."

      *>                   Health bonus: health +1, max 200
                           WHEN 02014
                               ADD 1 TO WS-HEALTH
                               IF WS-HEALTH > 200
                                   MOVE 200 TO WS-HEALTH
                               END-IF
                               DISPLAY
                                 "Picked up a health "
                                 "bonus."

      *>                   Armor bonus: +1 (no armor var,
      *>                   just display)
                           WHEN 02015
                               DISPLAY
                                 "Picked up an armor "
                                 "bonus."

      *>                   Shotgun: ammo +8
                           WHEN 02001
                               ADD 8 TO WS-AMMO
                               IF WS-AMMO > 999
                                   MOVE 999 TO WS-AMMO
                               END-IF
                               DISPLAY
                                 "Picked up a shotgun!"

      *>                   Chaingun: ammo +20
                           WHEN 02002
                               ADD 20 TO WS-AMMO
                               IF WS-AMMO > 999
                                   MOVE 999 TO WS-AMMO
                               END-IF
                               DISPLAY
                                 "Picked up a "
                                 "chaingun!"

                       END-EVALUATE

      *>               Deactivate the pickup
                       MOVE 0
                           TO WS-PK-ACTIVE(WS-PK-IDX)

                   END-IF
               END-IF
           END-PERFORM
           .
