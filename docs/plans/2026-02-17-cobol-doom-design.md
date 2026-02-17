# COBOL Doom 2 — Console Raycasting Game Design

**Date**: 2026-02-17
**Status**: Approved

## Overview

A pseudo-3D first-person shooter written in pure GnuCOBOL. ASCII raycasting with ANSI color codes, one level, walk + shoot gameplay, chase-AI enemies. Runs in a 120x40 terminal.

## Architecture

- **Single source file**: `doom.cob`
- **Pure GnuCOBOL**: no C helpers, no external renderers
- **Fixed-point math**: COBOL's `PIC S9(n)V9(4)` for all position/distance calculations
- **Trig lookup tables**: Pre-computed sin/cos for 360 degrees, stored in WORKING-STORAGE
- **Keypress-driven**: each keypress = one game tick (not real-time FPS)

### Program Structure

```
IDENTIFICATION DIVISION.
ENVIRONMENT DIVISION.
DATA DIVISION.
  WORKING-STORAGE SECTION.
    - Trig lookup tables (sin/cos x 360)
    - Map grid (16x16)
    - Player state (position, angle, health, ammo)
    - Enemy table (up to 10)
    - Frame buffer (120x40)
    - ANSI color code constants
    - Raycasting work variables
PROCEDURE DIVISION.
    PERFORM INITIALIZE-GAME
    PERFORM GAME-LOOP UNTIL WS-GAME-OVER = 1
    PERFORM SHOW-END-SCREEN
    STOP RUN.
```

## Raycasting Engine

**Algorithm**: DDA (Digital Differential Analyzer)

1. Player has position `(PX, PY)` and angle `PA` (0-359)
2. Field of view = 60 degrees
3. For each screen column `C` (0-119):
   - Ray angle = `PA - 30 + C * 60/120`
   - Step along ray direction using DDA until wall cell hit
   - Calculate distance, apply fisheye correction: `DIST = DIST * COS(ray - player angle)`
   - Wall height = `SCREEN-H / DIST` (capped at 40)
4. Fill column: ceiling chars, wall chars (shaded by distance), floor chars

**Trig tables**:
- `WS-SIN-TABLE`: 360 entries, `PIC S9(1)V9(4)`
- `WS-COS-TABLE`: same format
- Hardcoded values generated at code-write time

**Wall shading by distance**:

| Distance | Character |
|----------|-----------|
| < 3      | `@`       |
| 3-5      | `#`       |
| 5-8      | `=`       |
| 8-12     | `-`       |
| > 12     | `.`       |

## ANSI Color Scheme

| Element      | Color         | ANSI Code |
|--------------|---------------|-----------|
| Ceiling      | Dark blue     | `[34m`    |
| Walls close  | White/bright  | `[97m`    |
| Walls far    | Gray          | `[90m`    |
| Floor        | Dark yellow   | `[33m`    |
| Enemies      | Red           | `[31m`    |
| HUD text     | Green         | `[32m`    |
| Crosshair    | Yellow        | `[93m`    |
| Exit door    | Bright green  | `[92m`    |

Color codes embedded inline per-character in output strings.

## Map

16x16 hardcoded grid in WORKING-STORAGE.

Cell values:
- `0` = empty floor
- `1` = wall
- `2` = enemy spawn point (becomes floor after spawn)
- `9` = exit

Sample level:
```
1111111111111111
1000000100000001
1010100100101001
1010100000101001
1000100100001001
1110100111101001
1000100000001001
1011110101111001
1000000100000001
1010111100011101
1010000200010001
1010101111010101
1000100000010101
1011100101000101
1000000100000091
1111111111111111
```

Player starts at (1.5, 1.5), facing east (angle 0).

## Player State

```cobol
01 WS-PLAYER.
   05 WS-PX        PIC S9(3)V9(4).   *> X position
   05 WS-PY        PIC S9(3)V9(4).   *> Y position
   05 WS-PA        PIC 9(3).          *> Angle 0-359
   05 WS-HEALTH    PIC 9(3).          *> 0-100
   05 WS-AMMO      PIC 9(3).          *> shots remaining
```

Starting values: health=100, ammo=25.

## Enemies

Up to 10 enemies with chase AI.

```cobol
01 WS-ENEMIES.
   05 WS-ENEMY OCCURS 10 TIMES.
      10 WS-EX      PIC S9(3)V9(4).
      10 WS-EY      PIC S9(3)V9(4).
      10 WS-EALIVE  PIC 9.            *> 1=alive, 0=dead
      10 WS-EHEALTH PIC 9(2).         *> HP (10 = one shot kill)
      10 WS-ESPEED  PIC 9V9(2).       *> 0.50 cells/turn
```

**Chase AI** (per turn, per alive enemy):
1. Calculate `DX = PX - EX`, `DY = PY - EY`
2. Move along axis with larger absolute difference
3. Step size = `WS-ESPEED` (0.50)
4. Don't move into walls or other enemies
5. If within 1.5 cells of player: attack (deal 5 damage)

## Input & Controls

Using GnuCOBOL `ACCEPT`:

| Key   | Action |
|-------|--------|
| `W`   | Move forward (1 cell step in facing direction) |
| `S`   | Move backward |
| `A`   | Rotate left 10 degrees |
| `D`   | Rotate right 10 degrees |
| Space | Shoot (hitscan along facing direction) |
| `Q`   | Quit game |

## Game Loop

```
GAME-LOOP:
  ACCEPT WS-KEY                   (blocks until keypress)
  PERFORM PROCESS-INPUT           (move/rotate/shoot)
  PERFORM MOVE-ENEMIES            (chase AI for each alive enemy)
  PERFORM CHECK-ENEMY-ATTACKS     (enemies near player deal damage)
  PERFORM CHECK-WIN-CONDITION     (player on exit cell?)
  PERFORM CHECK-DEATH             (health <= 0?)
  PERFORM CAST-ALL-RAYS           (raycasting for 120 columns)
  PERFORM RENDER-ENEMIES          (overlay enemy sprites on frame)
  PERFORM RENDER-HUD              (health, ammo, kills at bottom)
  PERFORM DRAW-FRAME              (output frame buffer to terminal)
```

## Rendering

**Frame buffer**: 120x40 character array.

**Output method**: ANSI escape `ESC[H` (cursor home) + row-by-row display. No screen clear to avoid flicker.

**Enemy rendering**: Calculate angle from player to enemy. If within FOV, project to screen column. Draw `M` character in red at that column if closer than the wall.

**HUD** (bottom row):
```
HP: 100 | AMMO: 25 | KILLS: 3/5 | [+]
```

**Crosshair**: `+` at screen center (column 60, row 20) in yellow.

## Win/Lose

**Win**: Player steps on exit cell (value 9).
**Lose**: Health reaches 0.

Both display an end screen with kill count, remaining health, and option to quit.

## What's NOT in This Version

- Multiple levels
- Different weapon types
- Sound
- Save/load
- Difficulty settings
- Multiplayer
- Texture mapping
