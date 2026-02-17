# COBOL Doom 2 — SDL2 FFI Design

Date: 2026-02-17
Status: Approved
Decision: SDL2 via FFI with >95% COBOL (debate result: Gemini + Claude consensus)

## Goal

Build a playable first level (MAP01) of Doom 2 in GnuCOBOL with pixel-perfect visuals.
Assets from Freedoom WAD (compatible with DOOM2.WAD). C shim <2% of codebase.

## Architecture

```
doom2.cob  (~5500+ lines COBOL)  <- 98%+ of code
  |- WAD parser      reads Freedoom/DOOM2 WAD, extracts textures/sprites/maps
  |- Renderer         textured raycasting + floor/ceiling + sprites
  |- Game logic       enemy AI, shooting, collisions, doors
  |- HUD              pixel HUD: health, ammo, Doomguy face

display.c  (~60 lines C)  <- <2% of code
  |- sdl_init()       SDL_Init + CreateWindow + CreateTexture
  |- sdl_frame()      SDL_UpdateTexture + RenderPresent
  |- sdl_input()      SDL_PollEvent -> returns key states
  |- sdl_quit()       cleanup
```

Key principle: COBOL fills a `PIC X(256000)` framebuffer (320x200x4 RGBA).
C shim only displays this buffer via SDL2. All logic, all raycasting, all textures — in COBOL.

## WAD Parser

COBOL reads WAD at startup:
- PLAYPAL: 256 RGB triplets -> `PIC X(768)`
- COLORMAP: 34 light tables -> `PIC X(8704)`
- Wall textures: patches + texture defs -> OCCURS array of 64x128 pixels
- Flats: 64x64 raw bytes -> OCCURS array of 4096 bytes each
- Sprites: column-based posts -> OCCURS array, variable size
- MAP01: THINGS, LINEDEFS, SIDEDEFS, VERTEXES, SECTORS -> COBOL tables

Binary I/O via GnuCOBOL: `READ INTO PIC X(n)`, `REDEFINES` with `COMP-5` for little-endian integers.

## Rendering Pipeline

Three layers drawn into framebuffer sequentially:

### 1. Textured Walls (DDA raycasting)
- 320 rays for 320 columns
- DDA finds wall + hit point -> U texture coordinate (0-63)
- Wall height on screen -> V coordinate per pixel (0-127)
- Texture lookup (palette index) -> colormap (distance lighting) -> RGB from PLAYPAL
- Write to framebuffer

### 2. Floor and Ceiling (horizontal casting)
- For each row below/above wall:
  - `dist = screen_h / (2 * row - screen_h)`
  - World coordinates -> flat texture U,V (MOD 64)
  - Flat lookup -> colormap -> RGB -> framebuffer

### 3. Sprites (billboard rendering)
- For each visible object: compute angle + distance from player
- Project to screen (column + size)
- Draw column by column with z-buffer check (depth buffer from walls)
- Transparent pixels (palette index 0) skipped

### Z-buffer
Array `PIC S9(5)V9(4) OCCURS 320 TIMES` — wall depth per column.

## Game Logic

### Map
MAP01 converted to 128x128 grid (each cell ~64 Doom units).
Built from LINEDEFS/SIDEDEFS/SECTORS. Cell = wall/empty + texture ID + floor/ceiling height.

### Enemies
| Type | Thing ID | HP | Attack |
|------|----------|-----|--------|
| Zombieman | 3004 | 20 | Hitscan 3-15 dmg |
| Shotgun Guy | 9 | 30 | Hitscan 3-15 x3 |

AI: IDLE -> ALERT -> CHASE -> ATTACK -> PAIN -> DEATH
Animation: 2-4 frames per state from WAD sprites.

### Weapons
| Weapon | Damage | Sprite |
|--------|--------|--------|
| Pistol | 5-15 | PISG |
| Shotgun | 5-15 x7 | SHTG |

Weapon sprite drawn as overlay on bottom half of screen.

### Doors
Door linedefs: press E/Space near door -> ceiling height animates open/close.

### Pickups
Ammo clip (2007), Shotgun (2001), Medikit (2012).
Collision by distance -> add ammo/health -> remove object.

### HUD
Bottom 320x32 panel: health, ammo, weapon, Doomguy face.
Number sprites from WAD (STTNUM0-9). Face sprites (STF*) change by HP.

## C Shim (display.c)

4 functions, ~60 lines. Zero logic. Zero rendering.
- `sdl_init(w, h)` — create window (3x scaled) + texture
- `sdl_frame(buf, w, h)` — UpdateTexture + RenderPresent
- `sdl_input(keys)` — PollEvent + GetKeyboardState -> key array
- `sdl_quit()` — cleanup

Build: `cobc -x -o doom2 doom2.cob display.c -lSDL2`

## File Structure

```
Doom-Cob/
  doom2.cob              Main file: main loop, game states
  copy/
    wad-parser.cpy       WAD reader
    wad-textures.cpy     Texture/flat/sprite extraction
    map-loader.cpy       MAP01 -> grid + enemies + pickups
    raycaster.cpy        DDA + textured walls
    floor-ceil.cpy       Horizontal casting
    sprites.cpy          Billboard rendering + z-buffer
    enemies.cpy          AI state machine
    weapons.cpy          Pistol/Shotgun animation + damage
    doors.cpy            Door open/close logic
    pickups.cpy          Item pickup logic
    hud.cpy              Pixel HUD rendering
  display.c              SDL2 shim
  Makefile
  freedoom2.wad          Freedoom Phase 2 WAD
```

## Build Order (incremental, each step = working build)

1. C shim + empty framebuffer -> SDL window with test pattern
2. WAD parser -> palette + textures load
3. Textured walls -> see walls with textures
4. Floor/ceiling -> full 3D scene
5. Map loader -> walk through MAP01
6. Sprites -> see enemies and decorations
7. Enemies AI + weapons -> shooting, combat
8. Doors + pickups -> interactivity
9. HUD -> full Doom interface
10. Polish -> lighting, animations

## Code Estimates

| Module | COBOL lines | Complexity |
|--------|------------|------------|
| WAD parser + textures | ~1200 | High |
| Map loader | ~400 | Medium |
| Raycaster (textured) | ~800 | High |
| Floor/ceiling | ~400 | Medium |
| Sprites + z-buffer | ~600 | High |
| Enemies AI | ~500 | Medium |
| Weapons + animation | ~300 | Medium |
| Doors | ~200 | Low |
| Pickups | ~150 | Low |
| HUD (pixel) | ~400 | Medium |
| Main loop + data defs | ~500 | Low |
| **Total COBOL** | **~5450** | |
| display.c | ~60 | Low |
| **COBOL %** | **~98.9%** | |
