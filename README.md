# COBOL Doom 2

A Doom 2 MAP01 renderer written in **GnuCOBOL** with SDL2 pixel display.

~8,300 lines of COBOL (99.1%) + 72 lines of C shim (0.9%).

## Features

- WAD parser: textures, flats, palette, colormap, MAP01 geometry
- Textured walls via DDA raycasting with perpendicular distance correction
- Horizontal floor/ceiling casting with scanline rendering
- Depth-sorted sprite rendering with view-space projection
- 7-state enemy AI (IDLE/ALERT/CHASE/ATTACK/PAIN/DYING/DEAD) with line-of-sight
- Hitscan weapons (pistol + shotgun) with crosshair
- Doors, pickups (health/ammo/armor), level exit
- Sector-based lighting combined with distance falloff
- 5x7 bitmap font HUD with health, ammo, armor display
- Title screen with "COBOL DOOM 2" rendered at 3x scale

## Requirements

- GnuCOBOL 3.2+ (`sudo apt install gnucobol`)
- SDL2 (`sudo apt install libsdl2-dev`)
- Freedoom WAD file (see below)

## WAD File

This game requires `freedoom2.wad` — a free, open-source Doom 2 IWAD.

**Download:** https://github.com/freedoom/freedoom/releases

Get the latest `freedoom-0.13.0.zip` (or newer), extract `freedoom2.wad` and place it in this directory.

Or via command line:
```bash
wget https://github.com/freedoom/freedoom/releases/download/v0.13.0/freedoom-0.13.0.zip
unzip freedoom-0.13.0.zip freedoom-0.13.0/freedoom2.wad
mv freedoom-0.13.0/freedoom2.wad .
rm -rf freedoom-0.13.0 freedoom-0.13.0.zip
```

## Build & Run

```bash
make
./doom2
```

## Controls

| Key | Action |
|-----|--------|
| W/S | Move forward/backward |
| A/D | Turn left/right |
| SPACE | Shoot |
| E | Open door / use |
| LSHIFT | Run |
| 1/2 | Switch weapon |

## Architecture

```
COBOL (doom2.cob + 20 copybooks)
  ├── WAD parser (wad-parser.cpy, wad-textures.cpy)
  ├── Map loader (map-loader-proc.cpy) — Bresenham vector→grid
  ├── Raycaster (raycaster-proc.cpy) — DDA + depth buffer
  ├── Floor/ceiling (floor-ceil-proc.cpy) — scanline casting
  ├── Sprites (sprites-proc.cpy) — depth-sorted projection
  ├── Enemies (enemies-proc.cpy) — FSM AI with LOS
  ├── Weapons (weapons-proc.cpy) — hitscan + spread
  ├── Doors/Pickups (doors-proc.cpy)
  └── HUD (hud-proc.cpy) — bitmap font status bar

C shim (display.c, 72 lines)
  └── SDL2 window, texture upload, keyboard polling, WAD file I/O
```

The COBOL code fills a 320x200 RGBA framebuffer every frame. The C shim only uploads it to an SDL2 texture for display.
