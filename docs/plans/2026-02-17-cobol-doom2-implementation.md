# COBOL Doom 2 — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build a playable MAP01 of Doom 2 in GnuCOBOL with SDL2 pixel rendering, >98% COBOL.

**Architecture:** COBOL software renderer fills 320x200 RGBA framebuffer. Minimal C shim (~60 lines) passes buffer to SDL2 for display. Freedoom WAD provides textures, sprites, and map data. All game logic, raycasting, texture mapping, sprite rendering in COBOL.

**Tech Stack:** GnuCOBOL 3.x, SDL2, C (shim only), Freedoom2 WAD

---

### Task 0: Environment Setup

**Files:**
- Create: `Makefile`
- Create: `display.c`
- Create: `doom2.cob` (skeleton)
- Create: `copy/` directory
- Download: `freedoom2.wad`

**Step 1: Install dependencies**

```bash
sudo apt-get update && sudo apt-get install -y gnucobol4 libsdl2-dev wget
```

Verify: `cobc --version` prints GnuCOBOL version.

**Step 2: Download Freedoom WAD**

```bash
cd /home/ubuntu/Doom-Cob
wget -q https://github.com/niccokunzmann/freedoom/releases/download/v0.12.1/freedoom-0.12.1.zip
unzip -o freedoom-0.12.1.zip "freedoom-0.12.1/freedoom2.wad"
mv freedoom-0.12.1/freedoom2.wad .
rm -rf freedoom-0.12.1 freedoom-0.12.1.zip
```

If download fails, try alternative URL or `apt-get install freedoom` and copy from `/usr/share/games/doom/`.

Verify: `ls -la freedoom2.wad` shows file ~28MB.

**Step 3: Create display.c**

```c
/* display.c — SDL2 display driver for COBOL Doom 2 */
/* ~60 lines. No game logic. No rendering. Just show pixels. */

#include <SDL2/SDL.h>

static SDL_Window*   win;
static SDL_Renderer* ren;
static SDL_Texture*  tex;

void sdl_init(int *w, int *h) {
    SDL_Init(SDL_INIT_VIDEO);
    win = SDL_CreateWindow("COBOL DOOM 2",
        SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
        *w * 3, *h * 3, 0);
    ren = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED);
    SDL_RenderSetLogicalSize(ren, *w, *h);
    tex = SDL_CreateTexture(ren, SDL_PIXELFORMAT_RGBA32,
        SDL_TEXTUREACCESS_STREAMING, *w, *h);
}

void sdl_frame(unsigned char *buf, int *w, int *h) {
    SDL_UpdateTexture(tex, NULL, buf, *w * 4);
    SDL_RenderClear(ren);
    SDL_RenderCopy(ren, tex, NULL, NULL);
    SDL_RenderPresent(ren);
}

void sdl_input(int *keys) {
    SDL_Event e;
    const Uint8 *state;
    while (SDL_PollEvent(&e)) {
        if (e.type == SDL_QUIT) { keys[0] = -1; return; }
    }
    state = SDL_GetKeyboardState(NULL);
    keys[0] = 0;
    keys[1] = state[SDL_SCANCODE_W];
    keys[2] = state[SDL_SCANCODE_S];
    keys[3] = state[SDL_SCANCODE_A];
    keys[4] = state[SDL_SCANCODE_D];
    keys[5] = state[SDL_SCANCODE_SPACE];
    keys[6] = state[SDL_SCANCODE_E];
    keys[7] = state[SDL_SCANCODE_LSHIFT];
    keys[8] = state[SDL_SCANCODE_1];
    keys[9] = state[SDL_SCANCODE_2];
}

void sdl_quit(void) {
    if (tex) SDL_DestroyTexture(tex);
    if (ren) SDL_DestroyRenderer(ren);
    if (win) SDL_DestroyWindow(win);
    SDL_Quit();
}
```

**Step 4: Create Makefile**

```makefile
CC = gcc
COBC = cobc
CFLAGS = $(shell sdl2-config --cflags)
LDFLAGS = $(shell sdl2-config --libs)

doom2: doom2.cob display.c
	$(COBC) -x -o doom2 doom2.cob display.c $(CFLAGS) $(LDFLAGS)

clean:
	rm -f doom2 *.o

.PHONY: clean
```

**Step 5: Create doom2.cob skeleton with test pattern**

Create minimal `doom2.cob` that:
- Declares `WS-SCREEN-W PIC 9(3) VALUE 320` and `WS-SCREEN-H PIC 9(3) VALUE 200`
- Declares `WS-FRAMEBUFFER PIC X(256000)` (320*200*4 bytes)
- Declares `WS-KEYS` array of 10 integers
- Calls `sdl_init`, fills framebuffer with a gradient test pattern (red->blue),
  enters game loop calling `sdl_frame` + `sdl_input`, exits on keys[0] = -1
- Calls `sdl_quit`

The test pattern loop: for each pixel (x,y), set R=x, G=y, B=128, A=255.

**Step 6: Build and verify**

```bash
make
./doom2
```

Expected: SDL window opens showing a colorful gradient. Closes on window X or Ctrl+C.

**Step 7: Commit**

```bash
git add Makefile display.c doom2.cob copy/
git commit -m "feat: SDL2 shim + COBOL skeleton with test pattern"
```

---

### Task 1: WAD Parser — Header, Directory, Palette

**Files:**
- Create: `copy/wad-data.cpy` (shared WAD data definitions)
- Create: `copy/wad-parser.cpy` (WAD reading procedures)
- Modify: `doom2.cob` (add COPY statements, call WAD parser at startup)

**Step 1: Define WAD data structures in `copy/wad-data.cpy`**

```cobol
      * WAD file handling
       01 WAD-FILE-NAME     PIC X(256).
       01 WAD-FILE-STATUS   PIC XX.
       01 WAD-HEADER.
          05 WAD-ID         PIC X(4).
          05 WAD-NUM-LUMPS  PIC S9(9) COMP-5.
          05 WAD-DIR-OFFSET PIC S9(9) COMP-5.
       01 WAD-HEADER-RAW    PIC X(12).
       01 WAD-HEADER-R REDEFINES WAD-HEADER-RAW.
          05 WHR-ID         PIC X(4).
          05 WHR-NUM-LUMPS  PIC X(4).
          05 WHR-DIR-OFFSET PIC X(4).

      * Directory entry (16 bytes each)
       01 WAD-MAX-LUMPS     PIC 9(5) VALUE 04000.
       01 WAD-DIR-ENTRY.
          05 WDE-OFFSET     PIC S9(9) COMP-5.
          05 WDE-SIZE       PIC S9(9) COMP-5.
          05 WDE-NAME       PIC X(8).
       01 WAD-DIR-RAW       PIC X(16).
       01 WAD-DIR-R REDEFINES WAD-DIR-RAW.
          05 WDR-OFFSET     PIC X(4).
          05 WDR-SIZE       PIC X(4).
          05 WDR-NAME       PIC X(8).

      * Lump directory table
       01 WAD-LUMP-TABLE.
          05 WAD-LUMP OCCURS 4000 TIMES.
             10 WL-OFFSET   PIC S9(9) COMP-5.
             10 WL-SIZE     PIC S9(9) COMP-5.
             10 WL-NAME     PIC X(8).
       01 WAD-LUMP-COUNT    PIC 9(5).

      * Palette (PLAYPAL) — 256 colors, 3 bytes each (RGB)
       01 WAD-PALETTE.
          05 WAD-PAL-ENTRY OCCURS 256 TIMES.
             10 WAD-PAL-R   PIC X.
             10 WAD-PAL-G   PIC X.
             10 WAD-PAL-B   PIC X.
       01 WAD-PAL-RAW       PIC X(768).

      * Colormap — 34 tables of 256 bytes for lighting
       01 WAD-COLORMAP      PIC X(8704).

      * Temp read buffer (up to 64KB per read)
       01 WAD-READ-BUF      PIC X(65536).
       01 WAD-READ-LEN      PIC S9(9) COMP-5.
       01 WAD-BYTE           PIC X.
       01 WAD-BYTE-NUM REDEFINES WAD-BYTE PIC 9(2) COMP-5.
       01 WAD-INT32          PIC X(4).
       01 WAD-INT32-NUM REDEFINES WAD-INT32 PIC S9(9) COMP-5.
       01 WAD-INT16          PIC X(2).
       01 WAD-INT16-NUM REDEFINES WAD-INT16 PIC S9(4) COMP-5.
```

**Step 2: Implement WAD reader in `copy/wad-parser.cpy`**

Procedures:
- `OPEN-WAD`: Open WAD file, read 12-byte header, validate "IWAD"/"PWAD"
- `READ-WAD-DIRECTORY`: Seek to directory offset, read all lump entries into `WAD-LUMP-TABLE`
- `FIND-LUMP`: Search directory by name, return offset+size
- `READ-LUMP-DATA`: Seek to lump offset, read N bytes into buffer
- `LOAD-PALETTE`: Find "PLAYPAL", read 768 bytes, store in `WAD-PALETTE`
- `LOAD-COLORMAP`: Find "COLORMAP", read 8704 bytes

Uses GnuCOBOL file I/O:
```cobol
SELECT WAD-FILE ASSIGN TO WS-WAD-FILENAME
    ORGANIZATION IS SEQUENTIAL
    ACCESS MODE IS SEQUENTIAL
    FILE STATUS IS WAD-FILE-STATUS.
```
And `CALL "CBL_READ_FILE"` or sequential READ with positioning for random access.

**Step 3: Integrate into doom2.cob**

Add `COPY` statements for data and procedures. Call `OPEN-WAD` and `LOAD-PALETTE` at startup. Verify by rendering palette as test: draw 256 colored bars (16x16 grid) using loaded palette colors in framebuffer.

**Step 4: Build and verify**

```bash
make && ./doom2
```

Expected: SDL window shows 16x16 grid of 256 colors from Freedoom palette.

**Step 5: Commit**

```bash
git add copy/wad-data.cpy copy/wad-parser.cpy doom2.cob
git commit -m "feat: WAD parser with palette and colormap loading"
```

---

### Task 2: WAD Textures and Flats

**Files:**
- Create: `copy/wad-textures.cpy` (texture/flat extraction procedures)
- Modify: `copy/wad-data.cpy` (add texture/flat data structures)
- Modify: `doom2.cob` (call texture loader, render test texture)

**Step 1: Add texture data structures to `copy/wad-data.cpy`**

```cobol
      * Wall textures — stored as palette-indexed pixel arrays
       01 WAD-MAX-TEXTURES   PIC 9(3) VALUE 100.
       01 WAD-TEX-COUNT      PIC 9(3).
       01 WAD-TEXTURES.
          05 WAD-TEX OCCURS 100 TIMES.
             10 WT-NAME      PIC X(8).
             10 WT-WIDTH     PIC 9(3).
             10 WT-HEIGHT    PIC 9(3).
             10 WT-PIXELS    PIC X(8192).
      *>       64 wide * 128 tall = 8192 bytes max

      * Flats (floor/ceiling) — 64x64 palette-indexed
       01 WAD-MAX-FLATS      PIC 9(3) VALUE 050.
       01 WAD-FLAT-COUNT     PIC 9(3).
       01 WAD-FLATS.
          05 WAD-FLAT OCCURS 50 TIMES.
             10 WF-NAME      PIC X(8).
             10 WF-PIXELS    PIC X(4096).
      *>       64 * 64 = 4096 bytes
```

**Step 2: Implement texture extraction in `copy/wad-textures.cpy`**

Procedures:
- `LOAD-PNAMES`: Read PNAMES lump (list of patch names)
- `LOAD-TEXTURE-DEFS`: Read TEXTURE1/TEXTURE2 lumps, parse texture definitions (name, width, height, patch list). For each texture, compose patches into the `WT-PIXELS` array.
- `LOAD-PATCH`: Read a single patch lump (column-based format with posts). Each column: list of (top_offset, length, pixels) segments.
- `LOAD-FLATS`: Iterate F_START to F_END markers in lump directory, read each 4096-byte flat into `WAD-FLATS`.

Key detail: Doom textures are composed of multiple patches. Each patch is column-based. COBOL reads patch columns and composites them into the texture pixel array.

**Step 3: Test by rendering a wall texture**

In doom2.cob, after loading textures, pick the first loaded texture and render it fullscreen (stretched to 320x200) using palette RGB. Each pixel: read palette index from `WT-PIXELS`, look up RGB in `WAD-PALETTE`, write 4 bytes to framebuffer.

**Step 4: Build and verify**

```bash
make && ./doom2
```

Expected: SDL window shows a Doom wall texture (e.g., STARTAN3 or similar) filling the screen.

**Step 5: Commit**

```bash
git add copy/wad-textures.cpy copy/wad-data.cpy doom2.cob
git commit -m "feat: load wall textures and flats from WAD"
```

---

### Task 3: Textured Wall Raycasting

**Files:**
- Create: `copy/raycaster.cpy` (DDA + textured wall rendering)
- Modify: `doom2.cob` (add player state, simple hardcoded map, game loop with movement)

**Step 1: Define renderer data in doom2.cob**

Player state, framebuffer, depth buffer, trig tables — adapt from existing `doom.cob` but output to pixel framebuffer instead of ASCII.

Key data:
```cobol
       01 WS-FB.
          05 WS-FB-ROW OCCURS 200 TIMES.
             10 WS-FB-PIX OCCURS 320 TIMES.
                15 WS-FB-R  PIC X.
                15 WS-FB-G  PIC X.
                15 WS-FB-B  PIC X.
                15 WS-FB-A  PIC X.
       01 WS-DEPTH-BUF.
          05 WS-DEPTH OCCURS 320 TIMES PIC S9(5)V9(4).
```

**Step 2: Implement textured raycaster in `copy/raycaster.cpy`**

Adapt existing DDA from `doom.cob` but instead of writing `#%=-` characters:

For each column (1 to 320):
1. Cast ray using DDA (already implemented)
2. On wall hit: determine which texture (from map cell) and calculate U coordinate:
   - If side=0 (NS wall): `U = fractional part of (player_y + dist * ray_dir_y) * 64`
   - If side=1 (EW wall): `U = fractional part of (player_x + dist * ray_dir_x) * 64`
3. For each pixel row in the wall column:
   - Calculate V coordinate: `V = (row - wall_top) * tex_height / wall_height`
   - Read palette index from `WT-PIXELS(tex_id)` at offset `U * height + V`
   - Apply distance-based lighting via colormap: `light_level = dist / 2` (clamped 0-31)
   - Look up lit palette index from `WAD-COLORMAP`
   - Look up RGB from `WAD-PALETTE`
   - Write RGBA to `WS-FB-PIX(row, col)`
4. Fill ceiling pixels (rows above wall) with dark gray
5. Fill floor pixels (rows below wall) with brown

Use a small hardcoded 16x16 map (similar to existing one) with texture IDs for testing.

**Step 3: Add game loop with WASD movement**

Reuse movement logic from existing `doom.cob`. Loop: clear framebuffer → cast rays → call `sdl_frame` → call `sdl_input` → process movement → repeat.

**Step 4: Build and verify**

```bash
make && ./doom2
```

Expected: SDL window shows textured walls with proper perspective. WASD moves, A/D rotates. Walls have actual Doom textures.

**Step 5: Commit**

```bash
git add copy/raycaster.cpy doom2.cob
git commit -m "feat: textured wall raycasting with DDA"
```

---

### Task 4: Floor and Ceiling Rendering

**Files:**
- Create: `copy/floor-ceil.cpy`
- Modify: `doom2.cob` (call floor/ceiling renderer after walls)

**Step 1: Implement horizontal casting in `copy/floor-ceil.cpy`**

For each row below the lowest wall pixel (floor) and above the highest wall pixel (ceiling):

```
For row = screen_h/2+1 to screen_h (floor):
    row_distance = screen_h / (2.0 * row - screen_h)
    For col = 1 to 320:
        floor_x = player_x + row_distance * ray_dir_left_x
                  + (col/320) * row_distance * (ray_dir_right_x - ray_dir_left_x)
        floor_y = player_y + ... (same pattern for Y)
        tex_x = FUNCTION MOD(floor_x * 64, 64)
        tex_y = FUNCTION MOD(floor_y * 64, 64)
        palette_idx = WF-PIXELS(flat_id) at (tex_y * 64 + tex_x)
        Apply colormap lighting by row_distance
        Write RGB to framebuffer at (row, col)

Ceiling: mirror of floor (row from screen_h/2 down to 1)
```

Use flat textures: default floor flat (e.g., "FLOOR0_1") and ceiling flat (e.g., "CEIL1_1") from WAD.

**Step 2: Integrate into game loop**

Call `RENDER-FLOOR-CEILING` after `CAST-ALL-RAYS` but before any sprite rendering.

**Step 3: Build and verify**

```bash
make && ./doom2
```

Expected: Full 3D scene — textured walls, textured floor, textured ceiling. No more solid-color floor/ceiling.

**Step 4: Commit**

```bash
git add copy/floor-ceil.cpy doom2.cob
git commit -m "feat: textured floor and ceiling rendering"
```

---

### Task 5: MAP01 Loader

**Files:**
- Create: `copy/map-loader.cpy`
- Modify: `copy/wad-data.cpy` (add map data structures)
- Modify: `doom2.cob` (replace hardcoded map with WAD-loaded MAP01)

**Step 1: Add map data structures**

```cobol
      * Map grid — 128x128 cells
       01 MAP-SIZE          PIC 9(3) VALUE 128.
       01 MAP-GRID.
          05 MAP-ROW OCCURS 128 TIMES.
             10 MAP-CELL OCCURS 128 TIMES.
                15 MC-TYPE       PIC 9.
      *>          0=empty, 1=wall, 2=door
                15 MC-TEX-ID     PIC 9(3).
                15 MC-FLOOR-H    PIC S9(3).
                15 MC-CEIL-H     PIC S9(3).
                15 MC-FLOOR-TEX  PIC 9(3).
                15 MC-CEIL-TEX   PIC 9(3).
                15 MC-LIGHT      PIC 9(3).

      * Things (enemies, pickups, player start)
       01 MAP-MAX-THINGS    PIC 9(3) VALUE 200.
       01 MAP-THING-COUNT   PIC 9(3).
       01 MAP-THINGS.
          05 MAP-THING OCCURS 200 TIMES.
             10 MT-X         PIC S9(5).
             10 MT-Y         PIC S9(5).
             10 MT-ANGLE     PIC S9(5).
             10 MT-TYPE      PIC 9(5).
             10 MT-FLAGS     PIC 9(5).
```

**Step 2: Implement MAP01 loader in `copy/map-loader.cpy`**

Procedures:
- `LOAD-MAP`: Find MAP01 marker in lump directory. Read VERTEXES, LINEDEFS, SIDEDEFS, SECTORS, THINGS lumps.
- `BUILD-MAP-GRID`: Convert Doom's vector-based geometry to our 128x128 grid:
  - For each LINEDEF: rasterize the line between its two vertices onto the grid
  - Mark grid cells as wall (1) or door (2) based on linedef flags
  - Assign texture IDs from SIDEDEFS
  - Assign floor/ceiling heights and textures from SECTORS
- `LOAD-THINGS`: Read THINGS lump, populate `MAP-THINGS` table. Find Player 1 start (type 1) to set spawn position.

Note: Converting Doom's sector-based map to a grid is a simplification. Some detail will be lost. Acceptable tradeoff for the raycaster approach.

**Step 3: Replace hardcoded map**

In doom2.cob, replace the hardcoded 16x16 map with calls to `LOAD-MAP` and `BUILD-MAP-GRID`. Update player spawn position from MAP01's Player 1 start thing. Adjust raycaster to use `MAP-GRID` with 128x128 size.

**Step 4: Build and verify**

```bash
make && ./doom2
```

Expected: Walking through MAP01 geometry with textured walls, floor, ceiling. The layout should be recognizable as Doom 2's Entryway.

**Step 5: Commit**

```bash
git add copy/map-loader.cpy copy/wad-data.cpy doom2.cob
git commit -m "feat: load and render MAP01 from WAD"
```

---

### Task 6: Sprite Rendering

**Files:**
- Create: `copy/sprites.cpy`
- Modify: `copy/wad-data.cpy` (add sprite data structures)
- Modify: `copy/wad-textures.cpy` (add sprite loading)
- Modify: `doom2.cob` (call sprite renderer, populate game objects from THINGS)

**Step 1: Add sprite data structures**

```cobol
      * Sprite frames — column-based with transparency
       01 WAD-MAX-SPRITES   PIC 9(3) VALUE 200.
       01 WAD-SPRITE-COUNT  PIC 9(3).
       01 WAD-SPRITES.
          05 WAD-SPR OCCURS 200 TIMES.
             10 WS-SPR-NAME  PIC X(8).
             10 WS-SPR-W     PIC 9(3).
             10 WS-SPR-H     PIC 9(3).
             10 WS-SPR-OFFX  PIC S9(3).
             10 WS-SPR-OFFY  PIC S9(3).
             10 WS-SPR-PIX   PIC X(16384).
      *>       Max 128x128 pixels

      * Game objects (enemies, decorations, pickups visible in world)
       01 MAX-OBJECTS        PIC 9(3) VALUE 100.
       01 OBJ-COUNT          PIC 9(3).
       01 GAME-OBJECTS.
          05 GOBJ OCCURS 100 TIMES.
             10 GO-X          PIC S9(5)V9(4).
             10 GO-Y          PIC S9(5)V9(4).
             10 GO-TYPE        PIC 9(5).
             10 GO-SPRITE-ID   PIC 9(3).
             10 GO-ALIVE       PIC 9.
             10 GO-HEALTH      PIC 9(3).
             10 GO-STATE       PIC 9.
             10 GO-FRAME       PIC 9(2).
```

**Step 2: Implement sprite loader in `copy/wad-textures.cpy`**

Add `LOAD-SPRITE-FRAME`: Read a sprite lump (e.g., "POSSA1"), parse column-based format with posts (similar to patches), store in `WAD-SPRITES`. Handle transparency (pixels not covered by posts = transparent, stored as palette index 255 or a sentinel).

Load key sprites at startup: POSS (zombieman), SPOS (shotgun guy), basic decorations.

**Step 3: Implement billboard renderer in `copy/sprites.cpy`**

Procedures:
- `RENDER-ALL-SPRITES`: Sort game objects by distance (furthest first). For each visible object:
  1. Compute angle from player to object
  2. Check if within FOV
  3. Project to screen column and calculate sprite screen height
  4. For each column of the sprite on screen:
     - Check z-buffer: skip if wall is closer
     - For each pixel row: read palette index from sprite data
     - Skip transparent pixels (index 255)
     - Apply distance lighting via colormap
     - Write RGB to framebuffer

**Step 4: Populate objects from MAP01 THINGS**

After `LOAD-MAP`, iterate MAP-THINGS: for each enemy/decoration thing type, create a GAME-OBJECT with position and sprite ID.

**Step 5: Build and verify**

```bash
make && ./doom2
```

Expected: Enemies (zombiemen, shotgun guys) and decorations visible as Doom sprites in the 3D world. Properly depth-sorted against walls.

**Step 6: Commit**

```bash
git add copy/sprites.cpy copy/wad-data.cpy copy/wad-textures.cpy doom2.cob
git commit -m "feat: sprite loading and billboard rendering"
```

---

### Task 7: Enemy AI and Weapons

**Files:**
- Create: `copy/enemies.cpy`
- Create: `copy/weapons.cpy`
- Modify: `doom2.cob` (integrate combat loop)

**Step 1: Implement enemy AI in `copy/enemies.cpy`**

State machine per enemy:
```
IDLE:    Stand still. If player in line-of-sight and distance < threshold -> ALERT
ALERT:   Wake up (play alert frame). -> CHASE
CHASE:   Move toward player. If close enough and LOS clear -> ATTACK
ATTACK:  Fire hitscan at player. Random damage 3-15. -> CHASE
PAIN:    Hit reaction (1 frame). -> CHASE
DEATH:   Play death animation (5 frames). Set GO-ALIVE = 0.
```

Movement: move along axis toward player (reuse existing MOVE-ONE-ENEMY logic). Wall collision check against MAP-GRID.

Line-of-sight: cast a ray from enemy to player, check if it hits a wall before reaching player.

Sprite animation: cycle frames based on state (e.g., POSSA/B for walk, POSSE for attack, POSSH-POSSL for death).

**Step 2: Implement weapons in `copy/weapons.cpy`**

Procedures:
- `FIRE-PISTOL`: Hitscan from player. Check center column depth buffer. For each enemy, check if within ~5 degrees of crosshair and closer than wall. Deal 5-15 random damage.
- `FIRE-SHOTGUN`: 7 hitscans with slight angle spread. Each deals 5-15 damage.
- `WEAPON-ANIMATE`: Cycle weapon sprite frames (PISG for pistol idle, PISGA-PISGE for firing).
- `RENDER-WEAPON`: Draw current weapon sprite in bottom center of framebuffer (overlay, skip transparent pixels).

**Step 3: Integrate combat**

In game loop: after movement, call `UPDATE-ENEMIES` (AI ticks), `CHECK-ENEMY-ATTACKS` (enemies shoot player), process player shoot input via `FIRE-PISTOL`/`FIRE-SHOTGUN`. Call `RENDER-WEAPON` after all 3D rendering.

**Step 4: Build and verify**

```bash
make && ./doom2
```

Expected: Enemies wake up when they see you, chase, shoot. You can shoot them with pistol (space). Weapon visible at bottom of screen. Enemies play death animation.

**Step 5: Commit**

```bash
git add copy/enemies.cpy copy/weapons.cpy doom2.cob
git commit -m "feat: enemy AI and weapon combat system"
```

---

### Task 8: Doors and Pickups

**Files:**
- Create: `copy/doors.cpy`
- Create: `copy/pickups.cpy`
- Modify: `doom2.cob` (integrate doors and pickups into game loop)

**Step 1: Implement doors in `copy/doors.cpy`**

Data: door state table (OCCURS for each door cell in map):
```cobol
01 DOOR-TABLE.
   05 DOOR-ENTRY OCCURS 50 TIMES.
      10 DR-X       PIC 9(3).
      10 DR-Y       PIC 9(3).
      10 DR-STATE   PIC 9.     *> 0=closed, 1=opening, 2=open, 3=closing
      10 DR-HEIGHT  PIC S9(3). *> current ceiling offset
      10 DR-TIMER   PIC 9(3).  *> ticks until auto-close
```

Procedures:
- `USE-DOOR`: Player presses E. Check if facing a door cell (1-2 grid units ahead). If closed -> set to opening. If open -> set to closing.
- `UPDATE-DOORS`: Each frame, animate door heights. Opening: increase ceiling offset. Closing: decrease. When fully open, start close timer.
- During raycasting: door cells are treated as thin walls with variable height. If door is fully open, ray passes through.

**Step 2: Implement pickups in `copy/pickups.cpy`**

Procedures:
- `CHECK-PICKUPS`: Each frame, check distance from player to each pickup object. If distance < 1.0:
  - Ammo clip (type 2007): add 10 bullets
  - Shotgun (type 2001): give shotgun + 8 shells
  - Medikit (type 2012): add 25 health (max 100)
  - Set object ALIVE = 0 (remove from world)

**Step 3: Integrate**

In game loop: call `UPDATE-DOORS` each frame, process E key via `USE-DOOR`, call `CHECK-PICKUPS`.

**Step 4: Build and verify**

```bash
make && ./doom2
```

Expected: Doors open/close when pressing E near them. Picking up items adds ammo/health. MAP01 is navigable end-to-end.

**Step 5: Commit**

```bash
git add copy/doors.cpy copy/pickups.cpy doom2.cob
git commit -m "feat: door mechanics and item pickups"
```

---

### Task 9: HUD

**Files:**
- Create: `copy/hud.cpy`
- Modify: `doom2.cob` (call HUD renderer as final draw step)

**Step 1: Load HUD assets from WAD**

Load at startup:
- `STBAR` — status bar background (320x32)
- `STTNUM0` through `STTNUM9` — number font sprites
- `STFST*` — Doomguy face sprites (different expressions based on HP)
- `STARMS*` — weapon selector indicators

**Step 2: Implement HUD renderer in `copy/hud.cpy`**

Procedures:
- `RENDER-HUD`: Draw status bar background at bottom 32 rows of framebuffer
- `RENDER-HUD-NUMBER`: Draw a number (health, ammo) at given X position using STTNUM sprites. Handle multi-digit: extract hundreds/tens/ones, draw each digit sprite.
- `RENDER-HUD-FACE`: Select face sprite based on health percentage (5 levels: 80-100%, 60-79%, 40-59%, 20-39%, 0-19%). Draw at center of status bar.
- `RENDER-HUD-ARMS`: Draw weapon selector (highlight current weapon).

HUD layout (matching Doom 2):
```
|  AMMO  |                | HEALTH |  ARMS  | FACE  | ARMOR |  AMMO  |
| (left) |                | (mid)  | (keys) |(cent) |       | counts |
```

**Step 3: Integrate**

Call `RENDER-HUD` after all 3D rendering and weapon overlay. This draws over the bottom 32 pixels.

**Step 4: Build and verify**

```bash
make && ./doom2
```

Expected: Doom-style status bar at bottom with health, ammo, Doomguy face. Numbers update in real-time. Face changes expression when hurt.

**Step 5: Commit**

```bash
git add copy/hud.cpy doom2.cob
git commit -m "feat: pixel-perfect HUD with status bar"
```

---

### Task 10: Polish — Lighting, Exit, Final Touches

**Files:**
- Modify: `copy/raycaster.cpy` (distance-based + sector lighting)
- Modify: `copy/enemies.cpy` (animation frame cycling)
- Modify: `doom2.cob` (level exit, title screen, win condition)

**Step 1: Improve lighting**

- Apply sector light levels from MAP01 data to colormap lookups
- Combine sector light with distance-based falloff: `final_light = sector_light/8 + distance/2`
- Clamp to colormap range 0-31
- Dark sectors should feel dark, bright sectors bright — matching Doom 2 feel

**Step 2: Sprite animation**

- Enemy walk cycle: alternate frames A/B every N ticks
- Enemy attack: show attack frame for duration
- Death: cycle through death frames (E through H/L), then stay on last frame
- Direction-based sprites: Doom sprites have 8 angles (1-8). Calculate which angle to show based on enemy-to-player direction relative to enemy facing. For MVP: use front-facing sprite only.

**Step 3: Level exit**

- MAP01 exit switch: when player activates specific linedef, trigger level complete
- Show "LEVEL COMPLETE" screen with stats (kills, items, secrets, time)
- "COBOL DOOM 2 ENGINE" prominently displayed

**Step 4: Title screen**

- On startup, show Freedoom title screen (TITLEPIC from WAD) or custom "COBOL DOOM 2" text rendered in pixel font
- "Press any key to start"
- Then load MAP01 and begin

**Step 5: Final build and full playtest**

```bash
make && ./doom2
```

Expected: Complete playable experience. Start at MAP01 spawn, fight through enemies, open doors, pick up items, find exit. Visuals recognizably Doom 2 with proper textures, sprites, lighting.

**Step 6: Commit and tag**

```bash
git add -A
git commit -m "feat: lighting, animations, level exit, title screen"
git tag v1.0.0
```

---

## Summary

| Task | What | Deliverable |
|------|------|-------------|
| 0 | Environment + SDL shim | SDL window with test pattern |
| 1 | WAD parser + palette | Palette colors displayed |
| 2 | Texture + flat loading | Wall texture rendered |
| 3 | Textured raycaster | 3D textured walls, movement |
| 4 | Floor/ceiling | Full 3D scene |
| 5 | MAP01 loader | Walk through Entryway |
| 6 | Sprites | See enemies and decorations |
| 7 | Enemies + weapons | Combat system |
| 8 | Doors + pickups | Full interactivity |
| 9 | HUD | Status bar |
| 10 | Polish | Complete game |
