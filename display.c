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
