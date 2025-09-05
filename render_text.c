#include <assert.h>
#include <stdint.h>

#include <SDL.h>
#include <SDL_ttf.h>

void render_T_c(
    SDL_Renderer* renderer, TTF_Font* font, const char* text, int window_width, int text_height) {
  const SDL_Color text_color = {.r = 0, .g = 0, .b = 0, .a = 255};
  SDL_Surface* text_surface  = TTF_RenderText_Solid(font, text, text_color);
  SDL_Texture* text_texture  = SDL_CreateTextureFromSurface(renderer, text_surface);

  int w, h;
  TTF_SizeText(font, text, &w, &h);
  assert(h <= text_height);
  assert(w <= window_width);
  const int w_extra = window_width - w;
  SDL_Rect rect     = {.x = w_extra / 2, .y = 0, .w = w, .h = h};
  SDL_RenderCopy(renderer, text_texture, NULL, &rect);

  SDL_FreeSurface(text_surface);
  SDL_DestroyTexture(text_texture);
}
