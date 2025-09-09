#include <cstdio>

#include <SDL.h>

#include <imgui.h>
#include <imgui_impl_sdl2.h>
#include <imgui_impl_sdlrenderer2.h>

#include <Igor/Logging.hpp>

auto main() -> int {  // Setup SDL
  if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER | SDL_INIT_GAMECONTROLLER) != 0) {
    Igor::Warn("Error: {}", SDL_GetError());
    return 1;
  }

  SDL_SetHint(SDL_HINT_IME_SHOW_UI, "1");

  // Create window with SDL_Renderer graphics context
  float main_scale   = ImGui_ImplSDL2_GetContentScaleForDisplay(0);
  SDL_Window* window = SDL_CreateWindow("Dear ImGui SDL2+SDL_Renderer example",
                                        SDL_WINDOWPOS_CENTERED,
                                        SDL_WINDOWPOS_CENTERED,
                                        (int)(1280 * main_scale),
                                        (int)(720 * main_scale),
                                        SDL_WINDOW_OPENGL | SDL_WINDOW_ALLOW_HIGHDPI);
  if (window == nullptr) {
    Igor::Warn("Error: SDL_CreateWindow(): {}", SDL_GetError());
    return 1;
  }
  SDL_Renderer* renderer =
      SDL_CreateRenderer(window, -1, SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_ACCELERATED);
  if (renderer == nullptr) {
    Igor::Warn("Error: SDL_CreateRenderer(): {}", SDL_GetError());
    return 1;
  }
  SDL_RendererInfo info;
  SDL_GetRendererInfo(renderer, &info);
  Igor::Info("Current SDL_Renderer: {}", info.name);

  // Setup Dear ImGui context
  IMGUI_CHECKVERSION();
  ImGui::CreateContext();
  ImGuiIO& io     = ImGui::GetIO();
  io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;  // Enable Keyboard Controls
  io.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;   // Enable Gamepad Controls

  // Setup Dear ImGui style
  ImGui::StyleColorsDark();

  // Setup scaling
  ImGuiStyle& style = ImGui::GetStyle();
  style.ScaleAllSizes(
      main_scale);  // Bake a fixed style scale. (until we have a solution for dynamic style
                    // scaling, changing this requires resetting Style + calling this again)
  style.FontScaleDpi =
      main_scale;  // Set initial font scale. (using io.ConfigDpiScaleFonts=true makes this
                   // unnecessary. We leave both here for documentation purpose)

  // Setup Platform/Renderer backends
  ImGui_ImplSDL2_InitForSDLRenderer(window, renderer);
  ImGui_ImplSDLRenderer2_Init(renderer);

  // Load Fonts
  style.FontSizeBase = 20.0F;
  // io.Fonts->AddFontFromFileTTF("assets/LinLibertine_R.ttf");
  io.Fonts->AddFontFromFileTTF("assets/RobotoMonoNerdFont-Regular.ttf");

  // Our state
  ImVec4 clear_color = ImVec4(0.45F, 0.55F, 0.60F, 1.00F);

  // Main loop
  bool done = false;
  while (!done) {
    // Poll and handle events (inputs, window resize, etc.)
    // You can read the io.WantCaptureMouse, io.WantCaptureKeyboard flags to tell if dear imgui
    // wants to use your inputs.
    // - When io.WantCaptureMouse is true, do not dispatch mouse input data to your main
    // application, or clear/overwrite your copy of the mouse data.
    // - When io.WantCaptureKeyboard is true, do not dispatch keyboard input data to your main
    // application, or clear/overwrite your copy of the keyboard data. Generally you may always pass
    // all inputs to dear imgui, and hide them from your application based on those two flags.
    SDL_Event event;
    while (SDL_PollEvent(&event) != 0) {
      ImGui_ImplSDL2_ProcessEvent(&event);
      if (event.type == SDL_QUIT) { done = true; }
      if (event.type == SDL_WINDOWEVENT && event.window.event == SDL_WINDOWEVENT_CLOSE &&
          event.window.windowID == SDL_GetWindowID(window)) {
        done = true;
      }
      if (event.type == SDL_KEYDOWN && event.key.keysym.sym == SDLK_q) { done = true; }
    }
    if ((SDL_GetWindowFlags(window) & SDL_WINDOW_MINIMIZED) > 0) {
      SDL_Delay(10);
      continue;
    }

    // Start the Dear ImGui frame
    ImGui_ImplSDLRenderer2_NewFrame();
    ImGui_ImplSDL2_NewFrame();
    ImGui::NewFrame();

    {
      static float T = 0.5F;
      static float C = -1.0F;

      ImGui::Begin("Config");

      ImGui::SliderFloat("Temperature", &T, 0.0F, 2.0F);
      ImGui::SliderFloat("Chemical Potential", &C, -5.0F, 5.0F);
      ImGui::Text(
          "Application average %.3f ms/frame (%.1f FPS)", 1000.0F / io.Framerate, io.Framerate);
      ImGui::End();
    }

    // Rendering
    ImGui::Render();
    SDL_RenderSetScale(renderer, io.DisplayFramebufferScale.x, io.DisplayFramebufferScale.y);
    SDL_SetRenderDrawColor(renderer,
                           (Uint8)(clear_color.x * 255),
                           (Uint8)(clear_color.y * 255),
                           (Uint8)(clear_color.z * 255),
                           (Uint8)(clear_color.w * 255));
    SDL_RenderClear(renderer);
    ImGui_ImplSDLRenderer2_RenderDrawData(ImGui::GetDrawData(), renderer);

    SDL_SetRenderDrawColor(renderer, 255, 0, 0, 255);
    SDL_Rect rect{.x = 100, .y = 100, .w = 100, .h = 100};
    SDL_RenderFillRect(renderer, &rect);

    SDL_RenderPresent(renderer);
  }

  // Cleanup
  ImGui_ImplSDLRenderer2_Shutdown();
  ImGui_ImplSDL2_Shutdown();
  ImGui::DestroyContext();

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();

  return 0;
}
