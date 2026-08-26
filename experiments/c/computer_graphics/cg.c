#include <assert.h>
#include <time.h>
#include <SDL2/SDL.h>

static void sleep_ms(double seconds) {
    struct timespec ts;
    ts.tv_sec = (time_t) seconds;
    ts.tv_nsec = (long) (seconds * 1.0e9);
    nanosleep(&ts, NULL);
}

typedef unsigned char color8_t;

struct RGBA {
    color8_t red;
    color8_t green;
    color8_t blue;
    color8_t alpha;
};

/*
 * TODO: When creating screen, check for even width and height
   pragma Assert(Width mod 2 = 0, "Width must be even");
   pragma Assert(Height mod 2 = 0, "Height must be even");
*/
struct screen {
    unsigned int width;
    unsigned int height;
    struct RGBA* data;
};

/* Assuming height and width are even, then
 * screen indices range from
 * 0 to (height - 1) for height and
 * 0 to (width - 1) for width.
 * Canvas indices then range from
 * (-height / 2) + 1 to (height / 2) for height and
 * (-width / 2) to (width / 2) - 1 for width.
 */

struct RGBA get_screen_value(unsigned int h, unsigned int w, const struct screen* s) {
    unsigned int i = h * s->width + w;
    return s->data[i];    
}

struct RGBA get_canvas_value(int canvas_h, int canvas_w, const struct screen* s) {
    int half_screen_h = ((int) s->height) / 2;
    assert(canvas_h <= half_screen_h);
    assert(canvas_h > -half_screen_h);
    unsigned int h = (unsigned int) (half_screen_h - canvas_h);

    int half_screen_w = ((int) s->width) / 2;
    assert(canvas_w < half_screen_w);
    assert(canvas_w >= -half_screen_w);
    unsigned int w = (unsigned int) (half_screen_w + canvas_w);

    unsigned int i = h * s->width + w;
    return s->data[i];    
}

void put_pixel(
    SDL_Renderer* renderer,
    int x, int y,
    color8_t r, color8_t g, color8_t b, color8_t a
) {
    int unused_status;
    unused_status = SDL_SetRenderDrawColor(renderer, r, g, b, a);
    unused_status = SDL_RenderDrawPoint(renderer, x, y);
    (void) unused_status;
}

int main() {
    // 1. Initialize SDL
    /*
    SDL_INIT_VIDEO : constant :=16#00000020#;
    SDL_WINDOWPOS_CENTERED : constant := 16#2FFF0000#;
    SDL_RENDERER_ACCELERATED : constant := 16#00000002#;
    SDL_EVENTTYPE_QUIT : constant := 16#100#;
    */
    size_t i, j;

    /* TODO: Remove */
    printf("SDL_INIT_VIDEO: x%08x", SDL_INIT_VIDEO);

    if (SDL_Init(SDL_INIT_VIDEO) != 0) { // Initialize the video subsystem
        SDL_Log("Unable to initialize SDL: %s", SDL_GetError());
        return 1;
    }

    // 2. Create a window and renderer
    SDL_Window* window = SDL_CreateWindow(
        "SDL2 Simple Example",             // window title
        SDL_WINDOWPOS_CENTERED,            // x position
        SDL_WINDOWPOS_CENTERED,            // y position
        640,                               // width
        480,                               // height
        0                                  // flags (0 for default)
    );

    if (window == NULL) {
        SDL_Log("Could not create window: %s", SDL_GetError());
        SDL_Quit();
        return 1;
    }

    SDL_Renderer* renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED); // Create a renderer for the window
    if (renderer == NULL) {
        SDL_DestroyWindow(window);
        SDL_Quit();
        return 1;
    }


    // 3. The event loop
    int keep_going = 1;
    SDL_Event event;
    while (keep_going) {
        while (SDL_PollEvent(&event)) { // Poll for events
            if (event.type == SDL_QUIT) { // Check for the window close event
                printf("Got quit event with timestamp %d\n", event.quit.timestamp);
                keep_going = 0;
            }
        }

        // Rendering
        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255); // Set draw color to black (RGBA)
        SDL_RenderClear(renderer);                      // Clear the renderer with the current color

        for(i = (640 / 4); i < (3 * 640 / 4); i++) {
            for (j = 0*(480 / 4); j < (4 * 480 / 4); j++) {
                put_pixel(renderer, i, j, 255, 0, 0, 255);
            }
        }
        // Add drawing code here (e.g., lines, shapes, textures)
        SDL_RenderPresent(renderer);                   // Update the screen
        sleep_ms(1.0 / 30.0);
    }

    // 4. Clean up
    SDL_DestroyRenderer(renderer); // Destroy renderer
    SDL_DestroyWindow(window);     // Destroy window
    SDL_Quit();                    // Quit SDL subsystems

    return 0;
}
