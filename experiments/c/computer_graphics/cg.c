#include <assert.h>
#include <time.h>
#include <math.h>
#include <SDL2/SDL.h>

#define SCREEN_WIDTH  640
#define SCREEN_HEIGHT 480

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

struct ScreenDims {
    unsigned int width;
    unsigned int height;
};
    
struct CanvasDims {
    int width;
    int height;
};
    
struct ViewPortConfig {
    float width;
    float height;
    float d; /* camera to viewport distance along z axis */
};
    
struct ScreenPoint {
    unsigned int x;
    unsigned int y;
};

struct CanvasPoint {
    int x;
    int y;
};

struct Point3d {
    float x;
    float y;
    float z;
};

struct CanvasPoint screen_to_canvas_point(const struct ScreenDims* sdims, struct ScreenPoint spt) {
    assert(spt.x < sdims->width);
    assert(spt.y < sdims->height);

    int half_screen_w = ((int) sdims->width) / 2;
    int half_screen_h = ((int) sdims->height) / 2;

    int cx = (int) (((int) spt.x) - half_screen_w);
    int cy = (int) (half_screen_h - ((int) spt.y));

    return (struct CanvasPoint) { cx, cy };
}

struct Point3d canvas_to_viewport(const struct CanvasDims* cdims, const struct ViewPortConfig* vpconf, struct CanvasPoint cpt) {
    return (struct Point3d) {
        cpt.x * vpconf->width / (float) cdims->width,
        cpt.y * vpconf->height / (float) cdims->height,
        vpconf->d 
    };
}

struct Sphere {
    struct Point3d center;
    float radius;
    struct RGBA color;
};

#define NUMBER_SPHERES 3

const struct Sphere spheres[NUMBER_SPHERES] = {
    /*
    (struct Sphere) { (struct Point3d) {0, -1, 3}, 1.0, (struct RGBA) { 255, 0, 0, 0 } },
    (struct Sphere) { (struct Point3d) {0, -1, 3}, 1.0, (struct RGBA) { 255, 0, 0, 0 } },
    (struct Sphere) { (struct Point3d) {0, -1, 3}, 1.0, (struct RGBA) { 255, 0, 0, 0 } }
    */
    { { 0, -1, 3}, 1.0, { 255,   0,   0, 0 } },
    { { 2,  0, 4}, 1.0, {   0,   0, 255, 0 } },
    { {-2,  0, 4}, 1.0, {   0, 255,   0, 0 } }
};

const struct RGBA BACKGROUND_COLOR = { 0 };

/* For the viewport dimensions, we scale the height based on the
 * integer screen height and width. */
const struct ViewPortConfig viewport_config = { 1.0, (float) SCREEN_HEIGHT / SCREEN_WIDTH, 1.0 };

float dot3d(struct Point3d a, struct Point3d b) {
    return a.x * b.x + a.y * b.y + a.z * b.z;
}

struct ViewItsnParams {
    float coefs[2];
    size_t len;
};

size_t intersect_ray_sphere(struct Point3d o, struct Point3d d, struct Sphere s, struct ViewItsnParams* outpts) {
    float a, b, c, discriminant, t1, t2;

    struct Point3d co = o;
    co.x -= s.center.x;
    co.y -= s.center.y;
    co.z -= s.center.z;

    a = dot3d(d, d);
    b = 2.0*dot3d(co, d);
    c = dot3d(co, co) - (s.radius*s.radius);

    discriminant = b*b - 4*a*c;
    if (discriminant < 0) {
        outpts->len = 0;
        return 0;
    }

    t1 = (-b + sqrtf(discriminant)) / (2*a);
    t2 = (-b - sqrtf(discriminant)) / (2*a);

    if (t1 != t2) {
        outpts->coefs[0] = t1;
        outpts->coefs[1] = t2;
        outpts->len = 2;
        return 2;
    } else {
        outpts->coefs[0] = t1;
        outpts->len = 1;
        return 1;
    }
}

struct RGBA trace_ray(struct Point3d o, struct Point3d d, float t_min, float t_max) {
    assert(t_min < t_max);
    float closest_t = INFINITY;
    /* struct Sphere closest_sphere; */
    size_t closest_sphere_idx;
    struct Sphere sphere;
    /* struct RGBA color = { 0 }; */
    struct ViewItsnParams itsn_params;
    float t;
    size_t i;

    closest_sphere_idx = NUMBER_SPHERES;
    for (i = 0; i < NUMBER_SPHERES; i++) {
        sphere = spheres[i];
        intersect_ray_sphere(o, d, sphere, &itsn_params);
        if (itsn_params.len >= 1) {
            t = itsn_params.coefs[0];
            if ((t >= t_min) && (t <= t_max) && (t < closest_t)) {
                closest_t = t;
                closest_sphere_idx = i;
            }
        }
        if (itsn_params.len > 1) {
            t = itsn_params.coefs[1];
            if ((t >= t_min) && (t <= t_max) && (t < closest_t)) {
                closest_t = t;
                closest_sphere_idx = i;
            }
        }
    }

    if (closest_sphere_idx < NUMBER_SPHERES) {
        return spheres[closest_sphere_idx].color;
    } else {
        return BACKGROUND_COLOR;
    }
}

int main() {
    /* 1. Initialize SDL */
    /*
    SDL_INIT_VIDEO : constant :=16#00000020#;
    SDL_WINDOWPOS_CENTERED : constant := 16#2FFF0000#;
    SDL_RENDERER_ACCELERATED : constant := 16#00000002#;
    SDL_EVENTTYPE_QUIT : constant := 16#100#;
    */
    struct ScreenDims sdims = { SCREEN_WIDTH, SCREEN_HEIGHT }; /*{ 640u, 480u }; */
    struct CanvasDims cdims = { SCREEN_WIDTH, SCREEN_HEIGHT };
    size_t i, j;
    struct ScreenPoint spt;
    struct CanvasPoint cpt;
    struct Point3d vpt;
    struct RGBA color;

    printf("sizeof(Sphere): %lu\n", sizeof(struct Sphere));
    /* TODO: Remove */
    printf("SDL_INIT_VIDEO: x%08x\n", SDL_INIT_VIDEO);

    if (SDL_Init(SDL_INIT_VIDEO) != 0) { /* Initialize the video subsystem */
        SDL_Log("Unable to initialize SDL: %s", SDL_GetError());
        return 1;
    }

    /* 2. Create a window and renderer */
    SDL_Window* window = SDL_CreateWindow(
        "SDL2 Simple Example",             /* window title */
        SDL_WINDOWPOS_CENTERED,            /* x position */
        SDL_WINDOWPOS_CENTERED,            /* y position */
        SCREEN_WIDTH,                      /* width */
        SCREEN_HEIGHT,                     /* height */
        0                                  /* flags (0 for default) */
    );

    if (window == NULL) {
        SDL_Log("Could not create window: %s", SDL_GetError());
        SDL_Quit();
        return 1;
    }

    SDL_Renderer* renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED); /* Create a renderer for the window */
    if (renderer == NULL) {
        SDL_DestroyWindow(window);
        SDL_Quit();
        return 1;
    }


    /* 3. The event loop */
    int keep_going = 1;
    SDL_Event event;
    while (keep_going) {
        while (SDL_PollEvent(&event)) { /* Poll for events */
            if (event.type == SDL_QUIT) { /* Check for the window close event */
                printf("Got quit event with timestamp %d\n", event.quit.timestamp);
                keep_going = 0;
            }
        }

        /* Rendering */
        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255); /* Set draw color to black (RGBA) */
        SDL_RenderClear(renderer);                      /* Clear the renderer with the current color */

        struct Point3d origin = { 0.0, 0.0, 0.0 };
        for(i = 0; i < SCREEN_WIDTH; i++) {
            for (j = 0; j < SCREEN_HEIGHT; j++) {
                spt.x = i;
                spt.y = j;
                cpt = screen_to_canvas_point(&sdims, spt);
                vpt = canvas_to_viewport(&cdims, &viewport_config, cpt);
                color = trace_ray(origin, vpt, 1.0, INFINITY);
                put_pixel(renderer, i, j, color.red, color.green, color.blue, color.alpha);
            }
        }

        /*
        for(i = (640 / 4); i < (3 * 640 / 4); i++) {
            for (j = 0*(480 / 4); j < (4 * 480 / 4); j++) {
                put_pixel(renderer, i, j, 255, 0, 0, 255);
            }
        }
        */
        /* Add drawing code here (e.g., lines, shapes, textures) */
        SDL_RenderPresent(renderer);                   /* Update the screen */
        sleep_ms(1.0 / 30.0);
    }

    /* 4. Clean up */
    SDL_DestroyRenderer(renderer); /* Destroy renderer */
    SDL_DestroyWindow(window);     /* Destroy window */
    SDL_Quit();                    /* Quit SDL subsystems */

    return 0;
}
