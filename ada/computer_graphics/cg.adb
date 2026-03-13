with Interfaces.C; use Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;
with System;
with Ada.Command_Line; use Ada.Command_Line;
with Interfaces.C.Strings; use Interfaces.C.Strings;

-- https://github.com/libsdl-org/SDL/blob/SDL2/include/SDL.h
-- http://www.ada-auth.org/standards/22rm/html/rm-b-3.html

procedure Main is
    SDL_INIT_VIDEO : constant :=16#00000020#;
    SDL_WINDOWPOS_CENTERED : constant := 16#2FFF0000#;
    SDL_RENDERER_ACCELERATED : constant := 16#00000002#;

    SDL_EVENTTYPE_QUIT : constant := 16#100#;

    function SDL_Init(flags: unsigned) return int
      with Import => true,
           Convention => C,
           External_Name => "SDL_Init";

    procedure SDL_Quit
      with Import => true,
           Convention => C,
           External_Name => "SDL_Quit";

    procedure SDL_Log
        ( format : char_array;
          value  : chars_ptr ) -- char_array )
        with Import,
             Convention    => C_Variadic_1,
             External_Name => "SDL_Log";

    function SDL_GetError return chars_ptr -- char_array
        with Import,
             Convention    => C,
             External_Name => "SDL_GetError";

    subtype SDL_Window_Access is System.Address;
    function SDL_CreateWindow
        ( title: char_array;
          x, y, w, h: int;
          flags: unsigned ) return SDL_Window_Access
        with Import,
             Convention    => C,
             External_Name => "SDL_CreateWindow";

    procedure SDL_DestroyWindow
        ( window : SDL_Window_Access )
        with Import,
             Convention    => C,
             External_Name => "SDL_DestroyWindow";

    subtype SDL_Renderer_Access is System.Address;
    function SDL_CreateRenderer
        ( window: SDL_Window_Access;
          index: int;
          flags: unsigned ) return SDL_Renderer_Access
        with Import,
             Convention    => C,
             External_Name => "SDL_CreateRenderer";

    procedure SDL_DestroyRenderer
        ( renderer : SDL_Renderer_Access )
        with Import,
             Convention    => C,
             External_Name => "SDL_DestroyRenderer";

    function SDL_SetRenderDrawColor
        ( renderer : SDL_Renderer_Access;
          r, g, b, a: unsigned_short ) return int
        with Import,
             Convention    => C,
             External_Name => "SDL_SetRenderDrawColor";

    function SDL_RenderClear
        ( renderer : SDL_Renderer_Access ) return int
        with Import,
             Convention    => C,
             External_Name => "SDL_RenderClear";

    procedure SDL_RenderPresent
        ( renderer : SDL_Renderer_Access )
        with Import,
             Convention    => C,
             External_Name => "SDL_RenderPresent";

    function SDL_RenderDrawPoint
        ( renderer : SDL_Renderer_Access;
          x, y : int ) return int  -- /nix/store/6vl9b59i822mh3zmri5g4kywahzhp5zw-sdl2-compat-2.32.56-dev/include/SDL2/SDL_render.h:1191
        with Import => True, 
        Convention => C, 
        External_Name => "SDL_RenderDrawPoint";
    
    procedure PutPixel(
        renderer: SDL_Renderer_Access;
        x, y: int;
        r, g, b, a: unsigned_short ) is
        unused_status: int;
    begin
        unused_status := SDL_SetRenderDrawColor(renderer, r, g, b, a);
        unused_status := SDL_RenderDrawPoint(renderer, x, y);
    end PutPixel; 

    -- Define types and constants for the discriminant
    type Discriminant_Type is (TYPE_TAG, PADDING_TAG);
    Default_Tag : constant Discriminant_Type := TYPE_TAG;
    -- subtype SDL_Event is unsigned; -- TODO: This needs to be an unchecked union
    type SDL_Event (Tag : Discriminant_Type := Default_Tag) is record
      case Tag is
         when TYPE_TAG =>
            eventtype : unsigned;
         when PADDING_TAG =>
            padding : unsigned_long_long;
      end case;
    end record
    with Convention => C, -- This pragma/aspect is crucial
         Unchecked_Union;  -- Tells the compiler to treat it as a C union
    type SDL_Event_Access is access all SDL_Event;

    function SDL_PollEvent
        ( event: SDL_Event_Access ) return int 
        with Import,
             Convention    => C,
             External_Name => "SDL_PollEvent";

    keep_going: Boolean := True;
    event: aliased SDL_Event;
    event_ptr: SDL_Event_Access  := event'Access;
    val: int;
    window: SDL_Window_Access;
    renderer: SDL_Renderer_Access;
    unused_status: int;
begin
    Put_Line("SDL_INIT_VIDEO: " & SDL_INIT_VIDEO'Image);

    -- 1. Initialize SDL
    val := SDL_Init(SDL_INIT_VIDEO);
    if val /= 0 then
        SDL_Log(To_C("Unable to initialize SDL: %s"), SDL_GetError);
    end if;
    Put_Line("SDL_INIT_VIDEO return: " & val'Image);
    SDL_Log(To_C("Initialized SDL: %s"), New_String("example string"));
    window := SDL_CreateWindow(To_C("SDL2 Simple Example"), SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 640, 480, 0);
    if System."="(window, System.Null_Address) then
        SDL_Log(To_C("Could not create window: %s"), SDL_GetError);
        SDL_Quit;
        Set_Exit_Status(Failure);
        return;
    end if;
    renderer := SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
    if System."="(renderer, System.Null_Address) then
        SDL_Log(To_C("Could not create renderer: %s"), SDL_GetError);
        SDL_DestroyWindow(window);
        SDL_Quit;
        Set_Exit_Status(Failure);
        return;
    end if;

    while keep_going loop
        -- keep_going := False;
        while SDL_PollEvent(event_ptr) /= 0 loop
            if event.eventtype = SDL_EVENTTYPE_QUIT then  -- should be event.type
                keep_going := False;
                Put_Line("Got quit event");
            else
                Put_Line("Got event type " & event.eventtype'Image);
            end if;
        end loop;
        
        --- Rendering ---
        unused_status := SDL_SetRenderDrawColor(renderer, 0, 0, 255, 255); -- Set draw color to black (RGBA)
        unused_status := SDL_RenderClear(renderer);                      -- Clear the renderer with the current color
        for I in (640 / 4) .. (3 * 640 / 4) loop
            for J in (480 / 4) .. (3 * 480 / 4) loop
                PutPixel(renderer, int(I), int(J), 255, 0, 0, 255);
            end loop;
        end loop;
        -- Add drawing code here (e.g., lines, shapes, textures)
        SDL_RenderPresent(renderer);                    -- Update the screen
        delay (1.0 / 30.0);
    end loop;
    
    -- delay 2.0;

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit;
end Main;

-- #include <SDL2/SDL.h>
-- 
-- int main(int argc, char* argv[]) {
--     // 1. Initialize SDL
--     if (SDL_Init(SDL_INIT_VIDEO) != 0) { // Initialize the video subsystem
--         SDL_Log("Unable to initialize SDL: %s", SDL_GetError());
--         return 1;
--     }
-- 
--     // 2. Create a window and renderer
--     SDL_Window* window = SDL_CreateWindow(
--         "SDL2 Simple Example",             // window title
--         SDL_WINDOWPOS_CENTERED,            // x position
--         SDL_WINDOWPOS_CENTERED,            // y position
--         640,                               // width
--         480,                               // height
--         0                                  // flags (0 for default)
--     );
-- 
--     if (window == NULL) {
--         SDL_Log("Could not create window: %s", SDL_GetError());
--         SDL_Quit();
--         return 1;
--     }
-- 
--     SDL_Renderer* renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED); // Create a renderer for the window
--     if (renderer == NULL) {
--         SDL_DestroyWindow(window);
--         SDL_Quit();
--         return 1;
--     }
-- 
--     // 3. The event loop
--     int quit = 0;
--     while (!quit) {
--         SDL_Event event;
--         while (SDL_PollEvent(&event)) { // Poll for events
--             if (event.type == SDL_QUIT) { // Check for the window close event
--                 quit = 1;
--             }
--         }
-- 
--         // --- Rendering ---
--         SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255); // Set draw color to black (RGBA)
--         SDL_RenderClear(renderer);                     // Clear the renderer with the current color
-- 
--         // Add drawing code here (e.g., lines, shapes, textures)
-- 
--         SDL_RenderPresent(renderer);                   // Update the screen
--     }
-- 
--     // 4. Clean up
--     SDL_DestroyRenderer(renderer); // Destroy renderer
--     SDL_DestroyWindow(window);     // Destroy window
--     SDL_Quit();                    // Quit SDL subsystems
-- 
--     return 0;
-- }
