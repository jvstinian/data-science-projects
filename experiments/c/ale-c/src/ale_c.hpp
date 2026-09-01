/* ALE C Bindings
 * Copyright (C) 2026 Justin Smith
 * Released under the GNU General Public License; see LICENSE.md for details.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <https://www.gnu.org/licenses/>.
 * **************************************************************************
 *
 * Based on: A.L.E (Arcade Learning Environment)
 * Copyright (c) 2009-2013 by Yavar Naddaf, Joel Veness, Marc G. Bellemare and
 *   the Reinforcement Learning and Artificial Intelligence Laboratory
 * Released under the GNU General Public License; see License.txt for details.
 *
 * Based on: Stella  --  "An Atari 2600 VCS Emulator"
 * Copyright (c) 1995-2007 by Bradford W. Mott and the Stella team
 */
#ifndef INC_ALE_C_HPP
#define INC_ALE_C_HPP

#include <cstddef>
#include <cstdbool>
#include <string>
#include <ale_interface.hpp>

/*
enum Action, game_mode_t, and difficulty_t
are defined in ale/common/Constants.h.
We extract them from the ale namespace.

Note that game_mode_t, difficulty_t, and pixel_t
are defined in gym.h as well.
*/
using ale::ALEInterface;
using ale::game_mode_t; /* unsigned int */
using ale::difficulty_t; /* unsigned int */
using ale::Action;
using ale::pixel_t;  /* unsigned char */

struct Screen {
    size_t height;
    size_t width;
    unsigned short channels;
    pixel_t* screen;
};

extern "C" {
    size_t get_welcome_message_length();
    size_t get_welcome_message(char* msgp, size_t n);

    /* Create a new ALEInterface */
    ALEInterface* ale_interface_new();

    /* Destroy an ALEInterface */
    void ale_interface_delete(ALEInterface* ale_ptr);

    /* ale_get_string obtains the value for the key from the ALE interface,
     * writing the result to val.  We follow the snprintf format here,
     * i.e. n is the buffer size of val including the terminating null
     * character, while the return value is the number of characters that
     * would have been written if n had been sufficiently large,
     * not counting the terminating null character.
     */
    int ale_get_string(ALEInterface* aleptr, const char* key, char* val, size_t n);
    int ale_get_int(ALEInterface* aleptr, const char* key);
    bool ale_get_bool(ALEInterface* aleptr, const char* key);
    float ale_get_float(ALEInterface* aleptr, const char* key);
    void ale_set_string(ALEInterface* aleptr, const char* key, const char* val);
    void ale_set_int(ALEInterface* aleptr, const char* key, int val);
    void ale_set_bool(ALEInterface* aleptr, const char* key, bool val);
    void ale_set_float(ALEInterface* aleptr, const char* key, float val);
    void ale_load_rom(ALEInterface* aleptr, const char* rompath);
    int ale_act(ALEInterface* aleptr, Action act, float paddle_strength);
    bool ale_game_over(ALEInterface* aleptr, bool with_trunc);
    bool ale_game_truncated(ALEInterface* aleptr);
    void ale_reset_game(ALEInterface* aleptr);
    int ale_lives(ALEInterface* aleptr);

    size_t ale_get_available_modes_length(ALEInterface* aleptr);
    /* ale_get_available_modes returns the length of the game modes vector, i.e. the 
     * length of the output if n had been sufficiently large. */
    size_t ale_get_available_modes(ALEInterface* aleptr, game_mode_t* modes_out, size_t n);
    void ale_set_mode(ALEInterface* aleptr, game_mode_t gamemode);
    game_mode_t ale_get_mode(ALEInterface* aleptr);
    size_t ale_get_available_difficulties_length(ALEInterface* aleptr);
    /* ale_get_available_difficulties returns the length of the difficulties vector, i.e. the 
     * length of the output if n had been sufficiently large. */
    size_t ale_get_available_difficulties(ALEInterface* aleptr, difficulty_t* difficulties_out, size_t n);
    void ale_set_difficulty(ALEInterface* aleptr, difficulty_t difficulty);
    difficulty_t ale_get_difficulty(ALEInterface* aleptr);
    size_t ale_get_legal_action_set_length(ALEInterface* aleptr);
    /* ale_get_legal_action_set returns the length of the vector of legal actions, i.e. the 
     * length of the output if n had been sufficiently large. */
    size_t ale_get_legal_action_set(ALEInterface* aleptr, Action* actions_out, size_t n);
    size_t ale_get_minimal_action_set_length(ALEInterface* aleptr);
    /* ale_get_minimal_action_set returns the length of the minimal action set, i.e. the 
     * length of the output if n had been sufficiently large. */
    size_t ale_get_minimal_action_set(ALEInterface* aleptr, Action* actions_out, size_t n);
    int ale_get_frame_number(ALEInterface* aleptr);
    int ale_get_episode_frame_number(ALEInterface* aleptr);
    int ale_get_max_num_frames(ALEInterface* aleptr);
    void ale_get_screen_dims(ALEInterface* aleptr, size_t* heightp, size_t* widthp);
    void ale_get_screen_size(ALEInterface* aleptr, struct Screen* screen);
    void ale_get_screen_grayscale(ALEInterface* aleptr, struct Screen* screen);
    void ale_get_screen_rgb(ALEInterface* aleptr, struct Screen* screen);
    /* ale_get_rgb_array assumes rgbs_out is sufficiently large to
     * hold the contents of the screen.
     * This function is provided to avoid allocations associated with
     * the struct Screen type. */
    void ale_get_rgb_array(ALEInterface* aleptr, pixel_t* rgbs_out);
    void ale_save_screen_png(ALEInterface* aleptr, const char* filename);
}

#endif
