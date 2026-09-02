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
#ifndef INC_GYM_H
#define INC_GYM_H

#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

struct RomMD5Hash {
    const char* rom_file;
    char md5[33];
};

extern struct RomMD5Hash g_rom_md5[108];

typedef unsigned char pixel_t;
typedef unsigned int game_mode_t;
typedef unsigned int difficulty_t;

/* Define actions */
enum AtariAction {
  PLAYER_A_NOOP          = 0,
  PLAYER_A_FIRE          = 1,
  PLAYER_A_UP            = 2,
  PLAYER_A_RIGHT         = 3,
  PLAYER_A_LEFT          = 4,
  PLAYER_A_DOWN          = 5,
  PLAYER_A_UPRIGHT       = 6,
  PLAYER_A_UPLEFT        = 7,
  PLAYER_A_DOWNRIGHT     = 8,
  PLAYER_A_DOWNLEFT      = 9,
  PLAYER_A_UPFIRE        = 10,
  PLAYER_A_RIGHTFIRE     = 11,
  PLAYER_A_LEFTFIRE      = 12,
  PLAYER_A_DOWNFIRE      = 13,
  PLAYER_A_UPRIGHTFIRE   = 14,
  PLAYER_A_UPLEFTFIRE    = 15,
  PLAYER_A_DOWNRIGHTFIRE = 16,
  PLAYER_A_DOWNLEFTFIRE  = 17,
  PLAYER_B_NOOP          = 18,
  PLAYER_B_FIRE          = 19,
  PLAYER_B_UP            = 20,
  PLAYER_B_RIGHT         = 21,
  PLAYER_B_LEFT          = 22,
  PLAYER_B_DOWN          = 23,
  PLAYER_B_UPRIGHT       = 24,
  PLAYER_B_UPLEFT        = 25,
  PLAYER_B_DOWNRIGHT     = 26,
  PLAYER_B_DOWNLEFT      = 27,
  PLAYER_B_UPFIRE        = 28,
  PLAYER_B_RIGHTFIRE     = 29,
  PLAYER_B_LEFTFIRE      = 30,
  PLAYER_B_DOWNFIRE      = 31,
  PLAYER_B_UPRIGHTFIRE   = 32,
  PLAYER_B_UPLEFTFIRE    = 33,
  PLAYER_B_DOWNRIGHTFIRE = 34,
  PLAYER_B_DOWNLEFTFIRE  = 35,
  RESET                  = 40, /* MGB: Use SYSTEM_RESET to reset the environment. */
  UNDEFINED              = 41,
  RANDOM                 = 42,
  SAVE_STATE             = 43,
  LOAD_STATE             = 44,
  SYSTEM_RESET           = 45,
  LAST_ACTION_INDEX      = 50
};

struct AtariEnvStepMetadata {
    int lives;
    int episode_frame_number;
    int frame_number;
    unsigned int c_seed;
    int ale_seed;
};

/* Note in the following that RGBArray is not supported */
enum GymRenderMode {
    RENDER_HUMAN,
    NO_RENDER
};

/* Observation is an RGB observation */
struct RGBObservation {
    pixel_t rgb_array[210][160][3];
};

/* Frameskip values of (low, high) will enable stochastic frame skip
   which will sample a random frameskip uniformly each action.
*/
enum FrameskipKind {
    FRAMESKIP_VALUE,
    FRAMESKIP_TUPLE
};

struct FrameskipLowHigh {
    unsigned int low;
    unsigned int high;
};

union FrameskipParams {
    unsigned int value;
    struct FrameskipLowHigh tuple;
};

struct Frameskip {
    enum FrameskipKind tag;
    union FrameskipParams params;
};

/* For mode (Game mode) and difficulty (Game difficulty), see
 * Machado et al., 2018 */
/* We omit the continuous action variables from the configuration */
struct AtariConfig {
    char* rom_dir;
    char* rom_name;
    game_mode_t mode;
    difficulty_t difficulty;
    struct Frameskip frameskip;
    float repeat_action_probability;
    bool full_action_space;
    int max_num_frames_per_episode;
    enum GymRenderMode render_mode;
    bool sound_obs;
};

struct AtariEnvParams {
    game_mode_t mode;
    difficulty_t difficulty;
    struct Frameskip frameskip;
    float repeat_action_probability;
    bool full_action_space;
    int max_num_frames_per_episode;
    enum GymRenderMode render_mode;
    bool sound_obs;
};

struct AtariEnv;

struct AtariRGBStepReturn {
    struct RGBObservation observation;
    float reward;
    bool terminated;
};

enum RomPathError {
    ROM_PATH_OK,
    ROM_MD5_HASH_NOT_FOUND,
    ROM_MD5_HASH_MISMATCH,
    ROM_MD5_HASH_CALCULATION_FAILED
};

/* get_rom_path not only concatenates the rom directory and name to form a file
 * name, but also calculates the MD5 hash for the file and compares it to a
 * value stored in this library. */
enum RomPathError get_rom_path(const char* rom_dir, const char* rom_name, size_t buf_size, char* rom_file_out);

/* Supporting methods which could be useful when working
 * with these environments */
struct AtariConfig default_atari_env_config_init();

/* Gymnasium methods */
/* We provide two reset methods depending on whether a seed is provided */
struct AtariEnvStepMetadata atari_get_info(struct AtariEnv* env);
struct AtariEnv* atari_make(struct AtariConfig config);
void atari_destroy(struct AtariEnv* env);
struct RGBObservation atarirgb_reset_omit_seed(struct AtariEnv* env);
struct RGBObservation atarirgb_reset(struct AtariEnv* env, unsigned int seed);
struct AtariRGBStepReturn atarirgb_step(struct AtariEnv* env, enum AtariAction action);
enum AtariAction atari_random_action(struct AtariEnv* env);

#ifdef __cplusplus
}
#endif

#endif
