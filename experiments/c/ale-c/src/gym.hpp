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
#ifndef INC_GYM_HPP
#define INC_GYM_HPP

#include <climits>
#include "ale_c.hpp"


struct RomMD5Hash {
    const char* rom_file;
    char md5[33];
};

extern struct RomMD5Hash g_rom_md5[108];

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

struct AtariEnv {
    ALEInterface* aleptr;
    char* rom_file_path;
    unsigned int c_seed;
    int ale_seed;
    struct AtariEnvParams params;
};

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

extern "C" {
    /* Supporting methods which could be useful when working
     * with these environments */
    struct AtariConfig default_atari_env_config_init();
    enum RomPathError load_game(ALEInterface* aleptr, struct AtariConfig config);
    void atari_load_game_from_rom_file(
        ALEInterface* aleptr, const char* rom_file, struct AtariConfig config
    );

    /* Gymnasium methods */
    /* We provide two reset methods depending on whether a seed is provided */
    AtariEnvStepMetadata atari_get_info(AtariEnv* env);
    AtariEnv* atari_make(struct AtariConfig config);
    void atari_destroy(AtariEnv* env);
    RGBObservation atarirgb_reset_omit_seed(AtariEnv* env);
    RGBObservation atarirgb_reset(AtariEnv* env, unsigned int seed);
    struct AtariRGBStepReturn atarirgb_step(AtariEnv* env, enum Action action);
    enum Action atari_random_action(AtariEnv* env);
}

#endif
