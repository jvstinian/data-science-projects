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
#include "gym.hpp"
#include <cassert>

#include <openssl/evp.h>

#define BUFFER_SIZE 4096

/* Function to convert binary digest to a hex string */
static void md5_to_string(const unsigned char *digest, size_t digest_len, char *output_str) {
    assert(digest_len == 16);
    for (int i = 0; i < 16; i++) {
        /* %02x formats each byte as a 2-character lowercase hex string 
         * sprintf automatically appends the null terminator '\0' at the very end */
        sprintf(&output_str[i * 2], "%02x", digest[i]);
    }
    output_str[32] = '\0';
}

static int calculate_file_md5(const char *filename, char *md5_hex_out) {
    size_t digest_size = EVP_MD_size(EVP_md5());
    unsigned char digest[digest_size];
    /*
    FILE *file = fopen(filename, "rb");
    if (file == NULL) {
        return 1; // Failed to open file
    }

    MD5_CTX md5_context;
    MD5_Init(&md5_context);

    unsigned char buffer[BUFFER_SIZE];
    size_t bytes_read;

    // Read the file in chunks and stream it to the MD5 context
    while ((bytes_read = fread(buffer, 1, BUFFER_SIZE, file)) != 0) {
        MD5_Update(&md5_context, buffer, bytes_read);
    }

    MD5_Final(digest, &md5_context);
    fclose(file);

    md5_to_string(digest, md5_hex_out);
    return 0; // Success
    */
    EVP_MD_CTX *mdctx = EVP_MD_CTX_new();
    unsigned int digest_len;

    if (mdctx == NULL) {
        fprintf(stderr, "calculate_file_md5: unable to create context");
        return 1;
    }

    /* Initialize the context with the MD5 algorithm */
    if (!EVP_DigestInit_ex(mdctx, EVP_md5(), NULL)) {
        fprintf(stderr, "calculate_file_md5: unable to initialize context");
        EVP_MD_CTX_free(mdctx);
        return 1;
    }
    
    FILE *file = fopen(filename, "rb");
    if (file == NULL) {
        fprintf(stderr, "calculate_file_md5: unable to open rom file");
        EVP_MD_CTX_free(mdctx);
        return 1;
    }
    
    unsigned char buffer[BUFFER_SIZE];
    size_t bytes_read;

    /* Read the file in chunks and stream it to the MD5 context */
    while ((bytes_read = fread(buffer, 1, BUFFER_SIZE, file)) != 0) {
        /* Feed data into the hash context */
        if(!EVP_DigestUpdate(mdctx, buffer, bytes_read)) {
            fprintf(stderr, "calculate_file_md5: unable to update digest");
            fclose(file);
            EVP_MD_CTX_free(mdctx);
            return 1;
        }
    }

    /* Finalize and extract the hash result */
    if(!EVP_DigestFinal_ex(mdctx, digest, &digest_len)) {
        fprintf(stderr, "calculate_file_md5: unable to extract hash result");
        fclose(file);
        EVP_MD_CTX_free(mdctx);
        return 1;
    }

    /* Clean up allocated context memory */
    EVP_MD_CTX_free(mdctx);
    fclose(file);

    md5_to_string(digest, digest_len, md5_hex_out);
    return 0;
}

static int compare_rom_for_md5_hash(const char* rom_file, const struct RomMD5Hash* rom_hash) {
    return strcmp(rom_file, rom_hash->rom_file);
}

static int void_compare_rom_for_md5_hash(const void* a, const void* b) {
    return compare_rom_for_md5_hash((const char*) a, (const struct RomMD5Hash*) b);
}

/* compare_rom_filenames and void_compare_rom_filenames are
 * used for sorting g_rom_md5 */
static int compare_rom_filenames(const struct RomMD5Hash* rh1, const struct RomMD5Hash* rh2) {
    return strcmp(rh1->rom_file, rh2->rom_file);
}

static int void_compare_rom_filenames(const void* a, const void* b) {
    return compare_rom_filenames((const struct RomMD5Hash*) a, (const struct RomMD5Hash*) b);
}

enum RomPathError get_rom_path(const char* rom_dir, const char* rom_name, size_t buf_size, char* rom_file_out) {
    /* +5 for "/" and ".bin" */
    assert(buf_size >= strlen(rom_dir) + strlen(rom_name) + 5 + 1);
    size_t bin_file_start;

    strcpy(rom_file_out, rom_dir);
    if ((strlen(rom_dir) != 0) && (rom_dir[strlen(rom_dir) - 1] != '/')) {
        strcat(rom_file_out, "/");
    }
    /* We record the current position as this is the start of the rom file name */
    bin_file_start = strlen(rom_file_out);
    strcat(rom_file_out, rom_name);
    strcat(rom_file_out, ".bin");

    /* Sort g_rom_md5 */
    qsort(g_rom_md5, 108, sizeof(RomMD5Hash), void_compare_rom_filenames);

    const struct RomMD5Hash* rom_hash_match = (struct RomMD5Hash*) bsearch(
            rom_file_out + bin_file_start,
            g_rom_md5,
            108,
            sizeof(RomMD5Hash),
            void_compare_rom_for_md5_hash
    );
    if (rom_hash_match == NULL) {
        fprintf(stderr, "get_rom_path: md5 hash not found for rom %s", rom_file_out + bin_file_start);
        return ROM_MD5_HASH_NOT_FOUND;
    }
    char exp_hash[33];
    memcpy(exp_hash, rom_hash_match->md5, sizeof(exp_hash));
    char act_hash[33];
    if (calculate_file_md5(rom_file_out, act_hash)) {
        fprintf(stderr, "get_rom_path: error calculating file md5 hash");
        return ROM_MD5_HASH_CALCULATION_FAILED;
    }
    if (strcmp(exp_hash, act_hash)) {
        fprintf(stderr, "get_rom_path: actual rom file md5 hash does not match the expected value");
        return ROM_MD5_HASH_MISMATCH;
    }

    return ROM_PATH_OK;
}

struct AtariConfig default_atari_env_config_init() {
    struct Frameskip default_frameskip = {
        .tag = FRAMESKIP_VALUE, .params = (union FrameskipParams) { .value = 4 } 
    };
    
    return (struct AtariConfig) {
        .rom_dir = NULL, .rom_name = NULL,
        .mode = ~0u, .difficulty = ~0u,
        .frameskip = default_frameskip,
        .repeat_action_probability = 0.25,
        .full_action_space = false,
        .max_num_frames_per_episode = INT_MAX,
        .render_mode = NO_RENDER,
        .sound_obs = false
    };
}

/* This method follows the python implementation.
 * In the python implementation of reset, the load_game method is called
 * whenever a seed is passed to reset.
 * We probably don't need to call get_rom_path each time, but separating
 * this method  from atari_load_game would require some refactoring.
 * For instance, we'd need to call get_rom_path once in the atari_make
 * and store the resulting full rom file path so that it could be
 * used in atari_load_game.
 * For now we follow the python implementation, with the exception
 * of some of the seed handling. */
enum RomPathError atari_load_game(ALEInterface* aleptr, struct AtariConfig config) {
    /* +5 for "/" and ".bin" */
    size_t buf_size = strlen(config.rom_dir) + strlen(config.rom_name) + 5 + 1;
    char rom_file[buf_size];
    enum RomPathError status = ROM_PATH_OK;

    if ((status = get_rom_path(config.rom_dir, config.rom_name, buf_size, rom_file))) {
        fprintf(stderr, "load_game: could not determinae a valid rom path");
        return status;
    }
    ale_load_rom(aleptr, rom_file);
    if (config.mode != (~0u)) {
        ale_set_mode(aleptr, config.mode);
    }
    if (config.difficulty != (~0u)) {
        ale_set_difficulty(aleptr, config.difficulty);
    }
    return status;
}

static struct AtariEnvParams atari_config_to_params(struct AtariConfig config) {
    return (struct AtariEnvParams) {
        .mode = config.mode,
        .difficulty = config.difficulty,
        .frameskip = config.frameskip,
        .repeat_action_probability = config.repeat_action_probability,
        .full_action_space = config.full_action_space,
        .max_num_frames_per_episode = config.max_num_frames_per_episode,
        .render_mode = config.render_mode,
        .sound_obs = config.sound_obs
    };
}

/* NOTE: We omit the seed_game method and basically inline our modifications of
 * that function where needed. */
AtariEnv* atari_make(struct AtariConfig config) {
    struct AtariEnv* env = (struct AtariEnv*) malloc(sizeof(struct AtariEnv));
    if (env == NULL) {
        fprintf(stderr, "atari_make: unable to allocate environment");
        return NULL;
    }
    env->aleptr = ale_interface_new();
    if (env->aleptr == NULL) {
        fprintf(stderr, "atari_make: unable to create a new ALE interface");
        free(env);
        return NULL;
    }
    struct AtariEnvParams params = atari_config_to_params(config);
    env->params = params;

    ale_set_float(env->aleptr, "repeat_action_probability", params.repeat_action_probability);
    if (params.max_num_frames_per_episode < INT_MAX) {
        ale_set_int(env->aleptr, "max_num_frames_per_episode", params.max_num_frames_per_episode);
    }

    /* If render mode is human we can display screen and sound */
    switch (params.render_mode) {
        case RENDER_HUMAN:
             ale_set_bool(env->aleptr, "display_screen", true);
             ale_set_bool(env->aleptr, "sound", true);
             break;
        case NO_RENDER:
             break;
    };

    ale_set_bool(env->aleptr, "sound_obs", params.sound_obs);

    if (atari_load_game(env->aleptr, config)) {
        fprintf(stderr, "atari_make: unable to load game");
        ale_interface_delete(env->aleptr);
        free(env);
        return NULL;
    }
    return env;
}

AtariEnvStepMetadata atari_get_info(AtariEnv* env) {
    return (struct AtariEnvStepMetadata) {
        .lives = ale_lives(env->aleptr),
        .episode_frame_number = ale_get_episode_frame_number(env->aleptr),
        .frame_number = ale_get_frame_number(env->aleptr),
        .c_seed = env->c_seed,
        .ale_seed = env->ale_seed
    };
}

RGBObservation atarirgb_reset_default_seed(AtariEnv* env) {
    unsigned int seed = time(NULL);
    return atarirgb_reset(env, seed);
}

RGBObservation atarirgb_reset(AtariEnv* env, unsigned int seed) {
    env->c_seed = seed;
    struct RGBObservation obs;
    /* In the python code, if seed is not none, then
     * the following is run: 
           seeded_with = self.seed_game(seed)
           self.load_game()
    */
    /* Set the seed */
    srand(env->c_seed);
    env->ale_seed = (int) rand();
    printf("ale seed: %d\n", env->ale_seed); /* TODO */
    /* TODO: It appears that for the ale_seed to take effect, the rom
     * must be reloaded */
    ale_set_int(env->aleptr, "random_seed", env->ale_seed);
    ale_reset_game(env->aleptr);

    /* The following assumes the screen is an array of size (210, 160). */
    ale_get_rgb_array(env->aleptr, (pixel_t*) obs.rgb_array);

    return obs;
}

void atari_destroy(AtariEnv* env) {
    ale_interface_delete(env->aleptr);
    free(env);
}

struct AtariRGBStepReturn atarirgb_step(AtariEnv* env, enum Action action) {
    unsigned int fskip;
    unsigned int i;
    unsigned int low, high;
    float strength = 1.0f;
    bool terminal = false, truncated = false;
    struct AtariRGBStepReturn ret;
    ret.terminated = false;
    ret.reward = 0.0f;

    switch (env->params.frameskip.tag) {
        case FRAMESKIP_VALUE:
            fskip = env->params.frameskip.params.value;
            break;
        case FRAMESKIP_TUPLE:
            low = env->params.frameskip.params.tuple.low;
            high = env->params.frameskip.params.tuple.high;
            fskip = low + (rand() % (1 + high - low));
            break;
        default:
            fskip = 4;
            break;
    }
    for (i = 0; i < fskip; i++) {
        ret.reward += (float) ale_act(env->aleptr, action, strength);
    }
    terminal = ale_game_over(env->aleptr, false);
    truncated = ale_game_truncated(env->aleptr);

    ret.terminated = terminal || truncated;
    /* The following assumes the screen is an array of size (210, 160). */
    ale_get_rgb_array(env->aleptr, (pixel_t*) ret.observation.rgb_array);

    return ret;
}
    
enum Action atari_random_action(AtariEnv* env) {
    enum Action actions[PLAYER_A_MAX];
    size_t fulllen;

    if (env->params.full_action_space) {
        fulllen = ale_get_legal_action_set(env->aleptr, actions, PLAYER_A_MAX);
    } else {
        fulllen = ale_get_minimal_action_set(env->aleptr, actions, PLAYER_A_MAX);
    }
    assert(fulllen <= PLAYER_A_MAX);

    return actions[rand() % fulllen];
}
