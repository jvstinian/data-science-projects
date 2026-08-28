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
#include <iostream>
#include <unistd.h>
#include <climits>
#include <ale_c.hpp>
#include <gym.hpp>

struct RunConfig {
    char* rom_dir;
    char* rom_name;
    float repeat_action_probability;
    unsigned int frameskip_begin;
    unsigned int frameskip_end;
};


void print_help() {
    printf("Usage: program [-h] [-d ROM_DIR] [-r ROM_FILE] [-f FRAMESKIP_START[,FRAMESKIP_END] [-p PROB]\n");
    printf("Options:\n");
    printf("  -h               Show this help message\n");
    printf("  -d ROM_DIR       The directory where to look for the rom file.\n");
    printf("  -r ROM_FILE      The rom file name, should have a \"bin\" file extension.\n");
    printf("  -r FRAMESKIP     The frameskip range to use.  Two formats are possible.\n");
    printf("                     A single value can be provided for a deterministic frameskip, or\n");
    printf("                     two values separated by a comma (e.g., START,END) can be provided\n");
    printf("                     in which case the frameskip will be sampled from the range (inclusive).\n");
    printf("  -p PROB          The repeat action probability as a value in the range (0, 1)\n");
}

int process_env_vars(struct RunConfig *run_config) {
    run_config->rom_dir = getenv("ALE_ROMS_DIR");
    return 0;
}

int process_frameskip(const char *optarg, struct RunConfig *run_config) {
    const char* fsbreak;
    unsigned short fsstart_len;
    char fsstart[11];
    
    fsbreak = strchr(optarg, ',');
    if (fsbreak != NULL) {
        if ((fsbreak - optarg) <= 0) {
            fprintf(stderr, "Unexpected error processing frameskip begin\n");
            return 1;
        }
        fsstart_len = (unsigned short) (fsbreak - optarg);
        if (fsstart_len > 10) {
            fprintf(stderr, "Frameskip beginning value is larger than expected\n");
            return 1;
        }
        strncpy(fsstart, optarg, fsstart_len);
        fsstart[fsstart_len] = '\0';
        run_config->frameskip_begin = (unsigned int) strtoul(fsstart, NULL, 10);
        run_config->frameskip_end = (unsigned int) strtoul(fsbreak + 1, NULL, 10);
    } else {
        run_config->frameskip_begin = (unsigned int) strtoul(optarg, NULL, 10);
        run_config->frameskip_end = run_config->frameskip_begin;
    }
    return 0;
}

int process_arguments(int argc, char *argv[], struct RunConfig *run_config) {
    int opt;
    int i;
    
    while ((opt = getopt(argc, argv, "hd:r:f:p:")) != -1) {
        switch (opt) {
            case 'h':
                print_help();
                exit(0);
            case 'd':
                run_config->rom_dir = optarg;
                break;
            case 'r':
                run_config->rom_name = optarg;
                break;
            case 'f':
                if (process_frameskip(optarg, run_config)) {
                    fprintf(stderr, "process_arguments: error processing frameskip\n");
                    exit(2);
                }
                break;
            case 'p':
                run_config->repeat_action_probability = strtof(optarg, NULL);
                break;
            case '?': /* Unknown option or missing required argument */
                fprintf(stderr, "Unknown option or missing argument: -%c\n", optopt);
                return 1;
        }
    }

    if (optind < argc) {
        fprintf(stderr, "Non-option arguments:\n");
        for (i = optind; i < argc; i++) {
            printf("  %s\n", argv[i]);
        }
        return 1;
    }
    
    /* Additional validation */
    if (run_config->rom_dir == NULL) {
        fprintf(stderr, "Rom directory must be specified\n");
        return 1;
    }
    if (run_config->rom_name == NULL) {
        fprintf(stderr, "Rom name must be specified\n");
        return 1;
    }
    if (run_config->frameskip_begin == 0) {
        fprintf(stderr, "Frameskip must be positive\n");
        return 1;
    }
    if (run_config->frameskip_end < run_config->frameskip_begin) {
        fprintf(stderr, "Invalid frameskip, lower bound must be less than or equal to upper bound\n");
        return 1;
    }
    if (run_config->repeat_action_probability < 0.0 || run_config->repeat_action_probability > 1.0) {
        fprintf(stderr, "Repeat action probability must be between 0 and 1 inclusive\n");
        return 1;
    }
    return 0;
}

int main(int argc, char *argv[]) {
    struct RunConfig run_config = {
        .rom_dir = NULL, .rom_name = NULL,
        .repeat_action_probability = 0.25,
        .frameskip_begin = 4, .frameskip_end = 4,
    };
    
    if (process_arguments(argc, argv, &run_config) != 0) {
        return 1;
    }

    /* 
    size_t rom_file_buf_size = strlen(run_config.rom_dir) + strlen(run_config.rom_name) + 5 + 1;
    char rom_file_buf[rom_file_buf_size];
    */

    /*struct AtariEnvStepMetadata info; */
    struct RGBObservation obs;
    struct AtariRGBStepReturn state;
    enum Action action = (enum Action) 0;
    /* TODO: atari config should reflect all the run settings */
    struct AtariConfig config = default_atari_env_config_init();
    config.rom_dir = run_config.rom_dir;
    config.rom_name = run_config.rom_name;
    AtariEnv* env = atari_make(config);

    obs = atarirgb_reset(env, 123u);
    size_t step_count = 0;
    while (1) {
        action = atari_random_action(env);
        state = atarirgb_step(env, action);
        step_count++;
        obs = state.observation;
        if (state.terminated) break;
    }
    printf("Step count: %lu\n", step_count);

    obs = atarirgb_reset(env, 123u);
    step_count = 0;
    while (1) {
        action = atari_random_action(env);
        state = atarirgb_step(env, action);
        step_count++;
        // obs = state.observation;
        if (state.terminated) break;
    }
    printf("Step count: %lu\n", step_count);


    atari_destroy(env);

    return 0;
}
