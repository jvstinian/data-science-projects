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
#include "ale_c.hpp"
#include <cassert>

size_t get_welcome_message_length() {
    return ale::ALEInterface::welcomeMessage().length();
}

size_t get_welcome_message(char* msgp, size_t n) {
    if (n == 0) {
        return 0;
    }
    std::string message(ale::ALEInterface::welcomeMessage());
    strncpy(msgp, message.c_str(), n-1);
    msgp[n - 1] = '\0';
    return strlen(msgp);
}
    
ALEInterface* ale_interface_new() {
    ALEInterface* aleptr = new ale::ALEInterface();
    return aleptr;
}

void ale_interface_delete(ALEInterface* aleptr) {
    delete aleptr;
}

int ale_get_string(ALEInterface* aleptr, const char* key, char* val, size_t n) {
    if (n == 0) return 0;

    std::string keystr(key);
    std::string valstr(aleptr->getString(keystr));
    size_t vallen = valstr.length();
    size_t len = std::min<size_t>(vallen, n - 1);
    strncpy(val, valstr.c_str(), len);
    val[len] = '\0';
    return vallen;
}

int ale_get_int(ALEInterface* aleptr, const char* key) {
    std::string keystr(key);
    return aleptr->getInt(keystr);
}

bool ale_get_bool(ALEInterface* aleptr, const char* key) {
    std::string keystr(key);
    return aleptr->getBool(keystr);
}

float ale_get_float(ALEInterface* aleptr, const char* key) {
    std::string keystr(key);
    return aleptr->getFloat(keystr);
}

void ale_set_string(ALEInterface* aleptr, const char* key, const char* val) {
    std::string keystr(key);
    std::string valstr(val);
    aleptr->setString(keystr, valstr);
}

void ale_set_int(ALEInterface* aleptr, const char* key, int val) {
    std::string keystr(key);
    aleptr->setInt(keystr, val);
}

void ale_set_bool(ALEInterface* aleptr, const char* key, bool val) {
    std::string keystr(key);
    aleptr->setBool(keystr, val);
}

void ale_set_float(ALEInterface* aleptr, const char* key, float val) {
    std::string keystr(key);
    aleptr->setFloat(keystr, val);
}

void ale_load_rom(ALEInterface* aleptr, const char* rompath) {
    fs::path rom_file(rompath);
    aleptr->loadROM(rom_file);
}

int ale_act(ALEInterface* aleptr, Action act, float paddle_strength) {
    return aleptr->act(act, paddle_strength);
}

bool ale_game_over(ALEInterface* aleptr, bool with_trunc) {
    return aleptr->game_over(with_trunc);
}

bool ale_game_truncated(ALEInterface* aleptr) {
    return aleptr->game_truncated();
}

void ale_reset_game(ALEInterface* aleptr) {
    aleptr->reset_game();
}

int ale_lives(ALEInterface* aleptr) {
    return aleptr->lives();
}

size_t ale_get_available_modes_length(ALEInterface* aleptr) {
    return aleptr->getAvailableModes().size();
}

size_t ale_get_available_modes(ALEInterface* aleptr, game_mode_t* modes_out, size_t n) {
    std::vector<game_mode_t> game_modes(aleptr->getAvailableModes());
    size_t len = game_modes.size();
    size_t outlen = (len < n) ? len : n;
    std::copy(game_modes.begin(), game_modes.begin() + outlen, modes_out);
    return len;
}

void ale_set_mode(ALEInterface* aleptr, game_mode_t gamemode) {
    aleptr->setMode(gamemode);
}

game_mode_t ale_get_mode(ALEInterface* aleptr) {
    return aleptr->getMode();
}

size_t ale_get_available_difficulties_length(ALEInterface* aleptr) {
    return aleptr->getAvailableDifficulties().size();
}

size_t ale_get_available_difficulties(ALEInterface* aleptr, unsigned int* difficulties_out, size_t n) {
    std::vector<unsigned int> game_difficulties(aleptr->getAvailableDifficulties());
    size_t len = game_difficulties.size();
    size_t outlen = (len < n) ? len : n;
    std::copy(game_difficulties.begin(), game_difficulties.begin() + outlen, difficulties_out);
    return len;
}

void ale_set_difficulty(ALEInterface* aleptr, unsigned int difficulty) {
    aleptr->setDifficulty(difficulty);
}

unsigned int ale_get_difficulty(ALEInterface* aleptr) {
    return aleptr->getDifficulty();
}

size_t ale_get_legal_action_set_length(ALEInterface* aleptr) {
    return aleptr->getLegalActionSet().size();
}

size_t ale_get_legal_action_set(ALEInterface* aleptr, Action* actions_out, size_t n) {
    std::vector<Action> actions(aleptr->getLegalActionSet());
    size_t len = actions.size();
    size_t outlen = (len < n) ? len : n;
    std::copy(actions.begin(), actions.begin() + outlen, actions_out);
    return len;
}

size_t ale_get_minimal_action_set_length(ALEInterface* aleptr) {
    return aleptr->getMinimalActionSet().size();
}

size_t ale_get_minimal_action_set(ALEInterface* aleptr, Action* actions_out, size_t n) {
    std::vector<Action> actions(aleptr->getMinimalActionSet());
    size_t len = actions.size();
    len = (len < n) ? len : n;
    std::copy(actions.begin(), actions.begin() + len, actions_out);
    return len;
}

int ale_get_frame_number(ALEInterface* aleptr) {
    return aleptr->getFrameNumber();
}

int ale_get_episode_frame_number(ALEInterface* aleptr) {
    return aleptr->getEpisodeFrameNumber();
}

int ale_get_max_num_frames(ALEInterface* aleptr) {
    return aleptr->max_num_frames;
}

void ale_save_screen_png(ALEInterface* aleptr, const char* filename) {
    std::string file(filename);
    aleptr->saveScreenPNG(file);
}

void ale_get_screen_dims(ALEInterface* aleptr, size_t* heightp, size_t* widthp) {
    *heightp = aleptr->getScreen().height();
    *widthp = aleptr->getScreen().width();
}

void ale_get_screen_size(ALEInterface* aleptr, struct Screen* screen) {
    screen->height = aleptr->getScreen().height();
    screen->width = aleptr->getScreen().width();
}

void ale_get_screen_grayscale(ALEInterface* aleptr, struct Screen* screen) {
    size_t h = aleptr->getScreen().height();
    size_t w = aleptr->getScreen().width();
    assert(h == screen->height);
    assert(w == screen->width);
    assert(1 == screen->channels);

    size_t size = h * w;
    std::vector<pixel_t> grayscale_buffer(size, 0u);
    aleptr->getScreenGrayscale(grayscale_buffer);
    std::copy(grayscale_buffer.begin(), grayscale_buffer.end(), screen->screen);
}

void ale_get_screen_rgb(ALEInterface* aleptr, struct Screen* screen) {
    size_t h = aleptr->getScreen().height();
    size_t w = aleptr->getScreen().width();
    assert(h == screen->height);
    assert(w == screen->width);
    assert(3 == screen->channels);

    size_t size = 3 * h * w;
    std::vector<pixel_t> rgb_buffer(size, 0u);
    aleptr->getScreenRGB(rgb_buffer);
    std::copy(rgb_buffer.begin(), rgb_buffer.end(), screen->screen);
}
