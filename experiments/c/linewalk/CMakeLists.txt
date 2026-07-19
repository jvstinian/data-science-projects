cmake_minimum_required(VERSION 3.15)
project(reinforcement_learning VERSION 1.0.0 LANGUAGES C)

# C89
set(CMAKE_C_STANDARD 90)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

add_compile_options(-Wall -Wextra)

# Library
add_library(rllib
    src/linewalk.c
)

# Includes
target_include_directories(rllib
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
    PRIVATE
        src
)

# Executable
add_executable(rl_examples
    apps/main.c
)

# Link executable
target_link_libraries(rl_examples PRIVATE rllib)

# Testing
enable_testing()

add_executable(test_core
    tests/test_example.c
)
target_link_libraries(test_core PRIVATE rllib check)
add_test(NAME run_test_core COMMAND test_core)

# Install
include(GNUInstallDirs)
install(TARGETS rllib rl_examples)

install(DIRECTORY include/
        DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

