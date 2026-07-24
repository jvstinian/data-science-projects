cmake_minimum_required(VERSION 3.15)
project(reinforcement_learning VERSION 1.0.0 LANGUAGES C)

# C89
set(CMAKE_C_STANDARD 90)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

add_compile_options(-Wall -Wextra -Wswitch -Wswitch-enum)

# Look for the math library
find_library(MATH_LIBRARY m)

# Library
add_library(reinforcementlearning
    src/linewalk.c
    src/frozenlake.c
    src/cartpole.c
    src/ttt.c
    src/ataxx.c
    src/hex.c
    src/array_ops.c
    src/epsilon_policies.c
)

# link math library
target_link_libraries(reinforcementlearning PRIVATE ${MATH_LIBRARY})

# Includes
target_include_directories(reinforcementlearning
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
target_link_libraries(rl_examples PRIVATE reinforcementlearning)

# Testing
enable_testing()

add_executable(test_core
    tests/test_example.c
)
target_link_libraries(test_core PRIVATE reinforcementlearning check)
add_test(NAME run_test_core COMMAND test_core)

# Install
include(GNUInstallDirs)
install(TARGETS reinforcementlearning rl_examples)

install(DIRECTORY include/
        DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

