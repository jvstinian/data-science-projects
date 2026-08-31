cmake_minimum_required(VERSION 3.15)
project(ale-c VERSION 0.1.0 LANGUAGES C CXX)

add_compile_options(-Wall -Wextra)

find_package(PkgConfig REQUIRED)
pkg_check_modules(ALE REQUIRED IMPORTED_TARGET ale)
pkg_check_modules(SDL2 REQUIRED IMPORTED_TARGET sdl2)
pkg_check_modules(ZLIB REQUIRED IMPORTED_TARGET zlib)
find_package(OpenSSL REQUIRED)

# Library
add_library(ale-c
    src/ale_c.cpp
    src/gym.cpp
    src/rom_md5.cpp
)

# link ALE library
target_link_libraries(ale-c PRIVATE PkgConfig::ZLIB PkgConfig::SDL2 PkgConfig::ALE OpenSSL::SSL)

target_include_directories(ale-c
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>/include
        $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
    PRIVATE
        src
)

# Executable
add_executable(ale-example
    apps/main.c
)

# Link executable
target_link_libraries(ale-example PRIVATE ale-c PkgConfig::ALE)

# Testing
enable_testing()

add_executable(test-ale-c
    tests/test_ale.c
)
set_target_properties(test-ale-c PROPERTIES
    C_STANDARD 99
    C_STANDARD_REQUIRED ON
    C_EXTENSIONS OFF
)
target_link_libraries(test-ale-c PRIVATE ale-c check)
add_test(NAME run_ale_tests COMMAND test-ale-c)

# We use the tetris.bin file found in the ALE repo
# for testing
configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/resources/tetris.bin"
    "${CMAKE_CURRENT_BINARY_DIR}/resources/tetris.bin"
    COPYONLY
)

# # Define the source and destination paths
# set(RESOURCES_SRC_DIR "${CMAKE_CURRENT_SOURCE_DIR}/resources")
# set(RESOURCES_BIN_DIR "${CMAKE_CURRENT_BINARY_DIR}/resources")
# 
# # Create a custom command that performs the copy operation
# add_custom_target(copy_test_data ALL
#     COMMAND ${CMAKE_COMMAND} -E copy_directory "${RESOURCES_SRC_DIR}" "${RESOURCES_BIN_DIR}"
#     COMMENT "Copying test data to build directory..."
# )

# Install
include(GNUInstallDirs)
install(TARGETS ale-c ale-example)

# To auto-generate a "package" target
include(CPack)
