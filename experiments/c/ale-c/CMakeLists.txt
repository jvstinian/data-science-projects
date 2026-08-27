cmake_minimum_required(VERSION 3.15)
project(ale-c VERSION 0.1.0 LANGUAGES CXX)

add_compile_options(-Wall -Wextra)

find_package(PkgConfig REQUIRED)
pkg_check_modules(ALE REQUIRED IMPORTED_TARGET ale)
pkg_check_modules(SDL2 REQUIRED IMPORTED_TARGET sdl2)
pkg_check_modules(ZLIB REQUIRED IMPORTED_TARGET zlib)
find_package(OpenSSL REQUIRED)

# Look for the math library
# find_library(MATH_LIBRARY m)
# find_library(OPENCV_LIBRARY opencv4)

# Library
add_library(ale-c
    src/ale_c.cpp
    src/gym.cpp
    src/rom_md5.cpp
)

# link ALE library
target_link_libraries(ale-c PRIVATE PkgConfig::ZLIB PkgConfig::SDL2 PkgConfig::ALE OpenSSL::SSL)

# Includes TODO: Maybe change from src to include
target_include_directories(ale-c
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>/src
        $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
    PRIVATE
        src
)

# Executable
add_executable(ale-example
    apps/main.cpp
)

# Link executable
target_link_libraries(ale-example PRIVATE ale-c PkgConfig::ALE)

# # Install
# include(GNUInstallDirs)
# install(TARGETS opencv-example)
# 
# # To auto-generate a "package" target
# include(CPack)
