cmake_minimum_required(VERSION 3.15)
project(opencv-example VERSION 0.1.0 LANGUAGES CXX)

add_compile_options(-Wall -Wextra)

find_package(PkgConfig REQUIRED)
pkg_check_modules(OPENCV REQUIRED IMPORTED_TARGET opencv4)

# Look for the math library
# find_library(MATH_LIBRARY m)
# find_library(OPENCV_LIBRARY opencv4)

# Executable
add_executable(opencv-example
    apps/main.cpp
)

# Link executable
target_link_libraries(opencv-example PRIVATE PkgConfig::OPENCV)

# # Install
# include(GNUInstallDirs)
# install(TARGETS opencv-example)
# 
# # To auto-generate a "package" target
# include(CPack)
