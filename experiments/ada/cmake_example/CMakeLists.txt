cmake_minimum_required(VERSION 3.20)
project(ada_export_logic LANGUAGES C) # Or CXX if needed
include(CMakeAda.cmake)

SET(CMAKE_INSTALL_RPATH "$ORIGIN")

add_ada_shared_library(export_logic export_logic.gpr lib)

find_package(Python COMPONENTS Interpreter Development.Module REQUIRED)
find_package(Cython REQUIRED)
include(UseCython) # Provided by cython-cmake

# Transpile .pyx to .c
cython_transpile(cython_example/ada_wrapper.pyx LANGUAGE C OUTPUT cython_example/ada_wrapper.c OUTPUT_VARIABLE wrapper_c_file)
message(STATUS "File written by cython_transpile is: ${wrapper_c_file}")

# Avoid patchelf of DT_NEEDED later
set_property(TARGET export_logic PROPERTY IMPORTED_NO_SONAME 1)

python_add_library(ada_wrapper MODULE WITH_SOABI "${wrapper_c_file}")
target_link_libraries(ada_wrapper PRIVATE export_logic)

get_target_property(export_logic_location export_logic IMPORTED_LOCATION)
message(STATUS "export_logic location: ${export_logic_location}")
message(STATUS "ada_wrapper value: ${ada_wrapper}")

install(TARGETS ada_wrapper DESTINATION .)
install(FILES "${export_logic_location}" DESTINATION .)

# Original approach used for patching elf file
# # find_program(PATCHELF_EXECUTABLE patchelf)
# if(PATCHELF_EXECUTABLE)
#     install(CODE "execute_process(COMMAND ${PATCHELF_EXECUTABLE} --replace-needed lib/libexport_logic.so libexport_logic.so ${CMAKE_INSTALL_PREFIX}/ada_wrapper_pkg/ada_wrapper.so)")
# else()
#     message(WARNING "patchelf not found, cannot modify DT_NEEDED entry during install.")
# endif()
