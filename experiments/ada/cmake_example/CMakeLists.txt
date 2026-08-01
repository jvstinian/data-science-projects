cmake_minimum_required(VERSION 3.20)
project(ada_export_logic LANGUAGES C) # Or CXX if needed # ada_wrapper_pkg 
include(CMakeAda.cmake) # NOTE
# project(AdaCExport LANGUAGES Ada C)

# SET(BUILD_PATH_USE_ORIGIN true)  # NOTE: Not sure if this is the way to go
SET(CMAKE_INSTALL_RPATH "$ORIGIN")  # NOTE: Not sure if this is the way to go

# # 1. The Custom Target that runs gprbuild/ada build
# add_custom_command(
#     OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/lib/libexport_logic.so"
#     COMMAND echo -e "Running gprbuild..."
#     COMMAND gprbuild -p -P${CMAKE_SOURCE_DIR}/export_logic.gpr --relocate-build-tree=${CMAKE_CURRENT_BINARY_DIR}
#     WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
#     COMMENT "Building Ada library"
# )
# 
# # 2. Wrap that output in a Library Target
# # This tells CMake: "This library consists of this specific file"
# add_library(export_logic SHARED IMPORTED) # "${CMAKE_CURRENT_BINARY_DIR}/libexport_logic.so")
# 
# # 3. Tell CMake that the library is already "built" 
# # (prevents CMake from trying to compile it with a C compiler)
# # set_source_files_properties("${CMAKE_CURRENT_BINARY_DIR}/libexport_logic.so" PROPERTIES EXTERNAL_OBJECT TRUE GENERATED TRUE)
# set_target_properties(export_logic PROPERTIES IMPORTED_LOCATION "${CMAKE_CURRENT_BINARY_DIR}/lib/libexport_logic.so")
# # Add a custom target that depends on the generated file (optional but useful for dependency chains)
# add_custom_target(export_logic_target DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/lib/libexport_logic.so")
# # Make the imported library depend on this target
# add_dependencies(export_logic export_logic_target) 

# Define the Ada library that exports the function
# add_library(ada_logic STATIC src/export_logic.ads src/export_logic.adb)
add_ada_shared_library(export_logic export_logic.gpr lib) # lib  # NOTE: Trying the logic above instead

find_package(Python COMPONENTS Interpreter Development.Module REQUIRED)
find_package(Cython REQUIRED)
include(UseCython) # Provided by cython-cmake

# Transpile .pyx to .c
cython_transpile(cython_example/ada_wrapper.pyx LANGUAGE C OUTPUT cython_example/ada_wrapper.c OUTPUT_VARIABLE wrapper_c_file)
message(STATUS "File written by cython_transpile is: ${wrapper_c_file}")

# Avoid patchelf of DT_NEEDED later
set_property(TARGET export_logic PROPERTY IMPORTED_NO_SONAME 1) # TODO

# add_library("${wrapper_c_file}" MODULE ${ada_wrapper})
python_add_library(ada_wrapper MODULE WITH_SOABI "${wrapper_c_file}") # "${export_logic}") # ada_wrapper.c
target_link_libraries(ada_wrapper PRIVATE export_logic)

get_target_property(export_logic_location export_logic IMPORTED_LOCATION)
message(STATUS "export_logic location: ${export_logic_location}")
message(STATUS "ada_wrapper value: ${ada_wrapper}")

install(TARGETS ada_wrapper DESTINATION .)  # TODO: Enable
# install(FILES "${CMAKE_CURRENT_BINARY_DIR}/lib/libexport_logic.so" DESTINATION ./lib)
install(FILES "${export_logic_location}" DESTINATION .)

# # find_program(PATCHELF_EXECUTABLE patchelf)
# if(PATCHELF_EXECUTABLE)
#     install(CODE "execute_process(COMMAND ${PATCHELF_EXECUTABLE} --replace-needed lib/libexport_logic.so libexport_logic.so ${CMAKE_INSTALL_PREFIX}/ada_wrapper_pkg/ada_wrapper.so)")
# else()
#     message(WARNING "patchelf not found, cannot modify DT_NEEDED entry during install.")
# endif()


# # Ensure the module name matches what you'll import in Python
# set_target_properties(ada_wrapper PROPERTIES OUTPUT_NAME "ada_wrapper")

# # Define the C executable that consumes the Ada function
# # add_executable(main app/main.c)
# # # target_link_directories(main PRIVATE "/nix/store/sj5r1rscb8s3xn7yv9cfcyb8gzxr3qi5-gnat-13.3.0/lib/gcc/x86_64-unknown-linux-gnu/13.3.0/adalib")
# # # target_link_libraries(main PRIVATE export_logic gnat)
# # # target_link_directories(main PRIVATE "lib/")
# # target_link_libraries(main PRIVATE export_logic)
# 
# # pybind11
# # find_package(pybind11 REQUIRED)
# # pybind11_add_module(example app/main.c)
# 
# # find_package(PythonExtensions REQUIRED)
# 
# # add_library(_ada_message MODULE app/main.c)
# # python_extension_module(hello)
# # install(TARGETS export_logic LIBRARY DESTINATION hello)
# # install(TARGETS export_logic LIBRARY DESTINATION hello)
# 
# # 0. pkg-config
# find_package(PkgConfig REQUIRED)
# # Search for the 'python3' package and create an imported target
# pkg_search_module(PKG_PYTHON REQUIRED IMPORTED_TARGET python3)
# 
# # 1. Find Python and Cython
# find_package(Python REQUIRED COMPONENTS Interpreter Development.Module)
# # /nix/store/5083vfjnfvljv055yj990rwg8c9q3hwi-python3.12-scikit-build-0.18.1/lib/python3.12/site-packages/skbuild/resources/cmake
# # PYTHON_SITE_PACKAGES_DIR 
# # CMAKE_MODULE_PATH="/nix/store/5083vfjnfvljv055yj990rwg8c9q3hwi-python3.12-scikit-build-0.18.1/lib/python3.12/site-packages/skbuild/resources/cmake"
# # message(STATUS "PYTHON_SITE_PACKAGES_DIR has the value: ${PYTHON_SITE_PACKAGES_DIR}") 
# list(APPEND CMAKE_MODULE_PATH "${Python_SITELIB}/skbuild/resources/cmake")
# message(STATUS "Python site-packages directory: ${Python_SITELIB}")
# message(STATUS "CMAKE_MODULE_PATH (modified): ${CMAKE_MODULE_PATH}")
# find_package(Cython REQUIRED)
# # include(UsePythonExtensions)
# include(UseCython)
# 
# # # 2. Your existing Ada build logic (Shared Library)
# # add_library(export_logic SHARED ${ADA_SOURCES})
# # set_target_properties(export_logic PROPERTIES POSITION_INDEPENDENT_CODE ON)
# 
# # 3. Cython Extension
# # This compiles wrapper.pyx into a C file and then a Python module
# add_cython_target(ada_wrapper cython_example/ada_wrapper.pyx)  # NOTE: I changed this
# add_library(ada_wrapper MODULE ${ada_wrapper})
# # target_link_libraries(ada_wrapper PRIVATE export_logic)
# target_link_libraries(ada_wrapper PRIVATE PkgConfig::PKG_PYTHON export_logic)
# set_target_properties(ada_wrapper PROPERTIES PREFIX "")  # NOTE: I added this to remove the lib
# # cython_add_module(ada_wrapper SOURCES cython_example/wrapper.pyx)  # NOTE: I changed this
# 
# # 4. Installation (Crucial for scikit-build-core)
# # This places the .so files in the root of the installed package
# get_target_property(export_logic_location export_logic IMPORTED_LOCATION)  # NOTE: Added this
# install(FILES "${export_logic_location}"
#         DESTINATION ada_wrapper_pkg/lib)
# install(TARGETS ada_wrapper
#         DESTINATION ada_wrapper_pkg)
# 
# # find_program(PATCHELF_EXECUTABLE patchelf)
# # if(PATCHELF_EXECUTABLE)
# #     install(CODE "execute_process(COMMAND ${PATCHELF_EXECUTABLE} --replace-needed lib/libexport_logic.so libexport_logic.so ${CMAKE_INSTALL_PREFIX}/ada_wrapper_pkg/ada_wrapper.so)")
# # else()
# #     message(WARNING "patchelf not found, cannot modify DT_NEEDED entry during install.")
# # endif()
# 
