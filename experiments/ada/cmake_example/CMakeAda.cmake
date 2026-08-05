cmake_minimum_required(VERSION 3.20)

# Builds the project and creates a lib variable to be used in linking
# This is for building a static library
function(add_ada_library TARGET GPRFILE OUTPUT_REL_DIR)
    # Actual library file
    set(_result ${CMAKE_CURRENT_BINARY_DIR}/${OUTPUT_REL_DIR}/${CMAKE_STATIC_LIBRARY_PREFIX}${TARGET}${CMAKE_STATIC_LIBRARY_SUFFIX})

    # Build and leave Ada dependencies to gprbuild
    add_custom_target(${TARGET}_build
        COMMAND gprbuild -p -P${CMAKE_SOURCE_DIR}/${GPRFILE} --relocate-build-tree=${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Building library ${TARGET}..."
    )

    # Fake library target that depends on gprbuild result
    add_library(${TARGET} STATIC IMPORTED GLOBAL)
    add_dependencies(${TARGET} ${TARGET}_build)

    # Connect the library object with the library target
    set_target_properties(${TARGET}
            PROPERTIES
            IMPORTED_LOCATION ${_result})
endfunction(add_ada_library)

function(add_ada_shared_library TARGET GPRFILE OUTPUT_REL_DIR)
    # Actual library file
    set(_result ${CMAKE_CURRENT_BINARY_DIR}/${OUTPUT_REL_DIR}/${CMAKE_SHARED_LIBRARY_PREFIX}${TARGET}${CMAKE_SHARED_LIBRARY_SUFFIX})

    # Build and leave Ada dependencies to gprbuild
    # BYPRODUCTS is included below to indicate to Ninja to use this target to build the shared library
    add_custom_target(${TARGET}_build
        COMMAND gprbuild -p -P${CMAKE_SOURCE_DIR}/${GPRFILE} --relocate-build-tree=${CMAKE_CURRENT_BINARY_DIR}
        BYPRODUCTS ${_result}
        COMMENT "Building library ${TARGET}..."
    )

    # Fake library target that depends on gprbuild result
    add_library(${TARGET} SHARED IMPORTED GLOBAL)
    add_dependencies(${TARGET} ${TARGET}_build)

    # Connect the library object with the library target
    set_target_properties(${TARGET}
            PROPERTIES
            IMPORTED_LOCATION ${_result})
endfunction(add_ada_shared_library)

function(add_ada_executable TARGET GPRFILE)
    # Actual binary file
    set(_result ${CMAKE_CURRENT_BINARY_DIR}/${OUTPUT_REL_DIR}/${TARGET}${CMAKE_EXECUTABLE_SUFFIX})

    # Build and leave Ada dependencies to gprbuild
    add_custom_target(${TARGET}
        ALL
        COMMAND gprbuild -p -P${CMAKE_SOURCE_DIR}/${GPRFILE} --relocate-build-tree=${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Building binary ${TARGET}...")
endfunction(add_ada_executable)
