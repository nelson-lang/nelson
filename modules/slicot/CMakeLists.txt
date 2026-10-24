# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# ==============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
set(module_name slicot)
set(module_library_name nlsSlicot)
set(module_library_builtin_name ${module_library_name}_builtin)
# ==============================================================================
nelson_module_sources_from_glob(
  SLICOT_SRC
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp
)
file(GLOB SLICOT_FORTRAN_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/fortran/*.f")
# ==============================================================================
set(
  NELSON_SLICOT_F2C_SOURCE_DIR
  ""
  CACHE
    PATH
    "Directory containing host-generated SLICOT C sources for cross compilation"
)
if(NELSON_WASM_BUILD
  AND NOT NELSON_SLICOT_F2C_SOURCE_DIR
  AND IS_DIRECTORY "${CMAKE_SOURCE_DIR}/temp/wasm-deps/slicot-f2c"
)
  set(
    NELSON_SLICOT_F2C_SOURCE_DIR
    "${CMAKE_SOURCE_DIR}/temp/wasm-deps/slicot-f2c"
    CACHE
      PATH
      "Directory containing host-generated SLICOT C sources for cross compilation"
    FORCE
  )
endif()

set(CONVERTED_SLICOT_C_FILES "")
if(NELSON_SLICOT_F2C_SOURCE_DIR)
  foreach(F_FILE IN LISTS SLICOT_FORTRAN_SRC)
    get_filename_component(FILENAME_WE "${F_FILE}" NAME_WE)
    set(C_FILE "${NELSON_SLICOT_F2C_SOURCE_DIR}/${FILENAME_WE}.c")
    if(NOT EXISTS "${C_FILE}")
      message(FATAL_ERROR
        "Missing host-generated SLICOT source: ${C_FILE}. "
        "Run tools/wasm/build-slicot-f2c.sh before cross compilation."
      )
    endif()
    list(APPEND CONVERTED_SLICOT_C_FILES "${C_FILE}")
  endforeach()
  message(STATUS
    "Using host-generated SLICOT C sources: ${NELSON_SLICOT_F2C_SOURCE_DIR}"
  )
elseif(CMAKE_CROSSCOMPILING)
  message(FATAL_ERROR
    "SLICOT cross compilation requires NELSON_SLICOT_F2C_SOURCE_DIR. "
    "Run tools/wasm/build-slicot-f2c.sh before configuring."
  )
else()
  set(CONVERTED_SLICOT_C_DIR "${CMAKE_CURRENT_BINARY_DIR}/f2c")
  file(MAKE_DIRECTORY "${CONVERTED_SLICOT_C_DIR}")
  foreach(F_FILE IN LISTS SLICOT_FORTRAN_SRC)
    get_filename_component(FILENAME_WE "${F_FILE}" NAME_WE)
    set(C_FILE "${CONVERTED_SLICOT_C_DIR}/${FILENAME_WE}.c")
    add_custom_command(
      OUTPUT "${C_FILE}"
      COMMAND "$<TARGET_FILE:nelson_f2c>" "${F_FILE}" "-E"
        "-I${CMAKE_CURRENT_SOURCE_DIR}/../f2c/src/include"
        "-d${CONVERTED_SLICOT_C_DIR}"
      DEPENDS
        nelson_f2c
        "${F_FILE}"
      COMMENT "Converting ${F_FILE} to C using f2c"
      VERBATIM
    )
    list(APPEND CONVERTED_SLICOT_C_FILES "${C_FILE}")
  endforeach()
endif()
# ==============================================================================
add_library(${module_library_name} ${SLICOT_SRC} ${CONVERTED_SLICOT_C_FILES})
# The f2c-converted C sources have no visibility annotations, so we must
# make their symbols visible for other libraries to link against them.
# On Windows/MSVC, C_VISIBILITY_PRESET has no effect; WINDOWS_EXPORT_ALL_SYMBOLS
# is required to auto-generate the import library (.lib) alongside the DLL.
set_target_properties(${module_library_name}
  PROPERTIES
    C_VISIBILITY_PRESET default
    WINDOWS_EXPORT_ALL_SYMBOLS ON
)
# ==============================================================================
target_include_directories(${module_library_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../f2c/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include
)
# ==============================================================================
target_link_libraries(${module_library_name}
  PRIVATE
    nlsCommons
    nlsF2C
    nlsCharacters_encoding
    ${NELSON_BLAS_LAPACKX_LIBRARIES}
)
# ==============================================================================
# Builtin library
# ==============================================================================
nelson_module_sources_from_glob(
  SLICOT_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp
)
add_library(${module_library_builtin_name} ${SLICOT_BUILTIN})
# ==============================================================================
target_include_directories(${module_library_builtin_name}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
    ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
)
# ==============================================================================
target_link_libraries(${module_library_builtin_name}
  PRIVATE
    ${module_library_name}
    nlsError_manager
    nlsI18n
    nlsInterpreter
    nlsTypes
)
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(${module_name})
