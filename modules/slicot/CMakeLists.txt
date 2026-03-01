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
  ${CMAKE_CURRENT_SOURCE_DIR}/src/cpp)
file(
  GLOB
  SLICOT_FORTRAN_SRC
  "${CMAKE_CURRENT_SOURCE_DIR}/src/fortran/*.f")
# ==============================================================================
# f2c conversion setup
# ==============================================================================
set(CONVERTED_SLICOT_C_DIR "${CMAKE_CURRENT_SOURCE_DIR}/src/f2c")
file(MAKE_DIRECTORY ${CONVERTED_SLICOT_C_DIR})

if(CMAKE_SYSTEM_NAME
   STREQUAL
   "Linux")
  set(F2C_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../bin/linux")
elseif(
  CMAKE_SYSTEM_NAME
  STREQUAL
  "Darwin")
  set(F2C_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../bin/macOS")
else()
  message(FATAL_ERROR "Unsupported OS: ${CMAKE_SYSTEM_NAME}")
endif()

set(NELSON_F2C_EXECUTABLE "${F2C_PATH}/nelson_f2c")
set(CONVERTED_SLICOT_C_FILES "")

add_custom_target(convert_f2c DEPENDS nelson_f2c)

foreach(F_FILE IN LISTS SLICOT_FORTRAN_SRC)
  get_filename_component(
    FILENAME_WE
    ${F_FILE}
    NAME_WE)
  set(C_FILE "${CONVERTED_SLICOT_C_DIR}/${FILENAME_WE}.c")
  add_custom_command(
    OUTPUT ${C_FILE}
    COMMAND
      "${NELSON_F2C_EXECUTABLE}" "${F_FILE}" "-E"
      "-I${CMAKE_CURRENT_SOURCE_DIR}/../f2c/src/include"
      "-d${CONVERTED_SLICOT_C_DIR}"
    DEPENDS ${F_FILE}
    COMMENT "Converting ${F_FILE} to C using f2c"
    VERBATIM)
  list(
    APPEND
    CONVERTED_SLICOT_C_FILES
    ${C_FILE})
endforeach()
# ==============================================================================
add_library(${module_library_name} ${SLICOT_SRC} ${CONVERTED_SLICOT_C_FILES})
# The f2c-converted C sources have no visibility annotations, so we must
# make their symbols visible for other libraries to link against them.
set_target_properties(${module_library_name} PROPERTIES C_VISIBILITY_PRESET
                                                        default)
# ==============================================================================
# Now that the library target exists, we can add the dependency
add_dependencies(${module_library_name} nelson_f2c)
# ==============================================================================
target_include_directories(
  ${module_library_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../error_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../f2c/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../commons/src/include)

target_link_libraries(
  ${module_library_name}
  PRIVATE nlsCommons
          nlsF2C
          nlsCharacters_encoding
          ${NELSON_BLAS_LAPACKX_LIBRARIES})
# ==============================================================================
# Builtin library
# ==============================================================================
nelson_module_sources_from_glob(
  SLICOT_BUILTIN
  DIRS
  ${CMAKE_CURRENT_SOURCE_DIR}/builtin/cpp)

add_library(${module_library_builtin_name} ${SLICOT_BUILTIN})

target_include_directories(
  ${module_library_builtin_name}
  PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/builtin/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}/${module_name}>
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../interpreter/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../types/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../stream_manager/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../characters_encoding/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../overload/src/include
          ${CMAKE_CURRENT_SOURCE_DIR}/../i18n/src/include)

target_link_libraries(
  ${module_library_builtin_name}
  PRIVATE ${module_library_name}
          nlsError_manager
          nlsI18n
          nlsCharacters_encoding
          nlsInterpreter
          nlsTypes)
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(${module_name})
# ==============================================================================
