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
elseif(
  CMAKE_SYSTEM_NAME
  STREQUAL
  "Windows")
  set(F2C_PATH "${BIN_DIRECTORY}")
else()
  message(FATAL_ERROR "Unsupported OS: ${CMAKE_SYSTEM_NAME}")
endif()

set(NELSON_F2C_EXECUTABLE "${F2C_PATH}/nelson_f2c${CMAKE_EXECUTABLE_SUFFIX}")
set(CONVERTED_SLICOT_C_FILES "")

add_custom_target(convert_f2c DEPENDS nelson_f2c)

  # Generate a batch script that mirrors the invocation used by the
  # Visual Studio project. Keep scripts for convenient manual runs,
  # but generate per-file CMake custom commands so the generated
  # .vcxproj matches the original project's per-file custom build
  # rules (working dir + invocation using MSBuild macros).
  set(CONVERT_SCRIPT "${CMAKE_CURRENT_BINARY_DIR}/convert_slicot_f2c.bat")
  file(WRITE ${CONVERT_SCRIPT} "@echo off\r\n")
  file(APPEND ${CONVERT_SCRIPT} "if not exist \"${CONVERTED_SLICOT_C_DIR}\" mkdir \"${CONVERTED_SLICOT_C_DIR}\"\r\n")
  file(APPEND ${CONVERT_SCRIPT} "cd /d \"${CONVERTED_SLICOT_C_DIR}\"\r\n")

  set(CONVERT_SCRIPT_DEBUG "${CMAKE_CURRENT_BINARY_DIR}/convert_slicot_f2c_debug.bat")
  file(WRITE ${CONVERT_SCRIPT_DEBUG} "@echo off\r\n")
  file(APPEND ${CONVERT_SCRIPT_DEBUG} "if not exist \"${CONVERTED_SLICOT_C_DIR}\" mkdir \"${CONVERTED_SLICOT_C_DIR}\"\r\n")
  file(APPEND ${CONVERT_SCRIPT_DEBUG} "cd /d \"${CONVERTED_SLICOT_C_DIR}\"\r\n")

  foreach(F_FILE IN LISTS SLICOT_FORTRAN_SRC)
    get_filename_component(
      FILENAME_WE
      ${F_FILE}
      NAME_WE)
    get_filename_component(
      F_EXT
      ${F_FILE}
      EXT)
    set(C_FILE "${CONVERTED_SLICOT_C_DIR}/${FILENAME_WE}.c")
    set(REL_FORTRAN "../fortran/${FILENAME_WE}${F_EXT}")

    # Always register a per-file conversion rule for generated C sources.
    # This prevents missing-source failures after clean builds where files
    # may have been deleted from src/f2c.
    # Use a per-file temporary directory and capture stdout/stderr to files
    # to avoid silent crashes and to make failures diagnosable on Windows.
    file(APPEND ${CONVERT_SCRIPT} "\"${NELSON_F2C_EXECUTABLE}\" -T \"${CMAKE_BINARY_DIR}/tmp/f2c/${FILENAME_WE}\" \"${REL_FORTRAN}\" -E -I\"${CMAKE_SOURCE_DIR}/modules/f2c/src/include\" 1> \"${CMAKE_CURRENT_BINARY_DIR}/convert_${FILENAME_WE}_out.txt\" 2> \"${CMAKE_CURRENT_BINARY_DIR}/convert_${FILENAME_WE}_err.txt\"\r\n")
    file(APPEND ${CONVERT_SCRIPT_DEBUG} "echo Running: \"${NELSON_F2C_EXECUTABLE}\" -T \"${CMAKE_BINARY_DIR}/tmp/f2c/${FILENAME_WE}\" \"${REL_FORTRAN}\" -E -I\"${CMAKE_SOURCE_DIR}/modules/f2c/src/include\"\r\n")
    file(APPEND ${CONVERT_SCRIPT_DEBUG} "\"${NELSON_F2C_EXECUTABLE}\" -T \"${CMAKE_BINARY_DIR}/tmp/f2c/${FILENAME_WE}\" \"${REL_FORTRAN}\" -E -I\"${CMAKE_SOURCE_DIR}/modules/f2c/src/include\"\r\n")

    # For Visual Studio / MSBuild on Windows, use a .bat wrapper.
    # For Unix-like generators, use a .sh wrapper executed with /bin/sh.
    set(WRAPPER_TMPDIR "${CMAKE_BINARY_DIR}/tmp/f2c/${FILENAME_WE}")

    if(WIN32)
      set(CONVERT_WRAPPER "${CMAKE_CURRENT_BINARY_DIR}/convert_${FILENAME_WE}.bat")
      file(WRITE ${CONVERT_WRAPPER} "@echo off\r\n")
      file(APPEND ${CONVERT_WRAPPER} "if not exist \"${CONVERTED_SLICOT_C_DIR}\" mkdir \"${CONVERTED_SLICOT_C_DIR}\"\r\n")
      file(APPEND ${CONVERT_WRAPPER} "if not exist \"${WRAPPER_TMPDIR}\" mkdir \"${WRAPPER_TMPDIR}\"\r\n")
      file(APPEND ${CONVERT_WRAPPER} "cd /d \"${CONVERTED_SLICOT_C_DIR}\"\r\n")
      file(APPEND ${CONVERT_WRAPPER} "\"${NELSON_F2C_EXECUTABLE}\" -T \"${WRAPPER_TMPDIR}\" \"${REL_FORTRAN}\" -E -I\"${CMAKE_SOURCE_DIR}/modules/f2c/src/include\" 1> \"%~dp0convert_${FILENAME_WE}_out.txt\" 2> \"%~dp0convert_${FILENAME_WE}_err.txt\"\r\n")
      file(APPEND ${CONVERT_WRAPPER} "if %ERRORLEVEL% neq 0 exit /b %ERRORLEVEL%\r\n")
    else()
      set(CONVERT_WRAPPER "${CMAKE_CURRENT_BINARY_DIR}/convert_${FILENAME_WE}.sh")
      file(WRITE ${CONVERT_WRAPPER} "#!/usr/bin/env sh\n")
      file(APPEND ${CONVERT_WRAPPER} "set -e\n")
      file(APPEND ${CONVERT_WRAPPER} "mkdir -p \"${CONVERTED_SLICOT_C_DIR}\"\n")
      file(APPEND ${CONVERT_WRAPPER} "mkdir -p \"${WRAPPER_TMPDIR}\"\n")
      file(APPEND ${CONVERT_WRAPPER} "cd \"${CONVERTED_SLICOT_C_DIR}\"\n")
      file(APPEND ${CONVERT_WRAPPER} "\"${NELSON_F2C_EXECUTABLE}\" -T \"${WRAPPER_TMPDIR}\" \"${REL_FORTRAN}\" -E -I\"${CMAKE_SOURCE_DIR}/modules/f2c/src/include\" 1> \"${CMAKE_CURRENT_BINARY_DIR}/convert_${FILENAME_WE}_out.txt\" 2> \"${CMAKE_CURRENT_BINARY_DIR}/convert_${FILENAME_WE}_err.txt\"\n")
    endif()

    if(WIN32)
      add_custom_command(
        OUTPUT ${C_FILE}
        COMMAND ${CMAKE_COMMAND} -E make_directory ${CONVERTED_SLICOT_C_DIR}
        COMMAND ${CONVERT_WRAPPER}
        DEPENDS ${F_FILE} nelson_f2c
        COMMENT "Converting ${F_FILE} to ${C_FILE}"
        VERBATIM)
    else()
      add_custom_command(
        OUTPUT ${C_FILE}
        COMMAND ${CMAKE_COMMAND} -E make_directory ${CONVERTED_SLICOT_C_DIR}
        COMMAND /bin/sh ${CONVERT_WRAPPER}
        DEPENDS ${F_FILE} nelson_f2c
        COMMENT "Converting ${F_FILE} to ${C_FILE}"
        VERBATIM)
    endif()

    list(APPEND CONVERTED_SLICOT_C_FILES ${C_FILE})
  endforeach()
# ==============================================================================
add_library(${module_library_name} ${SLICOT_SRC} ${CONVERTED_SLICOT_C_FILES})
# The f2c-converted C sources have no visibility annotations, so we must
# make their symbols visible for other libraries to link against them.
# On Windows/MSVC, C_VISIBILITY_PRESET has no effect; WINDOWS_EXPORT_ALL_SYMBOLS
# is required to auto-generate the import library (.lib) alongside the DLL.
set_target_properties(${module_library_name} PROPERTIES
    C_VISIBILITY_PRESET default
    WINDOWS_EXPORT_ALL_SYMBOLS ON)
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
          nlsInterpreter
          nlsTypes)
# ==============================================================================
nelson_install_library(${module_library_name})
nelson_install_library(${module_library_builtin_name})
nelson_install_module_data(${module_name})
# ==============================================================================
