# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
# GIF, JPEG, PNG detection is a hell on MacOs or CONDA environments :'(
# ==============================================================================
# Helper: split checks into include / library macros and a small wrapper
macro(_nelson_check_gif_include PREFIX)
  if(PREFIX AND EXISTS "${PREFIX}/include/gif_lib.h")
    set(
      GIF_INCLUDE_DIR
      "${PREFIX}/include"
      CACHE
        STRING
        "Path to giflib include directory"
      FORCE
    )
  endif()
endmacro()
# ==============================================================================
macro(_nelson_check_gif_library PREFIX)
  if(PREFIX AND EXISTS "${PREFIX}/lib/libgif${CMAKE_SHARED_LIBRARY_SUFFIX}")
    set(
      GIF_LIBRARIES
      "${PREFIX}/lib/libgif${CMAKE_SHARED_LIBRARY_SUFFIX}"
      CACHE
        STRING
        "Path to giflib library"
      FORCE
    )
  endif()
endmacro()
# ==============================================================================
macro(_nelson_check_gif_prefix PREFIX)
  if(PREFIX)
    _nelson_check_gif_include("${PREFIX}")
    _nelson_check_gif_library("${PREFIX}")
  endif()
endmacro()
# ==============================================================================
macro(_nelson_check_gif_include_from_library LIBRARY)
  if(LIBRARY AND EXISTS "${LIBRARY}")
    get_filename_component(_nelson_gif_library_dir "${LIBRARY}" DIRECTORY)
    get_filename_component(_nelson_gif_prefix
      "${_nelson_gif_library_dir}"
      DIRECTORY
    )
    _nelson_check_gif_include("${_nelson_gif_prefix}")
  endif()
endmacro()
# ==============================================================================
set(_NELSON_MACOS_SONOMA FALSE)
if(CMAKE_SYSTEM_NAME MATCHES "Darwin")
  if(CMAKE_SYSTEM_VERSION
    MATCHES
    "^23\\."
    OR
    CMAKE_HOST_SYSTEM_VERSION
    MATCHES
    "^23\\."
  )
    set(_NELSON_MACOS_SONOMA TRUE)
  else()
    execute_process(
      COMMAND sw_vers -productVersion
      OUTPUT_VARIABLE _NELSON_MACOS_VERSION
      RESULT_VARIABLE _NELSON_SW_VERS_RESULT
      OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET
    )
    if(NOT _NELSON_SW_VERS_RESULT AND _NELSON_MACOS_VERSION MATCHES "^14\\.")
      set(_NELSON_MACOS_SONOMA TRUE)
    endif()
  endif()
endif()
if(_NELSON_MACOS_SONOMA)
  message(STATUS "macOS Sonoma detected: GIF support disabled.")
  set(WITHOUT_GIF TRUE CACHE BOOL "Disable GIF support" FORCE)
  unset(GIF_INCLUDE_DIR CACHE)
  unset(GIF_INCLUDE_DIR)
  unset(GIF_LIBRARY CACHE)
  unset(GIF_LIBRARY)
  unset(GIF_LIBRARIES CACHE)
  unset(GIF_LIBRARIES)
  set(GIF_FOUND FALSE CACHE BOOL "GIF library disabled on macOS Sonoma" FORCE)
endif()
# ==============================================================================
# Run environment prefix checks only when GIF support is enabled
if(NOT WITHOUT_GIF)
  if(DEFINED ENV{IN_NIX_SHELL})
    if(NOT NIX_EXIT_CODE)
      _nelson_check_gif_prefix("$ENV{NIX_GIFLIB_PREFIX}")
    endif()
  elseif(DEFINED ENV{CONDA_PREFIX})
    _nelson_check_gif_prefix("$ENV{CONDA_PREFIX}")
  endif()

  if(CMAKE_SYSTEM_NAME
    MATCHES
    "Darwin"
    AND
    GIF_INCLUDE_DIR
    MATCHES
    "/Library/Frameworks/Mono.framework/Headers"
  )
    unset(GIF_INCLUDE_DIR CACHE)
    unset(GIF_INCLUDE_DIR)
  endif()

  if(NOT GIF_INCLUDE_DIR OR NOT GIF_LIBRARIES)
    if(CMAKE_SYSTEM_NAME MATCHES "Darwin")
      if(DEFINED ENV{HOMEBREW_PREFIX})
        _nelson_check_gif_prefix("$ENV{HOMEBREW_PREFIX}")
        _nelson_check_gif_prefix("$ENV{HOMEBREW_PREFIX}/opt/giflib")
      endif()
      _nelson_check_gif_prefix("/opt/homebrew")
      _nelson_check_gif_prefix("/opt/homebrew/opt/giflib")
      _nelson_check_gif_prefix("/usr/local")
      _nelson_check_gif_prefix("/usr/local/opt/giflib")
      if(NOT GIF_INCLUDE_DIR OR NOT GIF_LIBRARIES)
        execute_process(
          COMMAND brew --prefix giflib
          OUTPUT_VARIABLE BREW_GIFLIB_PREFIX
          RESULT_VARIABLE BREW_EXIT_CODE
          ERROR_VARIABLE BREW_ERROR
          OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_STRIP_TRAILING_WHITESPACE
        )
        if(NOT BREW_EXIT_CODE)
          _nelson_check_gif_prefix("${BREW_GIFLIB_PREFIX}")
        endif()

      endif()
    endif()
  endif()
endif()
# ==============================================================================
if(NOT WITHOUT_GIF)
  if(NOT GIF_INCLUDE_DIR OR NOT GIF_LIBRARIES)
    include(FindGIF)
    find_package(GIF REQUIRED)
    if(CMAKE_SYSTEM_NAME
      MATCHES
      "Darwin"
      AND
      GIF_INCLUDE_DIR
      MATCHES
      "/Library/Frameworks/Mono.framework/Headers"
    )
      foreach(_nelson_gif_library IN LISTS GIF_LIBRARIES GIF_LIBRARY)
        _nelson_check_gif_include_from_library("${_nelson_gif_library}")
      endforeach()
    endif()
  else()
    set(GIF_FOUND TRUE CACHE BOOL "GIF library found" FORCE)
  endif()
else()
  set(GIF_FOUND FALSE CACHE BOOL "GIF library disabled" FORCE)
endif()
# ==============================================================================
if(NOT
  WITHOUT_GIF
  AND
  GIF_FOUND
  AND
  CMAKE_SYSTEM_NAME
  MATCHES
  "Darwin"
  AND
  GIF_INCLUDE_DIR
  MATCHES
  "/Library/Frameworks/Mono.framework/Headers"
)
  message(FATAL_ERROR
    "Found Mono's incompatible gif_lib.h at ${GIF_INCLUDE_DIR}. Install giflib with Homebrew or set GIF_INCLUDE_DIR/GIF_LIBRARIES to a giflib 5+ installation."
  )
endif()
# ==============================================================================
if(GIF_FOUND)
  include_directories(BEFORE ${GIF_INCLUDE_DIR})
  message(STATUS "libgif include directory: ${GIF_INCLUDE_DIR}")
  message(STATUS "libgif library path: ${GIF_LIBRARIES}")
else()
  message(STATUS "GIF library support disabled.")
endif()
# ==============================================================================
# Mark variables as advanced to avoid clutter in the CMake cache
mark_as_advanced(GIF_INCLUDE_DIR GIF_LIBRARIES)
