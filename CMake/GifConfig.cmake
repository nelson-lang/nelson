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
  if(EXISTS "${PREFIX}/include/gif_lib.h")
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
  if(EXISTS "${PREFIX}/lib/libgif${CMAKE_SHARED_LIBRARY_SUFFIX}")
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
  _nelson_check_gif_include(${PREFIX})
  _nelson_check_gif_library(${PREFIX})
endmacro()
# ==============================================================================
# Run environment prefix checks only when GIF support is enabled
if(NOT WITHOUT_GIF)
  if(DEFINED ENV{IN_NIX_SHELL})
    if(NOT NIX_EXIT_CODE)
      _nelson_check_gif_prefix("$ENV{NIX_GIFLIB_PREFIX}")
    endif()
  elseif(DEFINED ENV{CONDA_PREFIX})
    _nelson_check_gif_prefix("$ENV{CONDA_PREFIX}")
  elseif(DEFINED ENV{HOMEBREW_PREFIX})
    if(NOT GIF_INCLUDE_DIR OR NOT GIF_LIBRARIES)
      if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
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
  else()
    set(GIF_FOUND TRUE CACHE BOOL "GIF library found" FORCE)
  endif()
else()
  set(GIF_FOUND FALSE CACHE BOOL "GIF library disabled" FORCE)
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
