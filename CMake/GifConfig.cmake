# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
# GIF, JPEG, PNG detection is a hell on MacOs or CONDA environments :'(
# ==============================================================================
if (WITHOUT_GIF)
    set(GIF_FOUND FALSE CACHE BOOL "GIF library found" FORCE)
else()
    if (DEFINED ENV{IN_NIX_SHELL})
        if (NOT NIX_EXIT_CODE AND EXISTS "$ENV{NIX_GIFLIB_PREFIX}/include/gif_lib.h")
            set(GIF_INCLUDE_DIR "$ENV{NIX_GIFLIB_PREFIX}/include" CACHE STRING "Path to giflib include directory" FORCE)
        endif()
        if (NOT NIX_EXIT_CODE AND EXISTS "$ENV{NIX_GIFLIB_PREFIX}/lib/libgif${CMAKE_SHARED_LIBRARY_SUFFIX}")
            set(GIF_LIBRARIES "$ENV{NIX_GIFLIB_PREFIX}/lib/libgif${CMAKE_SHARED_LIBRARY_SUFFIX}" CACHE STRING "Path to giflib library" FORCE)
        endif()
    elseif (DEFINED ENV{CONDA_PREFIX})
        set(CONDA_INC_DIR "$ENV{CONDA_PREFIX}/include")
        set(CONDA_LIB_DIR "$ENV{CONDA_PREFIX}/lib")

        if (EXISTS "${CONDA_INC_DIR}/gif_lib.h")
            set(GIF_INCLUDE_DIR "${CONDA_INC_DIR}" CACHE STRING "Path to giflib include directory" FORCE)
        endif()

        if (EXISTS "${CONDA_LIB_DIR}/libgif${CMAKE_SHARED_LIBRARY_SUFFIX}")
            set(GIF_LIBRARIES "${CONDA_LIB_DIR}/libgif${CMAKE_SHARED_LIBRARY_SUFFIX}" CACHE STRING "Path to giflib library" FORCE)
        endif()
    elseif (DEFINED ENV{HOMEBREW_PREFIX})
        set(HOMEBREW_INC_DIR "$ENV{HOMEBREW_PREFIX}/include")
        set(HOMEBREW_LIB_DIR "$ENV{HOMEBREW_PREFIX}/lib")

        if (EXISTS "${HOMEBREW_INC_DIR}/gif_lib.h")
            set(GIF_INCLUDE_DIR "${HOMEBREW_INC_DIR}" CACHE STRING "Path to giflib include directory" FORCE)
        endif()

        if (EXISTS "${HOMEBREW_LIB_DIR}/libgif${CMAKE_SHARED_LIBRARY_SUFFIX}")
            set(GIF_LIBRARIES "${HOMEBREW_LIB_DIR}/libgif${CMAKE_SHARED_LIBRARY_SUFFIX}" CACHE STRING "Path to giflib library" FORCE)
        endif()

        if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin" AND (NOT DEFINED GIF_INCLUDE_DIR OR NOT DEFINED GIF_LIBRARIES))
            execute_process(
                COMMAND brew --prefix giflib
                OUTPUT_VARIABLE BREW_GIFLIB_PREFIX
                RESULT_VARIABLE BREW_EXIT_CODE
                ERROR_VARIABLE BREW_ERROR
                OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_STRIP_TRAILING_WHITESPACE
            )

            if (NOT BREW_EXIT_CODE AND EXISTS "${BREW_GIFLIB_PREFIX}/include/gif_lib.h")
                set(GIF_INCLUDE_DIR "${BREW_GIFLIB_PREFIX}/include" CACHE STRING "Path to giflib include directory" FORCE)
            endif()

            if (NOT BREW_EXIT_CODE AND EXISTS "${BREW_GIFLIB_PREFIX}/lib/libgif${CMAKE_SHARED_LIBRARY_SUFFIX}")
                set(GIF_LIBRARIES "${BREW_GIFLIB_PREFIX}/lib/libgif${CMAKE_SHARED_LIBRARY_SUFFIX}" CACHE STRING "Path to giflib library" FORCE)
            endif()
        endif()
    endif()
endif()
# ==============================================================================
if (NOT WITHOUT_GIF)
  if (NOT GIF_INCLUDE_DIR OR NOT GIF_LIBRARIES)
    include(FindGIF)
    find_package(GIF REQUIRED)
  else()
    set(GIF_FOUND TRUE CACHE BOOL "GIF library found" FORCE)
  endif()
else()
  set(GIF_FOUND FALSE CACHE BOOL "GIF library disabled" FORCE)
endif()
# ==============================================================================
if (GIF_FOUND)
    include_directories(BEFORE ${GIF_INCLUDE_DIR})
    message(STATUS "libgif include directory: ${GIF_INCLUDE_DIR}")
    message(STATUS "libgif library path: ${GIF_LIBRARIES}")
else()
    message(STATUS "GIF library support disabled.")
endif()
# ==============================================================================
# Mark variables as advanced to avoid clutter in the CMake cache
mark_as_advanced(GIF_INCLUDE_DIR GIF_LIBRARIES)
# ==============================================================================
