# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
# TIFF detection
# ==============================================================================
if (WITHOUT_TIFF)
    set(TIFF_FOUND FALSE CACHE BOOL "TIFF library found" FORCE)
else()
    if (DEFINED ENV{IN_NIX_SHELL})
        if (NOT NIX_EXIT_CODE AND EXISTS "$ENV{NIX_TIFFLIB_PREFIX}/include/tiffio.h")
            set(TIFF_INCLUDE_DIR "$ENV{NIX_TIFFLIB_PREFIX}/include" CACHE STRING "Path to libtiff include directory" FORCE)
        endif()
        if (NOT NIX_EXIT_CODE AND EXISTS "$ENV{NIX_TIFFLIB_PREFIX}/lib/libtiff${CMAKE_SHARED_LIBRARY_SUFFIX}")
            set(TIFF_LIBRARIES "$ENV{NIX_TIFFLIB_PREFIX}/lib/libtiff${CMAKE_SHARED_LIBRARY_SUFFIX}" CACHE STRING "Path to libtiff library" FORCE)
        endif()

    elseif (DEFINED ENV{CONDA_PREFIX})
        set(CONDA_INC_DIR "$ENV{CONDA_PREFIX}/include")
        set(CONDA_LIB_DIR "$ENV{CONDA_PREFIX}/lib")

        if (EXISTS "${CONDA_INC_DIR}/tiffio.h")
            set(TIFF_INCLUDE_DIR "${CONDA_INC_DIR}" CACHE STRING "Path to tiff library include directory" FORCE)
        endif()

        if (EXISTS "${CONDA_LIB_DIR}/libtiff${CMAKE_SHARED_LIBRARY_SUFFIX}")
            set(TIFF_LIBRARIES "${CONDA_LIB_DIR}/libtiff${CMAKE_SHARED_LIBRARY_SUFFIX}" CACHE STRING "Path to tiff library" FORCE)
        endif()
    elseif (DEFINED ENV{HOMEBREW_PREFIX})
        set(HOMEBREW_INC_DIR "$ENV{HOMEBREW_PREFIX}/include")
        set(HOMEBREW_LIB_DIR "$ENV{HOMEBREW_PREFIX}/lib")

        if (EXISTS "${HOMEBREW_INC_DIR}/tiffio.h")
            set(TIFF_INCLUDE_DIR "${HOMEBREW_INC_DIR}" CACHE STRING "Path to tiff library include directory" FORCE)
        endif()

        if (EXISTS "${HOMEBREW_LIB_DIR}/libtiff${CMAKE_SHARED_LIBRARY_SUFFIX}")
            set(TIFF_LIBRARIES "${HOMEBREW_LIB_DIR}/libtiff${CMAKE_SHARED_LIBRARY_SUFFIX}" CACHE STRING "Path to tiff library" FORCE)
        endif()

        if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin" AND (NOT DEFINED TIFF_INCLUDE_DIR OR NOT DEFINED TIFF_LIBRARIES))
            execute_process(
                COMMAND brew --prefix libtiff
                OUTPUT_VARIABLE BREW_TIFFLIB_PREFIX
                RESULT_VARIABLE BREW_EXIT_CODE
                ERROR_VARIABLE BREW_ERROR
                OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_STRIP_TRAILING_WHITESPACE
            )

            if (NOT BREW_EXIT_CODE AND EXISTS "${BREW_TIFFLIB_PREFIX}/include/tiffio.h")
                set(TIFF_INCLUDE_DIR "${BREW_TIFFLIB_PREFIX}/include" CACHE STRING "Path to tiff include directory" FORCE)
            endif()

            if (NOT BREW_EXIT_CODE AND EXISTS "${BREW_TIFFLIB_PREFIX}/lib/libtiff${CMAKE_SHARED_LIBRARY_SUFFIX}")
                set(TIFF_LIBRARIES "${BREW_TIFFLIB_PREFIX}/lib/libtiff${CMAKE_SHARED_LIBRARY_SUFFIX}" CACHE STRING "Path to tiff library" FORCE)
            endif()
        endif()
    endif()
endif()
# ==============================================================================
if (NOT WITHOUT_TIFF)
  if (NOT TIFF_INCLUDE_DIR OR NOT TIFF_LIBRARIES)
    include(FindTIFF)
    find_package(TIFF REQUIRED)
  else()
    set(TIFF_FOUND TRUE CACHE BOOL "TIFF library found" FORCE)
  endif()
else()
  set(TIFF_FOUND FALSE CACHE BOOL "TIFF library disabled" FORCE)
endif()
# ==============================================================================
if (TIFF_FOUND)
  include_directories(BEFORE ${TIFF_INCLUDE_DIR})
  message(STATUS "tiff include directory: ${TIFF_INCLUDE_DIR}")
  message(STATUS "tiff library path: ${TIFF_LIBRARIES}")
else()
  message(STATUS "tiff library support disabled.")
endif()
# ==============================================================================
# Mark variables as advanced to avoid clutter in the CMake cache
mark_as_advanced(TIFF_INCLUDE_DIR TIFF_LIBRARIES)
# ==============================================================================
