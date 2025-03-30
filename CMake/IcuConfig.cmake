# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin" AND NOT DEFINED ENV{CONDA_PREFIX} AND NOT DEFINED ENV{IN_NIX_SHELL})
    # Try finding ICU with Homebrew
    execute_process(
      COMMAND brew --prefix icu4c
      OUTPUT_VARIABLE BREW_ICU_PREFIX
      RESULT_VARIABLE NONZERO_BREW_EXIT_CODE
      ERROR_VARIABLE BREW_ERROR
      OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_STRIP_TRAILING_WHITESPACE)
    
    if(NOT NONZERO_BREW_EXIT_CODE)
      set(CMAKE_ICU_PATH ${BREW_ICU_PREFIX})
    endif()

    # If not found via Homebrew, try default paths
    if(NOT CMAKE_ICU_PATH)
      set(POSSIBLE_ICU_PATHS
        "$ENV{HOMEBREW_PREFIX}/opt/icu4c"
        "$ENV{HOMEBREW_CELLAR}/icu4c/67.1"
        "$ENV{HOMEBREW_CELLAR}/icu4c/64.2"
      )
    
      foreach(PATH ${POSSIBLE_ICU_PATHS})
        if(EXISTS "${PATH}/include" AND EXISTS "${PATH}/lib/libicuuc${CMAKE_SHARED_LIBRARY_SUFFIX}")
          set(CMAKE_ICU_PATH ${PATH})
          break()
        endif()
      endforeach()
    endif()  
    # Set ICU_INCLUDE_DIRS and ICU_LIBRARIES if found
    if(CMAKE_ICU_PATH)
      set(ICU_INCLUDE_DIRS "${CMAKE_ICU_PATH}/include")
      set(ICU_LIBRARIES "${CMAKE_ICU_PATH}/lib/libicuuc${CMAKE_SHARED_LIBRARY_SUFFIX};${CMAKE_ICU_PATH}/lib/libicui18n${CMAKE_SHARED_LIBRARY_SUFFIX}")
    endif()
endif()
# ==============================================================================
if(NOT ICU_INCLUDE_DIRS OR NOT ICU_LIBRARIES)
  find_package(
    ICU
    COMPONENTS uc i18n
    REQUIRED)
endif()
# ==============================================================================
if(ICU_INCLUDE_DIRS AND ICU_LIBRARIES)
  message(STATUS "ICU_INCLUDE_DIRS=${ICU_INCLUDE_DIRS}")
  message(STATUS "ICU_LIBRARIES=${ICU_LIBRARIES}")
else()
  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin" AND NOT DEFINED ENV{IN_NIX_SHELL})
    message(FATAL_ERROR "Please install: brew install icu4c and brew link icu4c --force ")
  else()
    message(FATAL_ERROR "Please install icu.")
  endif()
endif()
# ==============================================================================
