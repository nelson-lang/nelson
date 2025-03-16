# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  execute_process(
    COMMAND brew --prefix libffi
    OUTPUT_VARIABLE BREW_LIBFFI_PREFIX
    RESULT_VARIABLE NONZERO_BREW_EXIT_CODE
    ERROR_VARIABLE BREW_ERROR
    OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_STRIP_TRAILING_WHITESPACE)
  if(NOT (NONZERO_BREW_EXIT_CODE))
    set(CMAKE_LIBFFI_PATH ${BREW_LIBFFI_PREFIX})
  endif()

  # If not found via Homebrew, try default paths
  if(NOT LIBFFI_INCLUDE_DIR)
    set(POSSIBLE_LIBFFI_PATHS
      "$ENV{HOMEBREW_PREFIX}/opt/libffi/include"
      "$ENV{HOMEBREW_CELLAR}/opt/libffi/include"
      "/usr/local/opt/libffi/include"
      "${CMAKE_LIBFFI_PATH}/lib/libffi-3.2.1/include"
      )
    foreach(PATH ${POSSIBLE_LIBFFI_PATHS})
      if(EXISTS "${PATH}")
        set(LIBFFI_INCLUDE_DIR ${PATH})
        break()
      endif()
    endforeach()
  endif()  

  if(NOT LIBFFI_LIBRARY)
    set(POSSIBLE_LIBFFI_LIBRARIES
      "${CMAKE_LIBFFI_PATH}/lib/libffi-3.2.1/lib/libffi${CMAKE_SHARED_LIBRARY_SUFFIX}"
      "${CMAKE_LIBFFI_PATH}/lib/libffi${CMAKE_SHARED_LIBRARY_SUFFIX}"
      )
      foreach(PATH ${POSSIBLE_LIBFFI_LIBRARIES})
        if(EXISTS "${PATH}")
          set(LIBFFI_LIBRARY ${PATH})
          break()
        endif()
      endforeach()
  endif()

endif()
# ==============================================================================
if(NOT LIBFFI_LIBRARY)
  find_package(LibFFI REQUIRED)
endif()
# ==============================================================================
if(LIBFFI_LIBRARY)
  message(STATUS "LIBFFI_INCLUDE_DIR=${LIBFFI_INCLUDE_DIR}")
  message(STATUS "LIBFFI_LIBRARY=${LIBFFI_LIBRARY}")
else()
  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    message(FATAL_ERROR "Please install: brew install libffi.")
  else()
    message(FATAL_ERROR "Please install libffi.")
  endif()
endif()
# ==============================================================================
