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
  else()
    message(
      FATAL_ERROR
        "Brew reported an error:\n${BREW_ERROR}.\nPlease resolve this error.")
  endif()
  if(NOT LIBFFI_INCLUDE_DIR)
    if(EXISTS "$ENV{HOMEBREW_PREFIX}/opt/libffi/include/ffi.h")
      set(LIBFFI_INCLUDE_DIR $ENV{HOMEBREW_PREFIX}/opt/libffi/include)
    endif()
  endif()  
  if(NOT LIBFFI_INCLUDE_DIR)
    if(EXISTS "/usr/local/opt/libffi/include/ffi.h")
      set(LIBFFI_INCLUDE_DIR /usr/local/opt/libffi/include)
    endif()
  endif()
  if(NOT LIBFFI_INCLUDE_DIR)
    if(EXISTS "${CMAKE_LIBFFI_PATH}/lib/libffi-3.2.1/include")
      set(LIBFFI_INCLUDE_DIR ${CMAKE_LIBFFI_PATH}/lib/libffi-3.2.1/include)
    endif()
  endif()
  if(NOT LIBFFI_LIBRARY)
    if(EXISTS "${CMAKE_LIBFFI_PATH}/lib/libffi-3.2.1/lib/libffi.dylib")
      set(LIBFFI_LIBRARY
          "${CMAKE_LIBFFI_PATH}/lib/libffi-3.2.1/lib/libffi.dylib")
    endif()
    if(EXISTS "${CMAKE_LIBFFI_PATH}/lib/libffi.dylib")
      set(LIBFFI_LIBRARY ${CMAKE_LIBFFI_PATH}/lib/libffi.dylib)
    endif()
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
