# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# This program is free software; you can redistribute it
# and/or modify it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 2.1 of the License,
# or (at your option) any later version.
#
# Alternatively, you can redistribute it and/or modify it under the terms of the
# GNU General Public License as published by the Free Software Foundation;
# either version 2 of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
# details.
#
# You should have received a copy of the GNU Lesser General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/>.
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
  find_package(LIBFFI REQUIRED)
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
