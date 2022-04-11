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
    COMMAND brew --prefix icu4c
    OUTPUT_VARIABLE BREW_ICU_PREFIX
    RESULT_VARIABLE NONZERO_BREW_EXIT_CODE
    ERROR_VARIABLE BREW_ERROR
    OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_STRIP_TRAILING_WHITESPACE)
  if(NOT (NONZERO_BREW_EXIT_CODE))
    set(CMAKE_ICU_PATH ${BREW_ICU_PREFIX})
  else()
    message(
      FATAL_ERROR
        "Brew reported an error:\n${BREW_ERROR}.\nPlease resolve this error.")
  endif()

  if(NOT ICU_INCLUDE_DIRS)
    if(EXISTS "/usr/local/Cellar/icu4c/67.1/include")
      set(ICU_INCLUDE_DIRS "/usr/local/Cellar/icu4c/67.1/include")
    endif()
  endif()
  if(NOT ICU_INCLUDE_DIRS)
    if(EXISTS "/usr/local/Cellar/icu4c/64.2/include")
      set(ICU_INCLUDE_DIRS "/usr/local/Cellar/icu4c/64.2/include")
    endif()
  endif()
  if(NOT ICU_INCLUDE_DIRS)
    if(EXISTS "${CMAKE_ICU_PATH}/include")
      set(ICU_INCLUDE_DIRS "${CMAKE_ICU_PATH}/include")
    endif()
  endif()
  if(NOT ICU_LIBRARIES)
    if(EXISTS "/usr/local/Cellar/icu4c/67.1/lib/libicuuc.dylib")
      set(ICU_LIBRARIES /usr/local/Cellar/icu4c/67.1/lib/libicuuc.dylib)
    endif()
    if(EXISTS "/usr/local/Cellar/icu4c/67.1/lib/libicui18n.dylib")
      set(ICU_LIBRARIES
          "${ICU_LIBRARIES};/usr/local/Cellar/icu4c/67.1/lib/libicui18n.dylib")
    endif()
  endif()
  if(NOT ICU_LIBRARIES)
    if(EXISTS "/usr/local/Cellar/icu4c/64.2/lib/libicuuc.dylib")
      set(ICU_LIBRARIES /usr/local/Cellar/icu4c/64.2/lib/libicuuc.dylib)
    endif()
    if(EXISTS "/usr/local/Cellar/icu4c/64.2/lib/libicui18n.dylib")
      set(ICU_LIBRARIES
          "${ICU_LIBRARIES};/usr/local/Cellar/icu4c/64.2/lib/libicui18n.dylib")
    endif()
  endif()
  if(NOT ICU_LIBRARIES)
    if(EXISTS "${CMAKE_ICU_PATH}/lib/libicuuc.dylib")
      set(ICU_LIBRARIES ${CMAKE_ICU_PATH}/lib/libicuuc.dylib)
    endif()
    if(EXISTS ${CMAKE_ICU_PATH}/lib/libicui18n.dylib)
      set(ICU_LIBRARIES
          "${ICU_LIBRARIES};${CMAKE_ICU_PATH}/lib/libicui18n.dylib")
    endif()
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
  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    message(FATAL_ERROR "Please install: brew install icu4c.")
  else()
    message(FATAL_ERROR "Please install icu.")
  endif()
endif()
# ==============================================================================
