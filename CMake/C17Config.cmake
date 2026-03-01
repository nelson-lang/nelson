# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
include(Clang-cxx-dev-tools)
include(CheckCXXCompilerFlag)
# ==============================================================================
# C/C++ standard requirements
# ==============================================================================
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
# ==============================================================================
# Position-independent code (required for shared libraries)
# ==============================================================================
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
# ==============================================================================
# Visibility
# ==============================================================================
if(NOT WIN32)
  set(CMAKE_CXX_VISIBILITY_PRESET hidden)
  set(CMAKE_C_VISIBILITY_PRESET hidden)
  set(CMAKE_VISIBILITY_INLINES_HIDDEN ON)
else()
  add_compile_definitions(UNICODE)
endif()
# ==============================================================================
# AVX2
# ==============================================================================
if(ENABLE_AVX2)
  check_cxx_compiler_flag("-mavx2" COMPILER_SUPPORTS_AVX2)
  if(COMPILER_SUPPORTS_AVX2)
    message(STATUS "Enabling AVX2 optimizations")
    add_compile_options(-mavx2)
  endif()
endif()
# ==============================================================================
# Apple M-series optimizations
# ==============================================================================
if(CMAKE_SYSTEM_NAME
  STREQUAL
  "Darwin"
  AND
  CMAKE_SYSTEM_PROCESSOR
  STREQUAL
  "arm64"
)
  message(STATUS "Enabling Apple M-series optimizations")
  add_compile_options(-march=native)
endif()
# ==============================================================================
# Large file support checks
# ==============================================================================
include(CheckSymbolExists)
check_symbol_exists("fseek64" "stdio.h" HAVE_FSEEK64)
check_symbol_exists("ftell64" "stdio.h" HAVE_FTELL64)
