# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
include(Clang-cxx-dev-tools)
# ==============================================================================
include(CheckCXXCompilerFlag)
# ==============================================================================
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
# ==============================================================================
if("${CMAKE_CXX_COMPILE_FEATURES}" MATCHES "cxx_std_17")
  set(CMAKE_CXX_STANDARD 17)
else()
  message(
    FATAL_ERROR
      "The compiler ${CMAKE_CXX_COMPILER} has no C++17 support. Please use a different C++ compiler."
  )
endif()
# ==============================================================================
check_cxx_compiler_flag("-fPIC" COMPILER_SUPPORTS_FPIC)
if(COMPILER_SUPPORTS_FPIC)
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC")
endif()
if(NOT WIN32)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fvisibility=hidden")
else()
  add_compile_definitions(UNICODE)
endif()
# ==============================================================================
check_cxx_compiler_flag("-mavx2" COMPILER_SUPPORTS_AVX2)
if(ENABLE_AVX2 AND COMPILER_SUPPORTS_AVX2)
  message(STATUS "Enabling AVX2 optimizations")
  add_compile_options(-mavx2)
endif()
# ==============================================================================
if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  if(CMAKE_SYSTEM_PROCESSOR STREQUAL "arm64")
    message(STATUS "Enabling Apple M-series optimizations")
    add_compile_options(-march=native)
  endif()    
endif()
# ==============================================================================
include(CheckSymbolExists)
check_symbol_exists("fseek64" "stdio.h" HAVE_FSEEK64)
check_symbol_exists("ftell64" "stdio.h" HAVE_FTELL64)
# ==============================================================================
