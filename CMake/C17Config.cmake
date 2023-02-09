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
include(CheckSymbolExists)
check_symbol_exists("fseek64" "stdio.h" HAVE_FSEEK64)
check_symbol_exists("ftell64" "stdio.h" HAVE_FTELL64)
# ==============================================================================
