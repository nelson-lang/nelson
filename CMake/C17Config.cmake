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
include(Clang-cxx-dev-tools)
# ==============================================================================
include(CheckCXXCompilerFlag)
# ==============================================================================
check_cxx_compiler_flag("-std=c++17" COMPILER_SUPPORTS_CXX17)
if(COMPILER_SUPPORTS_CXX17)
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
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fvisibility=hidden")
# ==============================================================================
include(CheckSymbolExists)
check_symbol_exists("fseek64" "stdio.h" HAVE_FSEEK64)
check_symbol_exists("ftell64" "stdio.h" HAVE_FTELL64)
# ==============================================================================
