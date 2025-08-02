# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  EXECUTE_PROCESS(COMMAND uname -m COMMAND tr -d '\n' OUTPUT_VARIABLE ARCH)
  message("macOS architecture: ${ARCH}")
  set(BIN_DIRECTORY ${PROJECT_BINARY_DIR}/bin/macOS)
  set(CMAKE_OSX_ARCHITECTURES "${ARCH}")
  set(Boost_NO_SYSTEM_PATHS "TRUE")
  set(MAC_FRAMEWORK_FOUNDATION_LIBRARY "-framework Foundation")
  set(MAC_FRAMEWORK_APPKIT_LIBRARY "-framework AppKit")
  if(NOT DEFINED ENV{IN_NIX_SHELL} AND EXISTS "/opt/local/include")
    include_directories(/opt/local/include)
  endif()
  if(NOT DEFINED ENV{IN_NIX_SHELL} AND EXISTS "/opt/local/lib")
    link_directories(/opt/local/lib)
  endif()
  if(NOT DEFINED ENV{IN_NIX_SHELL} AND EXISTS "/usr/local/opt/openblas/lib/")
    link_directories(/usr/local/opt/openblas/lib/)
  endif()
  if(NOT DEFINED ENV{IN_NIX_SHELL} AND EXISTS "$ENV{HOMEBREW_PREFIX}/opt/openblas/lib/")
    link_directories($ENV{HOMEBREW_PREFIX}/opt/openblas/lib/)
  endif()

  if(NOT DEFINED ENV{IN_NIX_SHELL} AND EXISTS "$ENV{HOMEBREW_PREFIX}/opt/libomp/include")
    set(OpenMP_CXX_INCLUDE_DIRS $ENV{HOMEBREW_PREFIX}/opt/libomp/include)
    set(OpenMP_CXX_LIBRARIES $ENV{HOMEBREW_PREFIX}/opt/libomp/lib/libomp${CMAKE_SHARED_LIBRARY_SUFFIX})
  endif()
endif()
# ==============================================================================