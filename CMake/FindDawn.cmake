# FindDawn
#
# Try to find the Dawn (WebGPU) library.
#
# Once done this will define:
#
#  DAWN_FOUND          - True if Dawn found.
#  DAWN_LIBRARIES      - Dawn libraries.
#  DAWN_INCLUDE_DIRS   - where to find webgpu.h
#
# A Dawn CMake installation exporting dawn::webgpu_dawn is preferred.  The
# bundled Windows package layout remains supported:
#   <thirdparty>/dawn/include/webgpu.h
#   <thirdparty>/dawn/lib/webgpu_dawn.lib
#   <thirdparty>/dawn/bin/webgpu_dawn.dll
#
# DAWN_HOME can be set to point to a custom Dawn install prefix.
#
#=============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
#=============================================================================
# This file is part of Nelson.
#=============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
#=============================================================================
# Prefer Dawn's installed package.  CONFIG avoids recursively loading this
# find-module again.
find_package(Dawn CONFIG QUIET NO_MODULE HINTS ${DAWN_HOME})

if(TARGET dawn::webgpu_dawn)
  set(DAWN_LIBRARIES dawn::webgpu_dawn)
  if(TARGET dawn::dawn_public_config)
    get_target_property(DAWN_INCLUDE_DIRS
      dawn::dawn_public_config
      INTERFACE_INCLUDE_DIRECTORIES
    )
  else()
    set(DAWN_INCLUDE_DIRS)
  endif()
  set(Dawn_FOUND TRUE)
  set(DAWN_FOUND TRUE)
else()
  # Compatibility with the flat prebuilt package used on Windows.
  find_path(DAWN_INCLUDE_DIR
    NAMES
    webgpu/webgpu.h
    dawn/webgpu.h
    webgpu.h
    HINTS
    ${DAWN_HOME}/include
    ${NELSON_WINDOWS_THIRDPARTY_ROOT}/dawn/include
    PATH_SUFFIXES
    webgpu
    dawn
  )

  find_library(DAWN_LIBRARY
    NAMES
    webgpu_dawn
    HINTS
    ${DAWN_HOME}/lib
    ${NELSON_WINDOWS_THIRDPARTY_ROOT}/dawn/lib
  )

  mark_as_advanced(DAWN_INCLUDE_DIR DAWN_LIBRARY)

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(
    Dawn
    REQUIRED_VARS
    DAWN_LIBRARY
    DAWN_INCLUDE_DIR
  )

  set(DAWN_FOUND ${Dawn_FOUND})
  if(DAWN_FOUND)
    set(DAWN_LIBRARIES ${DAWN_LIBRARY})
    set(DAWN_INCLUDE_DIRS ${DAWN_INCLUDE_DIR})
  else()
    set(DAWN_LIBRARIES)
    set(DAWN_INCLUDE_DIRS)
  endif()
endif()
