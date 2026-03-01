# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
# Script to generate a .icns file from a source PNG using macOS sips + iconutil.
# Called at build time via add_custom_command().
#
# Required variables (passed via -D on the cmake -P command line):
#   SOURCE_PNG  – absolute path to a high-resolution source PNG (≥ 512×512)
#   OUTPUT_ICNS – absolute path for the generated .icns file
# ==============================================================================
if(NOT SOURCE_PNG)
  message(FATAL_ERROR "SOURCE_PNG is not set")
endif()
if(NOT OUTPUT_ICNS)
  message(FATAL_ERROR "OUTPUT_ICNS is not set")
endif()
if(NOT
   EXISTS
   "${SOURCE_PNG}")
  message(FATAL_ERROR "Source PNG not found: ${SOURCE_PNG}")
endif()
# ==============================================================================
# Create a temporary .iconset directory
# ==============================================================================
get_filename_component(
  _icns_dir
  "${OUTPUT_ICNS}"
  DIRECTORY)
set(_iconset "${_icns_dir}/Nelson.iconset")
file(MAKE_DIRECTORY "${_iconset}")

# Sizes required by Apple: 16, 32, 64, 128, 256, 512 (and @2x retina variants)
# Stored as two parallel lists to avoid CMake semicolon-in-list issues.
set(_pixel_sizes
    16
    32
    32
    64
    128
    256
    256
    512
    512
    1024)
set(_icon_names
    icon_16x16
    icon_16x16@2x
    icon_32x32
    icon_32x32@2x
    icon_128x128
    icon_128x128@2x
    icon_256x256
    icon_256x256@2x
    icon_512x512
    icon_512x512@2x)

list(
  LENGTH
  _pixel_sizes
  _count)
math(
  EXPR
  _last
  "${_count} - 1")
foreach(_i RANGE 0 ${_last})
  list(
    GET
    _pixel_sizes
    ${_i}
    _px)
  list(
    GET
    _icon_names
    ${_i}
    _name)
  set(_out "${_iconset}/${_name}.png")
  execute_process(
    COMMAND sips -z ${_px} ${_px} "${SOURCE_PNG}" --out "${_out}"
    RESULT_VARIABLE _rc
    OUTPUT_QUIET ERROR_QUIET)
  if(NOT
     _rc
     EQUAL
     0)
    message(WARNING "sips failed for ${_px}x${_px} (${_name}), rc=${_rc}")
  endif()
endforeach()

# ==============================================================================
# Convert the .iconset to .icns
# ==============================================================================
execute_process(
  COMMAND iconutil -c icns -o "${OUTPUT_ICNS}" "${_iconset}"
  RESULT_VARIABLE _rc
  OUTPUT_VARIABLE _out
  ERROR_VARIABLE _err)
if(NOT
   _rc
   EQUAL
   0)
  message(FATAL_ERROR "iconutil failed: ${_err}")
endif()

# Clean up
file(REMOVE_RECURSE "${_iconset}")
message(STATUS "Generated ${OUTPUT_ICNS}")
# ==============================================================================
