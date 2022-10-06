# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
include_directories(${EIGEN_INCLUDE_DIR})
# ==============================================================================
list(
  APPEND
  moduleslist
  nelson_manager
  characters_encoding
  i18n
  localization
  elementary_mathematics
  error_manager
  types
  api_nelson
  stream_manager
  display_format
  operators
  elementary_functions
  interpreter
  debugger
  double
  single
  sparse
  functions_manager
  constructors_functions
  console
  logical
  time
  string
  integer
  linear_algebra
  files_folders_functions
  os_functions
  core
  function_handle
  memory_manager
  overload
  data_structures
  history_manager
  terminal
  handle
  modules_manager
  engine
  profiler
  mex
  ipc
  sio_client
  file_archiver)
# ==============================================================================
list(
  APPEND
  modules_fully_disabled
  GUI
  FFTW
  SLICOT
  F2C
  MPI
  AUDIO
  WEBTOOLS
  PARALLEL
  GRAPHICS
  QML_ENGINE
  SIGNAL_PROCESSING
  DATA_ANALYSIS
  VALIDATORS
  HDF5
  MATIO
  HELP_TOOLS
  HELP_BROWSER
  TEXT_EDITOR
  DYNAMIC_LINK
  TESTS_MANAGER
  JSON
  NIG
  ASSERT_FUNCTIONS
  STATISTICS
  TRIGONOMETRIC_FUNCTIONS
  POLYNOMIAL_FUNCTIONS
  RANDOM
  SPECIAL_FUNCTIONS
  TEXT_COMPLETION)

foreach(mod ${modules_fully_disabled})
  if(NOT WITHOUT_${mod}_MODULE)
    string(TOLOWER ${mod} mod)
    list(APPEND moduleslist ${mod})
  endif()
endforeach(mod)
# ==============================================================================
list(APPEND moduleslist main)
# ==============================================================================
foreach(mod ${moduleslist})
  add_subdirectory(${mod})
endforeach(mod)
# ==============================================================================
install(FILES ${CMAKE_SOURCE_DIR}/modules/modules.m
        DESTINATION ${ROOT_OUTPUT}/modules)
# ==============================================================================
