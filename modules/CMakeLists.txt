# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
include_directories(${EIGEN_INCLUDE_DIR})
# ==============================================================================
list(
  APPEND
  moduleslist
  characters_encoding
  commons
  nelson_manager
  debugger
  i18n
  localization
  elementary_mathematics
  error_manager
  types
  stream_manager
  display_format
  operators
  elementary_functions
  interpreter
  modules_manager
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
  dictionary
  table
  history_manager
  terminal
  handle
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
  F2C
  GUI
  TEXT_COMPLETION
  QML_ENGINE
  GRAPHICS
  GRAPHICS_IO
  HELP_BROWSER
  TEXT_EDITOR
  FFTW
  VALIDATORS
  MPI
  AUDIO
  WEBTOOLS
  PARALLEL
  SIGNAL_PROCESSING
  DATA_ANALYSIS
  HDF5
  MATIO
  HELP_TOOLS
  DYNAMIC_LINK
  TESTS_MANAGER
  JSON
  NIG
  ASSERT_FUNCTIONS
  SPECIAL_FUNCTIONS
  STATISTICS
  TRIGONOMETRIC_FUNCTIONS
  POLYNOMIAL_FUNCTIONS
  RANDOM
  GEOMETRY
  SLICOT
  CONTROL_SYSTEM
  PYTHON_ENGINE
  SPREADSHEET
  JULIA_ENGINE
  IMAGE_PROCESSING)

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
include(GNUInstallDirs)
# ==============================================================================
install(FILES ${CMAKE_SOURCE_DIR}/modules/modules.m
        DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/modules)
# ==============================================================================
