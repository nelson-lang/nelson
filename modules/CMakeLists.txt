# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
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
  nelson_manager
  i18n
  elementary_mathematics
  error_manager
  types
  api_nelson
  stream_manager
  display_format
  operators
  elementary_functions
  data_analysis
  special_functions
  interpreter
  debugger
  double
  single
  sparse
  functions_manager
  constructors_functions
  linear_algebra
  console
  logical
  time
  trigonometric_functions
  string
  integer
  random
  files_folders_functions
  os_functions
  core
  function_handle
  memory_manager
  overload
  data_structures
  history_manager
  terminal
  gui
  text_completion
  text_editor
  handle
  qml_engine
  com_engine
  nig
  f2c
  help_browser
  dynamic_link
  help_tools
  localization
  modules_manager
  sio_client
  engine
  assert_functions
  tests_manager
  audio
  mpi
  json
  hdf5
  matio
  profiler
  file_archiver
  webtools
  graphics
  mex
  ipc
  statistics
  validators
  polynomial_functions
  parallel)
list(APPEND moduleslist main)
# ==============================================================================
if (NOT LGPL21_ONLY)
  if(WITH_FFTW)
    list(APPEND moduleslist fftw)
  endif()
  if(WITH_SLICOT)
    list(APPEND moduleslist slicot)
  endif()
endif()
# ==============================================================================
foreach(mod ${moduleslist})
  include_directories("../build/modules/${mod}/includes")
  include_directories("${mod}/src/c")
  include_directories("${mod}/src/cpp")
  add_subdirectory(${mod})
endforeach(mod)
# ==============================================================================
install(FILES ${CMAKE_SOURCE_DIR}/modules/modules.m
        DESTINATION ${ROOT_OUTPUT}/modules)
# ==============================================================================
