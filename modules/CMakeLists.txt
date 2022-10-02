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
  com_engine
  nig
  help_browser
  dynamic_link
  localization
  modules_manager
  engine
  assert_functions
  tests_manager
  json
  profiler
  statistics
  polynomial_functions)
# ==============================================================================
if (WITHOUT_MEX_MODULE)
list(APPEND moduleslist no-mex)
else()
list(APPEND moduleslist mex)
endif()
# ==============================================================================
if (WITHOUT_FILE_ARCHIVER_MODULE)
list(APPEND moduleslist no-file_archiver)
else()
list(APPEND moduleslist file_archiver)
endif()
# ==============================================================================
if (WITHOUT_IPC_MODULE)
list(APPEND moduleslist no-ipc)
else()
list(APPEND moduleslist ipc)
endif()
# ==============================================================================
if (WITHOUT_SIO_CLIENT_MODULE)
list(APPEND moduleslist no-sio_client)
else()
list(APPEND moduleslist sio_client)
endif()
# ==============================================================================
if (NOT WITHOUT_FFTW_MODULE)
list(APPEND moduleslist fftw)
endif()
# ==============================================================================
if(NOT WITHOUT_SLICOT_MODULE)
list(APPEND moduleslist slicot)
endif()
# ==============================================================================
if(NOT WITHOUT_F2C_MODULE)
list(APPEND moduleslist f2c)
endif()
# ==============================================================================
if(NOT WITHOUT_MPI_MODULE)
list(APPEND moduleslist mpi)
endif()
# ==============================================================================
if(NOT WITHOUT_AUDIO_MODULE)
list(APPEND moduleslist audio)
endif()
# ==============================================================================
if(NOT WITHOUT_WEBTOOLS_MODULE)
list(APPEND moduleslist webtools)
endif()
# ==============================================================================
if(NOT WITHOUT_PARALLEL_MODULE)
list(APPEND moduleslist parallel)
endif()
# ==============================================================================
if(NOT WITHOUT_GRAPHICS_MODULE)
list(APPEND moduleslist graphics)
endif()
# ==============================================================================
if(NOT WITHOUT_QML_ENGINE_MODULE)
list(APPEND moduleslist qml_engine)
endif()
# ==============================================================================
if(NOT WITHOUT_SIGNAL_PROCESSING_MODULE)
list(APPEND moduleslist signal_processing)
endif()
# ==============================================================================
if(NOT WITHOUT_VALIDATORS_MODULE)
list(APPEND moduleslist validators)
endif()
# ==============================================================================
if(NOT WITHOUT_HDF5_MODULE)
list(APPEND moduleslist hdf5)
endif()
# ==============================================================================
if(NOT WITHOUT_MATIO_MODULE)
list(APPEND moduleslist matio)
endif()
# ==============================================================================
if(NOT WITHOUT_HELP_TOOLS_MODULE)
list(APPEND moduleslist help_tools)
endif()
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
