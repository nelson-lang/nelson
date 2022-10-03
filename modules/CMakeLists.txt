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
  nelson_manager)
# ==============================================================================
if(WITHOUT_I18N_MODULE)
  list(APPEND moduleslist no-i18n)
else()
  list(APPEND moduleslist i18n)
endif()
# ==============================================================================
  list(
  APPEND
  moduleslist
  elementary_mathematics
  error_manager
  types
  api_nelson
  stream_manager
  display_format
  operators
  elementary_functions
  special_functions
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
  text_completion
  handle
  com_engine
  modules_manager
  engine
  profiler)
# ==============================================================================
list (APPEND
      modules_partial_disable
      MEX
      FILE_ARCHIVER
      IPC
      SIO_CLIENT
      LOCALIZATION
      )

foreach(mod ${modules_partial_disable})
  string(TOLOWER ${mod} lower_name)
  if(WITHOUT_${mod}_MODULE)
    list(APPEND moduleslist no-${lower_name})
  else()
    list(APPEND moduleslist ${lower_name})
  endif()
endforeach(mod)      
# ==============================================================================
list (APPEND
      modules_easy_to_disable
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
      )

foreach(mod ${modules_easy_to_disable})
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
