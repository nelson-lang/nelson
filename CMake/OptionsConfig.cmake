# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
# All optional module names (used for both modules.m and nlsBuildConfig.h)
# ==============================================================================
set(_options_modules
  MEX
  FFTW
  SLICOT
  CONTROL_SYSTEM
  F2C
  MPI
  AUDIO
  WEBTOOLS
  WEBVIEW
  FILE_ARCHIVER
  IPC
  PARALLEL
  GRAPHICS
  QML_ENGINE
  SIGNAL_PROCESSING
  VALIDATORS
  MATIO
  HDF5
  PREFERENCES
  NETCDF
  HELP_TOOLS
  TEXT_EDITOR
  DATA_ANALYSIS
  ODE_SOLVERS
  CATEGORICAL
  DYNAMIC_LINK
  COMPILER
  TESTS_MANAGER
  IMAGE_PROCESSING
  JSON
  AI
  XML
  GUI
  ASSERT_FUNCTIONS
  STATISTICS
  TRIGONOMETRIC_FUNCTIONS
  POLYNOMIAL_FUNCTIONS
  LOCALIZATION
  I18N
  RANDOM
  OPTIMIZATION
  SPECIAL_FUNCTIONS
  TEXT_COMPLETION
  CHARACTERS_ENCODING
  GEOMETRY
  PYTHON_ENGINE
  PARQUET
  SPREADSHEET
  NFLOW_BLOCKS
  NFLOW_ENGINE
  NFLOW_FMI
  NFLOW_GUI
  JULIA_ENGINE
  NMM_GUI
)
# ==============================================================================
# Generate modules.m (uses "true"/"false" strings)
# ==============================================================================
set(NELSON_GENERATED_CONFIG_DIR "${CMAKE_BINARY_DIR}/generated/config")
set(NELSON_GENERATED_INCLUDE_DIR "${NELSON_GENERATED_CONFIG_DIR}/include")
set(NELSON_GENERATED_MODULES_FILE "${NELSON_GENERATED_CONFIG_DIR}/modules.m")
set(NELSON_GENERATED_BUILD_CONFIG
  "${NELSON_GENERATED_INCLUDE_DIR}/nlsBuildConfig.h"
)
file(MAKE_DIRECTORY "${NELSON_GENERATED_INCLUDE_DIR}")
foreach(_mod IN LISTS _options_modules)
  if(WITHOUT_${_mod}_MODULE)
    set(WITH_${_mod}_MODULE "false")
  else()
    set(WITH_${_mod}_MODULE "true")
  endif()
endforeach()
# gpu_engine is opt-in (WITH_GPU_ENGINE_MODULE option, OFF by default)
if(WITH_GPU_ENGINE_MODULE)
  set(WITH_GPU_ENGINE_MODULE_LOADER "true")
else()
  set(WITH_GPU_ENGINE_MODULE_LOADER "false")
endif()
# history_manager's core library is always linked by the terminal, but its
# builtin layer only serves the interactive history of the advanced CLI and the
# GUI; a GUI-less (minimal/CLI) build does not load it.
if(WITHOUT_GUI_MODULE)
  set(WITH_HISTORY_MANAGER_MODULE_LOADER "false")
else()
  set(WITH_HISTORY_MANAGER_MODULE_LOADER "true")
endif()
# ==============================================================================
configure_file(
  "${CMAKE_SOURCE_DIR}/modules/modules.m.in"
  "${NELSON_GENERATED_MODULES_FILE}"
)
# ==============================================================================
# Native in-place dev builds run directly from the source tree: the runtime
# root-path detection walks up from the binaries directory looking for
# "<root>/modules/modules.m". WebAssembly preloads the generated file into its
# virtual filesystem instead, so it does not need the copy in the source tree.
# This path is listed in .gitignore.
# ==============================================================================
if(NOT CMAKE_SYSTEM_NAME STREQUAL "Emscripten")
  configure_file(
    "${CMAKE_SOURCE_DIR}/modules/modules.m.in"
    "${CMAKE_SOURCE_DIR}/modules/modules.m"
  )
endif()
# ==============================================================================
# Helper: set WITH_X to 0 or 1 from a boolean condition
# ==============================================================================
macro(_nelson_bool_to_01
  _condition
  _out
)
  if(${_condition})
    set(${_out} 1)
  else()
    set(${_out} 0)
  endif()
endmacro()
# ==============================================================================
_nelson_bool_to_01(GIF_FOUND WITH_GIF)
_nelson_bool_to_01(TIFF_FOUND WITH_TIFF)
_nelson_bool_to_01(TAGLIB_FOUND WITH_TAGLIB)
_nelson_bool_to_01(ARPACK_FOUND WITH_ARPACK)
# ==============================================================================
# Invert WITHOUT_* -> WITH_*
if(WITHOUT_OPENMP)
  set(WITH_OPENMP 0)
else()
  set(WITH_OPENMP 1)
endif()
if(WITHOUT_TBB)
  set(WITH_TBB 0)
else()
  set(WITH_TBB 1)
endif()
if(WITHOUT_LIBGIT2)
  set(WITH_LIBGIT2 0)
else()
  set(WITH_LIBGIT2 1)
endif()
if(WITHOUT_LIBCURL)
  set(WITH_LIBCURL 0)
else()
  set(WITH_LIBCURL 1)
endif()
if(WITHOUT_FILEWATCHER)
  set(WITH_FILE_WATCHER 0)
else()
  set(WITH_FILE_WATCHER 1)
endif()
# ==============================================================================
# Generate nlsBuildConfig.h (uses 0/1 integers)
# ==============================================================================
include(GNUInstallDirs)
set(NLS_LIBRARY_PATH_PREFIX ${CMAKE_INSTALL_LIBDIR})
set(NLS_RUNTIME_PATH_PREFIX ${CMAKE_INSTALL_BINDIR})
# ==============================================================================
foreach(_mod IN LISTS _options_modules)
  if(WITHOUT_${_mod}_MODULE)
    set(WITH_${_mod}_MODULE 0)
  else()
    set(WITH_${_mod}_MODULE 1)
  endif()
endforeach()
# ==============================================================================
configure_file(
  "${CMAKE_SOURCE_DIR}/modules/commons/src/include/nlsBuildConfig.h.in"
  "${NELSON_GENERATED_BUILD_CONFIG}"
)
include_directories(BEFORE "${NELSON_GENERATED_INCLUDE_DIR}")
