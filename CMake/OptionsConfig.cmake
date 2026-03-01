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
    FILE_ARCHIVER
    IPC
    PARALLEL
    GRAPHICS
    QML_ENGINE
    SIO_CLIENT
    SIGNAL_PROCESSING
    VALIDATORS
    MATIO
    HDF5
    HELP_TOOLS
    TEXT_EDITOR
    DATA_ANALYSIS
    DYNAMIC_LINK
    TESTS_MANAGER
    IMAGE_PROCESSING
    JSON
    XML
    GUI
    NIG
    ASSERT_FUNCTIONS
    STATISTICS
    TRIGONOMETRIC_FUNCTIONS
    POLYNOMIAL_FUNCTIONS
    LOCALIZATION
    I18N
    RANDOM
    SPECIAL_FUNCTIONS
    TEXT_COMPLETION
    CHARACTERS_ENCODING
    GEOMETRY
    PYTHON_ENGINE
    SPREADSHEET
    JULIA_ENGINE)
# ==============================================================================
# Generate modules.m (uses "true"/"false" strings)
# ==============================================================================
foreach(_mod IN LISTS _options_modules)
  if(WITHOUT_${_mod}_MODULE)
    set(WITH_${_mod}_MODULE "false")
  else()
    set(WITH_${_mod}_MODULE "true")
  endif()
endforeach()
# ==============================================================================
configure_file("${CMAKE_SOURCE_DIR}/modules/modules.m.in"
               "${CMAKE_SOURCE_DIR}/modules/modules.m")
# ==============================================================================
# Helper: set WITH_X to 0 or 1 from a boolean condition
# ==============================================================================
macro(
  _nelson_bool_to_01
  _condition
  _out)
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
  "${CMAKE_SOURCE_DIR}/modules/commons/src/include/nlsBuildConfig.h")
# ==============================================================================
