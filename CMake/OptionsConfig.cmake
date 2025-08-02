# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
list(APPEND
  without_module
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
  HELP_BROWSER
  TEXT_EDITOR
  DATA_ANALYSIS
  DYNAMIC_LINK
  TESTS_MANAGER
  JSON
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

foreach(mod ${without_module})
  if (WITHOUT_${mod}_MODULE)
    set(WITH_${mod}_MODULE "false")
  else()
    set(WITH_${mod}_MODULE "true")
  endif()
endforeach(mod)

configure_file("${CMAKE_SOURCE_DIR}/modules/modules.m.in"
              "${CMAKE_SOURCE_DIR}/modules/modules.m")
# ==============================================================================
if (GIF_FOUND)
  set(WITH_GIF 1)
else()
  set(WITH_GIF 0)
endif()
# ==============================================================================
if (TIFF_FOUND)
  set(WITH_TIFF 1)
else()
  set(WITH_TIFF 0)
endif()
# ==============================================================================
if (WITHOUT_OPENMP)
  set(WITH_OPENMP 0)
else()
  set(WITH_OPENMP 1)
endif()
# ==============================================================================
if (WITHOUT_TBB)
  set(WITH_TBB 0)
else()
  set(WITH_TBB 1)
endif()
# ==============================================================================
if (WITHOUT_LIBGIT2)
  set(WITH_LIBGIT2 0)
else()
  set(WITH_LIBGIT2 1)
endif()
# ==============================================================================
if (WITHOUT_LIBCURL)
  set(WITH_LIBCURL 0)
else()
  set(WITH_LIBCURL 1)
endif()
# ==============================================================================
if (WITHOUT_FILEWATCHER)
  set(WITH_FILE_WATCHER 0)
else()
  set(WITH_FILE_WATCHER 1)
endif()
# ==============================================================================
if(TAGLIB_FOUND)
  set(WITH_TAGLIB 1)
else()
  set(WITH_TAGLIB 0)
endif()
# ==============================================================================
include(GNUInstallDirs)
set(NLS_LIBRARY_PATH_PREFIX ${CMAKE_INSTALL_LIBDIR})
set(NLS_RUNTIME_PATH_PREFIX ${CMAKE_INSTALL_BINDIR})
# ==============================================================================
foreach(mod ${without_module})
  if (WITHOUT_${mod}_MODULE)
    set(WITH_${mod}_MODULE 0)
  else()
    set(WITH_${mod}_MODULE 1)
  endif()
endforeach(mod)

configure_file("${CMAKE_SOURCE_DIR}/modules/commons/src/include/nlsBuildConfig.h.in"
                 "${CMAKE_SOURCE_DIR}/modules/commons/src/include/nlsBuildConfig.h")
# ==============================================================================
