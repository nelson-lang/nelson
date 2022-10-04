# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
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
  CHARACTERS_ENCODING)

if(EXISTS ${CMAKE_SOURCE_DIR}/modules/modules.m)

else()
  foreach(mod ${without_module})
    if (WITHOUT_${mod}_MODULE)
      set(WITH_${mod}_MODULE "false")
    else()
      set(WITH_${mod}_MODULE "true")
    endif()
  endforeach(mod)

  configure_file("${CMAKE_SOURCE_DIR}/modules/modules.m.in"
                 "${CMAKE_SOURCE_DIR}/modules/modules.m")
endif()
# ==============================================================================
if(EXISTS ${CMAKE_SOURCE_DIR}/modules/types/src/include/nlsConfig.h)

else()

  if (WITHOUT_OPENMP)
    set(WITH_OPENMP 0)
  else()
    set(WITH_OPENMP 1)
  endif()

  foreach(mod ${without_module})
    if (WITHOUT_${mod}_MODULE)
      set(WITH_${mod}_MODULE 0)
    else()
      set(WITH_${mod}_MODULE 1)
    endif()
  endforeach(mod)

  configure_file("${CMAKE_SOURCE_DIR}/modules/types/src/include/nlsConfig.h.in"
                 "${CMAKE_SOURCE_DIR}/modules/types/src/include/nlsConfig.h")
endif()
# ==============================================================================
