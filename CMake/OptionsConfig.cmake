# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
if(EXISTS ${CMAKE_SOURCE_DIR}/modules/modules.m)

else()
  if (LGPL21_ONLY)
    set(WITH_FFTW_MODULE "true")
    set(WITH_SLICOT_MODULE "true")
  else()
    if (WITH_FFTW)
      set(WITH_FFTW_MODULE "true")
    else()
      set(WITH_FFTW_MODULE "false")
    endif()
    if (WITH_SLICOT)
      set(WITH_SLICOT_MODULE "true")
    else()
      set(WITH_SLICOT_MODULE "false")
    endif()
  endif()
  configure_file("${CMAKE_SOURCE_DIR}/modules/modules.m.in"
                 "${CMAKE_SOURCE_DIR}/modules/modules.m")
endif()
# ==============================================================================
