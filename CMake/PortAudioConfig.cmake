# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
if(PORTAUDIO_FOUND)
  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    if(DEFINED ENV{CONDA_PREFIX})
      set(PORTAUDIO_LIBRARIES $ENV{CONDA_PREFIX}/lib/libportaudio.dylib)
      set(PORTAUDIO_INCLUDE_DIRS $ENV{CONDA_PREFIX}/include)
    else()
      # workaround for github CI with catalina
      if(EXISTS "$ENV{HOMEBREW_CELLAR}/portaudio/19.7.0/include")
        set(PORTAUDIO_INCLUDE_DIRS $ENV{HOMEBREW_CELLAR}/portaudio/19.7.0/include)
      elseif(EXISTS "$ENV{HOMEBREW_CELLAR}/portaudio/19.6.0/include")
        set(PORTAUDIO_INCLUDE_DIRS $ENV{HOMEBREW_CELLAR}/portaudio/19.6.0/include)
      endif()

      if(EXISTS "$ENV{HOMEBREW_PREFIX}/lib/libportaudio.dylib")
        set(PORTAUDIO_LIBRARIES $ENV{HOMEBREW_PREFIX}/lib/libportaudio.dylib)
      elseif(EXISTS "$ENV{HOMEBREW_CELLAR}/portaudio/19.7.0/lib/libportaudio.dylib")
        set(PORTAUDIO_LIBRARIES
          $ENV{HOMEBREW_CELLAR}/portaudio/19.7.0/lib/libportaudio.dylib)
      elseif(EXISTS "$ENV{HOMEBREW_CELLAR}/portaudio/19.6.0/lib/libportaudio.dylib")
        set(PORTAUDIO_LIBRARIES
          $ENV{HOMEBREW_CELLAR}/portaudio/19.6.0/lib/libportaudio.dylib)
      endif()
    endif()
    message(STATUS "Found portaudio (MacOs): ${PORTAUDIO_LIBRARIES}")
  endif()
endif()
# ==============================================================================
