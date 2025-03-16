# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
if(PORTAUDIO_FOUND)
  if(DEFINED ENV{CONDA_PREFIX})
    set(PORTAUDIO_LIBRARIES $ENV{CONDA_PREFIX}/lib/libportaudio${CMAKE_SHARED_LIBRARY_SUFFIX})
    set(PORTAUDIO_INCLUDE_DIRS $ENV{CONDA_PREFIX}/include)
  elseif(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    if(EXISTS "$ENV{HOMEBREW_PREFIX}/include/portaudio.h")
      set(PORTAUDIO_INCLUDE_DIRS $ENV{HOMEBREW_PREFIX}/include)
      set(PORTAUDIO_LIBRARIES $ENV{HOMEBREW_PREFIX}/lib/libportaudio${CMAKE_SHARED_LIBRARY_SUFFIX})
    else()
      if(EXISTS "$ENV{HOMEBREW_CELLAR}/portaudio/19.7.0/include")
        set(PORTAUDIO_INCLUDE_DIRS $ENV{HOMEBREW_CELLAR}/portaudio/19.7.0/include)
      elseif(EXISTS "$ENV{HOMEBREW_CELLAR}/portaudio/19.6.0/include")
        set(PORTAUDIO_INCLUDE_DIRS $ENV{HOMEBREW_CELLAR}/portaudio/19.6.0/include)
      endif()
      if(EXISTS "$ENV{HOMEBREW_PREFIX}/lib/libportaudio${CMAKE_SHARED_LIBRARY_SUFFIX}")
        set(PORTAUDIO_LIBRARIES $ENV{HOMEBREW_PREFIX}/lib/libportaudio${CMAKE_SHARED_LIBRARY_SUFFIX})
      elseif(EXISTS "$ENV{HOMEBREW_CELLAR}/portaudio/19.7.0/lib/libportaudio${CMAKE_SHARED_LIBRARY_SUFFIX}")
        set(PORTAUDIO_LIBRARIES
          $ENV{HOMEBREW_CELLAR}/portaudio/19.7.0/lib/libportaudio${CMAKE_SHARED_LIBRARY_SUFFIX})
      elseif(EXISTS "$ENV{HOMEBREW_CELLAR}/portaudio/19.6.0/lib/libportaudio${CMAKE_SHARED_LIBRARY_SUFFIX}")
        set(PORTAUDIO_LIBRARIES
          $ENV{HOMEBREW_CELLAR}/portaudio/19.6.0/lib/libportaudio${CMAKE_SHARED_LIBRARY_SUFFIX})
      endif()
    endif()
  endif()
  
  if(NOT PORTAUDIO_INCLUDE_DIRS OR NOT EXISTS "${PORTAUDIO_INCLUDE_DIRS}/portaudio.h")
    message(WARNING "PortAudio include directory not found or invalid: ${PORTAUDIO_INCLUDE_DIRS}")
    set(PORTAUDIO_FOUND FALSE)
  elseif(NOT PORTAUDIO_LIBRARIES OR NOT EXISTS "${PORTAUDIO_LIBRARIES}")
    message(WARNING "PortAudio library not found or invalid: ${PORTAUDIO_LIBRARIES}")
    set(PORTAUDIO_FOUND FALSE)
  else()
    message(STATUS "Found PortAudio (${CMAKE_SYSTEM_NAME}): 
      Include: ${PORTAUDIO_INCLUDE_DIRS}
      Library: ${PORTAUDIO_LIBRARIES}")
  endif()
endif()
# ==============================================================================
