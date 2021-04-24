# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# This program is free software; you can redistribute it
# and/or modify it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 2.1 of the License,
# or (at your option) any later version.
#
# Alternatively, you can redistribute it and/or modify it under the terms of the
# GNU General Public License as published by the Free Software Foundation;
# either version 2 of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
# details.
#
# You should have received a copy of the GNU Lesser General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/>.
# LICENCE_BLOCK_END
# ==============================================================================
if(PORTAUDIO_FOUND)
  # workaround for github CI with catalina
  if(EXISTS "/usr/local/Cellar/portaudio/19.7.0/include")
    set(PORTAUDIO_INCLUDE_DIRS /usr/local/Cellar/portaudio/19.7.0/include)
  elseif(EXISTS "/usr/local/Cellar/portaudio/19.6.0/include")
    set(PORTAUDIO_INCLUDE_DIRS /usr/local/Cellar/portaudio/19.6.0/include)
  endif()
  if(EXISTS "/usr/local/Cellar/portaudio/19.7.0/include")
    set(PORTAUDIO_LIBRARIES
        /usr/local/Cellar/portaudio/19.7.0/lib/libportaudio.dylib)
  elseif(EXISTS "/usr/local/Cellar/portaudio/19.6.0/include")
    set(PORTAUDIO_LIBRARIES
        /usr/local/Cellar/portaudio/19.6.0/lib/libportaudio.dylib)
  endif()
  message(STATUS "Found portaudio: ${PORTAUDIO_LIBRARIES}")
endif()
# ==============================================================================
