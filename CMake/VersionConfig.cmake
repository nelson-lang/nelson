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
if(DEFINED ENV{NELSON_VERSION_MAJOR})
  set(Nelson_VERSION_MAJOR $ENV{NELSON_VERSION_MAJOR})
else()
  set(Nelson_VERSION_MAJOR ${Nelson_VERSION_MAJOR_DEFAULT})
endif()
# ==============================================================================
if(DEFINED ENV{Nelson_VERSION_MINOR})
  set(Nelson_VERSION_MINOR $ENV{Nelson_VERSION_MINOR})
else()
  set(Nelson_VERSION_MINOR ${Nelson_VERSION_MINOR_DEFAULT})
endif()
# ==============================================================================
if(DEFINED ENV{NELSON_VERSION_MAINTENANCE})
  set(Nelson_VERSION_MAINTENANCE $ENV{NELSON_VERSION_MAINTENANCE})
else()
  set(Nelson_VERSION_MAINTENANCE ${Nelson_VERSION_MAINTENANCE_DEFAULT})
endif()
# ==============================================================================
if(DEFINED ENV{NELSON_VERSION_BUILD})
  set(Nelson_VERSION_BUILD $ENV{NELSON_VERSION_BUILD})
else()
  set(Nelson_VERSION_BUILD ${Nelson_VERSION_BUILD_DEFAULT})
endif()
# ==============================================================================
if(EXISTS ${CMAKE_SOURCE_DIR}/modules/core/src/include/Nelson_VERSION.h)
else()
  configure_file(
    "${CMAKE_SOURCE_DIR}/modules/core/src/include/Nelson_VERSION.h.in"
    "${CMAKE_SOURCE_DIR}/modules/core/src/include/Nelson_VERSION.h")
endif()
# ==============================================================================
