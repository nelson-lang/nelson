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
if(DEFINED ENV{QTDIR})
  set(QTDIR $ENV{QTDIR})
else()
  if(NOT QTDIR)
    if(EXISTS $ENV{HOME}/Qt/5.15/clang_64)
      set(QTDIR $ENV{HOME}/Qt/5.15/clang_64)
    endif()
  endif()
endif()
set(CMAKE_PREFIX_PATH ${QTDIR})
if(DEFINED ENV{QTDIR_BINARIES})

else()
  if(EXISTS ${QTDIR}/bin)
    set(ENV{QTDIR_BINARIES} ${QTDIR}/bin)
    set(QTDIR_BINARIES ${QTDIR}/bin)
  else()
    message(WARNING "Please define QTDIR_BINARIES environment variable.")
  endif()
endif()
# ==============================================================================
