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
find_package(MPI REQUIRED)
if(MPI_INCLUDE_PATH AND MPI_LIBRARIES)
  message(STATUS "MPI_COMPILE_FLAGS=${MPI_COMPILE_FLAGS}")
  message(STATUS "MPI_LINK_FLAGS=${MPI_LINK_FLAGS}")
  message(STATUS "MPI_INCLUDE_PATH=${MPI_INCLUDE_PATH}")
  message(STATUS "MPI_LIBRARIES=${MPI_LIBRARIES}")
endif(MPI_INCLUDE_PATH AND MPI_LIBRARIES)
check_symbol_exists("MPI_Get_library_version" "mpi.h"
                    HAVE_MPI_GET_LIBRARY_VERSION)
# ==============================================================================
