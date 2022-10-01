# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
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
add_definitions(-DOMPI_SKIP_MPICXX)
# ==============================================================================
