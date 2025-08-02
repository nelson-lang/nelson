# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
find_package(MPI QUIET COMPONENTS C CXX)

  if(NOT MPI_FOUND)
    execute_process(
      COMMAND bash -c "grep -i fedora /etc/*release"
      RESULT_VARIABLE FEDORA_DETECTED
      OUTPUT_VARIABLE FEDORA_INFO
      ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE
    )

    if(FEDORA_DETECTED EQUAL 0)
      message(STATUS "Fedora detected. Automatic default setup MPI...")
      set(ENV{PATH} "/usr/lib64/openmpi/bin:$ENV{PATH}")
      set(ENV{LD_LIBRARY_PATH} "/usr/lib64/openmpi/lib:$ENV{LD_LIBRARY_PATH}")
      set(MPI_C_COMPILER "mpicc")
      set(MPI_CXX_COMPILER "mpicxx")

      find_package(MPI QUIET REQUIRED COMPONENTS C CXX)
    else()
      message(ERROR "MPI not detected...")
    endif()
  
  endif()

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
