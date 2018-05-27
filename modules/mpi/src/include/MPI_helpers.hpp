//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "nlsMpi_exports.h"
#include <mpi.h>
//=============================================================================
namespace Nelson {
NLSMPI_IMPEXP int
initializeMPI();
NLSMPI_IMPEXP void
packMPI(ArrayOf& A, void* buffer, int bufsize, int* packpos, MPI_Comm comm);
NLSMPI_IMPEXP ArrayOf
unpackMPI(void* buffer, int bufsize, int* packpos, MPI_Comm comm);
NLSMPI_IMPEXP int
getArrayOfFootPrint(ArrayOf& A, MPI_Comm comm);
NLSMPI_IMPEXP int
getCanonicalSize(int count, MPI_Datatype atype, MPI_Comm comm);
NLSMPI_IMPEXP std::string
getMpiLibraryVersion();
NLSMPI_IMPEXP MPI_Op
stringToMpiOp(std::wstring op_str);
NLSMPI_IMPEXP std::string
getMpiCommName(MPI_Comm comm);
} // namespace Nelson
//=============================================================================
