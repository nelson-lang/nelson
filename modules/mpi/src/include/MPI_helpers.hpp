//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
stringToMpiOp(const std::wstring& op_str);
NLSMPI_IMPEXP std::string
getMpiCommName(MPI_Comm comm);
} // namespace Nelson
//=============================================================================
