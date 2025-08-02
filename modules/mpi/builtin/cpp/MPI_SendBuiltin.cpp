//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MPI_SendBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "MPI_CommHandleObject.hpp"
#include "MPI_helpers.hpp"
#include <mpi.h>
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MpiGateway::MPI_SendBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    int flagInit = 0;
    MPI_Initialized(&flagInit);
    if (!flagInit) {
        Error(_W("MPI must be initialized."));
    }
    ArrayOf A;
    int dest = 0;
    int tag = 0;
    MPI_Comm comm = MPI_COMM_WORLD;
    switch (argIn.size()) {
    case 3: {
        A = argIn[0];
        ArrayOf param2 = argIn[1];
        ArrayOf param3 = argIn[2];
        dest = param2.getContentAsInteger32Scalar();
        tag = param3.getContentAsInteger32Scalar();
    } break;
    case 4: {
        A = argIn[0];
        ArrayOf param2 = argIn[1];
        ArrayOf param3 = argIn[2];
        ArrayOf param4 = argIn[3];
        dest = param2.getContentAsInteger32Scalar();
        tag = param3.getContentAsInteger32Scalar();
        comm = HandleToMpiComm(param4);
    } break;
    default:
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        break;
    }
    int Asize = getArrayOfFootPrint(A, comm);
    int bufsize = Asize;
    void* cp = malloc(Asize);
    if (cp) {
        int packpos = 0;
        packMPI(A, cp, bufsize, &packpos, comm);
        MPI_Send(&packpos, 1, MPI_INT, dest, tag, comm);
        MPI_Send(cp, packpos, MPI_PACKED, dest, tag, comm);
        free(cp);
    }
    return retval;
}
//=============================================================================
