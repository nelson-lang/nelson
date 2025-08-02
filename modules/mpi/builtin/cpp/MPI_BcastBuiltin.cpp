//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MPI_BcastBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "MPI_CommHandleObject.hpp"
#include "MPI_helpers.hpp"
#include <mpi.h>
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MpiGateway::MPI_BcastBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 2);
    int flagInit = 0;
    MPI_Initialized(&flagInit);
    if (!flagInit) {
        Error(_W("MPI must be initialized."));
    }
    ArrayOf A = argIn[0];
    ArrayOf tmp = argIn[1];
    int rootID = tmp.getContentAsInteger32Scalar();
    MPI_Comm comm = MPI_COMM_WORLD;
    if (argIn.size() > 2) {
        comm = HandleToMpiComm(argIn[2]);
    }
    int thisrank = 0;
    MPI_Comm_rank(comm, &thisrank);
    if (thisrank == rootID) {
        int Asize = getArrayOfFootPrint(A, comm);
        int bufsize = Asize;
        void* cp = malloc(Asize);
        if (cp) {
            int packpos = 0;
            packMPI(A, cp, bufsize, &packpos, comm);
            MPI_Bcast(&packpos, 1, MPI_INT, rootID, comm);
            MPI_Bcast(cp, packpos, MPI_PACKED, rootID, comm);
            free(cp);
            retval << A;
        } else {
            Error(ERROR_MEMORY_ALLOCATION);
        }
    } else {
        int msgsize = 0;
        MPI_Bcast(&msgsize, 1, MPI_INT, rootID, comm);
        void* cp = malloc(msgsize);
        if (cp) {
            MPI_Bcast(cp, msgsize, MPI_PACKED, rootID, comm);
            int packpos = 0;
            retval << unpackMPI(cp, msgsize, &packpos, comm);
            free(cp);
        } else {
            Error(ERROR_MEMORY_ALLOCATION);
        }
    }
    return retval;
}
//=============================================================================
