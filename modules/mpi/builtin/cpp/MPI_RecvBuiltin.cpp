//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MPI_RecvBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleManager.hpp"
#include "MPI_CommHandleObject.hpp"
#include "MPI_helpers.hpp"
#include <mpi.h>
#include "InputOutputArgumentsCheckers.hpp"

//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MpiGateway::MPI_RecvBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 2);
    int flagInit = 0;
    MPI_Initialized(&flagInit);
    if (!flagInit) {
        Error(_W("MPI must be initialized."));
    }
    ArrayOf tmp = argIn[0];
    int source = tmp.getContentAsInteger32Scalar();
    tmp = argIn[1];
    int tag = tmp.getContentAsInteger32Scalar();
    MPI_Comm comm = MPI_COMM_WORLD;
    if (argIn.size() > 2) {
        ArrayOf param3 = argIn[2];
        comm = HandleToMpiComm(param3);
    }
    int msgsize = 0;
    MPI_Status status;
    MPI_Recv(&msgsize, 1, MPI_INT, source, tag, comm, &status);
    void* cp = malloc(msgsize);
    MPI_Recv(cp, msgsize, MPI_PACKED, status.MPI_SOURCE, status.MPI_TAG, comm, MPI_STATUS_IGNORE);
    int packpos = 0;
    ArrayOf A(unpackMPI(cp, msgsize, &packpos, comm));
    free(cp);
    retval << A;
    if (nLhs > 1) {
        retval << ArrayOf::int32Constructor(status.MPI_SOURCE);
    }
    if (nLhs > 2) {
        retval << ArrayOf::int32Constructor(status.MPI_TAG);
    }
    return retval;
}
//=============================================================================
