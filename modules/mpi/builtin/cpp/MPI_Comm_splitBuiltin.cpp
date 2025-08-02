//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MPI_Comm_splitBuiltin.hpp"
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
Nelson::MpiGateway::MPI_Comm_splitBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 1);
    int flagInit = 0;
    MPI_Initialized(&flagInit);
    if (!flagInit) {
        Error(_W("MPI must be initialized."));
    }
    MPI_Comm comm = HandleToMpiComm(argIn[0]);
    ArrayOf param2 = argIn[1];
    int color = param2.getContentAsInteger32Scalar();
    ArrayOf param3 = argIn[2];
    int key = param3.getContentAsInteger32Scalar();
    MPI_Comm newcomm = MPI_COMM_NULL;
    if (MPI_Comm_split(comm, color, key, &newcomm) != MPI_SUCCESS) {
        Error(_W("MPI_Comm_split fails."));
    }
    retval << MpiCommToHandle(newcomm);
    return retval;
}
//=============================================================================
