//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MPI_Comm_objectBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "MPI_CommHandleObject.hpp"
#include <mpi.h>
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MpiGateway::MPI_Comm_objectBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    int flagInit = 0;
    MPI_Initialized(&flagInit);
    if (!flagInit) {
        raiseError2(L"nelson:mpi:mpiMustBeInit");
    }
    MPI_Comm comm = MPI_COMM_WORLD;
    if (argIn.size() == 1) {
        std::wstring description = argIn[0].getContentAsWideString();
        if (description == L"MPI_COMM_SELF") {
            comm = MPI_COMM_SELF;
        } else if (description == L"MPI_COMM_WORLD") {
            comm = MPI_COMM_WORLD;
        } else if (description == L"MPI_COMM_NULL") {
            raiseError(
                L"Nelson:mpi:ERROR_MPI_COMM_NULL_NOT_ALLOWED", ERROR_MPI_COMM_NULL_NOT_ALLOWED);
        } else {
            raiseError(
                L"Nelson:mpi:ERROR_MPI_COMM_NOT_ALLOWED", ERROR_MPI_COMM_NOT_ALLOWED, description);
        }
    }
    retval << MpiCommToHandle(comm);
    return retval;
}
//=============================================================================
