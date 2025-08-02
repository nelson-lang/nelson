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
#include "MPI_CommHandleObject.hpp"
#include <mpi.h>
#include "InputOutputArgumentsCheckers.hpp"
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
        Error(_W("MPI must be initialized."));
    }
    MPI_Comm comm = MPI_COMM_WORLD;
    if (argIn.size() == 1) {
        std::wstring description = argIn[0].getContentAsWideString();
        if (description == L"MPI_COMM_SELF") {
            comm = MPI_COMM_SELF;
        } else if (description == L"MPI_COMM_WORLD") {
            comm = MPI_COMM_WORLD;
        } else if (description == L"MPI_COMM_NULL") {
            Error(_W("MPI_COMM_NULL not allowed."));
        } else {
            Error(description + _W(" not allowed."));
        }
    }
    retval << MpiCommToHandle(comm);
    return retval;
}
//=============================================================================
