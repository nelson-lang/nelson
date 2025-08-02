//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MPI_Comm_deleteBuiltin.hpp"
#include "Error.hpp"
#include "MPI_CommHandleObject.hpp"
#include "MPI_helpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MpiGateway::MPI_Comm_deleteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf param1 = argIn[0];
    if (param1.isHandle()) {
        MPICommHandleDelete(param1);
    }
    ArrayOfVector retval;
    return retval;
}
//=============================================================================
