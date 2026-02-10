//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MPI_InitBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "MPI_helpers.hpp"
#include <mpi.h>
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MpiGateway::MPI_InitBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    if (eval->haveEventsLoop()) {
        raiseError(L"Nelson:mpi:ERROR_WRONG_ENGINE_MODE", ERROR_WRONG_ENGINE_MODE);
    }
    retval << ArrayOf::logicalConstructor(initializeMPI());
    return retval;
}
//=============================================================================
