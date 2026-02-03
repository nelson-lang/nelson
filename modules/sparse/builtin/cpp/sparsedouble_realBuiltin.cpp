//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "sparsedouble_realBuiltin.hpp"
#include "Error.hpp"
#include "SparseRealPart.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::sparsedouble_realBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[0].isSparseDoubleType()) {
        raiseError(ERROR_WRONG_ARGUMENT_X_TYPE_Y_EXPECTED, 1, NLS_SPARSE_DOUBLE_STR);
    }
    retval << SparseRealPart(argIn[0]);
    return retval;
}
//=============================================================================
