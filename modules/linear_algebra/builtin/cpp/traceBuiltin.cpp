//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "traceBuiltin.hpp"
#include "Error.hpp"
#include "OverloadRequired.hpp"
#include "TraceMatrix.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::traceBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].isReferenceType()) {
        OverloadRequired("trace");
    }
    ArrayOf A(argIn[0]);
    if (A.isLogical() || A.isCharacterArray() || A.isIntegerType()) {
        // logical, char and integer inputs give a double result
        A.promoteType(NLS_DOUBLE);
    }
    retval << TraceMatrix(A);
    return retval;
}
//=============================================================================
