//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "strcmpBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "StringCompare.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
strcmpBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool bCaseSensitive)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 2);
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        if (bCaseSensitive) {
            retval = OverloadFunction(eval, nLhs, argIn, "strcmp", bSuccess);
        } else {
            retval = OverloadFunction(eval, nLhs, argIn, "strcmpi", bSuccess);
        }
    }
    if (!bSuccess) {
        ArrayOf A = argIn[0];
        ArrayOf B = argIn[1];
        retval << StringCompare(A, B, bCaseSensitive);
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strcmpBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return ::strcmpBuiltin(eval, nLhs, argIn, true);
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strcmpiBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return ::strcmpBuiltin(eval, nLhs, argIn, false);
}
//=============================================================================
