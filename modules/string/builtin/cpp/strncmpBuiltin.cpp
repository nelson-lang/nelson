//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "strncmpBuiltin.hpp"
#include "Error.hpp"
#include "StringCompare.hpp"
#include "OverloadFunction.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
strncmpBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool bCaseSensitive)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 3, 3);
    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    ArrayOf C = argIn[2];
    indexType len = C.getContentAsScalarIndex(false);
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        if (bCaseSensitive) {
            retval = OverloadFunction(eval, nLhs, argIn, "strncmp", bSuccess);
        } else {
            retval = OverloadFunction(eval, nLhs, argIn, "strncmpi", bSuccess);
        }
    }
    if (!bSuccess) {
        retval << StringCompare(A, B, bCaseSensitive, len);
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strncmpBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return ::strncmpBuiltin(eval, nLhs, argIn, true);
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strncmpiBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return ::strncmpBuiltin(eval, nLhs, argIn, false);
}
//=============================================================================
