//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isnumericBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TypeGateway::isnumericBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "isnumeric", bSuccess);
    }
    if (argIn[0].isClassStruct() || argIn[0].isHandle()) {
        bool bSuccess = false;
        retval = OverloadFunction(eval, nLhs, argIn, "isnumeric", bSuccess);
        if (bSuccess) {
            return retval;
        }
    }
    if (!bSuccess) {
        retval << ArrayOf::logicalConstructor(argIn[0].isNumeric());
    }
    return retval;
}
//=============================================================================
