//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isvectorBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::isvectorBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    if (argIn[0].isClassStruct() || argIn[0].isHandle()) {
        bool bSuccess = false;
        retval = OverloadFunction(eval, nLhs, argIn, "isvector", bSuccess);
        if (bSuccess) {
            return retval;
        }
    }
    retval << ArrayOf::logicalConstructor(argIn[0].isVector());
    return retval;
}
//=============================================================================
