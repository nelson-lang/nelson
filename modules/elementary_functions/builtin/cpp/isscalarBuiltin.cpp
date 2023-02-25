//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isscalarBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::isscalarBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    if (param1.isClassStruct() || param1.isHandle()) {
        bool bSuccess = false;
        retval = OverloadFunction(eval, nLhs, argIn, "isscalar", bSuccess);
        if (bSuccess) {
            return retval;
        }
    }
    retval << ArrayOf::logicalConstructor(param1.isScalar());
    return retval;
}
//=============================================================================
