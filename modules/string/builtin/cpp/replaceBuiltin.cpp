//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "replaceBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "StringReplace.hpp"
#include "OverloadFunction.hpp"
#include "IsCellOfStrings.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::replaceBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 3, 3);
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "replace", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload;
        ArrayOf res = Replace(argIn[0], argIn[1], argIn[2], needToOverload);
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "replace", bSuccess);
            if (!bSuccess) {
                ;
                Error(_W("Invalid input argument(s): cell or string expected."));
            }
        } else {
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
