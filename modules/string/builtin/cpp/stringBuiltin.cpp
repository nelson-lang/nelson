//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "stringBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::stringBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 0, 1);
    if (argIn.empty()) {
        retval << ArrayOf::stringArrayConstructor(std::string());
    } else {
        bool bSuccess = false;
        if (eval->mustOverloadBasicTypes()) {
            retval = OverloadFunction(eval, nLhs, argIn, "string", bSuccess);
        }
        if (!bSuccess) {
            bool needToOverload = false;
            ArrayOf res = ArrayOf::toStringArray(argIn[0], needToOverload);
            if (needToOverload) {
                retval = OverloadFunction(eval, nLhs, argIn, "string");
            } else {
                retval << res;
            }
        }
    }
    return retval;
}
//=============================================================================
