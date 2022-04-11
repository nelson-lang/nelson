//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isequaltoBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "IsEqual.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::isequaltoBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "isequalto", bSuccess);
    }
    if (!bSuccess) {
        bool res = false;
        for (size_t k = 1; k < argIn.size(); k++) {
            bool needToOverload = false;
            ArrayOf param1 = argIn[k - 1];
            ArrayOf param2 = argIn[k];
            res = IsEqual(param1, param2, true, true, needToOverload);
            if (needToOverload) {
                ArrayOfVector v1v2;
                v1v2.push_back(param1);
                v1v2.push_back(param2);
                ArrayOfVector ret = OverloadFunction(eval, nLhs, v1v2, "isequalto", bSuccess);
                {
                    if (ret.size() == 1) {
                        res = ret[0].getContentAsLogicalScalar(false) == 0 ? false : true;
                        if (!res) {
                            retval << ArrayOf::logicalConstructor(res);
                            return retval;
                        }
                    } else {
                        Error(_W("overload of isequalto must return a logical."));
                    }
                }
            } else {
                if (!res) {
                    retval << ArrayOf::logicalConstructor(res);
                    return retval;
                }
            }
        }
        retval << ArrayOf::logicalConstructor(res);
    }
    return retval;
}
//=============================================================================
