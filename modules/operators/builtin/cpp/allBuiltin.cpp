//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "allBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "All.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::allBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    bool bSuccess = false;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "all", bSuccess);
    }
    if (!bSuccess) {
        bool doOverAllElements = false;
        indexType d = 0;
        ArrayOf arg1 = argIn[0];
        if (argIn.size() > 1) {
            ArrayOf arg2 = argIn[1];
            if (arg2.isRowVectorCharacterArray() || (arg2.isStringArray() && arg2.isScalar())) {
                std::wstring paramAsString = arg2.getContentAsWideString();
                if (paramAsString != L"all") {
                    Error(_W("Wrong value for #2 argument."));
                } else {
                    doOverAllElements = true;
                }
            } else {
                d = arg2.getContentAsScalarIndex(false);
            }
        }
        bool needToOverload = false;
        ArrayOf res = All(arg1, d, doOverAllElements, needToOverload);
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "all");
        } else {
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
