//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "allBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadRequired.hpp"
#include "All.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::allBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    const std::string functionName = "all";
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
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
        OverloadRequired(functionName);
    }
    return res;
}
//=============================================================================
