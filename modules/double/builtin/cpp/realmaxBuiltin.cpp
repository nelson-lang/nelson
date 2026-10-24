//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cfloat>
#include <limits>
#include "realmaxBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DoubleGateway::realmaxBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() == 1) {
        ArrayOf param1 = argIn[0];
        std::wstring paramStr = param1.asWideString();
        if ((paramStr == L"double" || paramStr == L"single")) {
            if (paramStr == L"double") {
                double realmax = std::numeric_limits<double>::max();
                retval << ArrayOf::doubleScalar(realmax);
            } else {
                single realmax = std::numeric_limits<single>::max();
                retval << ArrayOf::singleScalar(realmax);
            }
        } else {
            Error(L"Nelson:double:invalidClassName", L"%s expected.", L"#1 'double' or 'single'");
        }
    } else {
        double realmax = std::numeric_limits<double>::max();
        retval << ArrayOf::doubleScalar(realmax);
    }
    return retval;
}
//=============================================================================
