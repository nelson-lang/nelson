//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cfloat>
#include <limits>
#include "realminBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DoubleGateway::realminBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() == 1) {
        ArrayOf param1 = argIn[0];
        std::wstring paramStr = param1.getContentAsWideString();
        if ((paramStr == L"double" || paramStr == L"single")) {
            if (paramStr == L"double") {
                double realmin = std::numeric_limits<double>::min();
                retval << ArrayOf::doubleConstructor(realmin);
            } else {
                single realmin = std::numeric_limits<single>::min();
                retval << ArrayOf::singleConstructor(realmin);
            }
        } else {
            Error(_W("#1 'double' or 'single' expected."));
        }
    } else {
        double realmin = std::numeric_limits<double>::min();
        retval << ArrayOf::doubleConstructor(realmin);
    }
    return retval;
}
//=============================================================================
