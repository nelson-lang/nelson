//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gammaincBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "GammaIncomplete.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::gammaincBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    bool needToOverload = false;
    bool isLower = true;
    if (argIn.size() == 3) {
        std::wstring tail = argIn[2].asWideString();
        if (tail == L"upper") {
            isLower = false;
        } else if (tail == L"lower") {
            isLower = true;
        } else {
            Error("Nelson:special_functions:invalidTail", "%s expected.",
                "Wrong value of the third argument 'upper' or 'lower'");
        }
    }
    retval << GammaIncomplete(argIn[0], argIn[1], isLower, needToOverload);
    if (needToOverload) {
        OverloadRequired("gammainc");
    }
    return retval;
}
//=============================================================================
