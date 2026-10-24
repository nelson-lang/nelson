//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "countBuiltin.hpp"
#include "StringCount.hpp"
#include "StringHelpers.hpp"
#include "StringPattern.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::countBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 4);
    if (argIn.size() == 3) {
        Error(L"Nelson:wrongNumberOfInputs", L"Wrong number of input arguments.");
    }
    bool bCaseSensitive = true;
    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    if (A.isCharacterArray() || A.isStringArray() || A.isCellArrayOfCharacterVectors()) {
        if (argIn.size() == 4) {
            ArrayOf param3 = argIn[2];
            std::wstring fieldname = StringHelpers::to_lower_copy(param3.asWideString());
            if (fieldname != L"ignorecase") {
                Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (3));
            }
            ArrayOf param4 = argIn[3];
            if (!param4.isScalar() || !(param4.isLogical() || param4.isNumeric())) {
                Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (4));
            }
            bCaseSensitive = param4.isLogical() ? (param4.asLogicalScalar() == 0)
                                                : (param4.asDoubleScalar() == 0);
        }
        if (IsPatternObject(B)) {
            bool needToOverload = false;
            retval << PatternCount(A, PatternExpression(B), !bCaseSensitive, needToOverload);
            if (needToOverload) {
                OverloadRequired("count");
            }
            return retval;
        }
        retval << StringCount(A, B, bCaseSensitive);
    } else {
        OverloadRequired("count");
    }
    return retval;
}
//=============================================================================
