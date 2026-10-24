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
#include "endsWithBuiltin.hpp"
#include "StringTextMatch.hpp"
#include "StringHelpers.hpp"
#include "StringPattern.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::endsWithBuiltin(int nLhs, const ArrayOfVector& argIn)
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
            retval << PatternLogical(A, PatternExpression(B), !bCaseSensitive,
                PatternLogicalMode::EndsWith, needToOverload);
            if (needToOverload) {
                Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                    L"string or cell");
            }
            return retval;
        }
        retval << StringEndsWith(A, B, bCaseSensitive);
    } else {
        Error(L"Nelson:string:charVectorCellStringsExpected",
            L"character vector or cell of strings expected.");
    }
    return retval;
}
//=============================================================================
