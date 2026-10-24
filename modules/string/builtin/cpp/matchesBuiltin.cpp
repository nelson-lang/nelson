//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "matchesBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "StringTextMatch.hpp"
#include "StringPattern.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::matchesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 4);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() == 3) {
        Error(L"Nelson:wrongNumberOfInputs", L"Wrong number of input arguments.");
    }

    bool ignoreCase = false;
    if (argIn.size() == 4) {
        ArrayOf param3 = argIn[2];
        std::wstring fieldname = param3.asWideString();
        if (fieldname != L"IgnoreCase") {
            Error(L"Nelson:string:wrongValue3IgnoreCaseExpected",
                L"Wrong value for #%d: 'IgnoreCase' expected.", 3);
        }
        ArrayOf param4 = argIn[3];
        ignoreCase = (param4.asLogicalScalar() != 0u);
    }

    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    if (IsPatternObject(B)) {
        bool needToOverload = false;
        retval << PatternLogical(
            A, PatternExpression(B), ignoreCase, PatternLogicalMode::Matches, needToOverload);
        if (needToOverload) {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string or cell");
        }
        return retval;
    }
    retval << StringMatches(A, B, ignoreCase);
    return retval;
}
//=============================================================================
