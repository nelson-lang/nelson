//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "mustBeVectorBuiltin.hpp"
#include "ValidatorsInternal.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ValidatorsGateway::mustBeVectorBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 3);
    bool allowsAllEmpties = false;
    int argPos = -1;

    switch (argIn.size()) {
    case 2: {
        if (argIn[1].isNumeric() && argIn[1].isScalar()) {
            ArrayOf param2 = argIn[1];
            argPos = param2.asInt32Scalar();
            if (argPos < 1) {
                Error(L"Nelson:validators:invalidPosition",
                    L"The last argument must be a positive integer.");
            }
        } else if (argIn[1].isRowVectorCharacterArray()
            || (argIn[1].isStringArray() && argIn[1].isScalar())) {
            std::wstring flag = argIn[1].asWideString();
            if (flag != L"allows-all-empties") {
                Error(L"Nelson:validators:invalidPositionOrAllowsAllEmpties",
                    L"The last argument must be a positive integer or 'allows-all-empties'.");
            }
            allowsAllEmpties = true;
        } else {
            Error(L"Nelson:validators:invalidPositionOrAllowsAllEmpties",
                L"The last argument must be a positive integer or 'allows-all-empties'.");
        }
    } break;
    case 3: {
        std::wstring flag = argIn[1].asWideString();
        if (flag != L"allows-all-empties") {
            Error(L"Nelson:validators:invalidArgument",
                L"Second argument must be a 'allows-all-empties'.");
        }
        ArrayOf param2 = argIn[1];
        argPos = param2.asInt32Scalar();
        if (argPos < 1) {
            Error(L"Nelson:validators:invalidPosition",
                L"The last argument must be a positive integer.");
        }
    } break;
    }
    mustBeVector(argIn[0], allowsAllEmpties, argPos, true);
    return retval;
}
//=============================================================================
