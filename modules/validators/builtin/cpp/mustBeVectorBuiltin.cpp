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
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
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
            argPos = param2.getContentAsInteger32Scalar();
            if (argPos < 1) {
                raiseError(L"Nelson:validators:ERROR_LAST_ARGUMENT_MUST_BE_POSITIVE_INTEGER",
                    ERROR_LAST_ARGUMENT_MUST_BE_POSITIVE_INTEGER);
            }
        } else if (argIn[1].isRowVectorCharacterArray()
            || (argIn[1].isStringArray() && argIn[1].isScalar())) {
            std::wstring flag = argIn[1].getContentAsWideString();
            if (flag != L"allows-all-empties") {
                raiseError(L"Nelson:validators:ERROR_LAST_ARGUMENT_MUST_BE_POSITIVE_INTEGER_OR_"
                           L"ALLOWS_ALL_EMPTIES",
                    ERROR_LAST_ARGUMENT_MUST_BE_POSITIVE_INTEGER_OR_ALLOWS_ALL_EMPTIES);
            }
            allowsAllEmpties = true;
        } else {
            raiseError(L"Nelson:validators:ERROR_LAST_ARGUMENT_MUST_BE_POSITIVE_INTEGER_OR_ALLOWS_"
                       L"ALL_EMPTIES",
                ERROR_LAST_ARGUMENT_MUST_BE_POSITIVE_INTEGER_OR_ALLOWS_ALL_EMPTIES);
        }
    } break;
    case 3: {
        std::wstring flag = argIn[1].getContentAsWideString();
        if (flag != L"allows-all-empties") {
            raiseError(L"Nelson:validators:ERROR_SECOND_ARGUMENT_MUST_BE_ALLOWS_ALL_EMPTIES",
                ERROR_SECOND_ARGUMENT_MUST_BE_ALLOWS_ALL_EMPTIES);
        }
        ArrayOf param2 = argIn[1];
        argPos = param2.getContentAsInteger32Scalar();
        if (argPos < 1) {
            raiseError(L"Nelson:validators:ERROR_LAST_ARGUMENT_MUST_BE_POSITIVE_INTEGER",
                ERROR_LAST_ARGUMENT_MUST_BE_POSITIVE_INTEGER);
        }
    } break;
    }
    mustBeVector(argIn[0], allowsAllEmpties, argPos, true);
    return retval;
}
//=============================================================================
