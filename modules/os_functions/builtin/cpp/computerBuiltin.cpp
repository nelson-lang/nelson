//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "computerBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PlatformInfo.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OsFunctionsGateway::computerBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.empty()) {
        nargoutcheck(nLhs, 0, 3);
        if (nLhs >= 0) {
            retval << ArrayOf::characterArrayConstructor(GetArchitectureType());
        }
        if (nLhs > 1) {
            retval << ArrayOf::doubleConstructor(GetMaxArrayOfSizeSupported());
        }
        if (nLhs > 2) {
            if (IsBigEndian()) {
                retval << ArrayOf::characterArrayConstructor(L"B");
            } else {
                retval << ArrayOf::characterArrayConstructor(L"L");
            }
        }
    } else if (argIn.size() == 1) {
        nargoutcheck(nLhs, 0, 1);
        if (!argIn[0].isRowVectorCharacterArray()) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        std::wstring warg = argIn[0].getContentAsWideString();
        if (warg.compare(L"arch") != 0) {
            Error(_W("Unknown command option."));
        }
        retval << ArrayOf::characterArrayConstructor(GetArchitecture());
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
