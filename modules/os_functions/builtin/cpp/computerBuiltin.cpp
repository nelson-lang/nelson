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
    nargincheck(argIn, 0, 1);
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
        return retval;
    }
    if (argIn.size() == 1) {
        nargoutcheck(nLhs, 0, 1);
        if (!argIn[0].isRowVectorCharacterArray()) {
            raiseError2(L"nelson:validators:mustBeType", 1, NLS_STRING_ARRAY_STR);
        }
        std::wstring warg = argIn[0].getContentAsWideString();
        if (warg.compare(L"arch") != 0) {
            raiseError(
                L"Nelson:os_functions:ERROR_UNKNOWN_COMMAND_OPTION", ERROR_UNKNOWN_COMMAND_OPTION);
        }
        retval << ArrayOf::characterArrayConstructor(GetArchitecture());
    }
    return retval;
}
//=============================================================================
