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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "PlatformInfo.hpp"
#include "InputOutputArgumentsCheckers.hpp"
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
            retval << ArrayOf::characterArray(GetArchitectureType());
        }
        if (nLhs > 1) {
            retval << ArrayOf::doubleScalar(GetMaxArrayOfSizeSupported());
        }
        if (nLhs > 2) {
            if (IsBigEndian()) {
                retval << ArrayOf::characterArray(L"B");
            } else {
                retval << ArrayOf::characterArray(L"L");
            }
        }
    } else if (argIn.size() == 1) {
        nargoutcheck(nLhs, 0, 1);
        if (!argIn[0].isRowVectorCharacterArray()) {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string");
        }
        std::wstring warg = argIn[0].asWideString();
        if (warg.compare(L"arch") != 0) {
            Error(L"Nelson:computer:unknownOption", L"Unknown command option.");
        }
        retval << ArrayOf::characterArray(GetArchitecture());
    } else {
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
    }
    return retval;
}
//=============================================================================
