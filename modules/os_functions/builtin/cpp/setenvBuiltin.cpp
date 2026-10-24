//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "setenvBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "SetVariableEnvironment.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OsFunctionsGateway::setenvBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);
    std::wstring varEnvName;
    std::wstring varEnvValue;
    if (argIn[0].isRowVectorCharacterArray()) {
        varEnvName = argIn[0].asWideString();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    bool success = false;
    if (argIn.size() == 2) {
        varEnvValue = argIn[1].asWideString();
        success = SetVariableEnvironmentW(varEnvName, varEnvValue);
    } else {
        success = RemoveVariableEnvironmentW(varEnvName);
    }
    if (!success) {
        Error(L"Nelson:setenv:cannotSet", L"Cannot set environment variable.");
    }
    return retval;
}
//=============================================================================
