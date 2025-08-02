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
#include "i18n.hpp"
#include "SetVariableEnvironment.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
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
        varEnvName = argIn[0].getContentAsWideString();
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    if (argIn.size() == 2) {
        varEnvValue = argIn[1].getContentAsWideString();
    }
    if (!SetVariableEnvironmentW(varEnvName, varEnvValue)) {
        Error(_W("Cannot set environment variable."));
    }
    return retval;
}
//=============================================================================
