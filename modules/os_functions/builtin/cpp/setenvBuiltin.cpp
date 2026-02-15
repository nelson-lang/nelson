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
#include "PredefinedErrorMessages.hpp"
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
        varEnvName = argIn[0].getContentAsWideString();
    } else {
        raiseError2(L"nelson:validators:mustBeType", 1, NLS_STRING_ARRAY_STR);
    }
    if (argIn.size() == 2) {
        varEnvValue = argIn[1].getContentAsWideString();
    }
    if (!SetVariableEnvironmentW(varEnvName, varEnvValue)) {
        raiseError(L"Nelson:os_functions:ERROR_CANNOT_SET_ENVIRONMENT_VARIABLE",
            ERROR_CANNOT_SET_ENVIRONMENT_VARIABLE);
    }
    return retval;
}
//=============================================================================
