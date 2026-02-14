//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "JuliaEnvironment_getBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "HandleGenericObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "JuliaEnvironment.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::Julia_engineGateway::JuliaEnvironment_getBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring propertyName = param2.getContentAsWideString();
    ArrayOfVector retval(1);
    if (param1.getHandleCategory() != NLS_HANDLE_JULIA_ENVIRONMENT_CATEGORY_STR) {
        raiseError(L"Nelson:julia_engine:ERROR_JULIAENVIRONMENT_OBJECT_EXPECTED",
            ERROR_JULIAENVIRONMENT_OBJECT_EXPECTED);
    }

    ArrayOf res;
    JuliaEnvironment* juliaEnvironment = JuliaEnvironment::getInstance();

    if (!juliaEnvironment->get(propertyName, res)) {
        raiseError(
            L"Nelson:error_manager:ERROR_UNDEFINED_METHOD", ERROR_UNDEFINED_METHOD, propertyName);
    }
    retval << res;
    return retval;
}
//=============================================================================
