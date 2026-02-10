//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PythonEnvironment_setBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PythonEnvironment.hpp"
#include "characters_encoding.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::Python_engineGateway::PythonEnvironment_setBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];

    if (param1.getHandleCategory() != NLS_HANDLE_PYTHON_ENVIRONMENT_CATEGORY_STR) {
        raiseError(L"Nelson:python_engine:ERROR_PYTHON_ENVIRONMENT_OBJECT_EXPECTED",
            ERROR_PYTHON_ENVIRONMENT_OBJECT_EXPECTED);
    }

    raiseError(L"Nelson:python_engine:ERROR_UNABLE_TO_SET_PROPERTY_PYTHON_ENVIRONMENT",
        ERROR_UNABLE_TO_SET_PROPERTY_PYTHON_ENVIRONMENT);

    return {};
}
//=============================================================================
