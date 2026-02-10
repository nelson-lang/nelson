//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "onCleanup_getBuiltin.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OnCleanupObjectHandle.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::InterpreterGateway::onCleanup_getBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);

    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring propertyName = param2.getContentAsWideString();
    ArrayOfVector retval;
    if (param1.getHandleCategory() != NLS_HANDLE_ONCLEANUP_CATEGORY_STR) {
        raiseError(
            L"Nelson:interpreter:ERROR_ONCLEANUP_HANDLE_EXPECTED", ERROR_ONCLEANUP_HANDLE_EXPECTED);
    }
    auto* obj = (OnCleanupObjectHandle*)param1.getContentAsHandleScalar();
    ArrayOf res;
    if (propertyName == L"cancel") {
        obj->cancel();
        return retval;
    }
    if (propertyName == L"task") {
        res = obj->getTask();
        retval << res;
        return retval;
    }
    raiseError(L"Nelson:interpreter:ERROR_UNKNOWN_PROPERTY_NAME", ERROR_UNKNOWN_PROPERTY_NAME,
        propertyName);
    return retval;
}
//=============================================================================
