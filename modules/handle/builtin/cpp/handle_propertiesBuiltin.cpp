//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "handle_propertiesBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "HandleGenericObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::handle_propertiesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    ArrayOf param1 = argIn[0];
    if (param1.isHandle()) {
        auto* obj = (HandleGenericObject*)param1.getContentAsHandleScalar();
        if (obj) {
            retval << ArrayOf::toCellArrayOfCharacterColumnVectors(obj->getProperties());
        } else {
            raiseError(L"Nelson:handle:ERROR_INVALID_NELSON_HANDLE", ERROR_INVALID_NELSON_HANDLE);
        }
    } else {
        raiseError2(L"nelson:validators:mustBeType", 1, NLS_HANDLE_STR);
    }
    return retval;
}
//=============================================================================
