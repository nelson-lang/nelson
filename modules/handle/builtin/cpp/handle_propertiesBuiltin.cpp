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
#include "ClassdefParser.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "HandleGenericObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
#define HANDLE_PROPERTIES_ERROR_INVALID_HANDLE "Invalid handle."
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::handle_propertiesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    ArrayOf param1 = argIn[0];
    if (param1.isHandle()) {
        if (!param1.isScalar()) {
            std::string className = param1.handleClassName();
            if (!className.empty() && classdefLoadClass(className)) {
                retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(
                    classdefProperties(className));
                return retval;
            }
        }
        auto* obj = (HandleGenericObject*)param1.asHandleScalar();
        if (obj) {
            retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(obj->properties());
        } else {
            RaiseError(_W(HANDLE_PROPERTIES_ERROR_INVALID_HANDLE), L"Nelson:handle:invalidHandle");
        }
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"function handle");
    }
    return retval;
}
//=============================================================================
