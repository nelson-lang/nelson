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
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
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
            std::string className = param1.getHandleClassName();
            if (!className.empty()
                && ClassdefDefinitionManager::getInstance()->loadClass(className)) {
                retval << ArrayOf::toCellArrayOfCharacterColumnVectors(
                    ClassdefDefinitionManager::getInstance()->properties(className));
                return retval;
            }
        }
        auto* obj = (HandleGenericObject*)param1.getContentAsHandleScalar();
        if (obj) {
            retval << ArrayOf::toCellArrayOfCharacterColumnVectors(obj->getProperties());
        } else {
            Error(_W(HANDLE_PROPERTIES_ERROR_INVALID_HANDLE));
        }
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_FUNCTION_HANDLE_EXPECTED);
    }
    return retval;
}
//=============================================================================
