//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "handle_ispropBuiltin.hpp"
#include "Error.hpp"
#include "HandleManager.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::handle_ispropBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    ArrayOf param1 = argIn[0];
    if (param1.isHandle()) {
        std::wstring propertyName = argIn[1].getContentAsWideString();
        Dimensions dimensions = param1.getDimensions();
        logical* result
            = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, dimensions.getElementCount());
        nelson_handle* handles = (nelson_handle*)param1.getDataPointer();
        for (indexType k = 0; k < dimensions.getElementCount(); k++) {
            bool isProperty = false;
            if (handles[k]) {
                HandleGenericObject* object = HandleManager::getInstance()->getPointer(handles[k]);
                isProperty = object && object->isProperty(propertyName);
            }
            result[k] = isProperty;
        }
        retval << ArrayOf(NLS_LOGICAL, dimensions, result);
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_FUNCTION_HANDLE_EXPECTED);
    }
    return retval;
}
//=============================================================================
