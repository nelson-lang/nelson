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
#include "ErrorID.hpp"
#include "HandleManager.hpp"
#include "HandleState.hpp"
#include "i18n.hpp"
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
        std::wstring propertyName = argIn[1].asWideString();
        Dimensions dimensions = param1.dimensions();
        logical* result
            = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, dimensions.elementCount());
        HandleRef* handles = (HandleRef*)param1.dataPointer();
        for (indexType k = 0; k < dimensions.elementCount(); k++) {
            bool isProperty = false;
            if (handles[k]) {
                HandleGenericObject* object = handles[k]->object();
                isProperty = object && object->isProperty(propertyName);
            }
            result[k] = isProperty;
        }
        retval << ArrayOf(NLS_LOGICAL, dimensions, result);
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"function handle");
    }
    return retval;
}
//=============================================================================
