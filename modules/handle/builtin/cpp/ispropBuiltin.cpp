//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ispropBuiltin.hpp"
#include "ClassdefParser.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "HandleState.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOf
logicalArrayWithValue(const Dimensions& dimensions, bool value)
{
    logical* result = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, dimensions.elementCount());
    for (indexType k = 0; k < dimensions.elementCount(); k++) {
        result[k] = value;
    }
    return ArrayOf(NLS_LOGICAL, dimensions, result);
}
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::ispropBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    const bool propertyNameIsText
        = argIn[1].isRowVectorCharacterArray() || (argIn[1].isStringArray() && argIn[1].isScalar());
    std::string className;
    if (argIn[0].isClassType()) {
        className = argIn[0].classType();
    } else if (argIn[0].isHandle()) {
        className = argIn[0].handleClassName();
    } else if (argIn[0].isRowVectorCharacterArray()) {
        className = argIn[0].asUtf8String();
    }
    if (argIn[0].isHandle() && propertyNameIsText) {
        std::wstring propertyName = argIn[1].asWideString();
        Dimensions dimensions = argIn[0].dimensions();
        logical* result
            = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, dimensions.elementCount());
        HandleRef* handles = (HandleRef*)argIn[0].dataPointer();
        for (indexType k = 0; k < dimensions.elementCount(); k++) {
            bool hasProperty = false;
            if (handles[k]) {
                HandleGenericObject* object = handles[k]->object();
                hasProperty = object != nullptr && object->isProperty(propertyName);
            }
            result[k] = hasProperty;
        }
        retval << ArrayOf(NLS_LOGICAL, dimensions, result);
        return retval;
    }
    if (!className.empty() && propertyNameIsText && classdefLoadClass(className)) {
        bool hasProperty = classdefHasProperty(className, argIn[1].asUtf8String());
        if (argIn[0].isRowVectorCharacterArray()) {
            retval << ArrayOf::logicalScalar(hasProperty);
        } else {
            retval << logicalArrayWithValue(argIn[0].dimensions(), hasProperty);
        }
        return retval;
    }
    if (argIn[0].isGraphicsObject()) {
        OverloadRequired("isprop");
    }
    // Any other value gives false, not an error. An empty input gives a scalar false
    // so isprop(obj, "Sub") && isprop(obj.Sub, "Prop") works when Sub is [].
    if (argIn[0].isEmpty()) {
        retval << ArrayOf::logicalScalar(false);
    } else {
        retval << logicalArrayWithValue(argIn[0].dimensions(), false);
    }
    return retval;
}
//=============================================================================
