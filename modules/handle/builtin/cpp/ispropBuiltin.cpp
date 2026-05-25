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
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOf
logicalArrayWithValue(const Dimensions& dimensions, bool value)
{
    logical* result = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, dimensions.getElementCount());
    for (indexType k = 0; k < dimensions.getElementCount(); k++) {
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
    std::string className;
    if (argIn[0].isClassType()) {
        className = argIn[0].getClassType();
    } else if (argIn[0].isHandle()) {
        className = argIn[0].getHandleClassName();
    } else if (argIn[0].isRowVectorCharacterArray()) {
        className = argIn[0].getContentAsCString();
    }
    if (!className.empty() && argIn[1].isRowVectorCharacterArray()
        && ClassdefDefinitionManager::getInstance()->loadClass(className)) {
        bool hasProperty = ClassdefDefinitionManager::getInstance()->hasProperty(
            className, argIn[1].getContentAsCString());
        if (argIn[0].isRowVectorCharacterArray()) {
            retval << ArrayOf::logicalConstructor(hasProperty);
        } else {
            retval << logicalArrayWithValue(argIn[0].getDimensions(), hasProperty);
        }
        return retval;
    }
    OverloadRequired("isprop");
    return retval;
}
//=============================================================================
