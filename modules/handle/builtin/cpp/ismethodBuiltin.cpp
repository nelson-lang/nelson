//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ismethodBuiltin.hpp"
#include "ClassdefParser.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::ismethodBuiltin(int nLhs, const ArrayOfVector& argIn)
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
        std::string methodName = argIn[1].getContentAsCString();
        bool res = ClassdefDefinitionManager::getInstance()->hasMethod(className, methodName)
            || ClassdefDefinitionManager::getInstance()->hasStaticMethod(className, methodName);
        retval << ArrayOf::logicalConstructor(res);
        return retval;
    }
    OverloadRequired("ismethod");
    return retval;
}
//=============================================================================
