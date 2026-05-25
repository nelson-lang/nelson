//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "eventsBuiltin.hpp"
#include "ClassdefParser.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
#define EVENTS_FUNCTION_NAME "events"
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::eventsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    std::string className;
    if (argIn[0].isClassType()) {
        className = argIn[0].getClassType();
    } else if (argIn[0].isHandle()) {
        className = argIn[0].getHandleClassName();
    } else if (argIn[0].isRowVectorCharacterArray()) {
        className = argIn[0].getContentAsCString();
    }
    if (!className.empty() && ClassdefDefinitionManager::getInstance()->loadClass(className)) {
        retval << ArrayOf::toCellArrayOfCharacterColumnVectors(
            ClassdefDefinitionManager::getInstance()->events(className));
        return retval;
    }
    OverloadRequired(EVENTS_FUNCTION_NAME);
    return retval;
}
//=============================================================================
