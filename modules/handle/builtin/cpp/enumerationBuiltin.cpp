//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "enumerationBuiltin.hpp"
#include "ClassdefParser.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::enumerationBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);

    std::string className;
    if (argIn[0].isClassType()) {
        className = argIn[0].getClassType();
    } else if (argIn[0].isRowVectorCharacterArray()) {
        className = argIn[0].getContentAsCString();
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }

    stringVector members = ClassdefDefinitionManager::getInstance()->enumerations(className);
    retval << ArrayOf::toCellArrayOfCharacterColumnVectors(members);
    return retval;
}
//=============================================================================
