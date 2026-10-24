//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "methodsBuiltin.hpp"
#include "ClassdefParser.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::methodsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    std::string className;
    if (argIn[0].isClassType()) {
        className = argIn[0].classType();
    } else if (argIn[0].isHandle()) {
        className = argIn[0].handleClassName();
    } else if (argIn[0].isRowVectorCharacterArray()) {
        className = argIn[0].asUtf8String();
    }
    if (!className.empty() && classdefLoadClass(className)) {
        retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(classdefMethods(className));
        return retval;
    }
    OverloadRequired("methods");
    return retval;
}
//=============================================================================
