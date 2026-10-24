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
        className = argIn[0].classType();
    } else if (argIn[0].isHandle()) {
        className = argIn[0].handleClassName();
    } else if (argIn[0].isRowVectorCharacterArray()) {
        className = argIn[0].asUtf8String();
    }
    if (!className.empty() && argIn[1].isRowVectorCharacterArray()
        && classdefLoadClass(className)) {
        std::string methodName = argIn[1].asUtf8String();
        bool res = classdefHasMethod(className, methodName)
            || classdefHasStaticMethod(className, methodName);
        retval << ArrayOf::logicalScalar(res);
        return retval;
    }
    OverloadRequired("ismethod");
    return retval;
}
//=============================================================================
