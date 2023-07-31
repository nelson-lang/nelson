//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isclassBuiltin.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TypeGateway::isclassBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    bool bIsClass = argIn[0].isClassType();
    if (bIsClass) {
        std::string structType = argIn[0].getClassType();
        if ((structType == NLS_FUNCTION_HANDLE_STR) || (structType == NLS_GENERIC_STR)) {
            bIsClass = false;
        }
    }
    retval << ArrayOf::logicalConstructor(bIsClass);
    return retval;
}
//=============================================================================
