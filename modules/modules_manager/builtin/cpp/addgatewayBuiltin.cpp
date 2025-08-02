//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "addgatewayBuiltin.hpp"
#include "AddGateway.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ModulesManagerGateway::addgatewayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);
    std::wstring moduleName;
    if (argIn.size() > 1) {
        moduleName = argIn[1].getContentAsWideString();
    }
    if (argIn[0].isRowVectorCharacterArray()) {
        std::wstring dynlibName = argIn[0].getContentAsWideString();
        AddGateway(eval, dynlibName, moduleName);
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    return retval;
}
//=============================================================================
