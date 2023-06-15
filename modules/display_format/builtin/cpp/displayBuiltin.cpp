//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "displayBuiltin.hpp"
#include "Error.hpp"
#include "OverloadDisplay.hpp"
#include "DisplayVariable.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DisplayFormatGateway::displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);

    ArrayOf variable = argIn[0];
    std::wstring variableName = variable.wname();
    if (argIn.size() == 2) {
        variableName = argIn[1].getContentAsWideString();
    }

    OverloadDisplay(eval, variable, variableName, false);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::DisplayFormatGateway::generic_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf variable = argIn[0];
    std::wstring variableName = variable.wname();
    if (argIn.size() == 2) {
        variableName = argIn[1].getContentAsWideString();
    }
    bool needToOverload;
    DisplayVariable(
        eval->getID(), eval->getInterface(), variable, variableName, false, needToOverload);
    return retval;
}
//=============================================================================
