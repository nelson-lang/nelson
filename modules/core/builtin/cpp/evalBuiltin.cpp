//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "evalBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "EvaluateCommand.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::evalBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    std::wstring command;
    std::wstring catchCommand;
    if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        command = argIn[0].getContentAsWideString();
    } else {
        Error(_W("#1 string expected."));
    }
    if (argIn.size() > 1) {
        if (argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray()) {
            catchCommand = argIn[1].getContentAsWideString();
        } else {
            Error(_W("#2 string expected."));
        }
    }
    return EvaluateCommand(eval, nLhs, command, catchCommand);
}
//=============================================================================
