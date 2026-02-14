//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "evalcBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "EvaluateCommand.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::evalcBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    std::wstring command;
    std::wstring catchCommand;
    if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        command = argIn[0].getContentAsWideString();
    } else {
        raiseError2(L"Nelson:error_manager:wrong_type_with_expected", 1, L"string");
    }
    if (argIn.size() > 1) {
        if (argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray()) {
            catchCommand = argIn[1].getContentAsWideString();
        } else {
            raiseError2(L"Nelson:error_manager:wrong_type_with_expected", 2, L"string");
        }
    }
    return EvaluateConsoleCommand(eval, nLhs, command, catchCommand);
}
//=============================================================================
