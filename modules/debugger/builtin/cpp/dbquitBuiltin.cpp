//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dbquitBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DebuggerGateway::dbquitBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 0);
    if (argIn.size() == 1) {
        std::wstring argAll = argIn[0].getContentAsWideString();
        if (argAll != L"all") {
            raiseError(L"Nelson:debugger:ERROR_WRONG_VALUE_ARG1_ALL_EXPECTED",
                ERROR_WRONG_VALUE_ARG1_ALL_EXPECTED);
        }
        eval->setState(NLS_STATE_DEBUG_QUIT_ALL);
    } else {
        eval->setState(NLS_STATE_DEBUG_QUIT);
    }
    return {};
}
//=============================================================================
