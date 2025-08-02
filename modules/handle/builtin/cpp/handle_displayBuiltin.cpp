//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "handle_displayBuiltin.hpp"
#include "Error.hpp"
#include "DisplayVariableHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::handle_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_FUNCTION_HANDLE_EXPECTED);
    }
    Interface* io = eval->getInterface();
    std::wstring name;
    if (argIn.size() == 2) {
        name = argIn[1].getContentAsWideString();
    }
    if (io) {
        Interface* io = eval->getInterface();
        DisplayVariableHeader(io, param1, name, false);
        DisplayVariableFooter(io, name.empty());
    }
    return retval;
}
//=============================================================================
