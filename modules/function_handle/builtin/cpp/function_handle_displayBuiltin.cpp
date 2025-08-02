//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "function_handle_displayBuiltin.hpp"
#include "Error.hpp"
#include "DisplayFunctionHandle.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionHandleGateway::function_handle_dispBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 1);
    Interface* io = nullptr;
    if (eval) {
        io = eval->getInterface();
    }
    DisplayFunctionHandle(io, argIn[0], L"", true);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::FunctionHandleGateway::function_handle_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 2);
    ArrayOf param1 = argIn[0];
    std::wstring wname = param1.wname();
    if (argIn.size() == 2) {
        wname = argIn[1].getContentAsWideString();
    }
    Interface* io = nullptr;
    if (eval) {
        io = eval->getInterface();
    }
    DisplayFunctionHandle(io, argIn[0], wname, false);
    return retval;
}
//=============================================================================
