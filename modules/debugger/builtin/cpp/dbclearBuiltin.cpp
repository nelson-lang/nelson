//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dbclearBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static size_t
parsePositionArgument(const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
Nelson::DebuggerGateway::dbclearBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // dbclear all
    // dbclear in file
    // dbclear in file at location
    nargincheck(argIn, 1, 4);
    nargoutcheck(nLhs, 0, 0);

    if (argIn.size() == 3) {
        raiseError2(L"nelson:arguments:wrongNumberOfInputs");
    }

    if (argIn.size() == 1 && argIn[0].getContentAsWideString() == L"all") {
        eval->clearBreakpoints();
        return {};
    }
    size_t position = parsePositionArgument(argIn);
    std::wstring target = argIn[1].getContentAsWideString();

    if (!eval->removeBreakpoint(target, position)) {
        raiseError(L"Nelson:debugger:ERROR_NO_BREAKPOINT_FOUND_AT_SPECIFIED_LOCATION",
            ERROR_NO_BREAKPOINT_FOUND_AT_SPECIFIED_LOCATION);
    }

    return {};
}
//=============================================================================
size_t
parsePositionArgument(const ArrayOfVector& argIn)
{
    if (argIn.size() != 4) {
        return 1;
    }

    if (argIn[2].getContentAsWideString() != L"at") {
        raiseError(
            L"Nelson:debugger:ERROR_THIRD_ARGUMENT_MUST_BE_AT", ERROR_THIRD_ARGUMENT_MUST_BE_AT);
    }

    std::wstring posArg = argIn[3].getContentAsWideString();
    size_t idx = 0;

    try {
        size_t pos = std::stoul(posArg, &idx);
        if (idx != posArg.size()) {
            raiseError(L"Nelson:debugger:ERROR_INVALID_POSITION_ARGUMENT",
                ERROR_INVALID_POSITION_ARGUMENT);
        }
        return pos;
    } catch (...) {
        raiseError(
            L"Nelson:debugger:ERROR_INVALID_POSITION_ARGUMENT", ERROR_INVALID_POSITION_ARGUMENT);
    }

    return 1; // unreachable
}
//=============================================================================
