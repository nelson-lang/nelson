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
        Error("Wrong number of input arguments.");
    }

    if (argIn.size() == 1 && argIn[0].getContentAsWideString() == L"all") {
        eval->clearBreakpoints();
        return {};
    }
    size_t position = parsePositionArgument(argIn);
    std::wstring target = argIn[1].getContentAsWideString();

    if (!eval->removeBreakpoint(target, position)) {
        Error(_W("No breakpoint found at specified location."));
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
        Error(_W("Third argument must be 'at'."));
    }

    std::wstring posArg = argIn[3].getContentAsWideString();
    size_t idx = 0;

    try {
        size_t pos = std::stoul(posArg, &idx);
        if (idx != posArg.size()) {
            Error(_W("Invalid position argument."));
        }
        return pos;
    } catch (...) {
        Error(_W("Invalid position argument."));
    }

    return 1; // unreachable
}
//=============================================================================
