//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dbstatusBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DebuggerGateway::dbstatusBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    std::vector<Breakpoint> breakpoints = eval->getBreakpoints();
    Interface* io = eval->getInterface();
    for (auto breakpoint : breakpoints) {
        // Skip temporary step-mode breakpoints in status output
        if (breakpoint.stepMode) {
            continue;
        }
        // Skip placeholders with line 0 (used for step-out/step-next internals)
        if (breakpoint.line == 0) {
            continue;
        }
        io->outputMessage(L"Breakpoint at ");
        io->outputMessage(breakpoint.filename);
        io->outputMessage(L", line ");
        io->outputMessage(std::to_wstring(breakpoint.line));
        io->outputMessage(L"\n");
    }
    return retval;
}
//=============================================================================
