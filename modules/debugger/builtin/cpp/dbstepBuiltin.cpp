//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dbstepBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DebuggerGateway::dbstepBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 0);

    int steps = 1; // Default: step 1 statement

    if (argIn.size() == 1) {
        // Check if it's a number or string
        if (argIn[0].isNumeric()) {
            steps = (int)argIn[0].getContentAsScalarIndex(false);
            if (steps < 1) {
                steps = 1;
            }
        } else {
            Error(_W("dbstep argument must be a number."));
        }
    }

    if (!eval->isBreakpointActive()) {
        Error(_W("Debugger is not active."));
    }

    eval->stepMode = true;
    if (eval->stepBreakpoint.has_value()) {
        if (eval->stepBreakpoint->line + steps < eval->stepBreakpoint->maxLines) {
            Breakpoint stepBp;
            stepBp.filename = eval->stepBreakpoint->filename;
            stepBp.functionName = eval->stepBreakpoint->functionName;
            stepBp.maxLines = eval->stepBreakpoint->maxLines;
            stepBp.enabled = true;

            stepBp.line = eval->stepBreakpoint->line + steps;
            stepBp.stepMode = true;
            eval->addBreakpoint(stepBp);
            eval->setState(NLS_STATE_DEBUG_STEP);
        } else {
            eval->setState(NLS_STATE_DEBUG_CONTINUE);
        }
        eval->stepBreakpoint.reset();
    }    
    return {};
}
//=============================================================================
