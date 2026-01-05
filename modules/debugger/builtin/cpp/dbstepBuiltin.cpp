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
    enum Mode
    {
        Normal,
        In,
        Out
    } mode
        = Normal;

    if (argIn.size() == 1) {
        // Check if it's a number or string
        if (argIn[0].isNumeric()) {
            steps = (int)argIn[0].getContentAsScalarIndex(false);
            if (steps < 1) {
                steps = 1;
            }
            mode = Normal;
        } else if (argIn[0].isScalarStringArray() || argIn[0].isRowVectorCharacterArray()) {
            std::wstring modeStr = argIn[0].getContentAsWideString();
            if (modeStr == _W("in")) {
                mode = In;
            } else if (modeStr == _W("out")) {
                mode = Out;
            } else {
                Error(_W("Invalid string argument for dbstep. Expected 'in' or 'out'."));
            }
        } else {
            Error(_W("Invalid argument type for dbstep. Expected numeric or string."));
        }
    }

    if (!eval->isBreakpointActive()) {
        Error(_W("Debugger is not active."));
    }

    eval->stepMode = true;
    const Breakpoint& currentStepBreakPoint = *eval->stepBreakpoint;
    switch (mode) {
    case Mode::In: {
        // dbstep in: step to next line, and if that line contains a call to another
        // Nelson code file function, step into that function's first executable line.
        if (currentStepBreakPoint.line + steps <= currentStepBreakPoint.maxLines) {
            Breakpoint stepBp;
            stepBp.filename = currentStepBreakPoint.filename;
            stepBp.functionName = currentStepBreakPoint.functionName;
            stepBp.maxLines = currentStepBreakPoint.maxLines;
            stepBp.enabled = true;
            stepBp.line = currentStepBreakPoint.line + steps;
            stepBp.stepMode = true;
            stepBp.stepInto = true; // Mark for stepping into function calls
            eval->addBreakpoint(stepBp);
            eval->setState(NLS_STATE_DEBUG_STEP);
        } else {
            eval->setState(NLS_STATE_DEBUG_CONTINUE);
        }
        eval->stepBreakpoint.reset();
    } break;
    case Mode::Out: {
        // dbstep out: run the rest of the current function and pause just after leaving.
        // Set a step breakpoint that triggers when we return to a shallower call depth.
        Breakpoint stepBp;
        stepBp.filename = currentStepBreakPoint.filename;
        stepBp.functionName = currentStepBreakPoint.functionName;
        stepBp.maxLines = currentStepBreakPoint.maxLines;
        stepBp.enabled = true;
        stepBp.line = 0; // Line 0 means "any line at target depth"
        stepBp.stepMode = true;
        stepBp.stepOut = true;
        // targetDepth: we want to stop when callstack depth is less than current
        stepBp.targetDepth = static_cast<int>(eval->callstack.size()) - 1;
        eval->addBreakpoint(stepBp);
        eval->setState(NLS_STATE_DEBUG_CONTINUE);
        eval->stepBreakpoint.reset();
    } break;
    case Mode::Normal: {
        if (currentStepBreakPoint.line + steps <= currentStepBreakPoint.maxLines) {
            Breakpoint stepBp;
            stepBp.filename = currentStepBreakPoint.filename;
            stepBp.functionName = currentStepBreakPoint.functionName;
            stepBp.maxLines = currentStepBreakPoint.maxLines;
            stepBp.enabled = true;
            stepBp.line = currentStepBreakPoint.line + steps;
            stepBp.stepMode = true;
            eval->addBreakpoint(stepBp);
            eval->setState(NLS_STATE_DEBUG_STEP);
        } else {
            eval->setState(NLS_STATE_DEBUG_CONTINUE);
        }
        eval->stepBreakpoint.reset();
    } break;
    default: {
        Error(_W("Unknown mode for dbstep."));
    }
    }

    return {};
}
//=============================================================================
