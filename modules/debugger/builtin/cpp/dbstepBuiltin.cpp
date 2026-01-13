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
#include "characters_encoding.hpp"
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
    if (!(eval->stepBreakpoint.has_value())) {
        // If no step breakpoint is set but we're active, we might be at a persistent
        // user breakpoint that wasn't properly matched. Try to create one from current context.
        // If that also fails, then the script has truly finished.
        eval->bpActive = false;
        Error(_W("No step breakpoint is set."));
    }

    eval->stepMode = true;
    const Breakpoint& currentStepBreakPoint = *eval->stepBreakpoint;
    switch (mode) {
    case Mode::In: {
        // dbstep in: break at first executed line after current, and allow stepping into functions.
        // Use stepNext to avoid landing on non-executed branches.
        Breakpoint stepBp;
        stepBp.filename = currentStepBreakPoint.filename;
        // For scripts and robustness, always use empty functionName for stepNext
        stepBp.functionName = "";
        stepBp.maxLines = currentStepBreakPoint.maxLines;
        stepBp.enabled = true;
        stepBp.line = 0; // not used for stepNext
        stepBp.stepMode = true;
        stepBp.stepNext = true;
        stepBp.stepInto = true;
        // For step-into, allow breaking in deeper functions; depth check is not needed
        stepBp.targetDepth = -1;
        // Start after current line
        stepBp.fromLine = currentStepBreakPoint.line;
        eval->addBreakpoint(stepBp);
        eval->setState(NLS_STATE_DEBUG_STEP);
        // Don't reset stepBreakpoint - it will be updated by onBreakpoint() when we hit the next
        // line
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
        // Don't reset stepBreakpoint - it will be updated by onBreakpoint() when we return
    } break;
    case Mode::Normal: {
        if (steps == 1) {
            // Always use stepNext to properly handle all control flow (loops, conditionals)
            // This ensures we break at the next executed line, regardless of source order
            Breakpoint stepBp;
            stepBp.filename = currentStepBreakPoint.filename;
            // Stay in the current function for plain dbstep; dbstep in uses stepInto instead
            stepBp.functionName = currentStepBreakPoint.functionName;
            if (stepBp.functionName.empty()) {
                // If missing, default to current scope to ensure step-over behavior
                stepBp.functionName = eval->getContext()->getCurrentScope()->getName();
                if (stepBp.functionName.empty()) {
                    // As a last fallback, use the function name from the current breakpoint
                    stepBp.functionName = currentStepBreakPoint.functionName;
                }
            }
            stepBp.maxLines = currentStepBreakPoint.maxLines;
            stepBp.enabled = true;
            stepBp.line = 0;
            stepBp.stepMode = true;
            stepBp.stepNext = true;
            stepBp.stepInto = false; // Explicitly disable step-into for step-over behavior
            // Stop in this function or when callstack unwinds back to this depth (step-over)
            stepBp.targetDepth = static_cast<int>(eval->callstack.size());
            stepBp.fromLine = currentStepBreakPoint.line;
            eval->addBreakpoint(stepBp);
            eval->setState(NLS_STATE_DEBUG_STEP);
            // Don't set bpActive false - it should remain true for stepping
        } else {
            // Fallback for multi-step: scan forward for executable lines
            size_t nextLine = currentStepBreakPoint.line + steps;
            size_t actualNextLine = nextLine;
            std::wstring errorMsg;
            size_t adjustedLine = 0;
            while (actualNextLine <= currentStepBreakPoint.maxLines) {
                if (eval->adjustBreakpointLine(
                        currentStepBreakPoint.filename, actualNextLine, adjustedLine, errorMsg)) {
                    break;
                }
                actualNextLine++;
            }
            if (actualNextLine <= currentStepBreakPoint.maxLines) {
                Breakpoint stepBp;
                stepBp.filename = currentStepBreakPoint.filename;
                stepBp.functionName = currentStepBreakPoint.functionName;
                stepBp.maxLines = currentStepBreakPoint.maxLines;
                stepBp.enabled = true;
                stepBp.line = adjustedLine;
                stepBp.stepMode = true;
                eval->addBreakpoint(stepBp);
                eval->setState(NLS_STATE_DEBUG_STEP);
            } else {
                eval->bpActive = false;
                eval->setState(NLS_STATE_DEBUG_CONTINUE);
            }
            // Don't reset stepBreakpoint - it will be updated by onBreakpoint() when we hit the
            // next line
        }
    } break;
    default: {
        Error(_W("Unknown mode for dbstep."));
    }
    }

    return {};
}
//=============================================================================
