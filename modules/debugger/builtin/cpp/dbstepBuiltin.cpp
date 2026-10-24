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
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static void
handleNormalSingleStep(Evaluator* eval, const Breakpoint& currentStepBreakPoint);
static void
handleNormalMultiStep(Evaluator* eval, const Breakpoint& currentStepBreakPoint, int steps);
//=============================================================================
ArrayOfVector
Nelson::DebuggerGateway::dbstepBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 0);

    int steps = 1;
    enum Mode
    {
        Normal,
        In,
        Out
    } mode
        = Normal;

    if (argIn.size() == 1) {
        if (argIn[0].isNumeric()) {
            steps = (int)argIn[0].asScalarIndex(false);
            if (steps < 1) {
                steps = 1;
            }
            mode = Normal;
        } else if (argIn[0].isScalarStringArray() || argIn[0].isRowVectorCharacterArray()) {
            std::wstring modeStr = argIn[0].asWideString();
            if (modeStr == _W("in")) {
                mode = In;
            } else if (modeStr == _W("out")) {
                mode = Out;
            } else {
                Error(L"Nelson:dbstep:invalidArgument",
                    L"Invalid string argument for dbstep. Expected 'in' or 'out'.");
            }
        } else {
            Error(L"Nelson:dbstep:invalidArgumentType",
                L"Invalid argument type for dbstep. Expected numeric or string.");
        }
    }

    if (!eval->isBreakpointActive()) {
        Error(L"Nelson:dbstep:debuggerNotActive", L"Debugger is not active.");
    }
    if (!(eval->stepBreakpoint.has_value())) {
        eval->bpActive = false;
        Error(L"Nelson:dbstep:noStepBreakpoint", L"No step breakpoint is set.");
    }

    eval->stepMode = true;
    const Breakpoint& currentStepBreakPoint = *eval->stepBreakpoint;
    switch (mode) {
    case Mode::In: {
        // stepNext avoids landing on non-executed branches.
        Breakpoint stepBp;
        stepBp.filename = currentStepBreakPoint.filename;
        stepBp.functionName = "";
        stepBp.maxLines = currentStepBreakPoint.maxLines;
        stepBp.enabled = true;
        stepBp.line = 0; // not used for stepNext
        stepBp.stepMode = true;
        stepBp.stepNext = true;
        stepBp.stepInto = true;
        // -1: no depth limit, so deeper functions can break
        stepBp.targetDepth = -1;
        stepBp.fromLine = currentStepBreakPoint.line;
        eval->addBreakpoint(stepBp);
        eval->setState(NLS_STATE_DEBUG_STEP);
        // stepBreakpoint is updated by onBytecodeBreakpoint() on the next hit.
    } break;
    case Mode::Out: {
        Breakpoint stepBp;
        stepBp.filename = currentStepBreakPoint.filename;
        stepBp.functionName = currentStepBreakPoint.functionName;
        stepBp.maxLines = currentStepBreakPoint.maxLines;
        stepBp.enabled = true;
        stepBp.line = 0; // Line 0 means "any line at target depth"
        stepBp.stepMode = true;
        stepBp.stepOut = true;
        // stop once the call depth drops below the current one
        stepBp.targetDepth = static_cast<int>(eval->callstack.size()) - 1;
        eval->addBreakpoint(stepBp);
        eval->setState(NLS_STATE_DEBUG_CONTINUE);
    } break;
    case Mode::Normal: {
        if (steps == 1) {
            handleNormalSingleStep(eval, currentStepBreakPoint);
        } else {
            handleNormalMultiStep(eval, currentStepBreakPoint, steps);
        }
    } break;
    default: {
        Error(L"Nelson:dbstep:unknownMode", L"Unknown mode for dbstep.");
    }
    }

    return {};
}
//=============================================================================
void
handleNormalSingleStep(Evaluator* eval, const Breakpoint& currentStepBreakPoint)
{
    // stepNext breaks at the next executed line whatever the control flow.
    Breakpoint stepBp;
    stepBp.filename = currentStepBreakPoint.filename;
    stepBp.functionName = currentStepBreakPoint.functionName;
    if (stepBp.functionName.empty()) {
        Scope* scope = eval->getContext()->getCurrentScope();
        if (scope != nullptr) {
            stepBp.functionName = scope->getName();
        }
        if (stepBp.functionName.empty()) {
            stepBp.functionName = currentStepBreakPoint.functionName;
        }
    }
    stepBp.maxLines = currentStepBreakPoint.maxLines;
    stepBp.enabled = true;
    stepBp.line = 0;
    stepBp.stepMode = true;
    stepBp.stepNext = true;
    stepBp.stepInto = false;
    stepBp.targetDepth = static_cast<int>(eval->callstack.size());
    stepBp.fromLine = currentStepBreakPoint.line;
    eval->addBreakpoint(stepBp);
    eval->setState(NLS_STATE_DEBUG_STEP);
}
//=============================================================================
void
handleNormalMultiStep(Evaluator* eval, const Breakpoint& currentStepBreakPoint, int steps)
{
    size_t nextLine = currentStepBreakPoint.line + static_cast<size_t>(steps);
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
}
//=============================================================================
