//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Interactive debug prompt (K>>) entered from the bytecode VM, either on a
// breakpoint hit (DEBUG_STMT) or unconditionally on a `keyboard` statement
// (KEYBOARD). Cold: only reached when the user debugs.
//=============================================================================
#include "BytecodeVMInternal.hpp"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson::bcvm_detail {
//=============================================================================
static bool
runDebugPromptLoop(Evaluator* eval);
//=============================================================================
bool
runDebugPrompt(Evaluator* eval, const std::function<void()>& syncToContext,
    const std::function<void()>& refreshFromContext)
{
    syncToContext();
    bool stop = runDebugPromptLoop(eval);
    if (!stop) {
        refreshFromContext();
    }
    return stop;
}
//=============================================================================
bool
handleDebugStatement(Evaluator* eval, VMCallFrame& frame, const Instruction& ins,
    const std::function<void()>& syncToContext, const std::function<void()>& refreshFromContext)
{
    const BytecodeChunk& chunk = *frame.chunk;
    if (ins.A >= chunk.spans.size()) {
        return false;
    }
    const SourceSpan& span = chunk.spans[ins.A];
    size_t line = static_cast<size_t>(span.line);
    size_t maxLine = span.endLine == 0 ? line : static_cast<size_t>(span.endLine);
    if (!eval->onBytecodeBreakpoint(chunk.sourcePath, chunk.functionName, line, maxLine)) {
        return false;
    }
    return runDebugPrompt(eval, syncToContext, refreshFromContext);
}
//=============================================================================
bool
handleKeyboardStatement(Evaluator* eval, VMCallFrame& frame, const Instruction& ins,
    const std::function<void()>& syncToContext, const std::function<void()>& refreshFromContext)
{
    const BytecodeChunk& chunk = *frame.chunk;
    if (ins.A < chunk.spans.size()) {
        const SourceSpan& span = chunk.spans[ins.A];
        size_t line = static_cast<size_t>(span.line);
        size_t maxLine = span.endLine == 0 ? line : static_cast<size_t>(span.endLine);
        eval->onBytecodeKeyboard(chunk.sourcePath, chunk.functionName, line, maxLine);
    }
    return runDebugPrompt(eval, syncToContext, refreshFromContext);
}
//=============================================================================
static bool
runDebugPromptLoop(Evaluator* eval)
{
    eval->bpActive = true;
    while (true) {
        eval->debugCLI();
        State state = eval->getState();
        if (state == NLS_STATE_ABORT) {
            eval->bpActive = false;
            return true;
        }
        if (state == NLS_STATE_QUIT || state == NLS_STATE_FORCE_QUIT) {
            // End of input (batch run with stdin exhausted) or exit/quit.
            eval->bpActive = false;
            eval->resetDebugDepth();
            return true;
        }
        if (state == NLS_STATE_DEBUG_QUIT_ALL || state == NLS_STATE_DEBUG_QUIT) {
            eval->bpActive = false;
            eval->setState(NLS_STATE_ABORT);
            return true;
        }
        if (state == NLS_STATE_DEBUG_CONTINUE || state == NLS_STATE_DEBUG_STEP) {
            eval->bpActive = (state == NLS_STATE_DEBUG_STEP);
            eval->setState(NLS_STATE_OK);
            return false;
        }
    }
}
//=============================================================================
} // namespace Nelson::bcvm_detail
//=============================================================================
