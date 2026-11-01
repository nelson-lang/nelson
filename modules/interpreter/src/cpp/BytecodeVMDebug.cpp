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
// debugger stops: breakpoint hit, loop end, keyboard statement or error with dbstop if error
//=============================================================================
#include "BytecodeVMInternal.hpp"
#include "Evaluator.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson::bcvm_detail {
//=============================================================================
static bool
runDebugPromptLoop(Evaluator* eval);
static bool
debugStopAtSpan(Evaluator* eval, const BytecodeChunk& chunk, const SourceSpan& span,
    const std::function<void()>& syncToContext, const std::function<void()>& refreshFromContext);
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
    return debugStopAtSpan(eval, chunk, chunk.spans[ins.A], syncToContext, refreshFromContext);
}
//=============================================================================
bool
handleLoopEndDebugStatement(Evaluator* eval, VMCallFrame& frame, const SourceSpan& span,
    const std::function<void()>& syncToContext, const std::function<void()>& refreshFromContext)
{
    return debugStopAtSpan(eval, *frame.chunk, span, syncToContext, refreshFromContext);
}
//=============================================================================
static bool
debugStopAtSpan(Evaluator* eval, const BytecodeChunk& chunk, const SourceSpan& span,
    const std::function<void()>& syncToContext, const std::function<void()>& refreshFromContext)
{
    size_t line = static_cast<size_t>(span.line);
    size_t maxLine = span.endLine == 0 ? line : static_cast<size_t>(span.endLine);
    if (!eval->onBytecodeBreakpoint(chunk.sourcePath, chunk.functionName, line, maxLine)) {
        return false;
    }
    // locals must be visible to a condition and to the K>> commands
    syncToContext();
    if (!eval->confirmPendingDebugStop(static_cast<size_t>(span.col))) {
        refreshFromContext();
        return false;
    }
    eval->displayDebugStopLine();
    bool stop = runDebugPromptLoop(eval);
    if (!stop) {
        refreshFromContext();
    }
    return stop;
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
void
runErrorStopPrompt(Evaluator* eval, const NelsonException& e, const BytecodeChunk& chunk,
    size_t statementContextId, bool willBeCaught)
{
    size_t fallbackLine = statementContextId & 0xFFFF;
    eval->beginErrorStop(e, chunk.sourcePath, chunk.functionName, fallbackLine);
    Interface* io = eval->getDefaultInterface() != nullptr ? eval->getDefaultInterface()
                                                           : eval->getInterface();
    if (io != nullptr) {
        if (willBeCaught) {
            const DebugStopInfo& info = eval->getDebugStopInfo();
            std::wstring where
                = info.functionName.empty() ? info.filename : utf8_to_wstring(info.functionName);
            io->outputMessage(
                formatI18nMessage(_W("Caught-error breakpoint was hit in %s at line %d. The error "
                                     "was:"),
                    where, static_cast<int>(info.line))
                + L"\n\n" + e.message() + L"\n\n");
        } else {
            // the error is printed first and not again when the session ends
            eval->setLastErrorException(e);
            eval->setLastUncaughtException(e);
            e.printMe(io);
            eval->markErrorDisplayed();
        }
    }
    eval->displayDebugStopLine();
    runDebugPromptLoop(eval);
}
//=============================================================================
static bool
runDebugPromptLoop(Evaluator* eval)
{
    size_t bypassedScopesAtEntry = eval->getContext()->getBypassedScopeCount();
    int depthAtEntry = eval->getDebugDepth();
    auto leave = [&]() {
        eval->restoreDebugScopes(bypassedScopesAtEntry, depthAtEntry);
        eval->noteDebugStateChanged();
    };
    eval->bpActive = true;
    while (true) {
        eval->debugCLI();
        State state = eval->getState();
        if (state == NLS_STATE_ABORT) {
            eval->bpActive = false;
            leave();
            return true;
        }
        if (state == NLS_STATE_QUIT || state == NLS_STATE_FORCE_QUIT) {
            // End of input (batch run with stdin exhausted) or exit/quit.
            eval->bpActive = false;
            leave();
            eval->resetDebugDepth();
            return true;
        }
        if (state == NLS_STATE_DEBUG_QUIT_ALL || state == NLS_STATE_DEBUG_QUIT) {
            eval->bpActive = false;
            eval->setState(NLS_STATE_ABORT);
            leave();
            return true;
        }
        if (state == NLS_STATE_DEBUG_CONTINUE || state == NLS_STATE_DEBUG_STEP) {
            eval->bpActive = (state == NLS_STATE_DEBUG_STEP);
            eval->setState(NLS_STATE_OK);
            eval->markDebugResumed();
            leave();
            return false;
        }
    }
}
//=============================================================================
} // namespace Nelson::bcvm_detail
//=============================================================================
