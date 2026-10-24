//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#include <Windows.h>
#else
#include <unistd.h>
#endif
//=============================================================================
#include <regex>
#include "CallbackQueue.hpp"
#include "EventQueue.hpp"
#include "MainThreadTaskQueue.hpp"
#include "WebDesktopBridge.hpp"
#include "Evaluator.hpp"
#include "LexerInterface.hpp"
#include "NelsonConfiguration.hpp"
#include "NelsonReadyNamedMutex.hpp"
#include "ParserInterface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    bool doOnce = true;
    //=============================================================================
    void
    setNamedMutexNelsonReady()
    {
#ifdef _MSC_VER
        openIsReadyNelsonMutex(static_cast<int>(GetCurrentProcessId()));
#else
        openIsReadyNelsonMutex(static_cast<int>(getpid()));
#endif
    }
    //=============================================================================
    std::wstring
    normalizeCliNewlines(const std::wstring& input)
    {
        static const std::wregex newline_re(L"\n+");
        return std::regex_replace(input, newline_re, L"\n");
    }
    //=============================================================================
} // namespace
//=============================================================================
void
Evaluator::evalCLI()
{
    while (true) {
        if (!bpActive) {
            clearStacks();
        }
        if (processCliCallbacks()) {
            return;
        }
        std::wstring commandLine;
        if (!readCliCommandLine(commandLine)) {
            return;
        }
        try {
            if (!completeCliCommandLine(commandLine)) {
                return;
            }
        } catch (NelsonException& e) {
            setLastErrorException(e);
            setLastUncaughtException(e);
            e.printMe(io);
            commandLine.clear();
        }
        InCLI = true;
        if (!commandLine.empty() && !evaluateCliCommand(commandLine)) {
            return;
        }
    }
}
//=============================================================================
bool
Evaluator::processCliCallbacks()
{
    // Cross-thread tasks first: a background poster is blocked on its future.
    MainThreadTaskQueue::getInstance()->processTasks(this);
    // nelson-adv-cli --webview: service native figure windows between prompts so a
    // freshly created figure paints and stays responsive. No-op unless the
    // figures-only web desktop registered the pump hook.
    WebDesktopBridge* pumpBridge = WebDesktopBridge::getInstance();
    if (pumpBridge->hasWebEventPumpHook()) {
        pumpBridge->pumpWebEvents();
    }
    if (EventQueue::getInstance()->processCallback(this)) {
        return true;
    }
    return CallbackQueue::getInstance()->processCallback(this);
}
//=============================================================================
bool
Evaluator::readCliCommandLine(std::wstring& commandLine)
{
    commandQueue.get(commandLine);
    if (!commandLine.empty()) {
        return true;
    }
    if (doOnce) {
        setNamedMutexNelsonReady();
        doOnce = false;
    }
    resetDebugPromptIfExecutionFinished();
    commandLine = io->getLine(buildPrompt());
    if (commandLine.empty()) {
        InCLI = false;
        setState(NLS_STATE_QUIT);
        return false;
    }
    appendCliNewline(commandLine);
    return true;
}
//=============================================================================
void
Evaluator::resetDebugPromptIfExecutionFinished()
{
    bool noExecutionFrame = callstack.size() <= 1;
    if (bpActive && noExecutionFrame) {
        bpActive = false;
        stepMode = false;
        stepBreakpoint.reset();
        clearStepBreakpoints();
    }
}
//=============================================================================
bool
Evaluator::completeCliCommandLine(std::wstring& commandLine)
{
    AbstractSyntaxTree::clearReferences();
    setLexBuffer(lexerContext, commandLine);
    bool bContinueLine = lexCheckForMoreInput(lexerContext, 0);
    AbstractSyntaxTree::deleteReferences();
    if (!bContinueLine) {
        return true;
    }
    int lastCount = getContinuationCount(lexerContext);
    std::wstring lines = commandLine;
    bool enoughInput = lexerContext.lineNumber > 2;
    int emptyLineCount = 0;
    while (!enoughInput) {
        commandLine = io->getLine(getCliContinuationPrompt());
        if (commandLine == L"\n" || commandLine.empty()) {
            emptyLineCount++;
            if (NelsonConfiguration::getInstance()->getInterruptPending(ID)) {
                commandLine.clear();
                return false;
            }
            if (emptyLineCount >= 2 || isSafeToAutoTerminateCLI()) {
                enoughInput = true;
                break;
            }
            continue;
        }
        emptyLineCount = 0;
        appendCliNewline(commandLine);
        lines.append(commandLine);
        AbstractSyntaxTree::clearReferences();
        setLexBuffer(lexerContext, lines);
        enoughInput = !lexCheckForMoreInput(lexerContext, lastCount);
        AbstractSyntaxTree::deleteReferences();
        lastCount = getContinuationCount(lexerContext);
        if (enoughInput) {
            lines.append(L"\n");
        }
    }
    commandLine = std::move(lines);
    return true;
}
//=============================================================================
std::wstring
Evaluator::getCliContinuationPrompt() const
{
    if (lexerContext.inMultilineComment) {
        return L"%> ";
    }
    if (lexerContext.bracketStackSize > 0) {
        switch (lexerContext.bracketStack[lexerContext.bracketStackSize - 1]) {
        case '[':
            return L"[> ";
        case '{':
            return L"{> ";
        case '(':
            return L"(> ";
        default:
            break;
        }
    }
    if (lexerContext.inFunction) {
        return L"f> ";
    }
    if (lexerContext.inStatement > 0) {
        return L"s> ";
    }
    return L"";
}
//=============================================================================
bool
Evaluator::isSafeToAutoTerminateCLI()
{
    if (lexerContext.bracketStackSize > 0) {
        return false;
    }
    return (lexerContext.inStatement >= 0 && !lexerContext.inFunction);
}
//=============================================================================
void
Evaluator::appendCliNewline(std::wstring& commandLine)
{
    wchar_t ch = *commandLine.rbegin();
    if (ch != L'\n') {
        commandLine.push_back(L'\n');
    }
}
//=============================================================================
bool
Evaluator::evaluateCliCommand(std::wstring commandLine)
{
    size_t stackdepth = callstack.size();
    if (lexerContext.lineNumber > 1) {
        commandLine = normalizeCliNewlines(commandLine);
    }
    bool evalResult = false;
    try {
        evalResult = evaluateString(commandLine, false);
    } catch (NelsonException& e) {
        setLastErrorException(e);
        setLastUncaughtException(e);
        e.printMe(io);
    }
    while (callstack.size() > stackdepth) {
        callstack.pop_back();
    }
    return handleCliEvaluationResult(evalResult);
}
//=============================================================================
bool
Evaluator::handleCliEvaluationResult(bool evalResult)
{
    if (getState() == NLS_STATE_DEBUG_QUIT_ALL) {
        depth = 0;
        bpActive = false;
        InCLI = false;
        return false;
    }
    if (getState() == NLS_STATE_DEBUG_QUIT) {
        if (depth > 0) {
            depth--;
        }
        InCLI = false;
        return false;
    }
    if (getState() == NLS_STATE_DEBUG_STEP || getState() == NLS_STATE_DEBUG_CONTINUE) {
        InCLI = false;
        return false;
    }
    if (!evalResult || isQuitOrForceQuitState() || getState() == NLS_STATE_ABORT) {
        if (getState() == NLS_STATE_ABORT && bpActive) {
            resetState();
            return true;
        }
        InCLI = false;
        return false;
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
