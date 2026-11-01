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
#endif
//=============================================================================
#include <atomic>
#include <functional>
#include <cstdlib>
#include <cstdio>
#include <cwctype>
#include <fstream>
#include <algorithm>
#include "i18n.hpp"
#include "Evaluator.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "MacroFunctionDef.hpp"
#include "Warning.hpp"
#include "OpenToLineHyperlink.hpp"
#include "characters_encoding.hpp"
#include "ParseFile.hpp"
#include "ParserInterface.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "RuntimeDispatch.hpp"
#include "NelsonConfiguration.hpp"
#include "BytecodeVMInternal.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static inline std::wstring
normalizePath(const std::wstring& path);
static inline std::wstring
getBasename(const std::wstring& path);
static inline bool
filenamesMatch(const std::wstring& path1, const std::wstring& path2);
// exact match of two breakpoint file names (slashes normalized, case kept on POSIX)
static inline bool
sameBreakpointFile(const std::wstring& path1, const std::wstring& path2);
static std::wstring
readSourceLine(const std::wstring& filename, size_t line);
#define BREAKPOINT_CONDITION_TEMP_VARIABLE "__nelson_dbcond__"
//=============================================================================
static size_t
getLinePositionFromSubtreeHelper(AbstractSyntaxTreePtr t);
//=============================================================================
void
Evaluator::resetDebugDepth()
{
    if (depth > 0) {
        context->restoreBypassedScopes();
    }
    depth = 0;
}
//=============================================================================
int
Evaluator::getDebugDepth()
{
    return depth;
}
//=============================================================================
void
Evaluator::increaseDebugDepth()
{
    depth++;
}
//=============================================================================
void
Evaluator::decreaseDebugDepth()
{
    if (depth > 0) {
        depth--;
    }
}
//=============================================================================
void
Evaluator::debugCLI()
{
    depth++;
    bool previousBpActive = bpActive;
    bpActive = true;
    if (haveEventsLoop()) {
        Nelson::processGuiEvents(false);
    }
    Interface* savedInterface = io;
    if (defaultInterface != nullptr) {
        io = defaultInterface;
    }
    debugPromptNesting++;
    try {
        evalCLI();
    } catch (...) {
        if (debugPromptNesting > 0) {
            debugPromptNesting--;
        }
        if (defaultInterface != nullptr) {
            io = savedInterface;
        }
        if (depth > 0) {
            depth--;
        }
        throw;
    }
    if (debugPromptNesting > 0) {
        debugPromptNesting--;
    }
    if (defaultInterface != nullptr) {
        io = savedInterface;
    }
    if (state == NLS_STATE_DEBUG_CONTINUE || state == NLS_STATE_DEBUG_QUIT
        || state == NLS_STATE_DEBUG_QUIT_ALL || callstack.size() <= 1) {
        bpActive = previousBpActive && callstack.size() > 1 && state != NLS_STATE_DEBUG_CONTINUE
            && state != NLS_STATE_DEBUG_QUIT && state != NLS_STATE_DEBUG_QUIT_ALL;
    }
    if (depth > 0) {
        depth--;
    }
}
//=============================================================================
size_t
Evaluator::getLinePosition(AbstractSyntaxTreePtr t)
{
    if (t == nullptr) {
        return 0;
    }

    int contextValue = t->getContext();
    // Low 16 bits of the context store the line number (1-based)
    size_t linePosition = static_cast<size_t>(contextValue & 0x0000FFFF);

    // For scripts, context might be stored differently (especially line 1 might have context 0)
    // In such cases, the entire context value represents the line number
    if (linePosition == 0 && contextValue > 0) {
        linePosition = static_cast<size_t>(contextValue);
    }
    // If both are 0, it indicates line 1 (1-based line numbering)
    else if (contextValue == 0) {
        linePosition = 1;
    }

    // Control flow statements (for, while, if, switch) carry the end line as context,
    // but breakpoints must stop at the keyword line.
    if (t->opNum == OP_RSTATEMENT || t->opNum == OP_QSTATEMENT) {
        if (t->down != nullptr && t->down->type == reserved_node) {
            if (t->down->tokenNumber == NLS_KEYWORD_FOR || t->down->tokenNumber == NLS_KEYWORD_WHILE
                || t->down->tokenNumber == NLS_KEYWORD_IF
                || t->down->tokenNumber == NLS_KEYWORD_SWITCH) {
                if (t->down->down != nullptr) {
                    size_t childLine = getLinePositionFromSubtreeHelper(t->down->down);
                    return childLine;
                }
            }
        }
    }

    return linePosition;
}
//=============================================================================
size_t
Evaluator::maxLinePosition(AbstractSyntaxTreePtr t)
{
    if (t == nullptr) {
        return 0;
    }

    size_t maxLine = getLinePosition(t);

    std::function<void(AbstractSyntaxTreePtr)> traverse = [&](AbstractSyntaxTreePtr node) {
        if (node == nullptr) {
            return;
        }

        size_t currentLine = getLinePosition(node);
        if (currentLine > maxLine) {
            maxLine = currentLine;
        }

        if (node->down != nullptr) {
            traverse(node->down);
        }

        if (node->right != nullptr) {
            traverse(node->right);
        }
    };

    traverse(t);

    return maxLine;
}
//=============================================================================
void
Evaluator::noteBreakpointsChanged()
{
    breakpointsRevision.fetch_add(1, std::memory_order_relaxed);
    refreshEnabledBreakpointsFlag();
}
//=============================================================================
void
Evaluator::refreshEnabledBreakpointsFlag()
{
    bool anyEnabled = false;
    for (const auto& bp : breakpoints) {
        if (bp.enabled) {
            anyEnabled = true;
            break;
        }
    }
    if (anyEnabled) {
        statementHookFlags.fetch_or(STATEMENT_HOOK_BREAKPOINTS, std::memory_order_relaxed);
    } else {
        statementHookFlags.fetch_and(~STATEMENT_HOOK_BREAKPOINTS, std::memory_order_relaxed);
    }
}
//=============================================================================
void
Evaluator::noteDebugStateChanged()
{
    debugStateRevision.fetch_add(1, std::memory_order_relaxed);
}
//=============================================================================
Breakpoint*
Evaluator::findMutableBreakpointById(uint64_t id)
{
    if (id == 0) {
        return nullptr;
    }
    for (auto& bp : breakpoints) {
        if (bp.id == id) {
            return &bp;
        }
    }
    return nullptr;
}
//=============================================================================
Breakpoint*
Evaluator::findMutableUserBreakpoint(const std::wstring& filename, size_t line)
{
    for (auto& bp : breakpoints) {
        if (bp.isUserBreakpoint() && bp.line == line && sameBreakpointFile(bp.filename, filename)) {
            return &bp;
        }
    }
    return nullptr;
}
//=============================================================================
uint64_t
Evaluator::addBreakpoint(const Breakpoint& bp)
{
    Breakpoint stored = bp;
    if (stored.isUserBreakpoint()) {
        Breakpoint* existing = findMutableUserBreakpoint(stored.filename, stored.line);
        if (existing != nullptr) {
            // a new dbstop on the same line replaces the condition and re-arms it
            existing->condition = stored.condition;
            existing->enabled = true;
            if (!stored.functionName.empty()) {
                existing->functionName = stored.functionName;
            }
            noteBreakpointsChanged();
            return existing->id;
        }
    }
    stored.id = nextBreakpointId++;
    breakpoints.push_back(stored);
    noteBreakpointsChanged();
    return stored.id;
}
//=============================================================================
void
Evaluator::clearBreakpoints()
{
    breakpoints.clear();
    stopOnError = false;
    stopOnCaughtError = false;
    noteBreakpointsChanged();
}
//=============================================================================
void
Evaluator::clearStepBreakpoints()
{
    if (breakpoints.empty()) {
        return;
    }
    bool changed = false;
    auto it = breakpoints.begin();
    while (it != breakpoints.end()) {
        if (it->stepMode) {
            it = breakpoints.erase(it);
            changed = true;
        } else {
            ++it;
        }
    }
    if (changed) {
        noteBreakpointsChanged();
    }
}
//=============================================================================
void
Evaluator::clearRunToLineBreakpoints()
{
    if (breakpoints.empty()) {
        return;
    }
    bool changed = false;
    auto it = breakpoints.begin();
    while (it != breakpoints.end()) {
        if (it->runToLine) {
            it = breakpoints.erase(it);
            changed = true;
        } else {
            ++it;
        }
    }
    if (changed) {
        noteBreakpointsChanged();
    }
}
//=============================================================================
bool
Evaluator::removeBreakpoint(const std::wstring& filename, size_t line)
{
    for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
        if (it->isUserBreakpoint() && it->line == line
            && sameBreakpointFile(it->filename, filename)) {
            breakpoints.erase(it);
            noteBreakpointsChanged();
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
Evaluator::removeBreakpointById(uint64_t id)
{
    for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
        if (it->id == id) {
            breakpoints.erase(it);
            noteBreakpointsChanged();
            return true;
        }
    }
    return false;
}
//=============================================================================
size_t
Evaluator::removeAllBreakpoints(const std::wstring& filename)
{
    size_t removed = 0;
    auto it = breakpoints.begin();
    while (it != breakpoints.end()) {
        if (it->isUserBreakpoint()
            && (filename.empty() || sameBreakpointFile(it->filename, filename))) {
            it = breakpoints.erase(it);
            removed++;
        } else {
            ++it;
        }
    }
    if (removed > 0) {
        noteBreakpointsChanged();
    }
    return removed;
}
//=============================================================================
std::vector<Breakpoint>
Evaluator::getBreakpoints() const
{
    return breakpoints;
}
//=============================================================================
std::vector<Breakpoint>
Evaluator::getUserBreakpoints(const std::wstring& filename) const
{
    std::vector<Breakpoint> result;
    for (const auto& bp : breakpoints) {
        if (bp.isUserBreakpoint()
            && (filename.empty() || sameBreakpointFile(bp.filename, filename))) {
            result.push_back(bp);
        }
    }
    std::stable_sort(result.begin(), result.end(), [](const Breakpoint& a, const Breakpoint& b) {
        if (a.filename != b.filename) {
            return a.filename < b.filename;
        }
        return a.line < b.line;
    });
    return result;
}
//=============================================================================
std::vector<size_t>
Evaluator::getBreakpointLines(const std::wstring& filename) const
{
    std::vector<size_t> lines;
    for (const auto& bp : breakpoints) {
        if (bp.isUserBreakpoint() && sameBreakpointFile(bp.filename, filename)) {
            lines.push_back(bp.line);
        }
    }
    std::sort(lines.begin(), lines.end());
    return lines;
}
//=============================================================================
const Breakpoint*
Evaluator::findBreakpoint(const std::wstring& filename, size_t line) const
{
    for (const auto& bp : breakpoints) {
        if (bp.isUserBreakpoint() && bp.line == line && sameBreakpointFile(bp.filename, filename)) {
            return &bp;
        }
    }
    return nullptr;
}
//=============================================================================
const Breakpoint*
Evaluator::findBreakpointById(uint64_t id) const
{
    if (id == 0) {
        return nullptr;
    }
    for (const auto& bp : breakpoints) {
        if (bp.id == id) {
            return &bp;
        }
    }
    return nullptr;
}
//=============================================================================
bool
Evaluator::hasEnabledBreakpoints() const
{
    return (statementHookFlags.load(std::memory_order_relaxed) & STATEMENT_HOOK_BREAKPOINTS) != 0;
}
//=============================================================================
bool
Evaluator::hasBreakpoint(const std::wstring& filename, size_t line) const
{
    return findBreakpoint(filename, line) != nullptr;
}
//=============================================================================
bool
Evaluator::stepBreakpointExists(const Breakpoint& bp)
{
    for (const auto& breakpoint : breakpoints) {
        if ((breakpoint.filename == bp.filename) && (breakpoint.functionName == bp.functionName)
            && (breakpoint.line == bp.line) && breakpoint.stepMode) {
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
Evaluator::setBreakpointEnabled(const std::wstring& filename, size_t line, bool enabled)
{
    Breakpoint* bp = findMutableUserBreakpoint(filename, line);
    if (bp == nullptr) {
        return false;
    }
    if (bp->enabled != enabled) {
        bp->enabled = enabled;
        noteBreakpointsChanged();
    }
    return true;
}
//=============================================================================
size_t
Evaluator::setAllBreakpointsEnabled(bool enabled, const std::wstring& filename)
{
    size_t changed = 0;
    for (auto& bp : breakpoints) {
        if (bp.isUserBreakpoint() && bp.enabled != enabled
            && (filename.empty() || sameBreakpointFile(bp.filename, filename))) {
            bp.enabled = enabled;
            changed++;
        }
    }
    if (changed > 0) {
        noteBreakpointsChanged();
    }
    return changed;
}
//=============================================================================
static std::wstring
trimCondition(const std::wstring& condition)
{
    size_t first = condition.find_first_not_of(L" \t\r\n");
    if (first == std::wstring::npos) {
        return L"";
    }
    size_t last = condition.find_last_not_of(L" \t\r\n");
    return condition.substr(first, last - first + 1);
}
//=============================================================================
bool
Evaluator::validateBreakpointCondition(const std::wstring& condition, std::wstring& errorMessage)
{
    errorMessage.clear();
    std::wstring trimmed = trimCondition(condition);
    if (trimmed.empty()) {
        return true;
    }
    // wrapped in parentheses so that `x = 3` or `a; b` are rejected
    std::string command = std::string(BREAKPOINT_CONDITION_TEMP_VARIABLE) + " = ("
        + wstring_to_utf8(trimmed) + ");\n";
    NelsonException previousException(getLastErrorException());
    ParserState parserState = ParseError;
    AbstractSyntaxTree::clearReferences();
    try {
        parserState = parseString(lexerContext, command);
    } catch (const NelsonException&) {
        parserState = ParseError;
    }
    AbstractSyntaxTree::deleteReferences();
    resetParser();
    setLastErrorException(previousException);
    if (parserState != ScriptBlock) {
        errorMessage
            = formatI18nMessage(resolveMessageTemplate(L"Nelson:dbstop:conditionSyntaxError",
                                    L"The condition '%s' contains a syntax error."),
                trimmed);
        return false;
    }
    return true;
}
//=============================================================================
bool
Evaluator::setBreakpointCondition(const std::wstring& filename, size_t line,
    const std::wstring& condition, std::wstring& errorMessage)
{
    if (!validateBreakpointCondition(condition, errorMessage)) {
        return false;
    }
    Breakpoint* bp = findMutableUserBreakpoint(filename, line);
    if (bp == nullptr) {
        return false;
    }
    bp->condition = trimCondition(condition);
    noteBreakpointsChanged();
    return true;
}
//=============================================================================
bool
Evaluator::moveBreakpoint(const std::wstring& filename, size_t oldLine, size_t newLine)
{
    if (oldLine == newLine) {
        return findMutableUserBreakpoint(filename, oldLine) != nullptr;
    }
    Breakpoint* bp = findMutableUserBreakpoint(filename, oldLine);
    if (bp == nullptr) {
        return false;
    }
    Breakpoint* target = findMutableUserBreakpoint(filename, newLine);
    if (target != nullptr) {
        // the destination already has one: keep it, drop the moved breakpoint
        removeBreakpointById(bp->id);
        return true;
    }
    bp->line = newLine;
    noteBreakpointsChanged();
    return true;
}
//=============================================================================
uint64_t
Evaluator::addRunToLineBreakpoint(
    const std::wstring& filename, size_t line, std::wstring& errorMessage)
{
    errorMessage.clear();
    size_t adjustedLine = line;
    if (!adjustBreakpointLine(filename, line, adjustedLine, errorMessage)) {
        return 0;
    }
    const Breakpoint* existing = findBreakpoint(filename, adjustedLine);
    if (existing != nullptr && existing->enabled && existing->condition.empty()) {
        return existing->id;
    }
    Breakpoint bp;
    bp.filename = filename;
    bp.functionName = "";
    bp.line = adjustedLine;
    bp.enabled = true;
    bp.runToLine = true;
    bp.id = nextBreakpointId++;
    breakpoints.push_back(bp);
    noteBreakpointsChanged();
    return bp.id;
}
//=============================================================================
void
Evaluator::setStopOnError(bool on)
{
    if (stopOnError != on) {
        stopOnError = on;
        noteBreakpointsChanged();
    }
}
//=============================================================================
bool
Evaluator::getStopOnError() const
{
    return stopOnError;
}
//=============================================================================
void
Evaluator::setStopOnCaughtError(bool on)
{
    if (stopOnCaughtError != on) {
        stopOnCaughtError = on;
        noteBreakpointsChanged();
    }
}
//=============================================================================
bool
Evaluator::getStopOnCaughtError() const
{
    return stopOnCaughtError;
}
//=============================================================================
bool
Evaluator::hasErrorBreakpoints() const
{
    return stopOnError || stopOnCaughtError;
}
//=============================================================================
bool
Evaluator::isAtPrompt() const
{
    Interface* itf = defaultInterface != nullptr ? defaultInterface : io;
    return itf != nullptr && itf->isAtPrompt();
}
//=============================================================================
bool
Evaluator::requestPause()
{
    if (isAtPrompt()) {
        return false;
    }
    statementHookFlags.fetch_or(STATEMENT_HOOK_PAUSE, std::memory_order_relaxed);
    return true;
}
//=============================================================================
void
Evaluator::cancelPauseRequest()
{
    statementHookFlags.fetch_and(~STATEMENT_HOOK_PAUSE, std::memory_order_relaxed);
}
//=============================================================================
bool
Evaluator::isPauseRequested() const
{
    return (statementHookFlags.load(std::memory_order_relaxed) & STATEMENT_HOOK_PAUSE) != 0;
}
//=============================================================================
const DebugStopInfo&
Evaluator::getDebugStopInfo() const
{
    return debugStopInfo;
}
//=============================================================================
uint64_t
Evaluator::getBreakpointsRevision() const
{
    return breakpointsRevision.load(std::memory_order_relaxed);
}
//=============================================================================
uint64_t
Evaluator::getDebugStateRevision() const
{
    return debugStateRevision.load(std::memory_order_relaxed);
}
//=============================================================================
bool
Evaluator::onBytecodeBreakpoint(const std::wstring& bytecodeFilename,
    const std::string& bytecodeFunctionName, size_t currentLine, size_t maxLine)
{
    if (debugHooksSuppressDepth > 0) {
        return false;
    }
    if (breakpoints.empty() && !bpActive && !isPauseRequested()) {
        return false;
    }
    // a command typed at the K>> prompt (no source file) never pauses itself
    if (bytecodeFilename.empty() && debugPromptNesting > 0) {
        return false;
    }
    pendingStop = PendingDebugStop();
    pendingStop.previousStepBreakpoint = stepBreakpoint;
    std::wstring filename;
    std::string currentFunctionName;
    resolveBytecodeBreakpointContext(bytecodeFilename, bytecodeFunctionName, currentLine, filename,
        currentFunctionName, maxLine);
    if (handleActiveStepNextBreakpoint(filename, currentFunctionName, currentLine, maxLine)) {
        return true;
    }
    if (handleQueuedStepNextBreakpoint(filename, currentFunctionName, currentLine, maxLine)) {
        return true;
    }
    if (handleStepOutBreakpoint(filename, currentFunctionName, currentLine, maxLine)) {
        return true;
    }
    if (handlePauseRequest(filename, currentFunctionName, currentLine, maxLine)) {
        return true;
    }
    if (handleLineBreakpoint(filename, currentFunctionName, currentLine, maxLine, true)) {
        return true;
    }
    if (handleLineBreakpoint(filename, currentFunctionName, currentLine, maxLine, false)) {
        return true;
    }
    return handleStepIntoBreakpoint(filename, currentFunctionName, currentLine, maxLine);
}
//=============================================================================
bool
Evaluator::handlePauseRequest(const std::wstring& filename, const std::string& currentFunctionName,
    size_t currentLine, size_t maxLine)
{
    if (!isPauseRequested()) {
        return false;
    }
    cancelPauseRequest();
    Breakpoint stop;
    stop.filename = filename;
    stop.functionName = currentFunctionName;
    stop.line = currentLine;
    stop.maxLines = maxLine;
    stop.enabled = true;
    stepBreakpoint = stop;
    pendingStop.reason = DebugStopReason::Pause;
    return true;
}
//=============================================================================
bool
Evaluator::confirmPendingDebugStop(size_t column)
{
    PendingDebugStop pending = pendingStop;
    pendingStop = PendingDebugStop();
    if (pending.reason == DebugStopReason::Breakpoint) {
        Breakpoint* bp = findMutableBreakpointById(pending.breakpointId);
        if (bp == nullptr) {
            stepBreakpoint = pending.previousStepBreakpoint;
            return false;
        }
        if (!bp->condition.empty()) {
            // the table may change while the condition runs: keep a copy
            std::wstring condition = bp->condition;
            BreakpointConditionOutcome outcome = evaluateBreakpointCondition(condition);
            if (outcome == BreakpointConditionOutcome::False) {
                stepBreakpoint = pending.previousStepBreakpoint;
                return false;
            }
            // NonScalar and Error stop anyway, the warning was already emitted
        }
    } else if (pending.reason == DebugStopReason::RunToLine) {
        removeBreakpointById(pending.breakpointId);
    }
    fillDebugStopInfoFromStepBreakpoint(pending.reason, pending.breakpointId, column);
    return true;
}
//=============================================================================
void
Evaluator::fillDebugStopInfoFromStepBreakpoint(
    DebugStopReason reason, uint64_t breakpointId, size_t column)
{
    debugStopInfo = DebugStopInfo();
    debugStopInfo.reason = reason;
    debugStopInfo.breakpointId = breakpointId;
    debugStopInfo.column = column;
    debugStopInfo.canStep = true;
    if (stepBreakpoint.has_value()) {
        debugStopInfo.filename = stepBreakpoint->filename;
        debugStopInfo.functionName = stepBreakpoint->functionName;
        debugStopInfo.line = stepBreakpoint->line;
        debugStopInfo.maxLine = stepBreakpoint->maxLines;
    }
    noteDebugStateChanged();
}
//=============================================================================
BreakpointConditionOutcome
Evaluator::evaluateBreakpointCondition(const std::wstring& condition)
{
    DebugHooksSuppressor suppressor(this);
    NelsonException savedLastError(getLastErrorException());
    NelsonConfiguration* config = NelsonConfiguration::getInstance();
    bool interruptWasPending = config->getInterruptPending(ID);
    const std::string temporaryName(BREAKPOINT_CONDITION_TEMP_VARIABLE);
    auto restore = [&]() {
        context->deleteVariable(temporaryName);
        setLastErrorException(savedLastError);
        if (interruptWasPending) {
            config->setInterruptPending(true, ID);
        }
    };
    try {
        std::wstring command = utf8_to_wstring(temporaryName) + L" = (" + condition + L");";
        evaluateString(command, true);
        ArrayOf value;
        bool found = context->lookupVariableLocally(temporaryName, value);
        if (!found) {
            restore();
            Warning(L"Nelson:debugger:breakpointConditionError",
                L"Breakpoint expression '%s' resulted in an error. The error was %s", condition,
                resolveMessageTemplate(
                    L"Nelson:interpreter:validScript", L"a valid script expected."));
            return BreakpointConditionOutcome::Error;
        }
        if (!value.isScalar()) {
            restore();
            Warning(L"Nelson:debugger:breakpointConditionNonScalar",
                L"Breakpoint expression '%s' resulted in a non-scalar value.", condition);
            return BreakpointConditionOutcome::NonScalar;
        }
        bool result = bcvm_detail::conditionAsBoolean(this, value);
        restore();
        return result ? BreakpointConditionOutcome::True : BreakpointConditionOutcome::False;
    } catch (const NelsonException& e) {
        if (state == NLS_STATE_ABORT || isQuitOrForceQuitState()) {
            context->deleteVariable(temporaryName);
            throw;
        }
        std::wstring message = e.message();
        restore();
        Warning(L"Nelson:debugger:breakpointConditionError",
            L"Breakpoint expression '%s' resulted in an error. The error was %s", condition,
            message);
        return BreakpointConditionOutcome::Error;
    }
}
//=============================================================================
void
Evaluator::onBytecodeKeyboard(const std::wstring& bytecodeFilename,
    const std::string& bytecodeFunctionName, size_t currentLine, size_t maxLine)
{
    Breakpoint stop;
    resolveBytecodeBreakpointContext(bytecodeFilename, bytecodeFunctionName, currentLine,
        stop.filename, stop.functionName, maxLine);
    stop.line = currentLine;
    stop.maxLines = maxLine;
    stop.enabled = true;
    stop.stepMode = false;
    stepBreakpoint = stop;
    fillDebugStopInfoFromStepBreakpoint(DebugStopReason::Keyboard, 0, 0);
}
//=============================================================================
bool
Evaluator::shouldStopOnError(bool caughtInCurrentFrame, bool& willBeCaught)
{
    willBeCaught = caughtInCurrentFrame || vmTryDepth > 0 || cppCatchDepth > 0;
    if (!(stopOnError || stopOnCaughtError)) {
        return false;
    }
    if (errorStopTaken || debugHooksSuppressDepth > 0 || debugPromptNesting > 0) {
        return false;
    }
    bool stop = willBeCaught ? stopOnCaughtError : stopOnError;
    if (stop) {
        errorStopTaken = true;
    }
    return stop;
}
//=============================================================================
void
Evaluator::markErrorDisplayed()
{
    errorAlreadyDisplayed = true;
}
//=============================================================================
bool
Evaluator::consumeErrorDisplayed()
{
    bool displayed = errorAlreadyDisplayed;
    errorAlreadyDisplayed = false;
    return displayed;
}
//=============================================================================
void
Evaluator::restoreDebugScopes(size_t bypassedScopeCountAtEntry, int depthAtEntry)
{
    if (context != nullptr) {
        size_t current = context->getBypassedScopeCount();
        if (current > bypassedScopeCountAtEntry) {
            context->restoreBypassedScopes(static_cast<int>(current - bypassedScopeCountAtEntry));
        }
    }
    if (depthAtEntry >= 0) {
        depth = depthAtEntry;
    }
}
//=============================================================================
void
Evaluator::beginErrorStop(const NelsonException& e, const std::wstring& fallbackFilename,
    const std::string& fallbackFunctionName, size_t fallbackLine)
{
    Breakpoint stop;
    stop.filename = fallbackFilename;
    stop.functionName = fallbackFunctionName;
    stop.line = fallbackLine;
    stop.maxLines = fallbackLine;
    stop.enabled = true;
    const std::vector<PositionScript>& trace = e.trace();
    if (!trace.empty()) {
        // innermost position first
        const PositionScript& position = trace.front();
        if (!position.getFilename().empty()) {
            stop.filename = position.getFilename();
        }
        if (!position.getFunctionName().empty()) {
            stop.functionName = wstring_to_utf8(position.getFunctionName());
        }
        if (position.getLine() > 0) {
            stop.line = static_cast<size_t>(position.getLine());
            stop.maxLines = stop.line;
        }
    }
    stepBreakpoint = stop;
    fillDebugStopInfoFromStepBreakpoint(DebugStopReason::Error, 0, 0);
    debugStopInfo.errorIdentifier = e.identifier();
    debugStopInfo.errorMessage = e.message();
    debugStopInfo.canStep = false;
    noteDebugStateChanged();
}
//=============================================================================
void
Evaluator::clearErrorStopTaken()
{
    errorStopTaken = false;
}
//=============================================================================
int&
Evaluator::vmTryDepthRef()
{
    return vmTryDepth;
}
//=============================================================================
void
Evaluator::pushCppErrorCatcher()
{
    cppCatchDepth++;
}
//=============================================================================
void
Evaluator::popCppErrorCatcher()
{
    if (cppCatchDepth > 0) {
        cppCatchDepth--;
    }
    // the error was swallowed by a C++ catcher: a later one may stop again
    errorStopTaken = false;
}
//=============================================================================
void
Evaluator::pushDebugHooksSuppression()
{
    debugHooksSuppressDepth++;
}
//=============================================================================
void
Evaluator::popDebugHooksSuppression()
{
    if (debugHooksSuppressDepth > 0) {
        debugHooksSuppressDepth--;
    }
}
//=============================================================================
bool
Evaluator::debugHooksSuppressed() const
{
    return debugHooksSuppressDepth > 0;
}
//=============================================================================
void
Evaluator::pushDebugPromptCommand()
{
    debugPromptNesting++;
}
//=============================================================================
void
Evaluator::popDebugPromptCommand()
{
    if (debugPromptNesting > 0) {
        debugPromptNesting--;
    }
}
//=============================================================================
void
Evaluator::onExecutionReturnedToTopLevel()
{
    setNextDebugStopQuiet(false);
    stepMode = false;
    stepBreakpoint.reset();
    clearStepBreakpoints();
    clearRunToLineBreakpoints();
    cancelPauseRequest();
    errorStopTaken = false;
    errorAlreadyDisplayed = false;
    pendingStop = PendingDebugStop();
    if (debugStopInfo.reason != DebugStopReason::None) {
        debugStopInfo = DebugStopInfo();
        noteDebugStateChanged();
    }
}
//=============================================================================
void
Evaluator::markDebugResumed()
{
    if (debugStopInfo.reason != DebugStopReason::None) {
        debugStopInfo.reason = DebugStopReason::None;
        debugStopInfo.canStep = true;
        noteDebugStateChanged();
    }
}
//=============================================================================
void
Evaluator::displayDebugStopLine()
{
    if (takeNextDebugStopQuiet()) {
        return;
    }
    if (debugStopInfo.reason == DebugStopReason::None
        || debugStopInfo.reason == DebugStopReason::Keyboard || debugStopInfo.line == 0
        || debugStopInfo.filename.empty()) {
        return;
    }
    Interface* itf = defaultInterface != nullptr ? defaultInterface : io;
    if (itf == nullptr) {
        return;
    }
    std::wstring source = readSourceLine(debugStopInfo.filename, debugStopInfo.line);
    if (source.empty()) {
        return;
    }
    // layout: the line number left-aligned in a 4 character field
    std::wstring number = std::to_wstring(debugStopInfo.line);
    std::wstring padding = number.size() < 4 ? std::wstring(4 - number.size(), L' ') : L" ";
    // the number is a link in the desktop consoles: a click opens the editor at that line
    std::wstring label = itf->supportsHyperlinks()
        ? openToLineHyperlink(number, debugStopInfo.filename, static_cast<int>(debugStopInfo.line))
        : number;
    itf->outputMessage(label + padding + source + L"\n");
}
//=============================================================================
void
Evaluator::resolveBytecodeBreakpointContext(const std::wstring& bytecodeFilename,
    const std::string& bytecodeFunctionName, size_t currentLine, std::wstring& filename,
    std::string& currentFunctionName, size_t& maxLine)
{
    currentFunctionName = bytecodeFunctionName;
    if (currentFunctionName.empty() && context != nullptr
        && context->getCurrentScope() != nullptr) {
        currentFunctionName = context->getCurrentScope()->getName();
    }
    filename = bytecodeFilename;
    if (filename.empty() && context != nullptr && context->getCurrentScope() != nullptr) {
        filename = context->getCurrentScope()->getFilename();
    }
    if (filename.empty()) {
        filename = utf8_to_wstring(callstack.getLastContext());
    }
    if (maxLine == 0) {
        maxLine = currentLine;
    }
}
//=============================================================================
bool
Evaluator::handleActiveStepNextBreakpoint(const std::wstring& filename,
    const std::string& currentFunctionName, size_t currentLine, size_t maxLine)
{
    if (!(bpActive && stepMode && stepBreakpoint.has_value() && stepBreakpoint->stepNext)) {
        return false;
    }
    const Breakpoint& sb = *stepBreakpoint;
    if (sb.filename.empty() || !filenamesMatch(sb.filename, filename)) {
        return false;
    }
    bool inSameFunction = !sb.functionName.empty() && sb.functionName == currentFunctionName;
    bool shouldBreak = sb.stepInto || inSameFunction;
    if (!(shouldBreak && sb.fromLine != 0 && currentLine != sb.fromLine)) {
        return false;
    }
    Breakpoint matched = sb;
    matched.line = currentLine;
    matched.maxLines = maxLine;
    matched.stepMode = false;
    matched.stepNext = false;
    stepBreakpoint = matched;
    bool changed = false;
    auto tmpIt = breakpoints.begin();
    while (tmpIt != breakpoints.end()) {
        if (tmpIt->stepMode && tmpIt->stepNext) {
            tmpIt = breakpoints.erase(tmpIt);
            changed = true;
        } else {
            ++tmpIt;
        }
    }
    if (changed) {
        noteBreakpointsChanged();
    }
    pendingStop.reason = DebugStopReason::Step;
    return true;
}
//=============================================================================
bool
Evaluator::handleQueuedStepNextBreakpoint(const std::wstring& filename,
    const std::string& currentFunctionName, size_t currentLine, size_t maxLine)
{
    for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
        if (!(it->stepMode && it->stepNext) || !filenamesMatch(it->filename, filename)) {
            continue;
        }
        bool inSameFunction = !it->functionName.empty() && it->functionName == currentFunctionName;
        bool shouldBreak = it->stepInto || inSameFunction;
        if (shouldBreak && it->fromLine != 0 && currentLine != it->fromLine) {
            Breakpoint matchedBp;
            matchedBp.filename = filename;
            matchedBp.functionName = currentFunctionName;
            matchedBp.line = currentLine;
            matchedBp.maxLines = maxLine;
            matchedBp.targetDepth = it->targetDepth;
            matchedBp.enabled = true;
            matchedBp.stepMode = false;
            matchedBp.stepNext = true;
            matchedBp.fromLine = it->fromLine;
            stepBreakpoint = matchedBp;
            breakpoints.erase(it);
            noteBreakpointsChanged();
            pendingStop.reason = DebugStopReason::Step;
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
Evaluator::handleStepOutBreakpoint(const std::wstring& filename,
    const std::string& currentFunctionName, size_t currentLine, size_t maxLine)
{
    for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
        if (!(it->stepMode && it->stepOut)) {
            continue;
        }
        bool leftFunction = it->functionName != currentFunctionName;
        bool atOrBelowTargetDepth
            = (it->targetDepth < 0) || (static_cast<int>(callstack.size()) <= it->targetDepth);
        if (leftFunction && atOrBelowTargetDepth) {
            Breakpoint matchedBp;
            matchedBp.filename = filename;
            matchedBp.functionName = currentFunctionName;
            matchedBp.line = currentLine;
            matchedBp.maxLines = maxLine;
            matchedBp.enabled = true;
            matchedBp.stepMode = false;
            stepBreakpoint = matchedBp;
            breakpoints.erase(it);
            noteBreakpointsChanged();
            pendingStop.reason = DebugStopReason::Step;
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
Evaluator::handleLineBreakpoint(const std::wstring& filename,
    const std::string& currentFunctionName, size_t currentLine, size_t maxLine,
    bool requireFunctionName)
{
    for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
        if (it->stepOut || (!requireFunctionName && it->stepNext) || !it->enabled) {
            continue;
        }
        bool lineMatch = it->stepMode ? currentLine >= it->line : currentLine == it->line;
        // a user breakpoint belongs to a file line: any function owning that line matches
        bool functionMatch = requireFunctionName
            ? (it->functionName == currentFunctionName
                  || (!it->stepMode && !it->functionName.empty()))
            : it->functionName.empty();
        if (filenamesMatch(it->filename, filename) && functionMatch && lineMatch) {
            Breakpoint matchedBp = *it;
            matchedBp.maxLines = maxLine;
            if (!requireFunctionName) {
                matchedBp.functionName = "";
            }
            if (it->stepMode) {
                matchedBp.line = currentLine;
            }
            stepBreakpoint = matchedBp;
            pendingStop.breakpointId = it->id;
            if (it->stepMode) {
                pendingStop.reason = DebugStopReason::Step;
                breakpoints.erase(it);
                noteBreakpointsChanged();
            } else if (it->runToLine) {
                pendingStop.reason = DebugStopReason::RunToLine;
            } else {
                pendingStop.reason = DebugStopReason::Breakpoint;
            }
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
Evaluator::handleStepIntoBreakpoint(const std::wstring& filename,
    const std::string& currentFunctionName, size_t currentLine, size_t maxLine)
{
    for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
        if (it->stepMode && it->stepInto) {
            if (!filenamesMatch(it->filename, filename)
                || it->functionName != currentFunctionName) {
                Breakpoint matchedBp;
                matchedBp.filename = filename;
                matchedBp.functionName = currentFunctionName;
                matchedBp.line = currentLine;
                matchedBp.maxLines = maxLine;
                matchedBp.enabled = true;
                matchedBp.stepMode = false;
                stepBreakpoint = matchedBp;
                breakpoints.erase(it);
                noteBreakpointsChanged();
                pendingStop.reason = DebugStopReason::Step;
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
bool
Evaluator::dbUp(int n)
{
    if (n <= 0) {
        return false;
    }

    size_t availableScopes = context->getScopeStackSize();
    if (availableScopes <= 2) {
        // Already at base workspace (only global and base scopes)
        return false;
    }

    // Keep at least the global (index 0) and base (index 1) scopes.
    int maxBypass = static_cast<int>(availableScopes) - 2;
    int currentBypass = getDebugDepth();
    int newBypass = currentBypass + n;

    if (newBypass > maxBypass) {
        newBypass = maxBypass;
    }

    int toBypass = newBypass - currentBypass;
    if (toBypass > 0) {
        context->bypassScope(toBypass);
        depth = newBypass;
    }

    return toBypass > 0;
}
//=============================================================================
bool
Evaluator::dbDown(int n)
{
    if (n <= 0) {
        return false;
    }

    int currentBypass = getDebugDepth();
    if (currentBypass == 0) {
        // Already at the original (deepest) workspace
        return false;
    }

    int toRestore = n;
    if (toRestore > currentBypass) {
        toRestore = currentBypass;
    }

    for (int i = 0; i < toRestore; ++i) {
        context->restoreBypassedScopes(1);
    }
    depth = currentBypass - toRestore;

    return toRestore > 0;
}
//=============================================================================
inline std::wstring
normalizePath(const std::wstring& path)
{
    std::wstring normalized = path;
    std::replace(normalized.begin(), normalized.end(), L'\\', L'/');
    return normalized;
}
//=============================================================================
inline std::wstring
getBasename(const std::wstring& path)
{
    size_t lastSlash = path.find_last_of(L"/\\");
    if (lastSlash == std::wstring::npos) {
        return path;
    }
    return path.substr(lastSlash + 1);
}
//=============================================================================
inline bool
filenamesMatch(const std::wstring& path1, const std::wstring& path2)
{
    std::wstring normalized1 = normalizePath(path1);
    std::wstring normalized2 = normalizePath(path2);

    if (normalized1 == normalized2) {
        return true;
    }

    // One side may be a bare file name, so compare basenames.
    std::wstring basename1 = getBasename(normalized1);
    std::wstring basename2 = getBasename(normalized2);
    return basename1 == basename2;
}
//=============================================================================
inline bool
sameBreakpointFile(const std::wstring& path1, const std::wstring& path2)
{
    std::wstring normalized1 = normalizePath(path1);
    std::wstring normalized2 = normalizePath(path2);
#ifdef _MSC_VER
    if (normalized1.size() != normalized2.size()) {
        return false;
    }
    for (size_t k = 0; k < normalized1.size(); ++k) {
        if (std::towlower(normalized1[k]) != std::towlower(normalized2[k])) {
            return false;
        }
    }
    return true;
#else
    return normalized1 == normalized2;
#endif
}
//=============================================================================
std::wstring
readSourceLine(const std::wstring& filename, size_t line)
{
#ifdef _MSC_VER
    std::ifstream stream(filename, std::ios::binary);
#else
    std::ifstream stream(wstring_to_utf8(filename), std::ios::binary);
#endif
    if (!stream.is_open()) {
        return L"";
    }
    std::string current;
    size_t number = 0;
    while (std::getline(stream, current)) {
        number++;
        if (number == line) {
            while (!current.empty() && (current.back() == '\r' || current.back() == '\n')) {
                current.pop_back();
            }
            return utf8_to_wstring(current);
        }
    }
    return L"";
}
//=============================================================================
size_t
getLinePositionFromSubtreeHelper(AbstractSyntaxTreePtr t)
{
    if (t == nullptr) {
        return 0;
    }

    int contextValue = t->getContext();
    size_t currentLine = static_cast<size_t>(contextValue & 0x0000FFFF);
    if (currentLine == 0 && contextValue > 0) {
        currentLine = static_cast<size_t>(contextValue);
    } else if (contextValue == 0) {
        currentLine = 1;
    }

    size_t earliestLine = currentLine;

    if (t->down != nullptr) {
        size_t downLine = getLinePositionFromSubtreeHelper(t->down);
        if (downLine > 0 && (earliestLine == 0 || downLine < earliestLine)) {
            earliestLine = downLine;
        }
    }

    if (t->right != nullptr) {
        size_t rightLine = getLinePositionFromSubtreeHelper(t->right);
        if (rightLine > 0 && (earliestLine == 0 || rightLine < earliestLine)) {
            earliestLine = rightLine;
        }
    }

    return earliestLine;
}
//=============================================================================
//=============================================================================
static std::atomic<bool> nextDebugStopQuiet { false };
//=============================================================================
void
setNextDebugStopQuiet(bool quiet)
{
    nextDebugStopQuiet.store(quiet);
}
//=============================================================================
bool
takeNextDebugStopQuiet()
{
    return nextDebugStopQuiet.exchange(false);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
