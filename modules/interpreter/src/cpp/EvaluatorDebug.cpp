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
#include <functional>
#include <cstdlib>
#include <cstdio>
#include "i18n.hpp"
#include "Evaluator.hpp"
#include "Error.hpp"
#include "MacroFunctionDef.hpp"
#include "Warning.hpp"
#include "characters_encoding.hpp"
#include "ParseFile.hpp"
#include "ParserInterface.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "RuntimeDispatch.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static inline std::wstring
normalizePath(const std::wstring& path);
static inline std::wstring
getBasename(const std::wstring& path);
static inline bool
filenamesMatch(const std::wstring& path1, const std::wstring& path2);
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
    try {
        evalCLI();
    } catch (...) {
        if (defaultInterface != nullptr) {
            io = savedInterface;
        }
        if (depth > 0) {
            depth--;
        }
        throw;
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
Evaluator::addBreakpoint(const Breakpoint& bp)
{
    // Step-mode breakpoints are temporary and unique, never treat them as duplicates
    if (bp.stepMode) {
        breakpoints.push_back(bp);
        return;
    }

    bool haveAlreadyBreak = false;
    for (auto breakpoint : breakpoints) {
        if ((breakpoint.filename == bp.filename) && (breakpoint.functionName == bp.functionName)
            && (breakpoint.line == bp.line)) {
            haveAlreadyBreak = true;
        }
    }
    if (!haveAlreadyBreak) {
        breakpoints.push_back(bp);
    }
}
//=============================================================================
void
Evaluator::clearBreakpoints()
{
    breakpoints.clear();
}
//=============================================================================
void
Evaluator::clearStepBreakpoints()
{
    if (breakpoints.empty()) {
        return;
    }
    auto it = breakpoints.begin();
    while (it != breakpoints.end()) {
        if (it->stepMode) {
            it = breakpoints.erase(it);
        } else {
            ++it;
        }
    }
}
//=============================================================================
bool
Evaluator::removeBreakpoint(const std::wstring& filename, size_t line)
{
    for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
        if ((it->filename == filename) && (it->line == line) && !it->stepMode) {
            breakpoints.erase(it);
            return true;
        }
    }
    return false;
}
//=============================================================================
std::vector<Breakpoint>
Evaluator::getBreakpoints() const
{
    return breakpoints;
}
//=============================================================================
std::vector<size_t>
Evaluator::getBreakpointLines(const std::wstring& filename) const
{
    std::vector<size_t> lines;
    for (const auto& bp : breakpoints) {
        if ((bp.filename == filename) && bp.enabled && !bp.stepMode) {
            lines.push_back(bp.line);
        }
    }
    return lines;
}
//=============================================================================
bool
Evaluator::hasEnabledBreakpoints() const
{
    for (const auto& bp : breakpoints) {
        if (bp.enabled) {
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
Evaluator::hasBreakpoint(const std::wstring& filename, size_t line) const
{
    for (const auto& bp : breakpoints) {
        if ((bp.filename == filename) && (bp.line == line) && bp.enabled && !bp.stepMode) {
            return true;
        }
    }
    return false;
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
Evaluator::onBytecodeBreakpoint(const std::wstring& bytecodeFilename,
    const std::string& bytecodeFunctionName, size_t currentLine, size_t maxLine)
{
    if (breakpoints.empty() && !bpActive) {
        return false;
    }
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
    if (handleLineBreakpoint(filename, currentFunctionName, currentLine, maxLine, true)) {
        return true;
    }
    if (handleLineBreakpoint(filename, currentFunctionName, currentLine, maxLine, false)) {
        return true;
    }
    return handleStepIntoBreakpoint(filename, currentFunctionName, currentLine, maxLine);
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
    auto tmpIt = breakpoints.begin();
    while (tmpIt != breakpoints.end()) {
        if (tmpIt->stepMode && tmpIt->stepNext) {
            tmpIt = breakpoints.erase(tmpIt);
        } else {
            ++tmpIt;
        }
    }
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
        if (it->stepOut || (!requireFunctionName && it->stepNext)) {
            continue;
        }
        bool lineMatch = it->stepMode ? currentLine >= it->line : currentLine == it->line;
        bool functionMatch = requireFunctionName ? it->functionName == currentFunctionName
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
            if (it->stepMode) {
                breakpoints.erase(it);
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
} // namespace Nelson
//=============================================================================
