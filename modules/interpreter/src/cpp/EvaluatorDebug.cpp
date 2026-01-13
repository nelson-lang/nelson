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
getMaxLineFromChainHelper(
    AbstractSyntaxTreePtr t, std::function<size_t(AbstractSyntaxTreePtr)> getLineFunc);
static size_t
getLineNumberFromCode(AbstractSyntaxTreePtr code, size_t lineNumber, size_t maxLinesInFile,
    std::function<size_t(AbstractSyntaxTreePtr)> getLineFunc);
static size_t
getLinePositionFromSubtreeHelper(AbstractSyntaxTreePtr t);
//=============================================================================
void
Evaluator::resetDebugDepth()
{
    // Restore any bypassed scopes before resetting
    if (depth > 0) {
        context->restoreBypassedScopes();
    }
    depth = 0;
}
//=============================================================================
// Returns the current debug depth.
int
Evaluator::getDebugDepth()
{
    return depth;
}
//=============================================================================
// Increases the debug depth by one.
void
Evaluator::increaseDebugDepth()
{
    depth++;
}
//=============================================================================
// Decreases the debug depth by one.
void
Evaluator::decreaseDebugDepth()
{
    depth--;
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

    // For statement nodes (OP_RSTATEMENT, OP_QSTATEMENT), check if they contain control flow
    // keywords Control flow statements (for, while, if, switch) have their context set to the end
    // line, but we want breakpoints to stop at the keyword line (start of the statement)
    if (t->opNum == OP_RSTATEMENT || t->opNum == OP_QSTATEMENT) {
        if (t->down != nullptr && t->down->type == reserved_node) {
            // Check if the child is a control flow keyword
            if (t->down->tokenNumber == NLS_KEYWORD_FOR || t->down->tokenNumber == NLS_KEYWORD_WHILE
                || t->down->tokenNumber == NLS_KEYWORD_IF
                || t->down->tokenNumber == NLS_KEYWORD_SWITCH) {
                // For control statements, get the line from the control statement's child
                if (t->down->down != nullptr) {
                    // Get the earliest line from the control statement's body/condition
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
Evaluator::getMaxLinePosition(AbstractSyntaxTreePtr t)
{
    if (t == nullptr) {
        return 0;
    }

    size_t maxLine = getLinePosition(t);

    // Recursively traverse the entire AST to find the maximum line number
    std::function<void(AbstractSyntaxTreePtr)> traverse = [&](AbstractSyntaxTreePtr node) {
        if (node == nullptr) {
            return;
        }

        size_t currentLine = getLinePosition(node);
        if (currentLine > maxLine) {
            maxLine = currentLine;
        }

        // Traverse child nodes (down pointer)
        if (node->down != nullptr) {
            traverse(node->down);
        }

        // Traverse sibling nodes (right pointer)
        if (node->right != nullptr) {
            traverse(node->right);
        }
    };

    // Start traversal from the root node to find maximum line in entire tree
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

    // For persistent breakpoints, check for duplicates
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
Evaluator::onBreakpoint(AbstractSyntaxTreePtr t)
{
    std::string currentFunctionName = context->getCurrentScope()->getName();
    int currentCallStackSize = static_cast<int>(callstack.size());
    // Fast path: if we are in step-next mode and the line changed, break immediately
    if (bpActive && stepMode && stepBreakpoint.has_value() && stepBreakpoint->stepNext) {
        const Breakpoint& sb = *stepBreakpoint;
        std::wstring sbFile = sb.filename;
        if (!sbFile.empty() && filenamesMatch(sbFile, context->getCurrentScope()->getFilename())) {
            std::string currentFunction = context->getCurrentScope()->getName();
            // For step-over: ONLY break in the same function (no depth check to avoid confusion)
            bool inSameFunction = !sb.functionName.empty() && sb.functionName == currentFunction;
            bool shouldBreak = sb.stepInto || inSameFunction;

            if (!shouldBreak) {
                if (std::getenv("NELSON_DEBUG_STEP_TRACE")) {
                    std::printf("[stepNext-fast] skip currentFn=%s bpFn=%s depth=%d target=%d\n",
                        currentFunction.c_str(), sb.functionName.c_str(), currentCallStackSize,
                        sb.targetDepth);
                }
                goto skip_fast_path;
            }
            size_t currentLineFast = getLinePosition(t);
            // Skip if fromLine is 0 (uninitialized/invalid) to avoid matching line 1
            if (sb.fromLine != 0 && currentLineFast != sb.fromLine) {
                if (std::getenv("NELSON_DEBUG_STEP_TRACE")) {
                    std::printf("[stepNext-fast] hit file=%s currentFn=%s bpFn=%s from=%zu at=%zu "
                                "depth=%d targetDepth=%d\n",
                        wstring_to_utf8(sbFile).c_str(), currentFunction.c_str(),
                        sb.functionName.c_str(), sb.fromLine, currentLineFast, currentCallStackSize,
                        sb.targetDepth);
                }
                Breakpoint matched = sb;
                matched.line = currentLineFast;
                matched.stepMode = false;
                matched.stepNext = false; // disable further fast-path hits until user steps again
                stepBreakpoint = matched;
                // Remove any pending step-next temporary breakpoints now that we've hit one
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
        }
    }
skip_fast_path:

    if (breakpoints.empty() && !bpActive) {
        return false;
    }

    size_t currentLine = getLinePosition(t);
    size_t maxLine = getMaxLinePosition(t);

    std::wstring filename;
    filename = context->getCurrentScope()->getFilename();
    if (filename.empty()) {
        filename = utf8_to_wstring(callstack.getLastContext());
    }

    // Debug: log when we have breakpoints but they don't match
    if (!breakpoints.empty()) {
        std::printf("[DEBUG-BP] At line=%zu in fn='%s', file='%s'\n", currentLine,
            currentFunctionName.c_str(), wstring_to_utf8(filename).c_str());
        for (size_t i = 0; i < breakpoints.size(); ++i) {
            const auto& bp = breakpoints[i];
            bool fnMatch = (bp.functionName == currentFunctionName);
            bool fileMatch = filenamesMatch(bp.filename, filename);
            bool lineMatch = (bp.line == currentLine);
            const char* fnStr = fnMatch ? "Y" : "N";
            const char* lineStr = lineMatch ? "Y" : "N";
            const char* fileStr = fileMatch ? "Y" : "N";
            std::printf("  [%zu] fn='%s'(%s) line=%zu(%s) fileMatch=%s stepMode=%d\n", i,
                bp.functionName.c_str(), fnStr, bp.line, lineStr, fileStr, bp.stepMode);
        }
        std::fflush(stdout);
    }

    // Note: Step-out breakpoints are handled separately in checkStepOutAfterFunctionReturn()
    // which is called after function calls complete, allowing us to stop at the call site.

    // First: handle step-next breakpoints (break at first executed line after 'fromLine')
    for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
        if (!(it->stepMode && it->stepNext)) {
            continue;
        }

        // Step-next should be file-scoped; optionally also stay in the same function unless
        // stepInto
        if (!filenamesMatch(it->filename, filename)) {
            continue;
        }
        // For step-over: ONLY break in the same function (no depth check to avoid confusion)
        bool inSameFunction = !it->functionName.empty() && it->functionName == currentFunctionName;
        bool shouldBreak = it->stepInto || inSameFunction;

        if (!shouldBreak) {
            if (std::getenv("NELSON_DEBUG_STEP_TRACE")) {
                std::printf("[stepNext] skip currentFn=%s bpFn=%s depth=%d target=%d\n",
                    currentFunctionName.c_str(), it->functionName.c_str(), currentCallStackSize,
                    it->targetDepth);
            }
            continue;
        }

        // Break on any line different from the origin. This covers forward steps and loop backs.
        // Skip if fromLine is 0 (uninitialized/invalid) to avoid matching line 1
        if (it->fromLine != 0 && currentLine != it->fromLine) {
            if (std::getenv("NELSON_DEBUG_STEP_TRACE")) {
                std::printf("[stepNext] hit file=%s currentFn=%s bpFn=%s from=%zu at=%zu depth=%d "
                            "targetDepth=%d\n",
                    wstring_to_utf8(filename).c_str(), currentFunctionName.c_str(),
                    it->functionName.c_str(), it->fromLine, currentLine, currentCallStackSize,
                    it->targetDepth);
            }
            Breakpoint matchedBp;
            matchedBp.filename = filename;
            matchedBp.functionName = currentFunctionName; // carry current context for dbstack
            matchedBp.line = currentLine;
            matchedBp.maxLines = maxLine;
            matchedBp.targetDepth = it->targetDepth;
            matchedBp.enabled = true;
            matchedBp.stepMode = false;
            matchedBp.stepNext = true; // preserve step-next intent for fast path
            matchedBp.fromLine = it->fromLine;
            stepBreakpoint = matchedBp;
            breakpoints.erase(it);
            return true;
        }
    }

    // Check for regular and step-in breakpoints (exact line match)
    for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
        // Skip step-out breakpoints - they are handled after function returns
        if (it->stepOut) {
            continue;
        }
        bool lineMatch = it->line == currentLine;
        if (it->stepMode) {
            // For step breakpoints, allow triggering when currentLine has advanced past the
            // recorded line (helps for control-flow statements where getLinePosition returns the
            // earliest child line).
            lineMatch = currentLine >= it->line;
        }
        if (std::getenv("NELSON_DEBUG_BREAKPOINTS")) {
            std::printf("    Checking non-empty fn bp: fileMatch=%d fnMatch=%d lineMatch=%d\n",
                filenamesMatch(it->filename, filename), (it->functionName == currentFunctionName),
                lineMatch);
            std::fflush(stdout);
        }
        if (filenamesMatch(it->filename, filename) && (it->functionName == currentFunctionName)
            && lineMatch) {
            // Store the breakpoint info
            Breakpoint matchedBp = *it;
            matchedBp.maxLines = maxLine;
            // For step-mode breakpoints, update to the actual current line
            if (it->stepMode) {
                matchedBp.line = currentLine;
            }
            stepBreakpoint = matchedBp;
            // Remove step-mode breakpoints after they trigger (they are temporary)
            if (it->stepMode) {
                breakpoints.erase(it);
            }
            return true;
        }
    }

    // Try with empty function name (global scope / scripts)
    for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
        // Skip step-out breakpoints
        if (it->stepOut) {
            continue;
        }
        // Skip stepNext breakpoints - they should have been handled above and have
        // explicit function name filtering to prevent stepping into other functions
        if (it->stepNext) {
            continue;
        }
        bool lineMatch = it->line == currentLine;
        if (it->stepMode) {
            lineMatch = currentLine >= it->line;
        }
        if (std::getenv("NELSON_DEBUG_BREAKPOINTS")) {
            std::printf("    Checking empty fn bp: fileMatch=%d emptyFn=%d lineMatch=%d\n",
                filenamesMatch(it->filename, filename), it->functionName.empty(), lineMatch);
            std::fflush(stdout);
        }
        if (filenamesMatch(it->filename, filename) && (it->functionName.empty()) && lineMatch) {
            if (std::getenv("NELSON_DEBUG_BREAKPOINTS")) {
                std::printf("    MATCH! Returning true for empty fn breakpoint\n");
                std::fflush(stdout);
            }
            Breakpoint matchedBp = *it;
            matchedBp.maxLines = maxLine;
            // For scripts, use empty function name in stepBreakpoint
            matchedBp.functionName = "";
            // For step-mode breakpoints, update to the actual current line
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

    // For step-into: when entering a new function, check if we should break
    // This handles the case when we step into a called function
    for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
        if (it->stepMode && it->stepInto) {
            // If we're at a different file or function than the step breakpoint,
            // it means we've stepped into a new function
            if (!filenamesMatch(it->filename, filename)
                || it->functionName != currentFunctionName) {
                // We've entered a new function - break at first executable line
                Breakpoint matchedBp;
                matchedBp.filename = filename;
                matchedBp.functionName = currentFunctionName;
                matchedBp.line = currentLine;
                matchedBp.maxLines = maxLine;
                matchedBp.enabled = true;
                matchedBp.stepMode = false;
                stepBreakpoint = matchedBp;
                // Remove the consumed step-into breakpoint
                breakpoints.erase(it);
                return true;
            }
        }
    }

    return false;
}
//=============================================================================
bool
Evaluator::checkStepOutAfterFunctionReturn(AbstractSyntaxTreePtr t)
{
    // This method is called after a function call returns to check if we should
    // break due to a step-out breakpoint. This allows us to stop at the call site
    // (like MATLAB) rather than at the next statement.
    if (breakpoints.empty()) {
        return false;
    }

    size_t currentLine = getLinePosition(t);
    size_t maxLine = getMaxLinePosition(t);

    std::wstring filename = context->getCurrentScope()->getFilename();
    if (filename.empty()) {
        filename = utf8_to_wstring(callstack.getLastContext());
    }

    // Normalize filename for comparison
    std::wstring normalizedFilename = normalizePath(filename);

    std::string currentFunctionName = context->getCurrentScope()->getName();
    int currentCallStackSize = static_cast<int>(callstack.size());

    // Check for step out breakpoints AND step-next breakpoints that have returned to caller
    for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
        if (it->stepMode && it->stepOut) {
            // Step out triggers when we've returned to the caller function
            bool leftFunction = (it->functionName != currentFunctionName);
            bool atOrBelowTargetDepth
                = (it->targetDepth < 0) || (currentCallStackSize <= it->targetDepth);

            if (leftFunction && atOrBelowTargetDepth) {
                // Store breakpoint info before removing
                Breakpoint matchedBp;
                matchedBp.filename = filename;
                matchedBp.functionName = currentFunctionName;
                matchedBp.line = currentLine;
                matchedBp.maxLines = maxLine;
                matchedBp.enabled = true;
                matchedBp.stepMode = false;
                stepBreakpoint = matchedBp;
                // Remove the consumed step-out breakpoint
                breakpoints.erase(it);
                return true;
            }
        } else if (it->stepMode && it->stepNext && !it->stepInto) {
            // Step-next should break when returning to the original function from a subfunction
            // Check if we're back in the function where stepping started
            bool inTargetFunction
                = !it->functionName.empty() && (it->functionName == currentFunctionName);
            // Only break when at the same or deeper depth - never when shallower
            // If we're shallower, we've left the function entirely and shouldn't break here
            bool atValidDepth = (it->targetDepth > 0) && (currentCallStackSize >= it->targetDepth);

            if (inTargetFunction && atValidDepth) {
                if (true) {
                    std::printf("[stepReturn] returned to %s, depth=%d target=%d, breaking "
                                "at line %zu\n",
                        currentFunctionName.c_str(), currentCallStackSize, it->targetDepth,
                        currentLine);
                }
                // Store breakpoint info before removing
                Breakpoint matchedBp;
                matchedBp.filename = filename;
                matchedBp.functionName = currentFunctionName;
                matchedBp.line = currentLine;
                matchedBp.maxLines = maxLine;
                matchedBp.enabled = true;
                matchedBp.stepMode = false;
                stepBreakpoint = matchedBp;
                // Remove the consumed step-next breakpoint
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

    // Get available scope count (excluding global scope at index 0)
    size_t availableScopes = context->getScopeStackSize();
    if (availableScopes <= 2) {
        // Already at base workspace (only global and base scopes)
        return false;
    }

    // Calculate how many scopes we can actually bypass
    // We need to keep at least global (index 0) and base (index 1) scopes
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

    // Restore scopes one by one
    for (int i = 0; i < toRestore; ++i) {
        context->restoreBypassedScopes(1);
    }
    depth = currentBypass - toRestore;

    return toRestore > 0;
}
//=============================================================================
bool
Evaluator::adjustBreakpointLine(const std::wstring& filename, size_t requestedLine,
    size_t& adjustedLine, std::wstring& errorMessage)
{
    adjustedLine = 0;
    errorMessage.clear();

    if (filename.empty() || requestedLine == 0) {
        errorMessage = _W("Invalid parameters for breakpoint adjustment");
        return false;
    }

    // Create lambda to use existing getLinePosition method
    auto getLineFunc
        = [this](AbstractSyntaxTreePtr t) -> size_t { return this->getLinePosition(t); };

    AbstractSyntaxTreePtr code = nullptr;
    size_t maxLinesInFile = 0;

    // Try to lookup as a function first
    // Extract just the filename (without path) for function lookup
    FunctionDef* funcDef = nullptr;
    std::wstring basename = filename;
    size_t lastSlash = filename.find_last_of(L"/\\");
    if (lastSlash != std::wstring::npos) {
        basename = filename.substr(lastSlash + 1);
    }
    // Remove .m extension for function lookup
    std::wstring functionName = basename;
    if (functionName.length() > 2 && functionName.substr(functionName.length() - 2) == L".m") {
        functionName = functionName.substr(0, functionName.length() - 2);
    }
    std::string asFunctionName = wstring_to_utf8(functionName);

    // Try to lookup as a function first
    if (lookupFunction(asFunctionName, funcDef)) {
        if (funcDef->type() != NLS_MACRO_FUNCTION) {
            errorMessage = _W("Breakpoints can only be set in macro functions.");
            return false;
        }
        funcDef->updateCode();
        MacroFunctionDef* mFuncDef = static_cast<MacroFunctionDef*>(funcDef);

        // Calculate max lines in file by traversing all functions in the file
        // and find which function contains the requested line
        maxLinesInFile = 0;
        MacroFunctionDef* targetFunc = nullptr;
        MacroFunctionDef* currentFunc = mFuncDef;

        while (currentFunc != nullptr) {
            size_t funcMax = getMaxLineFromChainHelper(currentFunc->code, getLineFunc);
            if (funcMax > maxLinesInFile) {
                maxLinesInFile = funcMax;
            }

            // Check if this function might contain the requested line
            size_t funcMin = 0;
            if (currentFunc->code != nullptr) {
                funcMin = getLineFunc(currentFunc->code);
            }

            // If this function's range includes the requested line, or we haven't found any
            // function yet
            if ((funcMin > 0 && funcMin <= requestedLine && requestedLine <= funcMax)
                || (targetFunc == nullptr && funcMax > 0)) {
                targetFunc = currentFunc;
            }

            currentFunc = currentFunc->nextFunction;
        }

        if (targetFunc != nullptr) {
            code = targetFunc->code;
        } else {
            errorMessage = _W("Cannot locate function for breakpoint at line ")
                + std::to_wstring(requestedLine);
            return false;
        }
    } else {
        // Function not found - try to parse as a script file
        std::wstring resolvedFilename = filename;
        std::string fileAsString = wstring_to_utf8(filename);
        std::wstring filenameCopy = filename;
        if (PathFunctionIndexerManager::getInstance()->find(fileAsString, resolvedFilename)) {
            filenameCopy = resolvedFilename;
        }

        ParserState state = ParseFile(this, filenameCopy);
        if (state != ScriptBlock) {
            errorMessage = _W("Cannot adjust breakpoint: unable to parse script file '")
                + filenameCopy + L"'.";
            return false;
        }
        code = getParsedScriptBlock();
        maxLinesInFile = getMaxLineFromChainHelper(code, getLineFunc);
    }

    adjustedLine = getLineNumberFromCode(code, requestedLine, maxLinesInFile, getLineFunc);

    if (adjustedLine == 0) {
        errorMessage = _W("Cannot set breakpoint at line ") + std::to_wstring(requestedLine)
            + L": no executable statement found.";
        return false;
    }

    return true;
}
//=============================================================================
// Helper function to get max line from a node and ALL its children (down+right chain)
size_t
getMaxLineFromChainHelper(
    AbstractSyntaxTreePtr t, std::function<size_t(AbstractSyntaxTreePtr)> getLineFunc)
{
    if (t == nullptr) {
        return 0;
    }

    size_t maxLine = getLineFunc(t);

    // Get max from the down subtree
    if (t->down != nullptr) {
        size_t downMax = getMaxLineFromChainHelper(t->down, getLineFunc);
        if (downMax > maxLine) {
            maxLine = downMax;
        }
    }

    // Get max from right siblings at this level
    if (t->right != nullptr) {
        size_t rightMax = getMaxLineFromChainHelper(t->right, getLineFunc);
        if (rightMax > maxLine) {
            maxLine = rightMax;
        }
    }

    return maxLine;
}
//=============================================================================
size_t
getLineNumberFromCode(AbstractSyntaxTreePtr code, size_t lineNumber, size_t maxLinesInFile,
    std::function<size_t(AbstractSyntaxTreePtr)> getLineFunc)
{
    if (code == nullptr || lineNumber == 0) {
        return 0;
    }

    // Start from the first statement
    AbstractSyntaxTreePtr current = code;
    if (code->opNum == OP_BLOCK && code->down != nullptr) {
        current = code->down;
    }

    // First: Collect all top-level statements and their line ranges
    std::vector<std::pair<size_t, size_t>> statementRanges; // (startLine, endLine)
    AbstractSyntaxTreePtr stmt = current;
    while (stmt != nullptr) {
        size_t startLine = getLineFunc(stmt);

        // Sometimes the statement node itself has the end line, not the start line
        // Check if down child has an earlier line
        if (stmt->down != nullptr) {
            size_t downStartLine = getLineFunc(stmt->down);
            if (downStartLine > 0 && downStartLine < startLine) {
                startLine = downStartLine;
            }
        }

        // Calculate end line: traverse only the down subtree of this statement
        // Don't traverse stmt->right as that's the next statement
        size_t endLine = startLine;
        if (stmt->down != nullptr) {
            // Get max line from the entire down chain (including right siblings within the
            // expression)
            size_t downMax = getMaxLineFromChainHelper(stmt->down, getLineFunc);
            if (downMax > endLine) {
                endLine = downMax;
            }
        }

        // Also check the statement node itself for endLine
        size_t stmtLine = getLineFunc(stmt);
        if (stmtLine > endLine) {
            endLine = stmtLine;
        }

        if (startLine > 0) {
            statementRanges.push_back(std::make_pair(startLine, endLine));
        }

        // Move to next statement
        stmt = stmt->right;
    }

    if (statementRanges.empty()) {
        return 0;
    }

    // Second: Search the ENTIRE AST to see if the requested line starts any STATEMENT
    // Only consider actual statement nodes (OP_RSTATEMENT, OP_QSTATEMENT) that are
    // top-level statements (i.e., start lines in statementRanges), to avoid matching
    // internal expression nodes like array elements.
    std::function<size_t(AbstractSyntaxTreePtr)> findStatementAtLine
        = [&](AbstractSyntaxTreePtr node) -> size_t {
        if (node == nullptr) {
            return 0;
        }

        size_t nodeStartLine = getLineFunc(node);

        // If this node starts at the requested line and is an actual statement, check if it's
        // top-level
        if (nodeStartLine == lineNumber) {
            if (node->opNum == OP_RSTATEMENT || node->opNum == OP_QSTATEMENT) {
                // Only accept if this is a top-level statement start line
                for (const auto& range : statementRanges) {
                    if (range.first == lineNumber) {
                        return lineNumber; // This is a top-level statement
                    }
                }
                // Not a top-level statement - it's an internal node, skip it
            }
        }

        // Recursively check children
        if (node->down != nullptr) {
            size_t result = findStatementAtLine(node->down);
            if (result > 0) {
                return result;
            }
        }

        if (node->right != nullptr) {
            size_t result = findStatementAtLine(node->right);
            if (result > 0) {
                return result;
            }
        }

        return 0;
    };

    // Check entire tree for top-level statements starting at the requested line
    size_t foundLine = findStatementAtLine(current);
    if (foundLine > 0) {
        return foundLine;
    }

    // Alternative: Search deeper for statements that might be inside function definitions
    // Only accept actual statement nodes (OP_RSTATEMENT, OP_QSTATEMENT) at top-level in
    // sub-functions
    std::function<size_t(AbstractSyntaxTreePtr)> findStatementInSubtree
        = [&](AbstractSyntaxTreePtr node) -> size_t {
        if (node == nullptr) {
            return 0;
        }

        size_t nodeStartLine = getLineFunc(node);

        // Only return if this is an actual statement node at the requested line
        if (nodeStartLine == lineNumber) {
            if (node->opNum == OP_RSTATEMENT || node->opNum == OP_QSTATEMENT) {
                return lineNumber;
            }
        }

        // Check all children recursively
        if (node->down != nullptr) {
            size_t result = findStatementInSubtree(node->down);
            if (result > 0) {
                return result;
            }
        }

        if (node->right != nullptr) {
            size_t result = findStatementInSubtree(node->right);
            if (result > 0) {
                return result;
            }
        }

        return 0;
    };

    // Try deeper search
    foundLine = findStatementInSubtree(code);
    if (foundLine > 0) {
        return foundLine;
    }

    // Third: If no statement found at the requested line, check if it falls within
    // a multi-line statement. If it does and it's not at the start, move to the end line.
    // BUT: Skip absorption for large multi-line blocks (>5 lines, likely functions/control
    // structures). For large blocks, just return the requested line if it exists (don't absorb to
    // end).
    for (size_t i = 0; i < statementRanges.size(); ++i) {
        size_t stmtStart = statementRanges[i].first;
        size_t stmtEnd = statementRanges[i].second;

        // Check if the requested line falls within this statement
        if (lineNumber >= stmtStart && lineNumber <= stmtEnd) {
            // For large blocks (>5 lines), don't absorb - just accept the line as-is
            if (stmtEnd - stmtStart > 5) {
                return lineNumber;
            }

            // Multi-line statement with requested line not at start - use end line
            // This handles array definitions like [1; 2; 3; 4]
            if (stmtStart < stmtEnd && lineNumber > stmtStart) {
                return stmtEnd;
            }
            // Single-line or at the start
            return lineNumber;
        }
    }

    // Line is not within any statement - find the next executable line
    for (const auto& range : statementRanges) {
        if (range.first > lineNumber) {
            return range.first;
        }
    }

    // Line is after all statements in this function/script
    // Check if the line is still within the file bounds (for files with multiple functions)
    if (lineNumber <= maxLinesInFile) {
        // Return the last statement's start line - this allows setting breakpoints
        // on lines between functions or after the function ends but within the file
        return statementRanges.back().first;
    }

    // Line is past the end of the file - invalid
    return 0;
}
//=============================================================================
// Helper function to normalize paths for comparison (convert backslashes to forward slashes)
inline std::wstring
normalizePath(const std::wstring& path)
{
    std::wstring normalized = path;
    std::replace(normalized.begin(), normalized.end(), L'\\', L'/');
    return normalized;
}
//=============================================================================
// Helper function to extract basename from a path
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
// Helper function to check if filenames match (by full path or basename)
inline bool
filenamesMatch(const std::wstring& path1, const std::wstring& path2)
{
    std::wstring normalized1 = normalizePath(path1);
    std::wstring normalized2 = normalizePath(path2);

    // First try exact match
    if (normalized1 == normalized2) {
        return true;
    }

    // Fallback: compare basenames (handles cases where one is a full path and one is just a name)
    std::wstring basename1 = getBasename(normalized1);
    std::wstring basename2 = getBasename(normalized2);
    return basename1 == basename2;
}
//=============================================================================
// Helper function to get the earliest line number in a subtree
size_t
getLinePositionFromSubtreeHelper(AbstractSyntaxTreePtr t)
{
    if (t == nullptr) {
        return 0;
    }

    // Get line from current node's context (raw, without special handling)
    int contextValue = t->getContext();
    size_t currentLine = static_cast<size_t>(contextValue & 0x0000FFFF);
    if (currentLine == 0 && contextValue > 0) {
        currentLine = static_cast<size_t>(contextValue);
    } else if (contextValue == 0) {
        currentLine = 1;
    }

    // Recursively find the earliest line in children
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
