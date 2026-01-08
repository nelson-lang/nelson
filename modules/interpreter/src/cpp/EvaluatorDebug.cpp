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
#define FMT_HEADER_ONLY
#include <fmt/core.h>
#include <functional>
#include "i18n.hpp"
#include "Evaluator.hpp"
#include "Error.hpp"
#include "MacroFunctionDef.hpp"
#include "Warning.hpp"
#include "characters_encoding.hpp"
#include "ParseFile.hpp"
#include "ParserInterface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static size_t
getMaxLineFromChainHelper(
    AbstractSyntaxTreePtr t, std::function<size_t(AbstractSyntaxTreePtr)> getLineFunc);
static size_t
getLineNumberFromCode(AbstractSyntaxTreePtr code, size_t lineNumber, size_t maxLinesInFile,
    std::function<size_t(AbstractSyntaxTreePtr)> getLineFunc);
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
    if (breakpoints.empty()) {
        return false;
    }

    size_t currentLine = getLinePosition(t);
    size_t maxLine = getMaxLinePosition(t);

    std::wstring filename;
    filename = context->getCurrentScope()->getFilename();
    if (filename.empty()) {
        filename = utf8_to_wstring(callstack.getLastContext());
    }

    std::string currentFunctionName = context->getCurrentScope()->getName();
    int currentCallStackSize = static_cast<int>(callstack.size());

    // Note: Step-out breakpoints are handled separately in checkStepOutAfterFunctionReturn()
    // which is called after function calls complete, allowing us to stop at the call site.

    // Check for regular and step-in breakpoints
    for (auto it = breakpoints.begin(); it != breakpoints.end(); ++it) {
        // Skip step-out breakpoints - they are handled after function returns
        if (it->stepOut) {
            continue;
        }
        if ((it->filename == filename) && (it->functionName == currentFunctionName)
            && (it->line == currentLine)) {
            // Store the breakpoint info
            Breakpoint matchedBp = *it;
            matchedBp.maxLines = maxLine;
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
        if ((it->filename == filename) && (it->functionName.empty()) && (it->line == currentLine)) {
            Breakpoint matchedBp = *it;
            matchedBp.maxLines = maxLine;
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
            if (it->filename != filename || it->functionName != currentFunctionName) {
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

    std::string currentFunctionName = context->getCurrentScope()->getName();
    int currentCallStackSize = static_cast<int>(callstack.size());

    // Check for step out breakpoints
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
    FunctionDef* funcDef = nullptr;
    std::string asFunctionName = wstring_to_utf8(filename);

    if (lookupFunction(asFunctionName, funcDef)) {
        if (funcDef->type() != NLS_MACRO_FUNCTION) {
            errorMessage = _W("Breakpoints can only be set in macro functions.");
            return false;
        }
        funcDef->updateCode();
        MacroFunctionDef* mFuncDef = static_cast<MacroFunctionDef*>(funcDef);

        code = mFuncDef->code;

        // Calculate max lines in file by traversing all functions in the file
        maxLinesInFile = getMaxLineFromChainHelper(mFuncDef->code, getLineFunc);
        MacroFunctionDef* nextFunc = mFuncDef->nextFunction;
        while (nextFunc != nullptr) {
            size_t nextFuncMax = getMaxLineFromChainHelper(nextFunc->code, getLineFunc);
            if (nextFuncMax > maxLinesInFile) {
                maxLinesInFile = nextFuncMax;
            }
            nextFunc = nextFunc->nextFunction;
        }
    } else {
        // Try to parse as a script file
        ParserState state = ParseFile(this, filename);
        if (state != ScriptBlock) {
            errorMessage
                = _W("Cannot adjust breakpoint: unable to parse script file '") + filename + L"'.";
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

    // Collect all statements and their line ranges, in order
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

    // Find which statement contains the requested line
    for (size_t i = 0; i < statementRanges.size(); ++i) {
        size_t stmtStart = statementRanges[i].first;
        size_t stmtEnd = statementRanges[i].second;

        // Check if the requested line falls within this statement
        if (lineNumber >= stmtStart && lineNumber <= stmtEnd) {
            // Check if this is the start line of the statement
            if (lineNumber == stmtStart) {
                // Breakpoint on the first line of the statement - keep it there
                return stmtStart;
            }

            // Breakpoint is on a line within a multi-line statement (not the first line)
            // Adapt it to the next statement
            if (i + 1 < statementRanges.size()) {
                // Return the start of the next statement
                return statementRanges[i + 1].first;
            }

            // Line is within the last statement but not on its start - return the statement start
            return stmtStart;
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
} // namespace Nelson
//=============================================================================
