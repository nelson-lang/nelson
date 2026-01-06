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
//=============================================================================
namespace Nelson {
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
} // namespace Nelson
//=============================================================================
