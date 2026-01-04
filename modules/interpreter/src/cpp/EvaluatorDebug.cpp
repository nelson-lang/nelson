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
Evaluator::stepBreakpointExists(const Breakpoint& bp)
{
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

    for (auto breakpoint : breakpoints) {
        if ((breakpoint.filename == filename)
            && (breakpoint.functionName == context->getCurrentScope()->getName())
            && (breakpoint.line == currentLine)) {
            stepBreakpoint = breakpoint;
            return true;
        }
    }
    // Try with empty function name (global scope)
    for (auto breakpoint : breakpoints) {
        if ((breakpoint.filename == filename) && (breakpoint.functionName == "")
            && (breakpoint.line == currentLine)) {
            stepBreakpoint = breakpoint;
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
