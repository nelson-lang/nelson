//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <vector>
#include "NelsonCodeIndentHelpers.hpp"
//=============================================================================
namespace Nelson::codeindent_detail {
//=============================================================================
// The closing 'end' of a function is optional: the body then runs up to the
// next 'function' keyword or to the end of the file. When the only openers left
// without a closer are functions, give each of them that implicit closer so the
// block balance and the indentation follow the parser.
static void
closeEndlessFunctions(BlockEvents& events)
{
    struct Position
    {
        size_t line;
        size_t index;
    };
    std::vector<Position> open;
    for (size_t line = 0; line < events.size(); ++line) {
        for (size_t k = 0; k < events[line].size(); ++k) {
            const LineKind kind = events[line][k].kind;
            if (kind == LineKind::Opener || kind == LineKind::ClassBlockOpener) {
                open.push_back({ line, k });
            } else if (kind == LineKind::Closer && !open.empty()) {
                open.pop_back();
            }
        }
    }
    std::vector<std::vector<bool>> endless(events.size());
    for (size_t line = 0; line < events.size(); ++line) {
        endless[line].assign(events[line].size(), false);
    }
    for (const Position& position : open) {
        if (!events[position.line][position.index].isFunction) {
            return;
        }
        endless[position.line][position.index] = true;
    }
    if (open.empty()) {
        return;
    }
    // true: an endless function, closed by the next function at its level.
    std::vector<bool> stack;
    for (size_t line = 0; line < events.size(); ++line) {
        std::vector<BlockEvent> rewritten;
        for (size_t k = 0; k < events[line].size(); ++k) {
            const BlockEvent& event = events[line][k];
            if (event.kind == LineKind::Closer) {
                if (!stack.empty()) {
                    stack.pop_back();
                }
            } else if (event.kind == LineKind::Opener || event.kind == LineKind::ClassBlockOpener) {
                if (event.isFunction) {
                    while (!stack.empty() && stack.back()) {
                        rewritten.push_back({ LineKind::Closer, false, event.leading, false });
                        stack.pop_back();
                    }
                }
                stack.push_back(endless[line][k]);
            }
            rewritten.push_back(event);
        }
        events[line] = rewritten;
    }
    // The endless functions still open end with the file.
    if (!events.empty()) {
        for (bool isEndless : stack) {
            if (isEndless) {
                events.back().push_back({ LineKind::Closer, false, false, false });
            }
        }
    }
}
//=============================================================================
} // namespace Nelson::codeindent_detail
//=============================================================================
