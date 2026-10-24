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
// Where a line starts relative to the brackets left open by the lines above it.
struct BracketLineState
{
    // The line starts inside an open bracket: a row of a multi-line literal
    // ("{" ... "};") or an argument line, continuing its statement.
    bool insideBracket = false;
    // Its first token closes the outermost open bracket ("};", "]").
    bool closesOutermost = false;
};
//=============================================================================
static std::vector<BracketLineState>
collectBracketLineStates(const std::vector<FormattingToken>& tokens, size_t lineCount)
{
    std::vector<BracketLineState> states(lineCount);
    int depth = 0;
    size_t nextLine = 0;
    for (const FormattingToken& token : tokens) {
        if (token.line < 0 || static_cast<size_t>(token.line) >= lineCount) {
            continue;
        }
        const size_t line = static_cast<size_t>(token.line);
        const bool firstTokenOfLine = line >= nextLine;
        // Lines without tokens (blank or comment only) share the depth too.
        for (; nextLine <= line; ++nextLine) {
            states[nextLine].insideBracket = depth > 0;
        }
        if (firstTokenOfLine && depth == 1 && isCloseToken(token)) {
            states[line].closesOutermost = true;
        }
        if (isOpenToken(token)) {
            ++depth;
        } else if (isCloseToken(token) && depth > 0) {
            --depth;
        }
    }
    return states;
}
//=============================================================================
} // namespace Nelson::codeindent_detail
//=============================================================================
