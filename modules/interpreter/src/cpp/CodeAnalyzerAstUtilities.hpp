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
#include "Types.hpp"
#include <set>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace CodeAnalyzerAst {
    //=============================================================================
    std::string
    trim(const std::string& value);
    //=============================================================================
    std::string
    toLowerAscii(std::string value);
    //=============================================================================
    bool
    startsWithWord(const std::string& value, const std::string& word);
    //=============================================================================
    size_t
    codeAnalyzerCommentStart(const std::string& line);
    //=============================================================================
    std::string
    stripComment(const std::string& line);
    //=============================================================================
    std::string
    codeWithoutStringsAndComments(const std::string& line);
    //=============================================================================
    stringVector
    splitNames(const std::string& value);
    //=============================================================================
    bool
    isIdentifierStart(char c);
    //=============================================================================
    bool
    isIdentifierChar(char c);
    //=============================================================================
    size_t
    skipSpaces(const std::string& value, size_t cursor);
    //=============================================================================
    size_t
    codeAnalyzerAssignmentOperator(const std::string& line);
    //=============================================================================
    const std::set<std::string>&
    reservedWords();
    //=============================================================================
    int
    countOccurrences(const std::string& value, const std::string& token);
    //=============================================================================
    // Block keywords opened minus 'end' keywords found on one line, outside of indexing.
    int
    inlineBlockBalance(const std::string& line);
    //=============================================================================
    // Position of the first top-level ',' or ';' from cursor, or the code size.
    size_t
    firstStatementEnd(const std::string& code, size_t cursor);
    //=============================================================================
    // True when a block header line, like 'if c, x = 1;', also holds a statement.
    bool
    blockHeaderHasStatement(const std::string& line);
    //=============================================================================
    // 'arguments' or 'arguments (Input)' opens a block; 'arguments = argv()' does not.
    bool
    isArgumentsBlockHeader(const std::string& line);
    //=============================================================================
} // namespace CodeAnalyzerAst
//=============================================================================
} // namespace Nelson
//=============================================================================
