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
#include <string>
#include <vector>
#include "Types.hpp"
#include "ClassdefMixinNames.hpp"
//=============================================================================
#include "ClassdefParserConstants.hpp"
//=============================================================================
namespace Nelson::classdef_detail {
//=============================================================================
std::string
trim(const std::string& input);
std::string
tolowerString(std::string value);
bool
equalsIgnoreCase(const std::string& left, const std::string& right);
std::string
classNameLeaf(const std::string& className);
std::string
externalRootName();
std::string
copyableMixinName();
std::string
customDisplayMixinName();
std::string
setGetMixinName();
std::string
setGetExactNamesMixinName();
std::string
redefinesParenMixinName();
std::string
redefinesBraceMixinName();
std::string
redefinesDotMixinName();
std::string
scalarMixinName();
bool
equalsClassNameOrLeaf(const std::string& name, const std::string& className);
bool
startsWithKeyword(const std::string& line, const std::string& keyword);
bool
singleQuoteOpensCharArray(char previousChar);
std::string
stripComment(const std::string& line);
std::string
withoutTrailingStatementSeparator(std::string value);
std::vector<std::string>
splitLines(const std::string& source);
std::vector<std::string>
stripBlockComments(std::vector<std::string> lines);
size_t
findLineContinuation(const std::string& line);
bool
hasLineContinuation(const std::string& line);
std::string
withoutLineContinuation(const std::string& line);
int
bracketDepthDelta(const std::string& line);
std::string
joinedContinuationLine(
    const std::vector<std::string>& lines, size_t first, size_t limit, size_t& last);
std::vector<std::string>
splitTopLevelSeparators(const std::string& line);
std::vector<std::string>
expandInlineBlocks(const std::vector<std::string>& lines);
std::string
joinLines(const std::vector<std::string>& lines, size_t first, size_t last);
stringVector
splitCommaList(const std::string& value);
//=============================================================================
bool
isIdentifierStart(char ch);
bool
isIdentifierPart(char ch);
std::string
parseLeadingIdentifier(const std::string& value);
std::string
parseLeadingQualifiedIdentifier(const std::string& value);
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
