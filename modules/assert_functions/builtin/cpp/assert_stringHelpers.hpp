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
#include <regex>
#include "ErrorID.hpp"
#include "assert_extraBuiltins.hpp"
#include "assert_modernHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::AssertFunctionsGateway;
//=============================================================================
static bool
regexMatches(const std::wstring& text, const std::wstring& pattern)
{
    try {
        return std::regex_search(text, std::wregex(pattern));
    } catch (const std::regex_error&) {
        Error(L"Nelson:asserts:invalidRegularExpression", L"Invalid regular expression.");
    }
    return false;
}
//=============================================================================
static ArrayOfVector
containsPatternsResult(int nLhs, const ArrayOfVector& argIn, bool requireAll)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 2);
    std::wstring text = argIn[0].asWideString();
    wstringVector patterns = stringList(argIn[1]);
    bool passed = requireAll;
    for (const std::wstring& pattern : patterns) {
        bool found = text.find(pattern) != std::wstring::npos;
        passed = requireAll ? (passed && found) : (passed || found);
    }
    std::wstring name = requireAll ? L"asserts.containsAll" : L"asserts.containsAny";
    std::wstring msg = passed
        ? L""
        : formatI18nMessage(
              Nelson::resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedMatchingText",
                  L"Assertion failed: %s expected matching text patterns."),
              name);
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
static ArrayOfVector
matchesPatternsResult(int nLhs, const ArrayOfVector& argIn, bool requireAll)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 2);
    std::wstring text = argIn[0].asWideString();
    wstringVector patterns = stringList(argIn[1]);
    bool passed = requireAll;
    for (const std::wstring& pattern : patterns) {
        bool found = regexMatches(text, pattern);
        passed = requireAll ? (passed && found) : (passed || found);
    }
    std::wstring name = requireAll ? L"asserts.matchesAll" : L"asserts.matchesAny";
    std::wstring msg = passed
        ? L""
        : formatI18nMessage(Nelson::resolveMessageTemplate(
                                L"Nelson:assert_functions:assertionFailedMatchingRegular",
                                L"Assertion failed: %s expected matching regular expressions."),
              name);
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
