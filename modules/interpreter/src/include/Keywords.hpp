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
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
NLSINTERPRETER_IMPEXP wstringVector
GetKeywords(bool bSorted = false);
NLSINTERPRETER_IMPEXP bool
isKeyword(const std::wstring& key);
const unsigned int IDENTIFIER_LENGTH_MAX = 4096;
/**
 * These keywords must be in alphabetical order.
 */
const unsigned int NLS_KEYWORD_BREAK = 0;
const unsigned int NLS_KEYWORD_CASE = 1;
const unsigned int NLS_KEYWORD_CATCH = 2;
const unsigned int NLS_KEYWORD_CONTINUE = 3;
const unsigned int NLS_KEYWORD_ELSE = 4;
const unsigned int NLS_KEYWORD_ELSEIF = 5;
const unsigned int NLS_KEYWORD_END = 6;
const unsigned int NLS_KEYWORD_FOR = 7;
const unsigned int NLS_KEYWORD_FUNCTION = 8;
const unsigned int NLS_KEYWORD_IF = 9;
const unsigned int NLS_KEYWORD_KEYBOARD = 10;
const unsigned int NLS_KEYWORD_OTHERWISE = 11;
const unsigned int NLS_KEYWORD_ABORT = 12;
const unsigned int NLS_KEYWORD_RETURN = 13;
const unsigned int NLS_KEYWORD_SWITCH = 14;
const unsigned int NLS_KEYWORD_TRY = 15;
const unsigned int NLS_KEYWORD_WHILE = 16;
struct keywordStruct
{
    char word[IDENTIFIER_LENGTH_MAX];
    int token;
    int ordinal;
};
//=============================================================================
#define KEYWORDCOUNT 18
extern keywordStruct keyWord[KEYWORDCOUNT];
//=============================================================================
/**
 * Comparison function for the binary search on the keyWord list.
 */
int
compareKeyword(const void* /*a*/, const void* /*b*/);
//=============================================================================
} // namespace Nelson
//=============================================================================
