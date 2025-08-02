//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Keywords.hpp"
#include "NelSonParser.h"
#include "characters_encoding.hpp"
#include <algorithm>
#include <iostream>
#include <cstring>
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * NOTE: This list must be sorted alphabetically!
 */
keywordStruct keyWord[KEYWORDCOUNT] = { { "abort", ABORT, NLS_KEYWORD_ABORT },
    { "break", BREAK, NLS_KEYWORD_BREAK }, { "case", CASE, NLS_KEYWORD_CASE },
    { "catch", CATCH, NLS_KEYWORD_CATCH }, { "continue", CONTINUE, NLS_KEYWORD_CONTINUE },
    { "else", ELSE, NLS_KEYWORD_ELSE }, { "elseif", ELSEIF, NLS_KEYWORD_ELSEIF },
    { "end", END, NLS_KEYWORD_END }, { "endfunction", ENDFUNCTION, NLS_KEYWORD_ENDFUNCTION },
    { "for", FOR, NLS_KEYWORD_FOR }, { "function", FUNCTION, NLS_KEYWORD_FUNCTION },
    { "if", IF, NLS_KEYWORD_IF }, { "keyboard", KEYBOARD, NLS_KEYWORD_KEYBOARD },
    { "otherwise", OTHERWISE, NLS_KEYWORD_OTHERWISE }, { "parfor", FOR, NLS_KEYWORD_FOR },
    { "return", RETURN, NLS_KEYWORD_RETURN }, { "switch", SWITCH, NLS_KEYWORD_SWITCH },
    { "try", TRY, NLS_KEYWORD_TRY }, { "while", WHILE, NLS_KEYWORD_WHILE } };
//=============================================================================
int
compareKeyword(const void* a, const void* b)
{
    return strcmp(((keywordStruct*)a)->word, ((keywordStruct*)b)->word);
}
//=============================================================================
wstringVector
GetKeywords(bool bSorted)
{
    wstringVector strList;
    strList.reserve(KEYWORDCOUNT);
    for (auto& k : keyWord) {
        strList.emplace_back(utf8_to_wstring(k.word));
    }
    if (bSorted) {
        if (!strList.empty()) {
            std::sort(strList.begin(), strList.begin());
        }
    }
    return strList;
}
//=============================================================================
bool
isKeyword(const std::wstring& key)
{
    std::string _key = wstring_to_utf8(key);
    for (auto& k : keyWord) {
        if (_key == k.word) {
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
