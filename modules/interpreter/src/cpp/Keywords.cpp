//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "Keywords.hpp"
#include "NelSonParser.h"
#include "characters_encoding.hpp"
#include <algorithm>
#include <iostream>
#include <string.h>
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
    { "quit", QUIT, NLS_KEYWORD_QUIT }, { "return", RETURN, NLS_KEYWORD_RETURN },
    { "switch", SWITCH, NLS_KEYWORD_SWITCH }, { "try", TRY, NLS_KEYWORD_TRY },
    { "while", WHILE, NLS_KEYWORD_WHILE } };
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
    for (size_t k = 0; k < KEYWORDCOUNT; k++) {
        strList.push_back(utf8_to_wstring(keyWord[k].word));
    }
    if (bSorted) {
        std::sort(strList.begin(), strList.begin());
    }
    return strList;
}
//=============================================================================
bool
isKeyword(std::wstring key)
{
    std::string _key = wstring_to_utf8(key);
    for (size_t k = 0; k < KEYWORDCOUNT; k++) {
        if (_key.compare(keyWord[k].word) == 0) {
            return true;
        }
    }
    return false;
}
//=============================================================================
}
//=============================================================================
