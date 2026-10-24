//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cwctype>
#include "helpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static NelsonType
precisionNameToType(const std::wstring& str, bool& bOK)
{
    bOK = true;
    if (str == L"logical") {
        return NLS_LOGICAL;
    }
    if ((str == L"double") || (str == L"real*8") || (str == L"float64")) {
        return NLS_DOUBLE;
    }
    if ((str == L"single") || (str == L"real*4") || (str == L"float32")) {
        return NLS_SINGLE;
    }
    if (str == L"int") {
        return NLS_INT32;
    }
    if ((str == L"int8") || (str == L"integer*1") || (str == L"schar")) {
        return NLS_INT8;
    }
    if ((str == L"int16") || (str == L"integer*2")) {
        return NLS_INT16;
    }
    if ((str == L"int32") || (str == L"integer*4")) {
        return NLS_INT32;
    }
    if ((str == L"int64") || (str == L"integer*8")) {
        return NLS_INT64;
    }
    if ((str == L"uint8") || (str == L"uchar")) {
        return NLS_UINT8;
    }
    if (str == L"uint16") {
        return NLS_UINT16;
    }
    if (str == L"uint32") {
        return NLS_UINT32;
    }
    if (str == L"uint64") {
        return NLS_UINT64;
    }
    if (str == L"char" || (str == L"*char")) {
        return NLS_CHAR;
    }
    bOK = false;
    return NLS_UINT8;
}
//=============================================================================
NelsonType
precisionFromString(const std::wstring& str, bool& bOK)
{
    std::wstring s = str;
    // 'source=>dest': the destination determines the stored class.
    size_t arrow = s.find(L"=>");
    if (arrow != std::wstring::npos) {
        s = s.substr(arrow + 2);
    }
    // '*type' shorthand: read/store using the same (source) class.
    if (!s.empty() && s[0] == L'*') {
        s = s.substr(1);
    }
    NelsonType result = precisionNameToType(s, bOK);
    if (bOK) {
        return result;
    }
    // 'type*N' block-count form (e.g. 'char*1'). Aliases such as 'real*8' are
    // resolved above.
    size_t star = s.find(L'*');
    if (star != std::wstring::npos && star + 1 < s.size()) {
        bool allDigits = true;
        for (size_t i = star + 1; i < s.size(); ++i) {
            if (iswdigit(s[i]) == 0) {
                allDigits = false;
                break;
            }
        }
        if (allDigits) {
            return precisionNameToType(s.substr(0, star), bOK);
        }
    }
    bOK = false;
    return NLS_UINT8;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
