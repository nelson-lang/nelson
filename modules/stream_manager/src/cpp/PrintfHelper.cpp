//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <utility>
#include "PrintfHelper.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
PrintfHelper::PrintfHelper(ArrayOfVector arg_) : args(arg_)
{
    hasMoreData = (args.size() > 1);
    dataUsed = false;
}
//=============================================================================
void
PrintfHelper::IncrementDataPointer()
{
    indexType len;
    if (args[vectorIndex].isCharacterArray()) {
        len = 1;
    } else {
        len = args[vectorIndex].getElementCount();
    }
    if (++elementIndex >= len) {
        if (++vectorIndex < (indexType)args.size()) {
            elementIndex = 0;
        } else {
            hasMoreData = false;
        }
    }
    dataUsed = true;
}
//=============================================================================
bool
PrintfHelper::GetNextVariableAsLongLong(long long& data, std::wstring& errorMessage, bool& isEmpty)
{
    isEmpty = false;
    if (!hasMoreData) {
        errorMessage = _W("No more data.");
        return false;
    }
    if (args[vectorIndex].isEmpty()) {
        isEmpty = true;
        data = 0;
    } else {
        ArrayOf value = args[vectorIndex];
        if (value.isScalar()) {
            data = static_cast<long long>(value.getContentAsInteger64Scalar());
        } else {
            value.promoteType(NLS_INT64);
            auto* ptr = (long long*)value.getDataPointer();
            data = ptr[elementIndex];
        }
    }
    IncrementDataPointer();
    return true;
}
//=============================================================================
bool
PrintfHelper::GetNextVariableAsDouble(double& data, std::wstring& errorMessage, bool& isEmpty)
{
    isEmpty = false;
    if (!hasMoreData) {
        errorMessage = _W("No more data.");
        return false;
    }
    if (args[vectorIndex].isEmpty()) {
        isEmpty = true;
        data = 0;
    } else {
        ArrayOf value = args[vectorIndex];
        if (value.isScalar()) {
            data = value.getContentAsDoubleScalar();
        } else {
            auto* ptr = (double*)value.getDataPointer();
            data = ptr[elementIndex];
        }
    }
    IncrementDataPointer();
    return true;
}
//=============================================================================
bool
PrintfHelper::GetNextVariableAsString(std::wstring& str, std::wstring& errorMessage)
{
    if (!hasMoreData) {
        errorMessage = _W("No more data.");
        return false;
    }
    ArrayOf value = args[vectorIndex];
    if (value.isCharacterArray()) {
        if (!value.isEmpty()) {
            str = value.getContentAsArrayOfCharacters();
        }
    } else if (value.isStringArray()) {
        auto* ptr = (ArrayOf*)value.getDataPointer();
        if (ptr) {
            ArrayOf data = ptr[elementIndex];
            if (!data.isEmpty()) {
                str = data.getContentAsWideString();
            }
        }
    }
    IncrementDataPointer();
    return true;
}
//=============================================================================
bool
PrintfHelper::HasMoreData()
{
    return hasMoreData;
}
//=============================================================================
bool
PrintfHelper::WasDataUsed()
{
    return dataUsed;
}
//=============================================================================
bool
PrintfHelper::isEscape(const wchar_t* dp)
{
    return ((dp[0] == L'\\')
        && ((dp[1] == L'n') || (dp[1] == L'b') || (dp[1] == L't') || (dp[1] == L'r')
            || (dp[1] == L'v') || (dp[1] == L'f') || (dp[1] == L'\\')));
}
//=============================================================================
int
PrintfHelper::flagCharacter(wchar_t c)
{
    return ((c == L'#') || (c == L'0') || (c == L'-') || (c == L' ') || (c == L'+'));
}
//=============================================================================
int
PrintfHelper::convSpec(wchar_t c)
{
    return ((c == L'd') || (c == L'i') || (c == L'o') || (c == L'u') || (c == L'x') || (c == L'X')
        || (c == L'e') || (c == L'E') || (c == L'f') || (c == L'F') || (c == L'g') || (c == L'G')
        || (c == L'a') || (c == L'A') || (c == L'c') || (c == L's'));
}
//=============================================================================
wchar_t*
PrintfHelper::validateFormatSpec(wchar_t* cp)
{
    if (*cp == L'%') {
        return cp + 1;
    }
    while ((*cp) && flagCharacter(*cp)) {
        cp++;
    }
    while ((*cp) && iswdigit(*cp)) {
        cp++;
    }
    while ((*cp) && (*cp == L'.')) {
        cp++;
    }
    while ((*cp) && iswdigit(*cp)) {
        cp++;
    }
    if ((*cp) && convSpec(*cp)) {
        return cp + 1;
    }
    return nullptr;
}
//=============================================================================
std::wstring
PrintfHelper::ConvertEscapeSequences(const std::wstring& src)
{
    std::wstring dest;
    size_t i = 0;
    while (i < src.size()) {
        if ((src[i] != L'\\') || (i == (src.size() - 1))) {
            dest += src[i];
        } else {
            if (src[i + 1] == L'n') {
                dest += L'\n';
                i++;
            } else if (src[i + 1] == L't') {
                dest += L'\t';
                i++;
            } else if (src[i + 1] == L'r') {
                dest += L'\r';
                i++;
            } else if (src[i + 1] == L'v') {
                dest += L'\v';
                i++;
            } else if (src[i + 1] == L'f') {
                dest += L'\f';
                i++;
            } else {
                dest += src[i + 1];
                i++;
            }
        }
        i++;
    }
    return dest;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
