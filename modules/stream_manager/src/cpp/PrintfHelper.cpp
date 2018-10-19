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
#include "PrintfHelper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
PrintfHelper::PrintfHelper(const ArrayOfVector& arg_) : args(arg_), vectorIndex(1), elementIndex(0)
{
    hasMoreData = (args.size() > 1);
    dataUsed = false;
}
//=============================================================================
void
PrintfHelper::IncrementDataPointer(void)
{
    indexType len;
    if (args[vectorIndex].isCharacterArray()) {
        len = 1;
    } else {
        len = args[vectorIndex].getDimensions().getElementCount();
    }
    if (++elementIndex >= len) {
        if (++vectorIndex < args.size()) {
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
            data = (long long)value.getContentAsInteger64Scalar();
        } else {
            value.promoteType(NLS_INT64);
            long long* ptr = (long long*)value.getDataPointer();
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
            double* ptr = (double*)value.getDataPointer();
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
        ArrayOf* ptr = (ArrayOf*)value.getDataPointer();
        ArrayOf data = ptr[elementIndex];
        if (!data.isEmpty()) {
            str = data.getContentAsWideString();
        }
    }
    IncrementDataPointer();
    return true;
}
//=============================================================================
bool
PrintfHelper::HasMoreData(void)
{
    return hasMoreData;
}
//=============================================================================
bool
PrintfHelper::WasDataUsed(void)
{
    return dataUsed;
}
//=============================================================================
bool
PrintfHelper::isEscape(wchar_t* dp)
{
    return ((dp[0] == L'\\')
        && ((dp[1] == L'n') || (dp[1] == L't') || (dp[1] == L'r') || (dp[1] == L'v')
               || (dp[1] == L'f') || (dp[1] == L'\\')));
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
    return 0;
}
//=============================================================================
std::wstring
PrintfHelper::ConvertEscapeSequences(const std::wstring& src)
{
    std::wstring dest;
    int i = 0;
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
