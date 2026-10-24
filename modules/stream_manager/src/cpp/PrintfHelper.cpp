//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <climits>
#include <cmath>
#include <utility>
#include "PrintfHelper.hpp"
#include "StringHelpers.hpp"
#include "SprintF.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
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
    // Character arrays are consumed one character at a time (like every other
    // numeric conversion), so the length is always the element count.
    indexType len = args[vectorIndex].elementCount();
    if (++elementIndex >= len) {
        elementIndex = 0;
        if (++vectorIndex >= (indexType)args.size()) {
            hasMoreData = false;
        }
    }
    dataUsed = true;
}
//=============================================================================
void
PrintfHelper::AdvanceToNextArgument()
{
    // Used by %s, which consumes a whole argument at once.
    elementIndex = 0;
    if (++vectorIndex >= (int)args.size()) {
        hasMoreData = false;
    }
    dataUsed = true;
}
//=============================================================================
bool
PrintfHelper::GetNextVariableAsLongLong(long long& data, std::wstring& errorMessage, bool& isEmpty)
{
    isEmpty = false;
    if (!hasMoreData) {
        errorMessage
            = Nelson::resolveMessageTemplate(L"Nelson:stream_manager:moreData", L"No more data.");
        return false;
    }
    if (args[vectorIndex].isEmpty()) {
        isEmpty = true;
        data = 0;
    } else {
        ArrayOf value = args[vectorIndex];
        if (value.isScalar()) {
            data = static_cast<long long>(value.asInt64Scalar());
        } else {
            value.promoteType(NLS_INT64);
            auto* ptr = (long long*)value.dataPointer();
            data = ptr[elementIndex];
        }
    }
    IncrementDataPointer();
    return true;
}
//=============================================================================
bool
PrintfHelper::GetNextNumeric(PrintfNumeric& out, std::wstring& errorMessage)
{
    out = PrintfNumeric {};
    if (!hasMoreData) {
        errorMessage
            = Nelson::resolveMessageTemplate(L"Nelson:stream_manager:moreData", L"No more data.");
        return false;
    }
    ArrayOf value = args[vectorIndex];
    if (value.isEmpty()) {
        out.isEmpty = true;
        IncrementDataPointer();
        return true;
    }
    NelsonType dc = value.dataClass();
    if (dc == NLS_UINT64) {
        const auto* p = (const uint64*)value.dataPointer();
        out.u64 = static_cast<unsigned long long>(p[elementIndex]);
        out.i64 = static_cast<long long>(out.u64);
        out.dv = static_cast<double>(out.u64);
        out.integerValued = true;
        out.fitsInt64 = out.u64 <= static_cast<unsigned long long>(LLONG_MAX);
        out.fitsUint64 = true;
    } else if (dc == NLS_DOUBLE || dc == NLS_SINGLE || dc == NLS_DCOMPLEX || dc == NLS_SCOMPLEX) {
        double d;
        if (dc == NLS_DCOMPLEX || dc == NLS_SCOMPLEX) {
            ArrayOf v = value;
            v.promoteType(NLS_DCOMPLEX);
            const auto* p = (const double*)v.dataPointer();
            d = p[2 * elementIndex];
        } else if (value.isScalar()) {
            d = value.asDoubleScalar();
        } else {
            ArrayOf v = value;
            if (v.dataClass() != NLS_DOUBLE) {
                v.promoteType(NLS_DOUBLE);
            }
            const auto* p = (const double*)v.dataPointer();
            d = p[elementIndex];
        }
        out.dv = d;
        out.integerValued = std::isfinite(d) && (d == std::floor(d));
        // [-2^63, 2^63) and [0, 2^64) are the ranges exactly convertible to
        // signed / unsigned 64-bit integers.
        out.fitsInt64
            = out.integerValued && d >= -9223372036854775808.0 && d < 9223372036854775808.0;
        out.fitsUint64 = out.integerValued && d >= 0.0 && d < 18446744073709551616.0;
        out.i64 = out.fitsInt64 ? static_cast<long long>(d) : 0;
        out.u64 = out.fitsUint64 ? static_cast<unsigned long long>(d) : 0;
    } else {
        long long v64;
        if (value.isScalar() && dc != NLS_CHAR) {
            v64 = static_cast<long long>(value.asInt64Scalar());
        } else {
            // A character argument is read as its numeric code: promote first,
            // scalar case included.
            ArrayOf v = value;
            v.promoteType(NLS_INT64);
            const auto* p = (const int64*)v.dataPointer();
            v64 = static_cast<long long>(p[elementIndex]);
        }
        out.i64 = v64;
        out.u64 = static_cast<unsigned long long>(v64);
        out.dv = static_cast<double>(v64);
        out.integerValued = true;
        out.fitsInt64 = true;
        out.fitsUint64 = v64 >= 0;
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
        errorMessage
            = Nelson::resolveMessageTemplate(L"Nelson:stream_manager:moreData", L"No more data.");
        return false;
    }
    ArrayOf value = args[vectorIndex];
    if (value.isEmpty()) {
        isEmpty = true;
        data = 0;
    } else {
        NelsonType dc = value.dataClass();
        if (dc == NLS_DCOMPLEX || dc == NLS_SCOMPLEX) {
            ArrayOf v = value;
            v.promoteType(NLS_DCOMPLEX);
            const auto* p = (const double*)v.dataPointer();
            data = p[2 * elementIndex];
        } else if (value.isScalar() && dc != NLS_CHAR) {
            data = value.asDoubleScalar();
        } else {
            // A character argument is read as its numeric code: promote first,
            // scalar case included.
            ArrayOf v = value;
            if (v.dataClass() != NLS_DOUBLE) {
                v.promoteType(NLS_DOUBLE);
            }
            const auto* p = (const double*)v.dataPointer();
            data = p[elementIndex];
        }
    }
    IncrementDataPointer();
    return true;
}
//=============================================================================
bool
PrintfHelper::GetNextVariableAsString(std::wstring& str, std::wstring& errorMessage)
{
    str.clear();
    if (!hasMoreData) {
        errorMessage
            = Nelson::resolveMessageTemplate(L"Nelson:stream_manager:moreData", L"No more data.");
        return false;
    }
    ArrayOf value = args[vectorIndex];
    if (value.isCharacterArray()) {
        if (!value.isEmpty()) {
            std::wstring whole = value.asCharacterArrayWideString();
            if (static_cast<size_t>(elementIndex) < whole.size()) {
                str = whole.substr(elementIndex);
            }
        }
        AdvanceToNextArgument();
    } else if (value.isStringArray()) {
        auto* ptr = (ArrayOf*)value.dataPointer();
        if (ptr) {
            ArrayOf data = ptr[elementIndex];
            if (!data.isEmpty()) {
                str = data.asWideString();
            }
        }
        IncrementDataPointer();
    } else {
        // Numeric argument: %s consumes a run of consecutive elements that are
        // valid character codes; an element outside that range is consumed
        // alone and printed in exponential notation.
        indexType next = 0;
        if (!value.isEmpty()) {
            NelsonType dc = value.dataClass();
            bool complexSrc = (dc == NLS_DCOMPLEX || dc == NLS_SCOMPLEX);
            indexType n = value.elementCount();
            const double* p = nullptr;
            ArrayOf v = value;
            if (complexSrc) {
                v.promoteType(NLS_DCOMPLEX);
                p = (const double*)v.dataPointer();
            } else if (!(n == 1)) {
                if (v.dataClass() != NLS_DOUBLE) {
                    v.promoteType(NLS_DOUBLE);
                }
                p = (const double*)v.dataPointer();
            }
            next = elementIndex;
            while (next < n) {
                double d;
                if (complexSrc) {
                    d = p[2 * next];
                } else if (n == 1) {
                    d = value.asDoubleScalar();
                } else {
                    d = p[next];
                }
                if (std::isfinite(d) && d == std::floor(d) && d >= 0.0 && d <= 65535.0) {
                    str += static_cast<wchar_t>(static_cast<long long>(d));
                    next++;
                    continue;
                }
                if (next == elementIndex) {
                    std::wstring e = sprintf_w_utf8(std::wstring(L"%e"), d);
                    StringHelpers::replace_all(e, std::wstring(L"inf"), std::wstring(L"Inf"));
                    StringHelpers::replace_all(e, std::wstring(L"nan"), std::wstring(L"NaN"));
                    str += e;
                    next++;
                }
                break;
            }
        }
        if (next < static_cast<indexType>(value.elementCount())) {
            elementIndex = static_cast<int>(next);
            dataUsed = true;
        } else {
            AdvanceToNextArgument();
        }
    }
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
    if (*cp == L'*') {
        cp++;
    }
    while ((*cp) && iswdigit(*cp)) {
        cp++;
    }
    while ((*cp) && (*cp == L'.')) {
        cp++;
    }
    if (*cp == L'*') {
        cp++;
    }
    while ((*cp) && iswdigit(*cp)) {
        cp++;
    }
    // A single 'l' or 'h' length modifier is accepted and ignored.
    if (*cp == L'l' || *cp == L'h') {
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
