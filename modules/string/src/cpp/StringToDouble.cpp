//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
#include <algorithm>
#include <cmath>
#include <limits>
#include <cwctype>
#include <fast_float/fast_float.h>
#include <cstdlib>
#include "StringToDouble.hpp"
#include "StringHelpers.hpp"
#include "StringTrim.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define STR2DOUBLE_MAX_DIGIT_FORMAT L"%lg"
//=============================================================================
static std::wstring
ToUpper(const std::wstring& A)
{
    std::wstring res = A;
    transform(res.begin(), res.end(), res.begin(), towupper);
    return res;
}
//=============================================================================
static double
returnInfinity(bool bPositive)
{
    double res = std::numeric_limits<double>::infinity();
    if (!bPositive) {
        res = -res;
    }
    return res;
}
//=============================================================================
double
stringToDouble(const std::wstring& str, bool& wasConverted)
{
    double res = nan("");
    wasConverted = false;
    if (str.empty()) {
        wasConverted = true;
        return res;
    }
    // surrounding blanks (line breaks included, Unicode spaces only after the number)
    size_t first = 0;
    size_t last = str.size();
    while (first < last && IsNumberLeadingBlank(str[first])) {
        ++first;
    }
    while (
        last > first && (IsNumberLeadingBlank(str[last - 1]) || IsTextWhitespace(str[last - 1]))) {
        --last;
    }
    std::wstring STR = ToUpper(str.substr(first, last - first));
    if (STR.find(L'\0') != std::wstring::npos) {
        return res;
    }
    if (STR == ToUpper(NanString) || STR == ToUpper(NegNanString) || STR == ToUpper(PosNanString)) {
        res = nan("");
        wasConverted = true;
    } else if (STR == ToUpper(NegInfString)) {
        res = returnInfinity(false);
        wasConverted = true;
    } else if (STR == ToUpper(InfString) || STR == ToUpper(PosInfString)) {
        res = returnInfinity(true);
        wasConverted = true;
    } else {
        if (StringHelpers::contains(STR, L",")) {
            StringHelpers::replace_all(STR, L",", L"");
        }
        // only the exact Inf / NaN spellings above are numbers
        size_t body = (!STR.empty() && (STR[0] == L'-' || STR[0] == L'+')) ? 1 : 0;
        if (STR.compare(body, 3, L"INF") == 0 || STR.compare(body, 3, L"NAN") == 0) {
            return res;
        }
        bool isnegative = false;
        if (STR[0] == L'-') {
            STR.erase(0, 1);
            isnegative = true;
        } else if (STR[0] == L'+') {
            STR.erase(0, 1);
        }
        // A second leading sign (e.g. "--5" or "+-5") is not a valid number.
        if (!STR.empty() && (STR[0] == L'-' || STR[0] == L'+')) {
            return res;
        }

        std::string utf8str = wstring_to_utf8(STR);
        // Hexadecimal notation ("0x1A"), only when the whole string is a valid
        // hex literal.
        if (utf8str.size() > 2 && utf8str[0] == '0' && (utf8str[1] == 'x' || utf8str[1] == 'X')) {
            char* end = nullptr;
            double hexValue = strtod(utf8str.c_str(), &end);
            if (end != nullptr && *end == 0 && end != utf8str.c_str()) {
                res = isnegative ? -hexValue : hexValue;
                wasConverted = true;
            }
            return res;
        }
        // an internal sign is only valid right after the exponent marker
        const auto sign = utf8str.find_first_of("+-");
        if (sign != std::string::npos && (sign == 0 || utf8str[sign - 1] != 'E')) {
            return res;
        }
        fast_float::parse_options options { fast_float::chars_format::general };
        auto answer = fast_float::from_chars_advanced(
            utf8str.data(), utf8str.data() + utf8str.size(), res, options);
        size_t len = strlen(answer.ptr);
        // fast_float reports a result out of range (e.g. "1e400") with res already set to
        // +/-infinity (overflow) or 0 (underflow). Treat it as a successful conversion.
        if ((answer.ec == std::errc() || answer.ec == std::errc::result_out_of_range) && len == 0) {
            if (isnegative) {
                res = -res;
            }
            wasConverted = true;
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
