//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <cstdio>
#include <cstring>
#include <cwctype>
#include <vector>
#include "StringHelpers.hpp"
#include "PrintfFunction.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PrintfHelper.hpp"
#include "SprintF.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
formatFloating(const std::wstring& fmtSpec, double value);
static std::wstring
exponentFallback(const std::wstring& fmtSpec, double value);
static void
stripLengthModifier(std::wstring& fmtSpec);
//=============================================================================
bool
printfFunction(const ArrayOfVector& args, std::wstring& errorMessage, std::wstring& result)
{
    ArrayOf format(args[0]);
    std::wstring frmt = format.asWideString();

    std::vector<wchar_t> buffer(frmt.length() + 1);
    std::copy(frmt.begin(), frmt.end(), buffer.data());

    wchar_t* dp = &buffer[0];
#define BUFFER_SIZE_MAX 65534
    wchar_t nbuff[BUFFER_SIZE_MAX];
    errorMessage.clear();
    PrintfHelper ps(args);
    while ((*dp) || ps.HasMoreData()) {
        if (!(*dp) && ps.HasMoreData()) {
            if (!ps.WasDataUsed()) {
                // A format without conversion is written once.
                return true;
            }
            dp = &buffer[0];
        }
        wchar_t* np = dp;
        int nbuf_ind = 0;
        while ((*dp) && (*dp != L'%') && nbuf_ind < BUFFER_SIZE_MAX) {
            if (*dp == L'\\' && *(dp + 1) != L'\0') {
                wchar_t nx = *(dp + 1);
                switch (nx) {
                case L'\\':
                    nbuff[nbuf_ind++] = L'\\';
                    dp += 2;
                    break;
                case L'n':
                    nbuff[nbuf_ind++] = L'\n';
                    dp += 2;
                    break;
                case L't':
                    nbuff[nbuf_ind++] = L'\t';
                    dp += 2;
                    break;
                case L'r':
                    nbuff[nbuf_ind++] = L'\r';
                    dp += 2;
                    break;
                case L'b':
                    nbuff[nbuf_ind++] = L'\b';
                    dp += 2;
                    break;
                case L'v':
                    nbuff[nbuf_ind++] = L'\v';
                    dp += 2;
                    break;
                case L'f':
                    nbuff[nbuf_ind++] = L'\f';
                    dp += 2;
                    break;
                case L'a':
                    nbuff[nbuf_ind++] = L'\a';
                    dp += 2;
                    break;
                case L'"':
                    nbuff[nbuf_ind++] = L'"';
                    dp += 2;
                    break;
                case L'\'':
                    nbuff[nbuf_ind++] = L'\'';
                    dp += 2;
                    break;
                case L'?':
                    nbuff[nbuf_ind++] = L'?';
                    dp += 2;
                    break;
                case L'x':
                case L'X': {
                    dp += 2;
                    int val = 0;
                    int cnt = 0;
                    while (cnt < 2 && iswxdigit(*dp)) {
                        wchar_t h = *dp;
                        int digit = (h >= L'0' && h <= L'9') ? (int)(h - L'0')
                            : (h >= L'a' && h <= L'f')       ? (int)(h - L'a' + 10)
                                                             : (int)(h - L'A' + 10);
                        val = val * 16 + digit;
                        dp++;
                        cnt++;
                    }
                    if (cnt == 0) {
                        // no hex digit follows: keep the backslash literally
                        nbuff[nbuf_ind++] = L'\\';
                        dp -= 1;
                    } else {
                        nbuff[nbuf_ind++] = (wchar_t)val;
                    }
                } break;
                default:
                    if (nx >= L'0' && nx <= L'7') {
                        dp += 1;
                        int val = 0;
                        int cnt = 0;
                        while (cnt < 3 && *dp >= L'0' && *dp <= L'7') {
                            val = val * 8 + (int)(*dp - L'0');
                            dp++;
                            cnt++;
                        }
                        nbuff[nbuf_ind++] = (wchar_t)val;
                    } else {
                        // unknown escape: keep the backslash literally
                        nbuff[nbuf_ind++] = *dp;
                        dp++;
                    }
                    break;
                }
            } else {
                *(nbuff + nbuf_ind++) = *(dp++);
            }
        }
        *(nbuff + nbuf_ind) = 0;
        result.append(nbuff, nbuf_ind);
        if (*dp == L'%' && (*(dp + 1) == L'\0' || *(dp + 1) == L'%')) {
            if (*(dp + 1) == L'\0') {
                return true;
            }
            result.push_back(L'%');
            dp += 2;
        }
        if (*dp == L'%' && *(dp + 1)) {
            np = PrintfHelper::validateFormatSpec(dp + 1);
            if (!np) {
                errorMessage
                    = formatI18nMessage(Nelson::resolveMessageTemplate(
                                            L"Nelson:stream_manager:erroneousFormatSpecification",
                                            L"Erroneous format specification %s"),
                        std::wstring(dp));
                return false;
            }
            wchar_t sv = 0;
            if (*(np - 1) == L'%') {
                result.push_back(L'%');
                sv = 0;
            } else if (*(np - 1) == L's') {
                sv = *np;
                *np = 0;
                std::wstring stringFormat(dp);
                stripLengthModifier(stringFormat);
                size_t starPosition = stringFormat.find(L'*');
                while (starPosition != std::wstring::npos) {
                    PrintfNumeric width;
                    if (!ps.GetNextNumeric(width, errorMessage)) {
                        errorMessage.clear();
                        return true;
                    }
                    if (width.isEmpty || !width.integerValued) {
                        errorMessage
                            = resolveMessageTemplate(L"Nelson:stream_manager:widthIntegerScalar",
                                L"Width must be an integer scalar.");
                        return false;
                    }
                    stringFormat.replace(starPosition, 1, std::to_wstring(width.i64));
                    starPosition = stringFormat.find(L'*', starPosition + 1);
                }
                std::wstring str;
                if (!ps.GetNextVariableAsString(str, errorMessage)) {
                    *np = sv;
                    errorMessage.clear();
                    return true;
                }
                result += sprintf_w_utf8(stringFormat, str);
            } else {
                sv = *np;
                *np = 0;
                wchar_t conv = *(np - 1);
                // Dynamic width/precision '*' consumes leading integer arguments,
                // as in sprintf('%*.*f', 8, 3, pi).
                std::wstring fmtSpec(dp);
                stripLengthModifier(fmtSpec);
                {
                    size_t starPos = fmtSpec.find(L'*');
                    while (starPos != std::wstring::npos) {
                        PrintfNumeric width;
                        if (!ps.GetNextNumeric(width, errorMessage)) {
                            errorMessage.clear();
                            *np = sv;
                            return true;
                        }
                        if (width.isEmpty || !width.integerValued) {
                            errorMessage = resolveMessageTemplate(
                                L"Nelson:stream_manager:widthIntegerScalar",
                                L"Width must be an integer scalar.");
                            *np = sv;
                            return false;
                        }
                        fmtSpec.replace(starPos, 1, std::to_wstring(width.i64));
                        starPos = fmtSpec.find(L'*', starPos + 1);
                    }
                }
                switch (conv) {
                case L'd':
                case L'i':
                case L'o':
                case L'u':
                case L'x':
                case L'X':
                case L'c': {
                    PrintfNumeric num;
                    if (!ps.GetNextNumeric(num, errorMessage)) {
                        errorMessage.clear();
                        return true;
                    }
                    if (!num.isEmpty) {
                        if (conv == L'c') {
                            if (num.integerValued && num.dv >= 0.0 && num.dv <= 65535.0) {
                                if (fmtSpec.size() == 2) {
                                    result.push_back(static_cast<wchar_t>(num.i64));
                                } else {
                                    // reuse width/flags through %s.
                                    std::wstring one(1, static_cast<wchar_t>(num.i64));
                                    std::wstring f(fmtSpec);
                                    f.back() = L's';
                                    result += sprintf_w_utf8(f, one);
                                }
                            } else {
                                result += exponentFallback(fmtSpec, num.dv);
                            }
                        } else {
                            // %d/%i print any value of the signed 64-bit range,
                            // %o/%u/%x/%X any value of the unsigned 64-bit range;
                            // everything else (non-integer, non-finite, out of
                            // range) falls back to exponential notation.
                            bool signedConv = (conv == L'd' || conv == L'i');
                            bool fits = signedConv ? num.fitsInt64 : num.fitsUint64;
                            std::wstring tmp;
                            if (!fits) {
                                tmp = exponentFallback(fmtSpec, num.dv);
                            } else {
                                std::wstring f(fmtSpec);
                                f.insert(f.size() - 1, L"ll");
                                if (signedConv) {
                                    f.back() = L'd'; // normalise %i to %d for fmt
                                    tmp = sprintf_w_utf8(f, num.i64);
                                } else {
                                    tmp = sprintf_w_utf8(f, num.u64);
                                }
                            }
                            result += tmp;
                        }
                    }
                } break;
                case L'e':
                case L'E':
                case L'f':
                case L'F':
                case L'g':
                case L'G': {
                    bool isEmpty;
                    double data;
                    if (!ps.GetNextVariableAsDouble(data, errorMessage, isEmpty)) {
                        errorMessage.clear();
                        return true;
                    }
                    if (!isEmpty) {
                        result += formatFloating(fmtSpec, data);
                    }
                } break;
                }
            }
            if (sv) {
                *np = sv;
            }
            dp = np;
        }
    }
    return true;
}
//=============================================================================
// Floating-point conversion: non-finite values print as Inf / NaN, and NaN
// never carries a sign (the '+' and ' ' flags are dropped for it).
static std::wstring
formatFloating(const std::wstring& fmtSpec, double value)
{
    if (std::isfinite(value)) {
        return sprintf_w_utf8(fmtSpec, value);
    }
    std::wstring f(fmtSpec);
    if (std::isnan(value)) {
        size_t k = 1;
        while (k < f.size()
            && (f[k] == L'#' || f[k] == L'0' || f[k] == L'-' || f[k] == L' ' || f[k] == L'+')) {
            if (f[k] == L'+' || f[k] == L' ') {
                f.erase(k, 1);
            } else {
                k++;
            }
        }
    }
    std::wstring tmp = sprintf_w_utf8(f, value);
    StringHelpers::replace_all(tmp, std::wstring(L"inf"), std::wstring(L"Inf"));
    StringHelpers::replace_all(tmp, std::wstring(L"nan"), std::wstring(L"NaN"));
    return tmp;
}
//=============================================================================
// '%5d' -> '%5e': the integer conversion is replaced by %e, flags, width and
// precision are kept.
static std::wstring
exponentFallback(const std::wstring& fmtSpec, double value)
{
    std::wstring fe(fmtSpec);
    fe.back() = L'e';
    return formatFloating(fe, value);
}
//=============================================================================
// '%ld' -> '%d': the 'l' / 'h' length modifier accepted by validateFormatSpec
// carries no meaning here.
static void
stripLengthModifier(std::wstring& fmtSpec)
{
    size_t n = fmtSpec.size();
    if (n >= 2 && (fmtSpec[n - 2] == L'l' || fmtSpec[n - 2] == L'h')) {
        fmtSpec.erase(n - 2, 1);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
