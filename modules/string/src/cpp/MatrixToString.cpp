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
#include <cwchar>
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "MatrixToString.hpp"
#include "StringHelpers.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// A computed -0.0 prints as "0", not "-0". signbit + fabs is not elided under
// fast floating-point optimisation.
template <class T>
static inline T
normalizeNegativeZero(T v)
{
    if (v == T(0) && std::signbit(v)) {
        return std::fabs(v);
    }
    return v;
}
//=============================================================================
template <typename T>
static std::wstring
complexToString(T complexValue, const std::wstring& formatNumber)
{
    std::wstring res;
    std::wstring realPartStr;
    std::wstring imagPartStr;
    if (std::isnan(complexValue.real())) {
        realPartStr = L"NaN";
    } else {
        if (std::isinf(complexValue.real())) {
            if (complexValue.real() > 0) {
                realPartStr = L"Inf";
            } else {
                realPartStr = L"-Inf";
            }
        } else {
            realPartStr = fmt::format(
                fmt::runtime(formatNumber), normalizeNegativeZero(complexValue.real()));
        }
    }
    if (std::isnan(complexValue.imag())) {
        imagPartStr = L"NaN";
    } else {
        if (std::isinf(complexValue.imag())) {
            if (complexValue.imag() > 0) {
                imagPartStr = L"Inf";
            } else {
                imagPartStr = L"-Inf";
            }
        } else {
            imagPartStr = fmt::format(
                fmt::runtime(formatNumber), normalizeNegativeZero(complexValue.imag()));
        }
    }
    if (imagPartStr.compare(L"NaN") == 0 || imagPartStr.compare(L"Inf") == 0
        || imagPartStr.compare(L"-Inf") == 0) {
        if (imagPartStr.compare(L"-Inf") == 0) {
            res = realPartStr + L"-1i*" + L"Inf";
        } else {
            res = realPartStr + L"+1i*" + imagPartStr;
        }
    } else {
        if (complexValue.imag() > 0) {
            res = realPartStr + L"+" + imagPartStr + L"i";
        } else if (complexValue.imag() < 0) {
            res = realPartStr + imagPartStr + L"i";
        } else {
            // Imaginary part is exactly zero: display only the real part
            // (mat2str(complex(1,0)) -> "1").
            res = realPartStr;
        }
    }
    return res;
}
//=============================================================================
std::wstring
MatrixToString(ArrayOf A, indexType precision, bool withClass)
{
    std::wstring res;
    if (!A.is2D()) {
        Error(L"Nelson:validators:mustBeMatrix", L"%s must be a 2-D matrix.", L"Value");
    }
    std::wstring formatNumber = L"{:." + std::to_wstring(precision) + L"g}";
    std::wstring class_name = L"";
    ClassName(A, class_name);
    if (A.dataClass() == NLS_CHAR || A.dataClass() == NLS_STRING_ARRAY) {
        // text literals carry their class already
        withClass = false;
    }
    if (withClass) {
        res = class_name + L"(";
    }
    Dimensions dims = A.dimensions();
    if (A.isEmpty()) {
        if (A.dataClass() == NLS_STRING_ARRAY) {
            res += L"strings(" + std::to_wstring(dims.rows()) + L","
                + std::to_wstring(dims.columns()) + L")";
        } else if (A.dataClass() == NLS_CHAR && A.isEmpty(true)) {
            // A 0x0 empty character array prints as an empty string literal.
            res += L"''";
        } else if (A.isEmpty(true)) {
            res += L"[]";
        } else {
            res += L"zeros(" + std::to_wstring(dims.rows()) + L"," + std::to_wstring(dims.columns())
                + L")";
        }
    } else if (A.dataClass() == NLS_CHAR) {
        // Character arrays: each row is a single-quoted string; internal single
        // quotes are doubled. Line-break control characters and the null
        // character (0, 10, 11, 12, 13) cannot appear inside a single-line
        // literal, so they are emitted as char(N) and the surrounding literal
        // pieces (even empty ones) are concatenated in brackets. Multiple rows
        // are wrapped in brackets and separated by ';'.
        auto* pChar = (charType*)A.dataPointer();
        indexType rows = dims.rows();
        indexType cols = dims.columns();
        auto quoteChar = [](const std::wstring& s) -> std::wstring {
            std::wstring q = s;
            StringHelpers::replace_all(q, L"'", L"''");
            return L"'" + q + L"'";
        };
        auto renderCharRow = [&quoteChar](const std::wstring& s) -> std::wstring {
            auto isEscaped = [](wchar_t ch) {
                return ch == 0 || ch == 10 || ch == 11 || ch == 12 || ch == 13;
            };
            bool hasEscaped = false;
            for (wchar_t ch : s) {
                if (isEscaped(ch)) {
                    hasEscaped = true;
                    break;
                }
            }
            if (!hasEscaped) {
                return quoteChar(s);
            }
            std::wstring out = L"[";
            std::wstring literal;
            bool first = true;
            auto append = [&](const std::wstring& piece) {
                if (!first) {
                    out += L" ";
                }
                out += piece;
                first = false;
            };
            for (wchar_t ch : s) {
                if (isEscaped(ch)) {
                    append(quoteChar(literal));
                    literal.clear();
                    append(L"char(" + std::to_wstring((int)ch) + L")");
                } else {
                    literal += ch;
                }
            }
            append(quoteChar(literal));
            out += L"]";
            return out;
        };
        if (rows <= 1) {
            std::wstring rowStr;
            for (indexType c = 0; c < cols; c++) {
                rowStr += (wchar_t)pChar[c * rows];
            }
            res += renderCharRow(rowStr);
        } else {
            res += L"[";
            for (indexType r = 0; r < rows; r++) {
                std::wstring rowStr;
                for (indexType c = 0; c < cols; c++) {
                    rowStr += (wchar_t)pChar[r + c * rows];
                }
                res += renderCharRow(rowStr);
                if (r < rows - 1) {
                    res += L";";
                }
            }
            res += L"]";
        }
    } else if (A.dataClass() == NLS_STRING_ARRAY) {
        // String arrays: each element is a double-quoted string; internal double
        // quotes are doubled. Scalars are printed without brackets.
        auto* elts = (ArrayOf*)A.dataPointer();
        indexType rows = dims.rows();
        indexType cols = dims.columns();
        auto quoteStr = [](const ArrayOf& e) -> std::wstring {
            if (e.isCharacterArray()) {
                std::wstring s = e.asWideString();
                StringHelpers::replace_all(s, L"\"", L"\"\"");
                return L"\"" + s + L"\"";
            }
            return L"string(missing)";
        };
        if (A.isScalar()) {
            res += quoteStr(elts[0]);
        } else {
            res += L"[";
            for (indexType r = 0; r < rows; r++) {
                for (indexType c = 0; c < cols; c++) {
                    res += quoteStr(elts[r + c * rows]);
                    if (c < cols - 1) {
                        res += L" ";
                    }
                }
                if (r < rows - 1) {
                    res += L";";
                }
            }
            res += L"]";
        }
    } else {
        if (!A.isScalar()) {
            res += L"[";
        }
        switch (A.dataClass()) {
        default: {
            Error(L"Nelson:mat2str:typeNotSupported", L"Type not supported.");
        } break;
        case NLS_SCOMPLEX: {
            auto* pValue = (single*)A.dataPointer();
            auto* pComplexValue = reinterpret_cast<singlecomplex*>(pValue);
            for (indexType x = 0; x < dims.rows(); x++) {
                for (indexType y = 0; y < dims.columns(); y++) {
                    singlecomplex complexVal = pComplexValue[x + y * dims.rows()];
                    res += complexToString<singlecomplex>(complexVal, formatNumber) + L" ";
                }
                if (StringHelpers::ends_with(res, L" ")) {
                    res.pop_back();
                }
                res += L";";
            }
        } break;
        case NLS_DCOMPLEX: {
            auto* pValue = (double*)A.dataPointer();
            auto* pComplexValue = reinterpret_cast<doublecomplex*>(pValue);
            for (indexType x = 0; x < dims.rows(); x++) {
                for (indexType y = 0; y < dims.columns(); y++) {
                    doublecomplex complexVal = pComplexValue[x + y * dims.rows()];
                    res += complexToString<doublecomplex>(complexVal, formatNumber) + L" ";
                }
                if (StringHelpers::ends_with(res, L" ")) {
                    res.pop_back();
                }
                res += L";";
            }
        } break;
        case NLS_DOUBLE: {
            auto* pValue = (double*)A.dataPointer();
            for (indexType x = 0; x < dims.rows(); x++) {
                for (indexType y = 0; y < dims.columns(); y++) {
                    double val = pValue[x + y * dims.rows()];
                    if (std::isnan(val)) {
                        res += L"NaN ";
                    } else {
                        if (std::isinf(val)) {
                            if (val > 0) {
                                res += L"Inf ";
                            } else {
                                res += L"-Inf ";
                            }
                        } else {
                            res += fmt::format(
                                       fmt::runtime(formatNumber), normalizeNegativeZero(val))
                                + L" ";
                        }
                    }
                }
                if (StringHelpers::ends_with(res, L" ")) {
                    res.pop_back();
                }
                res += L";";
            }
        } break;
        case NLS_SINGLE: {
            auto* pValue = (single*)A.dataPointer();
            for (indexType x = 0; x < dims.rows(); x++) {
                for (indexType y = 0; y < dims.columns(); y++) {
                    single val = pValue[x + y * dims.rows()];
                    if (std::isnan(val)) {
                        res += L"NaN ";
                    } else {
                        if (std::isinf(val)) {
                            if (val > 0) {
                                res += L"Inf ";
                            } else {
                                res += L"-Inf ";
                            }
                        } else {
                            res += fmt::format(
                                       fmt::runtime(formatNumber), normalizeNegativeZero(val))
                                + L" ";
                        }
                    }
                }
                if (StringHelpers::ends_with(res, L" ")) {
                    res.pop_back();
                }
                res += L";";
            }
        } break;
        case NLS_LOGICAL: {
            auto* pValue = (logical*)A.dataPointer();
            for (indexType x = 0; x < dims.rows(); x++) {
                for (indexType y = 0; y < dims.columns(); y++) {
                    if (pValue[x + y * dims.rows()] == 1) {
                        res += L"true ";
                    } else {
                        res += L"false ";
                    }
                }
                if (StringHelpers::ends_with(res, L" ")) {
                    res.pop_back();
                }
                res += L";";
            }
        } break;
        case NLS_INT8:
        case NLS_INT16:
        case NLS_INT32:
        case NLS_INT64: {
            A.promoteType(NLS_INT64);
            auto* pValue = (int64*)A.dataPointer();
            for (indexType x = 0; x < dims.rows(); x++) {
                for (indexType y = 0; y < dims.columns(); y++) {
                    res += std::to_wstring(pValue[x + y * dims.rows()]) + L" ";
                }
                if (StringHelpers::ends_with(res, L" ")) {
                    res.pop_back();
                }
                res += L";";
            }
        } break;
        case NLS_UINT8:
        case NLS_UINT16:
        case NLS_UINT32:
        case NLS_UINT64: {
            A.promoteType(NLS_UINT64);
            auto* pValue = (uint64*)A.dataPointer();
            for (indexType x = 0; x < dims.rows(); x++) {
                for (indexType y = 0; y < dims.columns(); y++) {
                    res += std::to_wstring(pValue[x + y * dims.rows()]) + L" ";
                }
                if (StringHelpers::ends_with(res, L" ")) {
                    res.pop_back();
                }
                res += L";";
            }
        } break;
        }
        if (StringHelpers::ends_with(res, L";")) {
            res.pop_back();
        }
        if (!A.isScalar()) {
            res += L"]";
        }
    }
    if (withClass) {
        res += L")";
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
