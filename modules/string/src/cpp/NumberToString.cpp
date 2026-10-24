//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include <cstdio>
#include <cmath>
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <algorithm>
#include <cstring>
#include <vector>
#include "StringHelpers.hpp"
#include "NumberToString.hpp"
#include "PrintfFunction.hpp"
#include "nlsBuildConfig.h"
#include "characters_encoding.hpp"
#include "RealPart.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// A computed -0.0 prints as "0", not "-0". signbit + fabs is not elided under
// fast floating-point optimisation.
static inline double
normalizeNegativeZero(double v)
{
    if (v == 0.0 && std::signbit(v)) {
        return std::fabs(v);
    }
    return v;
}
//=============================================================================
enum NUM2STR_ENUM
{
    AUTO = 0,
    PRECISION = 1,
    FORMAT = 2
};
//=============================================================================
// ends each chunk of values in the formatted row
static const wchar_t CHUNK_END = L'\x01';
//=============================================================================
struct FormatPiece
{
    std::wstring text; // literal text or the whole conversion ('%08.3f')
    wchar_t conversion; // 0 for a literal text
};
//=============================================================================
static std::vector<FormatPiece>
splitFormat(const std::wstring& format);
static std::wstring
imaginaryConversion(const FormatPiece& piece);
static bool
formatRow(const ArrayOf& values, const std::vector<FormatPiece>& pieces, bool isComplex,
    bool imaginary, indexType row, indexType rows, indexType columns, size_t perChunk,
    std::vector<std::wstring>& chunks);
static wstringVector
trimBlankColumns(wstringVector lines);
static bool
isPlainFloatingFormat(const std::vector<FormatPiece>& pieces);
static bool
formatPlainReal(const ArrayOf& values, const std::wstring& format, indexType rows,
    indexType columns, wstringVector& lines);
static wstringVector
alignChunks(
    const std::vector<std::vector<std::pair<std::wstring, std::wstring>>>& chunks, bool isComplex);
//=============================================================================
template <class T>
static ArrayOf
NumberToStringHelperLogical(const ArrayOf& A, NUM2STR_ENUM formatType, const std::wstring& format)
{
    std::string uformat;
    if (formatType == NUM2STR_ENUM::FORMAT) {
        uformat = wstring_to_utf8(format);
        StringHelpers::replace_all(uformat, "%%", "%");
    } else if (formatType == NUM2STR_ENUM::PRECISION) {
        uformat = wstring_to_utf8(format);
    } else if (formatType == NUM2STR_ENUM::AUTO) {
        uformat = "%2d";
    }
    T* dp = (T*)A.dataPointer();
    size_t maxlen = 1;
    stringVector rows;
    Dimensions dimsA = A.dimensions();
    indexType m = dimsA.rows();
    indexType n = dimsA.columns();
    std::string s;
    for (indexType i = 0; i < m; i++) {
        std::string row;
        for (indexType j = 0; j < n; j++) {
            try {
                s = fmt::sprintf(uformat, (int)dp[j * m + i]);
            } catch (fmt::format_error&) {
                Error(L"Nelson:string:wrongFormatString", L"Wrong format string.");
            }
            if (formatType == NUM2STR_ENUM::AUTO) {
                int l = (int)(maxlen - s.length() + 2);
                row += std::string(l > 0 ? l : 2, ' ');
            }
            row += s;
        }
        StringHelpers::trim_left(row);
        rows.push_back(row);
    }
    return ArrayOf::characterArrayFromRows(rows);
}
//=============================================================================
template <class T>
static ArrayOf
NumberToStringHelperInteger(const ArrayOf& A)
{
    T* dp = (T*)A.dataPointer();
    Dimensions dimsA = A.dimensions();
    long double maxAbsValue = fabsl(static_cast<long double>(dp[0]));
    for (indexType i = 0; i < dimsA.elementCount(); i++) {
        maxAbsValue = std::max(maxAbsValue, fabsl(static_cast<long double>(dp[i])));
    }
    std::string maxString = fmt::to_string(static_cast<T>(maxAbsValue));
    size_t maxlen = maxString.size();
    stringVector rows;
    indexType m = dimsA.rows();
    indexType n = dimsA.columns();
    for (indexType i = 0; i < m; i++) {
        std::string row;
        for (indexType j = 0; j < n; j++) {
            std::string s = fmt::to_string(dp[j * m + i]);
            int l = (int)(maxlen - s.length() + 2);
            row += std::string(l > 0 ? l : 2, ' ');
            row += s;
        }
        StringHelpers::trim_left(row);
        rows.push_back(row);
    }
    return ArrayOf::characterArrayFromRows(rows);
}
//=============================================================================
template <class T>
static ArrayOf
NumberToStringHelperComplex(
    const ArrayOf& A, NUM2STR_ENUM formatType, const std::wstring& format, int precision)
{
    std::string uformat;
    T* dp = (T*)A.dataPointer();
    std::complex<T>* dpz = reinterpret_cast<std::complex<T>*>(dp);
    Dimensions dimsA = A.dimensions();
    bool allint = true;
    size_t maxlen = 0;

    if (formatType == NUM2STR_ENUM::AUTO) {
        long double maxAbsValue = std::nan("");
        for (indexType i = 0; i < dimsA.elementCount(); i++) {
            long double value_real = fabsl((long double)dpz[i].real());
            long double value_imag = fabsl((long double)dpz[i].imag());
            if (std::isfinite(value_real)) {
                if (std::isnan(maxAbsValue)) {
                    maxAbsValue = value_real;
                }
                maxAbsValue = std::max(maxAbsValue, value_real);
                allint = allint && (dpz[i].real() == round(dpz[i].real()));
            } else {
                allint = false;
            }
            if (std::isfinite(value_imag)) {
                if (std::isnan(maxAbsValue)) {
                    maxAbsValue = value_imag;
                }
                maxAbsValue = std::max(maxAbsValue, value_imag);
                allint = allint && (dpz[i].imag() == round(dpz[i].imag()));
            } else {
                allint = false;
            }
        }
        int ndigit = 0;
        if (std::isfinite(maxAbsValue)) {
            ndigit = (int)std::floor(log10(maxAbsValue));
        }

        if (ndigit > 15 || !allint) {
            if (dimsA.isScalar()) {
                ndigit = std::max(ndigit + 6, 6);
            } else {

                ndigit = std::max(ndigit + 5, 5);
            }
            ndigit = std::min(ndigit, 16);
            uformat = "%" + fmt::to_string(ndigit + 7) + "." + fmt::to_string(ndigit) + "g";
        } else {
            ndigit += 3;
            if (!std::isfinite(maxAbsValue)) {
                ndigit = std::max(ndigit, 5);
            }
            uformat = "%" + fmt::to_string(ndigit) + ".0f";
        }
        std::string s;
        try {
            if (allint) {
                s = fmt::sprintf(uformat, static_cast<long double>(maxAbsValue));
            } else {
                s = fmt::sprintf(uformat, maxAbsValue);
            }
        } catch (fmt::format_error&) {
            Error(L"Nelson:string:wrongFormatString", L"Wrong format string.");
        }

        precision = ndigit;
        maxlen = s.size();
    } else if (formatType == NUM2STR_ENUM::FORMAT) {
        uformat = wstring_to_utf8(format);
        StringHelpers::replace_all(uformat, "%%", "%");
    } else if (formatType == NUM2STR_ENUM::PRECISION) {
        uformat = wstring_to_utf8(format);
    }
    stringVector rows;
    indexType m = dimsA.rows();
    indexType n = dimsA.columns();
    for (indexType i = 0; i < m; i++) {
        std::string row;
        for (indexType j = 0; j < n; j++) {
            std::string strRealPart;
            try {
                if (allint) {
                    strRealPart = fmt::sprintf(uformat,
                        static_cast<long double>(normalizeNegativeZero(dpz[j * m + i].real())));
                } else {
                    strRealPart
                        = fmt::sprintf(uformat, normalizeNegativeZero(dpz[j * m + i].real()));
                }
            } catch (fmt::format_error&) {
                Error(L"Nelson:string:wrongFormatString", L"Wrong format string.");
            }
            std::string strImagPart;
            try {
                if (allint) {
                    strImagPart = fmt::sprintf(uformat,
                        static_cast<long double>(normalizeNegativeZero(dpz[j * m + i].imag())));
                } else {
                    strImagPart
                        = fmt::sprintf(uformat, normalizeNegativeZero(dpz[j * m + i].imag()));
                }
            } catch (fmt::format_error&) {
                Error(L"Nelson:string:wrongFormatString", L"Wrong format string.");
            }
            StringHelpers::erase_all(strImagPart, " ");
            std::string s;
            if (dpz[j * m + i].imag() < 0) {
                s = strRealPart + strImagPart + "i";
            } else {
                s = strRealPart + "+" + strImagPart + "i";
            }
            StringHelpers::replace_all(s, "inf", "Inf");
            StringHelpers::replace_all(s, "nan", "NaN");
            size_t nbSpace = 0;
            if (formatType == NUM2STR_ENUM::FORMAT) {
                nbSpace = 0;
            } else if (formatType == NUM2STR_ENUM::AUTO) {
                if (allint) {
                    nbSpace = (size_t)1;
                } else {
                    nbSpace = (size_t)10;
                }
            } else if (formatType == NUM2STR_ENUM::PRECISION) {
                nbSpace = (size_t)5 + (size_t)precision;
            }
            row += std::string(nbSpace, ' ');
            row += s;
        }
        StringHelpers::trim_left(row);
        rows.push_back(row);
    }
    return ArrayOf::characterArrayFromRows(rows);
}
//=============================================================================
template <class T>
static ArrayOf
NumberToStringHelperReal(
    const ArrayOf& A, NUM2STR_ENUM formatType, const std::wstring& format, int precision)
{
    std::string uformat;
    T* dp = (T*)A.dataPointer();
    Dimensions dimsA = A.dimensions();
    bool allint = true;
    size_t maxlen = 0;

    if (formatType == NUM2STR_ENUM::AUTO) {
        long double maxAbsValue = std::nan("");
        for (indexType i = 0; i < dimsA.elementCount(); i++) {
            long double value_real = fabsl((long double)dp[i]);
            if (std::isfinite(value_real)) {
                if (std::isnan(maxAbsValue)) {
                    maxAbsValue = value_real;
                }
                maxAbsValue = std::max(maxAbsValue, value_real);
                allint = allint && (dp[i] == round(dp[i]));
            } else {
                allint = false;
            }
        }
        int ndigit = 0;
        if (std::isfinite(maxAbsValue)) {
            if (maxAbsValue == 0.) {
                ndigit = 0;
            } else {
                ndigit = (int)std::floor(log10(maxAbsValue));
            }
        }
        if (ndigit > 15 || !allint) {
            // Significant digits: floor(log10(maxAbs)) + 5 (num2str(pi) -> "3.1416").
            ndigit = std::max(ndigit + 5, 5);
            ndigit = std::min(ndigit, 16);
            uformat = "%" + fmt::to_string(ndigit + 7) + "." + fmt::to_string(ndigit) + "g";
        } else {
            ndigit += 3;
            if (!std::isfinite(maxAbsValue)) {
                ndigit = std::max(ndigit, 5);
            }
            uformat = "%" + fmt::to_string(ndigit) + ".0f";
        }
        std::string s;
        try {
            if (allint) {
                s = fmt::sprintf(uformat, static_cast<long double>(maxAbsValue));
            } else {
                s = fmt::sprintf(uformat, maxAbsValue);
            }
        } catch (fmt::format_error&) {
            Error(L"Nelson:string:wrongFormatString", L"Wrong format string.");
        }
        precision = ndigit;
        maxlen = s.size();
    } else if (formatType == NUM2STR_ENUM::FORMAT) {
        uformat = wstring_to_utf8(format);
        StringHelpers::replace_all(uformat, "%%", "%");
    } else if (formatType == NUM2STR_ENUM::PRECISION) {
        uformat = wstring_to_utf8(format);
    }
    stringVector rows;
    indexType m = dimsA.rows();
    indexType n = dimsA.columns();
    for (indexType i = 0; i < m; i++) {
        std::string row;
        for (indexType j = 0; j < n; j++) {
            std::string s;
            try {
                if (allint) {
                    s = fmt::sprintf(
                        uformat, static_cast<long double>(normalizeNegativeZero(dp[j * m + i])));
                } else {
                    s = fmt::sprintf(uformat, normalizeNegativeZero(dp[j * m + i]));
                }
            } catch (fmt::format_error&) {
                // An integer conversion specifier (e.g. "%d") rejects a
                // floating-point argument: retry with the value rounded to an
                // integer (num2str(5, '%d') -> "5").
                try {
                    s = fmt::sprintf(
                        uformat, static_cast<long long>(std::llround((double)dp[j * m + i])));
                } catch (fmt::format_error&) {
                    Error(L"Nelson:string:wrongFormatString", L"Wrong format string.");
                }
            }
            StringHelpers::replace_all(s, "inf", "Inf");
            StringHelpers::replace_all(s, "nan", "NaN");
            row += s;
        }
        StringHelpers::trim_left(row);
        rows.push_back(row);
    }
    return ArrayOf::characterArrayFromRows(rows);
}
//=============================================================================
static ArrayOf
NumberToString(const ArrayOf& A, NUM2STR_ENUM formatType, const std::wstring& format, int precision,
    bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (A.isEmpty()) {
        return ArrayOf::characterArray(L"");
    }
    ArrayOf as2D;
    if (!A.is2D() && !A.isScalar()) {
        as2D = A;
        Dimensions dimsA = A.dimensions();
        indexType N = dimsA.elementCount();
        Dimensions dims2d(dimsA.rows(), N / dimsA.rows());
        as2D.reshape(dims2d);
    } else if (A.isSparse()) {
        as2D = A;
        as2D.makeDense();
    } else {
        as2D = A;
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE: {
        res = NumberToStringHelperReal<double>(as2D, formatType, format, precision);
    } break;
    case NLS_DCOMPLEX: {
        if (A.allReal()) {
            ArrayOf B = RealPart(as2D);
            res = NumberToStringHelperReal<double>(B, formatType, format, precision);
        } else {
            res = NumberToStringHelperComplex<double>(as2D, formatType, format, precision);
        }
    } break;
    case NLS_SINGLE: {
        res = NumberToStringHelperReal<single>(as2D, formatType, format, precision);
    } break;
    case NLS_SCOMPLEX: {
        if (as2D.allReal()) {
            ArrayOf B = RealPart(as2D);
            res = NumberToStringHelperReal<single>(B, formatType, format, precision);
        } else {
            res = NumberToStringHelperComplex<single>(as2D, formatType, format, precision);
        }
    } break;
    case NLS_LOGICAL: {
        res = NumberToStringHelperLogical<logical>(as2D, formatType, format);
    } break;
    case NLS_UINT8: {
        res = NumberToStringHelperInteger<uint8>(as2D);
    } break;
    case NLS_INT8: {
        res = NumberToStringHelperInteger<int8>(as2D);
    } break;
    case NLS_UINT16: {
        res = NumberToStringHelperInteger<uint16>(as2D);
    } break;
    case NLS_INT16: {
        res = NumberToStringHelperInteger<int16>(as2D);
    } break;
    case NLS_UINT32: {
        res = NumberToStringHelperInteger<uint32>(as2D);
    } break;
    case NLS_INT32: {
        res = NumberToStringHelperInteger<int32>(as2D);
    } break;
    case NLS_UINT64: {
        res = NumberToStringHelperInteger<uint64>(as2D);
    } break;
    case NLS_INT64: {
        res = NumberToStringHelperInteger<int64>(as2D);
    } break;
    case NLS_CHAR: {
        res = A;
    } break;
    case NLS_STRING_ARRAY: {
        // a string scalar is returned as characters, a missing string as ''
        if (!A.isScalar()) {
            needToOverload = true;
            break;
        }
        const auto* element = static_cast<const ArrayOf*>(A.dataPointer());
        res = ArrayOf::characterArray(
            element[0].isCharacterArray() ? element[0].asWideString() : L"");
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    return res;
}
//=============================================================================
ArrayOf
NumberToString(const ArrayOf& A, bool& needToOverload)
{
    return NumberToString(A, NUM2STR_ENUM::AUTO, L"", 0, needToOverload);
}
//=============================================================================
// num2str(A, format): the format is applied with sprintf semantics to chunks
// of as many values as it has conversions; the chunks of a row are joined and
// the rows form a character matrix stripped of its common leading and
// trailing blank columns. Real chunks are right-aligned to the longest one.
// The imaginary part of a complex value is formatted separately (signed,
// without width, followed by 'i') and appended to the real part.
ArrayOf
NumberToString(const ArrayOf& A, const std::wstring& format, bool& needToOverload)
{
    needToOverload = false;
    if (A.isCharacterArray() || A.isStringArray()) {
        return ArrayOf::characterArray(A.asWideString());
    }
    if (!A.isNumeric() && !A.isLogical()) {
        needToOverload = true;
        return {};
    }
    if (A.isEmpty()) {
        return ArrayOf::characterArray(L"");
    }
    if (format.find(L'%') == std::wstring::npos) {
        Error(L"Nelson:string:wrongFormatString", L"Wrong format string.");
    }
    ArrayOf values(A);
    if (values.isSparse()) {
        values.makeDense();
    }
    Dimensions dimsA = values.dimensions();
    indexType rows = dimsA.rows();
    indexType columns = dimsA.elementCount() / rows;
    bool isComplex = values.isComplex();
    if (isComplex) {
        // a complex array whose imaginary parts are all zero is real
        values.promoteType(NLS_DCOMPLEX);
        const auto* z = static_cast<const double*>(values.dataPointer());
        isComplex = false;
        for (indexType k = 0; k < dimsA.elementCount() && !isComplex; ++k) {
            isComplex = z[2 * k + 1] != 0;
        }
        if (!isComplex) {
            values = RealPart(values);
        }
    }
    if (!isComplex && values.dataClass() != NLS_INT64 && values.dataClass() != NLS_UINT64) {
        values.promoteType(NLS_DOUBLE);
    }
    std::vector<FormatPiece> pieces = splitFormat(format);
    size_t perChunk = 0;
    for (const auto& piece : pieces) {
        perChunk += (piece.conversion != 0) ? 1 : 0;
    }
    if (perChunk == 0) {
        perChunk = std::max<size_t>(columns, 1); // no conversion: the text is written once
    }
    wstringVector plainLines;
    if (!isComplex && values.dataClass() == NLS_DOUBLE && isPlainFloatingFormat(pieces)
        && formatPlainReal(values, format, rows, columns, plainLines)) {
        return ArrayOf::characterArrayFromRows(trimBlankColumns(plainLines), false);
    }
    // chunks[r][j] = { real part, imaginary part }
    std::vector<std::vector<std::pair<std::wstring, std::wstring>>> chunks(rows);
    std::vector<std::wstring> realChunks;
    std::vector<std::wstring> imaginaryChunks;
    for (indexType r = 0; r < rows; ++r) {
        if (!formatRow(values, pieces, isComplex, false, r, rows, columns, perChunk, realChunks)
            || (isComplex
                && !formatRow(
                    values, pieces, true, true, r, rows, columns, perChunk, imaginaryChunks))) {
            return ArrayOf::characterArray(L"");
        }
        for (size_t j = 0; j < realChunks.size(); ++j) {
            if (isComplex) {
                // the imaginary part follows the real one without blanks
                StringHelpers::trim_right(realChunks[j]);
            }
            chunks[r].emplace_back(realChunks[j], isComplex ? imaginaryChunks[j] : L"");
        }
    }
    wstringVector lines = alignChunks(chunks, isComplex);
    return ArrayOf::characterArrayFromRows(trimBlankColumns(lines), false);
}
//=============================================================================
// A format is a sequence of literal texts and conversions ('%%' stays in the
// literal text).
static std::vector<FormatPiece>
splitFormat(const std::wstring& format)
{
    std::vector<FormatPiece> pieces;
    std::wstring literal;
    for (size_t k = 0; k < format.size(); ++k) {
        if (format[k] != L'%') {
            literal.push_back(format[k]);
            continue;
        }
        if (k + 1 < format.size() && format[k + 1] == L'%') {
            literal.append(L"%%");
            ++k;
            continue;
        }
        size_t end = format.find_first_of(L"diouxXfFeEgGcsaA", k + 1);
        if (end == std::wstring::npos) {
            literal.append(format, k, std::wstring::npos);
            break;
        }
        if (!literal.empty()) {
            pieces.push_back({ literal, 0 });
            literal.clear();
        }
        pieces.push_back({ format.substr(k, end - k + 1), format[end] });
        k = end;
    }
    if (!literal.empty()) {
        pieces.push_back({ literal, 0 });
    }
    return pieces;
}
//=============================================================================
// '%08.3f' -> '%s%.3fi': the sign is printed apart from the magnitude, the
// width and the flags are dropped; a character conversion keeps its own form.
static std::wstring
imaginaryConversion(const FormatPiece& piece)
{
    if (piece.conversion == L'c' || piece.conversion == L's') {
        return L"%" + std::wstring(1, piece.conversion) + L"i";
    }
    std::wstring spec = L"%s%";
    if (piece.text.find(L'#') != std::wstring::npos) {
        spec.push_back(L'#');
    }
    size_t precision = piece.text.find(L'.');
    if (precision != std::wstring::npos) {
        spec.append(piece.text, precision, piece.text.size() - 1 - precision);
    }
    spec.push_back(piece.conversion);
    spec.push_back(L'i');
    return spec;
}
//=============================================================================
// sprintf(format, values) for a whole row (the real or the imaginary part when
// the values are complex): the format ends with a sentinel that tells where each
// chunk of values ends; the last chunk may be incomplete, it then loses the text
// that precedes the missing value. False when the format is not valid.
static bool
formatRow(const ArrayOf& values, const std::vector<FormatPiece>& pieces, bool isComplex,
    bool imaginary, indexType row, indexType rows, indexType columns, size_t perChunk,
    std::vector<std::wstring>& chunks)
{
    std::wstring format;
    std::vector<const FormatPiece*> conversions;
    for (const auto& piece : pieces) {
        format.append(imaginary && piece.conversion != 0 ? imaginaryConversion(piece) : piece.text);
        if (piece.conversion != 0) {
            conversions.push_back(&piece);
        }
    }
    format.push_back(CHUNK_END);
    ArrayOfVector args;
    args << ArrayOf::characterArray(format);
    if (isComplex || values.dataClass() == NLS_DOUBLE) {
        const auto* d = static_cast<const double*>(values.dataPointer());
        int step = isComplex ? 2 : 1;
        int offset = (isComplex && imaginary) ? 1 : 0;
        std::vector<double> v(columns);
        for (indexType c = 0; c < columns; ++c) {
            v[c] = d[step * (row + c * rows) + offset];
        }
        if (!imaginary) {
            args << ArrayOf::doubleRowVector(v);
        } else {
            // the sign is printed apart from the magnitude (except for characters)
            for (indexType c = 0; c < columns; ++c) {
                wchar_t conversion
                    = conversions.empty() ? L'd' : conversions[c % perChunk]->conversion;
                if (conversion == L'c' || conversion == L's') {
                    args << ArrayOf::doubleScalar(v[c]);
                } else {
                    args << ArrayOf::characterArray(v[c] < 0 ? L"-" : L"+");
                    args << ArrayOf::doubleScalar(std::fabs(v[c]));
                }
            }
        }
    } else {
        // 64-bit integers keep all their digits
        NelsonType cls = values.dataClass();
        size_t size = values.elementSize();
        void* ptr = ArrayOf::allocateArrayOf(cls, columns, stringVector(), false);
        const auto* src = static_cast<const char*>(values.dataPointer());
        for (indexType c = 0; c < columns; ++c) {
            memcpy(static_cast<char*>(ptr) + c * size, src + (row + c * rows) * size, size);
        }
        args << ArrayOf(cls, Dimensions(1, columns), ptr);
    }
    std::wstring errorMessage;
    std::wstring text;
    if (!printfFunction(args, errorMessage, text)) {
        return false;
    }
    chunks.clear();
    size_t start = 0;
    for (size_t end = text.find(CHUNK_END); end != std::wstring::npos;
        end = text.find(CHUNK_END, start)) {
        chunks.push_back(text.substr(start, end - start));
        start = end + 1;
    }
    if (start < text.size() || chunks.empty()) {
        chunks.push_back(text.substr(start));
        StringHelpers::trim_right(chunks.back());
    }
    return true;
}
//=============================================================================
// "%8.3f"-like format: one floating conversion, plain ASCII text around it and no
// escape sequence, so each value can be formatted directly.
static bool
isPlainFloatingFormat(const std::vector<FormatPiece>& pieces)
{
    size_t conversions = 0;
    for (const auto& piece : pieces) {
        for (wchar_t c : piece.text) {
            if (c > 127 || c == L'\\' || (piece.conversion == 0 && c == L'%')
                || (piece.conversion != 0
                    && std::wcschr(L"%-+ #0123456789.fFeEgG", c) == nullptr)) {
                return false;
            }
        }
        if (piece.conversion != 0) {
            conversions += (std::wcschr(L"fFeEgG", piece.conversion) != nullptr) ? 1 : 100;
        }
    }
    return conversions == 1;
}
//=============================================================================
// Right-aligned values of a real matrix with a plain floating format; false when
// a value is not finite (the general path prints it).
static bool
formatPlainReal(const ArrayOf& values, const std::wstring& format, indexType rows,
    indexType columns, wstringVector& lines)
{
    const auto* d = static_cast<const double*>(values.dataPointer());
    const std::string utf8Format = wstring_to_utf8(format);
    std::wstring all;
    std::vector<size_t> ends(static_cast<size_t>(rows * columns));
    size_t width = 0;
    size_t previousEnd = 0;
    try {
        for (indexType r = 0; r < rows; ++r) {
            for (indexType c = 0; c < columns; ++c) {
                double value = d[r + c * rows];
                if (!std::isfinite(value)) {
                    return false;
                }
                const std::string text = fmt::sprintf(utf8Format, value);
                all.append(text.begin(), text.end());
                size_t end = all.size();
                ends[static_cast<size_t>(r * columns + c)] = end;
                width = std::max(width, end - previousEnd);
                previousEnd = end;
            }
        }
    } catch (fmt::format_error&) {
        return false;
    }
    lines.assign(static_cast<size_t>(rows), std::wstring());
    size_t start = 0;
    for (indexType r = 0; r < rows; ++r) {
        lines[r].reserve(static_cast<size_t>(columns) * width);
        for (indexType c = 0; c < columns; ++c) {
            size_t end = ends[static_cast<size_t>(r * columns + c)];
            lines[r].append(width - (end - start), L' ');
            lines[r].append(all, start, end - start);
            start = end;
        }
    }
    return true;
}
//=============================================================================
// Real values: all the chunks are right-aligned to the longest one. Complex
// values: per chunk position, real parts are right-aligned and imaginary
// parts left-aligned.
static wstringVector
alignChunks(
    const std::vector<std::vector<std::pair<std::wstring, std::wstring>>>& chunks, bool isComplex)
{
    size_t count = chunks.empty() ? 0 : chunks[0].size();
    std::vector<size_t> realWidth(count, 0);
    std::vector<size_t> imagWidth(count, 0);
    size_t width = 0;
    for (const auto& row : chunks) {
        for (size_t j = 0; j < count; ++j) {
            realWidth[j] = std::max(realWidth[j], row[j].first.size());
            imagWidth[j] = std::max(imagWidth[j], row[j].second.size());
            width = std::max(width, row[j].first.size());
        }
    }
    wstringVector lines(chunks.size());
    for (size_t r = 0; r < chunks.size(); ++r) {
        for (size_t j = 0; j < count; ++j) {
            const auto& chunk = chunks[r][j];
            size_t w = isComplex ? realWidth[j] : width;
            lines[r].append(w - chunk.first.size(), L' ');
            lines[r].append(chunk.first);
            lines[r].append(chunk.second);
            lines[r].append(imagWidth[j] - chunk.second.size(), L' ');
        }
    }
    return lines;
}
//=============================================================================
// Removes the blank columns shared by all the rows, on the left and on the
// right.
static wstringVector
trimBlankColumns(wstringVector lines)
{
    const std::wstring blanks = L" \t\n\v\f\r";
    size_t lead = std::wstring::npos;
    size_t trail = std::wstring::npos;
    for (const auto& line : lines) {
        size_t first = line.find_first_not_of(blanks);
        size_t last = line.find_last_not_of(blanks);
        if (first == std::wstring::npos) {
            first = line.size();
            last = line.size();
        } else {
            last = line.size() - 1 - last;
        }
        lead = std::min(lead, first);
        trail = std::min(trail, last);
    }
    for (auto& line : lines) {
        size_t keep = line.size() - std::min(line.size(), lead + trail);
        line = line.substr(std::min(lead, line.size()), keep);
    }
    return lines;
}
//=============================================================================
ArrayOf
NumberToString(const ArrayOf& A, double N, bool& needToOverload)
{
    std::wstring format;
    if (!std::isfinite(N)) {
        Error("Nelson:string:PRECISIONScalarInteger0", "PRECISION must be a scalar integer >= 0.");
    }
    int precision = (int)N;
    if (A.dataClass() == NLS_LOGICAL) {
        format = L"%" + std::to_wstring(precision + 7) + L"d";
    } else {
        if (N < 0) {
            Error("Nelson:string:PRECISIONScalarInteger0",
                "PRECISION must be a scalar integer >= 0.");
        }
        format = L"%" + std::to_wstring(precision + 7) + L"." + std::to_wstring(precision) + L"g";
    }
    return NumberToString(A, NUM2STR_ENUM::PRECISION, format, precision, needToOverload);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
