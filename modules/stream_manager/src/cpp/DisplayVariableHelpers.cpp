//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <cstring>
#include <algorithm>
#include <limits>
#include <boost/algorithm/string.hpp>
#include <boost/rational.hpp>
#include "DisplayVariableHelpers.hpp"
#include "NelsonConfiguration.hpp"
#include "IEEEFP.hpp"
#include "FloatNumberToRational.hpp"
#include "characters_encoding.hpp"
//=============================================================================
#define BLANKS_AT_BOL L"    "
#define LENGTH_BLANKS_AT_BOL 4
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename I>
std::wstring
hexify(I w, size_t hex_len = sizeof(I) << 1)
{
    static const wchar_t* digits = L"0123456789ABCDEF";
    std::wstring rc(hex_len, '0');
    for (size_t i = 0, j = (hex_len - 1) * 4; i < hex_len; ++i, j -= 4) {
        rc[i] = digits[(w >> j) & 0x0f];
    }
    return rc;
}
//=============================================================================
template <class T>
bool
isInteger(T val)
{
    int truncated = (int)val;
    return (T)truncated == (T)val && val < 1e9;
}
//=============================================================================
template <class T>
void
getMinMax(T* val, indexType nbElements, T* min, T* max)
{
    T minValue = val[0];
    T maxValue = val[0];
    T shared_max = maxValue;
    T shared_min = minValue;

#pragma omp parallel
    {
#pragma omp for nowait
        for (ompIndexType idx = 0; idx < (ompIndexType)nbElements; ++idx) {
            maxValue = std::max(val[idx], maxValue);
            minValue = std::max(val[idx], minValue);
        }
#pragma omp critical
        {
            shared_max = std::max(shared_max, maxValue);
            shared_min = std::max(shared_min, minValue);
        }
    }
    *min = shared_min;
    *max = shared_max;
}
//=============================================================================
template <class T>
T
getMax(T* val, indexType nbElements)
{
    T maxValue = val[0];
    T shared_max = maxValue;
#pragma omp parallel
    {
#pragma omp for nowait
        for (ompIndexType idx = 0; idx < (ompIndexType)nbElements; ++idx) {
            maxValue = std::max(val[idx], maxValue);
        }
#pragma omp critical
        {
            shared_max = std::max(shared_max, maxValue);
        }
    }
    return shared_max;
}
//=============================================================================
template <class T>
T
getMin(T* val, indexType nbElements)
{
    T minValue = val[0];
    T shared_min = minValue;
#pragma omp parallel
    {
#pragma omp for nowait
        for (ompIndexType idx = 0; idx < (ompIndexType)nbElements; ++idx) {
            minValue = std::min(val[idx], minValue);
        }
#pragma omp critical
        {
            shared_min = std::max(shared_min, minValue);
        }
    }
    return shared_min;
}
//=============================================================================
static int
getCommonExponential(const ArrayOf& A)
{
    int exponent = 0;
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        double dMax = getMax<double>((double*)A.getDataPointer(), A.getElementCount());
        exponent = log10(dMax);
    } break;
    }
    return exponent;
}
//=============================================================================
static std::wstring
formatShortEng(double x)
{
    int exponent = 0;
    if (x != 0) {
        double absval = (x < 0 ? -x : x);
        int logabsval = static_cast<int>(std::floor(log10(absval)));
        if (logabsval < 0) {
            exponent = logabsval - 2 + ((-logabsval + 2) % 3);
        } else {
            exponent = logabsval - (logabsval % 3);
        }
    }
    double mantissa =  x / std::pow(static_cast<double>(10), exponent);
    std::wstring expStr;
    expStr.reserve(8);
    if (exponent >= 0) {
        expStr = L"e+";
    } else {
        exponent = -exponent;
        expStr = L"e-";
    }
    std::wstring exponentAsString = fmt::to_wstring(exponent);
    if (exponentAsString.length() < 3) {
        expStr.append(3 - exponentAsString.length(), L'0');
    }
    std::wstring format = L"%.4f%s%s";
    return fmt::sprintf(format, mantissa, expStr, exponentAsString);
}
//=============================================================================
static std::wstring
completeWithBlanksAtBeginning(const std::wstring& msg, size_t width)
{
    size_t len = msg.length();
    std::wstring blanks;
    if (int(width) - int(len) > 0) {
        blanks.append(width - len, L' ');
        return blanks + msg;
    }
    return msg.substr(0, width);
}
//=============================================================================
static std::wstring
lightDescription(const ArrayOf& A, const std::wstring& firstChar, const std::wstring& lastChar);
//=============================================================================
static void
print2dString(Interface* io, const ArrayOf& A, LineSpacingDisplay currentLineSpacingDisplay);
//=============================================================================
static void
printNdString(Interface* io, const ArrayOf& A, LineSpacingDisplay currentLineSpacingDisplay);
//=============================================================================
static void
print2dCell(Interface* io, const ArrayOf& A, LineSpacingDisplay currentLineSpacingDisplay);
//=============================================================================
static void
printNdCell(Interface* io, const ArrayOf& A, LineSpacingDisplay currentLineSpacingDisplay);
//=============================================================================
static std::wstring
outputDoublePrecisionFloat(double num, NumericFormatDisplay currentNumericFormat,
    bool asComplexPart = false, int exponantial = 0);
//=============================================================================
static std::wstring
outputDoubleComplexPrecisionFloat(
    double realPart, double imagPart, NumericFormatDisplay currentNumericFormat);
//=============================================================================
static std::wstring
outputSinglePrecisionFloat(single num, NumericFormatDisplay currentNumericFormat);
//=============================================================================
static std::wstring
summarizeStringArray(const ArrayOf& A, size_t beginingLineLength, size_t termWidth);
//=============================================================================
static std::wstring
summarizeCellEntry(const ArrayOf& A, size_t beginingLineLength, size_t termWidth);
//=============================================================================
static void
emitElement(Interface* io, const void* dp, indexType num, Class dcls,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing,
    indexType width, int exponential);
//=============================================================================
static std::wstring
sprintElement(const void* dp, indexType num, Class dcls, NumericFormatDisplay currentNumericFormat,
    LineSpacingDisplay currentLineSpacing, indexType width, int exponential);
//=============================================================================
static indexType
getNominalWidth(const ArrayOf& A, NumericFormatDisplay currentNumericFormat);
//=============================================================================
static void
printValue(Interface* io, const ArrayOf& A, const std::wstring& name);
//=============================================================================
static void
printEmptyValue(Interface* io, const ArrayOf& A);
//=============================================================================
static void
printMatrixValue(Interface* io, const ArrayOf& A);
//=============================================================================
static void
printNDArrayValue(Interface* io, const ArrayOf& A, const std::wstring& name);
//=============================================================================
static std::wstring
getClassAsWideString(const ArrayOf& A)
{
    std::wstring typeAsText;
    switch (A.getDataClass()) {
    case NLS_GO_HANDLE:
        typeAsText = L"graphic_object";
        break;
    case NLS_HANDLE:
        typeAsText = L"handle";
        break;
    case NLS_UINT8:
        typeAsText = L"uint8";
        break;
    case NLS_INT8:
        typeAsText = L"int8";
        break;
    case NLS_UINT16:
        typeAsText = L"uint16";
        break;
    case NLS_INT16:
        typeAsText = L"int16";
        break;
    case NLS_UINT32:
        typeAsText = L"uint32";
        break;
    case NLS_INT32:
        typeAsText = L"int32";
        break;
    case NLS_UINT64:
        typeAsText = L"uint64";
        break;
    case NLS_INT64:
        typeAsText = L"int64";
        break;
    case NLS_SINGLE:
        typeAsText = L"single";
        break;
    case NLS_DOUBLE:
        typeAsText = L"double";
        break;
    case NLS_LOGICAL:
        typeAsText = L"logical";
        break;
    case NLS_CHAR:
        typeAsText = L"char";
        break;
    case NLS_SCOMPLEX:
        typeAsText = L"single";
        break;
    case NLS_DCOMPLEX:
        typeAsText = L"double";
        break;
    case NLS_CELL_ARRAY:
        typeAsText = L"cell";
        break;
    case NLS_STRUCT_ARRAY: {
        if (A.isClassStruct()) {
            typeAsText = utf8_to_wstring(A.getStructType());
        } else {
            typeAsText = L"struct";
        }
    } break;
    case NLS_STRING_ARRAY:
        typeAsText = L"string";
        break;
    default: {
    } break;
    }
    return typeAsText;
}
//=============================================================================
void
DisplayVariableHeader(Interface* io, const ArrayOf& A, const std::wstring& name)
{
    if (!name.empty()) {
        if (NelsonConfiguration::getInstance()->getLineSpacingDisplay()
            == NLS_LINE_SPACING_COMPACT) {
            io->outputMessage(name + L" =\n");
        } else {
            io->outputMessage(L"\n");
            io->outputMessage(name + L" =\n\n");
        }

        switch (A.getDataClass()) {
        case NLS_DCOMPLEX:
        case NLS_DOUBLE: {
        } break;
        default: {
            std::wstring typeAsText = getClassAsWideString(A);
            io->outputMessage(L"  <" + typeAsText + L"> - size: ");
            A.getDimensions().printMe(io);
            if (NelsonConfiguration::getInstance()->getLineSpacingDisplay()
                == NLS_LINE_SPACING_COMPACT) {
                io->outputMessage(L"\n");
            } else {
                io->outputMessage(L"\n");
            }
        } break;
        }
    }
}
//=============================================================================
void
DisplayVariableFooter(Interface* io, const ArrayOf& A, const std::wstring& name)
{
    if (!name.empty()) {
        if (NelsonConfiguration::getInstance()->getLineSpacingDisplay() == NLS_LINE_SPACING_LOOSE) {
            io->outputMessage(L"\n");
        }
    }
}
//=============================================================================
void
DisplayVariableValue(Interface* io, const ArrayOf& A, const std::wstring& name)
{
    switch (A.getDataClass()) {
    case NLS_DCOMPLEX:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
    case NLS_SINGLE: {
        printValue(io, A, name);
    } break;
    case NLS_CHAR: {
        if (A.isRowVectorCharacterArray()) {
            std::wstring msg = A.getContentAsWideString();
            if (msg.empty()) {
                if (name.empty()) {
                    io->outputMessage(L"");
                } else {
                    io->outputMessage(L"''\n");
                }
            } else {
                if (name.empty()) {
                    io->outputMessage(msg + L"\n");
                } else {
                    io->outputMessage(L"\'" + msg + L"\'\n");
                }
            }
        } else {
            printValue(io, A, name);
        }
    } break;
    default: {
        printValue(io, A, name);
    } break;
    }
}
//=============================================================================
static std::wstring
outputSingleComplexPrecisionFloat(
    double realPart, double imagPart, NumericFormatDisplay currentNumericFormat)
{
    std::wstring str;

    /*
    std::string realPart = outputDoublePrecisionFloat(ap[2 * num], currentNumericFormat, true);
    if (ap[2 * num + 1] < 0) {
        std::string imagPart
            = outputDoublePrecisionFloat(abs(ap[2 * num + 1]), currentNumericFormat, true);
        io->outputMessage(fmt::sprintf(" %s - %si", realPart, imagPart));
    } else {
        std::string imagPart
            = outputDoublePrecisionFloat(ap[2 * num + 1], currentNumericFormat, true);
        io->outputMessage(fmt::sprintf(" %s + %si", realPart, imagPart));
    }
    */
    /*
    io->outputMessage(outputDoublePrecisionFloat(ap[2 * num], currentNumericFormat));
    io->outputMessage(" ");
    io->outputMessage(outputDoublePrecisionFloat(ap[2 * num + 1], currentNumericFormat));
    io->outputMessage("i  ");
    */
    switch (currentNumericFormat) {
    case NumericFormatDisplay::NLS_NUMERIC_FORMAT_SHORT: {
    } break;
    default: {
    } break;
    }
    return str;
}

static std::wstring
outputDoubleComplexPrecisionFloat(
    double realPart, double imagPart, NumericFormatDisplay currentNumericFormat)
{
    std::wstring str;

    /*
    std::string realPart = outputDoublePrecisionFloat(ap[2 * num], currentNumericFormat, true);
    if (ap[2 * num + 1] < 0) {
        std::string imagPart
            = outputDoublePrecisionFloat(abs(ap[2 * num + 1]), currentNumericFormat, true);
        io->outputMessage(fmt::sprintf(" %s - %si", realPart, imagPart));
    } else {
        std::string imagPart
            = outputDoublePrecisionFloat(ap[2 * num + 1], currentNumericFormat, true);
        io->outputMessage(fmt::sprintf(" %s + %si", realPart, imagPart));
    }
    */
    /*
    io->outputMessage(outputDoublePrecisionFloat(ap[2 * num], currentNumericFormat));
    io->outputMessage(" ");
    io->outputMessage(outputDoublePrecisionFloat(ap[2 * num + 1], currentNumericFormat));
    io->outputMessage("i  ");
    */
    switch (currentNumericFormat) {
    case NumericFormatDisplay::NLS_NUMERIC_FORMAT_SHORT: {
    } break;
    default: {
    } break;
    }
    return str;
}
//=============================================================================
std::wstring
outputDoublePrecisionFloat(
    double number, NumericFormatDisplay currentNumericFormat, bool asComplexPart, int exponential)
{
    if (currentNumericFormat == NumericFormatDisplay::NLS_NUMERIC_FORMAT_PLUS) {
        return (number >= 0) ? L"+" : L"-";
    }
    std::wstring str;
    switch (currentNumericFormat) {
    case NumericFormatDisplay::NLS_NUMERIC_FORMAT_PLUS: {
    } break;
    case NumericFormatDisplay::NLS_NUMERIC_FORMAT_RATIONAL: {
        str = floatNumberToApproxRational(number, 9);
    } break;
    case NumericFormatDisplay::NLS_NUMERIC_FORMAT_SHORTENG: {
        if (IsInfinite(number)) {
            std::wstring format = L"%*s";
            if (number < 0) {
                str = fmt::sprintf(format, 9, L"-Inf");
            } else {
                str = fmt::sprintf(format, 9, L"Inf");
            }
            return str;
        } else if (IsNaN(number)) {
            std::wstring format = L"%*s";
            str = fmt::sprintf(format, 9, L"NaN");
            return str;
        } else {
            double _number = number;
            if (exponential != 0) {
                _number = number / pow(10, exponential);

                str = formatShortEng(_number);
            } else {
                _number = number / pow(10, exponential);

                str = formatShortEng(_number);
            }
        }
    } break;
    case NumericFormatDisplay::NLS_NUMERIC_FORMAT_SHORT: {
        if (IsInfinite(number)) {
            std::wstring format = L"%*s";
            if (number < 0) {
                str = fmt::sprintf(format, 9, L"-Inf");
            } else {
                str = fmt::sprintf(format, 9, L"Inf");
            }
            return str;
        } else if (IsNaN(number)) {
            std::wstring format = L"%*s";
            str = fmt::sprintf(format, 9, L"NaN");
            return str;
        }
        double _number = number;
        if (exponential != 0) {
            _number = number / pow(10, exponential);
            std::wstring format = L"%*.*f";
            str = fmt::sprintf(format, 9, 4, _number);
        } else {
            if (isInteger<double>(number)) {
                std::wstring format = L"%*.f";
                str = fmt::sprintf(format, 9, _number);
            } else if (fabs(number) > 1e-4 && number < 1e9) {
                std::wstring format = L"%*.*f";
                str = fmt::sprintf(format, 9, 4, _number);
            } else {
                std::wstring format = L"%*.*e";
                str = fmt::sprintf(format, 9, 4, _number);
            }
        }
    } break;
    }
    boost::algorithm::trim_left(str);
    return str;
}
//=============================================================================
std::wstring
outputSinglePrecisionFloat(single number, NumericFormatDisplay currentNumericFormat)
{
    if (currentNumericFormat == NumericFormatDisplay::NLS_NUMERIC_FORMAT_PLUS) {
        return (number >= 0) ? L"+" : L"-";
    }
    std::wstring str;
    std::wstring format;
    if (IsNaN(number)) {
        str = fmt::sprintf(L"NaN");
    } else if ((fabs(number) >= 0.1f && fabs(number) < 1.0f)
        || number <= std::numeric_limits<single>::epsilon()) {
        format = L"%0.8f";
    } else if (fabs(number) >= 0.01f && fabs(number) < 0.1f) {
        format = L"%0.9f";
    } else if (fabs(number) >= 0.001f && fabs(number) < 0.01f) {
        format = L"%0.10f";
    } else if (fabs(number) >= 1.0f && fabs(number) < 10.0f) {
        format = L"%1.7f";
    } else if (fabs(number) >= 10.0f && fabs(number) < 100.0f) {
        format = L"%2.6f";
    } else if (fabs(number) >= 100.0f && fabs(number) < 1000.0f) {
        format = L"%3.5f";
    } else {
        format = L"%1.7e";
    }
    str = fmt::sprintf(format, number);
    return completeWithBlanksAtBeginning(str, 24);
}
//=============================================================================
template <typename T>
std::wstring
outputSignedIntegerPrecision(T num, NumericFormatDisplay currentNumericFormat)
{
    std::wstring msg;
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_HEX: {
        msg = hexify<T>(num);
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        if (num == 0) {
            msg = L" ";
        } else if (num > 0) {
            msg = L"+";
        } else {
            msg = L"-";
        }
    } break;
    default: {
        msg = fmt::to_wstring(num);
    } break;
    }
    return msg;
}
//=============================================================================
template <typename T>
std::wstring
outputUnsignedIntegerPrecision(T num, NumericFormatDisplay currentNumericFormat)
{
    std::wstring msg;
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_HEX: {
        msg = hexify<T>(num);
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        if (num == 0) {
            msg = L" ";
        } else {
            msg = L"+";
        }
    } break;
    default: {
        msg = fmt::to_wstring(num);
    } break;
    }
    return msg;
}
//=============================================================================
std::wstring
summarizeStringArray(const ArrayOf& A, size_t beginingLineLength, size_t termWidth)
{
    std::wstring str;
    if (A.isEmpty()) {
        if (A.isCharacterArray()) {
            str = L"\"\"";
        } else {
            str = L"<missing>";
        }
    } else {
        if (A.getDataClass() == NLS_DOUBLE) {
            str = L"<missing>";
        } else if (A.getDataClass() == NLS_CHAR) {
            Dimensions dims = A.getDimensions();
            if (dims.isRowVector()) {
                if (dims.getColumns() < static_cast<indexType>(termWidth - 3)) {
                    std::wstring str = A.getContentAsWideString();
                    str = L"\"" + str + L"\"";
                    return str;
                }
            }
            str = lightDescription(A, L"[", L"]");
        }
    }
    return str;
}
//=============================================================================
/**
 * Print this object when it is an element of a cell array.  This is
 * generally a shorthand summary of the description of the object.
 */
std::wstring
summarizeCellEntry(const ArrayOf& A, size_t beginingLineLength, size_t termWidth)
{
    std::wstring msg;
    if (A.isEmpty()) {
        if (A.getDataPointer() == nullptr) {
            msg = L"[]";
        } else {
            if (A.getDataClass() == NLS_CHAR) {
                msg = L"''";
            } else {
                msg = L"[]";
            }
        }
        return msg;
    }
    switch (A.getDataClass()) {
    case NLS_CELL_ARRAY: {
        if (A.isScalar()) {
            ArrayOf* elements = (ArrayOf*)A.getDataPointer();
            msg = L"{" + summarizeCellEntry(elements[0], beginingLineLength + 1, termWidth) + L"}";
        } else {
            msg = lightDescription(A, L"{", L"}");
        }
    } break;
    case NLS_STRING_ARRAY: {
        if (A.isScalar()) {
            ArrayOf* elements = (ArrayOf*)A.getDataPointer();
            if (elements[0].isCharacterArray()) {
                if (elements[0].getColumns() < termWidth - beginingLineLength - 3) {
                    msg = L"\"" + elements[0].getContentAsWideString() + L"\"";
                } else {
                    msg = lightDescription(A, L"[", L"]");
                }
            } else {
                msg = L"<missing>";
            }
        } else {
            msg = lightDescription(A, L"[", L"]");
        }
    } break;
    case NLS_STRUCT_ARRAY: {
        msg = lightDescription(A, L"[", L"]");
    } break;
    case NLS_CHAR: {
        if (A.isRowVector()) {
            if (A.getColumns() < termWidth - beginingLineLength - 3) {
                msg = L"\'" + A.getContentAsWideString() + L"\'";
            } else {
                msg = lightDescription(A, L"[", L"]");
            }
        } else {
            msg = lightDescription(A, L"[", L"]");
        }
    } break;
    case NLS_GO_HANDLE: {
        msg = lightDescription(A, L"[", L"]");
    } break;
    case NLS_HANDLE: {
        msg = lightDescription(A, L"[", L"]");
    } break;
    case NLS_LOGICAL: {
        if (A.isScalar()) {
            if (A.isSparse()) {
                msg = lightDescription(A, L"[", L"]");
            } else {
                logical val = A.getContentAsLogicalScalar();
                if (val) {
                    msg = L"true";
                } else {
                    msg = L"false";
                }
            }
        } else {
            msg = lightDescription(A, L"[", L"]");
        }
    } break;
    case NLS_UINT8: {
        if (A.isScalar()) {
            std::wstring format = L"%d";
            msg = fmt::sprintf(format, *(static_cast<const uint8*>(A.getDataPointer())));
        } else {
            msg = lightDescription(A, L"[", L"]");
        }
    } break;
    case NLS_INT8: {
        if (A.isScalar()) {
            std::wstring format = L"%d";
            msg = fmt::sprintf(format, *(static_cast<const int8*>(A.getDataPointer())));
        } else {
            msg = lightDescription(A, L"[", L"]");
        }
    } break;
    case NLS_UINT16: {
        if (A.isScalar()) {
            std::wstring format = L"%d";
            msg = fmt::sprintf(format, *(static_cast<const uint16*>(A.getDataPointer())));
        } else {
            msg = lightDescription(A, L"[", L"]");
        }
    } break;
    case NLS_INT16: {
        if (A.isScalar()) {
            std::wstring format = L"%d";
            msg = fmt::sprintf(format, *(static_cast<const int16*>(A.getDataPointer())));
        } else {
            msg = lightDescription(A, L"[", L"]");
        }
    } break;
    case NLS_UINT32: {
        if (A.isScalar()) {
            std::wstring format = L"%d";
            msg = fmt::sprintf(format, *(static_cast<const uint32*>(A.getDataPointer())));
        } else {
            msg = lightDescription(A, L"[", L"]");
        }
    } break;
    case NLS_INT32: {
        if (A.isScalar()) {
            std::wstring format = L"%d";
            msg = fmt::sprintf(format, *(static_cast<const int32*>(A.getDataPointer())));
        } else {
            msg = lightDescription(A, L"[", L"]");
        }
    } break;
    case NLS_UINT64: {
        if (A.isScalar()) {
            uint64 val = *(static_cast<const uint64*>(A.getDataPointer()));
            msg = fmt::to_wstring(val);
        } else {
            msg = lightDescription(A, L"[", L"]");
        }
    } break;
    case NLS_INT64: {
        if (A.isScalar()) {
            int64 val = *(static_cast<const int64*>(A.getDataPointer()));
            msg = fmt::to_wstring(val);
        } else {
            msg = lightDescription(A, L"[", L"]");
        }
    } break;
    case NLS_DOUBLE: {
        if (A.isSparse()) {
            msg = lightDescription(A, L"[", L"]");
        } else {
            if (A.isScalar()) {
                std::wstring format = L"%lf";
                msg = fmt::sprintf(format, *(static_cast<const double*>(A.getDataPointer())));
            } else {
                msg = lightDescription(A, L"[", L"]");
            }
        }
    } break;
    case NLS_DCOMPLEX: {
        if (A.isSparse()) {
            msg = lightDescription(A, L"[", L"]");
        } else {
            if (A.isScalar()) {
                const auto* ap = static_cast<const double*>(A.getDataPointer());
                std::wstring format = L"[%lf+%lfi]";
                msg = fmt::sprintf(format, ap[0], ap[1]);
            } else {
                msg = lightDescription(A, L"[", L"]");
            }
        }
    } break;
    case NLS_SINGLE: {
        if (A.isScalar()) {
            std::wstring format = L"%f";
            msg = fmt::sprintf(format, *(static_cast<const single*>(A.getDataPointer())));
        } else {
            msg = lightDescription(A, L"[", L"]");
        }
    } break;
    case NLS_SCOMPLEX: {
        if (A.isScalar()) {
            const auto* ap = static_cast<const single*>(A.getDataPointer());
            std::wstring format = L"[%f+%fi]";
            msg = fmt::sprintf(format, ap[0], ap[1]);
        } else {
            msg = lightDescription(A, L"[", L"]");
        }
    } break;
    default: {
    } break;
    }
    return msg;
}
//=============================================================================
std::wstring
sprintElement(const void* dp, indexType num, Class dcls, NumericFormatDisplay currentNumericFormat,
    LineSpacingDisplay currentLineSpacing, indexType width, int exponential)
{
    std::wstring msg;
    switch (dcls) {
    case NLS_STRUCT_ARRAY: {
    } break;
    case NLS_GO_HANDLE: {
    } break;
    case NLS_HANDLE: {
    } break;
    case NLS_INT8: {
        const int8* ap = static_cast<const int8*>(dp);
        msg = outputSignedIntegerPrecision<int8>(ap[num], currentNumericFormat);
        msg = completeWithBlanksAtBeginning(msg, width);
    } break;
    case NLS_UINT8: {
        const auto* ap = static_cast<const uint8*>(dp);
        msg = outputUnsignedIntegerPrecision<uint8>(ap[num], currentNumericFormat);
        msg = completeWithBlanksAtBeginning(msg, width);
    } break;
    case NLS_INT16: {
        const auto* ap = static_cast<const int16*>(dp);
        int16 value = ap[num];
        msg = outputSignedIntegerPrecision<int16>(ap[num], currentNumericFormat);
        msg = completeWithBlanksAtBeginning(msg, width);
    } break;
    case NLS_UINT16: {
        const auto* ap = static_cast<const uint16*>(dp);
        uint16 value = ap[num];
        msg = outputUnsignedIntegerPrecision<uint16>(ap[num], currentNumericFormat);
        msg = completeWithBlanksAtBeginning(msg, width);
    } break;
    case NLS_INT32: {
        const auto* ap = static_cast<const int32*>(dp);
        int32 value = ap[num];
        msg = outputSignedIntegerPrecision<int32>(ap[num], currentNumericFormat);
        msg = completeWithBlanksAtBeginning(msg, width);
    } break;
    case NLS_UINT32: {
        const auto* ap = static_cast<const uint32*>(dp);
        uint32 value = ap[num];
        msg = outputUnsignedIntegerPrecision<uint32>(ap[num], currentNumericFormat);
        msg = completeWithBlanksAtBeginning(msg, width);
    } break;
    case NLS_INT64: {
        const auto* ap = static_cast<const int64*>(dp);
        msg = outputSignedIntegerPrecision<int64>(ap[num], currentNumericFormat);
        msg = completeWithBlanksAtBeginning(msg, width);
    } break;
    case NLS_UINT64: {
        const uint64* ap = static_cast<const uint64*>(dp);
        msg = outputUnsignedIntegerPrecision<uint64>(ap[num], currentNumericFormat);
        msg = completeWithBlanksAtBeginning(msg, width);
    } break;
    case NLS_LOGICAL: {
        const auto* ap = static_cast<const logical*>(dp);
        if (ap[num] == 0) {
            msg = L"false";
        } else {
            msg = L"true";
        }
    } break;
    case NLS_CHAR: {
        const auto* ap = static_cast<const charType*>(dp);
        msg.push_back(ap[num]);
    } break;
    case NLS_SINGLE: {
        const auto* ap = static_cast<const single*>(dp);
        msg = outputSinglePrecisionFloat(ap[num], currentNumericFormat);
    } break;
    case NLS_DOUBLE: {
        const auto* ap = static_cast<const double*>(dp);
        msg = outputDoublePrecisionFloat(ap[num], currentNumericFormat, false, exponential);
    } break;
    case NLS_SCOMPLEX: {
        const auto* ap = static_cast<const single*>(dp);
        msg = outputSingleComplexPrecisionFloat(ap[2 * num], ap[2 * num + 1], currentNumericFormat);
    } break;
    case NLS_DCOMPLEX: {
        const auto* ap = static_cast<const double*>(dp);
        msg = outputDoubleComplexPrecisionFloat(ap[2 * num], ap[2 * num + 1], currentNumericFormat);
    } break;
    case NLS_CELL_ARRAY: {
        auto* ap = (ArrayOf*)dp;
        if (ap == nullptr) {
            msg = L"[]";
        } else {
            std::wstring msg = summarizeCellEntry(ap[num], 0, width);
            msg = completeWithBlanksAtBeginning(msg, width);
        }
    } break;
    case NLS_STRING_ARRAY: {
        auto* ap = (ArrayOf*)dp;
        if (ap == nullptr) {
            msg = L"[]";
        } else {
            msg = summarizeStringArray(ap[num], 0, width);
            // msg = completeWithBlanksAtBeginning(msg, width);
            // msg.append(msg.length() - width, L" ");
            size_t add = 0;
            if (width > msg.length()) {
                add = width - msg.length();
            }
            msg.append(add, L' ');
        }
    } break;
    default: {
    } break;
    }
    return msg;
}
//=============================================================================
void
emitElement(Interface* io, const void* dp, indexType num, Class dcls,
    NumericFormatDisplay currentNumericFormat, LineSpacingDisplay currentLineSpacing,
    indexType width, int exponantial)
{
    io->outputMessage(
        sprintElement(dp, num, dcls, currentNumericFormat, currentLineSpacing, width, exponantial));
}
//=============================================================================
/**
 * Display this variable on the given output stream.
 */
void
printValue(Interface* io, const ArrayOf& A, const std::wstring& name)
{
    Dimensions dims = A.getDimensions();
    if (dims.isEmpty(false)) {
        printEmptyValue(io, A);
    } else if (dims.isScalar() || dims.is2D()) {
        printMatrixValue(io, A);
    } else {
        printNDArrayValue(io, A, name);
    }
}
//=============================================================================
void
printEmptyValue(Interface* io, const ArrayOf& A)
{
    switch (A.getDataClass()) {
    case NLS_STRUCT_ARRAY: {
        stringVector fieldsName = A.getFieldNames();
        for (const auto& name : fieldsName) {
            io->outputMessage(BLANKS_AT_BOL);
            io->outputMessage(utf8_to_wstring(name));
            io->outputMessage(L"\n");
        }
    } break;
    case NLS_SINGLE:
    case NLS_SCOMPLEX:
    case NLS_DOUBLE:
    case NLS_DCOMPLEX: {
        if (A.isEmpty(true)) {
            if (NelsonConfiguration::getInstance()->getLineSpacingDisplay()
                != NLS_LINE_SPACING_COMPACT) {
                io->outputMessage(L"\n");
            }
            io->outputMessage(L"     []\n");
        }
    } break;
    case NLS_INT8:
    case NLS_UINT8:
    case NLS_INT16:
    case NLS_UINT16:
    case NLS_INT32:
    case NLS_UINT32:
    case NLS_INT64:
    case NLS_UINT64:
    default: {
    } break;
    }
}
//=============================================================================
void
printMatrixValue(Interface* io, const ArrayOf& A)
{
    if (A.getDataClass() == NLS_STRING_ARRAY) {
        print2dString(io, A, NelsonConfiguration::getInstance()->getLineSpacingDisplay());
        return;
    } else if (A.getDataClass() == NLS_CELL_ARRAY) {
        print2dCell(io, A, NelsonConfiguration::getInstance()->getLineSpacingDisplay());
        return;
    } else if (A.getDataClass() == NLS_STRUCT_ARRAY) {
        if (A.isScalar()) {
            if (NelsonConfiguration::getInstance()->getLineSpacingDisplay()
                != NLS_LINE_SPACING_COMPACT) {
                io->outputMessage(L"\n");
            }
            ArrayOf* ap = (ArrayOf*)A.getDataPointer();
            stringVector fieldnames = A.getFieldNames();
            size_t maxLen = 0;
            for (auto name : fieldnames) {
                maxLen = std::max(utf8_to_wstring(name).length(), maxLen);
            }
            for (size_t k = 0; k < fieldnames.size(); ++k) {
                std::wstring beginning = BLANKS_AT_BOL
                    + completeWithBlanksAtBeginning(utf8_to_wstring(fieldnames[k]), maxLen) + L": ";
                io->outputMessage(beginning
                    + summarizeCellEntry(ap[k], beginning.length() + 1, io->getTerminalWidth())
                    + L"\n");
            }
        } else {
            if (!A.getFieldNames().empty()) {
                if (NelsonConfiguration::getInstance()->getLineSpacingDisplay()
                    != NLS_LINE_SPACING_COMPACT) {
                    io->outputMessage("\n");
                }
                for (const auto& fieldName : A.getFieldNames()) {
                    io->outputMessage(BLANKS_AT_BOL);
                    io->outputMessage(fieldName);
                    io->outputMessage("\n");
                }
            }
        }
        return;
    }
    NumericFormatDisplay currentNumericFormat
        = NelsonConfiguration::getInstance()->getNumericFormatDisplay();
    LineSpacingDisplay currentLineSpacing
        = NelsonConfiguration::getInstance()->getLineSpacingDisplay();

    Dimensions dims = A.getDimensions();
    const void* ap = A.getDataPointer();
    indexType rows = dims.getRows();
    indexType columns = dims.getColumns();
    Class classA = A.getDataClass();
    indexType nominalWidth = getNominalWidth(A, currentNumericFormat);

    if (currentNumericFormat == NLS_NUMERIC_FORMAT_PLUS) {
        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            io->outputMessage("\n");
        }
        for (indexType i = 0; i < rows; i++) {
            for (indexType j = 0; j < columns; j++) {
                std::wstring msg = sprintElement(ap, i + (rows * j), classA, currentNumericFormat,
                    currentLineSpacing, nominalWidth, 0);
                io->outputMessage(msg);
            }
            io->outputMessage("\n");
        }
    } else {
        sizeType termWidth = io->getTerminalWidth();

        int exponential = getCommonExponential(A);
        if (!A.isScalar() && !A.isEmpty() && exponential != 0) {
            io->outputMessage(L"   ");
            std::wstring fmt = L"1.0e%s%d *\n";
            std::wstring sign;
            if (exponential >= 0) {
                sign = L"+";
            }
            std::wstring str = fmt::sprintf(fmt, sign, exponential);
            io->outputMessage(str);
        } else {
            exponential = 0;
        }

        indexType colsPerPage;
        if (exponential != 0) {
            colsPerPage
                = static_cast<indexType>(floor((termWidth - 1) / (static_cast<single>(10))));

        } else {
            colsPerPage = static_cast<indexType>(
                floor((termWidth - 1) / (static_cast<single>(nominalWidth))));
        }
        indexType pageCount
            = static_cast<indexType>(ceil(columns / (static_cast<single>(colsPerPage))));
        bool withColumsHeader = (rows * columns > 1) && pageCount > 1;

        for (indexType k = 0;
             k < pageCount && !NelsonConfiguration::getInstance()->getInterruptPending(); k++) {

            indexType colsInThisPage = columns - colsPerPage * k;
            colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;
            /* if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
                io->outputMessage("\n");
            }*/
            if (withColumsHeader) {
                std::string msg = fmt::sprintf(_("  Columns %d through %d").c_str(),
                    k * colsPerPage + 1, k * colsPerPage + colsInThisPage);
                if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
                    msg = msg + "\n\n";
                } else {
                    msg = msg + "\n";
                }
                io->outputMessage(msg);
            }
            indexType q = 0;
            wstringVector values(rows * colsInThisPage, L"");
            std::vector<size_t> vSize(colsInThisPage, (size_t)0);
            indexType nbCharMaxToDispForOneString = 24;

            for (indexType i = 0;
                 i < rows && !NelsonConfiguration::getInstance()->getInterruptPending(); i++) {
                q = 0;
                for (indexType i = 0; i < rows; i++) {
                    for (indexType j = 0; j < colsInThisPage; j++) {
                        values[q] = sprintElement(ap, i + (k * colsPerPage + j) * rows,
                            A.getDataClass(), currentNumericFormat, currentLineSpacing,
                            nominalWidth, exponential);
                        vSize[j] = std::max(vSize[j], values[q].length());
                        vSize[j] = std::min(vSize[j], nbCharMaxToDispForOneString + 1);
                        q++;
                    }
                }
            }
            q = 0;

            for (indexType i = 0; i < rows; i++) {
                for (indexType j = 0; j < colsInThisPage; j++) {
                    std::wstring msg = values[q];
                    size_t add = 0;
                    if (vSize[j] > msg.length()) {
                        add = vSize[j] - msg.length();
                    }
                    msg.append(add, L' ');
                    io->outputMessage(BLANKS_AT_BOL);
                    io->outputMessage(msg);
                    q++;
                }
                io->outputMessage(L"\n");
            }
            /*
            for (indexType j = 0; j < colsInThisPage; j++) {
                std::wstring msg = values[q];
                size_t add = 0;
                if (vSize[j] > msg.length()) {
                    add = vSize[j] - msg.length();
                }
                msg.append(add, L' ');
                io->outputMessage(BLANKS_AT_BOL);
                io->outputMessage(msg);
            }
            io->outputMessage("\n");
            */
        }
    }
}
//=============================================================================
void
printNDArrayValue(Interface* io, const ArrayOf& A, const std::wstring& name)
{
    if (A.getDataClass() == NLS_STRUCT_ARRAY) {
        if (!A.getFieldNames().empty()) {
            io->outputMessage("  Fields\n");
            for (const auto& fieldName : A.getFieldNames()) {
                io->outputMessage("    ");
                io->outputMessage(fieldName);
                io->outputMessage("\n");
            }
        }
        return;
    }
    NumericFormatDisplay currentNumericFormat
        = NelsonConfiguration::getInstance()->getNumericFormatDisplay();
    LineSpacingDisplay currentLineSpacing
        = NelsonConfiguration::getInstance()->getLineSpacingDisplay();

    sizeType termWidth = io->getTerminalWidth();
    indexType nominalWidth = getNominalWidth(A.getDataClass(), currentNumericFormat);
    Dimensions dims = A.getDimensions();
    const void* ap = A.getDataPointer();

    Dimensions wdims(dims.getLength());
    indexType rows(A.getRows());
    indexType columns(A.getColumns());
    indexType offset = 0;

    while (wdims.inside(dims)) {
        if (offset != 0) {
            if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
                io->outputMessage(L"\n");
            }
        }
        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            io->outputMessage(L"\n");
        }

        io->outputMessage(name + L"(:,:");
        for (indexType m = 2; m < dims.getLength(); m++) {
            io->outputMessage(fmt::sprintf(",%d", static_cast<int>(wdims[m]) + 1));
        }
        io->outputMessage(L")\n");
        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            io->outputMessage(L"\n");
        }

        auto colsPerPage
            = static_cast<indexType>(floor((termWidth - 1) / (static_cast<single>(nominalWidth))));

        int pageCount = static_cast<int>(ceil(columns / (static_cast<single>(colsPerPage))));
        bool withColumsHeader = (rows * columns > 1) && pageCount > 1;
        for (int k = 0; k < pageCount && !NelsonConfiguration::getInstance()->getInterruptPending();
             k++) {
            indexType colsInThisPage = columns - colsPerPage * k;
            colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;
            if (withColumsHeader) {
                std::wstring msg = fmt::sprintf(_W("  Columns %d through %d"), k * colsPerPage + 1,
                    k * colsPerPage + colsInThisPage);
                if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
                    msg = msg + L"\n\n";
                } else {
                    msg = msg + L"\n";
                }
                io->outputMessage(msg);
            }
            indexType q = 0;
            wstringVector values(rows * colsInThisPage, L"");
            std::vector<size_t> vSize(colsInThisPage, (size_t)0);
            indexType nbCharMaxToDispForOneString = 24;

            for (indexType i = 0; i < rows; i++) {
                for (indexType j = 0; j < colsInThisPage; j++) {
                    values[q] = sprintElement(ap, i + (k * colsPerPage + j) * rows + offset,
                        A.getDataClass(), currentNumericFormat, currentLineSpacing, nominalWidth,
                        0);
                    vSize[j] = std::max(vSize[j], values[q].length());
                    vSize[j] = std::min(vSize[j], nbCharMaxToDispForOneString + 1);
                    q++;
                }
            }

            q = 0;
            for (indexType i = 0; i < rows; i++) {
                for (indexType j = 0; j < colsInThisPage; j++) {
                    std::wstring msg = values[q];
                    size_t add = 0;
                    if (vSize[j] > msg.length()) {
                        add = vSize[j] - msg.length();
                    }
                    msg.append(add, L' ');
                    io->outputMessage(BLANKS_AT_BOL);
                    io->outputMessage(msg);

                    q++;
                }
                io->outputMessage(L"\n");
            }
        }
        offset += rows * columns;
        wdims.incrementModulo(dims, 2);
    }
}
//=============================================================================
template <typename T>
indexType
getSignedIntegerNominalWidth(T* dp, indexType nbElements, NumericFormatDisplay currentNumericFormat)
{
    indexType nominalWidth = 0;
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_PLUS: {
        nominalWidth = 1;
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        nominalWidth = std::wstring(BLANKS_AT_BOL).length() + (sizeof(T) << 1);
    } break;
    case NLS_NUMERIC_FORMAT_SHORT:
    case NLS_NUMERIC_FORMAT_LONG:
    case NLS_NUMERIC_FORMAT_SHORTE:
    case NLS_NUMERIC_FORMAT_LONGE:
    case NLS_NUMERIC_FORMAT_SHORTG:
    case NLS_NUMERIC_FORMAT_LONGG:
    case NLS_NUMERIC_FORMAT_SHORTENG:
    case NLS_NUMERIC_FORMAT_LONGENG:
    case NLS_NUMERIC_FORMAT_BANK:
    case NLS_NUMERIC_FORMAT_RATIONAL:
    default: {
        T maxValue;
        T minValue;
        getMinMax<T>(dp, nbElements, &minValue, &maxValue);
        std::wstring maxStr = BLANKS_AT_BOL + fmt::to_wstring(maxValue);
        std::wstring minStr = BLANKS_AT_BOL + fmt::to_wstring(minValue);
        nominalWidth = std::max(minStr.length(), maxStr.length());
    } break;
    }
    return nominalWidth;
}
//=============================================================================
template <typename T>
indexType
getUnsignedIntegerNominalWidth(
    T* dp, indexType nbElements, NumericFormatDisplay currentNumericFormat)
{
    indexType nominalWidth = 0;
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_PLUS: {
        nominalWidth = 1;
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        nominalWidth = std::wstring(BLANKS_AT_BOL).length() + (sizeof(T) << 1);
    } break;
    case NLS_NUMERIC_FORMAT_SHORT:
    case NLS_NUMERIC_FORMAT_LONG:
    case NLS_NUMERIC_FORMAT_SHORTE:
    case NLS_NUMERIC_FORMAT_LONGE:
    case NLS_NUMERIC_FORMAT_SHORTG:
    case NLS_NUMERIC_FORMAT_LONGG:
    case NLS_NUMERIC_FORMAT_SHORTENG:
    case NLS_NUMERIC_FORMAT_LONGENG:
    case NLS_NUMERIC_FORMAT_BANK:
    case NLS_NUMERIC_FORMAT_RATIONAL:
    default: {
        T maxValue = getMax<T>(dp, nbElements);
        std::wstring maxStr = BLANKS_AT_BOL + fmt::to_wstring(maxValue);
        nominalWidth = maxStr.length();
    } break;
    }
    return nominalWidth;
}
//=============================================================================
static indexType
getDoubleNominalWidth(double* dp, indexType nbElements, NumericFormatDisplay currentNumericFormat)
{
    indexType nominalWidth = 30;
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_SHORT: {
    } break;
    case NLS_NUMERIC_FORMAT_LONG: {
    } break;
    case NLS_NUMERIC_FORMAT_SHORTE: {
    } break;
    case NLS_NUMERIC_FORMAT_LONGE: {
    } break;
    case NLS_NUMERIC_FORMAT_SHORTG: {
    } break;
    case NLS_NUMERIC_FORMAT_LONGG: {
    } break;
    case NLS_NUMERIC_FORMAT_SHORTENG: {
    } break;
    case NLS_NUMERIC_FORMAT_LONGENG: {
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        nominalWidth = 1;
    } break;
    case NLS_NUMERIC_FORMAT_BANK: {
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
    } break;
    default: {
        nominalWidth = 30;
    } break;
    }
    return nominalWidth;
}
//=============================================================================
static indexType
getSingleNominalWidth(single* dp, indexType nbElements, NumericFormatDisplay currentNumericFormat)
{
    indexType nominalWidth = 30;
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_SHORT: {
    } break;
    case NLS_NUMERIC_FORMAT_LONG: {
    } break;
    case NLS_NUMERIC_FORMAT_SHORTE: {
    } break;
    case NLS_NUMERIC_FORMAT_LONGE: {
    } break;
    case NLS_NUMERIC_FORMAT_SHORTG: {
    } break;
    case NLS_NUMERIC_FORMAT_LONGG: {
    } break;
    case NLS_NUMERIC_FORMAT_SHORTENG: {
    } break;
    case NLS_NUMERIC_FORMAT_LONGENG: {
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        nominalWidth = 1;
    } break;
    case NLS_NUMERIC_FORMAT_BANK: {
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
    } break;
    default: {
        nominalWidth = 30;
    } break;
    }
    return nominalWidth;
}
//=============================================================================
static indexType
getLogicalNominalWidth(logical* dp, indexType nbElements, NumericFormatDisplay currentNumericFormat)
{
    std::wstring maxStr = BLANKS_AT_BOL + std::wstring(L"false");
    return maxStr.length();
}
//=============================================================================
indexType
getNominalWidth(const ArrayOf& A, NumericFormatDisplay currentNumericFormat)
{
    Class classA = A.getDataClass();
    indexType nominalWidth = 30;
    switch (classA) {
    case NLS_UINT8: {
        uint8* val = (uint8*)A.getDataPointer();
        nominalWidth
            = getUnsignedIntegerNominalWidth<uint8>(val, A.getElementCount(), currentNumericFormat);
    } break;
    case NLS_INT8: {
        int8* val = (int8*)A.getDataPointer();
        nominalWidth
            = getSignedIntegerNominalWidth<int8>(val, A.getElementCount(), currentNumericFormat);
    } break;
    case NLS_UINT16: {
        uint16* val = (uint16*)A.getDataPointer();
        nominalWidth = getUnsignedIntegerNominalWidth<uint16>(
            val, A.getElementCount(), currentNumericFormat);
    } break;
    case NLS_INT16: {
        int16* val = (int16*)A.getDataPointer();
        nominalWidth
            = getSignedIntegerNominalWidth<int16>(val, A.getElementCount(), currentNumericFormat);
    } break;
    case NLS_UINT32: {
        uint32* val = (uint32*)A.getDataPointer();
        nominalWidth = getUnsignedIntegerNominalWidth<uint32>(
            val, A.getElementCount(), currentNumericFormat);
    } break;
    case NLS_INT32: {
        int32* val = (int32*)A.getDataPointer();
        nominalWidth
            = getSignedIntegerNominalWidth<int32>(val, A.getElementCount(), currentNumericFormat);
    } break;
    case NLS_UINT64: {
        uint64* val = (uint64*)A.getDataPointer();
        nominalWidth = getUnsignedIntegerNominalWidth<uint64>(
            val, A.getElementCount(), currentNumericFormat);
    } break;
    case NLS_INT64: {
        int64* val = (int64*)A.getDataPointer();
        nominalWidth
            = getSignedIntegerNominalWidth<int64>(val, A.getElementCount(), currentNumericFormat);
    } break;
    case NLS_SINGLE: {
        single* val = (single*)A.getDataPointer();
        nominalWidth = getSingleNominalWidth(val, A.getElementCount(), currentNumericFormat);
    } break;
    case NLS_DOUBLE: {
        double* val = (double*)A.getDataPointer();
        nominalWidth = getDoubleNominalWidth(val, A.getElementCount(), currentNumericFormat);
    } break;
    case NLS_LOGICAL: {
        logical* val = (logical*)A.getDataPointer();
        nominalWidth = getLogicalNominalWidth(val, A.getElementCount(), currentNumericFormat);
    } break;
    case NLS_CHAR: {
        nominalWidth = 1;
    } break;
    case NLS_SCOMPLEX: {
        nominalWidth = 36;
    } break;
    case NLS_DCOMPLEX: {
        nominalWidth = 36;
    } break;
    case NLS_GO_HANDLE:
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_STRING_ARRAY:
    default: {
        nominalWidth = 10;
    } break;
    }
    return nominalWidth;
}
//=============================================================================
std::wstring
lightDescription(const ArrayOf& A, const std::wstring& firstChar, const std::wstring& lastChar)
{
    std::wstring format = L"%s%s %s%s";
    return fmt::sprintf(
        format, firstChar, A.getDimensions().toWideString(), getClassAsWideString(A), lastChar);
}
//=============================================================================
void
print2dCell(Interface* io, const ArrayOf& A, LineSpacingDisplay currentLineSpacing)
{
    ArrayOf* elements = (ArrayOf*)A.getDataPointer();
    if (A.isColumnVector()) {
        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            io->outputMessage(L"\n");
        }
        size_t nbBlanksAtBegin = LENGTH_BLANKS_AT_BOL;
        for (indexType k = 0;
             k < A.getElementCount() && !NelsonConfiguration::getInstance()->getInterruptPending();
             ++k) {
            std::wstring msg
                = summarizeCellEntry(elements[k], nbBlanksAtBegin, io->getTerminalWidth());
            io->outputMessage(BLANKS_AT_BOL + msg + L"\n");
        }
    } else {
        wstringVector cellSummarize;
        cellSummarize.reserve(A.getElementCount());
        size_t nominalWidth = 0;
        for (indexType k = 0; k < A.getElementCount(); ++k) {
            std::wstring msg = lightDescription(elements[k], L"{", L"}");
            nominalWidth = std::max(nominalWidth, msg.length());
            cellSummarize.push_back(msg);
        }
        nominalWidth = LENGTH_BLANKS_AT_BOL + nominalWidth;
        sizeType termWidth = io->getTerminalWidth();
        indexType rows = A.getRows();
        indexType columns = A.getColumns();
        auto colsPerPage
            = static_cast<indexType>(floor((termWidth - 1) / (static_cast<single>(nominalWidth))));
        auto pageCount = static_cast<indexType>(ceil(columns / (static_cast<single>(colsPerPage))));
        bool withColumsHeader = (rows * columns > 1) && pageCount > 1;

        for (indexType k = 0;
             k < pageCount && !NelsonConfiguration::getInstance()->getInterruptPending(); k++) {

            indexType colsInThisPage = columns - colsPerPage * k;
            colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;
            if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
                io->outputMessage(L"\n");
            }
            if (withColumsHeader) {
                std::wstring msg = fmt::sprintf(_W("  Columns %d through %d").c_str(),
                    k * colsPerPage + 1, k * colsPerPage + colsInThisPage);
                if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
                    msg = msg + L"\n\n";
                } else {
                    msg = msg + L"\n";
                }
                io->outputMessage(msg);
            }
            for (indexType i = 0;
                 i < rows && !NelsonConfiguration::getInstance()->getInterruptPending(); i++) {
                for (indexType j = 0; j < colsInThisPage; j++) {
                    indexType idx = i + (k * colsPerPage + j) * rows;
                    std::wstring msg
                        = completeWithBlanksAtBeginning(cellSummarize[idx], nominalWidth);
                    io->outputMessage(msg);
                }
                io->outputMessage(L"\n");
            }
        }
    }
}
//=============================================================================
void
printNdCell(Interface* io, const ArrayOf& A, LineSpacingDisplay currentLineSpacing)
{ }
//=============================================================================

void
print2dString(Interface* io, const ArrayOf& A, LineSpacingDisplay currentLineSpacing)
{
    ArrayOf* elements = (ArrayOf*)A.getDataPointer();
    if (A.isColumnVector()) {
        if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
            io->outputMessage(L"\n");
        }
        for (indexType k = 0;
             k < A.getElementCount() && !NelsonConfiguration::getInstance()->getInterruptPending();
             ++k) {
            std::wstring msg;
            if (elements[k].isCharacterArray()) {
                msg = L"\"" + elements[k].getContentAsWideString() + L"\"";
            } else {
                msg = L"<missing>";
            }
            io->outputMessage(BLANKS_AT_BOL + msg + L"\n");
        }
    } else {
        size_t nbCharMaxToDispForOneString = 27;

        std::vector<size_t> vSize(A.getColumns(), (size_t)0);
        wstringVector cellSummarize(A.getElementCount(), std::wstring(L""));

        indexType rowCount = A.getDimensions()[0];
        indexType colCount = A.getDimensions()[1];
        for (indexType i = 0; i < rowCount; i++) {
            for (indexType j = 0; j < colCount; j++) {
                size_t index = i + j * rowCount;
                std::wstring msg;
                if (elements[index].isCharacterArray()) {
                    msg = elements[index].getContentAsWideString(nbCharMaxToDispForOneString);
                    if (msg.length() == nbCharMaxToDispForOneString) {
                        msg.pop_back();
                        msg = L"\"" + msg + L"…";
                    } else {
                        msg = L"\"" + msg + L"\"";
                    }
                } else {
                    msg = L"<missing>";
                }
                cellSummarize[index] = msg;
                vSize[j] = std::max(vSize[j], msg.length());
                vSize[j] = std::min(vSize[j], nbCharMaxToDispForOneString + 1);
            }
        }

        for (indexType i = 0; i < rowCount; i++) {
            for (indexType j = 0; j < colCount; j++) {
                size_t index = i + j * rowCount;
                if (cellSummarize[index].length() > vSize[j]) {
                    cellSummarize[index] = cellSummarize[index].substr(0, vSize[j]);
                }
            }
        }
        sizeType termWidth = io->getTerminalWidth();
        indexType l = 0;
        indexType nominal = 0;
        for (indexType k = 0; k < vSize.size(); ++k) {
            l = LENGTH_BLANKS_AT_BOL + vSize[k] + l;
            if (l > termWidth) {
                nominal = k;
                break;
            }
        }
        indexType colsPerPage = nominal;
        if (nominal == 0) {
            colsPerPage = vSize.size();
        }

        indexType rows = A.getRows();
        indexType columns = A.getColumns();
        auto pageCount = static_cast<indexType>(ceil(columns / (static_cast<single>(colsPerPage))));
        bool withColumsHeader = (rows * columns > 1) && pageCount > 1;

        for (indexType k = 0;
             k < pageCount && !NelsonConfiguration::getInstance()->getInterruptPending(); k++) {

            indexType colsInThisPage = columns - colsPerPage * k;
            colsInThisPage = (colsInThisPage > colsPerPage) ? colsPerPage : colsInThisPage;
            if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
                io->outputMessage(L"\n");
            }
            if (withColumsHeader) {
                std::wstring msg = fmt::sprintf(_W("  Columns %d through %d").c_str(),
                    k * colsPerPage + 1, k * colsPerPage + colsInThisPage);
                if (currentLineSpacing == NLS_LINE_SPACING_LOOSE) {
                    msg = msg + L"\n\n";
                } else {
                    msg = msg + L"\n";
                }
                io->outputMessage(msg);
            }
            for (indexType i = 0;
                 i < rows && !NelsonConfiguration::getInstance()->getInterruptPending(); i++) {
                for (indexType j = 0; j < colsInThisPage; j++) {
                    indexType idx = i + (k * colsPerPage + j) * rows;
                    std::wstring blanks;
                    std::wstring msg = BLANKS_AT_BOL + cellSummarize[idx];
                    size_t len = cellSummarize[idx].length();
                    size_t add = 0;
                    if (vSize[j] > len) {
                        add = vSize[j] - len;
                    }
                    msg.append(add, L' ');
                    io->outputMessage(msg);
                }
                io->outputMessage(L"\n");
            }
        }
    }
}
//=============================================================================
void
printNdString(Interface* io, const ArrayOf& A, LineSpacingDisplay currentLineSpacingDisplay)
{ }
//=============================================================================
} // namespace Nelson
//=============================================================================
