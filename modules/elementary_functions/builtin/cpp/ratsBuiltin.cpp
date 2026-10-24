//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ratsBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "ImagPart.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "RationalApproximation.hpp"
#include "RealPart.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cmath>
#include <iomanip>
#include <sstream>
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
isFloatArray(const ArrayOf& value)
{
    return (value.isDoubleClass() || value.isSingleClass()) && !value.isSparse();
}
//=============================================================================
static double
readRealElement(const ArrayOf& value, indexType index)
{
    if (value.dataClass() == NLS_SINGLE) {
        return static_cast<double>(static_cast<const single*>(value.dataPointer())[index]);
    }
    return static_cast<const double*>(value.dataPointer())[index];
}
//=============================================================================
static double
finiteRealOneNorm(const ArrayOf& value)
{
    double total = 0.0;
    indexType elementCount = value.elementCount();
    if (value.dataClass() == NLS_SINGLE) {
        const auto* data = static_cast<const single*>(value.dataPointer());
        for (indexType k = 0; k < elementCount; ++k) {
            double x = static_cast<double>(data[k]);
            if (std::isfinite(x)) {
                total += std::abs(x);
            }
        }
        return total;
    }
    const auto* data = static_cast<const double*>(value.dataPointer());
    for (indexType k = 0; k < elementCount; ++k) {
        double x = data[k];
        if (std::isfinite(x)) {
            total += std::abs(x);
        }
    }
    return total;
}
//=============================================================================
static std::wstring
formatInteger(double value, int width, bool leftAligned = false)
{
    std::wostringstream stream;
    if (leftAligned) {
        stream << std::left;
    }
    if (width > 0) {
        stream << std::setw(width);
    }
    stream << std::fixed << std::setprecision(0) << value;
    return stream.str();
}
//=============================================================================
static std::wstring
repeatSpaces(int count)
{
    return count > 0 ? std::wstring(static_cast<size_t>(count), L' ') : L"";
}
//=============================================================================
static std::wstring
centerText(const std::wstring& text, int width)
{
    int gap = width - static_cast<int>(text.size());
    return repeatSpaces(static_cast<int>(std::floor(gap / 2.0))) + text
        + repeatSpaces(static_cast<int>(std::ceil(gap / 2.0)));
}
//=============================================================================
static std::wstring
padRight(const std::wstring& text, int width)
{
    return text + repeatSpaces(width - static_cast<int>(text.size()));
}
//=============================================================================
static std::wstring
realEntry(
    double numerator, double denominator, int numeratorWidth, int denominatorWidth, int fieldWidth)
{
    std::wstring entry;
    if (denominator != 1.0) {
        entry = formatInteger(numerator, numeratorWidth) + L"/"
            + formatInteger(denominator, denominatorWidth, true);
    } else {
        entry = centerText(formatInteger(numerator, 0), fieldWidth);
    }
    return entry.size() > static_cast<size_t>(fieldWidth) ? centerText(L"*", fieldWidth) : entry;
}
//=============================================================================
static std::wstring
complexEntry(double numeratorReal, double denominatorReal, double numeratorImag,
    double denominatorImag, int numeratorWidth, int denominatorWidth, double span, int fieldWidth)
{
    std::wstring entry
        = realEntry(numeratorReal, denominatorReal, numeratorWidth, denominatorWidth, fieldWidth);
    int denominatorField = static_cast<int>(std::ceil(span)) + 1;
    wchar_t sign = numeratorImag >= 0.0 ? L'+' : L'-';
    double imagMagnitude = std::abs(numeratorImag);
    std::wstring imagPart;
    if (denominatorImag != 1.0) {
        imagPart = std::wstring(1, sign) + formatInteger(imagMagnitude, numeratorWidth) + L"/"
            + padRight(formatInteger(denominatorImag, 0) + L"i", denominatorField);
    } else {
        imagPart = std::wstring(1, sign)
            + centerText(formatInteger(imagMagnitude, 0) + L"i", fieldWidth + 1);
    }
    if (imagPart.size() > static_cast<size_t>(fieldWidth + 2)) {
        imagPart = std::wstring(1, sign) + centerText(L"*i", fieldWidth + 1);
    }
    return entry + imagPart;
}
//=============================================================================
static ArrayOf
charMatrixFromRows(const std::vector<std::wstring>& rows)
{
    size_t width = 0;
    for (const auto& row : rows) {
        width = std::max(width, row.size());
    }
    Dimensions dims(static_cast<indexType>(rows.size()), static_cast<indexType>(width));
    if (rows.empty() || width == 0) {
        return ArrayOf(NLS_CHAR, dims, nullptr);
    }
    auto* data = static_cast<charType*>(ArrayOf::allocateArrayOf(NLS_CHAR, dims.elementCount()));
    for (indexType row = 0; row < dims.rows(); ++row) {
        for (indexType col = 0; col < dims.columns(); ++col) {
            const std::wstring& text = rows[row];
            data[col * dims.rows() + row] = col < static_cast<indexType>(text.size())
                ? static_cast<charType>(text[col])
                : static_cast<charType>(L' ');
        }
    }
    return ArrayOf(NLS_CHAR, dims, data);
}
//=============================================================================
static ArrayOf
emptyChar()
{
    return ArrayOf(NLS_CHAR, Dimensions(0, 0), nullptr);
}
//=============================================================================
static double
ratsTolerance(double span, const ArrayOf& value)
{
    return std::min(std::pow(10.0, -span) * finiteRealOneNorm(value), 0.1);
}
//=============================================================================
static ArrayOf
formatRealRationals(
    const ArrayOf& input, double tol, int fieldWidth, int numeratorWidth, int denominatorWidth)
{
    ArrayOfVector approx = RationalApproximation(input, tol, true);
    indexType rowCount = input.dimensions().rows();
    indexType columnCount = input.dimensions().columns();
    std::vector<std::wstring> rows;
    rows.reserve(rowCount);
    for (indexType row = 0; row < rowCount; ++row) {
        std::wstring line;
        for (indexType col = 0; col < columnCount; ++col) {
            indexType k = row + col * rowCount;
            line += L" ";
            line += realEntry(readRealElement(approx[0], k), readRealElement(approx[1], k),
                numeratorWidth, denominatorWidth, fieldWidth);
        }
        rows.push_back(line);
    }
    return charMatrixFromRows(rows);
}
//=============================================================================
static ArrayOf
formatComplexRationals(const ArrayOf& realPart, const ArrayOf& imagPart, double span,
    int fieldWidth, int numeratorWidth, int denominatorWidth)
{
    ArrayOfVector realApprox = RationalApproximation(realPart, ratsTolerance(span, realPart), true);
    ArrayOfVector imagApprox = RationalApproximation(imagPart, ratsTolerance(span, imagPart), true);
    indexType rowCount = realPart.dimensions().rows();
    indexType columnCount = realPart.dimensions().columns();
    std::vector<std::wstring> rows;
    rows.reserve(rowCount);
    for (indexType row = 0; row < rowCount; ++row) {
        std::wstring line;
        for (indexType col = 0; col < columnCount; ++col) {
            indexType k = row + col * rowCount;
            line += L" ";
            line += complexEntry(readRealElement(realApprox[0], k),
                readRealElement(realApprox[1], k), readRealElement(imagApprox[0], k),
                readRealElement(imagApprox[1], k), numeratorWidth, denominatorWidth, span,
                fieldWidth);
        }
        rows.push_back(line);
    }
    return charMatrixFromRows(rows);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::ratsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    const ArrayOf& input = argIn[0];
    if (!isFloatArray(input)) {
        Error(L"Nelson:elementary_functions:valueFloat", L"Value must be a float.");
    }
    if (input.isEmpty()) {
        ArrayOfVector retval;
        retval << emptyChar();
        return retval;
    }

    double fieldWidth = 13.0;
    if (argIn.size() == 2) {
        const ArrayOf& widthArgument = argIn[1];
        if (!widthArgument.isNumeric()) {
            Error(L"Nelson:validators:mustBeNumeric", L"%s must be numeric.", L"Value");
        }
        if (!widthArgument.isScalar() || widthArgument.isComplex() || widthArgument.isSparse()) {
            Error(L"Nelson:elementary_functions:fieldWidthFiniteScalar",
                L"Field width must be a finite scalar.");
        }
        fieldWidth = widthArgument.asDoubleScalar();
        if (!std::isfinite(fieldWidth)) {
            Error(L"Nelson:elementary_functions:fieldWidthFiniteScalar",
                L"Field width must be a finite scalar.");
        }
    }

    double span = (fieldWidth - 1.0) / 2.0;
    int numeratorWidth = static_cast<int>(std::floor(span));
    int denominatorWidth = static_cast<int>(std::ceil(span));
    int fieldWidthInteger = static_cast<int>(fieldWidth);
    ArrayOf result;
    if (input.isComplex()) {
        result = formatComplexRationals(RealPart(input), ImagPart(input), span, fieldWidthInteger,
            numeratorWidth, denominatorWidth);
    } else {
        result = formatRealRationals(
            input, ratsTolerance(span, input), fieldWidthInteger, numeratorWidth, denominatorWidth);
    }
    ArrayOfVector retval;
    retval << result;
    return retval;
}
//=============================================================================
