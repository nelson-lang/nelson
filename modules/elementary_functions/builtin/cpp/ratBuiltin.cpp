//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ratBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "ImagPart.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "RationalApproximation.hpp"
#include "RealPart.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cmath>
#include <numeric>
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
readRealElement(const void* data, NelsonType dataClass, indexType index)
{
    if (dataClass == NLS_SINGLE) {
        return static_cast<double>(static_cast<const single*>(data)[index]);
    }
    return static_cast<const double*>(data)[index];
}
//=============================================================================
static void
writeRealElement(void* data, NelsonType dataClass, indexType index, double value)
{
    if (dataClass == NLS_SINGLE) {
        static_cast<single*>(data)[index] = static_cast<single>(value);
    } else {
        static_cast<double*>(data)[index] = value;
    }
}
//=============================================================================
template <class T>
static double
componentOneNorm(const T* data, indexType elementCount, indexType componentOffset)
{
    double total = 0.0;
    for (indexType k = 0; k < elementCount; ++k) {
        total += std::abs(static_cast<double>(data[2 * k + componentOffset]));
    }
    return total;
}
//=============================================================================
template <class T>
static double
finiteComplexOneNorm(const T* data, indexType elementCount)
{
    double total = 0.0;
    for (indexType k = 0; k < elementCount; ++k) {
        double realValue = static_cast<double>(data[2 * k]);
        double imagValue = static_cast<double>(data[2 * k + 1]);
        if (std::isfinite(realValue) && std::isfinite(imagValue)) {
            total += std::hypot(realValue, imagValue);
        }
    }
    return total;
}
//=============================================================================
static double
finiteRealOneNorm(const ArrayOf& value)
{
    const void* data = value.dataPointer();
    indexType elementCount = value.elementCount();
    double total = 0.0;
    if (value.dataClass() == NLS_SINGLE) {
        const auto* singleData = static_cast<const single*>(data);
        for (indexType k = 0; k < elementCount; ++k) {
            double x = static_cast<double>(singleData[k]);
            if (std::isfinite(x)) {
                total += std::abs(x);
            }
        }
        return total;
    }
    const auto* doubleData = static_cast<const double*>(data);
    for (indexType k = 0; k < elementCount; ++k) {
        double x = doubleData[k];
        if (std::isfinite(x)) {
            total += std::abs(x);
        }
    }
    return total;
}
//=============================================================================
static double
defaultTolerance(const ArrayOf& value)
{
    indexType elementCount = value.elementCount();
    if (value.dataClass() == NLS_SCOMPLEX) {
        return 1e-6
            * finiteComplexOneNorm(static_cast<const single*>(value.dataPointer()), elementCount);
    }
    if (value.dataClass() == NLS_DCOMPLEX) {
        return 1e-6
            * finiteComplexOneNorm(static_cast<const double*>(value.dataPointer()), elementCount);
    }
    return 1e-6 * finiteRealOneNorm(value);
}
//=============================================================================
static double
realOneNorm(const ArrayOf& value)
{
    indexType elementCount = value.elementCount();
    if (value.dataClass() == NLS_SCOMPLEX) {
        return componentOneNorm(static_cast<const single*>(value.dataPointer()), elementCount, 0);
    }
    if (value.dataClass() == NLS_DCOMPLEX) {
        return componentOneNorm(static_cast<const double*>(value.dataPointer()), elementCount, 0);
    }
    return finiteRealOneNorm(value);
}
//=============================================================================
static double
imagOneNorm(const ArrayOf& value)
{
    indexType elementCount = value.elementCount();
    if (value.dataClass() == NLS_SCOMPLEX) {
        return componentOneNorm(static_cast<const single*>(value.dataPointer()), elementCount, 1);
    }
    if (value.dataClass() == NLS_DCOMPLEX) {
        return componentOneNorm(static_cast<const double*>(value.dataPointer()), elementCount, 1);
    }
    return 0.0;
}
//=============================================================================
static double
lcmDouble(double a, double b)
{
    if (a == 0.0 || b == 0.0 || !std::isfinite(a) || !std::isfinite(b)) {
        return 0.0;
    }
    long long ia = static_cast<long long>(std::llround(std::abs(a)));
    long long ib = static_cast<long long>(std::llround(std::abs(b)));
    return static_cast<double>((ia / std::gcd(ia, ib)) * ib);
}
//=============================================================================
static ArrayOf
combineComplexNumerator(const ArrayOf& numeratorReal, const ArrayOf& denominatorReal,
    const ArrayOf& numeratorImag, const ArrayOf& denominatorImag)
{
    NelsonType outputClass = numeratorReal.dataClass() == NLS_SINGLE ? NLS_SCOMPLEX : NLS_DCOMPLEX;
    Dimensions dims = numeratorReal.dimensions();
    indexType elementCount = dims.elementCount();
    void* data = ArrayOf::allocateArrayOf(outputClass, elementCount);
    for (indexType k = 0; k < elementCount; ++k) {
        double denReal
            = readRealElement(denominatorReal.dataPointer(), denominatorReal.dataClass(), k);
        double denImag
            = readRealElement(denominatorImag.dataPointer(), denominatorImag.dataClass(), k);
        double commonDenominator = lcmDouble(denReal, denImag);
        double realValue = commonDenominator / denReal
            * readRealElement(numeratorReal.dataPointer(), numeratorReal.dataClass(), k);
        double imagValue = commonDenominator / denImag
            * readRealElement(numeratorImag.dataPointer(), numeratorImag.dataClass(), k);
        if (outputClass == NLS_SCOMPLEX) {
            auto* out = static_cast<single*>(data);
            out[2 * k] = static_cast<single>(realValue);
            out[2 * k + 1] = static_cast<single>(imagValue);
        } else {
            auto* out = static_cast<double*>(data);
            out[2 * k] = realValue;
            out[2 * k + 1] = imagValue;
        }
    }
    return ArrayOf(outputClass, dims, data);
}
//=============================================================================
static ArrayOf
combineDenominator(const ArrayOf& denominatorReal, const ArrayOf& denominatorImag)
{
    NelsonType outputClass = denominatorReal.dataClass();
    Dimensions dims = denominatorReal.dimensions();
    indexType elementCount = dims.elementCount();
    void* data = ArrayOf::allocateArrayOf(outputClass, elementCount);
    for (indexType k = 0; k < elementCount; ++k) {
        double value = lcmDouble(readRealElement(denominatorReal.dataPointer(), outputClass, k),
            readRealElement(denominatorImag.dataPointer(), outputClass, k));
        writeRealElement(data, outputClass, k, value);
    }
    return ArrayOf(outputClass, dims, data);
}
//=============================================================================
static std::vector<std::wstring>
charRows(const ArrayOf& value)
{
    std::vector<std::wstring> rows;
    Dimensions dims = value.dimensions();
    indexType rowCount = dims.rows();
    indexType columnCount = dims.columns();
    rows.reserve(rowCount);
    const auto* data = static_cast<const charType*>(value.dataPointer());
    for (indexType row = 0; row < rowCount; ++row) {
        std::wstring text;
        text.reserve(columnCount);
        for (indexType col = 0; col < columnCount; ++col) {
            text.push_back(static_cast<wchar_t>(data[col * rowCount + row]));
        }
        rows.push_back(text);
    }
    return rows;
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
combineComplexString(const ArrayOf& realPart, const ArrayOf& imagPart, double tol)
{
    ArrayOfVector realRows = RationalApproximation(realPart, tol, false);
    ArrayOfVector imagRows = RationalApproximation(imagPart, tol, false);
    std::vector<std::wstring> rows = charRows(realRows[0]);
    rows.push_back(L" +i* ...");
    std::vector<std::wstring> imagText = charRows(imagRows[0]);
    rows.insert(rows.end(), imagText.begin(), imagText.end());
    return charMatrixFromRows(rows);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::ratBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 2);
    const ArrayOf& input = argIn[0];
    if (!isFloatArray(input)) {
        Error(L"Nelson:elementary_functions:valueFloat", L"Value must be a float.");
    }

    double tol = argIn.size() == 2 && !argIn[1].isEmpty() ? argIn[1].asDoubleScalar()
                                                          : defaultTolerance(input);
    if (!input.isComplex()) {
        return RationalApproximation(input, tol, nLhs > 1);
    }

    ArrayOf realPart = RealPart(input);
    if (imagOneNorm(input) <= tol * realOneNorm(input)) {
        return RationalApproximation(realPart, tol, nLhs > 1);
    }

    ArrayOf imagPart = ImagPart(input);
    if (nLhs > 1) {
        ArrayOfVector realApprox = RationalApproximation(realPart, tol, true);
        ArrayOfVector imagApprox = RationalApproximation(imagPart, tol, true);
        ArrayOf denominator = combineDenominator(realApprox[1], imagApprox[1]);
        ArrayOfVector retval;
        retval << combineComplexNumerator(
            realApprox[0], realApprox[1], imagApprox[0], imagApprox[1]);
        retval << denominator;
        return retval;
    }
    return ArrayOfVector { combineComplexString(realPart, imagPart, tol) };
}
//=============================================================================
