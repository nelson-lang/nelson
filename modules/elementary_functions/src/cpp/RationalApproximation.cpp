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
#include <limits>
#include <string>
#include <vector>
#include "RationalApproximation.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static double
spacingAt(double value, bool singlePrecision);
static std::wstring
integerToWideString(double value);
static std::wstring
nonFiniteContinuedFraction(double value);
//=============================================================================
// Convergents of the continued fraction of x0, up to the tolerance. The denominator
// stays positive. When terms is not null it receives the signed integer terms as strings.
static void
approximateElement(double x0, double tolerance, double spacing, double& numerator,
    double& denominator, std::vector<std::wstring>* terms)
{
    if (!std::isfinite(x0)) {
        numerator = std::isnan(x0) ? 0.0 : (x0 > 0.0 ? 1.0 : -1.0);
        denominator = 0.0;
        if (terms != nullptr) {
            terms->push_back(nonFiniteContinuedFraction(x0));
        }
        return;
    }
    double remainder = x0;
    double numCurr = 1.0, numPrev = 0.0;
    double denCurr = 0.0, denPrev = 1.0;
    while (true) {
        bool wasNegative = remainder < 0.0;
        double term = std::round(remainder);
        remainder -= term;
        double numNext = term * numCurr + numPrev;
        double denNext = term * denCurr + denPrev;
        numPrev = numCurr;
        numCurr = numNext;
        denPrev = denCurr;
        denCurr = denNext;
        if (terms != nullptr) {
            std::wstring rendered = integerToWideString(std::abs(term));
            if (wasNegative) {
                rendered.insert(rendered.begin(), L'-');
            }
            terms->push_back(rendered);
        }
        if (remainder == 0.0 || std::abs(numCurr / denCurr - x0) <= std::max(tolerance, spacing)) {
            break;
        }
        remainder = 1.0 / remainder;
    }
    if (denCurr < 0.0) {
        numCurr = -numCurr;
        denCurr = -denCurr;
    }
    numerator = numCurr;
    denominator = denCurr;
}
//=============================================================================
static std::wstring
foldContinuedFraction(const std::vector<std::wstring>& terms)
{
    if (terms.empty()) {
        return L"";
    }
    std::wstring folded = terms.back();
    for (size_t i = terms.size() - 1; i-- > 0;) {
        folded = terms[i] + L" + 1/(" + folded + L")";
    }
    return folded;
}
//=============================================================================
static double
readElement(const void* data, NelsonType dataClass, indexType index)
{
    if (dataClass == NLS_SINGLE) {
        return static_cast<double>(static_cast<const single*>(data)[index]);
    }
    return static_cast<const double*>(data)[index];
}
//=============================================================================
static void
writeElement(void* data, NelsonType dataClass, indexType index, double value)
{
    if (dataClass == NLS_SINGLE) {
        static_cast<single*>(data)[index] = static_cast<single>(value);
    } else {
        static_cast<double*>(data)[index] = value;
    }
}
//=============================================================================
ArrayOfVector
RationalApproximation(const ArrayOf& X, double tol, bool numericMode)
{
    ArrayOfVector retval;
    NelsonType dataClass = X.dataClass();
    bool singlePrecision = (dataClass == NLS_SINGLE);
    Dimensions dims = X.dimensions();
    indexType elementCount = dims.elementCount();
    const void* input = X.dataPointer();

    if (numericMode) {
        void* numData = ArrayOf::allocateArrayOf(dataClass, elementCount);
        ArrayOf numeratorArray(dataClass, dims, numData);
        void* denData = ArrayOf::allocateArrayOf(dataClass, elementCount);
        ArrayOf denominatorArray(dataClass, dims, denData);
        for (indexType k = 0; k < elementCount; ++k) {
            double x0 = readElement(input, dataClass, k);
            double numerator = 0.0;
            double denominator = 0.0;
            approximateElement(
                x0, tol, spacingAt(x0, singlePrecision), numerator, denominator, nullptr);
            writeElement(numData, dataClass, k, numerator);
            writeElement(denData, dataClass, k, denominator);
        }
        retval << numeratorArray;
        retval << denominatorArray;
        return retval;
    }

    if (elementCount == 0) {
        retval << ArrayOf(NLS_CHAR, Dimensions(0, 0), nullptr);
        return retval;
    }
    std::vector<std::wstring> rows;
    rows.reserve(elementCount);
    size_t width = 0;
    for (indexType k = 0; k < elementCount; ++k) {
        double x0 = readElement(input, dataClass, k);
        double numerator = 0.0;
        double denominator = 0.0;
        std::vector<std::wstring> terms;
        approximateElement(x0, tol, spacingAt(x0, singlePrecision), numerator, denominator, &terms);
        std::wstring row = foldContinuedFraction(terms);
        width = std::max(width, row.size());
        rows.push_back(row);
    }
    charType* buffer
        = static_cast<charType*>(ArrayOf::allocateArrayOf(NLS_CHAR, elementCount * width));
    for (indexType r = 0; r < elementCount; ++r) {
        const std::wstring& row = rows[r];
        for (size_t c = 0; c < width; ++c) {
            charType value = (c < row.size()) ? static_cast<charType>(row[c]) : L' ';
            buffer[c * elementCount + r] = value;
        }
    }
    retval << ArrayOf(NLS_CHAR, Dimensions(elementCount, static_cast<indexType>(width)), buffer);
    return retval;
}
//=============================================================================
static double
spacingAt(double value, bool singlePrecision)
{
    if (value == 0.0) {
        return singlePrecision ? static_cast<double>(std::numeric_limits<single>::denorm_min())
                               : std::numeric_limits<double>::denorm_min();
    }
    double magnitude = std::abs(value);
    if (singlePrecision) {
        single low = static_cast<single>(magnitude);
        single high = std::nextafter(low, std::numeric_limits<single>::infinity());
        return static_cast<double>(high - low);
    }
    double high = std::nextafter(magnitude, std::numeric_limits<double>::infinity());
    return high - magnitude;
}
//=============================================================================
static std::wstring
integerToWideString(double value)
{
    long long magnitude = static_cast<long long>(value);
    return std::to_wstring(magnitude);
}
//=============================================================================
static std::wstring
nonFiniteContinuedFraction(double value)
{
    if (std::isnan(value)) {
        return L"NaN";
    }
    return value > 0.0 ? L"Inf" : L"-Inf";
}
//=============================================================================
} // namespace Nelson
//=============================================================================
