//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StatisticsDistributions.hpp"
#include "StatisticsHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Rand.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
using namespace StatisticsHelpers;
//=============================================================================
static bool
validProbability(double p)
{
    return !std::isnan(p) && p >= 0 && p <= 1;
}
//=============================================================================
static bool
isNonnegativeInteger(double value)
{
    return std::isfinite(value) && value >= 0 && std::floor(value) == value;
}
//=============================================================================
static double
geometricPdfValue(double x, double p)
{
    if (std::isnan(x) || !validProbability(p)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (!isNonnegativeInteger(x)) {
        return 0;
    }
    if (p == 0) {
        return 0;
    }
    if (p == 1) {
        return x == 0 ? 1 : 0;
    }
    return std::exp(std::log(p) + x * std::log1p(-p));
}
//=============================================================================
static double
geometricCdfValue(double x, double p, bool upper)
{
    if (std::isnan(x) || !validProbability(p)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    double k = std::floor(x);
    if (k < 0) {
        return upper ? 1 : 0;
    }
    if (p == 0) {
        return upper ? 1 : 0;
    }
    if (p == 1) {
        return upper ? 0 : 1;
    }
    if (std::isinf(k)) {
        return upper ? 0 : 1;
    }
    double logUpper = (k + 1) * std::log1p(-p);
    return upper ? std::exp(logUpper) : -std::expm1(logUpper);
}
//=============================================================================
static double
geometricInvValue(double y, double p)
{
    if (std::isnan(y) || y < 0 || y > 1 || !validProbability(p)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (y == 0 || p == 1) {
        return 0;
    }
    if (p == 0 || y == 1) {
        return std::numeric_limits<double>::infinity();
    }
    double q = std::ceil(std::log1p(-y) / std::log1p(-p) - 1);
    q = std::max(0.0, q);
    double tolerance = 64 * std::numeric_limits<double>::epsilon() * std::max(1.0, std::fabs(y));
    while (q > 0 && geometricCdfValue(q - 1, p, false) >= y - tolerance) {
        q -= 1;
    }
    while (geometricCdfValue(q, p, false) < y - tolerance) {
        q += 1;
    }
    return q;
}
//=============================================================================
static ArrayOf
binaryGeometricFunction(
    const ArrayOfVector& args, const std::wstring& firstMessage, int mode, bool upper)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf first = asDoubleArray(args[0], firstMessage);
    ArrayOf p = asDoubleArray(args[1],
        resolveMessageTemplate(L"Nelson:statistics:pNumericArrayRange",
            L"P must be a numeric array in the range [0,1]."));
    Dimensions dims = commonDimensions(first, p);
    indexType count = dims.elementCount();
    void* out = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double a = valueAt(first, k);
        double pv = valueAt(p, k);
        double value = mode == 0
            ? geometricPdfValue(a, pv)
            : (mode == 1 ? geometricCdfValue(a, pv, upper) : geometricInvValue(a, pv));
        setFloatingValue(out, outputType, k, value);
    }
    return ArrayOf(outputType, dims, out);
}
//=============================================================================
ArrayOf
GeometricPdf(const ArrayOfVector& args)
{
    return binaryGeometricFunction(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        0, false);
}
//=============================================================================
ArrayOf
GeometricCdf(const ArrayOfVector& args)
{
    bool upper = false;
    ArrayOfVector numericArgs = args;
    if (!numericArgs.empty() && isTextScalar(numericArgs.back())) {
        upper = matchValue(numericArgs.back(), { L"upper" },
                    formatI18nMessage(
                        resolveMessageTemplate(L"Nelson:statistics:invalidDistributionOption",
                            L"Invalid %s distribution option."),
                        L"geometric"))
            == L"upper";
        numericArgs.pop_back();
    }
    if (numericArgs.size() < 2) {
        Error("Nelson:notEnoughInputs", "Not enough input arguments.");
    }
    if (numericArgs.size() > 2) {
        Error("Nelson:tooManyInputs", "Too many input arguments.");
    }
    return binaryGeometricFunction(numericArgs,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        1, upper);
}
//=============================================================================
ArrayOf
GeometricInv(const ArrayOfVector& args)
{
    return binaryGeometricFunction(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"Y"),
        2, false);
}
//=============================================================================
ArrayOf
GeometricRnd(const ArrayOfVector& args)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf p = asDoubleArray(args[0],
        resolveMessageTemplate(L"Nelson:statistics:pNumericArrayRange",
            L"P must be a numeric array in the range [0,1]."));
    Dimensions dims = args.size() > 1 ? dimensionsFromSizeArgs(args, 1) : p.dimensions();
    if (args.size() > 1) {
        validateRequestedDimensions(p, dims);
    }
    if (dims.isEmpty(false)) {
        ArrayOf res = ArrayOf::empty(dims);
        res.promoteType(outputType);
        return res;
    }
    ArrayOf r = Rand(dims, outputType);
    indexType count = dims.elementCount();
    void* ptr = r.readWriteDataPointer();
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double pv = valueAt(p, k);
        double u = getFloatingValue(ptr, outputType, k);
        double value = std::numeric_limits<double>::quiet_NaN();
        if (validProbability(pv)) {
            value = pv == 1 ? 0
                            : (pv == 0 ? std::numeric_limits<double>::infinity()
                                       : std::floor(std::log1p(-u) / std::log1p(-pv)));
        }
        setFloatingValue(ptr, outputType, k, value);
    }
    return r;
}
//=============================================================================
GeometricStatResult
GeometricStat(const ArrayOfVector& args)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf p = asDoubleArray(args[0],
        resolveMessageTemplate(L"Nelson:statistics:pNumericArrayRange",
            L"P must be a numeric array in the range [0,1]."));
    Dimensions dims = p.dimensions();
    indexType count = dims.elementCount();
    void* meanPtr = ArrayOf::allocateArrayOf(outputType, count);
    void* varPtr = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double pv = valueAt(p, k);
        double mean = std::numeric_limits<double>::quiet_NaN();
        double variance = mean;
        if (validProbability(pv)) {
            if (pv == 0) {
                mean = std::numeric_limits<double>::infinity();
                variance = mean;
            } else {
                mean = (1 - pv) / pv;
                variance = (1 - pv) / (pv * pv);
            }
        }
        setFloatingValue(meanPtr, outputType, k, mean);
        setFloatingValue(varPtr, outputType, k, variance);
    }
    GeometricStatResult result;
    result.mean = ArrayOf(outputType, dims, meanPtr);
    result.variance = ArrayOf(outputType, dims, varPtr);
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
