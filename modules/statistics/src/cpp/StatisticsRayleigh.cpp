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
static constexpr double PI_VALUE = 3.141592653589793238462643383279502884;
static double
pdfValue(double x, double b)
{
    if (std::isnan(x) || std::isnan(b) || b <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x < 0 || std::isinf(x)) {
        return 0;
    }
    return x / (b * b) * std::exp(-(x * x) / (2.0 * b * b));
}
//=============================================================================
static double
cdfValue(double x, double b, bool upper)
{
    if (std::isnan(x) || std::isnan(b) || b <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x < 0) {
        return upper ? 1.0 : 0.0;
    }
    if (std::isinf(x)) {
        return upper ? 0.0 : 1.0;
    }
    double z = -(x * x) / (2.0 * b * b);
    return upper ? std::exp(z) : -std::expm1(z);
}
//=============================================================================
static double
invValue(double p, double b)
{
    if (std::isnan(p) || std::isnan(b) || p < 0 || p > 1 || b <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (p == 1) {
        return std::numeric_limits<double>::infinity();
    }
    return b * std::sqrt(-2.0 * std::log1p(-p));
}
//=============================================================================
static ArrayOf
applyBinary(const ArrayOfVector& args, const std::wstring& firstMessage, int mode, bool upper)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf first = asDoubleArray(args[0], firstMessage);
    ArrayOf b = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"B"));
    Dimensions dims = commonDimensions(first, b);
    indexType count = dims.elementCount();
    void* out = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double x = valueAt(first, k);
        double scale = valueAt(b, k);
        double value = mode == 0 ? pdfValue(x, scale)
                                 : (mode == 1 ? cdfValue(x, scale, upper) : invValue(x, scale));
        setFloatingValue(out, outputType, k, value);
    }
    return ArrayOf(outputType, dims, out);
}
//=============================================================================
ArrayOf
RayleighPdf(const ArrayOfVector& args)
{
    return applyBinary(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        0, false);
}
//=============================================================================
ArrayOf
RayleighCdf(const ArrayOfVector& args)
{
    bool upper = false;
    ArrayOfVector numericArgs = args;
    if (!numericArgs.empty() && isTextScalar(numericArgs.back())) {
        upper = matchValue(numericArgs.back(), { L"upper" },
                    formatI18nMessage(
                        resolveMessageTemplate(L"Nelson:statistics:invalidDistributionOption",
                            L"Invalid %s distribution option."),
                        L"Rayleigh"))
            == L"upper";
        numericArgs.pop_back();
    }
    if (numericArgs.size() < 2) {
        Error("Nelson:notEnoughInputs", "Not enough input arguments.");
    }
    if (numericArgs.size() > 2) {
        Error("Nelson:tooManyInputs", "Too many input arguments.");
    }
    return applyBinary(numericArgs,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        1, upper);
}
//=============================================================================
ArrayOf
RayleighInv(const ArrayOfVector& args)
{
    return applyBinary(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"P"),
        2, false);
}
//=============================================================================
ArrayOf
RayleighRnd(const ArrayOfVector& args)
{
    ArrayOf b = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"B"));
    Dimensions dims = args.size() > 1 ? dimensionsFromSizeArgs(args, 1) : b.dimensions();
    if (args.size() > 1) {
        validateRequestedDimensions(b, dims);
    }
    ArrayOfVector typeArgs;
    typeArgs << args[0];
    NelsonType outputType = floatingOutputType(typeArgs);
    if (dims.isEmpty(false)) {
        ArrayOf res = ArrayOf::empty(dims);
        res.promoteType(outputType);
        return res;
    }
    ArrayOf u = Rand(dims, outputType);
    void* ptr = u.readWriteDataPointer();
    indexType count = dims.elementCount();
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double scale = valueAt(b, k);
        double value = scale <= 0 || std::isnan(scale)
            ? std::numeric_limits<double>::quiet_NaN()
            : scale * std::sqrt(-2.0 * std::log1p(-getFloatingValue(ptr, outputType, k)));
        setFloatingValue(ptr, outputType, k, value);
    }
    return u;
}
//=============================================================================
RayleighStatResult
RayleighStat(const ArrayOfVector& args)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf b = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"B"));
    if (b.isEmpty(true)) {
        ArrayOf mean = ArrayOf::empty(b.dimensions());
        ArrayOf variance = ArrayOf::empty(b.dimensions());
        mean.promoteType(outputType);
        variance.promoteType(outputType);
        RayleighStatResult result;
        result.mean = mean;
        result.variance = variance;
        return result;
    }
    Dimensions dims = b.dimensions();
    indexType count = dims.elementCount();
    void* meanPtr = ArrayOf::allocateArrayOf(outputType, count);
    void* varPtr = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double scale = valueAt(b, k);
        double mean = scale <= 0 || std::isnan(scale) ? std::numeric_limits<double>::quiet_NaN()
                                                      : scale * std::sqrt(PI_VALUE / 2.0);
        double variance = std::isnan(mean) ? mean : (2.0 - PI_VALUE / 2.0) * scale * scale;
        setFloatingValue(meanPtr, outputType, k, mean);
        setFloatingValue(varPtr, outputType, k, variance);
    }
    RayleighStatResult result;
    result.mean = ArrayOf(outputType, dims, meanPtr);
    result.variance = ArrayOf(outputType, dims, varPtr);
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
