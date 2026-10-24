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
static ArrayOfVector
withDefaultParameters(ArrayOfVector args)
{
    if (args.size() == 1) {
        args.push_back(ArrayOf::doubleScalar(1));
        args.push_back(ArrayOf::doubleScalar(1));
    } else if (args.size() == 2) {
        args.push_back(ArrayOf::doubleScalar(1));
    }
    return args;
}
//=============================================================================
static bool
validParameters(double a, double b)
{
    return !std::isnan(a) && !std::isnan(b) && a > 0 && b > 0;
}
//=============================================================================
static double
pdfValue(double x, double a, double b)
{
    if (std::isnan(x) || !validParameters(a, b)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x < 0 || std::isinf(x)) {
        return 0;
    }
    if (x == 0) {
        if (b < 1) {
            return std::numeric_limits<double>::infinity();
        }
        return b == 1 ? 1.0 / a : 0.0;
    }
    double z = x / a;
    return (b / a) * std::pow(z, b - 1.0) * std::exp(-std::pow(z, b));
}
//=============================================================================
static double
cdfValue(double x, double a, double b, bool upper)
{
    if (std::isnan(x) || !validParameters(a, b)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x < 0) {
        return upper ? 1.0 : 0.0;
    }
    if (std::isinf(x)) {
        return upper ? 0.0 : 1.0;
    }
    double z = -std::pow(x / a, b);
    return upper ? std::exp(z) : -std::expm1(z);
}
//=============================================================================
static double
invValue(double p, double a, double b)
{
    if (std::isnan(p) || p < 0 || p > 1 || !validParameters(a, b)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (p == 1) {
        return std::numeric_limits<double>::infinity();
    }
    return a * std::pow(-std::log1p(-p), 1.0 / b);
}
//=============================================================================
static ArrayOf
applyTernary(const ArrayOfVector& args, const std::wstring& firstMessage, int mode, bool upper)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf first = asDoubleArray(args[0], firstMessage);
    ArrayOf a = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"A"));
    ArrayOf b = asDoubleArray(args[2],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"B"));
    ArrayOfVector values;
    values << first << a << b;
    Dimensions dims = commonDimensions(values);
    indexType count = dims.elementCount();
    void* out = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double x = valueAt(first, k);
        double av = valueAt(a, k);
        double bv = valueAt(b, k);
        double value = mode == 0 ? pdfValue(x, av, bv)
                                 : (mode == 1 ? cdfValue(x, av, bv, upper) : invValue(x, av, bv));
        setFloatingValue(out, outputType, k, value);
    }
    return ArrayOf(outputType, dims, out);
}
//=============================================================================
ArrayOf
WeibullPdf(const ArrayOfVector& args)
{
    return applyTernary(withDefaultParameters(args),
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        0, false);
}
//=============================================================================
ArrayOf
WeibullCdf(const ArrayOfVector& args)
{
    bool upper = false;
    ArrayOfVector numericArgs = args;
    if (!numericArgs.empty() && isTextScalar(numericArgs.back())) {
        upper = matchValue(numericArgs.back(), { L"upper" },
                    formatI18nMessage(
                        resolveMessageTemplate(L"Nelson:statistics:invalidDistributionOption",
                            L"Invalid %s distribution option."),
                        L"Weibull"))
            == L"upper";
        numericArgs.pop_back();
    }
    numericArgs = withDefaultParameters(numericArgs);
    if (numericArgs.size() < 3) {
        Error("Nelson:notEnoughInputs", "Not enough input arguments.");
    }
    if (numericArgs.size() > 3) {
        Error("Nelson:tooManyInputs", "Too many input arguments.");
    }
    return applyTernary(numericArgs,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        1, upper);
}
//=============================================================================
ArrayOf
WeibullInv(const ArrayOfVector& args)
{
    return applyTernary(withDefaultParameters(args),
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"P"),
        2, false);
}
//=============================================================================
ArrayOf
WeibullRnd(const ArrayOfVector& args)
{
    ArrayOfVector typeArgs;
    typeArgs << args[0] << args[1];
    NelsonType outputType = floatingOutputType(typeArgs);
    ArrayOf a = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"A"));
    ArrayOf b = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"B"));
    Dimensions dims
        = args.size() > 2 ? dimensionsFromSizeArgs(args, 2) : commonDimensions(typeArgs);
    if (args.size() > 2) {
        validateRequestedDimensions(a, b, dims);
    }
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
        double av = valueAt(a, k);
        double bv = valueAt(b, k);
        double value = !validParameters(av, bv)
            ? std::numeric_limits<double>::quiet_NaN()
            : av * std::pow(-std::log1p(-getFloatingValue(ptr, outputType, k)), 1.0 / bv);
        setFloatingValue(ptr, outputType, k, value);
    }
    return u;
}
//=============================================================================
WeibullStatResult
WeibullStat(const ArrayOfVector& args)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf a = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"A"));
    ArrayOf b = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"B"));
    if (a.isEmpty(true) || b.isEmpty(true)) {
        Dimensions dims = a.isEmpty(true) ? a.dimensions() : b.dimensions();
        ArrayOf mean = ArrayOf::empty(dims);
        ArrayOf variance = ArrayOf::empty(dims);
        mean.promoteType(outputType);
        variance.promoteType(outputType);
        WeibullStatResult result;
        result.mean = mean;
        result.variance = variance;
        return result;
    }
    ArrayOfVector values;
    values << a << b;
    Dimensions dims = commonDimensions(values);
    indexType count = dims.elementCount();
    void* meanPtr = ArrayOf::allocateArrayOf(outputType, count);
    void* varPtr = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double av = valueAt(a, k);
        double bv = valueAt(b, k);
        double mean = std::numeric_limits<double>::quiet_NaN();
        double variance = mean;
        if (validParameters(av, bv)) {
            double g1 = std::tgamma(1.0 + 1.0 / bv);
            double g2 = std::tgamma(1.0 + 2.0 / bv);
            mean = av * g1;
            variance = av * av * (g2 - g1 * g1);
        }
        setFloatingValue(meanPtr, outputType, k, mean);
        setFloatingValue(varPtr, outputType, k, variance);
    }
    WeibullStatResult result;
    result.mean = ArrayOf(outputType, dims, meanPtr);
    result.variance = ArrayOf(outputType, dims, varPtr);
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
