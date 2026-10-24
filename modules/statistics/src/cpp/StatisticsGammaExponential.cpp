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
static double
gammaPdfValue(double x, double a, double b)
{
    if (std::isnan(x) || std::isnan(a) || std::isnan(b) || a <= 0 || b <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x < 0 || std::isinf(x)) {
        return 0;
    }
    if (x == 0) {
        if (a < 1) {
            return std::numeric_limits<double>::infinity();
        }
        if (a == 1) {
            return 1.0 / b;
        }
        return 0;
    }
    return std::exp((a - 1.0) * std::log(x) - x / b - std::lgamma(a) - a * std::log(b));
}
//=============================================================================
static double
gammaCdfValue(double x, double a, double b, bool upper)
{
    if (std::isnan(x) || std::isnan(a) || std::isnan(b) || a <= 0 || b <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    double z = 2.0 * x / b;
    return upper ? ChiSquareUpper(z, 2.0 * a) : ChiSquareCdf(z, 2.0 * a);
}
//=============================================================================
static double
gammaInvValue(double p, double a, double b)
{
    if (std::isnan(p) || std::isnan(a) || std::isnan(b) || a <= 0 || b <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    return 0.5 * b * ChiSquareInv(p, 2.0 * a);
}
//=============================================================================
static double
expPdfValue(double x, double mu)
{
    if (std::isnan(x) || std::isnan(mu) || mu <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x < 0 || std::isinf(x)) {
        return 0;
    }
    return std::exp(-x / mu) / mu;
}
//=============================================================================
static double
expCdfValue(double x, double mu, bool upper)
{
    if (std::isnan(x) || std::isnan(mu) || mu <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x < 0) {
        return upper ? 1.0 : 0.0;
    }
    if (std::isinf(x)) {
        return upper ? 0.0 : 1.0;
    }
    return upper ? std::exp(-x / mu) : -std::expm1(-x / mu);
}
//=============================================================================
static double
expInvValue(double p, double mu)
{
    if (std::isnan(p) || std::isnan(mu) || p < 0 || p > 1 || mu <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (p == 1) {
        return std::numeric_limits<double>::infinity();
    }
    return -mu * std::log1p(-p);
}
//=============================================================================
static ArrayOf
applyBinary(const ArrayOfVector& args, const std::wstring& firstMessage, int mode, bool upper)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf first = asDoubleArray(args[0], firstMessage);
    ArrayOf second = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"MU"));
    ArrayOfVector values;
    values.push_back(first);
    values.push_back(second);
    Dimensions dims = commonDimensions(values);
    indexType count = dims.elementCount();
    void* out = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double a = valueAt(first, k);
        double b = valueAt(second, k);
        double value = mode == 0 ? expPdfValue(a, b)
                                 : (mode == 1 ? expCdfValue(a, b, upper) : expInvValue(a, b));
        setFloatingValue(out, outputType, k, value);
    }
    return ArrayOf(outputType, dims, out);
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
    values.push_back(first);
    values.push_back(a);
    values.push_back(b);
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
        double value = mode == 0
            ? gammaPdfValue(x, av, bv)
            : (mode == 1 ? gammaCdfValue(x, av, bv, upper) : gammaInvValue(x, av, bv));
        setFloatingValue(out, outputType, k, value);
    }
    return ArrayOf(outputType, dims, out);
}
//=============================================================================
static ArrayOfVector
withDefaultScale(ArrayOfVector args, indexType expected)
{
    if (args.size() == expected - 1) {
        args.push_back(ArrayOf::doubleScalar(1));
    }
    return args;
}
//=============================================================================
ArrayOf
GammaPdf(const ArrayOfVector& args)
{
    return applyTernary(withDefaultScale(args, 3),
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        0, false);
}
//=============================================================================
ArrayOf
GammaCdf(const ArrayOfVector& args)
{
    bool upper = false;
    ArrayOfVector numericArgs = args;
    if (!numericArgs.empty() && isTextScalar(numericArgs.back())) {
        upper = matchValue(numericArgs.back(), { L"upper" },
                    formatI18nMessage(
                        resolveMessageTemplate(L"Nelson:statistics:invalidDistributionOption",
                            L"Invalid %s distribution option."),
                        L"gamma"))
            == L"upper";
        numericArgs.pop_back();
    }
    numericArgs = withDefaultScale(numericArgs, 3);
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
GammaInv(const ArrayOfVector& args)
{
    return applyTernary(withDefaultScale(args, 3),
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"P"),
        2, false);
}
//=============================================================================
ArrayOf
GammaRnd(const ArrayOfVector& args)
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
    ArrayOfVector params;
    params << a << b;
    Dimensions dims = args.size() > 2 ? dimensionsFromSizeArgs(args, 2) : commonDimensions(params);
    if (args.size() > 2) {
        validateRequestedDimensions(a, b, dims);
    }
    if (dims.isEmpty(false)) {
        ArrayOf result = ArrayOf::empty(dims);
        result.promoteType(outputType);
        return result;
    }
    ArrayOf r = Rand(dims, outputType);
    void* ptr = r.readWriteDataPointer();
    indexType count = dims.elementCount();
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double value
            = gammaInvValue(getFloatingValue(ptr, outputType, k), valueAt(a, k), valueAt(b, k));
        setFloatingValue(ptr, outputType, k, value);
    }
    return r;
}
//=============================================================================
ArrayOf
ExpPdf(const ArrayOfVector& args)
{
    return applyBinary(withDefaultScale(args, 2),
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        0, false);
}
//=============================================================================
ArrayOf
ExpCdf(const ArrayOfVector& args)
{
    bool upper = false;
    ArrayOfVector numericArgs = args;
    if (!numericArgs.empty() && isTextScalar(numericArgs.back())) {
        upper = matchValue(numericArgs.back(), { L"upper" },
                    formatI18nMessage(
                        resolveMessageTemplate(L"Nelson:statistics:invalidDistributionOption",
                            L"Invalid %s distribution option."),
                        L"exponential"))
            == L"upper";
        numericArgs.pop_back();
    }
    numericArgs = withDefaultScale(numericArgs, 2);
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
ExpInv(const ArrayOfVector& args)
{
    return applyBinary(withDefaultScale(args, 2),
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"P"),
        2, false);
}
//=============================================================================
ArrayOf
ExpRnd(const ArrayOfVector& args)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf mu = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"MU"));
    Dimensions dims = args.size() > 1 ? dimensionsFromSizeArgs(args, 1) : mu.dimensions();
    if (args.size() > 1) {
        validateRequestedDimensions(mu, dims);
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
        double mv = valueAt(mu, k);
        double u = getFloatingValue(ptr, outputType, k);
        double value = std::isnan(mv) || mv <= 0 ? std::numeric_limits<double>::quiet_NaN()
                                                 : -mv * std::log1p(-u);
        setFloatingValue(ptr, outputType, k, value);
    }
    return r;
}
//=============================================================================
ExpStatResult
ExpStat(const ArrayOfVector& args)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf mu = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"MU"));
    Dimensions dims = mu.dimensions();
    indexType count = dims.elementCount();
    void* meanPtr = ArrayOf::allocateArrayOf(outputType, count);
    void* varPtr = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double mv = valueAt(mu, k);
        double mean = std::isnan(mv) || mv <= 0 ? std::numeric_limits<double>::quiet_NaN() : mv;
        double variance = std::isnan(mean) ? mean : mv * mv;
        setFloatingValue(meanPtr, outputType, k, mean);
        setFloatingValue(varPtr, outputType, k, variance);
    }
    ExpStatResult result;
    result.mean = ArrayOf(outputType, dims, meanPtr);
    result.variance = ArrayOf(outputType, dims, varPtr);
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
