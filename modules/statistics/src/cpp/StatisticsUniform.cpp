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
#include <array>
#include <cmath>
#include <limits>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
using namespace StatisticsHelpers;
//=============================================================================
static bool
validBounds(double a, double b)
{
    return !std::isnan(a) && !std::isnan(b) && a < b;
}
//=============================================================================
static double
uniformPdfValue(double x, double a, double b)
{
    if (std::isnan(x) || !validBounds(a, b)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x < a || x > b) {
        return 0;
    }
    return 1.0 / (b - a);
}
//=============================================================================
static double
uniformCdfValue(double x, double a, double b, bool upper)
{
    if (std::isnan(x) || !validBounds(a, b)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x < a) {
        return upper ? 1.0 : 0.0;
    }
    if (x >= b) {
        return upper ? 0.0 : 1.0;
    }
    return upper ? (b - x) / (b - a) : (x - a) / (b - a);
}
//=============================================================================
static double
uniformInvValue(double p, double a, double b)
{
    if (std::isnan(p) || p < 0 || p > 1 || !validBounds(a, b)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    return a + p * (b - a);
}
//=============================================================================
static ArrayOfVector
withDefaultBounds(ArrayOfVector args)
{
    if (args.size() == 1) {
        args.push_back(ArrayOf::doubleScalar(0));
        args.push_back(ArrayOf::doubleScalar(1));
    }
    return args;
}
//=============================================================================
static ArrayOf
ternaryUniformFunction(
    const ArrayOfVector& args, const std::wstring& firstMessage, int mode, bool upper)
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
    Dimensions dims = commonDimensions(first, a, b);
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
            ? uniformPdfValue(x, av, bv)
            : (mode == 1 ? uniformCdfValue(x, av, bv, upper) : uniformInvValue(x, av, bv));
        setFloatingValue(out, outputType, k, value);
    }
    return ArrayOf(outputType, dims, out);
}
//=============================================================================
ArrayOf
UniformPdf(const ArrayOfVector& args)
{
    ArrayOfVector numericArgs = withDefaultBounds(args);
    if (numericArgs.size() < 3) {
        Error("Nelson:notEnoughInputs", "Not enough input arguments.");
    }
    if (numericArgs.size() > 3) {
        Error("Nelson:tooManyInputs", "Too many input arguments.");
    }
    return ternaryUniformFunction(numericArgs,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        0, false);
}
//=============================================================================
ArrayOf
UniformCdf(const ArrayOfVector& args)
{
    bool upper = false;
    ArrayOfVector numericArgs = args;
    if (!numericArgs.empty() && isTextScalar(numericArgs.back())) {
        upper = matchValue(numericArgs.back(), { L"upper" },
                    formatI18nMessage(
                        resolveMessageTemplate(L"Nelson:statistics:invalidDistributionOption",
                            L"Invalid %s distribution option."),
                        L"uniform"))
            == L"upper";
        numericArgs.pop_back();
    }
    numericArgs = withDefaultBounds(numericArgs);
    if (numericArgs.size() < 3) {
        Error("Nelson:notEnoughInputs", "Not enough input arguments.");
    }
    if (numericArgs.size() > 3) {
        Error("Nelson:tooManyInputs", "Too many input arguments.");
    }
    return ternaryUniformFunction(numericArgs,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        1, upper);
}
//=============================================================================
ArrayOf
UniformInv(const ArrayOfVector& args)
{
    ArrayOfVector numericArgs = withDefaultBounds(args);
    if (numericArgs.size() < 3) {
        Error("Nelson:notEnoughInputs", "Not enough input arguments.");
    }
    if (numericArgs.size() > 3) {
        Error("Nelson:tooManyInputs", "Too many input arguments.");
    }
    return ternaryUniformFunction(numericArgs,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"P"),
        2, false);
}
//=============================================================================
ArrayOf
UniformRnd(const ArrayOfVector& args)
{
    ArrayOfVector typeArgs;
    typeArgs << args[0];
    typeArgs << args[1];
    NelsonType outputType = floatingOutputType(typeArgs);
    ArrayOf a = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"A"));
    ArrayOf b = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"B"));
    Dimensions dims = args.size() > 2 ? dimensionsFromSizeArgs(args, 2) : commonDimensions(a, b, b);
    if (args.size() > 2) {
        validateRequestedDimensions(a, b, dims);
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
        double av = valueAt(a, k);
        double bv = valueAt(b, k);
        double u = getFloatingValue(ptr, outputType, k);
        double value = std::isnan(av) || std::isnan(bv) || av >= bv
            ? std::numeric_limits<double>::quiet_NaN()
            : av + (bv - av) * u;
        setFloatingValue(ptr, outputType, k, value);
    }
    return r;
}
//=============================================================================
UniformStatResult
UniformStat(const ArrayOfVector& args)
{
    ArrayOfVector typeArgs;
    typeArgs << args[0];
    typeArgs << args[1];
    NelsonType outputType = floatingOutputType(typeArgs);
    ArrayOf a = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"A"));
    ArrayOf b = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"B"));
    Dimensions dims = commonDimensions(a, b, b);
    indexType count = dims.elementCount();
    void* meanPtr = ArrayOf::allocateArrayOf(outputType, count);
    void* varPtr = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double av = valueAt(a, k);
        double bv = valueAt(b, k);
        double mean = std::isnan(av) || std::isnan(bv) || av >= bv
            ? std::numeric_limits<double>::quiet_NaN()
            : 0.5 * (av + bv);
        double width = bv - av;
        double variance = std::isnan(mean) ? mean : width * width / 12.0;
        setFloatingValue(meanPtr, outputType, k, mean);
        setFloatingValue(varPtr, outputType, k, variance);
    }
    UniformStatResult result;
    result.mean = ArrayOf(outputType, dims, meanPtr);
    result.variance = ArrayOf(outputType, dims, varPtr);
    return result;
}
//=============================================================================
static Dimensions
fitOutputDimensions(const Dimensions& dims)
{
    return Dimensions(1, dims.columns());
}
//=============================================================================
UniformFitResult
UniformFit(const ArrayOfVector& args)
{
    ArrayOf x = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    if (x.isEmpty(false) || x.dimensions().length() > 2) {
        Error("Nelson:statistics:xMustBeNonemptyVectorOrMatrix",
            "X must be a nonempty vector or matrix.");
    }
    double alpha = alphaValue(args);
    Dimensions dims = x.dimensions();
    bool vectorInput = x.isVector();
    indexType rows = vectorInput ? x.elementCount() : dims.rows();
    indexType cols = vectorInput ? 1 : dims.columns();
    Dimensions outDims = vectorInput ? Dimensions(1, 1) : fitOutputDimensions(dims);
    double* aHat = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, cols));
    double* bHat = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, cols));
    double* aCI = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 2 * cols));
    double* bCI = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 2 * cols));
    const double* xp = static_cast<const double*>(x.dataPointer());
#if WITH_OPENMP
#pragma omp parallel for if (cols > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType c = 0; c < (ompIndexType)cols; ++c) {
        double mn = xp[c * rows];
        double mx = mn;
        bool hasNaN = std::isnan(mn);
        for (indexType r = 1; r < rows; ++r) {
            double v = xp[c * rows + r];
            if (std::isnan(v)) {
                hasNaN = true;
            } else {
                mn = std::min(mn, v);
                mx = std::max(mx, v);
            }
        }
        if (hasNaN) {
            mn = std::numeric_limits<double>::quiet_NaN();
            mx = mn;
        }
        double delta = std::isnan(mn) ? mn : (mx - mn) * (std::pow(alpha, -1.0 / rows) - 1.0);
        aHat[c] = mn;
        bHat[c] = mx;
        aCI[2 * c] = mn - delta;
        aCI[2 * c + 1] = mn;
        bCI[2 * c] = mx;
        bCI[2 * c + 1] = mx + delta;
    }
    UniformFitResult result;
    result.aHat = ArrayOf(NLS_DOUBLE, outDims, aHat);
    result.bHat = ArrayOf(NLS_DOUBLE, outDims, bHat);
    result.aCI = ArrayOf(NLS_DOUBLE, Dimensions(2, cols), aCI);
    result.bCI = ArrayOf(NLS_DOUBLE, Dimensions(2, cols), bCI);
    return result;
}
//=============================================================================
static double
uniformLogSurvival(double x, double a, double b)
{
    if (x < a) {
        return 0;
    }
    if (x >= b) {
        return -std::numeric_limits<double>::infinity();
    }
    return std::log((b - x) / (b - a));
}
//=============================================================================
static double
uniformNegativeLogLikelihood(double a, double b, const double* x, const double* censoring,
    const double* frequency, indexType count)
{
    if (std::isnan(a) || std::isnan(b)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (!validBounds(a, b)) {
        return std::numeric_limits<double>::infinity();
    }
    double logLikelihood = 0;
    double logDensity = -std::log(b - a);
    for (indexType k = 0; k < count; ++k) {
        double xv = x[k];
        if (std::isnan(xv)) {
            return std::numeric_limits<double>::quiet_NaN();
        }
        double weight = frequency[k];
        if (weight == 0) {
            continue;
        }
        if (censoring[k] == 0) {
            if (xv < a || xv > b) {
                return std::numeric_limits<double>::infinity();
            }
            logLikelihood += weight * logDensity;
        } else {
            logLikelihood += weight * uniformLogSurvival(xv, a, b);
        }
    }
    return -logLikelihood;
}
//=============================================================================
static std::array<double, 4>
uniformCovariance(
    double a, double b, const double* x, const double* c, const double* f, indexType count)
{
    double width = b - a;
    double ha = std::max(1e-5 * std::abs(a), width * 1e-6);
    double hb = std::max(1e-5 * std::abs(b), width * 1e-6);
    double f00 = uniformNegativeLogLikelihood(a, b, x, c, f, count);
    double fap = uniformNegativeLogLikelihood(a + ha, b, x, c, f, count);
    double fam = uniformNegativeLogLikelihood(a - ha, b, x, c, f, count);
    double fbp = uniformNegativeLogLikelihood(a, b + hb, x, c, f, count);
    double fbm = uniformNegativeLogLikelihood(a, b - hb, x, c, f, count);
    double fPP = uniformNegativeLogLikelihood(a + ha, b + hb, x, c, f, count);
    double fPM = uniformNegativeLogLikelihood(a + ha, b - hb, x, c, f, count);
    double fMP = uniformNegativeLogLikelihood(a - ha, b + hb, x, c, f, count);
    double fMM = uniformNegativeLogLikelihood(a - ha, b - hb, x, c, f, count);
    double info00 = (fap - 2.0 * f00 + fam) / (ha * ha);
    double info11 = (fbp - 2.0 * f00 + fbm) / (hb * hb);
    double info01 = (fPP - fPM - fMP + fMM) / (4.0 * ha * hb);
    double det = info00 * info11 - info01 * info01;
    std::array<double, 4> covariance { std::numeric_limits<double>::quiet_NaN(),
        std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
        std::numeric_limits<double>::quiet_NaN() };
    if (std::isfinite(det) && det > 0) {
        covariance[0] = info11 / det;
        covariance[1] = -info01 / det;
        covariance[2] = covariance[1];
        covariance[3] = info00 / det;
    }
    return covariance;
}
//=============================================================================
static UniformLikeResult
makeUniformLikeResult(double nlogL, const std::array<double, 4>& covariance)
{
    double* nlogPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    nlogPtr[0] = nlogL;
    double* covPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 4));
    for (indexType k = 0; k < 4; ++k) {
        covPtr[k] = covariance[k];
    }
    UniformLikeResult result;
    result.negativeLogLikelihood = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), nlogPtr);
    result.covariance = ArrayOf(NLS_DOUBLE, Dimensions(2, 2), covPtr);
    return result;
}
//=============================================================================
UniformLikeResult
UniformLike(const ArrayOfVector& args)
{
    ArrayOf params = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"PARAMS"));
    if (!params.isVector() || params.elementCount() != 2) {
        Error("Nelson:statistics:invalidParams", "PARAMS must be a %s vector.", "two-element");
    }
    const double* pp = static_cast<const double*>(params.dataPointer());
    ArrayOf x = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    if (x.isEmpty(false)) {
        Error("Nelson:validators:mustBeNonempty", "%s must be nonempty.", "X");
    }
    ArrayOf censoring;
    ArrayOf frequency;
    prepareCensoringFrequency(args, x.dimensions(), censoring, frequency);
    std::array<double, 4> nanCov { std::numeric_limits<double>::quiet_NaN(),
        std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
        std::numeric_limits<double>::quiet_NaN() };
    indexType count = x.elementCount();
    ArrayOf flatX = x;
    ArrayOf flatC = censoring;
    ArrayOf flatF = frequency;
    Dimensions flatDims(count, 1);
    flatX.reshape(flatDims);
    flatC.reshape(flatDims);
    flatF.reshape(flatDims);
    const double* xp = static_cast<const double*>(flatX.dataPointer());
    const double* cp = static_cast<const double*>(flatC.dataPointer());
    const double* fp = static_cast<const double*>(flatF.dataPointer());
    double nlogL = uniformNegativeLogLikelihood(pp[0], pp[1], xp, cp, fp, count);
    if (std::isnan(nlogL) || std::isinf(nlogL)) {
        return makeUniformLikeResult(nlogL, nanCov);
    }
    return makeUniformLikeResult(nlogL, uniformCovariance(pp[0], pp[1], xp, cp, fp, count));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
