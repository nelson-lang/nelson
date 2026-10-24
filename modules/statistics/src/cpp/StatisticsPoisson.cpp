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
isNonnegativeInteger(double value)
{
    return std::isfinite(value) && value >= 0 && std::floor(value) == value;
}
//=============================================================================
static bool
validLambda(double lambda)
{
    return std::isfinite(lambda) && lambda >= 0;
}
//=============================================================================
static double
poissonPdfValue(double x, double lambda)
{
    if (std::isnan(x) || !validLambda(lambda)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (!isNonnegativeInteger(x)) {
        return 0;
    }
    if (lambda == 0) {
        return x == 0 ? 1 : 0;
    }
    return std::exp(-lambda + x * std::log(lambda) - std::lgamma(x + 1.0));
}
//=============================================================================
static double
poissonCdfValue(double x, double lambda)
{
    if (std::isnan(x) || !validLambda(lambda)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    double k = std::floor(x);
    if (k < 0) {
        return 0;
    }
    if (std::isinf(k)) {
        return 1;
    }
    if (lambda == 0) {
        return 1;
    }
    return ChiSquareUpper(2.0 * lambda, 2.0 * (k + 1.0));
}
//=============================================================================
static double
poissonInvValue(double y, double lambda)
{
    if (std::isnan(y) || y < 0 || y > 1 || !validLambda(lambda)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (y == 0 || lambda == 0) {
        return 0;
    }
    if (y == 1) {
        return std::numeric_limits<double>::infinity();
    }
    double hi = std::max(1.0, std::ceil(lambda));
    while (poissonCdfValue(hi, lambda) < y && hi < std::numeric_limits<double>::max() / 2.0) {
        hi = 2.0 * hi + 1.0;
    }
    double lo = 0;
    while (lo < hi) {
        double mid = std::floor(0.5 * (lo + hi));
        if (poissonCdfValue(mid, lambda) >= y) {
            hi = mid;
        } else {
            lo = mid + 1.0;
        }
    }
    return lo;
}
//=============================================================================
static ArrayOf
binaryPoissonFunction(
    const ArrayOfVector& args, const std::wstring& firstMessage, int mode, bool upper)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf first = asDoubleArray(args[0], firstMessage);
    ArrayOf lambda = asDoubleArray(args[1],
        resolveMessageTemplate(L"Nelson:statistics:lambdaNonnegativeNumericArray",
            L"LAMBDA must be a nonnegative numeric array."));
    Dimensions dims = commonDimensions(first, lambda);
    indexType count = dims.elementCount();
    void* out = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double a = valueAt(first, k);
        double l = valueAt(lambda, k);
        double value;
        if (mode == 0) {
            value = poissonPdfValue(a, l);
        } else if (mode == 1) {
            value = poissonCdfValue(a, l);
            value = upper && !std::isnan(value) ? std::max(0.0, 1.0 - value) : value;
        } else {
            value = poissonInvValue(a, l);
        }
        setFloatingValue(out, outputType, k, value);
    }
    return ArrayOf(outputType, dims, out);
}
//=============================================================================
ArrayOf
PoissonPdf(const ArrayOfVector& args)
{
    return binaryPoissonFunction(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        0, false);
}
//=============================================================================
ArrayOf
PoissonCdf(const ArrayOfVector& args)
{
    bool upper = false;
    ArrayOfVector numericArgs = args;
    if (!numericArgs.empty() && isTextScalar(numericArgs.back())) {
        upper = matchValue(numericArgs.back(), { L"upper" },
                    formatI18nMessage(
                        resolveMessageTemplate(L"Nelson:statistics:invalidDistributionOption",
                            L"Invalid %s distribution option."),
                        L"Poisson"))
            == L"upper";
        numericArgs.pop_back();
    }
    if (numericArgs.size() < 2) {
        Error("Nelson:notEnoughInputs", "Not enough input arguments.");
    }
    if (numericArgs.size() > 2) {
        Error("Nelson:tooManyInputs", "Too many input arguments.");
    }
    return binaryPoissonFunction(numericArgs,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        1, upper);
}
//=============================================================================
ArrayOf
PoissonInv(const ArrayOfVector& args)
{
    return binaryPoissonFunction(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"Y"),
        2, false);
}
//=============================================================================
ArrayOf
PoissonRnd(const ArrayOfVector& args)
{
    ArrayOfVector typeArgs;
    typeArgs << args[0];
    NelsonType outputType = floatingOutputType(typeArgs);
    ArrayOf lambda = asDoubleArray(args[0],
        resolveMessageTemplate(L"Nelson:statistics:lambdaNonnegativeNumericArray",
            L"LAMBDA must be a nonnegative numeric array."));
    Dimensions dims = args.size() > 1 ? dimensionsFromSizeArgs(args, 1) : lambda.dimensions();
    if (args.size() > 1) {
        validateRequestedDimensions(lambda, dims);
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
        double value = poissonInvValue(getFloatingValue(ptr, outputType, k), valueAt(lambda, k));
        setFloatingValue(ptr, outputType, k, value);
    }
    return r;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
