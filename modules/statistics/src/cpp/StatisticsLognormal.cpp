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
#include "Error.hpp"
#include "ErrorID.hpp"
#include "RandNormal.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
static constexpr double INV_SQRT2 = 0.70710678118654752440084436210484903928;
static constexpr double SQRT2PI = 2.50662827463100050241576528481104525301;
//=============================================================================
static bool
isTextScalar(const ArrayOf& value)
{
    return value.isRowVectorCharacterArray() || value.isScalarStringArray();
}
//=============================================================================
static std::wstring
lowerText(const ArrayOf& value)
{
    std::wstring text = value.asWideString();
    std::transform(text.begin(), text.end(), text.begin(), [](wchar_t c) { return towlower(c); });
    return text;
}
//=============================================================================
static std::wstring
matchValue(
    const ArrayOf& value, const std::vector<std::wstring>& names, const std::wstring& message)
{
    if (!isTextScalar(value)) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    std::wstring text = lowerText(value);
    std::vector<std::wstring> matches;
    for (const auto& name : names) {
        if (name.rfind(text, 0) == 0) {
            matches.push_back(name);
        }
    }
    if (matches.size() != 1) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    return matches[0];
}
//=============================================================================
static ArrayOf
asDoubleArray(const ArrayOf& value, const std::wstring& message)
{
    if (!value.isNumeric() || value.isSparse() || value.isComplex()) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    ArrayOf copy = value;
    copy.promoteType(NLS_DOUBLE);
    return copy;
}
//=============================================================================
static NelsonType
floatingOutputType(const ArrayOfVector& values)
{
    for (const auto& value : values) {
        if (value.isSingleClass()) {
            return NLS_SINGLE;
        }
    }
    return NLS_DOUBLE;
}
//=============================================================================
static void
setFloatingValue(void* ptr, NelsonType outputType, indexType index, double value)
{
    if (outputType == NLS_SINGLE) {
        static_cast<single*>(ptr)[index] = static_cast<single>(value);
    } else {
        static_cast<double*>(ptr)[index] = value;
    }
}
//=============================================================================
static double
getFloatingValue(const void* ptr, NelsonType outputType, indexType index)
{
    if (outputType == NLS_SINGLE) {
        return static_cast<const single*>(ptr)[index];
    }
    return static_cast<const double*>(ptr)[index];
}
//=============================================================================
static Dimensions
commonDimensions(const ArrayOf& a, const ArrayOf& b, const ArrayOf& c)
{
    Dimensions dims(1, 1);
    if (!a.isScalar()) {
        dims = a.dimensions();
    } else if (!b.isScalar()) {
        dims = b.dimensions();
    } else if (!c.isScalar()) {
        dims = c.dimensions();
    }
    if ((!a.isScalar() && !a.dimensions().equals(dims))
        || (!b.isScalar() && !b.dimensions().equals(dims))
        || (!c.isScalar() && !c.dimensions().equals(dims))) {
        Error("Nelson:dimagree:sizeMismatchInputArguments", "Size mismatch on input arguments");
    }
    return dims;
}
//=============================================================================
static double
valueAt(const ArrayOf& value, indexType k)
{
    const double* ptr = static_cast<const double*>(value.dataPointer());
    return ptr[value.isScalar() ? 0 : k];
}
//=============================================================================
static double
scalarDouble(const ArrayOf& value, const std::wstring& message)
{
    ArrayOf copy = asDoubleArray(value, message);
    return copy.asDoubleScalar(false, false);
}
//=============================================================================
static ArrayOf
defaultParameter(
    const ArrayOfVector& args, sizeType index, double defaultValue, const std::wstring& msg)
{
    return args.size() > index && !args[index].isEmpty(false) ? asDoubleArray(args[index], msg)
                                                              : ArrayOf::doubleScalar(defaultValue);
}
//=============================================================================
static double
pdfValue(double x, double mu, double sigma)
{
    if (std::isnan(x) || std::isnan(mu) || std::isnan(sigma) || sigma <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x <= 0) {
        return 0;
    }
    double z = (std::log(x) - mu) / sigma;
    return std::exp(-0.5 * z * z) / (x * sigma * SQRT2PI);
}
//=============================================================================
static double
cdfValue(double x, double mu, double sigma, bool upper)
{
    if (std::isnan(x) || std::isnan(mu) || std::isnan(sigma) || sigma <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x <= 0) {
        return upper ? 1 : 0;
    }
    double z = (std::log(x) - mu) / sigma;
    return upper ? 0.5 * std::erfc(z * INV_SQRT2) : NormalCdf01(z);
}
//=============================================================================
static double
invValue(double p, double mu, double sigma)
{
    if (std::isnan(p) || std::isnan(mu) || std::isnan(sigma) || p < 0 || p > 1 || sigma <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    return std::exp(mu + sigma * NormalInv01(p));
}
//=============================================================================
static indexType
sizeValue(double value)
{
    if (!std::isfinite(value) || std::floor(value) != value) {
        Error("Nelson:statistics:invalidSize", "Size must contain integer values.");
    }
    return value < 0 ? 0 : static_cast<indexType>(value);
}
//=============================================================================
static Dimensions
dimensionsFromSizeArgs(const ArrayOfVector& args, sizeType first)
{
    if (args.size() == first) {
        return Dimensions(1, 1);
    }
    if (args.size() == first + 1 && args[first].isRowVector()) {
        ArrayOf dimsArg = asDoubleArray(args[first],
            formatI18nMessage(Nelson::resolveMessageTemplate(
                                  L"Nelson:validators:mustBeNumeric", L"%s must be numeric."),
                L"Size"));
        const double* ptr = static_cast<const double*>(dimsArg.dataPointer());
        Dimensions dims;
        for (indexType k = 0; k < dimsArg.elementCount(); ++k) {
            dims[k] = sizeValue(ptr[k]);
        }
        if (dims.length() == 1) {
            dims[1] = dims[0];
        }
        dims.simplify();
        return dims;
    }
    Dimensions dims;
    for (sizeType k = first; k < args.size(); ++k) {
        dims[k - first] = sizeValue(scalarDouble(args[k],
            formatI18nMessage(Nelson::resolveMessageTemplate(
                                  L"Nelson:validators:mustBeNumeric", L"%s must be numeric."),
                L"Size")));
    }
    if (dims.length() == 1) {
        dims[1] = dims[0];
    }
    dims.simplify();
    return dims;
}
//=============================================================================
static void
validateRequestedDimensions(const ArrayOf& mu, const ArrayOf& sigma, const Dimensions& dims)
{
    if ((!mu.isScalar() && !mu.dimensions().equals(dims))
        || (!sigma.isScalar() && !sigma.dimensions().equals(dims))) {
        Error("Nelson:dimagree:sizeMismatchInputArguments", "Size mismatch on input arguments");
    }
}
//=============================================================================
static void
parseCdfTail(ArrayOfVector args, ArrayOfVector& numericArgs, bool& upper)
{
    upper = false;
    numericArgs = args;
    if (!numericArgs.empty() && isTextScalar(numericArgs.back())) {
        upper = matchValue(numericArgs.back(), { L"upper" },
                    formatI18nMessage(
                        resolveMessageTemplate(L"Nelson:statistics:invalidDistributionOption",
                            L"Invalid %s distribution option."),
                        L"lognormal"))
            == L"upper";
        numericArgs.pop_back();
    }
}
//=============================================================================
ArrayOf
LognormalPdf(const ArrayOfVector& args)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf x = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    ArrayOf mu = defaultParameter(args, 1, 0,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"MU"));
    ArrayOf sigma = defaultParameter(args, 2, 1,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"SIGMA"));
    Dimensions dims = commonDimensions(x, mu, sigma);
    indexType count = dims.elementCount();
    void* out = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        setFloatingValue(
            out, outputType, k, pdfValue(valueAt(x, k), valueAt(mu, k), valueAt(sigma, k)));
    }
    return ArrayOf(outputType, dims, out);
}
//=============================================================================
NormalBounds
LognormalCdf(const ArrayOfVector& args)
{
    ArrayOfVector numericArgs;
    bool upper;
    parseCdfTail(args, numericArgs, upper);
    NelsonType outputType = floatingOutputType(numericArgs);
    ArrayOf x = asDoubleArray(numericArgs[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    ArrayOf mu = defaultParameter(numericArgs, 1, 0,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"MU"));
    ArrayOf sigma = defaultParameter(numericArgs, 2, 1,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"SIGMA"));
    Dimensions dims = commonDimensions(x, mu, sigma);
    indexType count = dims.elementCount();
    void* out = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        setFloatingValue(
            out, outputType, k, cdfValue(valueAt(x, k), valueAt(mu, k), valueAt(sigma, k), upper));
    }
    NormalBounds result;
    result.value = ArrayOf(outputType, dims, out);
    if (numericArgs.size() > 3) {
        ArrayOf logx = x;
        double* ptr = static_cast<double*>(logx.readWriteDataPointer());
        for (indexType k = 0; k < x.elementCount(); ++k) {
            ptr[k] = ptr[k] <= 0 ? -std::numeric_limits<double>::infinity() : std::log(ptr[k]);
        }
        ArrayOfVector normalArgs;
        normalArgs << logx << mu << sigma << numericArgs[3];
        if (numericArgs.size() > 4) {
            normalArgs << numericArgs[4];
        }
        NormalBounds nb = NormalCdf(normalArgs);
        result.lower = nb.lower;
        result.upper = nb.upper;
    }
    return result;
}
//=============================================================================
NormalBounds
LognormalInv(const ArrayOfVector& args)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf p = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"P"));
    ArrayOf mu = defaultParameter(args, 1, 0,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"MU"));
    ArrayOf sigma = defaultParameter(args, 2, 1,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"SIGMA"));
    Dimensions dims = commonDimensions(p, mu, sigma);
    indexType count = dims.elementCount();
    void* out = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        setFloatingValue(
            out, outputType, k, invValue(valueAt(p, k), valueAt(mu, k), valueAt(sigma, k)));
    }
    NormalBounds result;
    result.value = ArrayOf(outputType, dims, out);
    if (args.size() > 3) {
        ArrayOfVector normalArgs;
        normalArgs << p << mu << sigma << args[3];
        if (args.size() > 4) {
            normalArgs << args[4];
        }
        NormalBounds nb = NormalInv(normalArgs);
        void* lo = ArrayOf::allocateArrayOf(outputType, count);
        void* hi = ArrayOf::allocateArrayOf(outputType, count);
        for (indexType k = 0; k < count; ++k) {
            setFloatingValue(lo, outputType, k,
                std::exp(getFloatingValue(nb.lower.dataPointer(), outputType, k)));
            setFloatingValue(hi, outputType, k,
                std::exp(getFloatingValue(nb.upper.dataPointer(), outputType, k)));
        }
        result.lower = ArrayOf(outputType, dims, lo);
        result.upper = ArrayOf(outputType, dims, hi);
    }
    return result;
}
//=============================================================================
ArrayOf
LognormalRnd(const ArrayOfVector& args)
{
    ArrayOfVector typeArgs;
    typeArgs << args[0];
    typeArgs << args[1];
    NelsonType outputType = floatingOutputType(typeArgs);
    ArrayOf mu = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"MU"));
    ArrayOf sigma = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"SIGMA"));
    Dimensions dims
        = args.size() > 2 ? dimensionsFromSizeArgs(args, 2) : commonDimensions(mu, sigma, sigma);
    if (args.size() > 2) {
        validateRequestedDimensions(mu, sigma, dims);
    }
    if (dims.isEmpty(false)) {
        ArrayOf res = ArrayOf::empty(dims);
        res.promoteType(outputType);
        return res;
    }
    ArrayOf z = RandNormal(dims, outputType);
    void* ptr = z.readWriteDataPointer();
    indexType count = dims.elementCount();
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double s = valueAt(sigma, k);
        double value = s < 0 || std::isnan(s)
            ? std::numeric_limits<double>::quiet_NaN()
            : std::exp(valueAt(mu, k) + s * getFloatingValue(ptr, outputType, k));
        setFloatingValue(ptr, outputType, k, value);
    }
    return z;
}
//=============================================================================
LognormalStatResult
LognormalStat(const ArrayOfVector& args)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf mu = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"MU"));
    ArrayOf sigma = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"SIGMA"));
    if (mu.isEmpty(true) || sigma.isEmpty(true)) {
        Dimensions dims = mu.isEmpty(true) ? mu.dimensions() : sigma.dimensions();
        ArrayOf mean = ArrayOf::empty(dims);
        ArrayOf variance = ArrayOf::empty(dims);
        mean.promoteType(outputType);
        variance.promoteType(outputType);
        LognormalStatResult result;
        result.mean = mean;
        result.variance = variance;
        return result;
    }
    Dimensions dims = commonDimensions(mu, sigma, sigma);
    indexType count = dims.elementCount();
    void* meanPtr = ArrayOf::allocateArrayOf(outputType, count);
    void* varPtr = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double m = valueAt(mu, k);
        double s = valueAt(sigma, k);
        double mean = s < 0 || std::isnan(s) ? std::numeric_limits<double>::quiet_NaN()
                                             : std::exp(m + 0.5 * s * s);
        double variance = std::isnan(mean) ? mean : std::exp(2 * m + s * s) * std::expm1(s * s);
        setFloatingValue(meanPtr, outputType, k, mean);
        setFloatingValue(varPtr, outputType, k, variance);
    }
    LognormalStatResult result;
    result.mean = ArrayOf(outputType, dims, meanPtr);
    result.variance = ArrayOf(outputType, dims, varPtr);
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
