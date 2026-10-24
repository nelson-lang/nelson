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
#include "Rand.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include <array>
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
static constexpr double EULER_GAMMA = 0.577215664901532860606512090082402431;
static constexpr double PI_VALUE = 3.141592653589793238462643383279502884;
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
commonDimensions(const ArrayOfVector& values)
{
    Dimensions dims(1, 1);
    for (const auto& value : values) {
        if (!value.isScalar()) {
            dims = value.dimensions();
            break;
        }
    }
    for (const auto& value : values) {
        if (!value.isScalar() && !value.dimensions().equals(dims)) {
            Error("Nelson:dimagree:sizeMismatchInputArguments", "Size mismatch on input arguments");
        }
    }
    return dims;
}
//=============================================================================
static double
valueAt(const ArrayOf& value, indexType index)
{
    const double* ptr = static_cast<const double*>(value.dataPointer());
    return ptr[value.isScalar() ? 0 : index];
}
//=============================================================================
static double
scalarDouble(const ArrayOf& value, const std::wstring& message)
{
    ArrayOf copy = asDoubleArray(value, message);
    return copy.asDoubleScalar(false, false);
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
        ArrayOfVector values;
        values << args[0] << args[1];
        return commonDimensions(values);
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
static bool
validScale(double sigma)
{
    return !std::isnan(sigma) && sigma > 0;
}
//=============================================================================
static double
pdfValue(double x, double mu, double sigma)
{
    if (std::isnan(x) || std::isnan(mu) || !validScale(sigma)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (std::isinf(x)) {
        return 0;
    }
    double z = (x - mu) / sigma;
    double ez = std::exp(z);
    return std::exp(z - ez) / sigma;
}
//=============================================================================
static double
cdfValue(double x, double mu, double sigma, bool upper)
{
    if (std::isnan(x) || std::isnan(mu) || !validScale(sigma)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x == -std::numeric_limits<double>::infinity()) {
        return upper ? 1 : 0;
    }
    if (x == std::numeric_limits<double>::infinity()) {
        return upper ? 0 : 1;
    }
    double ez = std::exp((x - mu) / sigma);
    return upper ? std::exp(-ez) : -std::expm1(-ez);
}
//=============================================================================
static double
invValue(double p, double mu, double sigma)
{
    if (std::isnan(p) || std::isnan(mu) || p < 0 || p > 1 || !validScale(sigma)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (p == 0) {
        return -std::numeric_limits<double>::infinity();
    }
    if (p == 1) {
        return std::numeric_limits<double>::infinity();
    }
    return mu + sigma * std::log(-std::log1p(-p));
}
//=============================================================================
static ArrayOfVector
withDefaultParameters(ArrayOfVector args)
{
    if (args.size() == 1) {
        args << ArrayOf::doubleScalar(0) << ArrayOf::doubleScalar(1);
    } else if (args.size() == 2) {
        args << ArrayOf::doubleScalar(1);
    }
    return args;
}
//=============================================================================
static ArrayOf
applyTernary(const ArrayOfVector& args, const std::wstring& firstMessage, int mode, bool upper)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf first = asDoubleArray(args[0], firstMessage);
    ArrayOf mu = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"MU"));
    ArrayOf sigma = asDoubleArray(args[2],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"SIGMA"));
    ArrayOfVector values;
    values << first << mu << sigma;
    Dimensions dims = commonDimensions(values);
    indexType count = dims.elementCount();
    void* out = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double a = valueAt(first, k);
        double mv = valueAt(mu, k);
        double sv = valueAt(sigma, k);
        double value = mode == 0 ? pdfValue(a, mv, sv)
                                 : (mode == 1 ? cdfValue(a, mv, sv, upper) : invValue(a, mv, sv));
        setFloatingValue(out, outputType, k, value);
    }
    return ArrayOf(outputType, dims, out);
}
//=============================================================================
ArrayOf
ExtremeValuePdf(const ArrayOfVector& args)
{
    return applyTernary(withDefaultParameters(args),
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        0, false);
}
//=============================================================================
ArrayOf
ExtremeValueCdf(const ArrayOfVector& args)
{
    bool upper = false;
    ArrayOfVector numericArgs = args;
    if (!numericArgs.empty() && isTextScalar(numericArgs.back())) {
        upper = matchValue(numericArgs.back(), { L"upper" },
                    formatI18nMessage(
                        resolveMessageTemplate(L"Nelson:statistics:invalidDistributionOption",
                            L"Invalid %s distribution option."),
                        L"extreme value"))
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
ExtremeValueInv(const ArrayOfVector& args)
{
    return applyTernary(withDefaultParameters(args),
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"P"),
        2, false);
}
//=============================================================================
ArrayOf
ExtremeValueRnd(const ArrayOfVector& args)
{
    ArrayOfVector typeArgs;
    typeArgs << args[0] << args[1];
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
        = args.size() > 2 ? dimensionsFromSizeArgs(args, 2) : commonDimensions(typeArgs);
    if (args.size() > 2) {
        validateRequestedDimensions(mu, sigma, dims);
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
            = invValue(getFloatingValue(ptr, outputType, k), valueAt(mu, k), valueAt(sigma, k));
        setFloatingValue(ptr, outputType, k, value);
    }
    return r;
}
//=============================================================================
ExtremeValueStatResult
ExtremeValueStat(const ArrayOfVector& args)
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
    ArrayOfVector values;
    values << mu << sigma;
    Dimensions dims = commonDimensions(values);
    indexType count = dims.elementCount();
    void* meanPtr = ArrayOf::allocateArrayOf(outputType, count);
    void* varPtr = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double mv = valueAt(mu, k);
        double sv = valueAt(sigma, k);
        double mean = std::isnan(mv) || !validScale(sv) ? std::numeric_limits<double>::quiet_NaN()
                                                        : mv - EULER_GAMMA * sv;
        double variance = std::isnan(mean) ? mean : (PI_VALUE * PI_VALUE / 6.0) * sv * sv;
        setFloatingValue(meanPtr, outputType, k, mean);
        setFloatingValue(varPtr, outputType, k, variance);
    }
    ExtremeValueStatResult result;
    result.mean = ArrayOf(outputType, dims, meanPtr);
    result.variance = ArrayOf(outputType, dims, varPtr);
    return result;
}
//=============================================================================
struct ExtremeValueColumnEstimate
{
    double mu;
    double sigma;
    double negativeLogLikelihood;
    std::array<double, 4> covariance;
};
//=============================================================================
struct ExtremeValueFitOptions
{
    int maxIter { 120 };
    double tolX { 1e-10 };
};
//=============================================================================
static ArrayOf
asOptionalDoubleArray(
    const ArrayOf& value, const Dimensions& dims, double defaultValue, const std::wstring& message)
{
    if (value.isEmpty(false)) {
        double* ptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dims.elementCount()));
        std::fill(ptr, ptr + dims.elementCount(), defaultValue);
        return ArrayOf(NLS_DOUBLE, dims, ptr);
    }
    ArrayOf copy = asDoubleArray(value, message);
    if (!copy.isScalar() && !copy.dimensions().equals(dims)) {
        Error("Nelson:dimagree:sizeMismatchInputArguments", "Size mismatch on input arguments");
    }
    if (copy.isScalar() && dims.elementCount() != 1) {
        double scalarValue = static_cast<const double*>(copy.dataPointer())[0];
        double* ptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dims.elementCount()));
        std::fill(ptr, ptr + dims.elementCount(), scalarValue);
        return ArrayOf(NLS_DOUBLE, dims, ptr);
    }
    return copy;
}
//=============================================================================
static void
validateCensoring(const ArrayOf& censoring)
{
    const double* ptr = static_cast<const double*>(censoring.dataPointer());
    indexType count = censoring.elementCount();
    for (indexType k = 0; k < count; ++k) {
        double v = ptr[k];
        if (std::isnan(v) || (v != 0.0 && v != 1.0)) {
            Error("Nelson:statistics:invalidCensoring", "CENSORING must contain 0 or 1 values.");
        }
    }
}
//=============================================================================
static void
validateFrequency(const ArrayOf& frequency)
{
    const double* ptr = static_cast<const double*>(frequency.dataPointer());
    indexType count = frequency.elementCount();
    for (indexType k = 0; k < count; ++k) {
        double v = ptr[k];
        if (!std::isfinite(v) || v < 0) {
            Error("Nelson:statistics:invalidFreq", "%s must contain nonnegative finite values.",
                "FREQ");
        }
    }
}
//=============================================================================
static double
normalInvApprox(double p)
{
    static constexpr double a1 = -3.969683028665376e+01;
    static constexpr double a2 = 2.209460984245205e+02;
    static constexpr double a3 = -2.759285104469687e+02;
    static constexpr double a4 = 1.383577518672690e+02;
    static constexpr double a5 = -3.066479806614716e+01;
    static constexpr double a6 = 2.506628277459239e+00;
    static constexpr double b1 = -5.447609879822406e+01;
    static constexpr double b2 = 1.615858368580409e+02;
    static constexpr double b3 = -1.556989798598866e+02;
    static constexpr double b4 = 6.680131188771972e+01;
    static constexpr double b5 = -1.328068155288572e+01;
    static constexpr double c1 = -7.784894002430293e-03;
    static constexpr double c2 = -3.223964580411365e-01;
    static constexpr double c3 = -2.400758277161838e+00;
    static constexpr double c4 = -2.549732539343734e+00;
    static constexpr double c5 = 4.374664141464968e+00;
    static constexpr double c6 = 2.938163982698783e+00;
    static constexpr double d1 = 7.784695709041462e-03;
    static constexpr double d2 = 3.224671290700398e-01;
    static constexpr double d3 = 2.445134137142996e+00;
    static constexpr double d4 = 3.754408661907416e+00;
    if (p <= 0) {
        return -std::numeric_limits<double>::infinity();
    }
    if (p >= 1) {
        return std::numeric_limits<double>::infinity();
    }
    if (p < 0.02425) {
        double q = std::sqrt(-2.0 * std::log(p));
        return (((((c1 * q + c2) * q + c3) * q + c4) * q + c5) * q + c6)
            / ((((d1 * q + d2) * q + d3) * q + d4) * q + 1.0);
    }
    if (p > 0.97575) {
        double q = std::sqrt(-2.0 * std::log1p(-p));
        return -(((((c1 * q + c2) * q + c3) * q + c4) * q + c5) * q + c6)
            / ((((d1 * q + d2) * q + d3) * q + d4) * q + 1.0);
    }
    double q = p - 0.5;
    double r = q * q;
    return (((((a1 * r + a2) * r + a3) * r + a4) * r + a5) * r + a6) * q
        / (((((b1 * r + b2) * r + b3) * r + b4) * r + b5) * r + 1.0);
}
//=============================================================================
static double
fitAlphaValue(const ArrayOfVector& args)
{
    if (args.size() < 2 || args[1].isEmpty(false)) {
        return 0.05;
    }
    double alpha = scalarDouble(args[1],
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:validators:mustBeNumeric", L"%s must be numeric."),
            L"ALPHA"));
    if (!(alpha >= 0 && alpha <= 1)) {
        Error("Nelson:statistics:alphaOutOfRangeClosed", "%s must be a scalar in the range [0,1].",
            "ALPHA");
    }
    return alpha;
}
//=============================================================================
static double
scalarOptionField(const ArrayOf& options, const std::string& field, double defaultValue)
{
    if (!options.isField(field)) {
        return defaultValue;
    }
    ArrayOf value = options.field(field);
    if (value.isEmpty(false)) {
        return defaultValue;
    }
    if (!value.isNumeric() || !value.isScalar() || value.isComplex()) {
        Error("Nelson:statistics:optionsFieldsNotNumeric",
            "OPTIONS fields must be numeric scalar values.");
    }
    return value.asDoubleScalar(false, false);
}
//=============================================================================
static ExtremeValueFitOptions
fitOptionsValue(const ArrayOfVector& args)
{
    ExtremeValueFitOptions fitOptions;
    if (args.size() > 4 && !args[4].isEmpty(false) && !args[4].isStruct()) {
        Error("Nelson:statistics:invalidOptions", "%s must be a scalar structure.", "OPTIONS");
    }
    if (args.size() > 4 && !args[4].isEmpty(false)) {
        double maxIter = scalarOptionField(args[4], "MaxIter", fitOptions.maxIter);
        double tolX = scalarOptionField(args[4], "TolX", fitOptions.tolX);
        if (!std::isfinite(maxIter) || maxIter <= 0 || std::floor(maxIter) != maxIter) {
            Error("Nelson:statistics:optionsMaxIterPositiveIntegerScalar",
                "%s must be a positive integer scalar.", "OPTIONS MaxIter");
        }
        if (!std::isfinite(tolX) || tolX <= 0) {
            Error("Nelson:statistics:optionsTolXPositiveScalar", "%s must be a positive scalar.",
                "OPTIONS TolX");
        }
        fitOptions.maxIter = static_cast<int>(maxIter);
        fitOptions.tolX = tolX;
    }
    return fitOptions;
}
//=============================================================================
static double
profileLogA(const double* x, const double* w, indexType rows, indexType column, double sigma)
{
    indexType offset = column * rows;
    double maxValue = -std::numeric_limits<double>::infinity();
    for (indexType r = 0; r < rows; ++r) {
        double weight = w[offset + r];
        if (weight > 0) {
            maxValue = std::max(maxValue, x[offset + r] / sigma);
        }
    }
    if (!std::isfinite(maxValue)) {
        return maxValue;
    }
    double sum = 0;
    for (indexType r = 0; r < rows; ++r) {
        double weight = w[offset + r];
        if (weight > 0) {
            sum += weight * std::exp(x[offset + r] / sigma - maxValue);
        }
    }
    return maxValue + std::log(sum);
}
//=============================================================================
static double
profileNegativeLogLikelihood(double logSigma, const double* x, const double* censoring,
    const double* frequency, indexType rows, indexType column, double uncensoredWeight,
    double uncensoredSum)
{
    double sigma = std::exp(logSigma);
    double logA = profileLogA(x, frequency, rows, column, sigma);
    if (!std::isfinite(logA)) {
        return std::numeric_limits<double>::infinity();
    }
    double mu = sigma * (logA - std::log(uncensoredWeight));
    double logLikelihood = -uncensoredWeight * logSigma
        + (uncensoredSum - uncensoredWeight * mu) / sigma - uncensoredWeight;
    return -logLikelihood;
}
//=============================================================================
static std::array<double, 4>
extremeValueCovariance(double mu, double sigma, const double* x, const double* censoring,
    const double* frequency, indexType rows, indexType column)
{
    indexType offset = column * rows;
    double a = 0;
    double b = 0;
    double c = 0;
    double scoreSigmaNumerator = 0;
    for (indexType r = 0; r < rows; ++r) {
        double weight = frequency[offset + r];
        double d = 1.0 - censoring[offset + r];
        double y = (x[offset + r] - mu) / sigma;
        double ey = std::exp(y);
        a += weight * ey;
        b += weight * (ey * y + ey - d);
        c += weight * (ey * y * (y + 2.0) - d * y);
        scoreSigmaNumerator += weight * (-d - d * y + ey * y);
    }
    double info00 = a / (sigma * sigma);
    double info01 = b / (sigma * sigma);
    double info11 = (c - scoreSigmaNumerator) / (sigma * sigma);
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
static ExtremeValueColumnEstimate
fitExtremeValueColumn(const double* x, const double* censoring, const double* frequency,
    indexType rows, indexType column, const ExtremeValueFitOptions& options)
{
    indexType offset = column * rows;
    double uncensoredWeight = 0;
    double uncensoredSum = 0;
    double weightedSum = 0;
    double totalWeight = 0;
    bool hasNaN = false;
    for (indexType r = 0; r < rows; ++r) {
        double xv = x[offset + r];
        double weight = frequency[offset + r];
        double cens = censoring[offset + r];
        if (std::isnan(xv)) {
            hasNaN = true;
        }
        totalWeight += weight;
        weightedSum += weight * xv;
        if (cens == 0 && weight > 0) {
            uncensoredWeight += weight;
            uncensoredSum += weight * xv;
        }
    }
    ExtremeValueColumnEstimate estimate;
    estimate.mu = std::numeric_limits<double>::quiet_NaN();
    estimate.sigma = estimate.mu;
    estimate.negativeLogLikelihood = estimate.mu;
    estimate.covariance = { estimate.mu, estimate.mu, estimate.mu, estimate.mu };
    if (hasNaN || totalWeight <= 0 || uncensoredWeight <= 0) {
        return estimate;
    }
    double mean = weightedSum / totalWeight;
    double secondMoment = 0;
    for (indexType r = 0; r < rows; ++r) {
        double delta = x[offset + r] - mean;
        secondMoment += frequency[offset + r] * delta * delta;
    }
    double variance = secondMoment / std::max(totalWeight, 1.0);
    double sigma0 = variance > 0 ? std::sqrt(6.0 * variance) / PI_VALUE : 1.0;
    double center = std::log(std::max(sigma0, std::numeric_limits<double>::min()));
    double lo = center - 8.0;
    double hi = center + 8.0;
    double gr = (std::sqrt(5.0) - 1.0) / 2.0;
    double x1 = hi - gr * (hi - lo);
    double x2 = lo + gr * (hi - lo);
    double f1 = profileNegativeLogLikelihood(
        x1, x, censoring, frequency, rows, column, uncensoredWeight, uncensoredSum);
    double f2 = profileNegativeLogLikelihood(
        x2, x, censoring, frequency, rows, column, uncensoredWeight, uncensoredSum);
    for (int iter = 0;
        iter < options.maxIter && std::abs(hi - lo) > options.tolX * (1.0 + std::abs(center));
        ++iter) {
        if (f1 > f2) {
            lo = x1;
            x1 = x2;
            f1 = f2;
            x2 = lo + gr * (hi - lo);
            f2 = profileNegativeLogLikelihood(
                x2, x, censoring, frequency, rows, column, uncensoredWeight, uncensoredSum);
        } else {
            hi = x2;
            x2 = x1;
            f2 = f1;
            x1 = hi - gr * (hi - lo);
            f1 = profileNegativeLogLikelihood(
                x1, x, censoring, frequency, rows, column, uncensoredWeight, uncensoredSum);
        }
    }
    double logSigma = (lo + hi) / 2.0;
    double sigma = std::exp(logSigma);
    double mu
        = sigma * (profileLogA(x, frequency, rows, column, sigma) - std::log(uncensoredWeight));
    estimate.mu = mu;
    estimate.sigma = sigma;
    estimate.negativeLogLikelihood = profileNegativeLogLikelihood(
        logSigma, x, censoring, frequency, rows, column, uncensoredWeight, uncensoredSum);
    estimate.covariance = extremeValueCovariance(mu, sigma, x, censoring, frequency, rows, column);
    return estimate;
}
//=============================================================================
static void
prepareCensoringFrequency(const ArrayOfVector& args, sizeType firstOptional, const Dimensions& dims,
    ArrayOf& censoring, ArrayOf& frequency)
{
    censoring = args.size() > firstOptional
        ? asOptionalDoubleArray(args[firstOptional], dims, 0.0,
              formatI18nMessage(resolveMessageTemplate(
                                    L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
                  L"CENSORING"))
        : asOptionalDoubleArray(ArrayOf::empty(), dims, 0.0, L"");
    frequency = args.size() > firstOptional + 1
        ? asOptionalDoubleArray(args[firstOptional + 1], dims, 1.0,
              formatI18nMessage(resolveMessageTemplate(
                                    L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
                  L"FREQ"))
        : asOptionalDoubleArray(ArrayOf::empty(), dims, 1.0, L"");
    validateCensoring(censoring);
    validateFrequency(frequency);
}
//=============================================================================
ExtremeValueFitResult
ExtremeValueFit(const ArrayOfVector& args)
{
    ArrayOf x = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    if (x.isEmpty(false) || x.dimensions().length() > 2) {
        Error("Nelson:statistics:xMustBeNonemptyVectorOrMatrix",
            "X must be a nonempty vector or matrix.");
    }
    double alpha = fitAlphaValue(args);
    ExtremeValueFitOptions fitOptions = fitOptionsValue(args);
    Dimensions dims = x.dimensions();
    bool vectorInput = x.isVector();
    indexType rows = vectorInput ? x.elementCount() : dims.rows();
    indexType cols = vectorInput ? 1 : dims.columns();
    Dimensions dataDims = vectorInput ? Dimensions(rows, 1) : dims;
    ArrayOf data = x;
    if (vectorInput && dims.rows() == 1) {
        data = x;
        Dimensions reshaped(rows, 1);
        data.reshape(reshaped);
    }
    ArrayOf censoring;
    ArrayOf frequency;
    prepareCensoringFrequency(args, 2, dims, censoring, frequency);
    if (vectorInput && dims.rows() == 1) {
        Dimensions reshaped(rows, 1);
        censoring.reshape(reshaped);
        frequency.reshape(reshaped);
    }
    const double* xp = static_cast<const double*>(data.dataPointer());
    const double* cp = static_cast<const double*>(censoring.dataPointer());
    const double* fp = static_cast<const double*>(frequency.dataPointer());
    double* params = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, cols * 2));
    double* pci = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, cols * 4));
    double z = normalInvApprox(1.0 - alpha / 2.0);
#if WITH_OPENMP
#pragma omp parallel for if (cols > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType c = 0; c < (ompIndexType)cols; ++c) {
        ExtremeValueColumnEstimate estimate
            = fitExtremeValueColumn(xp, cp, fp, rows, c, fitOptions);
        params[c] = estimate.mu;
        params[c + cols] = estimate.sigma;
        double muSe = estimate.covariance[0] >= 0 ? std::sqrt(estimate.covariance[0])
                                                  : std::numeric_limits<double>::quiet_NaN();
        double sigmaLogSe = estimate.covariance[3] >= 0 && estimate.sigma > 0
            ? std::sqrt(estimate.covariance[3]) / estimate.sigma
            : std::numeric_limits<double>::quiet_NaN();
        indexType base = c * 4;
        pci[base] = estimate.mu - z * muSe;
        pci[base + 1] = estimate.mu + z * muSe;
        pci[base + 2] = estimate.sigma * std::exp(-z * sigmaLogSe);
        pci[base + 3] = estimate.sigma * std::exp(z * sigmaLogSe);
    }
    ExtremeValueFitResult result;
    result.parameterHat
        = ArrayOf(NLS_DOUBLE, vectorInput ? Dimensions(1, 2) : Dimensions(cols, 2), params);
    result.parameterCI = ArrayOf(NLS_DOUBLE,
        vectorInput ? Dimensions(2, 2) : Dimensions(std::vector<indexType> { 2, 2, cols }), pci);
    return result;
}
//=============================================================================
static ExtremeValueLikeResult
makeLikeResult(double nlogL, const std::array<double, 4>& covariance)
{
    double* nlogPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    nlogPtr[0] = nlogL;
    double* covPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 4));
    for (indexType k = 0; k < 4; ++k) {
        covPtr[k] = covariance[k];
    }
    ExtremeValueLikeResult result;
    result.negativeLogLikelihood = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), nlogPtr);
    result.covariance = ArrayOf(NLS_DOUBLE, Dimensions(2, 2), covPtr);
    return result;
}
//=============================================================================
ExtremeValueLikeResult
ExtremeValueLike(const ArrayOfVector& args)
{
    ArrayOf params = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"PARAMS"));
    if (!params.isVector() || params.elementCount() != 2) {
        Error("Nelson:statistics:invalidParams", "PARAMS must be a %s vector.", "two-element");
    }
    const double* pp = static_cast<const double*>(params.dataPointer());
    double mu = pp[0];
    double sigma = pp[1];
    ArrayOf x = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    if (x.isEmpty(false)) {
        Error("Nelson:validators:mustBeNonempty", "%s must be nonempty.", "X");
    }
    Dimensions dims = x.dimensions();
    ArrayOf censoring;
    ArrayOf frequency;
    prepareCensoringFrequency(args, 2, dims, censoring, frequency);
    std::array<double, 4> nanCov { std::numeric_limits<double>::quiet_NaN(),
        std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
        std::numeric_limits<double>::quiet_NaN() };
    if (std::isnan(mu) || std::isnan(sigma)) {
        return makeLikeResult(std::numeric_limits<double>::quiet_NaN(), nanCov);
    }
    if (!validScale(sigma)) {
        return makeLikeResult(std::numeric_limits<double>::infinity(), nanCov);
    }
    const double* xp = static_cast<const double*>(x.dataPointer());
    const double* cp = static_cast<const double*>(censoring.dataPointer());
    const double* fp = static_cast<const double*>(frequency.dataPointer());
    indexType count = x.elementCount();
    double logLikelihood = 0;
    bool hasNaN = false;
    for (indexType k = 0; k < count; ++k) {
        double xv = xp[k];
        double cens = cp[censoring.isScalar() ? 0 : k];
        double freq = fp[frequency.isScalar() ? 0 : k];
        if (std::isnan(xv)) {
            hasNaN = true;
            break;
        }
        double y = (xv - mu) / sigma;
        if (cens == 0) {
            logLikelihood += freq * (-std::log(sigma) + y - std::exp(y));
        } else {
            logLikelihood += freq * (-std::exp(y));
        }
    }
    if (hasNaN) {
        return makeLikeResult(std::numeric_limits<double>::quiet_NaN(), nanCov);
    }
    ArrayOf flatX = x;
    Dimensions flatDims(count, 1);
    flatX.reshape(flatDims);
    ArrayOf flatC = censoring;
    flatC.reshape(flatDims);
    ArrayOf flatF = frequency;
    flatF.reshape(flatDims);
    std::array<double, 4> covariance
        = extremeValueCovariance(mu, sigma, static_cast<const double*>(flatX.dataPointer()),
            static_cast<const double*>(flatC.dataPointer()),
            static_cast<const double*>(flatF.dataPointer()), count, 0);
    return makeLikeResult(-logLikelihood, covariance);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
