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
#include <algorithm>
#include <array>
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
static double
scalarDouble(const ArrayOf& value, const std::wstring& message)
{
    ArrayOf copy = asDoubleArray(value, message);
    return copy.asDoubleScalar(false, false);
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
        values << args[0] << args[1] << args[2];
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
validateRequestedDimensions(
    const ArrayOf& k, const ArrayOf& sigma, const ArrayOf& mu, const Dimensions& dims)
{
    if ((!k.isScalar() && !k.dimensions().equals(dims))
        || (!sigma.isScalar() && !sigma.dimensions().equals(dims))
        || (!mu.isScalar() && !mu.dimensions().equals(dims))) {
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
supportLimit(double k, double sigma, double mu)
{
    return mu - sigma / k;
}
//=============================================================================
static double
gevPdfValue(double x, double k, double sigma, double mu)
{
    if (std::isnan(x) || std::isnan(k) || std::isnan(mu) || !validScale(sigma)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (k == 0) {
        double z = (x - mu) / sigma;
        if (std::isinf(z)) {
            return 0;
        }
        return std::exp(-(z + std::exp(-z))) / sigma;
    }
    double t = 1.0 + k * (x - mu) / sigma;
    if (t <= 0) {
        return 0;
    }
    double logt = std::log(t);
    double q = std::exp(-logt / k);
    return std::exp((-1.0 / k - 1.0) * logt - q) / sigma;
}
//=============================================================================
static double
gevCdfValue(double x, double k, double sigma, double mu, bool upper)
{
    if (std::isnan(x) || std::isnan(k) || std::isnan(mu) || !validScale(sigma)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (k == 0) {
        double z = (x - mu) / sigma;
        double cdf = std::exp(-std::exp(-z));
        return upper ? -std::expm1(-std::exp(-z)) : cdf;
    }
    double limit = supportLimit(k, sigma, mu);
    if ((k > 0 && x <= limit) || (k < 0 && x >= limit)) {
        return upper ? (k > 0 ? 1 : 0) : (k > 0 ? 0 : 1);
    }
    double t = 1.0 + k * (x - mu) / sigma;
    double q = std::exp(-std::log(t) / k);
    double cdf = std::exp(-q);
    return upper ? -std::expm1(-q) : cdf;
}
//=============================================================================
static double
gevInvValue(double p, double k, double sigma, double mu)
{
    if (std::isnan(p) || std::isnan(k) || std::isnan(mu) || p < 0 || p > 1 || !validScale(sigma)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (k == 0) {
        if (p == 0) {
            return -std::numeric_limits<double>::infinity();
        }
        if (p == 1) {
            return std::numeric_limits<double>::infinity();
        }
        return mu - sigma * std::log(-std::log(p));
    }
    double limit = supportLimit(k, sigma, mu);
    if (p == 0) {
        return k > 0 ? limit : -std::numeric_limits<double>::infinity();
    }
    if (p == 1) {
        return k > 0 ? std::numeric_limits<double>::infinity() : limit;
    }
    return mu + sigma * (std::pow(-std::log(p), -k) - 1.0) / k;
}
//=============================================================================
static ArrayOf
applyQuaternary(const ArrayOfVector& args, const std::wstring& firstMessage, int mode, bool upper)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf first = asDoubleArray(args[0], firstMessage);
    ArrayOf k = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"K"));
    ArrayOf sigma = asDoubleArray(args[2],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"SIGMA"));
    ArrayOf mu = asDoubleArray(args[3],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"MU"));
    ArrayOfVector values;
    values << first << k << sigma << mu;
    Dimensions dims = commonDimensions(values);
    indexType count = dims.elementCount();
    void* out = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType n = 0; n < (ompIndexType)count; ++n) {
        double firstValue = valueAt(first, n);
        double kv = valueAt(k, n);
        double sv = valueAt(sigma, n);
        double mv = valueAt(mu, n);
        double result = mode == 0 ? gevPdfValue(firstValue, kv, sv, mv)
                                  : (mode == 1 ? gevCdfValue(firstValue, kv, sv, mv, upper)
                                               : gevInvValue(firstValue, kv, sv, mv));
        setFloatingValue(out, outputType, n, result);
    }
    return ArrayOf(outputType, dims, out);
}
//=============================================================================
struct GevColumnEstimate
{
    double k;
    double sigma;
    double mu;
    double negativeLogLikelihood;
    std::array<double, 9> covariance;
};
//=============================================================================
struct GevFitOptions
{
    int maxIter { 160 };
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
    for (indexType n = 0; n < count; ++n) {
        double value = ptr[n];
        if (std::isnan(value) || (value != 0.0 && value != 1.0)) {
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
    for (indexType n = 0; n < count; ++n) {
        double value = ptr[n];
        if (!std::isfinite(value) || value < 0) {
            Error("Nelson:statistics:invalidFreq", "%s must contain nonnegative finite values.",
                "FREQ");
        }
    }
}
//=============================================================================
static void
validateFiniteData(const ArrayOf& x)
{
    const double* ptr = static_cast<const double*>(x.dataPointer());
    indexType count = x.elementCount();
    for (indexType n = 0; n < count; ++n) {
        if (std::isinf(ptr[n])) {
            Error("Nelson:statistics:xMustBeFinite", "%s must contain finite values.", "X");
        }
    }
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
static GevFitOptions
fitOptionsValue(const ArrayOfVector& args)
{
    GevFitOptions options;
    if (args.size() > 4 && !args[4].isEmpty(false) && !args[4].isStruct()) {
        Error("Nelson:statistics:invalidOptions", "%s must be a scalar structure.", "OPTIONS");
    }
    if (args.size() > 4 && !args[4].isEmpty(false)) {
        double maxIter = scalarOptionField(args[4], "MaxIter", options.maxIter);
        double tolX = scalarOptionField(args[4], "TolX", options.tolX);
        if (!std::isfinite(maxIter) || maxIter <= 0 || std::floor(maxIter) != maxIter) {
            Error("Nelson:statistics:optionsMaxIterPositiveIntegerScalar",
                "%s must be a positive integer scalar.", "OPTIONS MaxIter");
        }
        if (!std::isfinite(tolX) || tolX <= 0) {
            Error("Nelson:statistics:optionsTolXPositiveScalar", "%s must be a positive scalar.",
                "OPTIONS TolX");
        }
        options.maxIter = static_cast<int>(maxIter);
        options.tolX = tolX;
    }
    return options;
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
static double
gevLogSurvival(double x, double k, double sigma, double mu)
{
    if (k == 0) {
        double q = std::exp(-(x - mu) / sigma);
        return std::log(-std::expm1(-q));
    }
    double t = 1.0 + k * (x - mu) / sigma;
    if (t <= 0) {
        return k > 0 ? 0.0 : -std::numeric_limits<double>::infinity();
    }
    double q = std::exp(-std::log(t) / k);
    return std::log(-std::expm1(-q));
}
//=============================================================================
static double
gevNegativeLogLikelihood(double k, double sigma, double mu, const double* x, const double* c,
    const double* f, indexType count)
{
    if (std::isnan(k) || std::isnan(sigma) || std::isnan(mu)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (!validScale(sigma) || std::abs(k) > 5.0) {
        return std::numeric_limits<double>::infinity();
    }
    double logLikelihood = 0;
    for (indexType n = 0; n < count; ++n) {
        if (f[n] == 0) {
            continue;
        }
        if (std::isnan(x[n])) {
            return std::numeric_limits<double>::quiet_NaN();
        }
        double term = c[n] == 0 ? std::log(gevPdfValue(x[n], k, sigma, mu))
                                : gevLogSurvival(x[n], k, sigma, mu);
        if (!std::isfinite(term)) {
            return std::numeric_limits<double>::infinity();
        }
        logLikelihood += f[n] * term;
    }
    return -logLikelihood;
}
//=============================================================================
static std::array<double, 9>
nanCovariance()
{
    double nanValue = std::numeric_limits<double>::quiet_NaN();
    return { nanValue, nanValue, nanValue, nanValue, nanValue, nanValue, nanValue, nanValue,
        nanValue };
}
//=============================================================================
static bool
invert3x3(const std::array<double, 9>& a, std::array<double, 9>& inv)
{
    double det = a[0] * (a[4] * a[8] - a[7] * a[5]) - a[3] * (a[1] * a[8] - a[7] * a[2])
        + a[6] * (a[1] * a[5] - a[4] * a[2]);
    if (!std::isfinite(det) || det <= 0) {
        return false;
    }
    inv[0] = (a[4] * a[8] - a[7] * a[5]) / det;
    inv[1] = -(a[1] * a[8] - a[7] * a[2]) / det;
    inv[2] = (a[1] * a[5] - a[4] * a[2]) / det;
    inv[3] = -(a[3] * a[8] - a[6] * a[5]) / det;
    inv[4] = (a[0] * a[8] - a[6] * a[2]) / det;
    inv[5] = -(a[0] * a[5] - a[3] * a[2]) / det;
    inv[6] = (a[3] * a[7] - a[6] * a[4]) / det;
    inv[7] = -(a[0] * a[7] - a[6] * a[1]) / det;
    inv[8] = (a[0] * a[4] - a[3] * a[1]) / det;
    return true;
}
//=============================================================================
static std::array<double, 9>
gevCovariance(double k, double sigma, double mu, const double* x, const double* c, const double* f,
    indexType count)
{
    std::array<double, 3> step { std::max(1e-5 * std::abs(k), 1e-5), std::max(1e-5 * sigma, 1e-6),
        std::max(1e-5 * std::max(std::abs(mu), sigma), 1e-6) };
    std::array<double, 3> theta { k, sigma, mu };
    double f0 = gevNegativeLogLikelihood(k, sigma, mu, x, c, f, count);
    std::array<double, 9> h {};
    for (int i = 0; i < 3; ++i) {
        std::array<double, 3> up = theta;
        std::array<double, 3> down = theta;
        up[i] += step[i];
        down[i] -= step[i];
        double fp = gevNegativeLogLikelihood(up[0], up[1], up[2], x, c, f, count);
        double fm = gevNegativeLogLikelihood(down[0], down[1], down[2], x, c, f, count);
        h[i + i * 3] = (fp - 2.0 * f0 + fm) / (step[i] * step[i]);
        for (int j = i + 1; j < 3; ++j) {
            std::array<double, 3> pp = theta, pm = theta, mp = theta, mm = theta;
            pp[i] += step[i];
            pp[j] += step[j];
            pm[i] += step[i];
            pm[j] -= step[j];
            mp[i] -= step[i];
            mp[j] += step[j];
            mm[i] -= step[i];
            mm[j] -= step[j];
            double value = (gevNegativeLogLikelihood(pp[0], pp[1], pp[2], x, c, f, count)
                               - gevNegativeLogLikelihood(pm[0], pm[1], pm[2], x, c, f, count)
                               - gevNegativeLogLikelihood(mp[0], mp[1], mp[2], x, c, f, count)
                               + gevNegativeLogLikelihood(mm[0], mm[1], mm[2], x, c, f, count))
                / (4.0 * step[i] * step[j]);
            h[i + j * 3] = value;
            h[j + i * 3] = value;
        }
    }
    std::array<double, 9> cov = nanCovariance();
    return invert3x3(h, cov) ? cov : nanCovariance();
}
//=============================================================================
static double
thetaNLogL(const std::array<double, 3>& theta, const double* x, const double* c, const double* f,
    indexType count)
{
    return gevNegativeLogLikelihood(theta[0], std::exp(theta[1]), theta[2], x, c, f, count);
}
//=============================================================================
static void
sortSimplex(std::array<std::array<double, 3>, 4>& simplex, std::array<double, 4>& values)
{
    for (int i = 0; i < 3; ++i) {
        for (int j = i + 1; j < 4; ++j) {
            if (values[j] < values[i]) {
                std::swap(values[i], values[j]);
                std::swap(simplex[i], simplex[j]);
            }
        }
    }
}
//=============================================================================
static double
simplexSpread(const std::array<double, 4>& values)
{
    double spread = 0;
    for (int i = 1; i < 4; ++i) {
        spread = std::max(spread, std::abs(values[i] - values[0]));
    }
    return spread;
}
//=============================================================================
static std::array<double, 3>
fitGevSimplex(const std::array<double, 3>& start, const double* x, const double* c, const double* f,
    indexType count, const GevFitOptions& options)
{
    std::array<std::array<double, 3>, 4> simplex { start, start, start, start };
    simplex[1][0] += 0.1;
    simplex[2][1] += 0.1;
    simplex[3][2] += 0.1 * std::exp(start[1]);
    std::array<double, 4> values {};
    for (int i = 0; i < 4; ++i) {
        values[i] = thetaNLogL(simplex[i], x, c, f, count);
    }
    for (int iter = 0; iter < options.maxIter; ++iter) {
        sortSimplex(simplex, values);
        if (simplexSpread(values) <= options.tolX * (1.0 + std::abs(values[0]))) {
            break;
        }
        std::array<double, 3> centroid {};
        for (int i = 0; i < 3; ++i) {
            for (int d = 0; d < 3; ++d) {
                centroid[d] += simplex[i][d] / 3.0;
            }
        }
        std::array<double, 3> reflected {}, expanded {}, contracted {};
        for (int d = 0; d < 3; ++d) {
            reflected[d] = centroid[d] + (centroid[d] - simplex[3][d]);
        }
        double fr = thetaNLogL(reflected, x, c, f, count);
        if (fr < values[0]) {
            for (int d = 0; d < 3; ++d) {
                expanded[d] = centroid[d] + 2.0 * (reflected[d] - centroid[d]);
            }
            double fe = thetaNLogL(expanded, x, c, f, count);
            if (fe < fr) {
                simplex[3] = expanded;
                values[3] = fe;
            } else {
                simplex[3] = reflected;
                values[3] = fr;
            }
        } else if (fr < values[2]) {
            simplex[3] = reflected;
            values[3] = fr;
        } else {
            for (int d = 0; d < 3; ++d) {
                contracted[d] = centroid[d] + 0.5 * (simplex[3][d] - centroid[d]);
            }
            double fc = thetaNLogL(contracted, x, c, f, count);
            if (fc < values[3]) {
                simplex[3] = contracted;
                values[3] = fc;
            } else {
                for (int i = 1; i < 4; ++i) {
                    for (int d = 0; d < 3; ++d) {
                        simplex[i][d] = simplex[0][d] + 0.5 * (simplex[i][d] - simplex[0][d]);
                    }
                    values[i] = thetaNLogL(simplex[i], x, c, f, count);
                }
            }
        }
    }
    sortSimplex(simplex, values);
    return simplex[0];
}
//=============================================================================
static GevColumnEstimate
fitGevColumn(const double* x, const double* censoring, const double* frequency, indexType rows,
    indexType column, const GevFitOptions& options)
{
    indexType offset = column * rows;
    double totalWeight = 0;
    double uncensoredWeight = 0;
    double weightedSum = 0;
    bool hasNaN = false;
    for (indexType r = 0; r < rows; ++r) {
        double weight = frequency[offset + r];
        double xv = x[offset + r];
        hasNaN = hasNaN || std::isnan(xv);
        totalWeight += weight;
        weightedSum += weight * xv;
        if (censoring[offset + r] == 0 && weight > 0) {
            uncensoredWeight += weight;
        }
    }
    GevColumnEstimate estimate { std::numeric_limits<double>::quiet_NaN(),
        std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
        std::numeric_limits<double>::quiet_NaN(), nanCovariance() };
    if (hasNaN || totalWeight <= 0 || uncensoredWeight <= 0) {
        return estimate;
    }
    double mean = weightedSum / totalWeight;
    double secondMoment = 0;
    for (indexType r = 0; r < rows; ++r) {
        double delta = x[offset + r] - mean;
        secondMoment += frequency[offset + r] * delta * delta;
    }
    double variance = secondMoment / totalWeight;
    double sigma0 = variance > 0 ? std::sqrt(6.0 * variance) / PI_VALUE : 1.0;
    double mu0 = mean - EULER_GAMMA * sigma0;
    std::array<double, 3> theta = fitGevSimplex({ 0.0, std::log(sigma0), mu0 }, x + offset,
        censoring + offset, frequency + offset, rows, options);
    estimate.k = theta[0];
    estimate.sigma = std::exp(theta[1]);
    estimate.mu = theta[2];
    estimate.negativeLogLikelihood = gevNegativeLogLikelihood(estimate.k, estimate.sigma,
        estimate.mu, x + offset, censoring + offset, frequency + offset, rows);
    estimate.covariance = gevCovariance(estimate.k, estimate.sigma, estimate.mu, x + offset,
        censoring + offset, frequency + offset, rows);
    return estimate;
}
//=============================================================================
ArrayOf
GeneralizedExtremeValuePdf(const ArrayOfVector& args)
{
    return applyQuaternary(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        0, false);
}
//=============================================================================
ArrayOf
GeneralizedExtremeValueCdf(const ArrayOfVector& args)
{
    bool upper = false;
    ArrayOfVector numericArgs = args;
    if (!numericArgs.empty() && isTextScalar(numericArgs.back())) {
        upper = matchValue(numericArgs.back(), { L"upper" },
                    formatI18nMessage(
                        resolveMessageTemplate(L"Nelson:statistics:invalidDistributionOption",
                            L"Invalid %s distribution option."),
                        L"generalized extreme value"))
            == L"upper";
        numericArgs.pop_back();
    }
    if (numericArgs.size() < 4) {
        Error("Nelson:notEnoughInputs", "Not enough input arguments.");
    }
    if (numericArgs.size() > 4) {
        Error("Nelson:tooManyInputs", "Too many input arguments.");
    }
    return applyQuaternary(numericArgs,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        1, upper);
}
//=============================================================================
ArrayOf
GeneralizedExtremeValueInv(const ArrayOfVector& args)
{
    return applyQuaternary(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"P"),
        2, false);
}
//=============================================================================
ArrayOf
GeneralizedExtremeValueRnd(const ArrayOfVector& args)
{
    ArrayOfVector typeArgs;
    typeArgs << args[0] << args[1] << args[2];
    NelsonType outputType = floatingOutputType(typeArgs);
    ArrayOf k = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"K"));
    ArrayOf sigma = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"SIGMA"));
    ArrayOf mu = asDoubleArray(args[2],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"MU"));
    Dimensions dims
        = args.size() > 3 ? dimensionsFromSizeArgs(args, 3) : commonDimensions(typeArgs);
    if (args.size() > 3) {
        validateRequestedDimensions(k, sigma, mu, dims);
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
    for (ompIndexType n = 0; n < (ompIndexType)count; ++n) {
        double value = gevInvValue(
            getFloatingValue(ptr, outputType, n), valueAt(k, n), valueAt(sigma, n), valueAt(mu, n));
        setFloatingValue(ptr, outputType, n, value);
    }
    return r;
}
//=============================================================================
GeneralizedExtremeValueStatResult
GeneralizedExtremeValueStat(const ArrayOfVector& args)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf k = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"K"));
    ArrayOf sigma = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"SIGMA"));
    ArrayOf mu = asDoubleArray(args[2],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"MU"));
    ArrayOfVector values;
    values << k << sigma << mu;
    Dimensions dims = commonDimensions(values);
    indexType count = dims.elementCount();
    void* meanPtr = ArrayOf::allocateArrayOf(outputType, count);
    void* varPtr = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType n = 0; n < (ompIndexType)count; ++n) {
        double kv = valueAt(k, n);
        double sv = valueAt(sigma, n);
        double mv = valueAt(mu, n);
        double mean = std::numeric_limits<double>::quiet_NaN();
        double variance = mean;
        if (!std::isnan(kv) && !std::isnan(mv) && validScale(sv)) {
            if (kv == 0) {
                mean = mv + EULER_GAMMA * sv;
                variance = (PI_VALUE * PI_VALUE / 6.0) * sv * sv;
            } else {
                double g1 = std::tgamma(1.0 - kv);
                if (kv < 1.0) {
                    mean = mv + sv * (g1 - 1.0) / kv;
                }
                if (kv < 0.5) {
                    double g2 = std::tgamma(1.0 - 2.0 * kv);
                    variance = sv * sv * (g2 - g1 * g1) / (kv * kv);
                }
            }
        }
        setFloatingValue(meanPtr, outputType, n, mean);
        setFloatingValue(varPtr, outputType, n, variance);
    }
    GeneralizedExtremeValueStatResult result;
    result.mean = ArrayOf(outputType, dims, meanPtr);
    result.variance = ArrayOf(outputType, dims, varPtr);
    return result;
}
//=============================================================================
GeneralizedExtremeValueFitResult
GeneralizedExtremeValueFit(const ArrayOfVector& args)
{
    ArrayOf x = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    if (x.isEmpty(false) || x.dimensions().length() > 2) {
        Error("Nelson:statistics:xMustBeNonemptyVectorOrMatrix",
            "X must be a nonempty vector or matrix.");
    }
    validateFiniteData(x);
    double alpha = fitAlphaValue(args);
    GevFitOptions options = fitOptionsValue(args);
    Dimensions dims = x.dimensions();
    bool vectorInput = x.isVector();
    indexType rows = vectorInput ? x.elementCount() : dims.rows();
    indexType cols = vectorInput ? 1 : dims.columns();
    ArrayOf data = x;
    Dimensions reshaped(rows, 1);
    if (vectorInput && dims.rows() == 1) {
        data.reshape(reshaped);
    }
    ArrayOf censoring;
    ArrayOf frequency;
    prepareCensoringFrequency(args, 2, dims, censoring, frequency);
    if (vectorInput && dims.rows() == 1) {
        censoring.reshape(reshaped);
        frequency.reshape(reshaped);
    }
    const double* xp = static_cast<const double*>(data.dataPointer());
    const double* cp = static_cast<const double*>(censoring.dataPointer());
    const double* fp = static_cast<const double*>(frequency.dataPointer());
    double* params = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, cols * 3));
    double* pci = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, cols * 6));
    double z = NormalInv01(1.0 - alpha / 2.0);
#if WITH_OPENMP
#pragma omp parallel for if (cols > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType c = 0; c < (ompIndexType)cols; ++c) {
        GevColumnEstimate estimate = fitGevColumn(xp, cp, fp, rows, c, options);
        params[c] = estimate.k;
        params[c + cols] = estimate.sigma;
        params[c + 2 * cols] = estimate.mu;
        double kSe = estimate.covariance[0] >= 0 ? std::sqrt(estimate.covariance[0])
                                                 : std::numeric_limits<double>::quiet_NaN();
        double sigmaLogSe = estimate.covariance[4] >= 0 && estimate.sigma > 0
            ? std::sqrt(estimate.covariance[4]) / estimate.sigma
            : std::numeric_limits<double>::quiet_NaN();
        double muSe = estimate.covariance[8] >= 0 ? std::sqrt(estimate.covariance[8])
                                                  : std::numeric_limits<double>::quiet_NaN();
        indexType base = c * 6;
        pci[base] = estimate.k - z * kSe;
        pci[base + 1] = estimate.k + z * kSe;
        pci[base + 2] = estimate.sigma * std::exp(-z * sigmaLogSe);
        pci[base + 3] = estimate.sigma * std::exp(z * sigmaLogSe);
        pci[base + 4] = estimate.mu - z * muSe;
        pci[base + 5] = estimate.mu + z * muSe;
    }
    GeneralizedExtremeValueFitResult result;
    result.parameterHat
        = ArrayOf(NLS_DOUBLE, vectorInput ? Dimensions(1, 3) : Dimensions(cols, 3), params);
    result.parameterCI = ArrayOf(NLS_DOUBLE,
        vectorInput ? Dimensions(2, 3) : Dimensions(std::vector<indexType> { 2, 3, cols }), pci);
    return result;
}
//=============================================================================
static GeneralizedExtremeValueLikeResult
makeGevLikeResult(double nlogL, const std::array<double, 9>& covariance)
{
    double* nlogPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    nlogPtr[0] = nlogL;
    double* covPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 9));
    for (indexType n = 0; n < 9; ++n) {
        covPtr[n] = covariance[n];
    }
    GeneralizedExtremeValueLikeResult result;
    result.negativeLogLikelihood = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), nlogPtr);
    result.covariance = ArrayOf(NLS_DOUBLE, Dimensions(3, 3), covPtr);
    return result;
}
//=============================================================================
GeneralizedExtremeValueLikeResult
GeneralizedExtremeValueLike(const ArrayOfVector& args)
{
    ArrayOf params = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"PARAMS"));
    if (!params.isVector() || params.elementCount() != 3) {
        Error("Nelson:statistics:invalidParams", "PARAMS must be a %s vector.", "three-element");
    }
    const double* pp = static_cast<const double*>(params.dataPointer());
    ArrayOf x = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    if (x.isEmpty(false)) {
        Error("Nelson:validators:mustBeNonempty", "%s must be nonempty.", "X");
    }
    validateFiniteData(x);
    Dimensions dims = x.dimensions();
    ArrayOf censoring;
    ArrayOf frequency;
    prepareCensoringFrequency(args, 2, dims, censoring, frequency);
    std::array<double, 9> nanCov = nanCovariance();
    if (std::isnan(pp[0]) || std::isnan(pp[1]) || std::isnan(pp[2])) {
        return makeGevLikeResult(std::numeric_limits<double>::quiet_NaN(), nanCov);
    }
    if (!validScale(pp[1])) {
        return makeGevLikeResult(std::numeric_limits<double>::infinity(), nanCov);
    }
    const double* xp = static_cast<const double*>(x.dataPointer());
    const double* cp = static_cast<const double*>(censoring.dataPointer());
    const double* fp = static_cast<const double*>(frequency.dataPointer());
    indexType count = x.elementCount();
    double nlogL = gevNegativeLogLikelihood(pp[0], pp[1], pp[2], xp, cp, fp, count);
    if (std::isnan(nlogL) || std::isinf(nlogL)) {
        return makeGevLikeResult(nlogL, nanCov);
    }
    return makeGevLikeResult(nlogL, gevCovariance(pp[0], pp[1], pp[2], xp, cp, fp, count));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
