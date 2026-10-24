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
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include <cmath>
#include <limits>
#include <vector>
//=============================================================================
namespace Nelson {
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
valueAt(const ArrayOf& value, indexType k)
{
    const double* ptr = static_cast<const double*>(value.dataPointer());
    return ptr[value.isScalar() ? 0 : k];
}
//=============================================================================
static bool
isNonnegativeInteger(double value)
{
    return std::isfinite(value) && value >= 0 && std::floor(value) == value;
}
//=============================================================================
template <typename ResultType, typename Eval>
static ResultType
buildStat(const ArrayOfVector& inputs, const std::vector<std::wstring>& messages, Eval eval)
{
    ArrayOfVector values;
    for (size_t k = 0; k < inputs.size(); ++k) {
        values.push_back(asDoubleArray(inputs[k], messages[k]));
    }
    NelsonType outputType = floatingOutputType(inputs);
    Dimensions dims = commonDimensions(values);
    indexType count = dims.elementCount();
    void* meanPtr = ArrayOf::allocateArrayOf(outputType, count);
    void* varPtr = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType i = 0; i < (ompIndexType)count; ++i) {
        double args[3] = { 0, 0, 0 };
        for (size_t k = 0; k < values.size(); ++k) {
            args[k] = valueAt(values[k], i);
        }
        double mean;
        double variance;
        eval(args, mean, variance);
        setFloatingValue(meanPtr, outputType, i, mean);
        setFloatingValue(varPtr, outputType, i, variance);
    }
    ResultType result;
    result.mean = ArrayOf(outputType, dims, meanPtr);
    result.variance = ArrayOf(outputType, dims, varPtr);
    return result;
}
//=============================================================================
NormalStatResult
NormalStat(const ArrayOfVector& args)
{
    return buildStat<NormalStatResult>(args,
        { formatI18nMessage(
              resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
              L"MU"),
            formatI18nMessage(resolveMessageTemplate(
                                  L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
                L"SIGMA") },
        [](const double* a, double& mean, double& variance) {
            mean = a[0];
            variance = std::isnan(a[0]) || std::isnan(a[1]) || a[1] < 0
                ? std::numeric_limits<double>::quiet_NaN()
                : a[1] * a[1];
            if (std::isnan(variance)) {
                mean = variance;
            }
        });
}
//=============================================================================
GammaStatResult
GammaStat(const ArrayOfVector& args)
{
    return buildStat<GammaStatResult>(args,
        { formatI18nMessage(
              resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
              L"A"),
            formatI18nMessage(resolveMessageTemplate(
                                  L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
                L"B") },
        [](const double* a, double& mean, double& variance) {
            mean = std::isnan(a[0]) || std::isnan(a[1]) || a[0] <= 0 || a[1] <= 0
                ? std::numeric_limits<double>::quiet_NaN()
                : a[0] * a[1];
            variance = std::isnan(mean) ? mean : a[0] * a[1] * a[1];
        });
}
//=============================================================================
ChiSquareStatResult
ChiSquareStat(const ArrayOfVector& args)
{
    return buildStat<ChiSquareStatResult>(args,
        { formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"V") },
        [](const double* a, double& mean, double& variance) {
            mean = std::isnan(a[0]) || a[0] <= 0 ? std::numeric_limits<double>::quiet_NaN() : a[0];
            variance = std::isnan(mean) ? mean : 2 * a[0];
        });
}
//=============================================================================
TStatResult
TStat(const ArrayOfVector& args)
{
    return buildStat<TStatResult>(args,
        { formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"V") },
        [](const double* a, double& mean, double& variance) {
            double v = a[0];
            if (std::isnan(v) || v <= 0) {
                mean = std::numeric_limits<double>::quiet_NaN();
                variance = mean;
            } else {
                mean = v > 1 ? 0 : std::numeric_limits<double>::quiet_NaN();
                variance = std::isinf(v)
                    ? 1
                    : (v > 2 ? v / (v - 2)
                             : (v > 1 ? std::numeric_limits<double>::infinity()
                                      : std::numeric_limits<double>::quiet_NaN()));
            }
        });
}
//=============================================================================
FStatResult
FStat(const ArrayOfVector& args)
{
    return buildStat<FStatResult>(args,
        { formatI18nMessage(
              resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
              L"V1"),
            formatI18nMessage(resolveMessageTemplate(
                                  L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
                L"V2") },
        [](const double* a, double& mean, double& variance) {
            double v1 = a[0];
            double v2 = a[1];
            if (std::isnan(v1) || std::isnan(v2) || v1 <= 0 || v2 <= 0) {
                mean = std::numeric_limits<double>::quiet_NaN();
                variance = mean;
            } else {
                mean = v2 > 2 ? v2 / (v2 - 2) : std::numeric_limits<double>::quiet_NaN();
                variance = v2 > 4
                    ? 2 * v2 * v2 * (v1 + v2 - 2) / (v1 * (v2 - 2) * (v2 - 2) * (v2 - 4))
                    : (v2 > 2 ? std::numeric_limits<double>::infinity()
                              : std::numeric_limits<double>::quiet_NaN());
            }
        });
}
//=============================================================================
BetaStatResult
BetaStat(const ArrayOfVector& args)
{
    return buildStat<BetaStatResult>(args,
        { formatI18nMessage(
              resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
              L"A"),
            formatI18nMessage(resolveMessageTemplate(
                                  L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
                L"B") },
        [](const double* a, double& mean, double& variance) {
            double s = a[0] + a[1];
            mean = std::isnan(a[0]) || std::isnan(a[1]) || a[0] <= 0 || a[1] <= 0
                ? std::numeric_limits<double>::quiet_NaN()
                : a[0] / s;
            variance = std::isnan(mean) ? mean : (a[0] * a[1]) / (s * s * (s + 1));
        });
}
//=============================================================================
BinomialStatResult
BinomialStat(const ArrayOfVector& args)
{
    return buildStat<BinomialStatResult>(args,
        { resolveMessageTemplate(L"Nelson:statistics:nNonnegativeIntegerNumeric",
              L"N must be a nonnegative integer numeric array."),
            resolveMessageTemplate(L"Nelson:statistics:pNumericArrayRange",
                L"P must be a numeric array in the range [0,1].") },
        [](const double* a, double& mean, double& variance) {
            double n = a[0];
            double p = a[1];
            mean = !isNonnegativeInteger(n) || std::isnan(p) || p < 0 || p > 1
                ? std::numeric_limits<double>::quiet_NaN()
                : n * p;
            variance = std::isnan(mean) ? mean : n * p * (1 - p);
        });
}
//=============================================================================
PoissonStatResult
PoissonStat(const ArrayOfVector& args)
{
    return buildStat<PoissonStatResult>(args,
        { resolveMessageTemplate(L"Nelson:statistics:lambdaNonnegativeNumericArray",
            L"LAMBDA must be a nonnegative numeric array.") },
        [](const double* a, double& mean, double& variance) {
            mean = std::isnan(a[0]) || a[0] < 0 ? std::numeric_limits<double>::quiet_NaN() : a[0];
            variance = mean;
        });
}
//=============================================================================
} // namespace Nelson
//=============================================================================
