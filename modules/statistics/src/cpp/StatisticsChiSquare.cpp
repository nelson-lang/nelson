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
chi2PdfValue(double x, double nu)
{
    if (std::isnan(x) || std::isnan(nu) || nu <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x < 0) {
        return 0;
    }
    if (std::isinf(x)) {
        return 0;
    }
    if (x == 0) {
        if (nu < 2) {
            return std::numeric_limits<double>::infinity();
        }
        if (nu == 2) {
            return 0.5;
        }
        return 0;
    }
    double halfNu = 0.5 * nu;
    return std::exp(
        (halfNu - 1.0) * std::log(x) - 0.5 * x - halfNu * std::log(2.0) - std::lgamma(halfNu));
}
//=============================================================================
static ArrayOf
binaryChi2Function(
    const ArrayOfVector& args, const std::wstring& firstMessage, int mode, bool upper)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf first = asDoubleArray(args[0], firstMessage);
    ArrayOf nu = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"V"));
    Dimensions dims = commonDimensions(first, nu);
    indexType count = dims.elementCount();
    void* out = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double a = valueAt(first, k);
        double v = valueAt(nu, k);
        double value;
        if (mode == 0) {
            value = chi2PdfValue(a, v);
        } else if (mode == 1) {
            value = upper ? ChiSquareUpper(a, v) : ChiSquareCdf(a, v);
        } else {
            value = ChiSquareInv(a, v);
        }
        setFloatingValue(out, outputType, k, value);
    }
    return ArrayOf(outputType, dims, out);
}
//=============================================================================
ArrayOf
Chi2Pdf(const ArrayOfVector& args)
{
    return binaryChi2Function(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        0, false);
}
//=============================================================================
ArrayOf
Chi2Cdf(const ArrayOfVector& args)
{
    bool upper = false;
    ArrayOfVector numericArgs = args;
    if (!numericArgs.empty() && isTextScalar(numericArgs.back())) {
        upper = matchValue(numericArgs.back(), { L"upper" },
                    formatI18nMessage(
                        resolveMessageTemplate(L"Nelson:statistics:invalidDistributionOption",
                            L"Invalid %s distribution option."),
                        L"chi-square"))
            == L"upper";
        numericArgs.pop_back();
    }
    if (numericArgs.size() < 2) {
        Error("Nelson:notEnoughInputs", "Not enough input arguments.");
    }
    if (numericArgs.size() > 2) {
        Error("Nelson:tooManyInputs", "Too many input arguments.");
    }
    return binaryChi2Function(numericArgs,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        1, upper);
}
//=============================================================================
ArrayOf
Chi2Inv(const ArrayOfVector& args)
{
    return binaryChi2Function(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"P"),
        2, false);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
