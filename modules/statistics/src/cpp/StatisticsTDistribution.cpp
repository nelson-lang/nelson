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
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
//=============================================================================
namespace Nelson {
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
static Dimensions
commonDimensions(const ArrayOf& a, const ArrayOf& b)
{
    if (!a.isScalar() && !b.isScalar() && !a.dimensions().equals(b.dimensions())) {
        Error("Nelson:dimagree:sizeMismatchInputArguments", "Size mismatch on input arguments");
    }
    return !a.isScalar() ? a.dimensions() : (!b.isScalar() ? b.dimensions() : Dimensions(1, 1));
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
normalPdf(double x)
{
    static const double invSqrtTwoPi = 0.39894228040143267794;
    return invSqrtTwoPi * std::exp(-0.5 * x * x);
}
//=============================================================================
static double
tPdfValue(double x, double nu)
{
    if (std::isnan(x) || std::isnan(nu) || nu <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (std::isinf(x)) {
        return 0;
    }
    if (std::isinf(nu)) {
        return normalPdf(x);
    }
    double halfNu = 0.5 * nu;
    constexpr double pi = 3.14159265358979323846;
    double logScale
        = std::lgamma(0.5 * (nu + 1.0)) - std::lgamma(halfNu) - 0.5 * (std::log(nu) + std::log(pi));
    return std::exp(logScale - 0.5 * (nu + 1.0) * std::log1p((x * x) / nu));
}
//=============================================================================
static ArrayOf
binaryTFunction(const ArrayOfVector& args, const std::wstring& firstMessage, int mode, bool upper)
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
            value = tPdfValue(a, v);
        } else if (mode == 1) {
            value = std::isinf(v) ? NormalCdf01(a) : StudentTCdf(a, v);
            value = upper && !std::isnan(value) ? std::max(0.0, 1.0 - value) : value;
        } else {
            value = std::isinf(v) ? NormalInv01(a) : StudentTInv(a, v);
        }
        setFloatingValue(out, outputType, k, value);
    }
    return ArrayOf(outputType, dims, out);
}
//=============================================================================
ArrayOf
TPdf(const ArrayOfVector& args)
{
    return binaryTFunction(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        0, false);
}
//=============================================================================
ArrayOf
TCdf(const ArrayOfVector& args)
{
    bool upper = false;
    ArrayOfVector numericArgs = args;
    if (!numericArgs.empty() && isTextScalar(numericArgs.back())) {
        upper = matchValue(numericArgs.back(), { L"upper" },
                    formatI18nMessage(
                        resolveMessageTemplate(L"Nelson:statistics:invalidDistributionOption",
                            L"Invalid %s distribution option."),
                        L"t"))
            == L"upper";
        numericArgs.pop_back();
    }
    if (numericArgs.size() < 2) {
        Error("Nelson:notEnoughInputs", "Not enough input arguments.");
    }
    if (numericArgs.size() > 2) {
        Error("Nelson:tooManyInputs", "Too many input arguments.");
    }
    return binaryTFunction(numericArgs,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        1, upper);
}
//=============================================================================
ArrayOf
TInv(const ArrayOfVector& args)
{
    return binaryTFunction(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"P"),
        2, false);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
