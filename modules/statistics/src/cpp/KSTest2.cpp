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
matchOption(const ArrayOf& value, const std::vector<std::wstring>& names)
{
    if (!isTextScalar(value)) {
        Error("Nelson:validators:mustBeTextScalar",
            "%s must be a character vector or string scalar.", "Name");
    }
    std::wstring text = lowerText(value);
    std::vector<std::wstring> matches;
    for (const auto& name : names) {
        if (name.rfind(text, 0) == 0) {
            matches.push_back(name);
        }
    }
    if (matches.empty()) {
        Error("Nelson:statistics:unknownNameValue", "Invalid name-value argument name.");
    }
    if (matches.size() > 1) {
        Error("Nelson:statistics:ambiguousNameValue", "Ambiguous name-value argument name.");
    }
    return matches[0];
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
asDoubleVector(const ArrayOf& value, const std::wstring& message)
{
    if (!value.isNumeric() || value.isSparse() || value.isComplex()) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    if (!value.isVector()) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    ArrayOf copy = value;
    copy.promoteType(NLS_DOUBLE);
    return copy;
}
//=============================================================================
static double
scalarDouble(const ArrayOf& value, const std::wstring& message)
{
    if (!value.isNumeric() || value.isSparse() || value.isComplex()) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    ArrayOf copy = value;
    copy.promoteType(NLS_DOUBLE);
    return copy.asDoubleScalar(false, false);
}
//=============================================================================
static double
kolmogorovPValue(double z)
{
    if (z < 0 || std::isnan(z)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (z == 0) {
        return 1.0;
    }
    double sum = 0;
    for (int k = 1; k < 200; ++k) {
        double term = std::exp(-2.0 * k * k * z * z);
        sum += (k % 2 ? 1.0 : -1.0) * term;
        if (term < 1e-14) {
            break;
        }
    }
    return std::min(1.0, std::max(0.0, 2.0 * sum));
}
//=============================================================================
static std::vector<double>
sampleValues(const ArrayOf& value, const std::wstring& message)
{
    ArrayOf array = asDoubleVector(value, message);
    const double* data = static_cast<const double*>(array.dataPointer());
    std::vector<double> sample;
    sample.reserve(array.elementCount());
    for (indexType k = 0; k < array.elementCount(); ++k) {
        if (!std::isnan(data[k])) {
            sample.push_back(data[k]);
        }
    }
    if (sample.empty()) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    std::sort(sample.begin(), sample.end());
    return sample;
}
//=============================================================================
static double
ksStatistic(const std::vector<double>& x1, const std::vector<double>& x2, const std::wstring& tail)
{
    indexType i = 0;
    indexType j = 0;
    double dPlus = 0;
    double dMinus = 0;
    double n1 = static_cast<double>(x1.size());
    double n2 = static_cast<double>(x2.size());
    while (i < x1.size() || j < x2.size()) {
        double value;
        if (j == x2.size() || (i < x1.size() && x1[i] <= x2[j])) {
            value = x1[i];
        } else {
            value = x2[j];
        }
        while (i < x1.size() && x1[i] == value) {
            ++i;
        }
        while (j < x2.size() && x2[j] == value) {
            ++j;
        }
        double diff = static_cast<double>(i) / n1 - static_cast<double>(j) / n2;
        dPlus = std::max(dPlus, diff);
        dMinus = std::max(dMinus, -diff);
    }
    if (tail == L"larger") {
        return dPlus;
    }
    if (tail == L"smaller") {
        return dMinus;
    }
    return std::max(dPlus, dMinus);
}
//=============================================================================
KSTest2Result
KSTest2(const ArrayOfVector& args)
{
    std::vector<double> x1 = sampleValues(args[0],
        formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:statistics:mustBeRealVector",
                              L"%s must be a real numeric vector."),
            L"X1"));
    std::vector<double> x2 = sampleValues(args[1],
        formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:statistics:mustBeRealVector",
                              L"%s must be a real numeric vector."),
            L"X2"));
    double alpha = 0.05;
    std::wstring tail = L"unequal";
    if ((args.size() - 2) % 2 != 0) {
        Error("Nelson:statistics:nameValueNotPaired", "Name-value arguments must be paired.");
    }
    for (sizeType pos = 2; pos < args.size(); pos += 2) {
        std::wstring name = matchOption(args[pos], { L"alpha", L"tail" });
        if (name == L"alpha") {
            alpha = scalarDouble(args[pos + 1],
                formatI18nMessage(Nelson::resolveMessageTemplate(
                                      L"Nelson:validators:mustBeNumeric", L"%s must be numeric."),
                    L"ALPHA"));
            if (!(alpha > 0 && alpha < 1)) {
                Error("Nelson:statistics:alphaOutOfRange",
                    "%s must be a scalar in the range (0,1).", "ALPHA");
            }
        } else {
            tail = matchValue(args[pos + 1], { L"unequal", L"larger", L"smaller" },
                resolveMessageTemplate(L"Nelson:statistics:tailUnequalLargerSmaller",
                    L"TAIL must be 'unequal', 'larger', or 'smaller'."));
        }
    }
    double stat = ksStatistic(x1, x2, tail);
    double n = (static_cast<double>(x1.size()) * static_cast<double>(x2.size()))
        / (static_cast<double>(x1.size()) + static_cast<double>(x2.size()));
    double p = tail == L"unequal"
        ? kolmogorovPValue((std::sqrt(n) + 0.12 + 0.11 / std::sqrt(n)) * stat)
        : std::exp(-2.0 * n * stat * stat);
    KSTest2Result result;
    result.h = ArrayOf::logicalScalar(!std::isnan(p) && p < alpha);
    result.p = ArrayOf::doubleScalar(p);
    result.statistic = ArrayOf::doubleScalar(stat);
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
