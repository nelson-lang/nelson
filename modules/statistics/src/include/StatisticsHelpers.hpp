//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cmath>
#include <cwctype>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson::StatisticsHelpers {
//=============================================================================
struct FitOptions
{
    int maxIter;
    double tolX;
};
//=============================================================================
inline bool
isTextScalar(const ArrayOf& value)
{
    return value.isRowVectorCharacterArray() || value.isScalarStringArray();
}
//=============================================================================
inline std::wstring
lowerText(const ArrayOf& value)
{
    std::wstring text = value.asWideString();
    std::transform(text.begin(), text.end(), text.begin(), [](wchar_t c) { return towlower(c); });
    return text;
}
//=============================================================================
inline std::wstring
matchValue(
    const ArrayOf& value, const std::vector<std::wstring>& names, const std::wstring& message)
{
    if (!isTextScalar(value)) {
        Error(L"Nelson:statistics:invalidArgumentValue", message);
    }
    std::wstring text = lowerText(value);
    std::vector<std::wstring> matches;
    for (const auto& name : names) {
        if (name.rfind(text, 0) == 0) {
            matches.push_back(name);
        }
    }
    if (matches.size() != 1) {
        Error(L"Nelson:statistics:invalidArgumentValue", message);
    }
    return matches[0];
}
//=============================================================================
inline ArrayOf
asDoubleArray(const ArrayOf& value, const std::wstring& message)
{
    if (!value.isNumeric() || value.isSparse() || value.isComplex()) {
        Error(L"Nelson:statistics:invalidArgumentValue", message);
    }
    ArrayOf copy = value;
    copy.promoteType(NLS_DOUBLE);
    return copy;
}
//=============================================================================
inline double
scalarDouble(const ArrayOf& value, const std::wstring& message)
{
    ArrayOf copy = asDoubleArray(value, message);
    return copy.asDoubleScalar(false, false);
}
//=============================================================================
inline NelsonType
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
inline void
setFloatingValue(void* ptr, NelsonType outputType, indexType index, double value)
{
    if (outputType == NLS_SINGLE) {
        static_cast<single*>(ptr)[index] = static_cast<single>(value);
    } else {
        static_cast<double*>(ptr)[index] = value;
    }
}
//=============================================================================
inline double
getFloatingValue(const void* ptr, NelsonType outputType, indexType index)
{
    if (outputType == NLS_SINGLE) {
        return static_cast<const single*>(ptr)[index];
    }
    return static_cast<const double*>(ptr)[index];
}
//=============================================================================
inline Dimensions
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
inline Dimensions
commonDimensions(const ArrayOf& a, const ArrayOf& b)
{
    ArrayOfVector values;
    values.push_back(a);
    values.push_back(b);
    return commonDimensions(values);
}
//=============================================================================
inline Dimensions
commonDimensions(const ArrayOf& a, const ArrayOf& b, const ArrayOf& c)
{
    ArrayOfVector values;
    values.push_back(a);
    values.push_back(b);
    values.push_back(c);
    return commonDimensions(values);
}
//=============================================================================
inline double
valueAt(const ArrayOf& value, indexType k)
{
    const double* ptr = static_cast<const double*>(value.dataPointer());
    return ptr[value.isScalar() ? 0 : k];
}
//=============================================================================
inline indexType
sizeValue(double value)
{
    if (!std::isfinite(value) || std::floor(value) != value) {
        Error("Nelson:statistics:invalidSize", "Size must contain integer values.");
    }
    return value < 0 ? 0 : static_cast<indexType>(value);
}
//=============================================================================
inline Dimensions
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
inline void
validateRequestedDimensions(const ArrayOf& value, const Dimensions& dims)
{
    if (!value.isScalar() && !value.dimensions().equals(dims)) {
        Error("Nelson:dimagree:sizeMismatchInputArguments", "Size mismatch on input arguments");
    }
}
//=============================================================================
inline void
validateRequestedDimensions(const ArrayOf& a, const ArrayOf& b, const Dimensions& dims)
{
    validateRequestedDimensions(a, dims);
    validateRequestedDimensions(b, dims);
}
//=============================================================================
inline ArrayOfVector
withDefaultParameter(ArrayOfVector args, indexType expected, double defaultValue = 1.0)
{
    if (args.size() == expected - 1) {
        args.push_back(ArrayOf::doubleScalar(defaultValue));
    }
    return args;
}
//=============================================================================
inline ArrayOf
expandedArray(
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
inline void
validateCensoring(const ArrayOf& censoring)
{
    const double* ptr = static_cast<const double*>(censoring.dataPointer());
    for (indexType k = 0; k < censoring.elementCount(); ++k) {
        double value = ptr[k];
        if (std::isnan(value) || (value != 0.0 && value != 1.0)) {
            Error("Nelson:statistics:invalidCensoring", "CENSORING must contain 0 or 1 values.");
        }
    }
}
//=============================================================================
inline void
validateFrequency(const ArrayOf& frequency)
{
    const double* ptr = static_cast<const double*>(frequency.dataPointer());
    for (indexType k = 0; k < frequency.elementCount(); ++k) {
        double value = ptr[k];
        if (!std::isfinite(value) || value < 0) {
            Error("Nelson:statistics:invalidFreq", "%s must contain nonnegative finite values.",
                "FREQ");
        }
    }
}
//=============================================================================
inline double
alphaValue(const ArrayOfVector& args)
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
inline double
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
inline FitOptions
parseFitOptions(const ArrayOfVector& args, int defaultMaxIter = 120, double defaultTolX = 1e-10,
    sizeType optionsIndex = 4)
{
    FitOptions options { defaultMaxIter, defaultTolX };
    if (args.size() > optionsIndex && !args[optionsIndex].isEmpty(false)
        && !args[optionsIndex].isStruct()) {
        Error("Nelson:statistics:invalidOptions", "%s must be a scalar structure.", "OPTIONS");
    }
    if (args.size() > optionsIndex && !args[optionsIndex].isEmpty(false)) {
        double maxIter = scalarOptionField(args[optionsIndex], "MaxIter", options.maxIter);
        double tolX = scalarOptionField(args[optionsIndex], "TolX", options.tolX);
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
inline void
validateFitOptions(const ArrayOfVector& args, int defaultMaxIter = 100, double defaultTolX = 1e-8,
    sizeType optionsIndex = 4)
{
    (void)parseFitOptions(args, defaultMaxIter, defaultTolX, optionsIndex);
}
//=============================================================================
inline void
prepareCensoringFrequency(const ArrayOfVector& args, sizeType firstOptional, const Dimensions& dims,
    ArrayOf& censoring, ArrayOf& frequency)
{
    censoring = args.size() > firstOptional
        ? expandedArray(args[firstOptional], dims, 0.0,
              formatI18nMessage(Nelson::resolveMessageTemplate(
                                    L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
                  L"CENSORING"))
        : expandedArray(ArrayOf::empty(), dims, 0.0, _W(""));
    frequency = args.size() > firstOptional + 1
        ? expandedArray(args[firstOptional + 1], dims, 1.0,
              formatI18nMessage(Nelson::resolveMessageTemplate(
                                    L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
                  L"FREQ"))
        : expandedArray(ArrayOf::empty(), dims, 1.0, _W(""));
    validateCensoring(censoring);
    validateFrequency(frequency);
}
//=============================================================================
inline void
prepareCensoringFrequency(
    const ArrayOfVector& args, const Dimensions& dims, ArrayOf& censoring, ArrayOf& frequency)
{
    prepareCensoringFrequency(args, 2, dims, censoring, frequency);
}
//=============================================================================
} // namespace Nelson::StatisticsHelpers
//=============================================================================
