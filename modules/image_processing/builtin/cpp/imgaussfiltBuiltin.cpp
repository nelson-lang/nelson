//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "imgaussfiltBuiltin.hpp"
#include "ImageGaussianFilter.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <algorithm>
#include <cmath>
#include <vector>
//=============================================================================
namespace Nelson::ImageProcessingGateway {
//=============================================================================
static std::wstring
asLowerText(const ArrayOf& value)
{
    std::wstring text = value.asWideString();
    std::transform(text.begin(), text.end(), text.begin(), ::towlower);
    return text;
}
//=============================================================================
static std::vector<double>
realDataAsDouble(const ArrayOf& value)
{
    ArrayOf work(value);
    work.promoteType(NLS_DOUBLE);
    const double* ptr = static_cast<const double*>(work.dataPointer());
    return std::vector<double>(ptr, ptr + work.elementCount());
}
//=============================================================================
static std::pair<double, double>
parseSigma(const ArrayOf& value)
{
    if (!value.isNumeric() || value.isComplex() || value.isSparse()) {
        Error(L"Nelson:image_processing:sigmaContainPositiveFiniteValues",
            L"%s must contain positive finite values.", L"Sigma");
    }
    std::vector<double> sigma = realDataAsDouble(value);
    if (sigma.size() != 1 && sigma.size() != 2) {
        Error(L"Nelson:image_processing:sigmaScalarElementVector",
            L"Sigma must be a scalar or %d-element vector.", 2);
    }
    for (double v : sigma) {
        if (!std::isfinite(v) || v <= 0) {
            Error(L"Nelson:image_processing:sigmaContainPositiveFiniteValues",
                L"%s must contain positive finite values.", L"Sigma");
        }
    }
    if (sigma.size() == 1) {
        sigma.push_back(sigma[0]);
    }
    return { sigma[0], sigma[1] };
}
//=============================================================================
static std::pair<indexType, indexType>
parseFilterSize(const ArrayOf& value)
{
    if (!value.isNumeric() || value.isComplex() || value.isSparse()) {
        Error(L"Nelson:image_processing:containPositiveOddIntegers",
            L"%s must contain positive odd integers.", L"Filter size");
    }
    std::vector<double> sizes = realDataAsDouble(value);
    if (sizes.size() == 1) {
        sizes.push_back(sizes[0]);
    }
    if (sizes.size() != 2) {
        Error(L"Nelson:image_processing:containPositiveOddIntegers",
            L"%s must contain positive odd integers.", L"Filter size");
    }
    for (double v : sizes) {
        if (!std::isfinite(v) || v != std::floor(v) || v < 1 || std::fmod(v, 2.0) == 0.0) {
            Error(L"Nelson:image_processing:containPositiveOddIntegers",
                L"%s must contain positive odd integers.", L"Filter size");
        }
    }
    return { static_cast<indexType>(sizes[0]), static_cast<indexType>(sizes[1]) };
}
//=============================================================================
static ImageFilterPadding
parsePadding(const ArrayOf& value)
{
    if (value.isScalarStringArray() || value.isRowVectorCharacterArray()) {
        std::wstring text = asLowerText(value);
        if (text == L"replicate") {
            return { ImageFilterPaddingKind::Replicate, 0.0 };
        }
        if (text == L"symmetric") {
            return { ImageFilterPaddingKind::Symmetric, 0.0 };
        }
        if (text == L"circular") {
            return { ImageFilterPaddingKind::Circular, 0.0 };
        }
        Error(L"Nelson:image_processing:unsupportedPaddingOption", L"Unsupported \"%s\" option.",
            L"padding");
    }
    if ((value.isNumeric() || value.isLogical()) && value.isScalar() && !value.isComplex()
        && !value.isSparse()) {
        double constant = value.asDoubleScalar();
        if (!std::isfinite(constant)) {
            Error(L"Nelson:validators:mustBeFinite", L"%s must be finite.", L"Padding value");
        }
        return { ImageFilterPaddingKind::Constant, constant };
    }
    Error(L"Nelson:image_processing:unsupportedPaddingOption", L"Unsupported \"%s\" option.",
        L"padding");
    return {};
}
//=============================================================================
ArrayOfVector
imgaussfiltBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 8);
    nargoutcheck(nLhs, 0, 1);
    ImageGaussianFilterOptions options;
    size_t argIndex = 1;
    if (argIn.size() >= 2
        && !(argIn[1].isScalarStringArray() || argIn[1].isRowVectorCharacterArray())) {
        auto sigma = parseSigma(argIn[1]);
        options.sigmaRows = sigma.first;
        options.sigmaColumns = sigma.second;
        argIndex = 2;
    }
    options.filterRows = static_cast<indexType>(2 * std::ceil(2 * options.sigmaRows) + 1);
    options.filterColumns = static_cast<indexType>(2 * std::ceil(2 * options.sigmaColumns) + 1);
    for (size_t k = argIndex; k < argIn.size(); k += 2) {
        if (k + 1 >= argIn.size()) {
            Error(L"Nelson:image_processing:nameValueArgumentsAppearPairs",
                L"Name-value arguments must be paired.");
        }
        if (!(argIn[k].isScalarStringArray() || argIn[k].isRowVectorCharacterArray())) {
            Error(L"Nelson:imgaussfilt:invalidOptionName", L"%s must be a string.",
                L"imgaussfilt option name");
        }
        std::wstring name = asLowerText(argIn[k]);
        if (name == L"filtersize") {
            auto sizes = parseFilterSize(argIn[k + 1]);
            options.filterRows = sizes.first;
            options.filterColumns = sizes.second;
        } else if (name == L"padding") {
            options.padding = parsePadding(argIn[k + 1]);
        } else if (name == L"filterdomain") {
            if (!(argIn[k + 1].isScalarStringArray() || argIn[k + 1].isRowVectorCharacterArray())) {
                Error(L"Nelson:imgaussfilt:unsupportedOption", L"Unsupported \"%s\" option.",
                    L"imgaussfilt");
            }
            std::wstring domain = asLowerText(argIn[k + 1]);
            if (domain != L"auto" && domain != L"spatial" && domain != L"frequency") {
                Error(L"Nelson:imgaussfilt:unsupportedOption", L"Unsupported \"%s\" option.",
                    L"imgaussfilt");
            }
        } else {
            Error(L"Nelson:imgaussfilt:unsupportedOption", L"Unsupported \"%s\" option.",
                L"imgaussfilt");
        }
    }
    ArrayOfVector retval;
    retval << ImageGaussianFilter(argIn[0], options);
    return retval;
}
//=============================================================================
} // namespace Nelson::ImageProcessingGateway
//=============================================================================
