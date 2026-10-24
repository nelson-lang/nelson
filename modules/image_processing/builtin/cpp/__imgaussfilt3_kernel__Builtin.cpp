//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__imgaussfilt3_kernel__Builtin.hpp"
#include "ImageGaussianFilter3D.hpp"
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
realDataAsDouble(const ArrayOf& value, const std::wstring& message)
{
    if (!value.isNumeric() || value.isComplex() || value.isSparse()) {
        Error(L"Nelson:imgaussfilt3:invalidInput", message);
    }
    ArrayOf work(value);
    work.promoteType(NLS_DOUBLE);
    const double* ptr = static_cast<const double*>(work.dataPointer());
    std::vector<double> values(ptr, ptr + work.elementCount());
    for (double v : values) {
        if (!std::isfinite(v)) {
            Error(L"Nelson:imgaussfilt3:invalidInput", message);
        }
    }
    return values;
}
//=============================================================================
static std::vector<indexType>
parseFilterSize(const ArrayOf& value)
{
    std::vector<double> values = realDataAsDouble(value,
        formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:image_processing:containPositiveOddIntegers",
                L"%s must contain positive odd integers."),
            L"Filter size"));
    if (values.size() != 3) {
        Error(L"Nelson:image_processing:containPositiveOddIntegers",
            L"%s must contain positive odd integers.", L"Filter size");
    }
    std::vector<indexType> sizes(3);
    for (size_t k = 0; k < values.size(); k++) {
        double v = values[k];
        if (v != std::floor(v) || v < 1 || std::fmod(v, 2.0) == 0.0) {
            Error(L"Nelson:image_processing:containPositiveOddIntegers",
                L"%s must contain positive odd integers.", L"Filter size");
        }
        sizes[k] = static_cast<indexType>(v);
    }
    return sizes;
}
//=============================================================================
static std::vector<double>
parseSigma(const ArrayOf& value)
{
    std::vector<double> sigma = realDataAsDouble(value,
        formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:image_processing:containPositiveFinite",
                L"%s must contain positive finite values."),
            L"Sigma"));
    if (sigma.size() != 3) {
        Error(L"Nelson:image_processing:sigmaScalarElementVector",
            L"Sigma must be a scalar or %d-element vector.", 3);
    }
    for (double v : sigma) {
        if (v <= 0) {
            Error(L"Nelson:image_processing:sigmaContainPositiveFiniteValues",
                L"%s must contain positive finite values.", L"Sigma");
        }
    }
    return sigma;
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
__imgaussfilt3_kernel__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 4, 4);
    nargoutcheck(nLhs, 0, 1);
    std::vector<indexType> filterSize = parseFilterSize(argIn[1]);
    std::vector<double> sigma = parseSigma(argIn[2]);
    ImageGaussianFilter3DOptions options;
    options.filterRows = filterSize[0];
    options.filterColumns = filterSize[1];
    options.filterPlanes = filterSize[2];
    options.sigmaRows = sigma[0];
    options.sigmaColumns = sigma[1];
    options.sigmaPlanes = sigma[2];
    options.padding = parsePadding(argIn[3]);
    ArrayOfVector retval;
    retval << ImageGaussianFilter3D(argIn[0], options);
    return retval;
}
//=============================================================================
} // namespace Nelson::ImageProcessingGateway
//=============================================================================
