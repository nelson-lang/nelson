//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__imwarp_kernel__Builtin.hpp"
#include "ImageWarp.hpp"
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
        Error(L"Nelson:imwarp:invalidInput", message);
    }
    ArrayOf work(value);
    work.promoteType(NLS_DOUBLE);
    const double* ptr = static_cast<const double*>(work.dataPointer());
    std::vector<double> values(ptr, ptr + work.elementCount());
    for (double v : values) {
        if (!std::isfinite(v)) {
            Error(L"Nelson:imwarp:invalidInput", message);
        }
    }
    return values;
}
//=============================================================================
static std::array<double, 9>
parseMatrix3(const ArrayOf& value)
{
    std::vector<double> values = realDataAsDouble(value,
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:image_processing:mustBeFinite", L"%s must be finite."),
            L"Transformation matrix"));
    if (values.size() != 9) {
        Error(L"Nelson:image_processing:invalidTransform",
            L"Transformation matrix must be %d-by-%d.", 3, 3);
    }
    std::array<double, 9> matrix {};
    std::copy(values.begin(), values.end(), matrix.begin());
    return matrix;
}
//=============================================================================
static std::pair<double, double>
parsePair(const ArrayOf& value, const std::wstring& message)
{
    std::vector<double> values = realDataAsDouble(value, message);
    if (values.size() != 2) {
        Error(L"Nelson:imwarp:invalidInput", message);
    }
    return { values[0], values[1] };
}
//=============================================================================
static std::pair<indexType, indexType>
parseOutputSize(const ArrayOf& value)
{
    std::vector<double> values = realDataAsDouble(value,
        formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:image_processing:containFinite",
                              L"%s must contain finite values."),
            L"Output size"));
    if (values.size() < 2) {
        Error(L"Nelson:imwarp:outputSizeTooFewDims", L"%s must contain at least two dimensions.",
            L"Output size");
    }
    for (double v : values) {
        if (v < 0 || v != std::floor(v)) {
            Error(L"Nelson:image_processing:outputSizeNotInteger",
                L"%s must contain integer values.", L"Output size");
        }
    }
    return { static_cast<indexType>(values[0]), static_cast<indexType>(values[1]) };
}
//=============================================================================
static ImageWarpInterpolation
parseInterpolation(const ArrayOf& value)
{
    if (!(value.isScalarStringArray() || value.isRowVectorCharacterArray())) {
        Error(L"Nelson:image_processing:unsupportedInterpolationMethod",
            L"Unsupported \"%s\" option.", L"interpolation method");
    }
    std::wstring method = asLowerText(value);
    if (method == L"nearest") {
        return ImageWarpInterpolation::Nearest;
    }
    if (method == L"linear" || method == L"bilinear") {
        return ImageWarpInterpolation::Linear;
    }
    if (method == L"cubic") {
        return ImageWarpInterpolation::Cubic;
    }
    Error(L"Nelson:image_processing:unsupportedInterpolationMethod", L"Unsupported \"%s\" option.",
        L"interpolation method");
    return ImageWarpInterpolation::Linear;
}
//=============================================================================
ArrayOfVector
__imwarp_kernel__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 9, 9);
    nargoutcheck(nLhs, 0, 1);
    ImageWarpOptions options;
    options.inverseTransform = parseMatrix3(argIn[1]);
    auto sourceWorldStart = parsePair(argIn[2],
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:image_processing:mustBeFinite", L"%s must be finite."),
            L"Source world limits"));
    auto sourcePixelExtent = parsePair(argIn[3],
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:image_processing:mustBeFinite", L"%s must be finite."),
            L"Source pixel extents"));
    auto outputWorldStart = parsePair(argIn[4],
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:image_processing:mustBeFinite", L"%s must be finite."),
            L"Output world limits"));
    auto outputPixelExtent = parsePair(argIn[5],
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:image_processing:mustBeFinite", L"%s must be finite."),
            L"Output pixel extents"));
    auto outputSize = parseOutputSize(argIn[6]);
    options.sourceXWorldStart = sourceWorldStart.first;
    options.sourceYWorldStart = sourceWorldStart.second;
    options.sourcePixelExtentX = sourcePixelExtent.first;
    options.sourcePixelExtentY = sourcePixelExtent.second;
    options.outputXWorldStart = outputWorldStart.first;
    options.outputYWorldStart = outputWorldStart.second;
    options.outputPixelExtentX = outputPixelExtent.first;
    options.outputPixelExtentY = outputPixelExtent.second;
    options.outputRows = outputSize.first;
    options.outputColumns = outputSize.second;
    options.interpolation = parseInterpolation(argIn[7]);
    options.fillValues = realDataAsDouble(argIn[8],
        formatI18nMessage(
            Nelson::resolveMessageTemplate(
                L"Nelson:image_processing:fillValuesFinite", L"%s values must be finite."),
            L"Fill"));
    ArrayOfVector retval;
    retval << ImageWarp(argIn[0], options);
    return retval;
}
//=============================================================================
} // namespace Nelson::ImageProcessingGateway
//=============================================================================
