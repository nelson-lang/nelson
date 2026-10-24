//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__imwarp3d_kernel__Builtin.hpp"
#include "ImageWarp3D.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <algorithm>
#include <array>
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
    const auto* ptr = static_cast<const double*>(work.dataPointer());
    std::vector<double> values(ptr, ptr + work.elementCount());
    for (double v : values) {
        if (!std::isfinite(v)) {
            Error(L"Nelson:imwarp:invalidInput", message);
        }
    }
    return values;
}
//=============================================================================
static std::array<double, 16>
parseMatrix4(const ArrayOf& value)
{
    std::vector<double> values = realDataAsDouble(value,
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:image_processing:mustBeFinite", L"%s must be finite."),
            L"Transformation matrix"));
    if (values.size() != 16) {
        Error(L"Nelson:image_processing:invalidTransform",
            L"Transformation matrix must be %d-by-%d.", 4, 4);
    }
    std::array<double, 16> matrix {};
    std::copy(values.begin(), values.end(), matrix.begin());
    return matrix;
}
//=============================================================================
static std::array<double, 3>
parseTriplet(const ArrayOf& value, const std::wstring& message)
{
    std::vector<double> values = realDataAsDouble(value, message);
    if (values.size() != 3) {
        Error(L"Nelson:imwarp:invalidInput", message);
    }
    return { values[0], values[1], values[2] };
}
//=============================================================================
static std::array<indexType, 3>
parseOutputSize(const ArrayOf& value)
{
    std::vector<double> values = realDataAsDouble(value,
        formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:image_processing:containFinite",
                              L"%s must contain finite values."),
            L"Output size"));
    if (values.size() != 3) {
        Error(L"Nelson:imwarp3d:outputSizeNotThreeDims",
            L"Output size must contain three dimensions.");
    }
    std::array<indexType, 3> outputSize {};
    for (size_t k = 0; k < values.size(); k++) {
        if (values[k] < 0 || values[k] != std::floor(values[k])) {
            Error(L"Nelson:image_processing:outputSizeNotInteger",
                L"%s must contain integer values.", L"Output size");
        }
        outputSize[k] = static_cast<indexType>(values[k]);
    }
    return outputSize;
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
    Error(L"Nelson:image_processing:unsupportedInterpolationMethod", L"Unsupported \"%s\" option.",
        L"interpolation method");
    return ImageWarpInterpolation::Linear;
}
//=============================================================================
static double
parseFillValue(const ArrayOf& value)
{
    std::vector<double> values = realDataAsDouble(value,
        formatI18nMessage(
            Nelson::resolveMessageTemplate(
                L"Nelson:image_processing:fillValuesFinite", L"%s values must be finite."),
            L"Fill"));
    if (values.size() != 1) {
        Error(L"Nelson:image_processing:fillValuesScalarDVolumes",
            L"Fill values must be scalar for 3-D volumes.");
    }
    return values[0];
}
//=============================================================================
ArrayOfVector
__imwarp3d_kernel__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 9, 9);
    nargoutcheck(nLhs, 0, 1);
    ImageWarp3DOptions options;
    options.inverseTransform = parseMatrix4(argIn[1]);
    auto sourceWorldStart = parseTriplet(argIn[2],
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:image_processing:mustBeFinite", L"%s must be finite."),
            L"Source world limits"));
    auto sourcePixelExtent = parseTriplet(argIn[3],
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:image_processing:mustBeFinite", L"%s must be finite."),
            L"Source pixel extents"));
    auto outputWorldStart = parseTriplet(argIn[4],
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:image_processing:mustBeFinite", L"%s must be finite."),
            L"Output world limits"));
    auto outputPixelExtent = parseTriplet(argIn[5],
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:image_processing:mustBeFinite", L"%s must be finite."),
            L"Output pixel extents"));
    auto outputSize = parseOutputSize(argIn[6]);
    options.sourceXWorldStart = sourceWorldStart[0];
    options.sourceYWorldStart = sourceWorldStart[1];
    options.sourceZWorldStart = sourceWorldStart[2];
    options.sourcePixelExtentX = sourcePixelExtent[0];
    options.sourcePixelExtentY = sourcePixelExtent[1];
    options.sourcePixelExtentZ = sourcePixelExtent[2];
    options.outputXWorldStart = outputWorldStart[0];
    options.outputYWorldStart = outputWorldStart[1];
    options.outputZWorldStart = outputWorldStart[2];
    options.outputPixelExtentX = outputPixelExtent[0];
    options.outputPixelExtentY = outputPixelExtent[1];
    options.outputPixelExtentZ = outputPixelExtent[2];
    options.outputRows = outputSize[0];
    options.outputColumns = outputSize[1];
    options.outputPlanes = outputSize[2];
    options.interpolation = parseInterpolation(argIn[7]);
    options.fillValue = parseFillValue(argIn[8]);
    ArrayOfVector retval;
    retval << ImageWarp3D(argIn[0], options);
    return retval;
}
//=============================================================================
} // namespace Nelson::ImageProcessingGateway
//=============================================================================
