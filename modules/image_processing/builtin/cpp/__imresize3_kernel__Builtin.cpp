//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__imresize3_kernel__Builtin.hpp"
#include "ImageResize3D.hpp"
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
        Error(L"Nelson:imresize3:invalidInput", message);
    }
    ArrayOf work(value);
    work.promoteType(NLS_DOUBLE);
    const double* ptr = static_cast<const double*>(work.dataPointer());
    std::vector<double> values(ptr, ptr + work.elementCount());
    for (double v : values) {
        if (!std::isfinite(v)) {
            Error(L"Nelson:imresize3:invalidInput", message);
        }
    }
    return values;
}
//=============================================================================
static std::vector<indexType>
parseOutputSize(const ArrayOf& value)
{
    std::vector<double> values = realDataAsDouble(value,
        formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:image_processing:containPositiveFinite",
                L"%s must contain positive finite values."),
            L"Output size"));
    if (values.size() != 3) {
        Error(L"Nelson:image_processing:scaleFactorScalarOutputSizeElement",
            L"Scale factor must be a scalar or output size a 3-element vector.");
    }
    std::vector<indexType> outputSize(3);
    for (size_t k = 0; k < values.size(); k++) {
        if (values[k] <= 0 || values[k] != std::floor(values[k])) {
            Error(L"Nelson:image_processing:outputSizeContainPositiveFiniteValues",
                L"%s must contain positive finite values.", L"Output size");
        }
        outputSize[k] = static_cast<indexType>(values[k]);
    }
    return outputSize;
}
//=============================================================================
static ImageResize3DInterpolation
parseMethod(const ArrayOf& value)
{
    if (!(value.isScalarStringArray() || value.isRowVectorCharacterArray())) {
        Error(L"Nelson:image_processing:interpolationMethodString", L"%s must be a string.",
            L"Interpolation method");
    }
    std::wstring method = asLowerText(value);
    if (method == L"nearest") {
        return ImageResize3DInterpolation::Nearest;
    }
    if (method == L"linear") {
        return ImageResize3DInterpolation::Linear;
    }
    Error(L"Nelson:image_processing:unsupportedInterpolationMethod", L"Unsupported \"%s\" option.",
        L"interpolation method");
    return ImageResize3DInterpolation::Linear;
}
//=============================================================================
ArrayOfVector
__imresize3_kernel__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 1);
    std::vector<indexType> outputSize = parseOutputSize(argIn[1]);
    ImageResize3DOptions options;
    options.rows = outputSize[0];
    options.columns = outputSize[1];
    options.planes = outputSize[2];
    options.interpolation = parseMethod(argIn[2]);
    ArrayOfVector retval;
    retval << ImageResize3D(argIn[0], options);
    return retval;
}
//=============================================================================
} // namespace Nelson::ImageProcessingGateway
//=============================================================================
