//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ImageMorphology.hpp"
#include "ImageProcessingCore.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <limits>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
struct NeighborhoodShape
{
    std::vector<uint8_t> mask;
    indexType rows { 0 };
    indexType columns { 0 };
    indexType planes { 1 };
};
//=============================================================================
static NelsonType
outputClassFor(const ArrayOf& image)
{
    switch (image.dataClass()) {
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_INT8:
    case NLS_INT16:
    case NLS_SINGLE:
    case NLS_LOGICAL:
        return image.dataClass();
    default:
        return NLS_DOUBLE;
    }
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
static NeighborhoodShape
neighborhoodAsMask(const ArrayOf& neighborhood)
{
    Dimensions dims = neighborhood.dimensions();
    if ((!neighborhood.isNumeric() && !neighborhood.isLogical()) || neighborhood.isSparse()
        || neighborhood.isEmpty() || dims.length() > 3) {
        Error(L"Nelson:immorph:invalidStrel",
            L"Structuring element neighborhood must be a nonempty 2-D or 3-D numeric or logical "
            L"array.");
    }
    std::vector<double> values = realDataAsDouble(neighborhood);
    NeighborhoodShape shape;
    shape.rows = dims.rows();
    shape.columns = dims.columns();
    shape.planes = dims.length() > 2 ? dims.dimensionLength(2) : 1;
    shape.mask.resize(values.size());
    bool anyTrue = false;
    for (size_t k = 0; k < values.size(); k++) {
        shape.mask[k] = values[k] != 0;
        anyTrue = anyTrue || shape.mask[k] != 0;
    }
    if (!anyTrue) {
        Error(L"Nelson:immorph:emptyStrel",
            L"Structuring element neighborhood must contain at least one true value.");
    }
    return shape;
}
//=============================================================================
template <typename T>
static void
copyRealOutput(T* out, const std::vector<double>& values)
{
    for (size_t k = 0; k < values.size(); k++) {
        assignClampedPixel(out, k, values[k]);
    }
}
//=============================================================================
static void
copyOutput(ArrayOf& out, const std::vector<double>& values)
{
    switch (out.dataClass()) {
    case NLS_UINT8:
        copyRealOutput((uint8*)out.readWriteDataPointer(), values);
        break;
    case NLS_UINT16:
        copyRealOutput((uint16*)out.readWriteDataPointer(), values);
        break;
    case NLS_INT8:
        copyRealOutput((int8*)out.readWriteDataPointer(), values);
        break;
    case NLS_INT16:
        copyRealOutput((int16*)out.readWriteDataPointer(), values);
        break;
    case NLS_SINGLE:
        copyRealOutput((single*)out.readWriteDataPointer(), values);
        break;
    case NLS_LOGICAL:
        copyRealOutput((logical*)out.readWriteDataPointer(), values);
        break;
    default:
        std::copy(values.begin(), values.end(), (double*)out.readWriteDataPointer());
        break;
    }
}
//=============================================================================
static std::vector<double>
morphologyData(const std::vector<double>& input, indexType rows, indexType cols, indexType channels,
    const std::vector<uint8_t>& mask, indexType maskRows, indexType maskCols,
    ImageMorphologyOperation operation, bool logicalInput)
{
    std::vector<double> output(input.size());
    indexType padRows = maskRows / 2;
    indexType padCols = maskCols / 2;
    double padValue = operation == ImageMorphologyOperation::Dilate
        ? -std::numeric_limits<double>::infinity()
        : (logicalInput ? 0.0 : std::numeric_limits<double>::infinity());
    double initialValue = operation == ImageMorphologyOperation::Dilate
        ? -std::numeric_limits<double>::infinity()
        : std::numeric_limits<double>::infinity();
    for (indexType channel = 0; channel < channels; channel++) {
        indexType channelOffset = channel * rows * cols;
        for (indexType col = 0; col < cols; col++) {
            for (indexType row = 0; row < rows; row++) {
                double best = initialValue;
                for (indexType mc = 0; mc < maskCols; mc++) {
                    for (indexType mr = 0; mr < maskRows; mr++) {
                        if (!mask[mr + mc * maskRows]) {
                            continue;
                        }
                        signedIndexType sourceRow = static_cast<signedIndexType>(row)
                            + static_cast<signedIndexType>(mr)
                            - static_cast<signedIndexType>(padRows);
                        signedIndexType sourceCol = static_cast<signedIndexType>(col)
                            + static_cast<signedIndexType>(mc)
                            - static_cast<signedIndexType>(padCols);
                        double value = padValue;
                        if (sourceRow >= 0 && sourceRow < static_cast<signedIndexType>(rows)
                            && sourceCol >= 0 && sourceCol < static_cast<signedIndexType>(cols)) {
                            value = input[static_cast<indexType>(sourceRow)
                                + static_cast<indexType>(sourceCol) * rows + channelOffset];
                        }
                        if (operation == ImageMorphologyOperation::Dilate) {
                            best = std::max(best, value);
                        } else {
                            best = std::min(best, value);
                        }
                    }
                }
                output[row + col * rows + channelOffset] = best;
            }
        }
    }
    return output;
}
//=============================================================================
static std::vector<double>
morphologyData3D(const std::vector<double>& input, indexType rows, indexType cols, indexType planes,
    const NeighborhoodShape& nhood, ImageMorphologyOperation operation, bool logicalInput)
{
    std::vector<double> output(input.size());
    indexType padRows = nhood.rows / 2;
    indexType padCols = nhood.columns / 2;
    indexType padPlanes = nhood.planes / 2;
    double padValue = operation == ImageMorphologyOperation::Dilate
        ? -std::numeric_limits<double>::infinity()
        : (logicalInput ? 0.0 : std::numeric_limits<double>::infinity());
    double initialValue = operation == ImageMorphologyOperation::Dilate
        ? -std::numeric_limits<double>::infinity()
        : std::numeric_limits<double>::infinity();
    for (indexType plane = 0; plane < planes; plane++) {
        for (indexType col = 0; col < cols; col++) {
            for (indexType row = 0; row < rows; row++) {
                double best = initialValue;
                for (indexType mp = 0; mp < nhood.planes; mp++) {
                    for (indexType mc = 0; mc < nhood.columns; mc++) {
                        for (indexType mr = 0; mr < nhood.rows; mr++) {
                            indexType maskIndex
                                = mr + mc * nhood.rows + mp * nhood.rows * nhood.columns;
                            if (!nhood.mask[maskIndex]) {
                                continue;
                            }
                            signedIndexType sourceRow = static_cast<signedIndexType>(row)
                                + static_cast<signedIndexType>(mr)
                                - static_cast<signedIndexType>(padRows);
                            signedIndexType sourceCol = static_cast<signedIndexType>(col)
                                + static_cast<signedIndexType>(mc)
                                - static_cast<signedIndexType>(padCols);
                            signedIndexType sourcePlane = static_cast<signedIndexType>(plane)
                                + static_cast<signedIndexType>(mp)
                                - static_cast<signedIndexType>(padPlanes);
                            double value = padValue;
                            if (sourceRow >= 0 && sourceCol >= 0 && sourcePlane >= 0
                                && sourceRow < static_cast<signedIndexType>(rows)
                                && sourceCol < static_cast<signedIndexType>(cols)
                                && sourcePlane < static_cast<signedIndexType>(planes)) {
                                value = input[static_cast<indexType>(sourceRow)
                                    + static_cast<indexType>(sourceCol) * rows
                                    + static_cast<indexType>(sourcePlane) * rows * cols];
                            }
                            if (operation == ImageMorphologyOperation::Dilate) {
                                best = std::max(best, value);
                            } else {
                                best = std::min(best, value);
                            }
                        }
                    }
                }
                output[row + col * rows + plane * rows * cols] = best;
            }
        }
    }
    return output;
}
//=============================================================================
ArrayOf
ImageMorphology(
    const ArrayOf& image, const ArrayOf& neighborhood, ImageMorphologyOperation operation)
{
    if (image.isComplex()) {
        Error(L"Nelson:image_processing:complexNotSupported", L"%s are not supported.",
            L"Complex images");
    }
    NeighborhoodShape nhood = neighborhoodAsMask(neighborhood);
    bool volumeMorphology = nhood.planes > 1;
    Dimensions outDims;
    std::vector<double> input = realDataAsDouble(image);
    std::vector<double> values;
    if (volumeMorphology) {
        if (!image.isNumeric() && !image.isLogical()) {
            Error(L"Nelson:validators:mustBeNumericOrLogical", L"%s must be numeric or logical.",
                L"Image");
        }
        if (image.isSparse()) {
            Error(L"Nelson:image_processing:sparseImagesNotSupported", L"%s are not supported.",
                L"Sparse images");
        }
        Dimensions dims = image.dimensions();
        if (dims.length() != 3) {
            Error(L"Nelson:immorph:invalidImageSize",
                L"%s must be a 3-D array for 3-D structuring elements.", L"Image");
        }
        indexType rows = dims.rows();
        indexType cols = dims.columns();
        indexType planes = dims.dimensionLength(2);
        outDims = Dimensions(std::vector<indexType> { rows, cols, planes });
        values = morphologyData3D(input, rows, cols, planes, nhood, operation, image.isLogical());
    } else {
        ImageShape2D shape = validateImage2DOrRGB(image, true,
            formatI18nMessage(
                Nelson::resolveMessageTemplate(
                    L"Nelson:validators:mustBeNumericOrLogical", L"%s must be numeric or logical."),
                L"Image"),
            formatI18nMessage(
                Nelson::resolveMessageTemplate(
                    L"Nelson:image_processing:imageDRgbArray", L"%s must be a 2-D or RGB array."),
                L"Image"),
            formatI18nMessage(
                Nelson::resolveMessageTemplate(
                    L"Nelson:image_processing:sparseImagesNotSupported", L"%s are not supported."),
                L"Sparse images"));
        outDims = shape.channels == 1
            ? Dimensions(shape.rows, shape.columns)
            : Dimensions(std::vector<indexType> { shape.rows, shape.columns, shape.channels });
        values = morphologyData(input, shape.rows, shape.columns, shape.channels, nhood.mask,
            nhood.rows, nhood.columns, operation, image.isLogical());
    }
    NelsonType outClass = outputClassFor(image);
    ArrayOf out(outClass, outDims, ArrayOf::allocateArrayOf(outClass, outDims.elementCount()));
    if (image.isEmpty()) {
        return out;
    }
    copyOutput(out, values);
    return out;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
