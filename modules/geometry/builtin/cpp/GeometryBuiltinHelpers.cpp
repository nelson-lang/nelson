//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GeometryBuiltinHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <sstream>
//=============================================================================
namespace Nelson::GeometryGateway {
//=============================================================================
static std::string
wideToAscii(const std::wstring& text)
{
    std::string out;
    out.reserve(text.size());
    for (wchar_t ch : text) {
        out.push_back(static_cast<char>(ch));
    }
    return out;
}
//=============================================================================
Geometry::QhullInput
arrayToQhullInput(const ArrayOf& points)
{
    if (!points.isDoubleType(true) || points.isEmpty()) {
        Error(L"Nelson:geometry:invalidPoints:valueANonemptyRealDoubleMatrix",
            L"%s must be a nonempty real double matrix.", L"Points");
    }
    Geometry::QhullInput input;
    input.pointCount = static_cast<size_t>(points.rows());
    input.dimension = static_cast<size_t>(points.columns());
    if (input.dimension < 2) {
        Error(L"Nelson:geometry:invalidPoints", L"%s must have at least two columns.", L"Points");
    }
    std::vector<double> data = points.asDoubleVector();
    input.coordinates.resize(input.pointCount * input.dimension);
    for (size_t row = 0; row < input.pointCount; ++row) {
        for (size_t col = 0; col < input.dimension; ++col) {
            input.coordinates[row * input.dimension + col] = data[row + col * input.pointCount];
        }
    }
    return input;
}
//=============================================================================
std::string
qhullOptions(const ArrayOfVector& argIn, size_t optionsIndex, const std::string& defaultOptions)
{
    if (argIn.size() <= optionsIndex || argIn[optionsIndex].isEmpty()) {
        return defaultOptions;
    }
    const ArrayOf& options = argIn[optionsIndex];
    if (options.isRowVectorCharacterArray() || options.isScalarStringArray()) {
        return wideToAscii(options.asWideString());
    }
    if (options.isCellArrayOfCharacterVectors()) {
        std::wstring text;
        const ArrayOf* cells = static_cast<const ArrayOf*>(options.dataPointer());
        for (indexType k = 0; k < options.elementCount(); ++k) {
            std::wstring item = cells[k].asWideString();
            if (!item.empty()) {
                if (!text.empty()) {
                    text += L" ";
                }
                text += item;
            }
        }
        return wideToAscii(text);
    }
    Error(L"Nelson:geometry:invalidOptions",
        L"%s must be a character vector or cell array of character vectors.", L"Qhull options");
    return defaultOptions;
}
//=============================================================================
ArrayOf
facetsToArray(const std::vector<std::vector<int>>& facets)
{
    if (facets.empty()) {
        return ArrayOf::empty(0, 0);
    }
    const indexType rows = static_cast<indexType>(facets.size());
    const indexType cols = static_cast<indexType>(facets[0].size());
    ArrayOf out = ArrayOf::doubleMatrix2d(rows, cols);
    auto* data = static_cast<double*>(out.readWriteDataPointer());
    for (indexType r = 0; r < rows; ++r) {
        for (indexType c = 0; c < cols; ++c) {
            data[r + c * rows] = static_cast<double>(facets[r][c]);
        }
    }
    return out;
}
//=============================================================================
ArrayOf
cellsToArray(const std::vector<std::vector<int>>& cells)
{
    ArrayOfMatrix matrix;
    for (const auto& cell : cells) {
        std::vector<double> values;
        values.reserve(cell.size());
        for (int id : cell) {
            values.push_back(static_cast<double>(id));
        }
        ArrayOfVector row;
        row << ArrayOf::doubleRowVector(values);
        matrix.push_back(row);
    }
    return ArrayOf::cellArray(matrix);
}
//=============================================================================
ArrayOf
verticesToArray(const std::vector<double>& vertices, size_t vertexCount, size_t dimension)
{
    ArrayOf out = ArrayOf::doubleMatrix2d(
        static_cast<indexType>(vertexCount), static_cast<indexType>(dimension));
    auto* data = static_cast<double*>(out.readWriteDataPointer());
    for (size_t row = 0; row < vertexCount; ++row) {
        for (size_t col = 0; col < dimension; ++col) {
            data[row + col * vertexCount] = vertices[row * dimension + col];
        }
    }
    return out;
}
//=============================================================================
} // namespace Nelson::GeometryGateway
//=============================================================================
