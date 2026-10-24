//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__patch__Builtin.hpp"
#include "GOAxis.hpp"
#include "GOColorHelpers.hpp"
#include "GOConstructorHelpers.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "GOPatch.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GraphicsObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "StringHelpers.hpp"
#include "axesBuiltin.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static GOAxis*
currentAxis()
{
    if (getCurrentFigure() == -1 || !isVisibleGraphicsHandle(getCurrentFigure())) {
        createNewFigure();
    }
    GOFigure* fig = getCurrentGOFigure();
    int64 current = fig->findGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR);
    if (current == 0 || isDeletedGraphicsObject(current) || !isVisibleGraphicsHandle(current)) {
        ArrayOfVector args;
        axesBuiltin(0, args);
        current = fig->findGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR);
        fig->setRenderingStateInvalid(true);
    }
    return static_cast<GOAxis*>(findGraphicsObject(current));
}
//=============================================================================
static bool
isCharacterScalar(const ArrayOf& value)
{
    return value.isRowVectorCharacterArray() || (value.isStringArray() && value.isScalar());
}
//=============================================================================
static std::wstring
toLower(const std::wstring& value)
{
    std::wstring normalized(value);
    StringHelpers::to_lower(normalized);
    return normalized;
}
//=============================================================================
static bool
isPropertyName(const ArrayOf& value)
{
    if (!isCharacterScalar(value)) {
        return false;
    }
    static const std::vector<std::wstring> names = { L"alignvertexcenters", L"alphadatamapping",
        L"ambientstrength", L"annotation", L"backfacelighting", L"beingdeleted", L"busyaction",
        L"buttondownfcn", L"children", L"clipping", L"contextmenu", L"createfcn", L"cdatamapping",
        L"deletefcn", L"diffusestrength", L"displayname", L"edgealpha", L"edgecolor",
        L"edgelighting", L"facelighting", L"facealpha", L"facecolor", L"handlevisibility",
        L"hittest", L"interruptible", L"linestyle", L"linewidth", L"linejoin", L"marker",
        L"markeredgealpha", L"markeredgecolor", L"markerfacealpha", L"markerfacecolor",
        L"markersize", L"pickableparts", L"selected", L"selectionhighlight", L"seriesindex",
        L"specularcolorreflectance", L"specularexponent", L"specularstrength", L"tag", L"type",
        L"userdata", L"visible" };
    std::wstring normalized = toLower(value.asWideString());
    return std::find(names.begin(), names.end(), normalized) != names.end();
}
//=============================================================================
static bool
isDataOverrideProperty(const ArrayOf& value)
{
    if (!isCharacterScalar(value)) {
        return false;
    }
    std::wstring name = toLower(value.asWideString());
    return name == L"parent" || name == L"xdata" || name == L"ydata" || name == L"zdata"
        || name == L"cdata" || name == L"faces" || name == L"vertices"
        || name == L"facevertexcdata";
}
//=============================================================================
static bool
hasProperty(const ArrayOfVector& values, size_t firstProperty, const std::wstring& name)
{
    for (size_t k = firstProperty; k + 1 < values.size(); k += 2) {
        if (isCharacterScalar(values[k])
            && StringHelpers::iequals(values[k].asWideString(), name)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static std::wstring
propertyValueAsString(const ArrayOfVector& values, size_t firstProperty, const std::wstring& name,
    std::wstring defaultValue)
{
    for (size_t k = firstProperty; k + 1 < values.size(); k += 2) {
        if (isCharacterScalar(values[k])
            && StringHelpers::iequals(values[k].asWideString(), name)) {
            return toLower(values[k + 1].asWideString());
        }
    }
    return defaultValue;
}
//=============================================================================
static void
appendProperty(ArrayOfVector& values, const std::wstring& name, const ArrayOf& value)
{
    values << ArrayOf::characterArray(name);
    values << value;
}
//=============================================================================
static ArrayOf
makeDoubleMatrix(indexType rows, indexType columns, const std::vector<double>& values)
{
    double* data = static_cast<double*>(ArrayOf::allocateArrayOf(
        NLS_DOUBLE, static_cast<indexType>(values.size()), stringVector(), false));
    std::copy(values.begin(), values.end(), data);
    return ArrayOf(NLS_DOUBLE, Dimensions(rows, columns), data);
}
//=============================================================================
static ArrayOf
makeDoubleColumn(const std::vector<double>& values)
{
    return makeDoubleMatrix(static_cast<indexType>(values.size()), 1, values);
}
//=============================================================================
static ArrayOf
makeRgbRows(const std::vector<double>& rgbRows)
{
    indexType rows = static_cast<indexType>(rgbRows.size() / 3);
    std::vector<double> values(rgbRows.size());
    for (indexType i = 0; i < rows; i++) {
        for (indexType j = 0; j < 3; j++) {
            values[i + j * rows] = rgbRows[3 * i + j];
        }
    }
    return makeDoubleMatrix(rows, 3, values);
}
//=============================================================================
static std::vector<double>
columnizedVector(const ArrayOf& value)
{
    return value.asDoubleVector();
}
//=============================================================================
static ArrayOf
normalizedDataArray(const ArrayOf& value)
{
    if (value.isVector()) {
        return makeDoubleColumn(columnizedVector(value));
    }
    return value;
}
//=============================================================================
static void
checkMatchingDataSizes(const ArrayOf& x, const ArrayOf& y, const ArrayOf& z)
{
    if (!x.is2D() || !y.is2D() || !z.is2D() || x.rows() != y.rows() || x.columns() != y.columns()
        || x.rows() != z.rows() || x.columns() != z.columns()) {
        Error("Nelson:dimagree:sizeDataMismatch", "Size of data mismatch.");
    }
}
//=============================================================================
static std::vector<double>
defaultZData(indexType rows, indexType columns)
{
    return std::vector<double>(static_cast<size_t>(rows * columns), 1.);
}
//=============================================================================
static ArrayOf
closedTriangleData(const ArrayOf& data)
{
    if (!data.isVector() || data.elementCount() != 3) {
        return data;
    }
    std::vector<double> values = data.asDoubleVector();
    values.insert(values.begin(), values[0]);
    return makeDoubleColumn(values);
}
//=============================================================================
static ArrayOf
makeFaces(const ArrayOf& x)
{
    indexType rows = x.rows();
    indexType columns = x.columns();
    indexType faceRows = (rows == 1) ? 1 : columns;
    indexType faceColumns = (rows == 1) ? columns : rows;
    std::vector<double> values(static_cast<size_t>(faceRows * faceColumns));
    for (indexType faceColumn = 0; faceColumn < faceColumns; faceColumn++) {
        for (indexType faceRow = 0; faceRow < faceRows; faceRow++) {
            double index = (rows == 1) ? static_cast<double>(faceColumn + 1)
                                       : static_cast<double>(faceColumn + faceRow * rows + 1);
            values[faceRow + faceColumn * faceRows] = index;
        }
    }
    return makeDoubleMatrix(faceRows, faceColumns, values);
}
//=============================================================================
static ArrayOf
makeVertices(const ArrayOf& x, const ArrayOf& y, const ArrayOf& z)
{
    indexType count = x.elementCount();
    std::vector<double> xValues = x.asDoubleVector();
    std::vector<double> yValues = y.asDoubleVector();
    std::vector<double> zValues = z.asDoubleVector();
    std::vector<double> vertices(static_cast<size_t>(count * 3));
    for (indexType k = 0; k < count; k++) {
        vertices[k] = xValues[k];
        vertices[k + count] = yValues[k];
        vertices[k + static_cast<indexType>(2) * count] = zValues[k];
    }
    return makeDoubleMatrix(count, 3, vertices);
}
//=============================================================================
static bool
hasVaryingZ(const ArrayOf& z)
{
    std::vector<double> values = z.asDoubleVector();
    if (values.empty()) {
        return false;
    }
    for (double value : values) {
        if (value != values[0]) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static double
wrapScaledIndex(double value, double low, double high, indexType nbColors)
{
    double span = high - low;
    if (span == 0.) {
        span = 1.;
    }
    double index = std::ceil((value - low) / span * static_cast<double>(nbColors));
    if (index > nbColors) {
        index = std::fmod(index, static_cast<double>(nbColors)) + 1.;
    }
    if (index < 1.) {
        index = 1.;
    }
    if (index > nbColors) {
        index = static_cast<double>(nbColors);
    }
    return index;
}
//=============================================================================
static std::vector<double>
mapScalarsToColors(GOAxis* axis, const std::vector<double>& scalars, bool setCLimFromValues)
{
    std::vector<double> colormap = axis->findVectorDoubleProperty(GO_COLOR_MAP_PROPERTY_NAME_STR);
    if (colormap.empty()) {
        colormap = { 0, 0, 1, 0, 1, 0, 1, 0, 0 };
    }
    indexType nbColors = static_cast<indexType>(colormap.size() / 3);
    std::vector<double> clim = axis->findVectorDoubleProperty(GO_C_LIM_PROPERTY_NAME_STR);
    if (setCLimFromValues && !scalars.empty()) {
        auto [minIt, maxIt] = std::minmax_element(scalars.begin(), scalars.end());
        double minValue = *minIt;
        double maxValue = *maxIt;
        if (minValue == maxValue) {
            maxValue = minValue + 1.;
        }
        axis->setTwoVectorDefault(GO_C_LIM_PROPERTY_NAME_STR, minValue, maxValue);
        axis->toManual(GO_C_LIM_MODE_PROPERTY_NAME_STR);
        clim = { minValue, maxValue };
    }
    if (clim.size() < 2) {
        clim = { 0., 1. };
    }
    std::vector<double> colors;
    colors.reserve(scalars.size() * 3);
    for (double scalar : scalars) {
        indexType index
            = static_cast<indexType>(wrapScaledIndex(scalar, clim[0], clim[1], nbColors));
        size_t base = static_cast<size_t>((index - 1) * 3);
        colors.push_back(colormap[base]);
        colors.push_back(colormap[base + 1]);
        colors.push_back(colormap[base + 2]);
    }
    return colors;
}
//=============================================================================
static ArrayOf
normalizeCDataForVectorPatch(GOAxis* axis, const ArrayOf& cData, const std::wstring& cDataMapping)
{
    if (!cData.isNumeric()) {
        return cData;
    }
    if (!cData.isVector() || cData.elementCount() == 3) {
        return cData;
    }
    if (cDataMapping == GO_PROPERTY_VALUE_DIRECT_STR) {
        return cData;
    }
    if (cDataMapping != GO_PROPERTY_VALUE_SCALED_STR) {
        Error("Nelson:patch:invalidCDataMapping", "CDataMapping scaled or direct value expected.");
    }
    std::vector<double> colors = mapScalarsToColors(axis, cData.asDoubleVector(), false);
    return makeRgbRows(colors);
}
//=============================================================================
static std::vector<double>
meanColumns(const ArrayOf& values)
{
    indexType rows = values.rows();
    indexType columns = values.columns();
    std::vector<double> data = values.asDoubleVector();
    std::vector<double> means(static_cast<size_t>(columns), 0.);
    for (indexType column = 0; column < columns; column++) {
        double sum = 0.;
        for (indexType row = 0; row < rows; row++) {
            sum += data[row + column * rows];
        }
        means[column] = sum / static_cast<double>(rows);
    }
    return means;
}
//=============================================================================
static std::vector<double>
faceScalarValues(const ArrayOf& x, const ArrayOf& faces, const ArrayOf& cData)
{
    if (cData.isVector()) {
        std::vector<double> c = cData.asDoubleVector();
        std::vector<double> f = faces.asDoubleVector();
        indexType faceRows = faces.rows();
        indexType faceColumns = faces.columns();
        std::vector<double> values(static_cast<size_t>(faceRows), 0.);
        for (indexType row = 0; row < faceRows; row++) {
            double sum = 0.;
            for (indexType column = 0; column < faceColumns; column++) {
                indexType idx = static_cast<indexType>(f[row + column * faceRows]) - 1;
                sum += c[idx];
            }
            values[row] = sum / static_cast<double>(faceColumns);
        }
        return values;
    }
    if (cData.rows() == x.rows() && cData.columns() == x.columns()) {
        return meanColumns(cData);
    }
    return meanColumns(cData);
}
//=============================================================================
static ArrayOf
resolve3DNumericFlatColors(
    GOAxis* axis, const ArrayOf& x, const ArrayOf& faces, const ArrayOf& cData)
{
    if (cData.is2D() && cData.columns() == 3 && cData.rows() == faces.rows()) {
        return cData;
    }
    std::vector<double> values = faceScalarValues(x, faces, cData);
    return makeRgbRows(mapScalarsToColors(axis, values, true));
}
//=============================================================================
static bool
parseExplicitFaceColor(const ArrayOf& value, ArrayOf& faceColor)
{
    if (!isCharacterScalar(value)
        && !(value.isNumeric() && value.isVector() && value.elementCount() == 3)) {
        return false;
    }
    if (isCharacterScalar(value) && isPropertyName(value)) {
        return false;
    }
    if (isCharacterScalar(value)) {
        std::wstring color(value.asWideString());
        if (StringHelpers::iequals(color, GO_PROPERTY_VALUE_NONE_STR)) {
            faceColor = ArrayOf::characterArray(GO_PROPERTY_VALUE_NONE_STR);
            return true;
        }
    }
    std::vector<double> rgb;
    if (!ParseColorToRGB(value, rgb)) {
        return false;
    }
    faceColor = ArrayOf::doubleRowVector(rgb);
    return true;
}
//=============================================================================
static bool
parsePatchSignature(const ArrayOfVector& argIn, size_t& firstProperty, int& dataCount,
    bool& explicitFaceColor, ArrayOf& explicitFaceColorValue)
{
    firstProperty = argIn.size();
    dataCount = 0;
    explicitFaceColor = false;
    if (argIn.size() < 2 || !argIn[0].isNumeric() || !argIn[1].isNumeric()) {
        return false;
    }
    if (argIn.size() >= 3) {
        if (parseExplicitFaceColor(argIn[2], explicitFaceColorValue)) {
            explicitFaceColor = true;
            dataCount = 2;
            firstProperty = 3;
            return ((argIn.size() - firstProperty) % 2) == 0;
        }
        if (isPropertyName(argIn[2])) {
            return false;
        }
        dataCount = 3;
        firstProperty = 3;
    }
    if (argIn.size() >= 4) {
        if (parseExplicitFaceColor(argIn[3], explicitFaceColorValue)) {
            explicitFaceColor = true;
            dataCount = 3;
            firstProperty = 4;
            return ((argIn.size() - firstProperty) % 2) == 0;
        }
        if (isPropertyName(argIn[3])) {
            dataCount = 3;
            firstProperty = 3;
            return ((argIn.size() - firstProperty) % 2) == 0;
        }
        dataCount = 4;
        firstProperty = 4;
    }
    return dataCount == 3 || dataCount == 4;
}
//=============================================================================
static void
appendUserProperties(
    ArrayOfVector& constructorArgs, const ArrayOfVector& argIn, size_t firstProperty)
{
    for (size_t k = firstProperty; k + 1 < argIn.size(); k += 2) {
        if (!isPropertyName(argIn[k]) || isDataOverrideProperty(argIn[k])) {
            Error("Nelson:patch:unsupportedFastPath", "Unsupported patch fast path arguments.");
        }
        constructorArgs << argIn[k];
        constructorArgs << argIn[k + 1];
    }
}
//=============================================================================
static ArrayOfVector
patchFastBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 1);

    GOAxis* axis = currentAxis();
    size_t firstProperty = 0;
    int dataCount = 0;
    bool explicitFaceColor = false;
    ArrayOf explicitFaceColorValue;
    if (!parsePatchSignature(
            argIn, firstProperty, dataCount, explicitFaceColor, explicitFaceColorValue)) {
        Error("Nelson:patch:unsupportedFastPath", "Unsupported patch fast path arguments.");
    }

    ArrayOf xData = normalizedDataArray(argIn[0]);
    ArrayOf yData = normalizedDataArray(argIn[1]);
    ArrayOf zData;
    ArrayOf cData;
    bool haveCData = false;
    if (dataCount == 3 && explicitFaceColor) {
        zData = normalizedDataArray(argIn[2]);
    } else if (dataCount == 3) {
        cData = argIn[2];
        haveCData = true;
    } else if (dataCount == 4) {
        zData = normalizedDataArray(argIn[2]);
        cData = argIn[3];
        haveCData = true;
    }
    if (zData.isEmpty()) {
        zData = makeDoubleMatrix(
            xData.rows(), xData.columns(), defaultZData(xData.rows(), xData.columns()));
    }
    checkMatchingDataSizes(xData, yData, zData);

    ArrayOf faces = makeFaces(xData);
    ArrayOf vertices = makeVertices(xData, yData, zData);
    ArrayOf faceVertexCData;
    ArrayOf faceColor;
    std::wstring cDataMapping = propertyValueAsString(
        argIn, firstProperty, GO_C_DATA_MAPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALED_STR);

    if (explicitFaceColor) {
        faceVertexCData = axis->findProperty(GO_COLOR_PROPERTY_NAME_STR)->get();
        faceColor = explicitFaceColorValue;
    } else if (haveCData) {
        if (cData.isNumeric() && hasVaryingZ(zData)
            && !hasProperty(argIn, firstProperty, GO_FACE_COLOR_PROPERTY_NAME_STR)) {
            faceVertexCData = resolve3DNumericFlatColors(axis, xData, faces, cData);
        } else {
            faceVertexCData = normalizeCDataForVectorPatch(axis, cData, cDataMapping);
        }
        faceColor = ArrayOf::characterArray(GO_PROPERTY_VALUE_FLAT_STR);
    } else {
        faceVertexCData = axis->findProperty(GO_COLOR_PROPERTY_NAME_STR)->get();
        faceColor = ArrayOf::doubleRowVector(std::vector<double> { 0., 0., 0. });
    }

    ArrayOfVector constructorArgs;
    appendProperty(constructorArgs, GO_X_DATA_PROPERTY_NAME_STR, closedTriangleData(xData));
    appendProperty(constructorArgs, GO_Y_DATA_PROPERTY_NAME_STR, closedTriangleData(yData));
    appendProperty(constructorArgs, GO_Z_DATA_PROPERTY_NAME_STR, closedTriangleData(zData));
    appendProperty(
        constructorArgs, GO_C_DATA_PROPERTY_NAME_STR, haveCData ? cData : faceVertexCData);
    appendProperty(constructorArgs, GO_FACES_PROPERTY_NAME_STR, faces);
    appendProperty(constructorArgs, GO_VERTICES_PROPERTY_NAME_STR, vertices);
    appendProperty(constructorArgs, GO_FACE_VERTEX_C_DATA_PROPERTY_NAME_STR, faceVertexCData);
    appendProperty(constructorArgs, GO_FACE_COLOR_PROPERTY_NAME_STR, faceColor);
    appendProperty(constructorArgs, GO_EDGE_COLOR_PROPERTY_NAME_STR,
        ArrayOf::doubleRowVector(
            std::vector<double> { 0.129411764705882, 0.129411764705882, 0.129411764705882 }));
    appendProperty(constructorArgs, GO_FACE_ALPHA_PROPERTY_NAME_STR, ArrayOf::doubleScalar(1.));
    appendUserProperties(constructorArgs, argIn, firstProperty);

    ArrayOfVector retval;
    retval << graphicsObjectHandle(GOCommonConstructorHelper(new GOPatch, constructorArgs));
    return retval;
}
//=============================================================================
ArrayOfVector
__patch__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    size_t firstProperty = 0;
    int dataCount = 0;
    bool explicitFaceColor = false;
    ArrayOf explicitFaceColorValue;
    if (argIn.size() >= 3 && argIn[0].isNumeric() && argIn[1].isNumeric()
        && parsePatchSignature(
            argIn, firstProperty, dataCount, explicitFaceColor, explicitFaceColorValue)) {
        return patchFastBuiltin(nLhs, argIn);
    }

    ArrayOfVector retval;
    retval << graphicsObjectHandle(GOCommonConstructorHelper(new GOPatch, argIn));
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
