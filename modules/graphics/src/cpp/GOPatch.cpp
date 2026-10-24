//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <utility>
#include "GOPatch.hpp"
#include "GOAxis.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "RenderInterface.hpp"
#include "GOVectorProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "MinMaxHelpers.hpp"
#include "GOMappingModeProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOBackFaceLightingProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GODataMappingModeProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOEdgeAlphaProperty.hpp"
#include "GOColorInterpProperty.hpp"
#include "GOLightingModeProperty.hpp"
#include "GOFaceAlphaProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOSymbolProperty.hpp"
#include "GORowColumnProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "RGBAColorData.hpp"
#include "RenderHelpers.hpp"
#include "GOColorProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCommonProperties.hpp"
#include "GOFigure.hpp"
#include <algorithm>
#include <atomic>
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::atomic<uint64_t> nextPatchGeometryToken { 1 };
//=============================================================================
static point
patchSubtract(const point& a, const point& b)
{
    return point(a.x - b.x, a.y - b.y, a.z - b.z);
}
//=============================================================================
static point
patchCross(const point& a, const point& b)
{
    return point(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x);
}
//=============================================================================
static point
patchNormalize(point value)
{
    double norm = std::sqrt(value.x * value.x + value.y * value.y + value.z * value.z);
    if (!std::isfinite(norm) || norm <= 0) {
        return point(0, 0, 1);
    }
    value.x /= norm;
    value.y /= norm;
    value.z /= norm;
    return value;
}
//=============================================================================
static point
patchFaceNormal(const Face& face)
{
    if (face.vertices.size() < 3) {
        return point(0, 0, 1);
    }
    return patchNormalize(patchCross(patchSubtract(face.vertices[1], face.vertices[0]),
        patchSubtract(face.vertices[2], face.vertices[0])));
}
//=============================================================================
static enum LightingMode::LightingMode
patchLightingMode(GraphicsObject* object, const std::wstring& propertyName)
{
    if (object->stringCheck(propertyName, GO_PROPERTY_VALUE_FLAT_STR)) {
        return LightingMode::Flat;
    }
    if (object->stringCheck(propertyName, GO_PROPERTY_VALUE_GOURAUD_STR)
        || object->stringCheck(propertyName, GO_PROPERTY_VALUE_PHONG_STR)) {
        // There is no per-pixel Phong path: 'phong' renders with smooth (Gouraud)
        // shading. The stored FaceLighting value stays 'phong' so get() round-trips.
        return LightingMode::Gouraud;
    }
    return LightingMode::None;
}
//=============================================================================
GOPatch::GOPatch()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
GOPatch::~GOPatch() = default;
//=============================================================================
std::wstring
GOPatch::getType()
{
    return GO_PROPERTY_VALUE_PATCH_STR;
}
//=============================================================================
void
GOPatch::updateState()
{
    static const std::vector<std::wstring> rebuildProperties
        = { GO_FACES_PROPERTY_NAME_STR, GO_VERTICES_PROPERTY_NAME_STR,
              GO_FACE_VERTEX_C_DATA_PROPERTY_NAME_STR, GO_FACE_VERTEX_ALPHA_DATA_PROPERTY_NAME_STR,
              GO_C_DATA_PROPERTY_NAME_STR, GO_C_DATA_MAPPING_PROPERTY_NAME_STR,
              GO_C_DATA_MODE_PROPERTY_NAME_STR, GO_FACE_VERTEX_C_DATA_MODE_PROPERTY_NAME_STR,
              GO_ALPHA_DATA_MAPPING_PROPERTY_NAME_STR, GO_FACE_COLOR_PROPERTY_NAME_STR,
              GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_FACE_ALPHA_PROPERTY_NAME_STR,
              GO_EDGE_ALPHA_PROPERTY_NAME_STR, GO_FACE_LIGHTING_PROPERTY_NAME_STR,
              GO_EDGE_LIGHTING_PROPERTY_NAME_STR, GO_BACK_FACE_LIGHTING_PROPERTY_NAME_STR,
              GO_FACE_NORMALS_PROPERTY_NAME_STR, GO_FACE_NORMALS_MODE_PROPERTY_NAME_STR,
              GO_VERTEX_NORMALS_PROPERTY_NAME_STR, GO_VERTEX_NORMALS_MODE_PROPERTY_NAME_STR };

    bool dataChanged = hasChanged(GO_X_DATA_PROPERTY_NAME_STR)
        || hasChanged(GO_Y_DATA_PROPERTY_NAME_STR) || hasChanged(GO_Z_DATA_PROPERTY_NAME_STR);
    bool explicitGeometryChanged
        = hasChanged(GO_FACES_PROPERTY_NAME_STR) || hasChanged(GO_VERTICES_PROPERTY_NAME_STR);
    if (dataChanged && !explicitGeometryChanged && synchronizeDataProperties()) {
        limitsDirty = true;
    }

    if (hasChanged(GO_X_DATA_PROPERTY_NAME_STR)) {
        toManual(GO_X_DATA_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_DATA_PROPERTY_NAME_STR)) {
        toManual(GO_Y_DATA_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_C_DATA_PROPERTY_NAME_STR)) {
        toManual(GO_C_DATA_MODE_PROPERTY_NAME_STR);
        toManual(GO_FACE_VERTEX_C_DATA_MODE_PROPERTY_NAME_STR);
        if (!hasChanged(GO_FACE_VERTEX_C_DATA_PROPERTY_NAME_STR)) {
            GOArrayOfProperty* hp = static_cast<GOArrayOfProperty*>(
                findProperty(GO_FACE_VERTEX_C_DATA_PROPERTY_NAME_STR));
            hp->data(findArrayOfProperty(GO_C_DATA_PROPERTY_NAME_STR));
        }
    }
    if (hasChanged(GO_FACE_VERTEX_C_DATA_PROPERTY_NAME_STR)) {
        toManual(GO_FACE_VERTEX_C_DATA_MODE_PROPERTY_NAME_STR);
    }

    if (m_faces.empty() || dataChanged || hasChanged(rebuildProperties)) {
        limitsDirty = true;
        m_faces.clear();
        buildPolygons(m_faces);
        m_geometryToken = nextPatchGeometryToken.fetch_add(1, std::memory_order_relaxed);
        clearChanged(rebuildProperties);
        clearChanged(GO_X_DATA_PROPERTY_NAME_STR);
        clearChanged(GO_Y_DATA_PROPERTY_NAME_STR);
        clearChanged(GO_Z_DATA_PROPERTY_NAME_STR);
    }
}
//=============================================================================
void
GOPatch::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    double lineWidth(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    PatchRenderOptions options;
    options.lineWidth = lineWidth;
    options.lineStyle = findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR);
    options.lineJoin = findStringProperty(GO_LINE_JOIN_PROPERTY_NAME_STR);
    options.uniqueEdges = getType() == GO_PROPERTY_VALUE_IMPLICIT_FUNCTION_SURFACE_STR;
    options.geometryToken = m_geometryToken;
    if (options.uniqueEdges) {
        options.lineStyle = GO_PROPERTY_VALUE_NONE_STR;
    }
    options.faceLighting = patchLightingMode(this, GO_FACE_LIGHTING_PROPERTY_NAME_STR);
    options.edgeLighting = patchLightingMode(this, GO_EDGE_LIGHTING_PROPERTY_NAME_STR);

    RenderLightingMaterial material;
    material.ambientStrength = findScalarDoubleProperty(GO_AMBIENT_STRENGTH_PROPERTY_NAME_STR);
    material.diffuseStrength = findScalarDoubleProperty(GO_DIFFUSE_STRENGTH_PROPERTY_NAME_STR);
    material.specularStrength = findScalarDoubleProperty(GO_SPECULAR_STRENGTH_PROPERTY_NAME_STR);
    material.specularExponent = findScalarDoubleProperty(GO_SPECULAR_EXPONENT_PROPERTY_NAME_STR);
    material.specularColorReflectance
        = findScalarDoubleProperty(GO_SPECULAR_COLOR_REFLECTANCE_PROPERTY_NAME_STR);
    gc.setLightingMaterial(material);
    gc.drawPatch(m_faces, options);
    paintMarkers(gc, lineWidth);
}
//=============================================================================
std::vector<double>
GOPatch::getLimits()
{
    if (!limitsDirty) {
        return limits;
    }
    limits.resize(0);
    limits.reserve(10);

    ArrayOf vertices(findArrayOfProperty(GO_VERTICES_PROPERTY_NAME_STR));
    vertices.promoteType(NLS_DOUBLE);
    indexType nRows = vertices.rows();

    const double* pVerticesDataX = (const double*)vertices.dataPointer();
    const double* pVerticesDataY = nullptr;
    if (pVerticesDataX) {
        pVerticesDataY = pVerticesDataX + nRows;
    }
    const double* pVertDataZ = nullptr;
    if (pVerticesDataY) {
        pVertDataZ = pVerticesDataY + nRows;
    }
    double Xmax = 1;
    double Xmin = 1;
    double Ymax = 1;
    double Ymin = 1;
    double Zmax = 1;
    double Zmin = 1;

    if (nRows > 0) {
        Xmax = *pVerticesDataX;
        Xmin = Xmax;
        Ymax = *pVerticesDataY;
        Ymin = Ymax;
        if (pVertDataZ) {
            Zmax = *pVertDataZ;
        }
        Zmin = Zmax;

        for (indexType i = 0; i < nRows; i++) {
            Xmax = std::max(Xmax, *pVerticesDataX);
            Xmin = std::min(Xmin, *pVerticesDataX);
            Ymax = std::max(Ymax, *pVerticesDataY);
            Ymin = std::min(Ymin, *pVerticesDataY);
            Zmax = std::max(Zmax, *pVertDataZ);
            Zmin = std::min(Zmin, *pVertDataZ);
            ++pVerticesDataX;
            ++pVerticesDataY;
            ++pVertDataZ;
        }
    }

    ArrayOf faceVertexCData(findArrayOfProperty(GO_FACE_VERTEX_C_DATA_PROPERTY_NAME_STR));
    faceVertexCData.promoteType(NLS_DOUBLE);

    double CLimMin = ArrayOfMin(faceVertexCData);
    double CLimMax = ArrayOfMax(faceVertexCData);

    limits.push_back(Xmin);
    limits.push_back(Xmax);
    limits.push_back(Ymin);
    limits.push_back(Ymax);
    limits.push_back(Zmin);
    limits.push_back(Zmax);
    limits.push_back(CLimMin);
    limits.push_back(CLimMax);
    limits.push_back(1.);
    limits.push_back(1.);
    limitsDirty = false;
    return limits;
}
//=============================================================================
void
GOPatch::constructProperties()
{
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOMappingModeProperty, GO_ALPHA_DATA_MAPPING_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_AMBIENT_STRENGTH_PROPERTY_NAME_STR);
    registerProperty(new GOBackFaceLightingProperty, GO_BACK_FACE_LIGHTING_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_C_DATA_PROPERTY_NAME_STR);
    registerProperty(new GODataMappingModeProperty, GO_C_DATA_MAPPING_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_DIFFUSE_STRENGTH_PROPERTY_NAME_STR);
    registerProperty(new GOEdgeAlphaProperty, GO_EDGE_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOColorInterpProperty, GO_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOLightingModeProperty, GO_EDGE_LIGHTING_PROPERTY_NAME_STR);
    registerProperty(new GOFaceAlphaProperty, GO_FACE_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOColorInterpProperty, GO_FACE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOLightingModeProperty, GO_FACE_LIGHTING_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_FACES_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_FACE_VERTEX_C_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOSymbolProperty, GO_MARKER_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_MARKER_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_SPECULAR_COLOR_REFLECTANCE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_SPECULAR_EXPONENT_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_SPECULAR_STRENGTH_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_VERTEX_NORMALS_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_VERTICES_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_X_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_X_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Y_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_Z_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerCommonProperties(this);
    sortProperties();
}
//=============================================================================
void
GOPatch::setupDefaults()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(
        GO_ALPHA_DATA_MAPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALED_STR);
    setScalarDoubleDefault(GO_AMBIENT_STRENGTH_PROPERTY_NAME_STR, 0.3);
    setRestrictedStringDefault(
        GO_BACK_FACE_LIGHTING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REVERSELIT_STR);
    setRestrictedStringDefault(GO_C_DATA_MAPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALED_STR);
    setScalarDoubleDefault(GO_DIFFUSE_STRENGTH_PROPERTY_NAME_STR, 0.6);
    setScalarDoubleDefault(GO_SPECULAR_COLOR_REFLECTANCE_PROPERTY_NAME_STR, 1);
    setScalarDoubleDefault(GO_SPECULAR_EXPONENT_PROPERTY_NAME_STR, 10);
    setScalarDoubleDefault(GO_SPECULAR_STRENGTH_PROPERTY_NAME_STR, 0.9);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PATCH_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringScalarDefault(
        GO_EDGE_ALPHA_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALAR_STR, 1);
    setRestrictedStringColorDefault(GO_EDGE_COLOR_PROPERTY_NAME_STR,
        GO_PROPERTY_VALUE_COLORSPEC_STR, 0.129411764705882, 0.129411764705882, 0.129411764705882);
    setRestrictedStringDefault(GO_EDGE_LIGHTING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setRestrictedStringScalarDefault(
        GO_FACE_ALPHA_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALAR_STR, 1);
    setRestrictedStringColorDefault(GO_FACE_COLOR_PROPERTY_NAME_STR,
        GO_PROPERTY_VALUE_COLORSPEC_STR, 0.129411764705882, 0.129411764705882, 0.129411764705882);
    setRestrictedStringDefault(GO_FACE_LIGHTING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_MARKER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setRestrictedStringColorDefault(
        GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR, 0, 0, 0);
    setRestrictedStringColorDefault(
        GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR, 0, 0, 0);
    setScalarDoubleDefault(GO_MARKER_SIZE_PROPERTY_NAME_STR, 6);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PATCH_STR);
    setStringDefault(GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setStringDefault(GO_Y_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setStringDefault(GO_DISPLAY_NAME_PROPERTY_NAME_STR, L"");
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    initializeCommonProperties(this);
}
//=============================================================================
inline const double*
getVerticesData(const double* pVertData, indexType i, indexType j, indexType nVertices,
    indexType nbColumnsVerticesData)
{
    return ((i < nVertices && j < nbColumnsVerticesData) ? (pVertData + i + nVertices * j)
                                                         : (pVertData));
}
//=============================================================================
inline const double*
getVerticesColor(const double* pVertColor, indexType i, indexType j,
    indexType nbRowsFaceVertexCdata, indexType nbColumnsFaceVertexCdata)
{
    return ((i < nbRowsFaceVertexCdata && j < nbColumnsFaceVertexCdata)
            ? (pVertColor + i + (int)nbRowsFaceVertexCdata * j)
            : (pVertColor));
}
//=============================================================================
static RGBAColorData
patchMappedColor(GraphicsObject* object, double value, double alpha)
{
    GOAxis* axis = object ? object->getParentAxis() : nullptr;
    std::vector<double> cmap;
    if (axis) {
        cmap = axis->findVectorDoubleProperty(GO_COLOR_MAP_PROPERTY_NAME_STR);
    }
    if (cmap.empty()) {
        GOFigure* figure = object ? object->getParentFigure() : nullptr;
        if (figure) {
            cmap = figure->findVectorDoubleProperty(GO_COLOR_MAP_PROPERTY_NAME_STR);
        }
    }
    size_t length = cmap.size() / 3;
    if (length == 0) {
        return RGBAColorData(0., 0., 0., alpha);
    }

    size_t index = 0;
    if (object
        && object->stringCheck(GO_C_DATA_MAPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_DIRECT_STR)) {
        double directIndex = std::floor(value);
        if (std::isfinite(directIndex)) {
            index = static_cast<size_t>(
                std::clamp(directIndex - 1., 0., static_cast<double>(length - 1)));
        }
    } else {
        std::vector<double> clim = axis ? axis->findVectorDoubleProperty(GO_C_LIM_PROPERTY_NAME_STR)
                                        : std::vector<double> { 0., 1. };
        double low = std::min(clim[0], clim[1]);
        double high = std::max(clim[0], clim[1]);
        if (high > low && std::isfinite(value)) {
            double scaled = (value - low) / (high - low);
            index = static_cast<size_t>(std::clamp(std::floor(scaled * static_cast<double>(length)),
                0., static_cast<double>(length - 1)));
        }
    }
    return RGBAColorData(cmap[index * 3], cmap[index * 3 + 1], cmap[index * 3 + 2], alpha);
}
//=============================================================================
static RGBAColorData
patchVertexColor(GraphicsObject* object, const double* pVertColor, indexType row, double alpha,
    indexType nbRowsFaceVertexCdata, indexType nbColumnsFaceVertexCdata)
{
    if (nbColumnsFaceVertexCdata == 1) {
        double value = *(
            getVerticesColor(pVertColor, row, 0, nbRowsFaceVertexCdata, nbColumnsFaceVertexCdata));
        return patchMappedColor(object, value, alpha);
    }

    double r
        = *(getVerticesColor(pVertColor, row, 0, nbRowsFaceVertexCdata, nbColumnsFaceVertexCdata));
    double g
        = *(getVerticesColor(pVertColor, row, 1, nbRowsFaceVertexCdata, nbColumnsFaceVertexCdata));
    double b
        = *(getVerticesColor(pVertColor, row, 2, nbRowsFaceVertexCdata, nbColumnsFaceVertexCdata));
    return RGBAColorData(r, g, b, alpha);
}
//=============================================================================
bool
GOPatch::synchronizeDataProperties()
{
    ArrayOf xData(findArrayOfProperty(GO_X_DATA_PROPERTY_NAME_STR));
    ArrayOf yData(findArrayOfProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    if (!xData.isNumeric() || !yData.isNumeric() || xData.isEmpty() || yData.isEmpty()) {
        return false;
    }
    xData.promoteType(NLS_DOUBLE);
    yData.promoteType(NLS_DOUBLE);
    ArrayOf zData(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    if (!zData.isEmpty() && !zData.isNumeric()) {
        return false;
    }
    if (!zData.isEmpty()) {
        zData.promoteType(NLS_DOUBLE);
    }

    indexType rows = xData.isVector() ? xData.elementCount() : xData.rows();
    indexType cols = xData.isVector() ? 1 : xData.columns();
    indexType n = rows * cols;
    bool useZData = !zData.isEmpty() && zData.elementCount() == n;
    if (yData.elementCount() != n
        || (!zData.isEmpty() && !useZData && hasChanged(GO_Z_DATA_PROPERTY_NAME_STR))) {
        return false;
    }

    ArrayOf vertices(ArrayOf::doubleMatrix2d(n, 3));
    double* vertexData = static_cast<double*>(vertices.readWriteDataPointer());
    const double* x = static_cast<const double*>(xData.dataPointer());
    const double* y = static_cast<const double*>(yData.dataPointer());
    const double* z = useZData ? static_cast<const double*>(zData.dataPointer()) : nullptr;
    for (indexType i = 0; i < n; ++i) {
        vertexData[i] = x[i];
        vertexData[i + n] = y[i];
        vertexData[i + 2 * n] = z ? z[i] : 1.;
    }

    ArrayOf faces(ArrayOf::doubleMatrix2d(cols, rows));
    double* faceData = static_cast<double*>(faces.readWriteDataPointer());
    for (indexType col = 0; col < cols; ++col) {
        for (indexType row = 0; row < rows; ++row) {
            faceData[col + row * cols] = static_cast<double>(row + col * rows + 1);
        }
    }

    GOArrayOfProperty* verticesProperty
        = static_cast<GOArrayOfProperty*>(findProperty(GO_VERTICES_PROPERTY_NAME_STR));
    verticesProperty->data(vertices);
    GOArrayOfProperty* facesProperty
        = static_cast<GOArrayOfProperty*>(findProperty(GO_FACES_PROPERTY_NAME_STR));
    facesProperty->data(faces);
    return true;
}
//=============================================================================
void
GOPatch::buildPolygons(FaceList& faces)
{
    ArrayOf facesData(findArrayOfProperty(GO_FACES_PROPERTY_NAME_STR));
    facesData.promoteType(NLS_DOUBLE);

    ArrayOf verticesdata(findArrayOfProperty(GO_VERTICES_PROPERTY_NAME_STR));
    verticesdata.promoteType(NLS_DOUBLE);
    indexType nbColumnsVerticesData = verticesdata.columns();
    indexType nVertices = verticesdata.rows();

    ArrayOf faceVertexCdata(findArrayOfProperty(GO_FACE_VERTEX_C_DATA_PROPERTY_NAME_STR));
    faceVertexCdata.promoteType(NLS_DOUBLE);

    ColorMode::ColorMode FaceColorMode = ColorMode::ColorMode::None;
    ColorMode::ColorMode EdgeColorMode = ColorMode::ColorMode::None;

    if (stringCheck(GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR)) {
        FaceColorMode = ColorMode::Flat;
    } else if (stringCheck(GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)) {
        FaceColorMode = ColorMode::None;
    } else if (stringCheck(GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_INTERP_STR)) {
        FaceColorMode = ColorMode::Interp;
    } else {
        FaceColorMode = ColorMode::ColorSpec;
    }

    if (stringCheck(GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR)) {
        EdgeColorMode = ColorMode::Flat;
    } else if (stringCheck(GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)) {
        EdgeColorMode = ColorMode::None;
    } else if (stringCheck(GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_INTERP_STR)) {
        EdgeColorMode = ColorMode::Interp;
    } else {
        EdgeColorMode = ColorMode::ColorSpec;
    }

    if (verticesdata.isEmpty() || facesData.isEmpty()) {
        return;
    }

    if (nbColumnsVerticesData != 3) {
        Error("Nelson:patch:invalidVertices", "Nx3 dimensional matrix expected for 'Vertices'.");
    }

    const double* pVertOrder = static_cast<const double*>(facesData.dataPointer());
    const double* pVertData = static_cast<const double*>(verticesdata.dataPointer());
    const double* pVertColor = static_cast<const double*>(faceVertexCdata.dataPointer());

    GOFaceAlphaProperty* fa
        = static_cast<GOFaceAlphaProperty*>(findProperty(GO_FACE_ALPHA_PROPERTY_NAME_STR));
    GOEdgeAlphaProperty* ea
        = static_cast<GOEdgeAlphaProperty*>(findProperty(GO_EDGE_ALPHA_PROPERTY_NAME_STR));
    double faceAlphaLevel = fa->scalar();
    double edgeAlphaLevel = ea->scalar();

    indexType nbColumnsFaceVertexCdata = faceVertexCdata.columns();
    indexType nbRowsFaceVertexCdata = faceVertexCdata.rows();

    indexType nFaces = facesData.rows();
    indexType maxVertsPerFace = facesData.columns();
    RGBAColorData faceColorSpec;
    if (FaceColorMode == ColorMode::ColorSpec) {
        GORestrictedStringColorProperty* fc = static_cast<GORestrictedStringColorProperty*>(
            findProperty(GO_FACE_COLOR_PROPERTY_NAME_STR));
        if (!fc) {
            Error(L"Nelson:patch:invalidFaceColor", L"Invalid Face Colorspec.");
        }
        std::vector<double> colorspec = fc->colorSpec();
        faceColorSpec = RGBAColorData(colorspec[0], colorspec[1], colorspec[2], faceAlphaLevel);
    }
    RGBAColorData edgeColorSpec;
    if (EdgeColorMode == ColorMode::ColorSpec) {
        GORestrictedStringColorProperty* ec = static_cast<GORestrictedStringColorProperty*>(
            findProperty(GO_EDGE_COLOR_PROPERTY_NAME_STR));
        if (!ec) {
            Error(L"Nelson:patch:invalidEdgeColor", L"Invalid EdgeColor parameter.");
        }
        std::vector<double> colorspec = ec->colorSpec();
        edgeColorSpec = RGBAColorData(colorspec[0], colorspec[1], colorspec[2], edgeAlphaLevel);
    }
    // Flat/Interp face colouring reads FaceVertexCData; with no colour data the
    // lookup would dereference a null buffer. Reject the empty case so that, e.g.,
    // set(h,'FaceColor','interp') on a patch with no CData raises an error.
    if ((FaceColorMode == ColorMode::Flat || FaceColorMode == ColorMode::Interp)
        && faceVertexCdata.isEmpty()) {
        Error(L"Nelson:patch:invalidFaceVertexCDataFaceColor",
            FaceColorMode == ColorMode::Interp
                ? L"Invalid FaceVertexCData parameter with FaceColor to 'interp'."
                : L"Invalid FaceVertexCData parameter with FaceColor to 'flat'.");
    }
    if (EdgeColorMode == ColorMode::Flat && (nbColumnsFaceVertexCdata != 3)
        && ((nbRowsFaceVertexCdata != 1) || ((nbRowsFaceVertexCdata != nVertices)))) {
        Error(L"Nelson:patch:invalidFaceVertexCData",
            L"Invalid FaceVertexCData parameter with EdgeColor to '%s'.", L"flat");
    }
    if (EdgeColorMode == ColorMode::Interp && (nbColumnsFaceVertexCdata != 3)
        && (nbRowsFaceVertexCdata != nVertices)) {
        Error(L"Nelson:patch:invalidFaceVertexCData",
            L"Invalid FaceVertexCData parameter with EdgeColor to '%s'.", L"interp");
    }
    ArrayOf faceNormalsData(findArrayOfProperty(GO_FACE_NORMALS_PROPERTY_NAME_STR));
    bool useFaceNormals
        = stringCheck(GO_FACE_NORMALS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)
        && faceNormalsData.isNumeric() && faceNormalsData.rows() == nFaces
        && faceNormalsData.columns() == 3;
    if (useFaceNormals) {
        faceNormalsData.promoteType(NLS_DOUBLE);
    }
    const double* pFaceNormals
        = useFaceNormals ? static_cast<const double*>(faceNormalsData.dataPointer()) : nullptr;
    ArrayOf vertexNormalsData(findArrayOfProperty(GO_VERTEX_NORMALS_PROPERTY_NAME_STR));
    bool useVertexNormals
        = stringCheck(GO_VERTEX_NORMALS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)
        && vertexNormalsData.isNumeric() && vertexNormalsData.rows() == nVertices
        && vertexNormalsData.columns() == 3;
    if (useVertexNormals) {
        vertexNormalsData.promoteType(NLS_DOUBLE);
    }
    const double* pVertexNormals
        = useVertexNormals ? static_cast<const double*>(vertexNormalsData.dataPointer()) : nullptr;
    bool hasRenderedFaces = FaceColorMode != ColorMode::None;
    bool hasRenderedEdges = EdgeColorMode != ColorMode::None
        && !stringCheck(GO_LINE_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    bool needNormals = useFaceNormals || useVertexNormals
        || (hasRenderedFaces
            && patchLightingMode(this, GO_FACE_LIGHTING_PROPERTY_NAME_STR) != LightingMode::None)
        || (hasRenderedEdges
            && patchLightingMode(this, GO_EDGE_LIGHTING_PROPERTY_NAME_STR) != LightingMode::None);
    faces.reserve(nFaces);
    for (indexType j = 0; j < nFaces; j++) {
        Face face;
        face.FaceColorMode = FaceColorMode;
        face.EdgeColorMode = EdgeColorMode;
        face.vertices.reserve(maxVertsPerFace);
        if (needNormals) {
            face.normals.reserve(maxVertsPerFace);
        }
        std::vector<int> faceVertexIndices;
        if (pVertexNormals) {
            faceVertexIndices.reserve(maxVertsPerFace);
        }
        if (FaceColorMode == ColorMode::ColorSpec) {
            face.FaceColor = faceColorSpec;
        } else if (FaceColorMode == ColorMode::Flat || FaceColorMode == ColorMode::Interp) {
            face.vertexcolors.reserve(maxVertsPerFace);
        }
        if (EdgeColorMode == ColorMode::ColorSpec) {
            face.EdgeColor = edgeColorSpec;
        } else if (EdgeColorMode == ColorMode::Flat || EdgeColorMode == ColorMode::Interp) {
            face.edgecolors.reserve(maxVertsPerFace);
        }
        for (int k = 0; k < maxVertsPerFace; k++) {
            if (!std::isnan(*(pVertOrder + j + k * nFaces))) {
                point vert;
                int vertIndex = (int)(*(pVertOrder + j + k * nFaces)) - 1;

                if (vertIndex >= nVertices || vertIndex < 0) {
                    Error(L"Nelson:patch:vertexIndexOutOfBounds", L"Vertex Index out of bounds.");
                }

                vert.x
                    = *(getVerticesData(pVertData, vertIndex, 0, nVertices, nbColumnsVerticesData));

                vert.y
                    = *(getVerticesData(pVertData, vertIndex, 1, nVertices, nbColumnsVerticesData));

                vert.z
                    = *(getVerticesData(pVertData, vertIndex, 2, nVertices, nbColumnsVerticesData));

                face.vertices.push_back(vert);
                if (pVertexNormals) {
                    faceVertexIndices.push_back(vertIndex);
                }

                double A = faceAlphaLevel;
                if (face.FaceColorMode == ColorMode::Flat) {
                    if (nbRowsFaceVertexCdata == 3) {
                        RGBAColorData vertColor(pVertColor[0], pVertColor[1], pVertColor[2], A);
                        face.vertexcolors.push_back(vertColor);
                    } else {
                        indexType firstVertIndex = (nbRowsFaceVertexCdata != 1)
                            ? (indexType)(*(pVertOrder + j + k * nFaces) - 1)
                            : 0;
                        firstVertIndex = (nbRowsFaceVertexCdata == nFaces) ? j : firstVertIndex;
                        face.vertexcolors.push_back(patchVertexColor(this, pVertColor,
                            firstVertIndex, A, nbRowsFaceVertexCdata, nbColumnsFaceVertexCdata));
                    }
                } else if (face.FaceColorMode == ColorMode::Interp) {
                    face.vertexcolors.push_back(patchVertexColor(this, pVertColor, vertIndex, A,
                        nbRowsFaceVertexCdata, nbColumnsFaceVertexCdata));
                }
                A = edgeAlphaLevel;
                if (face.EdgeColorMode == ColorMode::Flat) {
                    indexType firstVertIndex = (nbRowsFaceVertexCdata != 1)
                        ? (indexType)(*(pVertOrder + j + k * nFaces) - 1)
                        : 0;
                    firstVertIndex = (nbRowsFaceVertexCdata == nFaces) ? j : firstVertIndex;
                    face.edgecolors.push_back(patchVertexColor(this, pVertColor, firstVertIndex, A,
                        nbRowsFaceVertexCdata, nbColumnsFaceVertexCdata));
                } else if (face.EdgeColorMode == ColorMode::Interp) {
                    face.edgecolors.push_back(patchVertexColor(this, pVertColor, vertIndex, A,
                        nbRowsFaceVertexCdata, nbColumnsFaceVertexCdata));
                }
            }
        }
        if (needNormals) {
            point normal = patchFaceNormal(face);
            if (pFaceNormals) {
                normal = patchNormalize(point(*getVerticesData(pFaceNormals, j, 0, nFaces, 3),
                    *getVerticesData(pFaceNormals, j, 1, nFaces, 3),
                    *getVerticesData(pFaceNormals, j, 2, nFaces, 3)));
            }
            for (size_t normalIndex = 0; normalIndex < face.vertices.size(); ++normalIndex) {
                if (pVertexNormals) {
                    int vertexIndex = faceVertexIndices[normalIndex];
                    face.normals.push_back(patchNormalize(
                        point(*getVerticesData(pVertexNormals, vertexIndex, 0, nVertices, 3),
                            *getVerticesData(pVertexNormals, vertexIndex, 1, nVertices, 3),
                            *getVerticesData(pVertexNormals, vertexIndex, 2, nVertices, 3))));
                } else {
                    face.normals.push_back(normal);
                }
            }
        }
        faces.push_back(std::move(face));
    }
}
//=============================================================================
static std::vector<double>
patchMarkerColor(GORestrictedStringColorProperty* property, const RGBAColorData& autoColor)
{
    if (property->isEqual(GO_PROPERTY_VALUE_NONE_STR)) {
        return { -1., -1., -1. };
    }
    if (property->isEqual(GO_PROPERTY_VALUE_AUTO_STR)
        || property->isEqual(GO_PROPERTY_VALUE_FLAT_STR)) {
        return { autoColor.r, autoColor.g, autoColor.b, autoColor.a };
    }
    std::vector<double> color = property->colorSpec();
    if (color.size() == 3) {
        color.push_back(autoColor.a);
    }
    return color;
}
//=============================================================================
void
GOPatch::paintMarkers(RenderInterface& gc, double lineWidth)
{
    RenderInterface::SymbolType marker
        = StringToSymbol(findStringProperty(GO_MARKER_PROPERTY_NAME_STR));
    if (marker == RenderInterface::None) {
        return;
    }

    GORestrictedStringColorProperty* edgeProperty = static_cast<GORestrictedStringColorProperty*>(
        findProperty(GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR));
    GORestrictedStringColorProperty* faceProperty = static_cast<GORestrictedStringColorProperty*>(
        findProperty(GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR));
    double markerSize = findScalarDoubleProperty(GO_MARKER_SIZE_PROPERTY_NAME_STR);
    gc.setupDirectDraw();
    for (const Face& face : m_faces) {
        RGBAColorData autoColor
            = face.FaceColorMode == ColorMode::ColorSpec ? face.FaceColor : face.EdgeColor;
        std::vector<double> edgeColor = patchMarkerColor(edgeProperty, autoColor);
        std::vector<double> faceColor = patchMarkerColor(faceProperty, autoColor);
        for (const point& vertex : face.vertices) {
            if (!std::isfinite(vertex.x) || !std::isfinite(vertex.y) || !std::isfinite(vertex.z)) {
                continue;
            }
            double x = 0;
            double y = 0;
            bool clipped = false;
            gc.toPixels(vertex.x, vertex.y, vertex.z, x, y, clipped);
            if (!clipped) {
                DrawSymbol(gc, marker, x, y, 0, markerSize, edgeColor, faceColor, lineWidth);
            }
        }
    }
    gc.releaseDirectDraw();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
