//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOSurface.hpp"
#include "GOAxis.hpp"
#include "GOLightingHelpers.hpp"
#include "GOBackFaceLightingProperty.hpp"
#include "GOFaceAlphaProperty.hpp"
#include "GOColorInterpProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOLightingModeProperty.hpp"
#include "GORowColumnProperty.hpp"
#include "GODataMappingModeProperty.hpp"
#include "GOMappingModeProperty.hpp"
#include "GOSymbolProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GORestrictedStringColorProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOEdgeAlphaProperty.hpp"
#include "GORestrictedStringScalarProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "MinMaxHelpers.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCommonProperties.hpp"
#include <algorithm>
#include <atomic>
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::atomic<uint64_t> nextSurfaceGeometryToken { 1 };
//=============================================================================
namespace {
    struct SurfaceRgba
    {
        unsigned char r;
        unsigned char g;
        unsigned char b;
        unsigned char a;
    };
    //=============================================================================
    // Surface FaceColor/EdgeColor also accept 'texturemap': the CData image (any
    // size, independent of Z) is mapped onto the surface. Patch and contour keep the
    // plain interp dictionary, so the value stays scoped to surfaces.
    const wchar_t* SURFACE_COLOR_DICT[6]
        = { GO_PROPERTY_VALUE_NONE_STR, GO_PROPERTY_VALUE_FLAT_STR, GO_PROPERTY_VALUE_INTERP_STR,
              GO_PROPERTY_VALUE_TEXTUREMAP_STR, GO_PROPERTY_VALUE_COLORSPEC_STR, 0 };
    //=============================================================================
    class GOSurfaceColorProperty : public GORestrictedStringColorProperty
    {
    public:
        GOSurfaceColorProperty() : GORestrictedStringColorProperty(SURFACE_COLOR_DICT) { }
        ~GOSurfaceColorProperty() override = default;
    };
    //=============================================================================
} // namespace
//=============================================================================
static point
surfaceCross(const point& a, const point& b)
{
    return point(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x);
}
//=============================================================================
static point
surfaceSubtract(const point& a, const point& b)
{
    return point(a.x - b.x, a.y - b.y, a.z - b.z);
}
//=============================================================================
static point
surfaceNormalize(point value)
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
static bool
surfaceFinitePoint(const double* xdp, const double* ydp, const double* zdp, indexType idx)
{
    return std::isfinite(xdp[idx]) && std::isfinite(ydp[idx]) && std::isfinite(zdp[idx]);
}
//=============================================================================
static point
surfaceColoredPointToPoint(const coloredPoint& value)
{
    return point(value.x, value.y, value.z);
}
//=============================================================================
static double
surfaceDot(const point& a, const point& b)
{
    return a.x * b.x + a.y * b.y + a.z * b.z;
}
//=============================================================================
static point
surfaceQuadStripNormal(const std::vector<coloredPoint>& strip, size_t start)
{
    point p0 = surfaceColoredPointToPoint(strip[start]);
    point p1 = surfaceColoredPointToPoint(strip[start + 1]);
    point p2 = surfaceColoredPointToPoint(strip[start + 3]);
    return surfaceNormalize(surfaceCross(surfaceSubtract(p1, p0), surfaceSubtract(p2, p0)));
}
//=============================================================================
static void
appendCoplanarQuadStripRuns(
    std::vector<std::vector<coloredPoint>>& strips, const std::vector<coloredPoint>& source)
{
    if (source.size() < 4) {
        return;
    }
    size_t quadCount = (source.size() - 2) / 2;
    if (quadCount <= 1) {
        strips.push_back(source);
        return;
    }

    constexpr double coplanarDotLimit = 0.85;
    size_t runStartQuad = 0;
    point previousNormal = surfaceQuadStripNormal(source, 0);
    for (size_t quad = 1; quad < quadCount; ++quad) {
        point normal = surfaceQuadStripNormal(source, 2 * quad);
        if (surfaceDot(previousNormal, normal) < coplanarDotLimit) {
            size_t start = 2 * runStartQuad;
            size_t end = 2 * (quad - 1) + 4;
            strips.emplace_back(source.begin() + start, source.begin() + end);
            runStartQuad = quad;
        }
        previousNormal = normal;
    }

    size_t start = 2 * runStartQuad;
    strips.emplace_back(source.begin() + start, source.end());
}
//=============================================================================
static enum LightingMode::LightingMode
surfaceLightingMode(GraphicsObject* object, const std::wstring& propertyName)
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
static bool
surfaceAxisNeedsCoordinateMap(GOAxis* axis)
{
    return axis
        && (!axis->stringCheck(GO_X_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR)
            || !axis->stringCheck(GO_Y_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR)
            || !axis->stringCheck(GO_Z_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR)
            || !axis->stringCheck(GO_X_SCALE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LINEAR_STR)
            || !axis->stringCheck(GO_Y_SCALE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LINEAR_STR)
            || !axis->stringCheck(GO_Z_SCALE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LINEAR_STR));
}
//=============================================================================
static std::vector<point>
surfaceVertexNormals(
    const double* xdp, const double* ydp, const double* zdp, indexType rows, indexType cols)
{
    std::vector<point> normals(rows * cols);
    if (rows < 2 || cols < 2) {
        std::fill(normals.begin(), normals.end(), point(0, 0, 1));
        return normals;
    }
    for (indexType row = 0; row < rows; ++row) {
        for (indexType col = 0; col < cols; ++col) {
            indexType center = row + col * rows;
            indexType left = row + (col > 0 ? col - 1 : col) * rows;
            indexType right = row + (col + 1 < cols ? col + 1 : col) * rows;
            indexType down = (row > 0 ? row - 1 : row) + col * rows;
            indexType up = (row + 1 < rows ? row + 1 : row) + col * rows;
            if (!surfaceFinitePoint(xdp, ydp, zdp, center)
                || !surfaceFinitePoint(xdp, ydp, zdp, left)
                || !surfaceFinitePoint(xdp, ydp, zdp, right)
                || !surfaceFinitePoint(xdp, ydp, zdp, down)
                || !surfaceFinitePoint(xdp, ydp, zdp, up)) {
                normals[center] = point(0, 0, 1);
                continue;
            }
            point tangentCol(
                xdp[right] - xdp[left], ydp[right] - ydp[left], zdp[right] - zdp[left]);
            point tangentRow(xdp[up] - xdp[down], ydp[up] - ydp[down], zdp[up] - zdp[down]);
            normals[center] = surfaceNormalize(surfaceCross(tangentCol, tangentRow));
        }
    }
    return normals;
}
//=============================================================================
static bool
surfaceGridPoint(const ArrayOf& xdata, const ArrayOf& ydata, const double* zdp, indexType rows,
    indexType cols, indexType r, indexType c, point& p)
{
    const double* xdp = static_cast<const double*>(xdata.dataPointer());
    const double* ydp = static_cast<const double*>(ydata.dataPointer());
    double x;
    double y;
    if (xdata.isVector()) {
        x = xdp[c];
    } else {
        x = xdp[r + c * rows];
    }
    if (ydata.isVector()) {
        y = ydp[r];
    } else {
        y = ydp[r + c * rows];
    }
    double z = zdp[r + c * rows];
    p = point(x, y, z);
    return std::isfinite(x) && std::isfinite(y) && std::isfinite(z);
}
//=============================================================================
static void
updateAutoSurfaceNormals(GraphicsObject* go)
{
    bool vertexAuto = go->isAuto(GO_VERTEX_NORMALS_MODE_PROPERTY_NAME_STR);
    bool faceAuto = go->isAuto(GO_FACE_NORMALS_MODE_PROPERTY_NAME_STR);
    if (!vertexAuto && !faceAuto) {
        return;
    }
    ArrayOf zdata(go->findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    ArrayOf xdata(go->findArrayOfProperty(GO_X_DATA_PROPERTY_NAME_STR));
    ArrayOf ydata(go->findArrayOfProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    if (!zdata.isNumeric() || !xdata.isNumeric() || !ydata.isNumeric() || zdata.isEmpty()) {
        return;
    }
    zdata.promoteType(NLS_DOUBLE);
    xdata.promoteType(NLS_DOUBLE);
    ydata.promoteType(NLS_DOUBLE);
    indexType rows = zdata.rows();
    indexType cols = zdata.columns();
    if (rows < 2 || cols < 2) {
        return;
    }
    bool xOk = xdata.isVector() ? (xdata.elementCount() == cols)
                                : (xdata.rows() == rows && xdata.columns() == cols);
    bool yOk = ydata.isVector() ? (ydata.elementCount() == rows)
                                : (ydata.rows() == rows && ydata.columns() == cols);
    if (!xOk || !yOk) {
        return;
    }
    const double* zdp = static_cast<const double*>(zdata.dataPointer());
    indexType fr = rows - 1;
    indexType fc = cols - 1;
    std::vector<point> faceNormals(fr * fc, point(0, 0, 0));
    for (indexType r = 0; r < fr; ++r) {
        for (indexType c = 0; c < fc; ++c) {
            point p11, p21, p12, p22;
            bool ok = surfaceGridPoint(xdata, ydata, zdp, rows, cols, r, c, p11)
                && surfaceGridPoint(xdata, ydata, zdp, rows, cols, r, c + 1, p21)
                && surfaceGridPoint(xdata, ydata, zdp, rows, cols, r + 1, c, p12)
                && surfaceGridPoint(xdata, ydata, zdp, rows, cols, r + 1, c + 1, p22);
            if (!ok) {
                continue;
            }
            point diag1(p22.x - p11.x, p22.y - p11.y, p22.z - p11.z);
            point diag2(p12.x - p21.x, p12.y - p21.y, p12.z - p21.z);
            faceNormals[r + c * fr] = surfaceNormalize(surfaceCross(diag1, diag2));
        }
    }
    if (faceAuto) {
        Dimensions dims(std::vector<indexType> { fr, fc, 3 });
        double* fp
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dims.elementCount()));
        ArrayOf faceArray(NLS_DOUBLE, dims, fp);
        indexType plane = fr * fc;
        for (indexType k = 0; k < plane; ++k) {
            fp[k] = faceNormals[k].x;
            fp[k + plane] = faceNormals[k].y;
            fp[k + 2 * plane] = faceNormals[k].z;
        }
        GOArrayOfProperty* prop = static_cast<GOArrayOfProperty*>(
            go->findProperty(GO_FACE_NORMALS_PROPERTY_NAME_STR, false));
        if (prop) {
            prop->data(faceArray);
            prop->clearModified();
        }
    }
    if (vertexAuto) {
        Dimensions dims(std::vector<indexType> { rows, cols, 3 });
        double* vp
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dims.elementCount()));
        ArrayOf vertexArray(NLS_DOUBLE, dims, vp);
        indexType plane = rows * cols;
        for (indexType r = 0; r < rows; ++r) {
            for (indexType c = 0; c < cols; ++c) {
                point sum(0, 0, 0);
                for (indexType dr = 0; dr < 2; ++dr) {
                    for (indexType dc = 0; dc < 2; ++dc) {
                        if (r + dr < 1 || c + dc < 1 || r + dr > fr || c + dc > fc) {
                            continue;
                        }
                        const point& n = faceNormals[(r + dr - 1) + (c + dc - 1) * fr];
                        sum.x += n.x;
                        sum.y += n.y;
                        sum.z += n.z;
                    }
                }
                point n = surfaceNormalize(sum);
                indexType idx = r + c * rows;
                vp[idx] = n.x;
                vp[idx + plane] = n.y;
                vp[idx + 2 * plane] = n.z;
            }
        }
        GOArrayOfProperty* prop = static_cast<GOArrayOfProperty*>(
            go->findProperty(GO_VERTEX_NORMALS_PROPERTY_NAME_STR, false));
        if (prop) {
            prop->data(vertexArray);
            prop->clearModified();
        }
    }
}
//=============================================================================
static std::vector<point>
surfaceNormals(GraphicsObject* object, const double* xdp, const double* ydp, const double* zdp,
    indexType rows, indexType cols)
{
    std::vector<point> normals = surfaceVertexNormals(xdp, ydp, zdp, rows, cols);
    if (!object->stringCheck(
            GO_VERTEX_NORMALS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)) {
        return normals;
    }
    ArrayOf normalData(object->findArrayOfProperty(GO_VERTEX_NORMALS_PROPERTY_NAME_STR));
    if (!normalData.isNumeric() || normalData.rows() != rows || normalData.columns() != cols
        || normalData.dimensions().length() != 3 || normalData.dimensionLength(2) != 3) {
        return normals;
    }
    normalData.promoteType(NLS_DOUBLE);
    const double* ndp = static_cast<const double*>(normalData.dataPointer());
    indexType elementCount = rows * cols;
    for (indexType k = 0; k < elementCount; ++k) {
        normals[k]
            = surfaceNormalize(point(ndp[k], ndp[k + elementCount], ndp[k + 2 * elementCount]));
    }
    return normals;
}
//=============================================================================
std::wstring
GOSurface::getType()
{
    return GO_PROPERTY_VALUE_SURFACE_STR;
}
//=============================================================================
GOSurface::GOSurface()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
GOSurface::~GOSurface() { }
//=============================================================================
void
GOSurface::constructProperties()
{
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_ALPHA_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOMappingModeProperty, GO_ALPHA_DATA_MAPPING_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_AMBIENT_STRENGTH_PROPERTY_NAME_STR);
    registerProperty(new GOBackFaceLightingProperty, GO_BACK_FACE_LIGHTING_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_C_DATA_PROPERTY_NAME_STR);
    registerProperty(new GODataMappingModeProperty, GO_C_DATA_MAPPING_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_C_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_DIFFUSE_STRENGTH_PROPERTY_NAME_STR);
    registerProperty(new GOEdgeAlphaProperty, GO_EDGE_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOSurfaceColorProperty, GO_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOLightingModeProperty, GO_EDGE_LIGHTING_PROPERTY_NAME_STR);
    registerProperty(new GOFaceAlphaProperty, GO_FACE_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOSurfaceColorProperty, GO_FACE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOLightingModeProperty, GO_FACE_LIGHTING_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOSymbolProperty, GO_MARKER_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_MARKER_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GORowColumnProperty, GO_MESH_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_SPECULAR_COLOR_REFLECTANCE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_SPECULAR_EXPONENT_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_SPECULAR_STRENGTH_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_VERTEX_NORMALS_PROPERTY_NAME_STR);
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
GOSurface::setupDefaults()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    GOVectorProperty* hp = (GOVectorProperty*)findProperty(GO_ALPHA_DATA_PROPERTY_NAME_STR);
    std::vector<double> gp;
    gp.push_back(1.0);
    hp->data(gp);
    setRestrictedStringDefault(
        GO_ALPHA_DATA_MAPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALED_STR);
    setScalarDoubleDefault(GO_AMBIENT_STRENGTH_PROPERTY_NAME_STR, 0.3);
    setRestrictedStringDefault(
        GO_BACK_FACE_LIGHTING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REVERSELIT_STR);
    setRestrictedStringDefault(GO_C_DATA_MAPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALED_STR);
    setRestrictedStringDefault(GO_C_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_DIFFUSE_STRENGTH_PROPERTY_NAME_STR, 0.6);
    setScalarDoubleDefault(GO_SPECULAR_COLOR_REFLECTANCE_PROPERTY_NAME_STR, 1);
    setScalarDoubleDefault(GO_SPECULAR_EXPONENT_PROPERTY_NAME_STR, 10);
    setScalarDoubleDefault(GO_SPECULAR_STRENGTH_PROPERTY_NAME_STR, 0.9);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SURFACE_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringScalarDefault(
        GO_EDGE_ALPHA_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALAR_STR, 1);
    setRestrictedStringColorDefault(
        GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_COLORSPEC_STR, 0, 0, 0);
    setRestrictedStringDefault(GO_EDGE_LIGHTING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setRestrictedStringScalarDefault(
        GO_FACE_ALPHA_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALAR_STR, 1);
    setRestrictedStringDefault(GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR);
    setRestrictedStringDefault(GO_FACE_LIGHTING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_MARKER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setRestrictedStringColorDefault(
        GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR, 0, 0, 0);
    setRestrictedStringColorDefault(
        GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR, 0, 0, 0);
    setScalarDoubleDefault(GO_MARKER_SIZE_PROPERTY_NAME_STR, 6);
    setRestrictedStringDefault(GO_MESH_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOTH_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setStringDefault(GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setStringDefault(GO_Y_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    initializeCommonProperties(this);
}
//=============================================================================
std::vector<std::vector<coloredPoint>>
GOSurface::buildQuadsNoTexMap(
    GORestrictedStringColorProperty* cp, GORestrictedStringScalarProperty* ap)
{
    std::vector<std::vector<coloredPoint>> retval;
    ArrayOf xdata(getCoordinateMatrix(GO_X_DATA_PROPERTY_NAME_STR, true));
    ArrayOf ydata(getCoordinateMatrix(GO_Y_DATA_PROPERTY_NAME_STR, false));
    ydata.promoteType(NLS_DOUBLE);
    ArrayOf zdata(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    zdata.promoteType(NLS_DOUBLE);
    if ((xdata.elementCount() != zdata.elementCount())
        || (xdata.elementCount() != ydata.elementCount())) {
        return retval;
    }
    if (zdata.isEmpty()) {
        return retval;
    }
    double* xdp = (double*)xdata.dataPointer();
    double* ydp = (double*)ydata.dataPointer();
    double* zdp = (double*)zdata.dataPointer();
    indexType rows = zdata.rows();
    indexType cols = zdata.columns();
    indexType elementCount = zdata.elementCount();
    std::vector<double> mappedX;
    std::vector<double> mappedY;
    std::vector<double> mappedZ;
    GOAxis* axis = getParentAxis();
    if (surfaceAxisNeedsCoordinateMap(axis)) {
        std::vector<double> xs(xdp, xdp + elementCount);
        std::vector<double> ys(ydp, ydp + elementCount);
        std::vector<double> zs(zdp, zdp + elementCount);
        axis->reMap(xs, ys, zs, mappedX, mappedY, mappedZ);
        xdp = mappedX.data();
        ydp = mappedY.data();
        zdp = mappedZ.data();
    }
    bool needsNormals = axisHasVisibleLights(axis);
    std::vector<point> normals = needsNormals ? surfaceNormals(this, xdp, ydp, zdp, rows, cols)
                                              : std::vector<point>(elementCount, point(0, 0, 1));
    bool colorInterp = cp->isEqual(GO_PROPERTY_VALUE_INTERP_STR)
        || cp->isEqual(GO_PROPERTY_VALUE_TEXTUREMAP_STR);
    bool alphaInterp = ap->isEqual(GO_PROPERTY_VALUE_INTERP_STR)
        || ap->isEqual(GO_PROPERTY_VALUE_TEXTUREMAP_STR);
    if (colorInterp && ((imgHeight < rows) || (imgWidth < cols))) {
        return retval;
    }
    if (alphaInterp && ((imgHeight < rows) || (imgWidth < cols))) {
        return retval;
    }
    if (cp->isEqual(GO_PROPERTY_VALUE_FLAT_STR)
        && ((imgHeight < rows - 1) || (imgWidth < cols - 1))) {
        return retval;
    }
    if (ap->isEqual(GO_PROPERTY_VALUE_FLAT_STR)
        && ((imgHeight < rows - 1) || (imgWidth < cols - 1))) {
        return retval;
    }
    if (cp->isEqual(GO_PROPERTY_VALUE_NONE_STR)) {
        return retval;
    }
    SurfaceRgba* dummyline = nullptr;
    if (cp->isEqual(GO_PROPERTY_VALUE_COLORSPEC_STR) || ap->isEqual(GO_PROPERTY_VALUE_SCALAR_STR)) {
        dummyline = new SurfaceRgba[cols];
        double r = 0;
        double g = 0;
        double b = 0;
        double alphaval = 1.0;
        if (ap->isEqual(GO_PROPERTY_VALUE_SCALAR_STR)) {
            alphaval = ap->scalar();
        }
        if (cp->isEqual(GO_PROPERTY_VALUE_COLORSPEC_STR)) {
            std::vector<double> p(cp->colorSpec());
            if (p[0] == -1) {
                delete[] dummyline;
                return retval;
            }
            r = p[0];
            g = p[1];
            b = p[2];
        }
        for (indexType i = 0; i < cols; i++) {
            dummyline[i] = SurfaceRgba { (unsigned char)(255 * r), (unsigned char)(255 * g),
                (unsigned char)(255 * b), (unsigned char)(255 * alphaval) };
        }
    }
    bool faceFlat = cp->isEqual(GO_PROPERTY_VALUE_FLAT_STR);
    bool alphaFlat = ap->isEqual(GO_PROPERTY_VALUE_FLAT_STR);
    bool hasVisibleEdges = !stringCheck(GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)
        && !stringCheck(GO_LINE_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    bool surfaceUsesCellQuads
        = stringCheck(GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR)
        || stringCheck(GO_FACE_ALPHA_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR);
    if (faceFlat || alphaFlat || (surfaceUsesCellQuads && hasVisibleEdges)) {
        std::vector<coloredPoint> packedQuads;
        if (!hasVisibleEdges) {
            packedQuads.reserve(static_cast<size_t>(rows - 1) * static_cast<size_t>(cols - 1) * 4);
        }
        auto scanPixel = [this](int row, int col) -> SurfaceRgba {
            const unsigned char* p = imgRgba.data() + ((size_t)row * imgWidth + col) * 4;
            return SurfaceRgba { p[0], p[1], p[2], p[3] };
        };
        auto vertexColor
            = [&](int faceRow, int faceCol, int vertexRow, int vertexCol) -> SurfaceRgba {
            if (colorInterp) {
                return scanPixel(vertexRow, vertexCol);
            }
            if (faceFlat) {
                return scanPixel(faceRow, faceCol);
            }
            if (cp->isEqual(GO_PROPERTY_VALUE_COLORSPEC_STR)) {
                return dummyline ? dummyline[0] : SurfaceRgba { 0, 0, 0, 255 };
            }
            return SurfaceRgba { 0, 0, 0, 255 };
        };
        auto vertexAlpha
            = [&](int faceRow, int faceCol, int vertexRow, int vertexCol) -> SurfaceRgba {
            if (alphaInterp) {
                return scanPixel(vertexRow, vertexCol);
            }
            if (alphaFlat) {
                return scanPixel(faceRow, faceCol);
            }
            if (ap->isEqual(GO_PROPERTY_VALUE_SCALAR_STR) && dummyline != nullptr) {
                return dummyline[0];
            }
            return SurfaceRgba { 0, 0, 0, 255 };
        };
        for (int i = 0; i < rows - 1; i++) {
            for (int j = 0; j < cols - 1; j++) {
                indexType idx00 = i + j * rows;
                indexType idx10 = i + 1 + j * rows;
                indexType idx01 = i + (j + 1) * rows;
                indexType idx11 = i + 1 + (j + 1) * rows;
                if (!surfaceFinitePoint(xdp, ydp, zdp, idx00)
                    || !surfaceFinitePoint(xdp, ydp, zdp, idx10)
                    || !surfaceFinitePoint(xdp, ydp, zdp, idx01)
                    || !surfaceFinitePoint(xdp, ydp, zdp, idx11)) {
                    continue;
                }
                SurfaceRgba color00 = vertexColor(i, j, i, j);
                SurfaceRgba color10 = vertexColor(i, j, i + 1, j);
                SurfaceRgba color01 = vertexColor(i, j, i, j + 1);
                SurfaceRgba color11 = vertexColor(i, j, i + 1, j + 1);
                SurfaceRgba alpha00 = vertexAlpha(i, j, i, j);
                SurfaceRgba alpha10 = vertexAlpha(i, j, i + 1, j);
                SurfaceRgba alpha01 = vertexAlpha(i, j, i, j + 1);
                SurfaceRgba alpha11 = vertexAlpha(i, j, i + 1, j + 1);
                std::vector<coloredPoint> quad;
                if (hasVisibleEdges) {
                    quad.reserve(4);
                }
                auto appendQuadPoint = [&](coloredPoint&& p) {
                    hasVisibleEdges ? quad.push_back(std::move(p))
                                    : packedQuads.push_back(std::move(p));
                };
                appendQuadPoint(coloredPoint(xdp[idx00], ydp[idx00], zdp[idx00], color00.r / 255.0,
                    color00.g / 255.0, color00.b / 255.0, alpha00.a / 255.0, normals[idx00].x,
                    normals[idx00].y, normals[idx00].z));
                appendQuadPoint(coloredPoint(xdp[idx10], ydp[idx10], zdp[idx10], color10.r / 255.0,
                    color10.g / 255.0, color10.b / 255.0, alpha10.a / 255.0, normals[idx10].x,
                    normals[idx10].y, normals[idx10].z));
                appendQuadPoint(coloredPoint(xdp[idx01], ydp[idx01], zdp[idx01], color01.r / 255.0,
                    color01.g / 255.0, color01.b / 255.0, alpha01.a / 255.0, normals[idx01].x,
                    normals[idx01].y, normals[idx01].z));
                appendQuadPoint(coloredPoint(xdp[idx11], ydp[idx11], zdp[idx11], color11.r / 255.0,
                    color11.g / 255.0, color11.b / 255.0, alpha11.a / 255.0, normals[idx11].x,
                    normals[idx11].y, normals[idx11].z));
                if (hasVisibleEdges) {
                    retval.push_back(std::move(quad));
                }
            }
        }
        if (!packedQuads.empty()) {
            retval.push_back(std::move(packedQuads));
        }
        if (cp->isEqual(GO_PROPERTY_VALUE_COLORSPEC_STR)
            || ap->isEqual(GO_PROPERTY_VALUE_SCALAR_STR)) {
            delete[] dummyline;
        }
        return retval;
    }
    for (int i = 0; i < rows - 1; i++) {
        const SurfaceRgba *cbits1 = nullptr, *cbits2 = nullptr, *abits1 = nullptr,
                          *abits2 = nullptr;
        auto scanLineRgba = [this](int row) -> const SurfaceRgba* {
            return reinterpret_cast<const SurfaceRgba*>(
                imgRgba.data() + (size_t)row * imgWidth * 4);
        };
        int col_lim, alp_lim;
        if (colorInterp) {
            cbits1 = scanLineRgba(i);
            cbits2 = scanLineRgba(i + 1);
            col_lim = (int)(cols - 1);
        } else if (cp->isEqual(GO_PROPERTY_VALUE_FLAT_STR)) {
            cbits1 = scanLineRgba(i);
            cbits2 = scanLineRgba(i);
            col_lim = (int)(cols - 2);
        } else if (cp->isEqual(GO_PROPERTY_VALUE_COLORSPEC_STR)) {
            cbits1 = dummyline;
            cbits2 = dummyline;
            col_lim = (int)(cols - 1);
        }
        if (alphaInterp) {
            abits1 = scanLineRgba(i);
            abits2 = scanLineRgba(i + 1);
            alp_lim = (int)(cols - 1);
        } else if (ap->isEqual(GO_PROPERTY_VALUE_FLAT_STR)) {
            abits1 = scanLineRgba(i);
            abits2 = scanLineRgba(i);
            alp_lim = (int)(cols - 2);
        } else if (ap->isEqual(GO_PROPERTY_VALUE_SCALAR_STR)) {
            if (dummyline != nullptr) {
                abits1 = dummyline;
                abits2 = dummyline;
            }
            alp_lim = (int)(cols - 1);
        }
        std::vector<coloredPoint> linequads;
        auto flushLineQuads = [&retval](std::vector<coloredPoint>& quads) {
            if (quads.size() >= 4) {
                appendCoplanarQuadStripRuns(retval, quads);
            }
            quads.clear();
        };
        for (int j = 0; j < cols; j++) {
            int ccol = std::min(j, col_lim);
            int acol = std::min(j, alp_lim);
            if (cbits1 && cbits2 && abits2 && abits1) {
                indexType idx1 = i + j * rows;
                indexType idx2 = i + 1 + j * rows;
                if (!surfaceFinitePoint(xdp, ydp, zdp, idx1)
                    || !surfaceFinitePoint(xdp, ydp, zdp, idx2)) {
                    flushLineQuads(linequads);
                    continue;
                }
                linequads.push_back(coloredPoint(xdp[idx1], ydp[idx1], zdp[idx1],
                    cbits1[ccol].r / 255.0, cbits1[ccol].g / 255.0, cbits1[ccol].b / 255.0,
                    abits1[acol].a / 255.0, normals[idx1].x, normals[idx1].y, normals[idx1].z));
                linequads.push_back(coloredPoint(xdp[idx2], ydp[idx2], zdp[idx2],
                    cbits2[ccol].r / 255.0, cbits2[ccol].g / 255.0, cbits2[ccol].b / 255.0,
                    abits2[acol].a / 255.0, normals[idx2].x, normals[idx2].y, normals[idx2].z));
            }
        }
        flushLineQuads(linequads);
    }
    if (cp->isEqual(GO_PROPERTY_VALUE_COLORSPEC_STR) || ap->isEqual(GO_PROPERTY_VALUE_SCALAR_STR)) {
        delete[] dummyline;
    }
    return retval;
}
//=============================================================================
ArrayOf
GOSurface::getCoordinateMatrix(const std::wstring& propertyName, bool isXCoord)
{
    ArrayOf ZData(findArrayOfProperty(L"ZData"));
    indexType zRows = ZData.rows();
    indexType zCols = ZData.columns();
    if (stringCheck(propertyName + L"Mode", L"manual")) {
        ArrayOf CData(findArrayOfProperty(propertyName));
        if (CData.isVector()
            && ((isXCoord && (CData.elementCount() == zCols))
                || (!isXCoord && (CData.elementCount() == zRows)))) {
            CData.promoteType(NLS_DOUBLE);
            const double* qp = (const double*)CData.dataPointer();
            Dimensions dimsZ(zRows, zCols);
            double* dp = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsZ.elementCount());
            ArrayOf mat = ArrayOf(NLS_DOUBLE, dimsZ, dp);
            for (indexType i = 0; i < zCols; i++) {
                for (indexType j = 0; j < zRows; j++) {
                    if (isXCoord) {
                        *dp = qp[i];
                    } else {
                        *dp = qp[j];
                    }
                    dp++;
                }
            }
            return mat;
        } else if (CData.is2D() && (CData.rows() == zRows) && (CData.columns() == zCols)) {
            return CData;
        }
    }
    Dimensions dimsZ(zRows, zCols);
    double* dp = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsZ.elementCount());
    ArrayOf mat = ArrayOf(NLS_DOUBLE, dimsZ, dp);
    for (indexType i = 0; i < zCols; i++) {
        for (indexType j = 0; j < zRows; j++) {
            if (isXCoord) {
                *dp = static_cast<double>(i + 1);
            } else {
                *dp = static_cast<double>(j + 1);
            }
            dp++;
        }
    }
    return mat;
}
//=============================================================================
void
GOSurface::sampleTexel(double u, double v, double& r, double& g, double& b) const
{
    if (imgRgba.empty() || imgWidth < 1 || imgHeight < 1) {
        r = g = b = 1.0;
        return;
    }
    double fx = std::max(0.0, std::min(1.0, u)) * static_cast<double>(imgWidth - 1);
    double fy = std::max(0.0, std::min(1.0, v)) * static_cast<double>(imgHeight - 1);
    int x0 = static_cast<int>(std::floor(fx));
    int y0 = static_cast<int>(std::floor(fy));
    int x1 = std::min(x0 + 1, imgWidth - 1);
    int y1 = std::min(y0 + 1, imgHeight - 1);
    double wx = fx - x0;
    double wy = fy - y0;
    const unsigned char* p00 = imgRgba.data() + (static_cast<size_t>(y0) * imgWidth + x0) * 4;
    const unsigned char* p01 = imgRgba.data() + (static_cast<size_t>(y0) * imgWidth + x1) * 4;
    const unsigned char* p10 = imgRgba.data() + (static_cast<size_t>(y1) * imgWidth + x0) * 4;
    const unsigned char* p11 = imgRgba.data() + (static_cast<size_t>(y1) * imgWidth + x1) * 4;
    double out[3];
    for (int k = 0; k < 3; ++k) {
        double top = p00[k] * (1.0 - wx) + p01[k] * wx;
        double bottom = p10[k] * (1.0 - wx) + p11[k] * wx;
        out[k] = (top * (1.0 - wy) + bottom * wy) / 255.0;
    }
    r = out[0];
    g = out[1];
    b = out[2];
}
//=============================================================================
std::vector<std::vector<coloredPoint>>
GOSurface::buildTexturedQuads(bool bakeColor)
{
    // 'texturemap' FaceColor: the full-resolution CData image (imgRgba) is bound on
    // the renderer and sampled per fragment, so the texture stays crisp whatever the
    // Z grid. Only the geometry is emitted here: one quad strip per grid row, each
    // vertex with its normalized texture coordinate (u, v) and a white base colour.
    // The renderer multiplies the texel by that colour, so baked Gouraud lighting
    // modulates the texture. When bakeColor is true (the backend cannot sample the
    // bound texture) the vertex colour is the texel sampled at (u, v) instead.
    std::vector<std::vector<coloredPoint>> retval;
    ArrayOf xdata(getCoordinateMatrix(GO_X_DATA_PROPERTY_NAME_STR, true));
    ArrayOf ydata(getCoordinateMatrix(GO_Y_DATA_PROPERTY_NAME_STR, false));
    ydata.promoteType(NLS_DOUBLE);
    ArrayOf zdata(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    zdata.promoteType(NLS_DOUBLE);
    if ((xdata.elementCount() != zdata.elementCount())
        || (xdata.elementCount() != ydata.elementCount()) || zdata.isEmpty()) {
        return retval;
    }
    double* xdp = (double*)xdata.dataPointer();
    double* ydp = (double*)ydata.dataPointer();
    double* zdp = (double*)zdata.dataPointer();
    indexType rows = zdata.rows();
    indexType cols = zdata.columns();
    indexType elementCount = zdata.elementCount();
    if (rows < 2 || cols < 2) {
        return retval;
    }
    std::vector<double> mappedX;
    std::vector<double> mappedY;
    std::vector<double> mappedZ;
    GOAxis* axis = getParentAxis();
    if (surfaceAxisNeedsCoordinateMap(axis)) {
        std::vector<double> xs(xdp, xdp + elementCount);
        std::vector<double> ys(ydp, ydp + elementCount);
        std::vector<double> zs(zdp, zdp + elementCount);
        axis->reMap(xs, ys, zs, mappedX, mappedY, mappedZ);
        xdp = mappedX.data();
        ydp = mappedY.data();
        zdp = mappedZ.data();
    }
    bool needsNormals = axisHasVisibleLights(axis);
    std::vector<point> normals = needsNormals ? surfaceNormals(this, xdp, ydp, zdp, rows, cols)
                                              : std::vector<point>(elementCount, point(0, 0, 1));
    double faceAlpha = 1.0;
    GORestrictedStringScalarProperty* ap = static_cast<GORestrictedStringScalarProperty*>(
        findProperty(GO_FACE_ALPHA_PROPERTY_NAME_STR));
    if (ap && ap->isEqual(GO_PROPERTY_VALUE_SCALAR_STR)) {
        faceAlpha = ap->scalar();
    }
    auto texCoord = [](indexType index, indexType count) -> double {
        return (count > 1) ? static_cast<double>(index) / static_cast<double>(count - 1) : 0.0;
    };
    retval.reserve(rows - 1);
    for (indexType i = 0; i < rows - 1; i++) {
        std::vector<coloredPoint> strip;
        strip.reserve(static_cast<size_t>(cols) * 2);
        double v1 = texCoord(i, rows);
        double v2 = texCoord(i + 1, rows);
        for (indexType j = 0; j < cols; j++) {
            double u = texCoord(j, cols);
            indexType idx1 = i + j * rows;
            indexType idx2 = i + 1 + j * rows;
            if (!surfaceFinitePoint(xdp, ydp, zdp, idx1)
                || !surfaceFinitePoint(xdp, ydp, zdp, idx2)) {
                if (strip.size() >= 4) {
                    retval.push_back(std::move(strip));
                }
                strip.clear();
                continue;
            }
            double r1 = 1.0, g1 = 1.0, b1 = 1.0, r2 = 1.0, g2 = 1.0, b2 = 1.0;
            if (bakeColor) {
                sampleTexel(u, v1, r1, g1, b1);
                sampleTexel(u, v2, r2, g2, b2);
            }
            strip.push_back(coloredPoint(xdp[idx1], ydp[idx1], zdp[idx1], r1, g1, b1, faceAlpha,
                normals[idx1].x, normals[idx1].y, normals[idx1].z, u, v1));
            strip.push_back(coloredPoint(xdp[idx2], ydp[idx2], zdp[idx2], r2, g2, b2, faceAlpha,
                normals[idx2].x, normals[idx2].y, normals[idx2].z, u, v2));
        }
        if (strip.size() >= 4) {
            retval.push_back(std::move(strip));
        }
    }
    return retval;
}
//=============================================================================
RGBAImageView
GOSurface::textureView() const
{
    RGBAImageView view;
    view.width = imgWidth;
    view.height = imgHeight;
    view.bytesPerLine = imgWidth * 4;
    view.pixels = imgRgba.data();
    view.resourceKey = imageResourceKey;
    view.contentRevision = imageRevision;
    view.opaque = imageOpaque;
    return view;
}
//=============================================================================
void
GOSurface::updateState()
{
    GOAxis* ax = getParentAxis();
    GOFigure* fig = getParentFigure();
    bool geometryChanged = m_quadsDirty;
    geometryChanged = geometryChanged || hasChanged(GO_X_DATA_PROPERTY_NAME_STR)
        || hasChanged(GO_Y_DATA_PROPERTY_NAME_STR) || hasChanged(GO_Z_DATA_PROPERTY_NAME_STR)
        || hasChanged(GO_X_DATA_MODE_PROPERTY_NAME_STR)
        || hasChanged(GO_Y_DATA_MODE_PROPERTY_NAME_STR) || hasChanged(GO_C_DATA_PROPERTY_NAME_STR)
        || hasChanged(GO_C_DATA_MAPPING_PROPERTY_NAME_STR)
        || hasChanged(GO_C_DATA_MODE_PROPERTY_NAME_STR)
        || hasChanged(GO_ALPHA_DATA_PROPERTY_NAME_STR)
        || hasChanged(GO_ALPHA_DATA_MAPPING_PROPERTY_NAME_STR)
        || hasChanged(GO_FACE_COLOR_PROPERTY_NAME_STR)
        || hasChanged(GO_EDGE_COLOR_PROPERTY_NAME_STR)
        || hasChanged(GO_FACE_ALPHA_PROPERTY_NAME_STR)
        || hasChanged(GO_EDGE_ALPHA_PROPERTY_NAME_STR)
        || hasChanged(GO_FACE_LIGHTING_PROPERTY_NAME_STR)
        || hasChanged(GO_EDGE_LIGHTING_PROPERTY_NAME_STR)
        || hasChanged(GO_MESH_STYLE_PROPERTY_NAME_STR)
        || hasChanged(GO_LINE_STYLE_PROPERTY_NAME_STR)
        || hasChanged(GO_VERTEX_NORMALS_PROPERTY_NAME_STR)
        || hasChanged(GO_VERTEX_NORMALS_MODE_PROPERTY_NAME_STR);
    if (ax) {
        geometryChanged = geometryChanged || ax->hasChanged(GO_X_SCALE_PROPERTY_NAME_STR)
            || ax->hasChanged(GO_Y_SCALE_PROPERTY_NAME_STR)
            || ax->hasChanged(GO_Z_SCALE_PROPERTY_NAME_STR)
            || ax->hasChanged(GO_X_DIR_PROPERTY_NAME_STR)
            || ax->hasChanged(GO_Y_DIR_PROPERTY_NAME_STR)
            || ax->hasChanged(GO_Z_DIR_PROPERTY_NAME_STR);
    }
    bool updateImage = hasChanged(GO_C_DATA_PROPERTY_NAME_STR)
        || hasChanged(GO_C_DATA_MAPPING_PROPERTY_NAME_STR)
        || hasChanged(GO_ALPHA_DATA_PROPERTY_NAME_STR);
    if (ax) {
        updateImage = updateImage || ax->hasChanged(GO_C_LIM_PROPERTY_NAME_STR)
            || ax->hasChanged(GO_COLOR_MAP_PROPERTY_NAME_STR);
    }
    if (fig) {
        updateImage = updateImage || fig->hasChanged(GO_COLOR_MAP_PROPERTY_NAME_STR);
    }
    // Auto CLim is published through setTwoVectorDefault, which leaves the CLim
    // changed flag down. ax->hasChanged() cannot catch a CLim widened by a sibling
    // added after this surface (the contour of meshc/surfc), so compare the current
    // CLim with the one the cached color image was built with.
    if (ax) {
        std::vector<double> currentClim(ax->findVectorDoubleProperty(GO_C_LIM_PROPERTY_NAME_STR));
        if (currentClim.size() >= 2
            && (!m_lastClimValid || currentClim[0] != m_lastClimLo
                || currentClim[1] != m_lastClimHi)) {
            updateImage = true;
        }
    }
    if (hasChanged(GO_X_DATA_PROPERTY_NAME_STR)) {
        limitsDirty = true;
        toManual(GO_X_DATA_MODE_PROPERTY_NAME_STR);
        clearChanged(GO_X_DATA_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_DATA_PROPERTY_NAME_STR)) {
        limitsDirty = true;
        toManual(GO_Y_DATA_MODE_PROPERTY_NAME_STR);
        clearChanged(GO_Y_DATA_PROPERTY_NAME_STR);
    }
    bool zDataChanged = hasChanged(GO_Z_DATA_PROPERTY_NAME_STR);
    if ((zDataChanged && isAuto(GO_C_DATA_MODE_PROPERTY_NAME_STR))
        || hasChanged(GO_C_DATA_MODE_PROPERTY_NAME_STR)) {
        updateImage = true;
    }
    if (zDataChanged || hasChanged(GO_ALPHA_DATA_PROPERTY_NAME_STR)) {
        limitsDirty = true;
    }
    if (hasChanged(GO_C_DATA_PROPERTY_NAME_STR)) {
        limitsDirty = true;
        toManual(GO_C_DATA_MODE_PROPERTY_NAME_STR);
        clearChanged(GO_C_DATA_PROPERTY_NAME_STR);
    }
    if (isAuto(GO_X_DATA_MODE_PROPERTY_NAME_STR)) {
        autoXMode();
        clearChanged(GO_X_DATA_MODE_PROPERTY_NAME_STR);
    }
    if (isAuto(GO_Y_DATA_MODE_PROPERTY_NAME_STR)) {
        autoYMode();
        clearChanged(GO_Y_DATA_MODE_PROPERTY_NAME_STR);
    }
    if (isAuto(GO_C_DATA_MODE_PROPERTY_NAME_STR)) {
        // Re-derive the auto CData from ZData only when an input changed. Doing it
        // on every paint recomputed the colour texture and bumped the geometry token,
        // which defeated the mesh geometry caches and forced a full re-encode on each
        // cosmetic edit.
        if (zDataChanged || hasChanged(GO_C_DATA_MODE_PROPERTY_NAME_STR)) {
            autoCMode();
            updateImage = true;
        }
        clearChanged(GO_C_DATA_MODE_PROPERTY_NAME_STR);
    }
    if (updateImage) {
        updateCAlphadata();
        // Remember the CLim the texture was rasterized with, to detect a later
        // sibling-driven CLim change.
        if (ax) {
            std::vector<double> usedClim(ax->findVectorDoubleProperty(GO_C_LIM_PROPERTY_NAME_STR));
            if (usedClim.size() >= 2) {
                m_lastClimLo = usedClim[0];
                m_lastClimHi = usedClim[1];
                m_lastClimValid = true;
            }
        }
        // Quad vertex colors are sampled from imgRgba, so the cached quads are stale
        // once the color/alpha texture is recomputed (colormap, CLim, CData...).
        geometryChanged = true;
    }
    // A visible light needs vertex normals in the cached quads but does not
    // invalidate them by itself. Rebuild only when the geometry changed or when the
    // light presence flipped (quads built without normals must be rebuilt with them,
    // and vice versa). Rebuilding on every paint forced static lit surfaces through
    // a full rebuild per animation frame.
    bool lightsVisible = axisHasVisibleLights(getParentAxis());
    if (lightsVisible != m_quadsBuiltWithLights) {
        m_quadsBuiltWithLights = lightsVisible;
        geometryChanged = true;
    }
    if (lightsVisible && geometryChanged) {
        updateAutoSurfaceNormals(this);
    }
    if (geometryChanged) {
        m_quadsDirty = true;
        m_geometryToken = nextSurfaceGeometryToken.fetch_add(1, std::memory_order_relaxed);
    }
    clearAllChanged();
}
//=============================================================================
void
GOSurface::paintMe(RenderInterface& gc)
{
    updateState();
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    ArrayOf xdata(findArrayOfProperty(GO_X_DATA_PROPERTY_NAME_STR));
    xdata.promoteType(NLS_DOUBLE);
    ArrayOf ydata(findArrayOfProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    ydata.promoteType(NLS_DOUBLE);
    ArrayOf zdata(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    if (!zdata.isNumeric()) {
        return;
    }
    zdata.promoteType(NLS_DOUBLE);
    if (zdata.isEmpty()) {
        return;
    }
    GORestrictedStringScalarProperty* meshStyleProperty
        = static_cast<GORestrictedStringScalarProperty*>(
            findProperty(GO_MESH_STYLE_PROPERTY_NAME_STR));
    meshStyle meshstyle = meshStyle::Both;
    if (meshStyleProperty->isEqual(GO_PROPERTY_VALUE_BOTH_STR)) {
        meshstyle = meshStyle::Both;
    } else if (meshStyleProperty->isEqual(GO_PROPERTY_VALUE_ROW_STR)) {
        meshstyle = meshStyle::Row;
    } else {
        meshstyle = meshStyle::Column;
    }
    std::wstring lineStyle
        = ((GORestrictedStringColorProperty*)findProperty(GO_LINE_STYLE_PROPERTY_NAME_STR))->data();

    if (stringCheck(GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)) {
        lineStyle = GO_PROPERTY_VALUE_NONE_STR;
    }
    // 'texturemap' FaceColor binds the full-resolution CData as a texture sampled
    // per fragment; the face quads only carry geometry and texture coordinates.
    const bool textured
        = stringCheck(GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEXTUREMAP_STR)
        && !imgRgba.empty();
    // Only a backend that samples the bound texture gets the white+UV quads. Other
    // backends get the texel colour baked per vertex (grid-sampled).
    const bool crispTexture = textured && gc.supportsSurfaceTexture();
    // Several renderers paint the same surface in one pass (the window and the
    // getframe / saveas capture) and share the quad cache. A texture-sampling
    // backend needs white + UV quads, a non-sampling one needs the texel baked per
    // vertex, so rebuild whenever the cached build does not match the texture mode
    // this backend requires.
    const int textureMode = !textured ? 0 : (crispTexture ? 1 : 2);
    if (textured && textureMode != m_quadsTextureMode) {
        m_quadsDirty = true;
    }
    m_quadsTextureMode = textureMode;
    if (m_quadsDirty) {
        if (textured) {
            m_surfaceQuads = buildTexturedQuads(!crispTexture);
        } else {
            m_surfaceQuads = buildQuadsNoTexMap(
                (GORestrictedStringColorProperty*)findProperty(GO_FACE_COLOR_PROPERTY_NAME_STR),
                (GORestrictedStringScalarProperty*)findProperty(GO_FACE_ALPHA_PROPERTY_NAME_STR));
        }
        m_edgeQuads.clear();
        if (lineStyle != GO_PROPERTY_VALUE_NONE_STR) {
            m_edgeQuads = buildQuadsNoTexMap(
                (GORestrictedStringColorProperty*)findProperty(GO_EDGE_COLOR_PROPERTY_NAME_STR),
                (GORestrictedStringScalarProperty*)findProperty(GO_EDGE_ALPHA_PROPERTY_NAME_STR));
        }
        m_quadsDirty = false;
    }

    double lineWidth = findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR);
    RenderLightingMaterial material;
    material.ambientStrength = findScalarDoubleProperty(GO_AMBIENT_STRENGTH_PROPERTY_NAME_STR);
    material.diffuseStrength = findScalarDoubleProperty(GO_DIFFUSE_STRENGTH_PROPERTY_NAME_STR);
    material.specularStrength = findScalarDoubleProperty(GO_SPECULAR_STRENGTH_PROPERTY_NAME_STR);
    material.specularExponent = findScalarDoubleProperty(GO_SPECULAR_EXPONENT_PROPERTY_NAME_STR);
    material.specularColorReflectance
        = findScalarDoubleProperty(GO_SPECULAR_COLOR_REFLECTANCE_PROPERTY_NAME_STR);
    gc.setLightingMaterial(material);
    // The quads are emitted as a packed per-face quad list (one strip, 4 points per
    // face) whenever the colour OR the alpha is flat. The renderers key the layout
    // off this flag, so it must match that condition, not just a flat FaceColor (a
    // solid FaceColor with FaceAlpha 'flat' + AlphaData still needs per-face
    // transparency).
    bool packedQuadLayout = !textured
        && (stringCheck(GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR)
            || stringCheck(GO_FACE_ALPHA_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR));
    // Bind the texture around the surface draw so a capable renderer samples it
    // per fragment, then clear it so later objects are untextured. A renderer
    // without texture binding ignores these calls and draws the white base colour.
    if (crispTexture) {
        gc.setSurfaceTexture(textureView());
    }
    gc.quadStrips(m_surfaceQuads, packedQuadLayout, m_edgeQuads,
        stringCheck(GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR), meshstyle,
        lineWidth, lineStyle, surfaceLightingMode(this, GO_FACE_LIGHTING_PROPERTY_NAME_STR),
        surfaceLightingMode(this, GO_EDGE_LIGHTING_PROPERTY_NAME_STR), m_geometryToken);
    if (crispTexture) {
        gc.clearSurfaceTexture();
    }
}
//=============================================================================
std::vector<double>
GOSurface::getLimits()
{
    if (!limitsDirty) {
        return limits;
    }
    limitsDirty = false;
    limits.resize(0);
    limits.reserve(10);
    // Axis limits can be computed before the first paint runs updateState().
    // Materialize auto XData/YData from the ZData dimensions so a fresh surface(Z)
    // reports its real X/Y extent. Only do it while the stored data is empty:
    // supplied XData/YData may carry a pending auto mode (the auto-to-manual flip
    // also happens in updateState) and must not be overwritten.
    if (isAuto(GO_X_DATA_MODE_PROPERTY_NAME_STR)
        && findArrayOfProperty(GO_X_DATA_PROPERTY_NAME_STR).isEmpty()) {
        autoXMode();
    }
    if (isAuto(GO_Y_DATA_MODE_PROPERTY_NAME_STR)
        && findArrayOfProperty(GO_Y_DATA_PROPERTY_NAME_STR).isEmpty()) {
        autoYMode();
    }
    ArrayOf xdata(findArrayOfProperty(GO_X_DATA_PROPERTY_NAME_STR));
    ArrayOf ydata(findArrayOfProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    ArrayOf zdata(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    ArrayOf cdata(findArrayOfProperty(GO_C_DATA_PROPERTY_NAME_STR));
    limits.push_back(ArrayOfMin(xdata));
    limits.push_back(ArrayOfMax(xdata));
    limits.push_back(ArrayOfMin(ydata));
    limits.push_back(ArrayOfMax(ydata));
    limits.push_back(ArrayOfMin(zdata));
    limits.push_back(ArrayOfMax(zdata));
    limits.push_back(ArrayOfMin(cdata));
    limits.push_back(ArrayOfMax(cdata));
    std::vector<double> alphadata(findVectorDoubleProperty(GO_ALPHA_DATA_PROPERTY_NAME_STR));
    limits.push_back(findVectorMin(alphadata));
    limits.push_back(findVectorMax(alphadata));
    return limits;
}
//=============================================================================
void
GOSurface::autoCMode()
{
    ArrayOf zdata(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    GOArrayOfProperty* hp = (GOArrayOfProperty*)findProperty(GO_C_DATA_PROPERTY_NAME_STR);
    hp->data(zdata);
}
//=============================================================================
void
GOSurface::autoYMode()
{
    ArrayOf zdata(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    ArrayOf ydata(ArrayOf::doubleMatrix2d(zdata.rows(), 1));
    double* dp = (double*)ydata.readWriteDataPointer();
    indexType rows(zdata.rows());
    OMP_PARALLEL_FOR_LOOP(rows)
    for (ompIndexType i = 0; i < (ompIndexType)rows; i++) {
        dp[i] = static_cast<double>(i + 1);
    }
    GOArrayOfProperty* hp = (GOArrayOfProperty*)findProperty(GO_Y_DATA_PROPERTY_NAME_STR);
    hp->data(ydata);
}
//=============================================================================
void
GOSurface::autoXMode()
{
    ArrayOf zdata(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    ArrayOf xdata(ArrayOf::doubleRowVector(zdata.columns()));
    double* dp = (double*)xdata.readWriteDataPointer();
    indexType cols(zdata.columns());
    OMP_PARALLEL_FOR_LOOP(cols)
    for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
        dp[j] = static_cast<double>(j + 1);
    }
    GOArrayOfProperty* hp = (GOArrayOfProperty*)findProperty(GO_X_DATA_PROPERTY_NAME_STR);
    hp->data(xdata);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
