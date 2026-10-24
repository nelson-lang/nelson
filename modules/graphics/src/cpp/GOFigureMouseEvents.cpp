//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <array>
#include <cmath>
#include "GOFigureMouseEvents.hpp"
#include "GOFigure.hpp"
#include "GOAxis.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBubbleChart.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOImage.hpp"
#include "GOLineSeries.hpp"
#include "GOPatch.hpp"
#include "GOPropertyNames.hpp"
#include "GOScatterSeries.hpp"
#include "GOSurface.hpp"
#include "GOText.hpp"
#include "GOPropertyValues.hpp"
#include "GOTiledChartLayout.hpp"
#include "GOUIContainer.h"
#include "RenderWeb.hpp"
#include "UnitsConversionHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
using Vec3 = std::array<double, 3>;
//=============================================================================
static void
updateChildrenCurrentPoint(
    GraphicsObject* parent, double x, double y, double width, double height, double scale);
static void
updateAxesCurrentPoint(GOAxis* axis, double x, double y, double width, double height);
static bool
pushFigureCallback(GOFigure* fig, const std::wstring& propertyName, const std::wstring& eventType);
static void
pushObjectCallback(
    GraphicsObject* object, const std::wstring& propertyName, const std::wstring& eventType);
static void
pickInChildren(GraphicsObject* parent, double x, double y, double width, double height,
    double scale, int64& bestHandle, double& bestDepth, int64& fallbackAxes);
static void
pickInAxes(GOAxis* axis, int64 axisHandle, double x, double y, double width, double height,
    int64& bestHandle, double& bestDepth, int64& fallbackAxes);
static bool
hitFilledPolygon(GOAxis* axis, RenderWeb& gc, const std::vector<double>& xs,
    const std::vector<double>& ys, const std::vector<double>& zs, double px, double py,
    double& polygonDepth);
static void
hitTestScatterLike(GOAxis* axis, RenderWeb& gc, GraphicsObject* object, int64 handle, bool isBubble,
    double x, double y, int64& bestHandle, double& bestDepth);
//=============================================================================
void
updateFigureCurrentPoint(GOFigure* fig, double x, double y)
{
    if (fig == nullptr) {
        return;
    }
    double width = fig->getLayoutWidth();
    double height = fig->getLayoutHeight();
    if (width <= 0 || height <= 0) {
        return;
    }
    std::wstring units = fig->findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
    auto [cx, cy] = convertFromPixels(units, x, y, width, height);
    fig->setTwoVectorDefault(GO_CURRENT_POINT_PROPERTY_NAME_STR, cx, cy);
    updateChildrenCurrentPoint(fig, x, y, width, height, fig->getRenderScale());
}
//=============================================================================
bool
figureListensToMotion(GOFigure* fig)
{
    auto* callback = fig ? dynamic_cast<GOCallbackProperty*>(fig->findProperty(
                               GO_WINDOW_BUTTON_MOTION_FCN_PROPERTY_NAME_STR, false))
                         : nullptr;
    return callback != nullptr && !callback->get().isEmpty();
}
//=============================================================================
std::wstring
resolveSelectionType(int button, bool shiftDown, bool ctrlDown, bool doubleClick)
{
    if (doubleClick) {
        return GO_PROPERTY_VALUE_OPEN_STR;
    }
    if (button == 2) {
        return GO_PROPERTY_VALUE_ALT_STR;
    }
    if (button == 1) {
        return GO_PROPERTY_VALUE_EXTEND_STR;
    }
    if (ctrlDown) {
        return GO_PROPERTY_VALUE_ALT_STR;
    }
    if (shiftDown) {
        return GO_PROPERTY_VALUE_EXTEND_STR;
    }
    return GO_PROPERTY_VALUE_NORMAL_STR;
}
//=============================================================================
void
setFigureSelectionType(GOFigure* fig, const std::wstring& selectionType)
{
    if (fig == nullptr || selectionType.empty()) {
        return;
    }
    GOGenericProperty* property = fig->findProperty(GO_SELECTION_TYPE_PROPERTY_NAME_STR, false);
    if (property != nullptr) {
        property->set(ArrayOf::characterArray(selectionType));
        property->clearModified();
    }
}
//=============================================================================
bool
dispatchFigureMouseEvent(GOFigure* fig, const std::wstring& action, double x, double y,
    const std::wstring& selectionType)
{
    if (fig == nullptr) {
        return false;
    }
    const bool listensToMotion = figureListensToMotion(fig);
    if (action == L"motion" && !listensToMotion) {
        return false;
    }
    updateFigureCurrentPoint(fig, x, y);
    if (action == L"press") {
        setFigureSelectionType(fig, selectionType);
        pushFigureCallback(fig, GO_WINDOW_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"WindowMousePress");
        int64 picked = pickObjectAtPixel(fig, x, y);
        if (picked >= 0) {
            setCurrentObject(picked);
            GraphicsObject* object = findGraphicsObject(picked, false);
            if (object != nullptr) {
                pushObjectCallback(object, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"ButtonDown");
            }
        } else {
            pushFigureCallback(fig, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"ButtonDown");
        }
    } else if (action == L"motion") {
        pushFigureCallback(
            fig, GO_WINDOW_BUTTON_MOTION_FCN_PROPERTY_NAME_STR, L"WindowMouseMotion");
    } else if (action == L"release") {
        pushFigureCallback(fig, GO_WINDOW_BUTTON_UP_FCN_PROPERTY_NAME_STR, L"WindowMouseRelease");
    }
    return listensToMotion;
}
//=============================================================================
static bool
pushFigureCallback(GOFigure* fig, const std::wstring& propertyName, const std::wstring& eventType)
{
    auto* callback = dynamic_cast<GOCallbackProperty*>(fig->findProperty(propertyName, false));
    if (callback == nullptr || callback->get().isEmpty()) {
        return false;
    }
    callback->pushEvent(fig, L"MouseData", eventType);
    return true;
}
//=============================================================================
static void
pushObjectCallback(
    GraphicsObject* object, const std::wstring& propertyName, const std::wstring& eventType)
{
    auto* callback = dynamic_cast<GOCallbackProperty*>(object->findProperty(propertyName, false));
    if (callback == nullptr || callback->get().isEmpty()) {
        return;
    }
    callback->pushEvent(object, L"MouseData", eventType);
}
//=============================================================================
// An object is pickable unless it is hidden, or HitTest == 'off', or
// PickableParts == 'none'. Same rule as the Qt isPickable helper.
static bool
isPickableObject(GraphicsObject* object)
{
    if (object == nullptr) {
        return false;
    }
    if (object->findProperty(GO_VISIBLE_PROPERTY_NAME_STR, false) != nullptr
        && object->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return false;
    }
    if (object->findProperty(GO_HIT_TEST_PROPERTY_NAME_STR, false) != nullptr
        && object->stringCheck(GO_HIT_TEST_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return false;
    }
    if (object->findProperty(GO_PICKABLE_PARTS_PROPERTY_NAME_STR, false) != nullptr
        && object->stringCheck(GO_PICKABLE_PARTS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)) {
        return false;
    }
    return true;
}
//=============================================================================
static double
pointSegmentDistance(double px, double py, double ax, double ay, double bx, double by)
{
    double dx = bx - ax;
    double dy = by - ay;
    double len2 = dx * dx + dy * dy;
    double t = (len2 > 0.0) ? ((px - ax) * dx + (py - ay) * dy) / len2 : 0.0;
    t = std::max(0.0, std::min(1.0, t));
    double cx = ax + t * dx;
    double cy = ay + t * dy;
    double ex = px - cx;
    double ey = py - cy;
    return std::sqrt(ex * ex + ey * ey);
}
//=============================================================================
// Tests the pointer against a line series: its stroke (within half the line
// width plus a tolerance) and its markers. Keeps the front-most hit (smallest
// clip-space depth); on a tie the later-drawn object wins because the caller
// walks children in draw order.
static void
hitTestLine(GOAxis* axis, RenderWeb& gc, GraphicsObject* line, int64 handle, double x, double y,
    int64& bestHandle, double& bestDepth)
{
    std::vector<double> xs = line->findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR);
    std::vector<double> ys = line->findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR);
    std::vector<double> zs = line->findVectorDoubleProperty(GO_Z_DATA_PROPERTY_NAME_STR);
    if (xs.empty() || xs.size() != ys.size()) {
        return;
    }
    if (zs.size() != xs.size()) {
        zs.assign(xs.size(), 0.0);
    }
    std::vector<double> mxs, mys, mzs;
    axis->reMap(xs, ys, zs, mxs, mys, mzs);
    if (mxs.size() != xs.size()) {
        return;
    }
    const double tolerance = 4.0;
    double lineWidth = line->findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR);
    double strokeTol = std::max(lineWidth * 0.5, tolerance);
    bool hasStroke
        = !line->stringCheck(GO_LINE_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    bool hasMarker = line->findProperty(GO_MARKER_PROPERTY_NAME_STR, false) != nullptr
        && !line->stringCheck(GO_MARKER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    double markerTol = 0.0;
    if (hasMarker) {
        markerTol = std::max(
            line->findScalarDoubleProperty(GO_MARKER_SIZE_PROPERTY_NAME_STR) * 0.5, tolerance);
    }
    size_t n = mxs.size();
    std::vector<double> pu(n), pv(n), pd(n);
    std::vector<char> ok(n, 0);
    for (size_t i = 0; i < n; i++) {
        double u = 0, v = 0, d = 0;
        bool clipped = false;
        gc.toPixels(mxs[i], mys[i], mzs[i], u, v, d, clipped);
        ok[i] = clipped ? 0 : 1;
        pu[i] = u;
        pv[i] = v;
        pd[i] = d;
    }
    bool hit = false;
    double hitDepth = HUGE_VAL;
    if (hasStroke) {
        for (size_t i = 0; i + 1 < n; i++) {
            if (!ok[i] || !ok[i + 1]) {
                continue;
            }
            if (pointSegmentDistance(x, y, pu[i], pv[i], pu[i + 1], pv[i + 1]) <= strokeTol) {
                double depth = std::min(pd[i], pd[i + 1]);
                hitDepth = hit ? std::min(hitDepth, depth) : depth;
                hit = true;
            }
        }
    }
    if (hasMarker) {
        for (size_t i = 0; i < n; i++) {
            if (!ok[i]) {
                continue;
            }
            double ex = x - pu[i];
            double ey = y - pv[i];
            if (std::sqrt(ex * ex + ey * ey) <= markerTol) {
                hitDepth = hit ? std::min(hitDepth, pd[i]) : pd[i];
                hit = true;
            }
        }
    }
    if (hit && hitDepth <= bestDepth) {
        bestDepth = hitDepth;
        bestHandle = handle;
    }
}
//=============================================================================
static bool
pointInPolygon(double px, double py, const std::vector<double>& xs, const std::vector<double>& ys)
{
    size_t n = xs.size();
    if (n < 3 || ys.size() != n) {
        return false;
    }
    bool inside = false;
    for (size_t i = 0, j = n - 1; i < n; j = i++) {
        bool straddles = (ys[i] > py) != (ys[j] > py);
        if (straddles) {
            double xCross = xs[j] + (py - ys[j]) * (xs[i] - xs[j]) / (ys[i] - ys[j]);
            if (px < xCross) {
                inside = !inside;
            }
        }
    }
    return inside;
}
//=============================================================================
// Projects a filled polygon (data coordinates) and tests whether (px, py) is
// inside it. polygonDepth returns the nearest projected vertex depth.
static bool
hitFilledPolygon(GOAxis* axis, RenderWeb& gc, const std::vector<double>& xs,
    const std::vector<double>& ys, const std::vector<double>& zs, double px, double py,
    double& polygonDepth)
{
    size_t n = xs.size();
    if (n < 3 || ys.size() != n || zs.size() != n) {
        return false;
    }
    std::vector<double> mx, my, mz;
    axis->reMap(xs, ys, zs, mx, my, mz);
    if (mx.size() != n) {
        return false;
    }
    std::vector<double> pxv(n), pyv(n);
    double depth = HUGE_VAL;
    for (size_t i = 0; i < n; i++) {
        double u = 0, v = 0, d = 0;
        bool clipped = false;
        gc.toPixels(mx[i], my[i], mz[i], u, v, d, clipped);
        pxv[i] = u;
        pyv[i] = v;
        depth = std::min(depth, d);
    }
    if (!pointInPolygon(px, py, pxv, pyv)) {
        return false;
    }
    polygonDepth = depth;
    return true;
}
//=============================================================================
// Image: the filled rectangle spanning its x/y extent (from getLimits).
static void
hitTestImage(GOAxis* axis, RenderWeb& gc, GraphicsObject* image, int64 handle, double x, double y,
    int64& bestHandle, double& bestDepth)
{
    std::vector<double> lims = image->getLimits();
    if (lims.size() < 4) {
        return;
    }
    std::vector<double> xs = { lims[0], lims[1], lims[1], lims[0] };
    std::vector<double> ys = { lims[2], lims[2], lims[3], lims[3] };
    std::vector<double> zs(4, 0.0);
    double depth = HUGE_VAL;
    if (hitFilledPolygon(axis, gc, xs, ys, zs, x, y, depth) && depth <= bestDepth) {
        bestDepth = depth;
        bestHandle = handle;
    }
}
//=============================================================================
// Patch: each face is a filled polygon of its vertices. Vertices is an N-by-2
// or N-by-3 column-major matrix; Faces is nFaces-by-K of 1-based vertex
// indices, NaN padding unused slots.
static void
hitTestPatch(GOAxis* axis, RenderWeb& gc, GraphicsObject* patch, int64 handle, double x, double y,
    int64& bestHandle, double& bestDepth)
{
    ArrayOf vertices = patch->findArrayOfProperty(GO_VERTICES_PROPERTY_NAME_STR);
    ArrayOf faces = patch->findArrayOfProperty(GO_FACES_PROPERTY_NAME_STR);
    if (vertices.isEmpty() || faces.isEmpty()) {
        return;
    }
    vertices.promoteType(NLS_DOUBLE);
    faces.promoteType(NLS_DOUBLE);
    indexType nVerts = vertices.rows();
    indexType vCols = vertices.columns();
    const double* v = static_cast<const double*>(vertices.dataPointer());
    indexType nFaces = faces.rows();
    indexType maxK = faces.columns();
    const double* f = static_cast<const double*>(faces.dataPointer());
    if (v == nullptr || f == nullptr || nVerts == 0 || nFaces == 0) {
        return;
    }
    bool hit = false;
    double hitDepth = HUGE_VAL;
    for (indexType j = 0; j < nFaces; j++) {
        std::vector<double> xs, ys, zs;
        xs.reserve(maxK);
        ys.reserve(maxK);
        zs.reserve(maxK);
        for (indexType k = 0; k < maxK; k++) {
            double idx = f[j + k * nFaces];
            if (std::isnan(idx) || idx < 1.0) {
                continue;
            }
            indexType vi = static_cast<indexType>(idx) - 1;
            if (vi >= nVerts) {
                continue;
            }
            xs.push_back(v[vi]);
            ys.push_back(v[vi + nVerts]);
            zs.push_back(vCols >= 3 ? v[vi + 2 * nVerts] : 0.0);
        }
        double depth = HUGE_VAL;
        if (hitFilledPolygon(axis, gc, xs, ys, zs, x, y, depth)) {
            hitDepth = hit ? std::min(hitDepth, depth) : depth;
            hit = true;
        }
    }
    if (hit && hitDepth <= bestDepth) {
        bestDepth = hitDepth;
        bestHandle = handle;
    }
}
//=============================================================================
// Surface: each grid cell is a filled quad. XData/YData are full matrices or
// the vectors of a meshgrid (X along columns, Y along rows). ZData is the
// rows-by-cols column-major grid.
static void
hitTestSurface(GOAxis* axis, RenderWeb& gc, GraphicsObject* surface, int64 handle, double x,
    double y, int64& bestHandle, double& bestDepth)
{
    ArrayOf zdata = surface->findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR);
    if (zdata.isEmpty()) {
        return;
    }
    zdata.promoteType(NLS_DOUBLE);
    ArrayOf xdata = surface->findArrayOfProperty(GO_X_DATA_PROPERTY_NAME_STR);
    ArrayOf ydata = surface->findArrayOfProperty(GO_Y_DATA_PROPERTY_NAME_STR);
    xdata.promoteType(NLS_DOUBLE);
    ydata.promoteType(NLS_DOUBLE);
    indexType rows = zdata.rows();
    indexType cols = zdata.columns();
    if (rows < 2 || cols < 2) {
        return;
    }
    const double* zp = static_cast<const double*>(zdata.dataPointer());
    const double* xp = static_cast<const double*>(xdata.dataPointer());
    const double* yp = static_cast<const double*>(ydata.dataPointer());
    indexType xCount = static_cast<indexType>(xdata.elementCount());
    indexType yCount = static_cast<indexType>(ydata.elementCount());
    auto xAt = [&](indexType r, indexType c) -> double {
        if (xp == nullptr) {
            return static_cast<double>(c + 1);
        }
        if (xCount == rows * cols) {
            return xp[r + c * rows];
        }
        if (xCount == cols) {
            return xp[c];
        }
        if (xCount == rows) {
            return xp[r];
        }
        return static_cast<double>(c + 1);
    };
    auto yAt = [&](indexType r, indexType c) -> double {
        if (yp == nullptr) {
            return static_cast<double>(r + 1);
        }
        if (yCount == rows * cols) {
            return yp[r + c * rows];
        }
        if (yCount == rows) {
            return yp[r];
        }
        if (yCount == cols) {
            return yp[c];
        }
        return static_cast<double>(r + 1);
    };
    bool hit = false;
    double hitDepth = HUGE_VAL;
    for (indexType c = 0; c + 1 < cols; c++) {
        for (indexType r = 0; r + 1 < rows; r++) {
            std::vector<double> xs = { xAt(r, c), xAt(r, c + 1), xAt(r + 1, c + 1), xAt(r + 1, c) };
            std::vector<double> ys = { yAt(r, c), yAt(r, c + 1), yAt(r + 1, c + 1), yAt(r + 1, c) };
            std::vector<double> zs = { zp[r + c * rows], zp[r + (c + 1) * rows],
                zp[(r + 1) + (c + 1) * rows], zp[(r + 1) + c * rows] };
            double depth = HUGE_VAL;
            if (hitFilledPolygon(axis, gc, xs, ys, zs, x, y, depth)) {
                hitDepth = hit ? std::min(hitDepth, depth) : depth;
                hit = true;
            }
        }
    }
    if (hit && hitDepth <= bestDepth) {
        bestDepth = hitDepth;
        bestHandle = handle;
    }
}
//=============================================================================
// Text: the filled rectangle of its Extent (in data units). Skipped when the
// extent is not expressed in data coordinates.
static void
hitTestText(GOAxis* axis, RenderWeb& gc, GraphicsObject* text, int64 handle, double x, double y,
    int64& bestHandle, double& bestDepth)
{
    if (text->findProperty(GO_UNITS_PROPERTY_NAME_STR, false) != nullptr
        && !text->stringCheck(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_DATA_STR)) {
        return;
    }
    std::vector<double> extent = text->findVectorDoubleProperty(GO_EXTENT_PROPERTY_NAME_STR);
    std::vector<double> pos = text->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (extent.size() < 4 || extent[2] <= 0 || extent[3] <= 0) {
        return;
    }
    double z = (pos.size() >= 3) ? pos[2] : 0.0;
    double x0 = extent[0];
    double y0 = extent[1];
    double x1 = extent[0] + extent[2];
    double y1 = extent[1] + extent[3];
    std::vector<double> xs = { x0, x1, x1, x0 };
    std::vector<double> ys = { y0, y0, y1, y1 };
    std::vector<double> zs(4, z);
    double depth = HUGE_VAL;
    if (hitFilledPolygon(axis, gc, xs, ys, zs, x, y, depth) && depth <= bestDepth) {
        bestDepth = depth;
        bestHandle = handle;
    }
}
//=============================================================================
// Scatter / bubble: a hit is a click within a marker. The marker pixel radius is
// sqrt(SizeData)/2 for a scatter (SizeData is the marker area). For a bubble
// chart SizeData is a value mapped to a pixel diameter through the axes' size
// range. A tolerance keeps small markers easy to click.
static void
hitTestScatterLike(GOAxis* axis, RenderWeb& gc, GraphicsObject* object, int64 handle, bool isBubble,
    double x, double y, int64& bestHandle, double& bestDepth)
{
    std::vector<double> xs = object->findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR);
    std::vector<double> ys = object->findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR);
    std::vector<double> zs = object->findVectorDoubleProperty(GO_Z_DATA_PROPERTY_NAME_STR);
    if (xs.empty() || xs.size() != ys.size()) {
        return;
    }
    if (zs.size() != xs.size()) {
        zs.assign(xs.size(), 0.0);
    }
    std::vector<double> sizeData = object->findVectorDoubleProperty(GO_SIZE_DATA_PROPERTY_NAME_STR);
    const double tolerance = 4.0;
    double minValue = 0.0, maxValue = 1.0, minDiameter = 3.0, maxDiameter = 50.0;
    if (isBubble) {
        if (!sizeData.empty()) {
            minValue = *std::min_element(sizeData.begin(), sizeData.end());
            maxValue = *std::max_element(sizeData.begin(), sizeData.end());
            if (maxValue <= minValue) {
                maxValue = (minValue > 0.0) ? minValue : 1.0;
                minValue = (minValue > 0.0) ? minValue * 0.9 : 0.0;
            }
        }
        std::vector<double> range
            = axis->findVectorDoubleProperty(GO_BUBBLE_SIZE_RANGE_INTERNAL_PROPERTY_NAME_STR);
        if (range.size() >= 2 && range[0] > 0.0 && range[1] > range[0]) {
            minDiameter = range[0];
            maxDiameter = range[1];
        }
    }
    std::vector<double> mx, my, mz;
    axis->reMap(xs, ys, zs, mx, my, mz);
    if (mx.size() != xs.size()) {
        return;
    }
    bool hit = false;
    double hitDepth = HUGE_VAL;
    for (size_t i = 0; i < mx.size(); i++) {
        double u = 0, v = 0, d = 0;
        bool clipped = false;
        gc.toPixels(mx[i], my[i], mz[i], u, v, d, clipped);
        if (clipped) {
            continue;
        }
        double radius = tolerance;
        if (isBubble) {
            double value = sizeData.empty() ? maxValue : sizeData[std::min(i, sizeData.size() - 1)];
            double diameter = maxDiameter;
            if (maxValue > minValue) {
                double normalized = (std::max(0.0, value) - minValue) / (maxValue - minValue);
                normalized = std::max(0.0, std::min(1.0, normalized));
                diameter = minDiameter + std::sqrt(normalized) * (maxDiameter - minDiameter);
            }
            radius = std::max(diameter * 0.5, tolerance);
        } else if (!sizeData.empty()) {
            double area = sizeData[std::min(i, sizeData.size() - 1)];
            radius = std::max(std::sqrt(std::max(0.0, area)) * 0.5, tolerance);
        }
        double ex = x - u;
        double ey = y - v;
        if (std::sqrt(ex * ex + ey * ey) <= radius) {
            hitDepth = hit ? std::min(hitDepth, d) : d;
            hit = true;
        }
    }
    if (hit && hitDepth <= bestDepth) {
        bestDepth = hitDepth;
        bestHandle = handle;
    }
}
//=============================================================================
// Tests the pointer against the pickable children of one axes, using the
// axes' own projection (the surface it is painted on, width x height pixels).
static void
pickInAxes(GOAxis* axis, int64 axisHandle, double x, double y, double width, double height,
    int64& bestHandle, double& bestDepth, int64& fallbackAxes)
{
    auto* children = dynamic_cast<GOGObjectsProperty*>(
        axis->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (children == nullptr || width <= 0 || height <= 0) {
        return;
    }
    RenderWeb gc(0, 0, width, height, false);
    axis->setupProjectionOnly(gc, false);
    // The axes itself is the fallback target when no child is hit and the
    // pointer is inside its box (last pickable axes under the pointer wins).
    if (isPickableObject(axis)) {
        std::vector<double> box
            = axis->getPropertyVectorAsPixels(gc, GO_POSITION_PROPERTY_NAME_STR);
        if (box.size() == 4 && x >= box[0] && x <= box[0] + box[2] && y >= box[1]
            && y <= box[1] + box[3]) {
            fallbackAxes = axisHandle;
        }
    }
    for (int64 handle : children->data()) {
        GraphicsObject* child = findGraphicsObject(handle, false);
        if (child == nullptr || !isPickableObject(child)) {
            continue;
        }
        if (dynamic_cast<GOLineSeries*>(child) != nullptr) {
            hitTestLine(axis, gc, child, handle, x, y, bestHandle, bestDepth);
        } else if (dynamic_cast<GOImage*>(child) != nullptr) {
            hitTestImage(axis, gc, child, handle, x, y, bestHandle, bestDepth);
        } else if (dynamic_cast<GOSurface*>(child) != nullptr) {
            hitTestSurface(axis, gc, child, handle, x, y, bestHandle, bestDepth);
        } else if (dynamic_cast<GOBubbleChart*>(child) != nullptr) {
            hitTestScatterLike(axis, gc, child, handle, true, x, y, bestHandle, bestDepth);
        } else if (dynamic_cast<GOScatterSeries*>(child) != nullptr) {
            hitTestScatterLike(axis, gc, child, handle, false, x, y, bestHandle, bestDepth);
        } else if (dynamic_cast<GOPatch*>(child) != nullptr) {
            hitTestPatch(axis, gc, child, handle, x, y, bestHandle, bestDepth);
        } else if (dynamic_cast<GOText*>(child) != nullptr) {
            hitTestText(axis, gc, child, handle, x, y, bestHandle, bestDepth);
        }
    }
}
//=============================================================================
static void
pickInChildren(GraphicsObject* parent, double x, double y, double width, double height,
    double scale, int64& bestHandle, double& bestDepth, int64& fallbackAxes)
{
    auto* children = dynamic_cast<GOGObjectsProperty*>(
        parent->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (children == nullptr) {
        return;
    }
    for (int64 handle : children->data()) {
        GraphicsObject* child = findGraphicsObject(handle, false);
        if (child == nullptr) {
            continue;
        }
        if (auto* axis = dynamic_cast<GOAxis*>(child)) {
            pickInAxes(axis, handle, x * scale, y * scale, width * scale, height * scale,
                bestHandle, bestDepth, fallbackAxes);
        } else if (auto* container = dynamic_cast<GOUIContainer*>(child)) {
            RenderWeb parentFrame(0, 0, width, height, false);
            std::vector<double> frame = container->frameRect(parentFrame);
            if (frame.size() != 4 || frame[2] <= 0 || frame[3] <= 0) {
                continue;
            }
            RenderWeb containerFrame(0, 0, frame[2], frame[3], false);
            std::vector<double> content = container->contentRect(containerFrame);
            if (content.size() != 4) {
                continue;
            }
            pickInChildren(container, x - frame[0] - content[0], y - frame[1] - content[1],
                content[2], content[3], 1.0, bestHandle, bestDepth, fallbackAxes);
        } else if (dynamic_cast<GOTiledChartLayout*>(child) != nullptr) {
            pickInChildren(child, x, y, width, height, scale, bestHandle, bestDepth, fallbackAxes);
        }
    }
}
//=============================================================================
int64
pickObjectAtPixel(GOFigure* fig, double x, double y)
{
    if (fig == nullptr) {
        return -1;
    }
    double width = fig->getLayoutWidth();
    double height = fig->getLayoutHeight();
    if (width <= 0 || height <= 0) {
        return -1;
    }
    int64 bestHandle = -1;
    double bestDepth = HUGE_VAL;
    int64 fallbackAxes = -1;
    pickInChildren(
        fig, x, y, width, height, fig->getRenderScale(), bestHandle, bestDepth, fallbackAxes);
    // An object drawn under the pointer wins; otherwise the axes under the
    // pointer is the target (as the native window does); otherwise nothing.
    return (bestHandle >= 0) ? bestHandle : fallbackAxes;
}
//=============================================================================
// Walks the objects that can hold axes. (x, y) and (width, height) are the
// pointer and the size of the current frame in logical pixels: the figure
// drawing area, then the content area of each nested container. scale is the
// ratio of the surface the axes of that frame are painted on: the figure
// canvas is rendered in device pixels, containers paint in logical pixels.
static void
updateChildrenCurrentPoint(
    GraphicsObject* parent, double x, double y, double width, double height, double scale)
{
    auto* children = dynamic_cast<GOGObjectsProperty*>(
        parent->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (children == nullptr) {
        return;
    }
    for (int64 handle : children->data()) {
        GraphicsObject* child = findGraphicsObject(handle, false);
        if (child == nullptr) {
            continue;
        }
        if (auto* axis = dynamic_cast<GOAxis*>(child)) {
            updateAxesCurrentPoint(axis, x * scale, y * scale, width * scale, height * scale);
        } else if (auto* container = dynamic_cast<GOUIContainer*>(child)) {
            RenderWeb parentFrame(0, 0, width, height, false);
            std::vector<double> frame = container->frameRect(parentFrame);
            if (frame.size() != 4 || frame[2] <= 0 || frame[3] <= 0) {
                continue;
            }
            RenderWeb containerFrame(0, 0, frame[2], frame[3], false);
            std::vector<double> content = container->contentRect(containerFrame);
            if (content.size() != 4) {
                continue;
            }
            updateChildrenCurrentPoint(container, x - frame[0] - content[0],
                y - frame[1] - content[1], content[2], content[3], 1.0);
        } else if (dynamic_cast<GOTiledChartLayout*>(child) != nullptr) {
            updateChildrenCurrentPoint(child, x, y, width, height, scale);
        }
    }
}
//=============================================================================
// Inverse of GOAxis::mapX/mapY/mapZ: the log scale is applied first, then the
// reversed direction mirrors the value inside the limits.
static double
unmapCoordinate(GOAxis* axis, double value, const std::wstring& dirName,
    const std::wstring& limName, const std::wstring& scaleName)
{
    if (axis->stringCheck(dirName, GO_PROPERTY_VALUE_REVERSE_STR)) {
        std::vector<double> lims = axis->findVectorDoubleProperty(limName);
        if (lims.size() == 2) {
            value = lims[0] + lims[1] - value;
        }
    }
    if (axis->stringCheck(scaleName, GO_PROPERTY_VALUE_LOG_STR)) {
        value = std::pow(10.0, value);
    }
    return value;
}
//=============================================================================
static Vec3
toWindow(RenderWeb& gc, const Vec3& p)
{
    Vec3 w { 0, 0, 0 };
    double eyeX = 0;
    double eyeY = 0;
    gc.toPixels(p[0], p[1], p[2], w[0], w[1]);
    gc.mapPoint(p[0], p[1], p[2], eyeX, eyeY, w[2]);
    return w;
}
//=============================================================================
static double
determinant(const std::array<Vec3, 3>& m)
{
    return m[0][0] * (m[1][1] * m[2][2] - m[2][1] * m[1][2])
        - m[1][0] * (m[0][1] * m[2][2] - m[2][1] * m[0][2])
        + m[2][0] * (m[0][1] * m[1][2] - m[1][1] * m[0][2]);
}
//=============================================================================
// Solves columns * d = rhs (Cramer), columns[i] being the i-th column.
static bool
solve3(const std::array<Vec3, 3>& columns, const Vec3& rhs, Vec3& d)
{
    double det = determinant(columns);
    if (!std::isfinite(det) || std::abs(det) < 1e-300) {
        return false;
    }
    for (int i = 0; i < 3; i++) {
        std::array<Vec3, 3> m = columns;
        m[i] = rhs;
        d[i] = determinant(m) / det;
    }
    return true;
}
//=============================================================================
// The axes projection is orthographic, so mapped data -> (pixel x, pixel y,
// eye depth) is affine: it is sampled on the projection the painter uses and
// inverted. The view line under the pointer is cut at the depths of the
// nearest and farthest corners of the axes box: front and back points.
static void
updateAxesCurrentPoint(GOAxis* axis, double x, double y, double width, double height)
{
    auto* currentPoint = dynamic_cast<GOArrayOfProperty*>(
        axis->findProperty(GO_CURRENT_POINT_PROPERTY_NAME_STR, false));
    if (currentPoint == nullptr) {
        return;
    }
    std::vector<double> xlim = axis->findVectorDoubleProperty(GO_X_LIM_PROPERTY_NAME_STR);
    std::vector<double> ylim = axis->findVectorDoubleProperty(GO_Y_LIM_PROPERTY_NAME_STR);
    std::vector<double> zlim = axis->findVectorDoubleProperty(GO_Z_LIM_PROPERTY_NAME_STR);
    if (xlim.size() != 2 || ylim.size() != 2 || zlim.size() != 2) {
        return;
    }
    RenderWeb gc(0, 0, width, height, false);
    axis->setupProjectionOnly(gc, false);

    std::vector<double> corners;
    corners.reserve(24);
    for (double cx : xlim) {
        for (double cy : ylim) {
            for (double cz : zlim) {
                corners.insert(corners.end(), { cx, cy, cz });
            }
        }
    }
    std::vector<double> mapped = axis->reMap(corners, false);
    Vec3 low { mapped[0], mapped[1], mapped[2] };
    Vec3 high = low;
    double nearDepth = -HUGE_VAL;
    double farDepth = HUGE_VAL;
    for (size_t k = 0; k + 2 < mapped.size(); k += 3) {
        Vec3 corner { mapped[k], mapped[k + 1], mapped[k + 2] };
        for (int i = 0; i < 3; i++) {
            low[i] = std::min(low[i], corner[i]);
            high[i] = std::max(high[i], corner[i]);
        }
        double depth = toWindow(gc, corner)[2];
        nearDepth = std::max(nearDepth, depth);
        farDepth = std::min(farDepth, depth);
    }
    Vec3 origin;
    std::array<Vec3, 3> columns;
    for (int i = 0; i < 3; i++) {
        origin[i] = (low[i] + high[i]) / 2.0;
    }
    Vec3 base = toWindow(gc, origin);
    for (int i = 0; i < 3; i++) {
        double step = std::max(high[i] - low[i], 1.0);
        Vec3 shifted = origin;
        shifted[i] += step;
        Vec3 moved = toWindow(gc, shifted);
        for (int r = 0; r < 3; r++) {
            columns[i][r] = (moved[r] - base[r]) / step;
        }
    }
    double* values = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 6));
    ArrayOf result(NLS_DOUBLE, Dimensions(2, 3), values);
    const double depths[2] = { nearDepth, farDepth };
    for (int row = 0; row < 2; row++) {
        Vec3 d { 0, 0, 0 };
        if (!solve3(columns, { x - base[0], y - base[1], depths[row] - base[2] }, d)) {
            return;
        }
        values[row] = unmapCoordinate(axis, origin[0] + d[0], GO_X_DIR_PROPERTY_NAME_STR,
            GO_X_LIM_PROPERTY_NAME_STR, GO_X_SCALE_PROPERTY_NAME_STR);
        values[row + 2] = unmapCoordinate(axis, origin[1] + d[1], GO_Y_DIR_PROPERTY_NAME_STR,
            GO_Y_LIM_PROPERTY_NAME_STR, GO_Y_SCALE_PROPERTY_NAME_STR);
        values[row + 4] = unmapCoordinate(axis, origin[2] + d[2], GO_Z_DIR_PROPERTY_NAME_STR,
            GO_Z_LIM_PROPERTY_NAME_STR, GO_Z_SCALE_PROPERTY_NAME_STR);
    }
    currentPoint->data(result);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
