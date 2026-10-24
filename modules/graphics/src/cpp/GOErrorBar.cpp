//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOErrorBar.hpp"
#include "GOChartDecorators.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOAxis.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOPickablePartsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOSeriesIndexProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOSymbolProperty.hpp"
#include "GOVectorProperty.hpp"
#include "RenderHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    struct PixelSegment
    {
        double x1;
        double y1;
        double x2;
        double y2;
    };
    //=============================================================================
    struct PixelPoint
    {
        double x;
        double y;
    };
    //=============================================================================
    static void
    addProperty(GraphicsObject* go, GOGenericProperty* property, const std::wstring& name,
        bool writable = true)
    {
        go->registerProperty(property, name, writable);
    }
    //=============================================================================
    static std::vector<double>
    finiteVector(const std::vector<double>& values)
    {
        std::vector<double> result;
        result.reserve(values.size());
        for (double value : values) {
            if (std::isfinite(value)) {
                result.push_back(value);
            }
        }
        return result;
    }
    //=============================================================================
    static std::vector<double>
    expandedDelta(const std::vector<double>& delta, size_t count, const std::wstring& name)
    {
        if (delta.empty()) {
            return {};
        }
        if (delta.size() == 1 && count != 1) {
            return std::vector<double>(count, delta[0]);
        }
        if (delta.size() != count) {
            Error(L"Nelson:errorbar:invalidLength",
                L"%s must be a scalar or match XData and YData length.", name);
        }
        return delta;
    }
    //=============================================================================
    static std::vector<double>
    resolvedRestrictedColor(
        GOAutoFlatColorProperty* property, const std::vector<double>& automaticColor)
    {
        if (!property || property->isEqual(GO_PROPERTY_VALUE_AUTO_STR)
            || property->isEqual(GO_PROPERTY_VALUE_FLAT_STR)) {
            return automaticColor;
        }
        if (property->isEqual(GO_PROPERTY_VALUE_NONE_STR)) {
            return { -1, -1, -1 };
        }
        return property->colorSpec();
    }
    //=============================================================================
    static bool
    isDrawableColor(const std::vector<double>& color)
    {
        return color.size() >= 3 && color[0] >= 0.;
    }
    //=============================================================================
    static bool
    mapToPixel(GOAxis* axis, RenderInterface& gc, double x, double y, double& px, double& py)
    {
        if (!axis || !std::isfinite(x) || !std::isfinite(y)) {
            return false;
        }
        std::vector<double> xs { x };
        std::vector<double> ys { y };
        std::vector<double> zs { 0. };
        std::vector<double> mxs;
        std::vector<double> mys;
        std::vector<double> mzs;
        axis->reMap(xs, ys, zs, mxs, mys, mzs);
        if (mxs.empty()) {
            return false;
        }
        bool clipped = false;
        gc.toPixels(mxs[0], mys[0], mzs[0], px, py, clipped);
        return !clipped;
    }
    //=============================================================================
    static void
    appendValue(std::vector<double>& values, double value)
    {
        if (std::isfinite(value)) {
            values.push_back(value);
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
GOErrorBar::GOErrorBar()
{
    addProperty(this, new GOOnOffProperty, GO_ALIGN_VERTEX_CENTERS_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_CAP_SIZE_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_CLIPPING_PROPERTY_NAME_STR);
    addProperty(this, new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_COLOR_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    addProperty(this, new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_LINE_STYLE_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOSymbolProperty, GO_MARKER_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_MARKER_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoFlatColorProperty, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoFlatColorProperty, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_MARKER_SIZE_PROPERTY_NAME_STR);
    addProperty(this, new GOVectorProperty, GO_X_NEGATIVE_DELTA_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_X_NEGATIVE_DELTA_SOURCE_PROPERTY_NAME_STR);
    addProperty(this, new GOVectorProperty, GO_X_POSITIVE_DELTA_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_X_POSITIVE_DELTA_SOURCE_PROPERTY_NAME_STR);
    addProperty(this, new GOVectorProperty, GO_Y_NEGATIVE_DELTA_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_Y_NEGATIVE_DELTA_SOURCE_PROPERTY_NAME_STR);
    addProperty(this, new GOVectorProperty, GO_Y_POSITIVE_DELTA_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_Y_POSITIVE_DELTA_SOURCE_PROPERTY_NAME_STR);
    addProperty(this, new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR, false);
    addProperty(this, new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    addProperty(this, new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    addProperty(this, new GOCallbackProperty, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    addProperty(this, new GOGObjectsProperty, GO_CONTEXT_MENU_PROPERTY_NAME_STR);
    addProperty(this, new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    addProperty(this, new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    addProperty(this, new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    addProperty(this, new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    addProperty(this, new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_SELECTED_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_HIT_TEST_PROPERTY_NAME_STR);
    addProperty(this, new GOPickablePartsProperty, GO_PICKABLE_PARTS_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    addProperty(this, new GOLazyDecoratorProperty(GODecoratorKind::AnnotationEventData),
        GO_ANNOTATION_PROPERTY_NAME_STR, false);
    addProperty(this, new GOSeriesIndexProperty, GO_SERIES_INDEX_PROPERTY_NAME_STR);
    addProperty(this, new GOVectorProperty, GO_X_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_X_DATA_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_X_DATA_SOURCE_PROPERTY_NAME_STR);
    addProperty(this, new GOVectorProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_Y_DATA_SOURCE_PROPERTY_NAME_STR);
    addProperty(this, new GOLazyDecoratorProperty(GODecoratorKind::DataTipTemplate),
        GO_DATA_TIP_TEMPLATE_PROPERTY_NAME_STR, false);

    setRestrictedStringDefault(GO_ALIGN_VERTEX_CENTERS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setScalarDoubleDefault(GO_CAP_SIZE_PROPERTY_NAME_STR, 6.);
    setRestrictedStringDefault(GO_CLIPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 0.066, 0.443, 0.745);
    setRestrictedStringDefault(GO_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setRestrictedStringDefault(GO_LINE_STYLE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_MARKER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setRestrictedStringDefault(GO_MARKER_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringColorDefault(
        GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR, -1., -1., -1.);
    setRestrictedStringColorDefault(
        GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR, -1., -1., -1.);
    setScalarDoubleDefault(GO_MARKER_SIZE_PROPERTY_NAME_STR, 6.);
    setStringDefault(GO_X_NEGATIVE_DELTA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setStringDefault(GO_X_POSITIVE_DELTA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setStringDefault(GO_Y_NEGATIVE_DELTA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setStringDefault(GO_Y_POSITIVE_DELTA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ERRORBAR_STR);
    setStringDefault(GO_TAG_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setRestrictedStringDefault(GO_SELECTED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HIT_TEST_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_PICKABLE_PARTS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VISIBLE_STR);
    setStringDefault(GO_DISPLAY_NAME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setRestrictedStringDefault(GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setStringDefault(GO_X_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setStringDefault(GO_Y_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    auto* seriesIndex
        = static_cast<GOSeriesIndexProperty*>(findProperty(GO_SERIES_INDEX_PROPERTY_NAME_STR));
    seriesIndex->setValue(1.);
    seriesIndex->clearModified();
    clearAllChanged();
    sortProperties();
}
//=============================================================================
GOErrorBar::~GOErrorBar() = default;
//=============================================================================
std::wstring
GOErrorBar::getType()
{
    return GO_PROPERTY_VALUE_ERRORBAR_STR;
}
//=============================================================================
void
GOErrorBar::updateState()
{
    if (hasChanged(GO_COLOR_PROPERTY_NAME_STR)) {
        toManual(GO_COLOR_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_LINE_STYLE_PROPERTY_NAME_STR)) {
        toManual(GO_LINE_STYLE_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_MARKER_PROPERTY_NAME_STR)) {
        toManual(GO_MARKER_MODE_PROPERTY_NAME_STR);
    }
    bool dataChanged = hasChanged(GO_X_DATA_PROPERTY_NAME_STR)
        || hasChanged(GO_Y_DATA_PROPERTY_NAME_STR)
        || hasChanged(GO_X_NEGATIVE_DELTA_PROPERTY_NAME_STR)
        || hasChanged(GO_X_POSITIVE_DELTA_PROPERTY_NAME_STR)
        || hasChanged(GO_Y_NEGATIVE_DELTA_PROPERTY_NAME_STR)
        || hasChanged(GO_Y_POSITIVE_DELTA_PROPERTY_NAME_STR);
    if (isAuto(GO_X_DATA_MODE_PROPERTY_NAME_STR) && hasChanged(GO_Y_DATA_PROPERTY_NAME_STR)) {
        std::vector<double> y = findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR);
        std::vector<double> x(y.size());
        for (size_t k = 0; k < y.size(); ++k) {
            x[k] = static_cast<double>(k + 1);
        }
        auto* property = static_cast<GOVectorProperty*>(findProperty(GO_X_DATA_PROPERTY_NAME_STR));
        property->data(x);
        dataChanged = true;
    }
    if (dataChanged) {
        std::vector<double> x = findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR);
        std::vector<double> y = findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR);
        if (!x.empty() && !y.empty() && x.size() != y.size()) {
            Error("Nelson:graphics:xyDataSizeMismatch",
                "XData and YData must have the same number of elements.");
        }
        size_t count = y.size();
        expandedDelta(findVectorDoubleProperty(GO_X_NEGATIVE_DELTA_PROPERTY_NAME_STR), count,
            GO_X_NEGATIVE_DELTA_PROPERTY_NAME_STR);
        expandedDelta(findVectorDoubleProperty(GO_X_POSITIVE_DELTA_PROPERTY_NAME_STR), count,
            GO_X_POSITIVE_DELTA_PROPERTY_NAME_STR);
        expandedDelta(findVectorDoubleProperty(GO_Y_NEGATIVE_DELTA_PROPERTY_NAME_STR), count,
            GO_Y_NEGATIVE_DELTA_PROPERTY_NAME_STR);
        expandedDelta(findVectorDoubleProperty(GO_Y_POSITIVE_DELTA_PROPERTY_NAME_STR), count,
            GO_Y_POSITIVE_DELTA_PROPERTY_NAME_STR);
        limitsDirty = true;
    }
    clearChanged({ GO_COLOR_PROPERTY_NAME_STR, GO_LINE_STYLE_PROPERTY_NAME_STR,
        GO_MARKER_PROPERTY_NAME_STR, GO_X_DATA_PROPERTY_NAME_STR, GO_Y_DATA_PROPERTY_NAME_STR,
        GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_X_NEGATIVE_DELTA_PROPERTY_NAME_STR,
        GO_X_POSITIVE_DELTA_PROPERTY_NAME_STR, GO_Y_NEGATIVE_DELTA_PROPERTY_NAME_STR,
        GO_Y_POSITIVE_DELTA_PROPERTY_NAME_STR });
}
//=============================================================================
void
GOErrorBar::paintMe(RenderInterface& gc)
{
    updateState();
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    GOAxis* axis = static_cast<GOAxis*>(getParentAxis());
    if (!axis) {
        return;
    }
    std::vector<double> x = findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR);
    std::vector<double> y = findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR);
    if (x.empty() || x.size() != y.size()) {
        return;
    }
    std::vector<double> xNeg
        = expandedDelta(findVectorDoubleProperty(GO_X_NEGATIVE_DELTA_PROPERTY_NAME_STR), y.size(),
            GO_X_NEGATIVE_DELTA_PROPERTY_NAME_STR);
    std::vector<double> xPos
        = expandedDelta(findVectorDoubleProperty(GO_X_POSITIVE_DELTA_PROPERTY_NAME_STR), y.size(),
            GO_X_POSITIVE_DELTA_PROPERTY_NAME_STR);
    std::vector<double> yNeg
        = expandedDelta(findVectorDoubleProperty(GO_Y_NEGATIVE_DELTA_PROPERTY_NAME_STR), y.size(),
            GO_Y_NEGATIVE_DELTA_PROPERTY_NAME_STR);
    std::vector<double> yPos
        = expandedDelta(findVectorDoubleProperty(GO_Y_POSITIVE_DELTA_PROPERTY_NAME_STR), y.size(),
            GO_Y_POSITIVE_DELTA_PROPERTY_NAME_STR);

    GOColorProperty* colorProperty
        = static_cast<GOColorProperty*>(findProperty(GO_COLOR_PROPERTY_NAME_STR));
    if (colorProperty->isNone()) {
        return;
    }
    std::vector<double> color = colorProperty->data();
    double width = findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR);
    gc.color(color);
    gc.lineWidth(width);
    std::wstring lineStyle = findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR);
    if (lineStyle != GO_PROPERTY_VALUE_NONE_STR) {
        std::vector<double> z(y.size(), 0.);
        std::vector<double> mx;
        std::vector<double> my;
        std::vector<double> mz;
        axis->reMap(x, y, z, mx, my, mz);
        gc.setLineStyle(lineStyle);
        gc.lineSeries(mx, my, mz);
    }

    double cap = pointsToPixels(findScalarDoubleProperty(GO_CAP_SIZE_PROPERTY_NAME_STR)) / 2.;
    std::vector<PixelSegment> errorSegments;
    std::vector<PixelPoint> markerPoints;
    for (size_t k = 0; k < x.size(); ++k) {
        double x0 = 0.;
        double y0 = 0.;
        if (!mapToPixel(axis, gc, x[k], y[k], x0, y0)) {
            continue;
        }
        if (!yNeg.empty() || !yPos.empty()) {
            double low = y[k] - (yNeg.empty() ? 0. : yNeg[k]);
            double high = y[k] + (yPos.empty() ? 0. : yPos[k]);
            double xl = 0.;
            double yl = 0.;
            double xh = 0.;
            double yh = 0.;
            if (mapToPixel(axis, gc, x[k], low, xl, yl)
                && mapToPixel(axis, gc, x[k], high, xh, yh)) {
                errorSegments.push_back({ xl, yl, xh, yh });
                errorSegments.push_back({ xl - cap, yl, xl + cap, yl });
                errorSegments.push_back({ xh - cap, yh, xh + cap, yh });
            }
        }
        if (!xNeg.empty() || !xPos.empty()) {
            double low = x[k] - (xNeg.empty() ? 0. : xNeg[k]);
            double high = x[k] + (xPos.empty() ? 0. : xPos[k]);
            double xl = 0.;
            double yl = 0.;
            double xh = 0.;
            double yh = 0.;
            if (mapToPixel(axis, gc, low, y[k], xl, yl)
                && mapToPixel(axis, gc, high, y[k], xh, yh)) {
                errorSegments.push_back({ xl, yl, xh, yh });
                errorSegments.push_back({ xl, yl - cap, xl, yl + cap });
                errorSegments.push_back({ xh, yh - cap, xh, yh + cap });
            }
        }
        markerPoints.push_back({ x0, y0 });
    }

    RenderInterface::SymbolType marker
        = StringToSymbol(findStringProperty(GO_MARKER_PROPERTY_NAME_STR));
    if (marker != RenderInterface::None) {
        std::vector<double> edgeColor
            = resolvedRestrictedColor(static_cast<GOAutoFlatColorProperty*>(
                                          findProperty(GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR)),
                color);
        std::vector<double> faceColor
            = resolvedRestrictedColor(static_cast<GOAutoFlatColorProperty*>(
                                          findProperty(GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR)),
                { -1., -1., -1. });
        double markerSize = findScalarDoubleProperty(GO_MARKER_SIZE_PROPERTY_NAME_STR);
        gc.setupDirectDraw();
        gc.color(color);
        gc.lineWidth(width);
        gc.setLineStyle(L"-");
        for (const auto& segment : errorSegments) {
            gc.line(segment.x1, segment.y1, segment.x2, segment.y2);
        }
        if (isDrawableColor(edgeColor) || isDrawableColor(faceColor)) {
            std::vector<double> ux;
            std::vector<double> uy;
            ux.reserve(markerPoints.size());
            uy.reserve(markerPoints.size());
            for (const auto& point : markerPoints) {
                ux.push_back(point.x);
                uy.push_back(point.y);
            }
            // One batched primitive (RenderWeb); Qt/OpenGL return false and loop.
            if (!DrawSymbolBatchUniform(
                    gc, marker, ux, uy, markerSize, edgeColor, faceColor, width)) {
                for (const auto& point : markerPoints) {
                    DrawSymbol(
                        gc, marker, point.x, point.y, 0., markerSize, edgeColor, faceColor, width);
                }
            }
        }
        gc.releaseDirectDraw();
    } else {
        gc.setupDirectDraw();
        gc.color(color);
        gc.lineWidth(width);
        gc.setLineStyle(L"-");
        for (const auto& segment : errorSegments) {
            gc.line(segment.x1, segment.y1, segment.x2, segment.y2);
        }
        gc.releaseDirectDraw();
    }
}
//=============================================================================
std::vector<double>
GOErrorBar::getLimits()
{
    if (!limitsDirty) {
        return limits;
    }
    std::vector<double> x = findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR);
    std::vector<double> y = findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR);
    std::vector<double> xValues = finiteVector(x);
    std::vector<double> yValues = finiteVector(y);
    size_t count = y.size();
    std::vector<double> xNeg
        = expandedDelta(findVectorDoubleProperty(GO_X_NEGATIVE_DELTA_PROPERTY_NAME_STR), count,
            GO_X_NEGATIVE_DELTA_PROPERTY_NAME_STR);
    std::vector<double> xPos
        = expandedDelta(findVectorDoubleProperty(GO_X_POSITIVE_DELTA_PROPERTY_NAME_STR), count,
            GO_X_POSITIVE_DELTA_PROPERTY_NAME_STR);
    std::vector<double> yNeg
        = expandedDelta(findVectorDoubleProperty(GO_Y_NEGATIVE_DELTA_PROPERTY_NAME_STR), count,
            GO_Y_NEGATIVE_DELTA_PROPERTY_NAME_STR);
    std::vector<double> yPos
        = expandedDelta(findVectorDoubleProperty(GO_Y_POSITIVE_DELTA_PROPERTY_NAME_STR), count,
            GO_Y_POSITIVE_DELTA_PROPERTY_NAME_STR);
    for (size_t k = 0; k < x.size() && k < y.size(); ++k) {
        if (!xNeg.empty()) {
            appendValue(xValues, x[k] - xNeg[k]);
        }
        if (!xPos.empty()) {
            appendValue(xValues, x[k] + xPos[k]);
        }
        if (!yNeg.empty()) {
            appendValue(yValues, y[k] - yNeg[k]);
        }
        if (!yPos.empty()) {
            appendValue(yValues, y[k] + yPos[k]);
        }
    }
    if (xValues.empty()) {
        xValues = { 0., 1. };
    }
    if (yValues.empty()) {
        yValues = { 0., 1. };
    }
    auto [xMin, xMax] = std::minmax_element(xValues.begin(), xValues.end());
    auto [yMin, yMax] = std::minmax_element(yValues.begin(), yValues.end());
    limits = { *xMin, *xMax, *yMin, *yMax, 0., 0., 0., 0., 0., 0. };
    limitsDirty = false;
    return limits;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
