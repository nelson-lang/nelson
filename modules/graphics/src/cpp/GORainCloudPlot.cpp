//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GORainCloudPlot.hpp"
#include "GOChartDecorators.hpp"
#include "ViolinDensityHelpers.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOAxis.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOPickablePartsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOSeriesIndexProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOSymbolProperty.hpp"
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
    static const std::vector<std::wstring> ORIENTATION_VALUES = { L"horizontal", L"vertical" };
    // Rain band layout, as fractions of half the density width.
    static constexpr double RAIN_GAP = 0.08;
    static constexpr double RAIN_MIN_SPREAD = 0.2;
    //=============================================================================
    static std::vector<double>
    defaultSeriesColor();
    static std::vector<double>
    numericVector(const ArrayOf& value);
    static std::vector<double>
    automaticColor(GraphicsObject* object);
    static void
    setColorSpec(
        GraphicsObject* object, const std::wstring& propertyName, const std::vector<double>& color);
    static void
    syncColorMode(
        GraphicsObject* object, const std::wstring& propertyName, const std::wstring& modeName);
    static std::vector<double>
    drawableColor(GraphicsObject* object, const std::wstring& propertyName);
    static void
    validatePositive(GraphicsObject* object, const std::wstring& name);
    static std::vector<double>
    rainOffsets(const std::vector<double>& values, const std::vector<double>& centers,
        const std::vector<double>& widths, double halfWidth);
    static void
    registerRainCloudProperties(GraphicsObject* go);
    static void
    setRainCloudDefaults(GraphicsObject* go);
    //=============================================================================
} // namespace
//=============================================================================
GORainCloudPlot::GORainCloudPlot()
{
    registerRainCloudProperties(this);
    setRainCloudDefaults(this);
    clearAllChanged();
    sortProperties();
}
//=============================================================================
GORainCloudPlot::~GORainCloudPlot() = default;
//=============================================================================
std::wstring
GORainCloudPlot::getType()
{
    return GO_PROPERTY_VALUE_RAIN_CLOUD_PLOT_STR;
}
//=============================================================================
void
GORainCloudPlot::updateState()
{
    if (hasChanged(GO_X_DATA_PROPERTY_NAME_STR) && !hasChanged(GO_X_DATA_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_X_DATA_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_DATA_PROPERTY_NAME_STR) && !hasChanged(GO_Y_DATA_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_Y_DATA_MODE_PROPERTY_NAME_STR);
    }
    syncColorMode(this, GO_FACE_COLOR_PROPERTY_NAME_STR, GO_FACE_COLOR_MODE_PROPERTY_NAME_STR);
    syncColorMode(this, GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_EDGE_COLOR_MODE_PROPERTY_NAME_STR);
    syncColorMode(
        this, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, GO_MARKER_FACE_COLOR_MODE_PROPERTY_NAME_STR);
    syncColorMode(
        this, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, GO_MARKER_EDGE_COLOR_MODE_PROPERTY_NAME_STR);
    validatePositive(this, GO_DENSITY_WIDTH_PROPERTY_NAME_STR);
    validatePositive(this, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    validatePositive(this, GO_MARKER_SIZE_PROPERTY_NAME_STR);
    double alpha = findScalarDoubleProperty(GO_FACE_ALPHA_PROPERTY_NAME_STR);
    if (!std::isfinite(alpha) || alpha < 0. || alpha > 1.) {
        Error(L"Nelson:raincloudplot:outOfRange", L"%s is outside its valid range.",
            GO_FACE_ALPHA_PROPERTY_NAME_STR);
    }
    if (hasChanged({ GO_X_DATA_PROPERTY_NAME_STR, GO_Y_DATA_PROPERTY_NAME_STR,
            GO_DENSITY_WIDTH_PROPERTY_NAME_STR })) {
        shapesDirty = true;
    }
    resolveColors();
    clearChanged({ GO_X_DATA_PROPERTY_NAME_STR, GO_Y_DATA_PROPERTY_NAME_STR,
        GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_Y_DATA_MODE_PROPERTY_NAME_STR,
        GO_DENSITY_WIDTH_PROPERTY_NAME_STR, GO_FACE_COLOR_PROPERTY_NAME_STR,
        GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR,
        GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, GO_FACE_COLOR_MODE_PROPERTY_NAME_STR,
        GO_EDGE_COLOR_MODE_PROPERTY_NAME_STR, GO_MARKER_FACE_COLOR_MODE_PROPERTY_NAME_STR,
        GO_MARKER_EDGE_COLOR_MODE_PROPERTY_NAME_STR, GO_SERIES_INDEX_PROPERTY_NAME_STR });
}
//=============================================================================
void
GORainCloudPlot::resolveColors()
{
    if (isAuto(GO_FACE_COLOR_MODE_PROPERTY_NAME_STR)) {
        setColorSpec(this, GO_FACE_COLOR_PROPERTY_NAME_STR, automaticColor(this));
    }
    if (isAuto(GO_EDGE_COLOR_MODE_PROPERTY_NAME_STR)) {
        setColorSpec(this, GO_EDGE_COLOR_PROPERTY_NAME_STR,
            drawableColor(this, GO_FACE_COLOR_PROPERTY_NAME_STR));
    }
    if (isAuto(GO_MARKER_FACE_COLOR_MODE_PROPERTY_NAME_STR)) {
        setColorSpec(this, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR,
            drawableColor(this, GO_FACE_COLOR_PROPERTY_NAME_STR));
    }
    if (isAuto(GO_MARKER_EDGE_COLOR_MODE_PROPERTY_NAME_STR)) {
        setColorSpec(this, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR,
            drawableColor(this, GO_EDGE_COLOR_PROPERTY_NAME_STR));
    }
}
//=============================================================================
void
GORainCloudPlot::updateShapes()
{
    std::vector<double> y = numericVector(
        static_cast<GOArrayOfProperty*>(findProperty(GO_Y_DATA_PROPERTY_NAME_STR))->data());
    std::vector<double> x = numericVector(
        static_cast<GOArrayOfProperty*>(findProperty(GO_X_DATA_PROPERTY_NAME_STR))->data());
    if (x.empty()) {
        x.assign(y.size(), 1.);
    }
    double halfWidth = findScalarDoubleProperty(GO_DENSITY_WIDTH_PROPERTY_NAME_STR) / 2.;
    std::vector<double> positions = ViolinDensityHelpers::uniquePositions(x, y);
    groups.clear();
    groups.resize(positions.size());
    double globalMax = 0.;
    for (size_t k = 0; k < positions.size(); ++k) {
        Group& group = groups[k];
        group.position = positions[k];
        group.values = ViolinDensityHelpers::valuesAtPosition(x, y, positions[k]);
        ViolinDensityHelpers::densityShape(group.values, group.centers, group.widths);
        for (double width : group.widths) {
            globalMax = std::max(globalMax, width);
        }
    }
    for (Group& group : groups) {
        if (globalMax > 0.) {
            for (double& width : group.widths) {
                width = halfWidth * width / globalMax;
            }
        }
        group.rainOffsets = rainOffsets(group.values, group.centers, group.widths, halfWidth);
    }
    shapesDirty = false;
}
//=============================================================================
void
GORainCloudPlot::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    updateState();
    if (shapesDirty) {
        updateShapes();
    }
    auto* axis = static_cast<GOAxis*>(getParentAxis());
    if (groups.empty() || !axis) {
        return;
    }
    bool horizontal
        = findStringProperty(GO_ORIENTATION_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_HORIZONTAL_STR;
    double faceAlpha = findScalarDoubleProperty(GO_FACE_ALPHA_PROPERTY_NAME_STR);
    double lineWidth = findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR);
    std::vector<double> faceColor = drawableColor(this, GO_FACE_COLOR_PROPERTY_NAME_STR);
    std::vector<double> edgeColor = drawableColor(this, GO_EDGE_COLOR_PROPERTY_NAME_STR);
    if (!faceColor.empty()) {
        faceColor.push_back(faceAlpha);
    }
    gc.lineWidth(lineWidth);
    gc.setLineStyle(L"-");
    std::vector<double> xs, ys, zs;
    for (const Group& group : groups) {
        ViolinDensityHelpers::drawViolin(gc, this, group.position, group.values, group.centers,
            group.widths, GO_PROPERTY_VALUE_POSITIVE_STR, horizontal, faceColor, edgeColor);
        for (size_t k = 0; k < group.values.size(); ++k) {
            double position = group.position - group.rainOffsets[k];
            xs.push_back(horizontal ? group.values[k] : position);
            ys.push_back(horizontal ? position : group.values[k]);
        }
    }
    RenderInterface::SymbolType symbol
        = StringToSymbol(findStringProperty(GO_MARKER_PROPERTY_NAME_STR));
    std::vector<double> markerEdge = drawableColor(this, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR);
    std::vector<double> markerFace = drawableColor(this, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR);
    if (symbol == RenderInterface::None || (markerEdge.empty() && markerFace.empty())) {
        return;
    }
    markerEdge = markerEdge.empty() ? std::vector<double> { -1., -1., -1. } : markerEdge;
    if (markerFace.empty()) {
        markerFace = { -1., -1., -1. };
    } else {
        markerFace.push_back(faceAlpha);
    }
    zs.assign(xs.size(), 0.);
    std::vector<double> mxs, mys, mzs;
    axis->reMap(xs, ys, zs, mxs, mys, mzs);
    double size = std::sqrt(findScalarDoubleProperty(GO_MARKER_SIZE_PROPERTY_NAME_STR));
    std::vector<double> us, vs;
    us.reserve(mxs.size());
    vs.reserve(mxs.size());
    for (size_t k = 0; k < mxs.size(); ++k) {
        double u, v;
        bool clipped;
        gc.toPixels(mxs[k], mys[k], mzs[k], u, v, clipped);
        if (!clipped) {
            us.push_back(u);
            vs.push_back(v);
        }
    }
    gc.setupDirectDraw();
    // One batched primitive (RenderWeb) instead of a marker glyph per point; a
    // backend that cannot batch (Qt/OpenGL) returns false and the loop runs.
    if (!DrawSymbolBatchUniform(gc, symbol, us, vs, size, markerEdge, markerFace, lineWidth)) {
        for (size_t k = 0; k < us.size(); ++k) {
            DrawSymbol(gc, symbol, us[k], vs[k], 0, size, markerEdge, markerFace, lineWidth);
        }
    }
    gc.releaseDirectDraw();
}
//=============================================================================
std::vector<double>
GORainCloudPlot::getLimits()
{
    if (shapesDirty) {
        updateShapes();
    }
    double minPosition = std::numeric_limits<double>::infinity();
    double maxPosition = -std::numeric_limits<double>::infinity();
    double minValue = std::numeric_limits<double>::infinity();
    double maxValue = -std::numeric_limits<double>::infinity();
    for (const Group& group : groups) {
        minPosition = std::min(minPosition, group.position);
        maxPosition = std::max(maxPosition, group.position);
        if (!group.centers.empty()) {
            minValue = std::min(minValue, group.centers.front());
            maxValue = std::max(maxValue, group.centers.back());
        }
    }
    if (!std::isfinite(minPosition)) {
        minPosition = 1.;
        maxPosition = 1.;
    }
    double margin
        = std::max(0.5, findScalarDoubleProperty(GO_DENSITY_WIDTH_PROPERTY_NAME_STR) / 2.);
    minPosition -= margin;
    maxPosition += margin;
    if (!std::isfinite(minValue) || !std::isfinite(maxValue)) {
        minValue = 0.;
        maxValue = 1.;
    }
    bool horizontal
        = findStringProperty(GO_ORIENTATION_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_HORIZONTAL_STR;
    if (horizontal) {
        limits = { minValue, maxValue, minPosition, maxPosition, 0., 0., 0., 1., 1., 1. };
    } else {
        limits = { minPosition, maxPosition, minValue, maxValue, 0., 0., 0., 1., 1., 1. };
    }
    return limits;
}
//=============================================================================
namespace {
    //=============================================================================
    std::vector<double>
    defaultSeriesColor()
    {
        return { 0., 0.447, 0.741 };
    }
    //=============================================================================
    std::vector<double>
    numericVector(const ArrayOf& value)
    {
        if (value.isEmpty()) {
            return {};
        }
        if (!value.isNumeric() || value.isComplex()) {
            Error("Nelson:graphics:realNumericExpected", "Real numeric data expected.");
        }
        ArrayOf asDouble(value);
        asDouble.promoteType(NLS_DOUBLE);
        return asDouble.asDoubleVector();
    }
    //=============================================================================
    std::vector<double>
    automaticColor(GraphicsObject* object)
    {
        auto* seriesIndex = static_cast<GOSeriesIndexProperty*>(
            object->findProperty(GO_SERIES_INDEX_PROPERTY_NAME_STR));
        double index = (!seriesIndex || seriesIndex->isNone()) ? 1. : seriesIndex->value();
        GraphicsObject* axis = object->getParentAxis();
        if (!axis || !std::isfinite(index) || index < 1) {
            return defaultSeriesColor();
        }
        std::vector<double> colorOrder(
            axis->findVectorDoubleProperty(GO_COLOR_ORDER_PROPERTY_NAME_STR));
        size_t rows = colorOrder.size() / 3;
        if (rows == 0) {
            return defaultSeriesColor();
        }
        size_t row = (static_cast<size_t>(index) - 1) % rows;
        return { colorOrder[row * 3], colorOrder[row * 3 + 1], colorOrder[row * 3 + 2] };
    }
    //=============================================================================
    void
    setColorSpec(
        GraphicsObject* object, const std::wstring& propertyName, const std::vector<double>& color)
    {
        if (color.size() < 3) {
            object->setRestrictedStringDefault(propertyName, GO_PROPERTY_VALUE_NONE_STR);
        } else {
            object->setRestrictedStringColorDefault(
                propertyName, GO_PROPERTY_VALUE_COLORSPEC_STR, color[0], color[1], color[2]);
        }
        object->findProperty(propertyName)->clearModified();
    }
    //=============================================================================
    void
    syncColorMode(
        GraphicsObject* object, const std::wstring& propertyName, const std::wstring& modeName)
    {
        if (!object->hasChanged(propertyName) || object->hasChanged(modeName)) {
            return;
        }
        auto* property = static_cast<GOAutoFlatColorProperty*>(object->findProperty(propertyName));
        if (property->isEqual(GO_PROPERTY_VALUE_AUTO_STR)) {
            object->toAuto(modeName);
        } else {
            object->toManual(modeName);
        }
    }
    //=============================================================================
    // RGB color of a color property, or an empty vector for 'none'.
    std::vector<double>
    drawableColor(GraphicsObject* object, const std::wstring& propertyName)
    {
        auto* property = static_cast<GOAutoFlatColorProperty*>(object->findProperty(propertyName));
        if (property->isEqual(GO_PROPERTY_VALUE_NONE_STR)) {
            return {};
        }
        std::vector<double> color = property->colorSpec();
        if (color.size() < 3 || color[0] < 0.) {
            return defaultSeriesColor();
        }
        return { color[0], color[1], color[2] };
    }
    //=============================================================================
    void
    validatePositive(GraphicsObject* object, const std::wstring& name)
    {
        double value = object->findScalarDoubleProperty(name);
        if (!std::isfinite(value) || value <= 0.) {
            Error(L"Nelson:graphics:mustBePositiveFinite", L"%s must be a positive finite scalar.",
                name);
        }
    }
    //=============================================================================
    // Distance of each sample below the cloud baseline: a deterministic
    // low-discrepancy jitter whose spread follows the local density, so the
    // rain widens where the cloud is thick (swarm-like layout).
    std::vector<double>
    rainOffsets(const std::vector<double>& values, const std::vector<double>& centers,
        const std::vector<double>& widths, double halfWidth)
    {
        std::vector<double> offsets(values.size(), 0.);
        if (centers.size() < 2 || widths.size() != centers.size()) {
            return offsets;
        }
        const double goldenRatio = 0.6180339887498949;
        double first = centers.front();
        double step = (centers.back() - first) / static_cast<double>(centers.size() - 1);
        double gap = RAIN_GAP * halfWidth;
        double minSpread = RAIN_MIN_SPREAD * halfWidth;
        size_t last = centers.size() - 1;
        for (size_t k = 0; k < values.size(); ++k) {
            double t = step > 0. ? (values[k] - first) / step : 0.;
            t = std::min(static_cast<double>(last), std::max(0., t));
            size_t i = std::min(last - 1, static_cast<size_t>(t));
            double fraction = t - static_cast<double>(i);
            double density = widths[i] + fraction * (widths[i + 1] - widths[i]);
            double spread = std::max(density, minSpread) - gap;
            double jitter = std::fmod(static_cast<double>(k + 1) * goldenRatio, 1.);
            offsets[k] = gap + jitter * spread;
        }
        return offsets;
    }
    //=============================================================================
    void
    registerRainCloudProperties(GraphicsObject* go)
    {
        go->registerProperty(new GOArrayOfProperty, GO_X_DATA_PROPERTY_NAME_STR);
        go->registerProperty(new GOArrayOfProperty, GO_Y_DATA_PROPERTY_NAME_STR);
        go->registerProperty(new GOAutoManualProperty, GO_X_DATA_MODE_PROPERTY_NAME_STR);
        go->registerProperty(new GOAutoManualProperty, GO_Y_DATA_MODE_PROPERTY_NAME_STR);
        go->registerProperty(new GOStringProperty, GO_X_VARIABLE_PROPERTY_NAME_STR);
        go->registerProperty(new GOStringProperty, GO_Y_VARIABLE_PROPERTY_NAME_STR);
        go->registerProperty(new GOSourceTableProperty, GO_SOURCE_TABLE_PROPERTY_NAME_STR);
        go->registerProperty(new GOScalarProperty, GO_DENSITY_WIDTH_PROPERTY_NAME_STR);
        go->registerProperty(
            new GORestrictedStringProperty(ORIENTATION_VALUES), GO_ORIENTATION_PROPERTY_NAME_STR);
        go->registerProperty(new GOAutoFlatColorProperty, GO_FACE_COLOR_PROPERTY_NAME_STR);
        go->registerProperty(new GOAutoManualProperty, GO_FACE_COLOR_MODE_PROPERTY_NAME_STR);
        go->registerProperty(new GOScalarProperty, GO_FACE_ALPHA_PROPERTY_NAME_STR);
        go->registerProperty(new GOAutoFlatColorProperty, GO_EDGE_COLOR_PROPERTY_NAME_STR);
        go->registerProperty(new GOAutoManualProperty, GO_EDGE_COLOR_MODE_PROPERTY_NAME_STR);
        go->registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
        go->registerProperty(new GOSeriesIndexProperty, GO_SERIES_INDEX_PROPERTY_NAME_STR);
        go->registerProperty(new GOSymbolProperty, GO_MARKER_PROPERTY_NAME_STR);
        go->registerProperty(new GOScalarProperty, GO_MARKER_SIZE_PROPERTY_NAME_STR);
        go->registerProperty(new GOAutoFlatColorProperty, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR);
        go->registerProperty(new GOAutoManualProperty, GO_MARKER_FACE_COLOR_MODE_PROPERTY_NAME_STR);
        go->registerProperty(new GOAutoFlatColorProperty, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR);
        go->registerProperty(new GOAutoManualProperty, GO_MARKER_EDGE_COLOR_MODE_PROPERTY_NAME_STR);
        go->registerProperty(new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
        go->registerProperty(new GOLazyDecoratorProperty(GODecoratorKind::AnnotationEventData),
            GO_ANNOTATION_PROPERTY_NAME_STR);
        go->registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
        go->registerProperty(new GOLazyDecoratorProperty(GODecoratorKind::DataTipTemplate),
            GO_DATA_TIP_TEMPLATE_PROPERTY_NAME_STR, false);
        go->registerProperty(new GOGObjectsProperty, GO_CONTEXT_MENU_PROPERTY_NAME_STR);
        go->registerProperty(new GOOnOffProperty, GO_SELECTED_PROPERTY_NAME_STR);
        go->registerProperty(new GOOnOffProperty, GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR);
        go->registerProperty(new GOOnOffProperty, GO_CLIPPING_PROPERTY_NAME_STR);
        go->registerProperty(new GOCallbackProperty, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
        go->registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
        go->registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
        go->registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
        go->registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
        go->registerProperty(new GOPickablePartsProperty, GO_PICKABLE_PARTS_PROPERTY_NAME_STR);
        go->registerProperty(new GOOnOffProperty, GO_HIT_TEST_PROPERTY_NAME_STR);
        go->registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
        go->registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
        go->registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR, false);
        go->registerProperty(
            new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
        go->registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
        go->registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
        go->registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    }
    //=============================================================================
    void
    setRainCloudDefaults(GraphicsObject* go)
    {
        go->setRestrictedStringDefault(
            GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
        go->setRestrictedStringDefault(
            GO_Y_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
        go->setStringDefault(GO_X_VARIABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
        go->setStringDefault(GO_Y_VARIABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
        go->setScalarDoubleDefault(GO_DENSITY_WIDTH_PROPERTY_NAME_STR, 0.9);
        go->setRestrictedStringDefault(
            GO_ORIENTATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_HORIZONTAL_STR);
        for (const std::wstring& name :
            { GO_FACE_COLOR_PROPERTY_NAME_STR, GO_EDGE_COLOR_PROPERTY_NAME_STR,
                GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR }) {
            setColorSpec(go, name, defaultSeriesColor());
        }
        for (const std::wstring& name : { GO_FACE_COLOR_MODE_PROPERTY_NAME_STR,
                 GO_EDGE_COLOR_MODE_PROPERTY_NAME_STR, GO_MARKER_FACE_COLOR_MODE_PROPERTY_NAME_STR,
                 GO_MARKER_EDGE_COLOR_MODE_PROPERTY_NAME_STR }) {
            go->setRestrictedStringDefault(name, GO_PROPERTY_VALUE_AUTO_STR);
        }
        go->setScalarDoubleDefault(GO_FACE_ALPHA_PROPERTY_NAME_STR, 0.2);
        go->setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
        go->setRestrictedStringDefault(GO_MARKER_PROPERTY_NAME_STR, L"o");
        go->setScalarDoubleDefault(GO_MARKER_SIZE_PROPERTY_NAME_STR, 36.);
        go->setStringDefault(GO_DISPLAY_NAME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
        for (const std::wstring& name :
            { GO_VISIBLE_PROPERTY_NAME_STR, GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR,
                GO_CLIPPING_PROPERTY_NAME_STR, GO_INTERRUPTIBLE_PROPERTY_NAME_STR,
                GO_HIT_TEST_PROPERTY_NAME_STR, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR }) {
            go->setRestrictedStringDefault(name, GO_PROPERTY_VALUE_ON_STR);
        }
        go->setRestrictedStringDefault(GO_SELECTED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
        go->setRestrictedStringDefault(
            GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
        go->setRestrictedStringDefault(
            GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
        go->setRestrictedStringDefault(
            GO_PICKABLE_PARTS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VISIBLE_STR);
        go->setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RAIN_CLOUD_PLOT_STR);
        go->setStringDefault(GO_TAG_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
        auto* seriesIndex = static_cast<GOSeriesIndexProperty*>(
            go->findProperty(GO_SERIES_INDEX_PROPERTY_NAME_STR));
        seriesIndex->setValue(1.);
        seriesIndex->clearModified();
    }
    //=============================================================================
} // namespace
//=============================================================================
} // namespace Nelson
//=============================================================================
