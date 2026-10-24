//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "GOStair.hpp"
#include "GOChartDecorators.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GraphicsObject.hpp"
#include "GOAxis.hpp"
#include "GOVectorProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOSymbolProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOPickablePartsProperty.hpp"
#include "GOSeriesIndexProperty.hpp"
#include "MinMaxHelpers.hpp"
#include "RenderHelpers.hpp"
//=============================================================================
namespace Nelson {
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
static void
buildStaircase(const std::vector<double>& xs, const std::vector<double>& ys,
    std::vector<double>& lineX, std::vector<double>& lineY)
{
    // Expand the raw data points (x_i, y_i) into a staircase polyline:
    // (x_i, y_i) -> (x_{i+1}, y_i) -> (x_{i+1}, y_{i+1}). Length 2n-1.
    lineX.clear();
    lineY.clear();
    const size_t n = xs.size();
    if (n == 0) {
        return;
    }
    lineX.reserve(2 * n - 1);
    lineY.reserve(2 * n - 1);
    for (size_t i = 0; i < n; ++i) {
        lineX.push_back(xs[i]);
        lineY.push_back(ys[i]);
        if (i + 1 < n) {
            lineX.push_back(xs[i + 1]);
            lineY.push_back(ys[i]);
        }
    }
}
//=============================================================================
GOStair::GOStair()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
GOStair::~GOStair() { }
//=============================================================================
std::wstring
GOStair::getType()
{
    return GO_PROPERTY_VALUE_STAIR_STR;
}
//=============================================================================
void
GOStair::constructProperties()
{
    // Data (2-D only: no Z/Theta/R). XData/YData carry the raw data points; the staircase
    // polyline is built at paint time.
    registerProperty(new GOVectorProperty, GO_X_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_X_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_X_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Y_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_Y_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_X_DATA_SOURCE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_Y_DATA_SOURCE_PROPERTY_NAME_STR);

    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_COLOR_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_LINE_STYLE_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOSymbolProperty, GO_MARKER_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_MARKER_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_MARKER_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR);

    registerProperty(new GOOnOffProperty, GO_CLIPPING_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR, false);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    registerProperty(new GOSourceTableProperty, GO_SOURCE_TABLE_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CONTEXT_MENU_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_SELECTED_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_HIT_TEST_PROPERTY_NAME_STR);
    registerProperty(new GOPickablePartsProperty, GO_PICKABLE_PARTS_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOLazyDecoratorProperty(GODecoratorKind::AnnotationEventData),
        GO_ANNOTATION_PROPERTY_NAME_STR, false);
    registerProperty(new GOSeriesIndexProperty, GO_SERIES_INDEX_PROPERTY_NAME_STR);
    registerProperty(new GOLazyDecoratorProperty(GODecoratorKind::DataTipTemplate),
        GO_DATA_TIP_TEMPLATE_PROPERTY_NAME_STR);

    sortProperties();
}
//=============================================================================
void
GOStair::setupDefaults()
{
    setRestrictedStringDefault(GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    setRestrictedStringDefault(GO_Y_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    setStringDefault(GO_X_VARIABLE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_Y_VARIABLE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_X_DATA_SOURCE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_Y_DATA_SOURCE_PROPERTY_NAME_STR, L"");

    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 0, 0.4470, 0.7410);
    setRestrictedStringDefault(GO_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setRestrictedStringDefault(GO_LINE_STYLE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_MARKER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setRestrictedStringDefault(GO_MARKER_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_MARKER_SIZE_PROPERTY_NAME_STR, 6);
    setRestrictedStringColorDefault(
        GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR, -1, -1, -1);
    setRestrictedStringColorDefault(
        GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR, -1, -1, -1);

    setRestrictedStringDefault(GO_CLIPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_SELECTED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HIT_TEST_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_PICKABLE_PARTS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VISIBLE_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setStringDefault(GO_TAG_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_DISPLAY_NAME_PROPERTY_NAME_STR, L"");
    static_cast<GOSeriesIndexProperty*>(findProperty(GO_SERIES_INDEX_PROPERTY_NAME_STR))
        ->setValue(1.);
    clearAllChanged();

    /*
    GO_X_DATA_PROPERTY_NAME_STR, GO_Y_DATA_PROPERTY_NAME_STR
    voluntary not initialized (empty is the default here)
    */
}
//=============================================================================
void
GOStair::updateState()
{
    if (hasChanged(GO_X_DATA_PROPERTY_NAME_STR) || hasChanged(GO_Y_DATA_PROPERTY_NAME_STR)) {
        limitsDirty = true;
    }
}
//=============================================================================
void
GOStair::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    double width(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    gc.lineWidth(width);
    GOColorProperty* lc = (GOColorProperty*)findProperty(GO_COLOR_PROPERTY_NAME_STR);
    std::vector<double> xs(findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR));
    std::vector<double> ys(findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    if ((xs.size() != ys.size()) || xs.empty()) {
        return;
    }
    GOAxis* parent = static_cast<GOAxis*>(getParentAxis());
    std::vector<double> lineX, lineY;
    buildStaircase(xs, ys, lineX, lineY);
    std::vector<double> lineZ(lineX.size(), 0.0);
    std::vector<double> mlx, mly, mlz;
    if (parent) {
        parent->reMap(lineX, lineY, lineZ, mlx, mly, mlz);
    }
    std::wstring lineStyle = findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR);
    if (!lc->isNone() && lineStyle != GO_PROPERTY_VALUE_NONE_STR) {
        gc.color(lc->data());
        gc.setLineStyle(lineStyle);
        gc.lineSeries(mlx, mly, mlz);
    }
    // Markers: placed at the raw data points (not at every staircase vertex).
    std::vector<double> zs(ys.size(), 0.0);
    std::vector<double> mxs, mys, mzs;
    if (parent) {
        parent->reMap(xs, ys, zs, mxs, mys, mzs);
    }
    std::vector<double> edgeColor = resolvedRestrictedColor(
        static_cast<GOAutoFlatColorProperty*>(findProperty(GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR)),
        lc->data());
    std::vector<double> faceColor = resolvedRestrictedColor(
        static_cast<GOAutoFlatColorProperty*>(findProperty(GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR)),
        { -1, -1, -1 });
    RenderInterface::SymbolType typ
        = StringToSymbol(findStringProperty(GO_MARKER_PROPERTY_NAME_STR));
    double sze = findScalarDoubleProperty(GO_MARKER_SIZE_PROPERTY_NAME_STR);
    if (typ != RenderInterface::None) {
        std::vector<double> uc;
        std::vector<double> vc;
        uc.reserve(mxs.size());
        vc.reserve(mxs.size());
        for (int i = 0; i < mxs.size(); i++) {
            double u, v;
            bool clipped;
            gc.toPixels(mxs[i], mys[i], mzs[i], u, v, clipped);
            if (!clipped) {
                uc.push_back(u);
                vc.push_back(v);
            }
        }
        gc.setupDirectDraw();
        if (!DrawSymbolBatchUniform(gc, typ, uc, vc, sze, edgeColor, faceColor, width)) {
            for (int i = 0; i < uc.size(); i++) {
                DrawSymbol(gc, typ, uc[i], vc[i], 0, sze, edgeColor, faceColor, width);
            }
        }
        gc.releaseDirectDraw();
    }
}
//=============================================================================
std::vector<double>
GOStair::getLimits()
{
    if (!limitsDirty) {
        return limits;
    }
    std::vector<double> xs(findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR));
    std::vector<double> ys(findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    std::vector<double> zs(ys.size(), 0.0);
    limits.resize(0);
    limits.reserve(12);
    limits.push_back(findVectorMin(xs));
    limits.push_back(findVectorMax(xs));
    limits.push_back(findVectorMin(ys));
    limits.push_back(findVectorMax(ys));
    limits.push_back(findVectorMin(zs));
    limits.push_back(findVectorMax(zs));
    limits.push_back(0);
    limits.push_back(0);
    limits.push_back(0);
    limits.push_back(0);
    limitsDirty = false;
    return limits;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
