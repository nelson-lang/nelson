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
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOLineSeries.hpp"
#include "GOList.hpp"
#include "GraphicsObject.hpp"
#include "GOAxis.hpp"
#include "GOSymbolProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "MinMaxHelpers.hpp"
#include "RenderHelpers.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCommonProperties.hpp"
#include "GOHelpers.hpp"
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
// Read the ThetaZeroLocation / ThetaDir convention from the parent polar axes.
// A polar-flagged plain axes may not carry these, so every read is guarded and
// falls back to the default (zero to the right, counterclockwise).
static void
polarAngleConvention(GraphicsObject* go, double& zeroOffset, double& dirSign)
{
    constexpr double pi = 3.141592653589793238462643383279502884;
    zeroOffset = 0.0;
    dirSign = 1.0;
    GOAxis* axis = go->getParentAxis();
    if (!axis) {
        return;
    }
    GOGenericProperty* zeroProp
        = axis->findProperty(GO_THETA_ZERO_LOCATION_PROPERTY_NAME_STR, false);
    if (zeroProp) {
        std::wstring location = zeroProp->get().asWideString();
        if (location == GO_PROPERTY_VALUE_TOP_STR) {
            zeroOffset = pi / 2.0;
        } else if (location == GO_PROPERTY_VALUE_LEFT_STR) {
            zeroOffset = pi;
        } else if (location == GO_PROPERTY_VALUE_BOTTOM_STR) {
            zeroOffset = -pi / 2.0;
        }
    }
    GOGenericProperty* dirProp = axis->findProperty(GO_THETA_DIR_PROPERTY_NAME_STR, false);
    if (dirProp && dirProp->get().asWideString() == GO_PROPERTY_VALUE_CLOCKWISE_STR) {
        dirSign = -1.0;
    }
}
//=============================================================================
// Read the RDir convention from the parent polar axes. 'reverse' mirrors each
// radius about the RLim span, so r=RLim(1) sits at the outer edge. Guarded: a
// polar-flagged plain axes may carry neither RDir nor RLim.
static void
polarRadialConvention(GraphicsObject* go, bool& reverse, double& rSpanSum)
{
    reverse = false;
    rSpanSum = 0.0;
    GOAxis* axis = go->getParentAxis();
    if (!axis) {
        return;
    }
    GOGenericProperty* dirProp = axis->findProperty(GO_R_DIR_PROPERTY_NAME_STR, false);
    if (!dirProp || dirProp->get().asWideString() != GO_PROPERTY_VALUE_REVERSE_STR) {
        return;
    }
    std::vector<double> rLim(axis->findVectorDoubleProperty(GO_R_LIM_PROPERTY_NAME_STR));
    if (rLim.size() >= 2) {
        reverse = true;
        rSpanSum = rLim[0] + rLim[1];
    }
}
//=============================================================================
static bool
buildPolarLineData(GraphicsObject* go, std::vector<double>& xs, std::vector<double>& ys)
{
    std::vector<double> theta(go->findVectorDoubleProperty(GO_THETA_DATA_PROPERTY_NAME_STR));
    std::vector<double> radius(go->findVectorDoubleProperty(GO_R_DATA_PROPERTY_NAME_STR));
    if (theta.empty() || theta.size() != radius.size()) {
        return false;
    }
    double zeroOffset = 0.0;
    double dirSign = 1.0;
    polarAngleConvention(go, zeroOffset, dirSign);
    bool reverse = false;
    double rSpanSum = 0.0;
    polarRadialConvention(go, reverse, rSpanSum);
    xs.resize(theta.size());
    ys.resize(theta.size());
    OMP_PARALLEL_FOR_LOOP(theta.size())
    for (ompIndexType i = 0; i < (ompIndexType)theta.size(); i++) {
        double angle = zeroOffset + dirSign * theta[i];
        double value = radius[i];
        if (reverse) {
            value = rSpanSum - value;
        }
        if (std::isfinite(value) && value < 0) {
            constexpr double pi = 3.141592653589793238462643383279502884;
            angle += pi;
            value = -value;
        }
        xs[i] = value * std::cos(angle);
        ys[i] = value * std::sin(angle);
    }
    return true;
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
std::wstring
GOLineSeries::getType()
{
    return _type.empty() ? GO_PROPERTY_VALUE_LINE_STR : _type;
}
//=============================================================================
wstringVector
GOLineSeries::getVisibleFieldnames()
{
    wstringVector fieldnames = GraphicsObject::getVisibleFieldnames();
    // RData/ThetaData are exposed on a Line only for a polar axes (polarplot). Cartesian
    // lines (plot, compass, feather) share this class but hide them. They stay usable
    // through findProperty, which drives the polar-data machinery. Types with a dedicated
    // Type (stair, functionline, ...) use the static mask instead.
    if (getType() == GO_PROPERTY_VALUE_LINE_STR) {
        GOAxis* axis = getParentAxis();
        const bool polar = axis && axis->isPolarAxes();
        if (!polar) {
            static const wstringVector polarLineProps
                = { GO_R_DATA_PROPERTY_NAME_STR, GO_R_DATA_MODE_PROPERTY_NAME_STR,
                      GO_R_DATA_SOURCE_PROPERTY_NAME_STR, GO_R_VARIABLE_PROPERTY_NAME_STR,
                      GO_THETA_DATA_PROPERTY_NAME_STR, GO_THETA_DATA_MODE_PROPERTY_NAME_STR,
                      GO_THETA_DATA_SOURCE_PROPERTY_NAME_STR, GO_THETA_VARIABLE_PROPERTY_NAME_STR };
            fieldnames.erase(std::remove_if(fieldnames.begin(), fieldnames.end(),
                                 [](const std::wstring& name) {
                                     return std::find(
                                                polarLineProps.begin(), polarLineProps.end(), name)
                                         != polarLineProps.end();
                                 }),
                fieldnames.end());
        }
    }
    return fieldnames;
}
//=============================================================================
GOLineSeries::GOLineSeries() : GOLineSeries(GO_PROPERTY_VALUE_LINE_STR) { }
//=============================================================================
GOLineSeries::GOLineSeries(const std::wstring& type) : _type(type)
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
GOLineSeries::~GOLineSeries() { }
//=============================================================================
void
GOLineSeries::constructProperties()
{
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);

    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOSymbolProperty, GO_MARKER_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_MARKER_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_X_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Y_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Z_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_X_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_Z_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_X_DATA_SOURCE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_Y_DATA_SOURCE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_Z_DATA_SOURCE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_X_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_Y_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_Z_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_THETA_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_THETA_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_R_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_R_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerCommonProperties(this);
    sortProperties();
}
//=============================================================================
void
GOLineSeries::setupDefaults()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 0, 0.4470, 0.7410);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_MARKER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setRestrictedStringColorDefault(
        GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR, -1, -1, -1);
    setRestrictedStringColorDefault(
        GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR, -1, -1, -1);
    setScalarDoubleDefault(GO_MARKER_SIZE_PROPERTY_NAME_STR, 6);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    setRestrictedStringDefault(GO_Y_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    setRestrictedStringDefault(GO_Z_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setStringDefault(GO_X_DATA_SOURCE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_Y_DATA_SOURCE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_Z_DATA_SOURCE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_X_VARIABLE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_Y_VARIABLE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_Z_VARIABLE_PROPERTY_NAME_STR, L"");
    setRestrictedStringDefault(GO_THETA_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    setRestrictedStringDefault(GO_R_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    initializeCommonProperties(this);

    /*
    GO_X_DATA_PROPERTY_NAME_STR, GO_Y_DATA_PROPERTY_NAME_STR, GO_Z_DATA_PROPERTY_NAME_STR
    voluntary not initialized (empty is the default here)
    */
}
//=============================================================================
static void
refreshFloorProjection(GOLineSeries* projection)
{
    if (!projection->stringCheck(GO_TAG_PROPERTY_NAME_STR, L"__web_floor_projection__")) {
        return;
    }
    const ArrayOf sourceValue = projection->findProperty(GO_USER_DATA_PROPERTY_NAME_STR)->get();
    auto* source = sourceValue.isGraphicsObject() && sourceValue.isScalar()
        ? dynamic_cast<GOLineSeries*>(
              findGraphicsObject(sourceValue.asGraphicsObjectScalar(), false))
        : nullptr;
    auto* axis = dynamic_cast<GOAxis*>(projection->getParentAxis());
    if (source == nullptr || source == projection || axis == nullptr) {
        projection->setStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
        return;
    }
    for (const auto* name : { GO_X_DATA_PROPERTY_NAME_STR, GO_Y_DATA_PROPERTY_NAME_STR }) {
        projection->findProperty(name)->set(source->findProperty(name)->get());
    }
    const auto xs = source->findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR);
    const auto limits = axis->findVectorDoubleProperty(GO_Z_LIM_PROPERTY_NAME_STR);
    auto* zs
        = static_cast<GOVectorProperty*>(projection->findProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    zs->data(std::vector<double>(xs.size(), limits.empty() ? 0.0 : limits[0]));
    projection->findProperty(GO_VISIBLE_PROPERTY_NAME_STR)
        ->set(source->findProperty(GO_VISIBLE_PROPERTY_NAME_STR)->get());
}
//=============================================================================
void
GOLineSeries::updateState()
{
    refreshFloorProjection(this);
    bool xDataChanged = hasChanged(GO_X_DATA_PROPERTY_NAME_STR);
    bool yDataChanged = hasChanged(GO_Y_DATA_PROPERTY_NAME_STR);
    bool zDataChanged = hasChanged(GO_Z_DATA_PROPERTY_NAME_STR);
    bool thetaDataChanged = hasChanged(GO_THETA_DATA_PROPERTY_NAME_STR);
    bool rDataChanged = hasChanged(GO_R_DATA_PROPERTY_NAME_STR);

    if (xDataChanged || yDataChanged || zDataChanged || thetaDataChanged || rDataChanged) {
        limitsDirty = true;
    }

    std::vector<double> xs(findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR));
    std::vector<double> ys(findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    std::vector<double> zs(findVectorDoubleProperty(GO_Z_DATA_PROPERTY_NAME_STR));

    // If X is in auto mode, generate X = 1..N only when we have Y values.
    if (isAuto(GO_X_DATA_MODE_PROPERTY_NAME_STR)) {
        if (!ys.empty()) {
            xs.clear();
            xs.resize(ys.size());
            OMP_PARALLEL_FOR_LOOP(ys.size())
            for (ompIndexType i = 0; i < (ompIndexType)ys.size(); ++i) {
                xs[i] = (static_cast<double>(i) + 1.0);
            }
        } else {
            xs.clear();
        }
    }

    bool zsEmpty = zs.empty();

    if (xDataChanged || isAuto(GO_X_DATA_MODE_PROPERTY_NAME_STR) || yDataChanged) {
        GOVectorProperty* sp
            = static_cast<GOVectorProperty*>(findProperty(GO_X_DATA_PROPERTY_NAME_STR));
        sp->data(xs);
    }

    // An explicit change (including a clear) writes Z. Otherwise an empty Z is left alone.
    if (zDataChanged || !zsEmpty) {
        GOVectorProperty* spz
            = static_cast<GOVectorProperty*>(findProperty(GO_Z_DATA_PROPERTY_NAME_STR));
        spz->data(zs);
    }
}
//=============================================================================
void
GOLineSeries::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    double width(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    gc.lineWidth(width);
    GOColorProperty* lc = (GOColorProperty*)findProperty(GO_COLOR_PROPERTY_NAME_STR);
    std::vector<double> xs(findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR));
    std::vector<double> ys(findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    std::vector<double> zs(findVectorDoubleProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    buildPolarLineData(this, xs, ys);
    if (zs.size() == 0) {
        zs.resize(ys.size());
        OMP_PARALLEL_FOR_LOOP(ys.size())
        for (ompIndexType i = 0; i < (ompIndexType)ys.size(); i++) {
            zs[i] = 0.0;
        }
    }
    if (!((xs.size() == ys.size()) && (ys.size() == zs.size()))) {
        return;
    }
    std::vector<double> mxs, mys, mzs;
    GOAxis* parent = static_cast<GOAxis*>(getParentAxis());
    if (parent) {
        parent->reMap(xs, ys, zs, mxs, mys, mzs);
    }
    std::wstring lineStyle = findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR);
    if (!lc->isNone() && lineStyle != GO_PROPERTY_VALUE_NONE_STR) {
        gc.color(lc->data());
        gc.setLineStyle(lineStyle);
        gc.lineSeries(mxs, mys, mzs);
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
        std::vector<double> dc;
        uc.reserve(mxs.size());
        vc.reserve(mxs.size());
        dc.reserve(mxs.size());
        for (int i = 0; i < mxs.size(); i++) {
            double u, v, d;
            bool clipped;
            // The clip-space z (d) lets markers be depth-tested against opaque surfaces in
            // front of them.
            gc.toPixels(mxs[i], mys[i], mzs[i], u, v, d, clipped);
            if (!clipped) {
                uc.push_back(u);
                vc.push_back(v);
                dc.push_back(d);
            }
        }
        gc.setupDirectDraw();
        // One batched primitive (RenderWeb) instead of a polygon set per marker. A backend
        // that cannot batch (Qt/OpenGL) returns false and the per-point loop runs.
        if (!DrawSymbolBatchUniform(gc, typ, uc, vc, sze, edgeColor, faceColor, width, dc)) {
            for (int i = 0; i < uc.size(); i++) {
                if (gc.markerOccluded(uc[i], vc[i], dc[i])) {
                    continue;
                }
                DrawSymbol(gc, typ, uc[i], vc[i], 0, sze, edgeColor, faceColor, width);
            }
        }
        gc.releaseDirectDraw();
    }
}
//=============================================================================
std::vector<double>
GOLineSeries::getLimits()
{
    if (stringCheck(GO_TAG_PROPERTY_NAME_STR, L"__web_floor_projection__")) {
        return {};
    }
    if (!limitsDirty) {
        return limits;
    }
    std::vector<double> xs(findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR));
    std::vector<double> ys(findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    std::vector<double> zs(findVectorDoubleProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    buildPolarLineData(this, xs, ys);
    if (zs.size() == 0) {
        zs.resize(ys.size());
        OMP_PARALLEL_FOR_LOOP(ys.size())
        for (ompIndexType i = 0; i < (ompIndexType)ys.size(); i++) {
            zs[i] = 0.0;
        }
    }
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
