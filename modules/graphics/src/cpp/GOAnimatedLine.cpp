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
#include <cmath>
#include "GOAnimatedLine.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOAxis.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOCommonProperties.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOMaximumNumPointsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOSeriesIndexProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOSymbolProperty.hpp"
#include "MinMaxHelpers.hpp"
#include "RenderHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::vector<double>
defaultAnimatedLineColor()
{
    return { 33.0 / 255.0, 33.0 / 255.0, 33.0 / 255.0 };
}
//=============================================================================
static bool
isLineStyleToken(const std::wstring& token)
{
    return token == L"-" || token == L"--" || token == L"-." || token == L":"
        || token == GO_PROPERTY_VALUE_NONE_STR;
}
//=============================================================================
static std::wstring
lineStyleFromOrderEntry(const std::wstring& value)
{
    if (value.find(L"--") != std::wstring::npos) {
        return L"--";
    }
    if (value.find(L"-.") != std::wstring::npos) {
        return L"-.";
    }
    if (value.find(L":") != std::wstring::npos) {
        return L":";
    }
    if (value.find(L"-") != std::wstring::npos) {
        return L"-";
    }
    if (value == GO_PROPERTY_VALUE_NONE_STR) {
        return GO_PROPERTY_VALUE_NONE_STR;
    }
    return L"-";
}
//=============================================================================
static std::wstring
markerFromOrderEntry(const std::wstring& value)
{
    static const std::vector<std::wstring> longMarkers
        = { GO_PROPERTY_VALUE_SQUARE_STR, GO_PROPERTY_VALUE_DIAMOND_STR,
              GO_PROPERTY_VALUE_PENTAGRAM_STR, GO_PROPERTY_VALUE_HEXAGRAM_STR };
    for (const auto& marker : longMarkers) {
        if (value.find(marker) != std::wstring::npos) {
            return marker;
        }
    }
    static const std::vector<std::wstring> shortMarkers
        = { L"+", L"o", L"*", L"x", L"s", L"d", L"^", L"v", L">", L"<", L"p", L"h" };
    for (const auto& marker : shortMarkers) {
        if (value.find(marker) != std::wstring::npos) {
            return marker;
        }
    }
    if (value == L"." && !isLineStyleToken(value)) {
        return L".";
    }
    return GO_PROPERTY_VALUE_NONE_STR;
}
//=============================================================================
static bool
isInsideLimits(const std::vector<double>& limits, double value)
{
    if (limits.size() != 2) {
        return true;
    }
    double lo = std::min(limits[0], limits[1]);
    double hi = std::max(limits[0], limits[1]);
    return value >= lo && value <= hi;
}
//=============================================================================
static bool
isInsideAxesLimits(GOAxis* axis, double x, double y, double z)
{
    if (!axis) {
        return true;
    }
    return isInsideLimits(axis->findVectorDoubleProperty(GO_X_LIM_PROPERTY_NAME_STR), x)
        && isInsideLimits(axis->findVectorDoubleProperty(GO_Y_LIM_PROPERTY_NAME_STR), y)
        && isInsideLimits(axis->findVectorDoubleProperty(GO_Z_LIM_PROPERTY_NAME_STR), z);
}
//=============================================================================
static std::vector<double>
colorOrderColor(GOAxis* axis, double seriesIndex)
{
    if (!axis) {
        return defaultAnimatedLineColor();
    }
    std::vector<double> colorOrder(
        axis->findVectorDoubleProperty(GO_COLOR_ORDER_PROPERTY_NAME_STR));
    size_t rows = colorOrder.size() / 3;
    if (rows == 0 || !std::isfinite(seriesIndex) || seriesIndex < 1) {
        return defaultAnimatedLineColor();
    }
    size_t row = ((size_t)seriesIndex - 1) % rows;
    return { colorOrder[row * 3], colorOrder[row * 3 + 1], colorOrder[row * 3 + 2] };
}
//=============================================================================
static std::wstring
lineStyleOrderEntry(GOAxis* axis, double seriesIndex)
{
    if (!axis || !std::isfinite(seriesIndex) || seriesIndex < 1) {
        return L"-";
    }
    std::vector<std::wstring> values(
        axis->findStringVectorProperty(GO_LINE_STYLE_ORDER_PROPERTY_NAME_STR));
    if (values.empty()) {
        return L"-";
    }
    size_t row = ((size_t)seriesIndex - 1) % values.size();
    return values[row];
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
GOAnimatedLine::getType()
{
    return GO_PROPERTY_VALUE_ANIMATED_LINE_STR;
}
//=============================================================================
GOAnimatedLine::GOAnimatedLine()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
GOAnimatedLine::~GOAnimatedLine() { }
//=============================================================================
void
GOAnimatedLine::constructProperties()
{
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);

    registerProperty(new GOMaximumNumPointsProperty, GO_MAXIMUM_NUM_POINTS_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_COLOR_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_LINE_STYLE_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_ALIGN_VERTEX_CENTERS_PROPERTY_NAME_STR);
    registerProperty(new GOSymbolProperty, GO_MARKER_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_MARKER_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_MARKER_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR);

    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerCommonProperties(this);
    sortProperties();
}
//=============================================================================
void
GOAnimatedLine::setupDefaults()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    std::vector<double> color = defaultAnimatedLineColor();
    static_cast<GOColorProperty*>(findProperty(GO_COLOR_PROPERTY_NAME_STR))->data(color);
    setRestrictedStringDefault(GO_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setRestrictedStringDefault(GO_LINE_STYLE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(
        GO_ALIGN_VERTEX_CENTERS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_MARKER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setRestrictedStringDefault(GO_MARKER_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_MARKER_SIZE_PROPERTY_NAME_STR, 6);
    setRestrictedStringColorDefault(
        GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR, -1, -1, -1);
    setRestrictedStringColorDefault(
        GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR, -1, -1, -1);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    initializeCommonProperties(this);
    clearAllChanged();
}
//=============================================================================
void
GOAnimatedLine::trimToMaximumNumPoints()
{
    double maximum = static_cast<GOMaximumNumPointsProperty*>(
        findProperty(GO_MAXIMUM_NUM_POINTS_PROPERTY_NAME_STR))
                         ->data();
    if (std::isinf(maximum) || maximum >= (double)m_xData.size()) {
        return;
    }
    size_t keep = (size_t)maximum;
    size_t removeCount = m_xData.size() - keep;
    m_xData.erase(m_xData.begin(), m_xData.begin() + removeCount);
    m_yData.erase(m_yData.begin(), m_yData.begin() + removeCount);
    m_zData.erase(m_zData.begin(), m_zData.begin() + removeCount);
}
//=============================================================================
void
GOAnimatedLine::applyAutomaticStyle()
{
    GOSeriesIndexProperty* seriesIndex
        = static_cast<GOSeriesIndexProperty*>(findProperty(GO_SERIES_INDEX_PROPERTY_NAME_STR));
    GOAxis* axis = static_cast<GOAxis*>(getParentAxis());
    bool hasSeriesIndex = seriesIndex && !seriesIndex->isNone();
    double index = hasSeriesIndex ? seriesIndex->value() : 1;

    if (isAuto(GO_COLOR_MODE_PROPERTY_NAME_STR)) {
        std::vector<double> color
            = hasSeriesIndex ? colorOrderColor(axis, index) : defaultAnimatedLineColor();
        static_cast<GOColorProperty*>(findProperty(GO_COLOR_PROPERTY_NAME_STR))->data(color);
        findProperty(GO_COLOR_PROPERTY_NAME_STR)->clearModified();
    }
    std::wstring styleEntry = hasSeriesIndex ? lineStyleOrderEntry(axis, index) : L"-";
    if (isAuto(GO_LINE_STYLE_MODE_PROPERTY_NAME_STR)) {
        setRestrictedStringDefault(
            GO_LINE_STYLE_PROPERTY_NAME_STR, lineStyleFromOrderEntry(styleEntry));
        findProperty(GO_LINE_STYLE_PROPERTY_NAME_STR)->clearModified();
    }
    if (isAuto(GO_MARKER_MODE_PROPERTY_NAME_STR)) {
        setRestrictedStringDefault(GO_MARKER_PROPERTY_NAME_STR, markerFromOrderEntry(styleEntry));
        findProperty(GO_MARKER_PROPERTY_NAME_STR)->clearModified();
    }
}
//=============================================================================
void
GOAnimatedLine::updateState()
{
    if (hasChanged(GO_COLOR_PROPERTY_NAME_STR) && !hasChanged(GO_COLOR_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_COLOR_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_LINE_STYLE_PROPERTY_NAME_STR)
        && !hasChanged(GO_LINE_STYLE_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_LINE_STYLE_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_MARKER_PROPERTY_NAME_STR) && !hasChanged(GO_MARKER_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_MARKER_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_MAXIMUM_NUM_POINTS_PROPERTY_NAME_STR)) {
        trimToMaximumNumPoints();
        m_limitsDirty = true;
    }
    applyAutomaticStyle();
}
//=============================================================================
void
GOAnimatedLine::addPoints(
    const std::vector<double>& x, const std::vector<double>& y, const std::vector<double>& z)
{
    if (x.size() != y.size() || x.size() != z.size()) {
        Error(L"Nelson:animatedline:coordinateSizeMismatch",
            L"Coordinate vectors must have the same number of elements.");
    }
    m_xData.insert(m_xData.end(), x.begin(), x.end());
    m_yData.insert(m_yData.end(), y.begin(), y.end());
    m_zData.insert(m_zData.end(), z.begin(), z.end());
    trimToMaximumNumPoints();
    m_limitsDirty = true;
}
//=============================================================================
void
GOAnimatedLine::clearPoints()
{
    m_xData.clear();
    m_yData.clear();
    m_zData.clear();
    m_limitsDirty = true;
}
//=============================================================================
void
GOAnimatedLine::setPoints(
    const std::vector<double>& x, const std::vector<double>& y, const std::vector<double>& z)
{
    if (x.size() != y.size() || x.size() != z.size()) {
        Error(L"Nelson:animatedline:coordinateSizeMismatch",
            L"Coordinate vectors must have the same number of elements.");
    }
    m_xData = x;
    m_yData = y;
    m_zData = z;
    trimToMaximumNumPoints();
    m_limitsDirty = true;
}
//=============================================================================
std::vector<double>
GOAnimatedLine::xData() const
{
    return m_xData;
}
//=============================================================================
std::vector<double>
GOAnimatedLine::yData() const
{
    return m_yData;
}
//=============================================================================
std::vector<double>
GOAnimatedLine::zData() const
{
    return m_zData;
}
//=============================================================================
void
GOAnimatedLine::paintMe(RenderInterface& gc)
{
    updateState();
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR) || m_xData.empty()
        || m_yData.empty()) {
        return;
    }
    if (!(m_xData.size() == m_yData.size() && m_yData.size() == m_zData.size())) {
        return;
    }

    std::vector<double> mxs, mys, mzs;
    GOAxis* parent = static_cast<GOAxis*>(getParentAxis());
    if (parent) {
        parent->reMap(m_xData, m_yData, m_zData, mxs, mys, mzs);
    }
    if (!(mxs.size() == mys.size() && mys.size() == mzs.size())) {
        return;
    }
    bool clippingOn = stringCheck(GO_CLIPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    double width = findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR);
    gc.lineWidth(width);

    GOColorProperty* lineColorProperty
        = static_cast<GOColorProperty*>(findProperty(GO_COLOR_PROPERTY_NAME_STR));
    if (!lineColorProperty->isNone()) {
        gc.color(lineColorProperty->data());
        gc.setLineStyle(findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR));
        size_t n = 0;
        while (n < mxs.size()) {
            std::vector<double> localX;
            std::vector<double> localY;
            std::vector<double> localZ;
            while (n < mxs.size() && std::isfinite(mxs[n]) && std::isfinite(mys[n])
                && std::isfinite(mzs[n])
                && (!clippingOn
                    || isInsideAxesLimits(parent, m_xData[n], m_yData[n], m_zData[n]))) {
                localX.push_back(mxs[n]);
                localY.push_back(mys[n]);
                localZ.push_back(mzs[n]);
                n++;
            }
            gc.lineSeries(localX, localY, localZ);
            while (n < mxs.size()
                && !(std::isfinite(mxs[n]) && std::isfinite(mys[n]) && std::isfinite(mzs[n])
                    && (!clippingOn
                        || isInsideAxesLimits(parent, m_xData[n], m_yData[n], m_zData[n])))) {
                n++;
            }
        }
    }

    RenderInterface::SymbolType marker
        = StringToSymbol(findStringProperty(GO_MARKER_PROPERTY_NAME_STR));
    if (marker == RenderInterface::None) {
        return;
    }
    std::vector<double> lineColor(lineColorProperty->data());
    std::vector<double> edgeColor = resolvedRestrictedColor(
        static_cast<GOAutoFlatColorProperty*>(findProperty(GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR)),
        lineColor);
    std::vector<double> faceColor = resolvedRestrictedColor(
        static_cast<GOAutoFlatColorProperty*>(findProperty(GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR)),
        { -1, -1, -1 });
    double markerSize = findScalarDoubleProperty(GO_MARKER_SIZE_PROPERTY_NAME_STR);
    bool alignVertexCenters
        = stringCheck(GO_ALIGN_VERTEX_CENTERS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);

    std::vector<double> markerX;
    std::vector<double> markerY;
    markerX.reserve(mxs.size());
    markerY.reserve(mys.size());
    for (size_t i = 0; i < mxs.size(); i++) {
        if (clippingOn && !isInsideAxesLimits(parent, m_xData[i], m_yData[i], m_zData[i])) {
            continue;
        }
        double u, v;
        bool clipped;
        gc.toPixels(mxs[i], mys[i], mzs[i], u, v, clipped);
        if (clippingOn && clipped) {
            continue;
        }
        if (alignVertexCenters) {
            u = std::round(u) + 0.5;
            v = std::round(v) + 0.5;
        }
        markerX.push_back(u);
        markerY.push_back(v);
    }

    gc.setupDirectDraw();
    // One batched primitive (RenderWeb) instead of a marker glyph per point; a
    // backend that cannot batch (Qt/OpenGL) returns false and the loop runs.
    if (!DrawSymbolBatchUniform(
            gc, marker, markerX, markerY, markerSize, edgeColor, faceColor, width)) {
        for (size_t i = 0; i < markerX.size(); i++) {
            DrawSymbol(
                gc, marker, markerX[i], markerY[i], 0, markerSize, edgeColor, faceColor, width);
        }
    }
    gc.releaseDirectDraw();
}
//=============================================================================
std::vector<double>
GOAnimatedLine::getLimits()
{
    if (!m_limitsDirty) {
        return m_limits;
    }
    if (m_xData.empty() || m_yData.empty()) {
        m_limits.clear();
        return m_limits;
    }
    m_limits.resize(0);
    m_limits.reserve(10);
    m_limits.push_back(findVectorMin(m_xData));
    m_limits.push_back(findVectorMax(m_xData));
    m_limits.push_back(findVectorMin(m_yData));
    m_limits.push_back(findVectorMax(m_yData));
    m_limits.push_back(findVectorMin(m_zData));
    m_limits.push_back(findVectorMax(m_zData));
    m_limits.push_back(0);
    m_limits.push_back(0);
    m_limits.push_back(0);
    m_limits.push_back(0);
    m_limitsDirty = false;
    return m_limits;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
