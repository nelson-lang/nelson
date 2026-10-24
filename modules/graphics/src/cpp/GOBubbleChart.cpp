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
#include "GOBubbleChart.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOAxis.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOColorVectorProperty.hpp"
#include "GOCommonProperties.hpp"
#include "GOFigure.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOMappingModeProperty.hpp"
#include "GOPickablePartsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarAlphaProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOSeriesIndexProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOVectorProperty.hpp"
#include "MinMaxHelpers.hpp"
#include "RenderHelpers.hpp"
#include <algorithm>
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    std::vector<double>
    defaultBubbleColor()
    {
        return { 0, 0.4470, 0.7410 };
    }
    //=============================================================================
    bool
    isTrueColorCData(const ArrayOf& cdata, size_t pointCount)
    {
        return cdata.is2D() && cdata.columns() == 3
            && cdata.rows() >= static_cast<indexType>(pointCount);
    }
    //=============================================================================
    std::vector<double>
    trueColorAt(const double* ptrCData, indexType rows, size_t pointIndex)
    {
        return { ptrCData[pointIndex], ptrCData[pointIndex + rows],
            ptrCData[pointIndex + 2 * rows] };
    }
    //=============================================================================
    void
    refreshCDataRange(
        const double* ptrCData, indexType elementCount, bool& dirty, double& minC, double& maxC)
    {
        if (!dirty) {
            return;
        }
        if (elementCount <= 0) {
            minC = 0.;
            maxC = 1.;
        } else {
            auto range = std::minmax_element(ptrCData, ptrCData + elementCount);
            minC = *range.first;
            maxC = *range.second;
        }
        dirty = false;
    }
    //=============================================================================
    std::vector<double>
    mappedCDataColor(const double* ptrCData, indexType elementCount, size_t pointIndex,
        const std::vector<double>& colorMapData, bool& dirty, double& minC, double& maxC)
    {
        if (colorMapData.empty() || elementCount <= 0) {
            return defaultBubbleColor();
        }
        refreshCDataRange(ptrCData, elementCount, dirty, minC, maxC);
        int colormapSize = static_cast<int>(colorMapData.size() / 3);
        if (colormapSize <= 0) {
            return defaultBubbleColor();
        }
        size_t scalarIndex = std::min(pointIndex, static_cast<size_t>(elementCount - 1));
        int colorIndex = 0;
        if (maxC > minC) {
            colorIndex = static_cast<int>(
                std::round((ptrCData[scalarIndex] - minC) / (maxC - minC) * (colormapSize - 1)));
        }
        colorIndex = std::clamp(colorIndex, 0, colormapSize - 1);
        size_t offset = static_cast<size_t>(colorIndex) * 3;
        return { colorMapData[offset], colorMapData[offset + 1], colorMapData[offset + 2] };
    }
    //=============================================================================
    std::vector<double>
    cDataColorAt(const ArrayOf& cdata, const double* ptrCData, size_t pointCount, size_t pointIndex,
        const std::vector<double>& colorMapData, bool& dirty, double& minC, double& maxC)
    {
        if (cdata.isEmpty() || !ptrCData) {
            return defaultBubbleColor();
        }
        if (isTrueColorCData(cdata, pointCount)) {
            return trueColorAt(ptrCData, cdata.rows(), pointIndex);
        }
        // A shared RGB triplet is a 1x3 row. A column with one scalar per point (CData
        // [1; 2; 3]) also has three elements but is colormapped data, not a triplet.
        if (cdata.rows() == 1 && cdata.columns() == 3) {
            return { ptrCData[0], ptrCData[1], ptrCData[2] };
        }
        return mappedCDataColor(
            ptrCData, cdata.elementCount(), pointIndex, colorMapData, dirty, minC, maxC);
    }
    //=============================================================================
    double
    sizeDataAt(const std::vector<double>& sizeData, size_t pointIndex)
    {
        if (sizeData.size() == 1) {
            return sizeData[0];
        }
        if (pointIndex < sizeData.size()) {
            return sizeData[pointIndex];
        }
        return 36.;
    }
    //=============================================================================
    bool
    buildPolarBubbleData(GraphicsObject* go, std::vector<double>& xs, std::vector<double>& ys)
    {
        std::vector<double> theta(go->findVectorDoubleProperty(GO_THETA_DATA_PROPERTY_NAME_STR));
        std::vector<double> radius(go->findVectorDoubleProperty(GO_R_DATA_PROPERTY_NAME_STR));
        if (theta.empty() || theta.size() != radius.size()) {
            return false;
        }
        xs.resize(theta.size());
        ys.resize(theta.size());
        OMP_PARALLEL_FOR_LOOP(theta.size())
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(theta.size()); i++) {
            double angle = theta[i];
            double value = radius[i];
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
    void
    updateSizeRangeFromValues(
        const std::vector<double>& values, bool& hasValue, double& minValue, double& maxValue)
    {
        for (double value : values) {
            if (!std::isfinite(value)) {
                continue;
            }
            value = std::max(0., value);
            if (!hasValue) {
                minValue = value;
                maxValue = value;
                hasValue = true;
            } else {
                minValue = std::min(minValue, value);
                maxValue = std::max(maxValue, value);
            }
        }
    }
    //=============================================================================
    void
    bubbleValueRange(GOAxis* parent, const std::vector<double>& fallbackSizeData, double& minSize,
        double& maxSize)
    {
        bool hasValue = false;
        minSize = 0.;
        maxSize = 1.;
        if (parent
            && parent->findStringProperty(GO_BUBBLE_LIMITS_MODE_INTERNAL_PROPERTY_NAME_STR)
                == GO_PROPERTY_VALUE_MANUAL_STR) {
            std::vector<double> limits
                = parent->findVectorDoubleProperty(GO_BUBBLE_LIMITS_INTERNAL_PROPERTY_NAME_STR);
            if (limits.size() >= 2 && limits[1] > limits[0]) {
                minSize = limits[0];
                maxSize = limits[1];
                return;
            }
        }
        if (parent) {
            auto* children = static_cast<GOGObjectsProperty*>(
                parent->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
            if (children) {
                for (int64 handle : children->data()) {
                    GraphicsObject* child = findGraphicsObject(handle, false);
                    if (!child || child->getType() != GO_PROPERTY_VALUE_BUBBLE_CHART_STR) {
                        continue;
                    }
                    updateSizeRangeFromValues(
                        child->findVectorDoubleProperty(GO_SIZE_DATA_PROPERTY_NAME_STR), hasValue,
                        minSize, maxSize);
                }
            }
        }
        if (!hasValue) {
            updateSizeRangeFromValues(fallbackSizeData, hasValue, minSize, maxSize);
        }
        if (!hasValue) {
            minSize = 0.;
            maxSize = 1.;
            return;
        }
        if (minSize == maxSize) {
            if (maxSize > 0.) {
                minSize = maxSize * 0.9;
            } else {
                maxSize = 1.;
            }
        }
    }
    //=============================================================================
    double
    bubbleDiameter(
        double sizeValue, double minValue, double maxValue, double minDiameter, double maxDiameter)
    {
        if (maxValue <= minValue) {
            return maxDiameter;
        }
        double normalized = (std::max(0., sizeValue) - minValue) / (maxValue - minValue);
        normalized = std::clamp(normalized, 0., 1.);
        return minDiameter + std::sqrt(normalized) * (maxDiameter - minDiameter);
    }
    //=============================================================================
    std::vector<double>
    bubbleDiameterRange(GOAxis* parent)
    {
        if (!parent) {
            return { 3., 50. };
        }
        std::vector<double> range
            = parent->findVectorDoubleProperty(GO_BUBBLE_SIZE_RANGE_INTERNAL_PROPERTY_NAME_STR);
        if (range.size() < 2 || range[0] <= 0. || range[1] <= range[0]) {
            return { 3., 50. };
        }
        return range;
    }
    //=============================================================================
    void
    appendAlpha(std::vector<double>& color, const std::vector<double>& alphaData, bool isFlat,
        double scalarAlpha, size_t pointIndex)
    {
        if (!isFlat) {
            color.push_back(scalarAlpha);
            return;
        }
        if (alphaData.empty()) {
            color.push_back(1.);
        } else if (alphaData.size() == 1) {
            color.push_back(alphaData[0]);
        } else if (pointIndex < alphaData.size()) {
            color.push_back(alphaData[pointIndex]);
        } else {
            color.push_back(1.);
        }
    }
    //=============================================================================
    void
    ensureColorSpec(std::vector<double>& color)
    {
        if (color.size() < 3) {
            color = defaultBubbleColor();
        }
    }
    //=============================================================================
    constexpr double BUBBLE_RENDER_DIAMETER_SCALE = 1.0;
    //=============================================================================
    double
    bubbleDataPadding(double dataSpan, double axisPixels, double radiusPixels)
    {
        if (!std::isfinite(dataSpan) || dataSpan <= 0. || axisPixels <= 2. * radiusPixels) {
            return 0.;
        }
        return radiusPixels * dataSpan / (axisPixels - 2. * radiusPixels);
    }
    //=============================================================================
    void
    updateSpanRangeFromValues(
        const std::vector<double>& values, bool& hasValue, double& minValue, double& maxValue)
    {
        for (double value : values) {
            if (!std::isfinite(value)) {
                continue;
            }
            if (!hasValue) {
                minValue = value;
                maxValue = value;
                hasValue = true;
            } else {
                minValue = std::min(minValue, value);
                maxValue = std::max(maxValue, value);
            }
        }
    }
    //=============================================================================
    // Union of the X/Y/Z data extents over every bubble chart sharing the parent axis.
    // The edge padding must use the whole axis span, or narrow series underestimate the
    // bubble radius in data units and clip edge bubbles.
    void
    bubbleAxisDataSpan(GOAxis* parent, double& xSpan, double& ySpan, double& zSpan)
    {
        xSpan = 0.;
        ySpan = 0.;
        zSpan = 0.;
        if (!parent) {
            return;
        }
        auto* children = static_cast<GOGObjectsProperty*>(
            parent->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
        if (!children) {
            return;
        }
        bool hasX = false, hasY = false, hasZ = false;
        double xMin = 0., xMax = 0., yMin = 0., yMax = 0., zMin = 0., zMax = 0.;
        for (int64 handle : children->data()) {
            GraphicsObject* child = findGraphicsObject(handle, false);
            if (!child || child->getType() != GO_PROPERTY_VALUE_BUBBLE_CHART_STR) {
                continue;
            }
            std::vector<double> xs(child->findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR));
            std::vector<double> ys(child->findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR));
            std::vector<double> zs(child->findVectorDoubleProperty(GO_Z_DATA_PROPERTY_NAME_STR));
            buildPolarBubbleData(child, xs, ys);
            updateSpanRangeFromValues(xs, hasX, xMin, xMax);
            updateSpanRangeFromValues(ys, hasY, yMin, yMax);
            updateSpanRangeFromValues(zs, hasZ, zMin, zMax);
        }
        xSpan = hasX ? xMax - xMin : 0.;
        ySpan = hasY ? yMax - yMin : 0.;
        zSpan = hasZ ? zMax - zMin : 0.;
    }
    //=============================================================================
    std::vector<double>
    axesPositionInPixels(GOAxis* parent)
    {
        if (!parent) {
            return {};
        }
        std::vector<double> position
            = parent->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        if (position.size() < 4) {
            return {};
        }
        if (parent->findStringProperty(GO_UNITS_PROPERTY_NAME_STR)
            == GO_PROPERTY_VALUE_PIXELS_STR) {
            return position;
        }
        GOFigure* figure = parent->getParentFigure();
        if (!figure) {
            return {};
        }
        position[0] *= figure->getWidth();
        position[2] *= figure->getWidth();
        position[1] *= figure->getHeight();
        position[3] *= figure->getHeight();
        return position;
    }
    //=============================================================================
} // namespace
//=============================================================================
std::wstring
GOBubbleChart::getType()
{
    return GO_PROPERTY_VALUE_BUBBLE_CHART_STR;
}
//=============================================================================
bool
GOBubbleChart::preservesAutoDataLimits()
{
    return true;
}
//=============================================================================
GOBubbleChart::GOBubbleChart()
{
    registerProperty(new GOVectorProperty, GO_ALPHA_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_C_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_C_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarAlphaProperty, GO_MARKER_EDGE_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarAlphaProperty, GO_MARKER_FACE_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_THETA_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_THETA_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_THETA_DATA_SOURCE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_THETA_JITTER_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_THETA_JITTER_DIRECTION_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_THETA_JITTER_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_THETA_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_R_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_R_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_R_DATA_SOURCE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_R_JITTER_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_R_JITTER_DIRECTION_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_R_JITTER_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_R_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_X_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_X_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_Z_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_SIZE_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerCommonProperties(this);
    sortProperties();

    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    static_cast<GOArrayOfProperty*>(findProperty(GO_C_DATA_PROPERTY_NAME_STR))
        ->data(ArrayOf::doubleRowVector(defaultBubbleColor()));
    setRestrictedStringDefault(GO_C_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringColorDefault(
        GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR, -1, -1, -1);
    setRestrictedStringColorDefault(
        GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR, -1, -1, -1);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_THETA_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    setRestrictedStringDefault(GO_R_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    setStringDefault(GO_THETA_DATA_SOURCE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_R_DATA_SOURCE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_THETA_JITTER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setStringDefault(GO_THETA_JITTER_DIRECTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOTH_STR);
    setScalarDoubleDefault(GO_THETA_JITTER_WIDTH_PROPERTY_NAME_STR, 0.9);
    setStringDefault(GO_R_JITTER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setStringDefault(GO_R_JITTER_DIRECTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOTH_STR);
    setScalarDoubleDefault(GO_R_JITTER_WIDTH_PROPERTY_NAME_STR, 0.9);
    setStringDefault(GO_THETA_VARIABLE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_R_VARIABLE_PROPERTY_NAME_STR, L"");
    setRestrictedStringDefault(GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    setTwoVectorDefault(GO_X_DATA_PROPERTY_NAME_STR, 0, 1);
    setTwoVectorDefault(GO_Y_DATA_PROPERTY_NAME_STR, 0, 1);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setVectorDoubleDefault(GO_SIZE_DATA_PROPERTY_NAME_STR, { 36 });
    static_cast<GOScalarAlphaProperty*>(findProperty(GO_MARKER_EDGE_ALPHA_PROPERTY_NAME_STR))
        ->data(1.);
    static_cast<GOScalarAlphaProperty*>(findProperty(GO_MARKER_FACE_ALPHA_PROPERTY_NAME_STR))
        ->data(0.6);
    initializeCommonProperties(this);
    setRestrictedStringDefault(
        GO_ALPHA_DATA_MAPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALED_STR);
}
//=============================================================================
GOBubbleChart::~GOBubbleChart() { }
//=============================================================================
void
GOBubbleChart::updateState()
{
    if (hasChanged(GO_C_DATA_PROPERTY_NAME_STR)) {
        // Setting CData switches CDataMode to manual. bubblechart.m restores 'auto' when it
        // uses the color-order color.
        toManual(GO_C_DATA_MODE_PROPERTY_NAME_STR);
        clearChanged(GO_C_DATA_PROPERTY_NAME_STR);
        cDataDirty = true;
    }
    if (hasChanged(GO_X_DATA_PROPERTY_NAME_STR) || hasChanged(GO_Y_DATA_PROPERTY_NAME_STR)
        || hasChanged(GO_Z_DATA_PROPERTY_NAME_STR) || hasChanged(GO_THETA_DATA_PROPERTY_NAME_STR)
        || hasChanged(GO_R_DATA_PROPERTY_NAME_STR)) {
        limitsDirty = true;
    }
    // X is handled by the auto-index generation below.
    if (hasChanged(GO_Y_DATA_PROPERTY_NAME_STR) && !hasChanged(GO_Y_DATA_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_Y_DATA_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Z_DATA_PROPERTY_NAME_STR) && !hasChanged(GO_Z_DATA_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_Z_DATA_MODE_PROPERTY_NAME_STR);
    }
    std::vector<double> xs(findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR));
    std::vector<double> ys(findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    if (isAuto(GO_X_DATA_MODE_PROPERTY_NAME_STR)) {
        xs.resize(ys.size());
        OMP_PARALLEL_FOR_LOOP(ys.size())
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(ys.size()); i++) {
            xs[i] = i + 1.0;
        }
        static_cast<GOVectorProperty*>(findProperty(GO_X_DATA_PROPERTY_NAME_STR))->data(xs);
    }
}
//=============================================================================
void
GOBubbleChart::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    double width(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    gc.lineWidth(width);

    ArrayOf cdata(findArrayOfProperty(GO_C_DATA_PROPERTY_NAME_STR));
    cdata.promoteType(NLS_DOUBLE);
    const double* ptrCData = static_cast<const double*>(cdata.dataPointer());

    std::vector<double> xs(findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR));
    std::vector<double> ys(findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    std::vector<double> zs(findVectorDoubleProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    buildPolarBubbleData(this, xs, ys);
    if (zs.empty()) {
        zs.resize(xs.size(), 0.0);
    }
    if (!((xs.size() == ys.size()) && (ys.size() == zs.size()))) {
        return;
    }
    std::vector<double> mxs, mys, mzs;
    GOAxis* parent = static_cast<GOAxis*>(getParentAxis());
    if (parent) {
        parent->reMap(xs, ys, zs, mxs, mys, mzs);
    }
    if (!parent || mxs.empty()) {
        return;
    }

    auto* ea
        = static_cast<GOScalarAlphaProperty*>(findProperty(GO_MARKER_EDGE_ALPHA_PROPERTY_NAME_STR));
    auto* fa
        = static_cast<GOScalarAlphaProperty*>(findProperty(GO_MARKER_FACE_ALPHA_PROPERTY_NAME_STR));
    auto* ec = static_cast<GOAutoFlatColorProperty*>(
        findProperty(GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR));
    auto* fc = static_cast<GOAutoFlatColorProperty*>(
        findProperty(GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR));
    std::vector<double> alphaData(findVectorDoubleProperty(GO_ALPHA_DATA_PROPERTY_NAME_STR));
    std::vector<double> sizeData(findVectorDoubleProperty(GO_SIZE_DATA_PROPERTY_NAME_STR));
    double minSize = 0.;
    double maxSize = 1.;
    bubbleValueRange(parent, sizeData, minSize, maxSize);
    std::vector<double> diameterRange = bubbleDiameterRange(parent);
    std::vector<double> colorMapData;
    auto* colorMap
        = static_cast<GOColorVectorProperty*>(parent->findProperty(GO_COLOR_MAP_PROPERTY_NAME_STR));
    if (colorMap) {
        colorMapData = colorMap->data();
    }
    bool clippingOn = stringCheck(GO_CLIPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    std::vector<double> clipBox = clippingOn
        ? parent->getPropertyVectorAsPixels(gc, GO_POSITION_PROPERTY_NAME_STR)
        : std::vector<double>();

    std::vector<double> uc, vc;
    std::vector<size_t> sourceIndices;
    for (size_t i = 0; i < mxs.size(); i++) {
        double u, v;
        bool clipped;
        gc.toPixels(mxs[i], mys[i], mzs[i], u, v, clipped);
        if (!clipped) {
            uc.push_back(u);
            vc.push_back(v);
            sourceIndices.push_back(i);
        }
    }
    gc.setupDirectDraw();
    if (clippingOn && clipBox.size() >= 4) {
        gc.setClipBox(clipBox[0], clipBox[1], clipBox[2], clipBox[3]);
    }
    std::vector<double> markerSizes;
    std::vector<std::vector<double>> edgeColors;
    std::vector<std::vector<double>> faceColors;
    markerSizes.reserve(uc.size());
    edgeColors.reserve(uc.size());
    faceColors.reserve(uc.size());
    for (size_t k = 0; k < uc.size(); k++) {
        size_t pointIndex = sourceIndices[k];
        double markerSize = bubbleDiameter(
            sizeDataAt(sizeData, pointIndex), minSize, maxSize, diameterRange[0], diameterRange[1]);
        markerSize *= BUBBLE_RENDER_DIAMETER_SCALE;
        std::vector<double> cdataColor = cDataColorAt(cdata, ptrCData, mxs.size(), pointIndex,
            colorMapData, cDataDirty, cachedMinC, cachedMaxC);
        std::vector<double> edgeColor
            = ec->isEqual(GO_PROPERTY_VALUE_FLAT_STR) ? cdataColor : ec->colorSpec();
        std::vector<double> faceColor
            = fc->isEqual(GO_PROPERTY_VALUE_FLAT_STR) ? cdataColor : fc->colorSpec();
        ensureColorSpec(edgeColor);
        ensureColorSpec(faceColor);
        appendAlpha(edgeColor, alphaData, ea->isFlat(), ea->data(), pointIndex);
        appendAlpha(faceColor, alphaData, fa->isFlat(), fa->data(), pointIndex);
        markerSizes.push_back(markerSize);
        edgeColors.push_back(edgeColor);
        faceColors.push_back(faceColor);
    }
    // One batched primitive (RenderWeb) instead of a circle per bubble. 'none' (the -1
    // sentinel) maps to an empty colour. A backend that cannot batch (Qt/OpenGL) returns
    // false and the loop runs.
    const bool edgeDrawable = !edgeColors.empty() && edgeColors.front()[0] != -1;
    const bool faceDrawable = !faceColors.empty() && faceColors.front()[0] != -1;
    std::vector<double> edgeFlat;
    std::vector<double> faceFlat;
    if (edgeDrawable) {
        edgeFlat.reserve(edgeColors.size() * 4);
        for (const std::vector<double>& c : edgeColors) {
            for (int j = 0; j < 4; j++) {
                edgeFlat.push_back(j < static_cast<int>(c.size()) ? c[j] : 1.0);
            }
        }
    }
    if (faceDrawable) {
        faceFlat.reserve(faceColors.size() * 4);
        for (const std::vector<double>& c : faceColors) {
            for (int j = 0; j < 4; j++) {
                faceFlat.push_back(j < static_cast<int>(c.size()) ? c[j] : 1.0);
            }
        }
    }
    if (!DrawSymbolBatch(
            gc, RenderInterface::Circle, uc, vc, markerSizes, edgeFlat, faceFlat, width)) {
        for (size_t k = 0; k < uc.size(); k++) {
            DrawSymbol(gc, RenderInterface::Circle, uc[k], vc[k], 0, markerSizes[k], edgeColors[k],
                faceColors[k], width);
        }
    }
    if (clippingOn) {
        gc.clearClipBox();
    }
    gc.releaseDirectDraw();
}
//=============================================================================
std::vector<double>
GOBubbleChart::getLimits()
{
    if (!limitsDirty) {
        return limits;
    }
    std::vector<double> xs(findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR));
    std::vector<double> ys(findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    std::vector<double> zs(findVectorDoubleProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    buildPolarBubbleData(this, xs, ys);
    limits.resize(0);
    limits.reserve(12);
    double xMin = findVectorMin(xs);
    double xMax = findVectorMax(xs);
    double yMin = findVectorMin(ys);
    double yMax = findVectorMax(ys);
    bool hasZ = !zs.empty();
    double zMin = hasZ ? findVectorMin(zs) : 0.;
    double zMax = hasZ ? findVectorMax(zs) : 0.;
    GOAxis* parent = static_cast<GOAxis*>(getParentAxis());
    double xSpan = 0., ySpan = 0., zSpan = 0.;
    bubbleAxisDataSpan(parent, xSpan, ySpan, zSpan);
    if (xSpan <= 0. && std::isfinite(xMin) && std::isfinite(xMax)) {
        xSpan = xMax - xMin;
    }
    if (ySpan <= 0. && std::isfinite(yMin) && std::isfinite(yMax)) {
        ySpan = yMax - yMin;
    }
    if (zSpan <= 0. && hasZ && std::isfinite(zMin) && std::isfinite(zMax)) {
        zSpan = zMax - zMin;
    }
    // Default margin keeps whitespace around edge bubbles before the pixel geometry is known.
    double xPad = xSpan > 0. ? xSpan * 0.025 : 0.;
    double yPad = ySpan > 0. ? ySpan * 0.025 : 0.;
    double zPad = zSpan > 0. ? zSpan * 0.025 : 0.;
    if (parent && std::isfinite(xMin) && std::isfinite(xMax) && std::isfinite(yMin)
        && std::isfinite(yMax)) {
        std::vector<double> position = axesPositionInPixels(parent);
        if (position.size() >= 4 && position[2] > 0. && position[3] > 0.) {
            std::vector<double> diameterRange = bubbleDiameterRange(parent);
            double lineWidth = findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR);
            double radiusPixels
                = pointsToPixels(diameterRange[1] * BUBBLE_RENDER_DIAMETER_SCALE + lineWidth) / 2.;
            // Extend by the largest bubble radius in data units, on top of the default margin.
            xPad += bubbleDataPadding(xSpan, position[2], radiusPixels);
            yPad += bubbleDataPadding(ySpan, position[3], radiusPixels);
            if (hasZ && std::isfinite(zMin) && std::isfinite(zMax)) {
                zPad += bubbleDataPadding(zSpan, position[3], radiusPixels);
            }
        }
    }
    limits.push_back(xMin - xPad);
    limits.push_back(xMax + xPad);
    limits.push_back(yMin - yPad);
    limits.push_back(yMax + yPad);
    if (hasZ) {
        limits.push_back(zMin - zPad);
        limits.push_back(zMax + zPad);
    } else {
        // 2D chart: report a non-degenerate Z range so GOAxis preserveChildAutoLimits keeps
        // the padded X/Y auto-limits. A [0 0] Z range disables that path and tick rounding
        // shrinks the padding.
        limits.push_back(0);
        limits.push_back(1);
    }
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
