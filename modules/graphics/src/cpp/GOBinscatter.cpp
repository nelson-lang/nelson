//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOBinscatter.hpp"
#include "GOChartDecorators.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOAxis.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOFaceAlphaProperty.hpp"
#include "GOFigure.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOPickablePartsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOVectorProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "RenderInterface.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    static void
    addProperty(GraphicsObject* go, GOGenericProperty* property, const std::wstring& name,
        bool writable = true)
    {
        go->registerProperty(property, name, writable);
    }
    //=============================================================================
    static ArrayOf
    rowVector(const std::vector<double>& values)
    {
        return ArrayOf::doubleRowVector(values);
    }
    //=============================================================================
    static ArrayOf
    matrixArray(size_t rows, size_t columns, const std::vector<double>& values)
    {
        ArrayOf result = ArrayOf::doubleMatrix2d(
            static_cast<indexType>(rows), static_cast<indexType>(columns));
        double* dp = static_cast<double*>(result.readWriteDataPointer());
        std::copy(values.begin(), values.end(), dp);
        return result;
    }
    //=============================================================================
    static void
    setArray(GraphicsObject* go, const std::wstring& name, const ArrayOf& value)
    {
        auto* property = static_cast<GOArrayOfProperty*>(go->findProperty(name));
        property->data(value);
        property->clearModified();
    }
    //=============================================================================
    static void
    setVector(GraphicsObject* go, const std::wstring& name, const std::vector<double>& values)
    {
        auto* property = static_cast<GOVectorProperty*>(go->findProperty(name));
        property->data(values);
        property->clearModified();
    }
    //=============================================================================
    static std::vector<double>
    validatedLimits(const std::vector<double>& values, const std::wstring& name)
    {
        if (values.size() != 2 || !std::isfinite(values[0]) || !std::isfinite(values[1])
            || values[0] >= values[1]) {
            Error(L"Nelson:graphics:mustBeTwoElementVector",
                L"%s must be a two-element increasing finite vector.", name);
        }
        return values;
    }
    //=============================================================================
    static std::vector<double>
    limitsFromData(const std::vector<double>& values)
    {
        std::vector<double> finiteValues;
        finiteValues.reserve(values.size());
        for (double value : values) {
            if (std::isfinite(value)) {
                finiteValues.push_back(value);
            }
        }
        if (finiteValues.empty()) {
            return { 0., 1. };
        }
        auto [minIt, maxIt] = std::minmax_element(finiteValues.begin(), finiteValues.end());
        double low = *minIt;
        double high = *maxIt;
        if (low == high) {
            low -= 0.5;
            high += 0.5;
        }
        return { low, high };
    }
    //=============================================================================
    static std::vector<double>
    uniformEdges(double low, double high, size_t nBins)
    {
        nBins = std::max<size_t>(1, nBins);
        double width = (high - low) / static_cast<double>(nBins);
        std::vector<double> edges(nBins + 1);
        for (size_t k = 0; k <= nBins; ++k) {
            edges[k] = low + width * static_cast<double>(k);
        }
        edges[nBins] = high;
        return edges;
    }
    //=============================================================================
    static double
    standardDeviation(const std::vector<double>& values)
    {
        double sum = 0.;
        double sumSquares = 0.;
        size_t count = 0;
        for (double value : values) {
            if (!std::isfinite(value)) {
                continue;
            }
            sum += value;
            sumSquares += value * value;
            ++count;
        }
        if (count < 2) {
            return 0.;
        }
        double mean = sum / static_cast<double>(count);
        double variance = sumSquares / static_cast<double>(count) - mean * mean;
        return std::sqrt(std::max(0., variance));
    }
    //=============================================================================
    static size_t
    finitePairCount(const std::vector<double>& x, const std::vector<double>& y)
    {
        size_t count = 0;
        for (size_t k = 0; k < x.size() && k < y.size(); ++k) {
            if (std::isfinite(x[k]) && std::isfinite(y[k])) {
                ++count;
            }
        }
        return count;
    }
    //=============================================================================
    static size_t
    automaticBinCount(const std::vector<double>& values, double low, double high, size_t pairCount)
    {
        if (pairCount == 0 || high <= low) {
            return 1;
        }
        double sigma = standardDeviation(values);
        if (!std::isfinite(sigma) || sigma <= 0.) {
            return 1;
        }
        double width = 3.5 * sigma * std::pow(static_cast<double>(pairCount), -0.25);
        if (!std::isfinite(width) || width <= 0.) {
            return 1;
        }
        double bins = std::round((high - low) / width);
        return std::max<size_t>(1, std::min<size_t>(250, static_cast<size_t>(bins)));
    }
    //=============================================================================
    static std::vector<double>
    countsForEdges(const std::vector<double>& x, const std::vector<double>& y,
        const std::vector<double>& xEdges, const std::vector<double>& yEdges)
    {
        size_t xBins = xEdges.size() > 1 ? xEdges.size() - 1 : 0;
        size_t yBins = yEdges.size() > 1 ? yEdges.size() - 1 : 0;
        std::vector<double> counts(xBins * yBins, 0.);
        for (size_t k = 0; k < x.size() && k < y.size(); ++k) {
            double xv = x[k];
            double yv = y[k];
            if (!std::isfinite(xv) || !std::isfinite(yv) || xv < xEdges.front()
                || xv > xEdges.back() || yv < yEdges.front() || yv > yEdges.back()) {
                continue;
            }
            auto xIt = std::upper_bound(xEdges.begin(), xEdges.end(), xv);
            auto yIt = std::upper_bound(yEdges.begin(), yEdges.end(), yv);
            size_t xi = xv == xEdges.back()
                ? xBins - 1
                : static_cast<size_t>(std::distance(xEdges.begin(), xIt) - 1);
            size_t yi = yv == yEdges.back()
                ? yBins - 1
                : static_cast<size_t>(std::distance(yEdges.begin(), yIt) - 1);
            if (xi < xBins && yi < yBins) {
                counts[xi + xBins * yi] += 1.;
            }
        }
        return counts;
    }
    //=============================================================================
    static std::vector<double>
    updateComputedBins(GraphicsObject* object, const std::vector<double>& x,
        const std::vector<double>& y, const std::vector<double>& xLimits,
        const std::vector<double>& yLimits, size_t xBins, size_t yBins)
    {
        std::vector<double> xEdges = uniformEdges(xLimits[0], xLimits[1], xBins);
        std::vector<double> yEdges = uniformEdges(yLimits[0], yLimits[1], yBins);
        std::vector<double> values = countsForEdges(x, y, xEdges, yEdges);
        setArray(object, GO_X_BIN_EDGES_PROPERTY_NAME_STR, rowVector(xEdges));
        setArray(object, GO_Y_BIN_EDGES_PROPERTY_NAME_STR, rowVector(yEdges));
        setArray(object, GO_VALUES_PROPERTY_NAME_STR, matrixArray(xBins, yBins, values));
        return values;
    }
    //=============================================================================
    static std::vector<double>
    currentLimits(GraphicsObject* object, const std::wstring& propertyName)
    {
        return validatedLimits(object->findVectorDoubleProperty(propertyName), propertyName);
    }
    //=============================================================================
    static double
    numericScalarProperty(GraphicsObject* object, const std::wstring& propertyName, double fallback)
    {
        ArrayOf value = object->findProperty(propertyName)->get();
        if (value.isNumeric()) {
            return value.asDoubleScalar();
        }
        return fallback;
    }
    //=============================================================================
    static std::vector<double>
    colormapColor(GraphicsObject* object, double value, double alpha)
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
            return { 0., 0.447, 0.741, alpha };
        }
        std::vector<double> clim = axis ? axis->findVectorDoubleProperty(GO_C_LIM_PROPERTY_NAME_STR)
                                        : std::vector<double> { 0., 1. };
        double low = std::min(clim[0], clim[1]);
        double high = std::max(clim[0], clim[1]);
        size_t index = 0;
        if (high > low) {
            double scaled = (value - low) / (high - low);
            index = static_cast<size_t>(std::clamp(std::floor(scaled * static_cast<double>(length)),
                0., static_cast<double>(length - 1)));
        }
        return { cmap[index * 3], cmap[index * 3 + 1], cmap[index * 3 + 2], alpha };
    }
    //=============================================================================
    static size_t
    displayBinCount(
        double low, double high, double visibleLow, double visibleHigh, double requested)
    {
        size_t baseBins = std::max<size_t>(1, static_cast<size_t>(std::floor(requested)));
        double baseRange = high - low;
        double visibleRange = std::min(high, visibleHigh) - std::max(low, visibleLow);
        if (!std::isfinite(visibleRange) || visibleRange <= 0.
            || visibleRange >= baseRange * 0.999) {
            return baseBins;
        }
        double scaledBins = std::round(baseRange / visibleRange * static_cast<double>(baseBins));
        return std::max<size_t>(baseBins, std::min<size_t>(250, static_cast<size_t>(scaledBins)));
    }
    //=============================================================================
    static void
    updateColorLimits(GOAxis* axis, const std::vector<double>& values)
    {
        if (!axis || values.empty()) {
            return;
        }
        double maxValue = *std::max_element(values.begin(), values.end());
        if (!std::isfinite(maxValue) || maxValue <= 0.) {
            maxValue = 1.;
        }
        axis->setTwoVectorDefault(GO_C_LIM_PROPERTY_NAME_STR, 0., maxValue);
    }
    //=============================================================================
    static void
    drawTile(RenderInterface& gc, double x0, double x1, double y0, double y1,
        const std::vector<double>& color)
    {
        gc.color(color);
        gc.quad(x0, y0, 0., x1, y0, 0., x1, y1, 0., x0, y1, 0.);
    }
    //=============================================================================
    static std::vector<double>
    arrayVector(const ArrayOf& value)
    {
        if (value.isEmpty()) {
            return {};
        }
        ArrayOf asDouble(value);
        asDouble.promoteType(NLS_DOUBLE);
        return asDouble.asDoubleVector();
    }
    //=============================================================================
} // namespace
//=============================================================================
GOBinscatter::GOBinscatter()
{
    addProperty(this, new GOVectorProperty, GO_X_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOVectorProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOTwoVectorProperty, GO_NUM_BINS_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_NUM_BINS_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_SHOW_EMPTY_BINS_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_X_BIN_EDGES_PROPERTY_NAME_STR, false);
    addProperty(this, new GOArrayOfProperty, GO_Y_BIN_EDGES_PROPERTY_NAME_STR, false);
    addProperty(this, new GOTwoVectorProperty, GO_X_LIMITS_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_X_LIMITS_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOTwoVectorProperty, GO_Y_LIMITS_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_Y_LIMITS_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_VALUES_PROPERTY_NAME_STR, false);
    addProperty(this, new GOFaceAlphaProperty, GO_FACE_ALPHA_PROPERTY_NAME_STR);
    addProperty(this, new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR, false);
    addProperty(this, new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    addProperty(this, new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    addProperty(this, new GOLazyDecoratorProperty(GODecoratorKind::AnnotationEventData),
        GO_ANNOTATION_PROPERTY_NAME_STR, false);
    addProperty(this, new GOLazyDecoratorProperty(GODecoratorKind::DataTipTemplate),
        GO_DATA_TIP_TEMPLATE_PROPERTY_NAME_STR, false);
    addProperty(this, new GOOnOffProperty, GO_SELECTED_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_HIT_TEST_PROPERTY_NAME_STR);
    addProperty(this, new GOPickablePartsProperty, GO_PICKABLE_PARTS_PROPERTY_NAME_STR);
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

    setVector(this, GO_X_DATA_PROPERTY_NAME_STR, {});
    setVector(this, GO_Y_DATA_PROPERTY_NAME_STR, {});
    setTwoVectorDefault(GO_NUM_BINS_PROPERTY_NAME_STR, 1., 1.);
    setRestrictedStringDefault(GO_NUM_BINS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_SHOW_EMPTY_BINS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setTwoVectorDefault(GO_X_LIMITS_PROPERTY_NAME_STR, 0., 1.);
    setRestrictedStringDefault(GO_X_LIMITS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setTwoVectorDefault(GO_Y_LIMITS_PROPERTY_NAME_STR, 0., 1.);
    setRestrictedStringDefault(GO_Y_LIMITS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringScalarDefault(
        GO_FACE_ALPHA_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALAR_STR, 1.);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_DISPLAY_NAME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setRestrictedStringDefault(GO_SELECTED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HIT_TEST_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_PICKABLE_PARTS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VISIBLE_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BINSCATTER_STR);
    setStringDefault(GO_TAG_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    clearAllChanged();
    sortProperties();
}
//=============================================================================
GOBinscatter::~GOBinscatter() = default;
//=============================================================================
std::wstring
GOBinscatter::getType()
{
    return GO_PROPERTY_VALUE_BINSCATTER_STR;
}
//=============================================================================
bool
GOBinscatter::refreshBins(bool force)
{
    std::vector<double> x = findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR);
    std::vector<double> y = findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR);
    if (x.size() != y.size()) {
        Error("Nelson:graphics:xyDataSizeMismatch",
            "XData and YData must have the same number of elements.");
    }
    if (isAuto(GO_X_LIMITS_MODE_PROPERTY_NAME_STR)) {
        std::vector<double> xLimits = limitsFromData(x);
        setTwoVectorDefault(GO_X_LIMITS_PROPERTY_NAME_STR, xLimits[0], xLimits[1]);
    }
    if (isAuto(GO_Y_LIMITS_MODE_PROPERTY_NAME_STR)) {
        std::vector<double> yLimits = limitsFromData(y);
        setTwoVectorDefault(GO_Y_LIMITS_PROPERTY_NAME_STR, yLimits[0], yLimits[1]);
    }
    std::vector<double> xLimits = currentLimits(this, GO_X_LIMITS_PROPERTY_NAME_STR);
    std::vector<double> yLimits = currentLimits(this, GO_Y_LIMITS_PROPERTY_NAME_STR);
    std::vector<double> numBins = findVectorDoubleProperty(GO_NUM_BINS_PROPERTY_NAME_STR);
    if (isAuto(GO_NUM_BINS_MODE_PROPERTY_NAME_STR)) {
        size_t pairCount = finitePairCount(x, y);
        size_t xAutoBins = automaticBinCount(x, xLimits[0], xLimits[1], pairCount);
        size_t yAutoBins = automaticBinCount(y, yLimits[0], yLimits[1], pairCount);
        numBins = { static_cast<double>(xAutoBins), static_cast<double>(yAutoBins) };
        setTwoVectorDefault(GO_NUM_BINS_PROPERTY_NAME_STR, numBins[0], numBins[1]);
    }
    if (numBins.size() != 2 || numBins[0] < 1. || numBins[1] < 1.) {
        Error("Nelson:graphics:invalidNumBins",
            "NumBins must be a positive scalar or two-element vector.");
    }
    size_t xBins = static_cast<size_t>(std::floor(numBins[0]));
    size_t yBins = static_cast<size_t>(std::floor(numBins[1]));
    GOAxis* axis = getParentAxis();
    if (axis) {
        std::vector<double> xAxisLimits
            = axis->findVectorDoubleProperty(GO_X_LIM_PROPERTY_NAME_STR);
        std::vector<double> yAxisLimits
            = axis->findVectorDoubleProperty(GO_Y_LIM_PROPERTY_NAME_STR);
        xBins = displayBinCount(xLimits[0], xLimits[1], xAxisLimits[0], xAxisLimits[1], numBins[0]);
        yBins = displayBinCount(yLimits[0], yLimits[1], yAxisLimits[0], yAxisLimits[1], numBins[1]);
    }
    std::vector<double> xEdges = arrayVector(findArrayOfProperty(GO_X_BIN_EDGES_PROPERTY_NAME_STR));
    std::vector<double> yEdges = arrayVector(findArrayOfProperty(GO_Y_BIN_EDGES_PROPERTY_NAME_STR));
    size_t currentXBins = xEdges.size() > 1 ? xEdges.size() - 1 : 0;
    size_t currentYBins = yEdges.size() > 1 ? yEdges.size() - 1 : 0;
    if (!force && currentXBins == xBins && currentYBins == yBins) {
        if (axis) {
            updateColorLimits(axis, arrayVector(findArrayOfProperty(GO_VALUES_PROPERTY_NAME_STR)));
        }
        return false;
    }
    std::vector<double> values = updateComputedBins(this, x, y, xLimits, yLimits, xBins, yBins);
    if (axis) {
        updateColorLimits(axis, values);
    }
    limitsDirty = true;
    return true;
}
//=============================================================================
void
GOBinscatter::updateState()
{
    if (hasChanged(GO_NUM_BINS_PROPERTY_NAME_STR)) {
        toManual(GO_NUM_BINS_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_X_LIMITS_PROPERTY_NAME_STR)) {
        toManual(GO_X_LIMITS_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_LIMITS_PROPERTY_NAME_STR)) {
        toManual(GO_Y_LIMITS_MODE_PROPERTY_NAME_STR);
    }
    bool force = hasChanged({ GO_X_DATA_PROPERTY_NAME_STR, GO_Y_DATA_PROPERTY_NAME_STR,
        GO_NUM_BINS_PROPERTY_NAME_STR, GO_NUM_BINS_MODE_PROPERTY_NAME_STR,
        GO_X_LIMITS_PROPERTY_NAME_STR, GO_X_LIMITS_MODE_PROPERTY_NAME_STR,
        GO_Y_LIMITS_PROPERTY_NAME_STR, GO_Y_LIMITS_MODE_PROPERTY_NAME_STR });

    bool refreshed = refreshBins(force);
    if (!force && !refreshed) {
        return;
    }
    clearChanged({ GO_X_DATA_PROPERTY_NAME_STR, GO_Y_DATA_PROPERTY_NAME_STR,
        GO_NUM_BINS_PROPERTY_NAME_STR, GO_NUM_BINS_MODE_PROPERTY_NAME_STR,
        GO_X_LIMITS_PROPERTY_NAME_STR, GO_X_LIMITS_MODE_PROPERTY_NAME_STR,
        GO_Y_LIMITS_PROPERTY_NAME_STR, GO_Y_LIMITS_MODE_PROPERTY_NAME_STR });
}
//=============================================================================
void
GOBinscatter::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    refreshBins(false);
    std::vector<double> xEdges = arrayVector(findArrayOfProperty(GO_X_BIN_EDGES_PROPERTY_NAME_STR));
    std::vector<double> yEdges = arrayVector(findArrayOfProperty(GO_Y_BIN_EDGES_PROPERTY_NAME_STR));
    std::vector<double> values = arrayVector(findArrayOfProperty(GO_VALUES_PROPERTY_NAME_STR));
    size_t xBins = xEdges.size() > 1 ? xEdges.size() - 1 : 0;
    size_t yBins = yEdges.size() > 1 ? yEdges.size() - 1 : 0;
    if (xBins == 0 || yBins == 0 || values.size() != xBins * yBins) {
        return;
    }

    bool showEmptyBins
        = findStringProperty(GO_SHOW_EMPTY_BINS_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_ON_STR;
    double alpha = numericScalarProperty(this, GO_FACE_ALPHA_PROPERTY_NAME_STR, 1.);
    for (size_t yi = 0; yi < yBins; ++yi) {
        for (size_t xi = 0; xi < xBins; ++xi) {
            double value = values[xi + xBins * yi];
            if (!showEmptyBins && value == 0.) {
                continue;
            }
            drawTile(gc, xEdges[xi], xEdges[xi + 1], yEdges[yi], yEdges[yi + 1],
                colormapColor(this, value, std::clamp(alpha, 0., 1.)));
        }
    }
}
//=============================================================================
std::vector<double>
GOBinscatter::getLimits()
{
    if (!limitsDirty) {
        return limits;
    }
    std::vector<double> xEdges = arrayVector(findArrayOfProperty(GO_X_BIN_EDGES_PROPERTY_NAME_STR));
    std::vector<double> yEdges = arrayVector(findArrayOfProperty(GO_Y_BIN_EDGES_PROPERTY_NAME_STR));
    if (xEdges.empty()) {
        xEdges = { 0., 1. };
    }
    if (yEdges.empty()) {
        yEdges = { 0., 1. };
    }
    limits
        = { xEdges.front(), xEdges.back(), yEdges.front(), yEdges.back(), -1., 1., 0., 1., 1., 1. };
    limitsDirty = false;
    return limits;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
