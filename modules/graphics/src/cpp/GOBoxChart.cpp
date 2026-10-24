//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOBoxChart.hpp"
#include "GOChartDecorators.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOAxis.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCallbackProperty.hpp"
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
    static const std::vector<std::wstring> ORIENTATION_VALUES = { L"vertical", L"horizontal" };
    static const std::vector<std::wstring> COLOR_GROUP_LAYOUT_VALUES = { L"grouped", L"overlaid" };
    //=============================================================================
    static std::vector<double>
    defaultSeriesColor()
    {
        return { 0.066, 0.443, 0.745 };
    }
    //=============================================================================
    static std::vector<double>
    defaultWhiskerColor()
    {
        return { 33. / 255., 33. / 255., 33. / 255. };
    }
    //=============================================================================
    static void
    addProperty(GraphicsObject* go, GOGenericProperty* property, const std::wstring& name,
        bool writable = true, bool visible = true)
    {
        go->registerProperty(property, name, writable, visible);
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
    static ArrayOf
    rowVector(const std::vector<double>& values)
    {
        return ArrayOf::doubleRowVector(values);
    }
    //=============================================================================
    static std::vector<double>
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
    static bool
    isDrawableColor(const std::vector<double>& color)
    {
        return color.size() >= 3 && color[0] >= 0.;
    }
    //=============================================================================
    static std::vector<double>
    colorOrderColor(GOAxis* axis, double seriesIndex)
    {
        if (!axis || !std::isfinite(seriesIndex) || seriesIndex < 1) {
            return defaultSeriesColor();
        }
        std::vector<double> colorOrder(
            axis->findVectorDoubleProperty(GO_COLOR_ORDER_PROPERTY_NAME_STR));
        size_t rows = colorOrder.size() / 3;
        if (rows == 0) {
            return defaultSeriesColor();
        }
        size_t row = (static_cast<size_t>(seriesIndex) - 1) % rows;
        return { colorOrder[row * 3], colorOrder[row * 3 + 1], colorOrder[row * 3 + 2] };
    }
    //=============================================================================
    static double
    seriesIndexValue(GraphicsObject* object)
    {
        auto* seriesIndex = static_cast<GOSeriesIndexProperty*>(
            object->findProperty(GO_SERIES_INDEX_PROPERTY_NAME_STR));
        if (!seriesIndex || seriesIndex->isNone()) {
            return 1.;
        }
        return seriesIndex->value();
    }
    //=============================================================================
    static int
    numPeersValue(GraphicsObject* object)
    {
        auto* property = object->findProperty(GO_NUM_PEERS_PROPERTY_NAME_STR, false);
        if (!property) {
            return 1;
        }
        double value = object->findScalarDoubleProperty(GO_NUM_PEERS_PROPERTY_NAME_STR);
        if (!std::isfinite(value) || value < 1.) {
            return 1;
        }
        return std::max(1, static_cast<int>(std::round(value)));
    }
    //=============================================================================
    static bool
    isColorGroupOverlaid(GraphicsObject* object)
    {
        return object->findStringProperty(GO_COLOR_GROUP_LAYOUT_PROPERTY_NAME_STR) == L"overlaid";
    }
    //=============================================================================
    static double
    automaticColorGroupWidth(int peers)
    {
        if (peers <= 1) {
            return 1.;
        }
        double count = static_cast<double>(peers);
        return std::min(0.8, count / (count + 1.5));
    }
    //=============================================================================
    static double
    colorGroupWidthValue(GraphicsObject* object)
    {
        int peers = numPeersValue(object);
        if (object->findStringProperty(GO_COLOR_GROUP_WIDTH_MODE_PROPERTY_NAME_STR)
            == GO_PROPERTY_VALUE_AUTO_STR) {
            return automaticColorGroupWidth(peers);
        }
        return object->findScalarDoubleProperty(GO_COLOR_GROUP_WIDTH_PROPERTY_NAME_STR);
    }
    //=============================================================================
    static double
    colorGroupOffset(GraphicsObject* object, double unitWidth)
    {
        int peers = numPeersValue(object);
        if (peers <= 1 || isColorGroupOverlaid(object)) {
            return 0.;
        }
        double index = std::min(static_cast<double>(peers), std::max(1., seriesIndexValue(object)));
        double peersCount = static_cast<double>(peers);
        double groupWidth = colorGroupWidthValue(object) * unitWidth;
        double groupStep = groupWidth / peersCount;
        return groupWidth * (1. / peersCount - 1.) / 2. + (index - 1.) * groupStep;
    }
    //=============================================================================
    static double
    drawBoxWidth(GraphicsObject* object)
    {
        double boxWidth = object->findScalarDoubleProperty(GO_BOX_WIDTH_PROPERTY_NAME_STR);
        int peers = numPeersValue(object);
        if (peers <= 1 || isColorGroupOverlaid(object)) {
            return boxWidth;
        }
        return boxWidth * colorGroupWidthValue(object) / static_cast<double>(peers);
    }
    //=============================================================================
    static BoxChartItem
    shiftedItem(const BoxChartItem& item, double offset)
    {
        BoxChartItem shifted = item;
        shifted.position += offset;
        return shifted;
    }
    //=============================================================================
    static double
    positionUnitWidth(const std::vector<BoxChartItem>& items, double fallback)
    {
        std::vector<double> positions;
        positions.reserve(items.size());
        for (const BoxChartItem& item : items) {
            if (std::isfinite(item.position)) {
                positions.push_back(item.position);
            }
        }
        if (positions.size() < 2) {
            return fallback;
        }
        std::sort(positions.begin(), positions.end());
        double unitWidth = std::numeric_limits<double>::infinity();
        double previous = positions.front();
        for (size_t k = 1; k < positions.size(); ++k) {
            double step = positions[k] - previous;
            if (step > 0.) {
                unitWidth = std::min(unitWidth, step);
            }
            previous = positions[k];
        }
        return std::isfinite(unitWidth) ? unitWidth : fallback;
    }
    //=============================================================================
    static std::vector<double>
    automaticColor(GraphicsObject* object)
    {
        return colorOrderColor(object->getParentAxis(), seriesIndexValue(object));
    }
    //=============================================================================
    static void
    setColorSpec(
        GraphicsObject* object, const std::wstring& propertyName, const std::vector<double>& color)
    {
        if (color.size() < 3) {
            return;
        }
        object->setRestrictedStringColorDefault(
            propertyName, GO_PROPERTY_VALUE_COLORSPEC_STR, color[0], color[1], color[2]);
        object->findProperty(propertyName)->clearModified();
    }
    //=============================================================================
    static void
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
    static std::vector<double>
    resolvedColor(GraphicsObject* object, const std::wstring& propertyName,
        const std::wstring& modeName, const std::vector<double>& automatic)
    {
        if (object->isAuto(modeName)) {
            return automatic;
        }
        auto* property = static_cast<GOAutoFlatColorProperty*>(object->findProperty(propertyName));
        if (!property || property->isEqual(GO_PROPERTY_VALUE_AUTO_STR)
            || property->isEqual(GO_PROPERTY_VALUE_FLAT_STR)) {
            return automatic;
        }
        if (property->isEqual(GO_PROPERTY_VALUE_NONE_STR)) {
            return { -1., -1., -1. };
        }
        return property->colorSpec();
    }
    //=============================================================================
    static std::vector<double>
    resolvedUnmodedColor(GraphicsObject* object, const std::wstring& propertyName,
        const std::vector<double>& automatic)
    {
        auto* property = static_cast<GOAutoFlatColorProperty*>(object->findProperty(propertyName));
        if (!property || property->isEqual(GO_PROPERTY_VALUE_AUTO_STR)
            || property->isEqual(GO_PROPERTY_VALUE_FLAT_STR)) {
            return automatic;
        }
        if (property->isEqual(GO_PROPERTY_VALUE_NONE_STR)) {
            return { -1., -1., -1. };
        }
        return property->colorSpec();
    }
    //=============================================================================
    static void
    appendAlpha(std::vector<double>& color, double alpha)
    {
        if (color.size() >= 3) {
            color.push_back(std::clamp(alpha, 0., 1.));
        }
    }
    //=============================================================================
    static double
    percentile(const std::vector<double>& values, double pct)
    {
        if (values.empty()) {
            return std::numeric_limits<double>::quiet_NaN();
        }
        if (values.size() == 1) {
            return values[0];
        }
        double pos = static_cast<double>(values.size()) * pct / 100. + 0.5;
        if (pos <= 1.) {
            return values.front();
        }
        if (pos >= static_cast<double>(values.size())) {
            return values.back();
        }
        double loFloor = std::floor(pos);
        double hiCeil = std::ceil(pos);
        size_t lo = static_cast<size_t>(loFloor);
        size_t hi = static_cast<size_t>(hiCeil);
        if (lo == hi) {
            return values[lo - 1];
        }
        return values[lo - 1] + (pos - loFloor) * (values[hi - 1] - values[lo - 1]);
    }
    //=============================================================================
    static BoxChartStats
    computeStats(std::vector<double> values)
    {
        values.erase(std::remove_if(values.begin(), values.end(),
                         [](double value) { return !std::isfinite(value); }),
            values.end());
        if (values.empty()) {
            return {};
        }
        std::sort(values.begin(), values.end());
        BoxChartStats stats;
        stats.valid = true;
        stats.q1 = percentile(values, 25.);
        stats.median = percentile(values, 50.);
        stats.q3 = percentile(values, 75.);
        double iqr = stats.q3 - stats.q1;
        double lowFence = stats.q1 - 1.5 * iqr;
        double highFence = stats.q3 + 1.5 * iqr;
        stats.whiskerLow = stats.q1;
        stats.whiskerHigh = stats.q3;
        for (double value : values) {
            if (value >= lowFence && value <= highFence) {
                if (value <= stats.q1) {
                    stats.whiskerLow = std::min(stats.whiskerLow, value);
                }
                if (value >= stats.q3) {
                    stats.whiskerHigh = std::max(stats.whiskerHigh, value);
                }
            }
        }
        for (double value : values) {
            if (value < stats.whiskerLow || value > stats.whiskerHigh) {
                stats.outliers.push_back(value);
            }
        }
        double notch = 1.57 * iqr / std::sqrt(static_cast<double>(values.size()));
        stats.notchLow = stats.median - notch;
        stats.notchHigh = stats.median + notch;
        return stats;
    }
    //=============================================================================
    static void
    addItem(std::vector<BoxChartItem>& items, double position, const std::vector<double>& values)
    {
        BoxChartStats stats = computeStats(values);
        if (!stats.valid || !std::isfinite(position)) {
            return;
        }
        BoxChartItem item;
        item.position = position;
        item.stats = stats;
        items.push_back(item);
    }
    //=============================================================================
    static std::vector<double>
    columnValues(const std::vector<double>& data, size_t rows, size_t column)
    {
        std::vector<double> values;
        values.reserve(rows);
        size_t offset = column * rows;
        for (size_t row = 0; row < rows; ++row) {
            values.push_back(data[offset + row]);
        }
        return values;
    }
    //=============================================================================
    static std::vector<double>
    autoPositions(size_t count)
    {
        std::vector<double> positions(count);
        for (size_t k = 0; k < count; ++k) {
            positions[k] = static_cast<double>(k + 1);
        }
        return positions;
    }
    //=============================================================================
    static void
    groupVectorData(std::vector<BoxChartItem>& items, const std::vector<double>& x,
        const std::vector<double>& y)
    {
        if (y.empty()) {
            return;
        }
        if (x.empty() || x.size() == 1) {
            double position = x.empty() ? 1. : x[0];
            addItem(items, position, y);
            return;
        }
        if (x.size() != y.size()) {
            Error("Nelson:graphics:xyDataSizeMismatch",
                "XData and YData must have the same number of elements.");
        }
        std::vector<double> positions;
        std::vector<std::vector<double>> values;
        for (size_t k = 0; k < y.size(); ++k) {
            double position = x[k];
            if (!std::isfinite(position)) {
                continue;
            }
            auto it = std::find(positions.begin(), positions.end(), position);
            if (it == positions.end()) {
                positions.push_back(position);
                values.push_back({ y[k] });
            } else {
                size_t index = static_cast<size_t>(std::distance(positions.begin(), it));
                values[index].push_back(y[k]);
            }
        }
        for (size_t k = 0; k < positions.size(); ++k) {
            addItem(items, positions[k], values[k]);
        }
    }
    //=============================================================================
    static std::vector<BoxChartItem>
    buildItems(GraphicsObject* object)
    {
        ArrayOf yArray = object->findArrayOfProperty(GO_Y_DATA_PROPERTY_NAME_STR);
        ArrayOf xArray = object->findArrayOfProperty(GO_X_DATA_PROPERTY_NAME_STR);
        std::vector<double> y = numericVector(yArray);
        std::vector<double> x = numericVector(xArray);
        std::vector<BoxChartItem> items;
        if (y.empty()) {
            return items;
        }
        if (yArray.isVector()) {
            groupVectorData(items, x, y);
            if (x.empty() && object->isAuto(GO_X_DATA_MODE_PROPERTY_NAME_STR)) {
                setArray(object, GO_X_DATA_PROPERTY_NAME_STR,
                    rowVector(std::vector<double>(y.size(), 1.)));
            }
            return items;
        }
        size_t rows = static_cast<size_t>(yArray.rows());
        size_t columns = static_cast<size_t>(yArray.columns());
        if (rows == 0 || columns == 0) {
            return items;
        }
        if (x.size() == y.size()) {
            groupVectorData(items, x, y);
            if (x.empty() && object->isAuto(GO_X_DATA_MODE_PROPERTY_NAME_STR)) {
                setArray(object, GO_X_DATA_PROPERTY_NAME_STR,
                    rowVector(std::vector<double>(y.size(), 1.)));
            }
            return items;
        }
        if (!x.empty() && x.size() != columns) {
            Error("Nelson:boxchart:xDataMismatch", "XData must match YData columns or elements.");
        }
        std::vector<double> positions = x.empty() ? autoPositions(columns) : x;
        for (size_t column = 0; column < columns; ++column) {
            addItem(items, positions[column], columnValues(y, rows, column));
        }
        if (x.empty() && object->isAuto(GO_X_DATA_MODE_PROPERTY_NAME_STR)) {
            setArray(object, GO_X_DATA_PROPERTY_NAME_STR, rowVector(positions));
        }
        return items;
    }
    //=============================================================================
    static bool
    withinLimits(const std::vector<double>& limits, double value)
    {
        if (limits.size() < 2 || !std::isfinite(value)) {
            return false;
        }
        double low = std::min(limits[0], limits[1]);
        double high = std::max(limits[0], limits[1]);
        double tolerance = std::max(1e-12, (high - low) * 1e-10);
        return value >= low - tolerance && value <= high + tolerance;
    }
    //=============================================================================
    static bool
    withinAxisLimits(GOAxis* axis, double x, double y)
    {
        if (!axis) {
            return false;
        }
        return withinLimits(axis->findVectorDoubleProperty(GO_X_LIM_PROPERTY_NAME_STR), x)
            && withinLimits(axis->findVectorDoubleProperty(GO_Y_LIM_PROPERTY_NAME_STR), y);
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
        return !clipped || withinAxisLimits(axis, x, y);
    }
    //=============================================================================
    static double
    jitterOffset(size_t index, double width)
    {
        double pattern = static_cast<double>(static_cast<int>(index % 7) - 3) / 3.;
        return pattern * width * 0.25;
    }
    //=============================================================================
    static void
    expandToNiceValueLimits(double& minValue, double& maxValue)
    {
        double range = maxValue - minValue;
        if (!std::isfinite(range) || range <= 0.) {
            return;
        }
        double step = std::pow(10., std::floor(std::log10(range)));
        double steps = range / step;
        if (steps <= 1.) {
            step /= 10.;
        } else if (steps <= 2.) {
            step /= 5.;
        } else if (steps <= 5.) {
            step /= 2.;
        }
        if (!std::isfinite(step) || step <= 0.) {
            return;
        }
        minValue = std::floor(minValue / step) * step;
        maxValue = std::ceil(maxValue / step) * step;
        if (std::fabs(minValue) < step * 1e-10) {
            minValue = 0.;
        }
        if (std::fabs(maxValue) < step * 1e-10) {
            maxValue = 0.;
        }
    }
    //=============================================================================
    static void
    drawFilledPolygon(RenderInterface& gc, const point& center, const std::vector<point>& vertices,
        const std::vector<double>& faceColor)
    {
        if (vertices.size() < 3) {
            return;
        }
        gc.setLineStyle(GO_PROPERTY_VALUE_NONE_STR);
        std::vector<std::vector<double>> polygon;
        polygon.reserve(vertices.size());
        for (const point& vertex : vertices) {
            polygon.push_back({ vertex.x, vertex.y });
        }
        if (!gc.polygonFill(polygon, faceColor)) {
            // Fallback: fan the outline from its centroid. Renderers without a
            // native polygon fill show the fan's shared edges as faint rays, so
            // this path is only used when polygonFill is unavailable.
            for (size_t k = 0; k < vertices.size(); ++k) {
                const point& a = vertices[k];
                const point& b = vertices[(k + 1) % vertices.size()];
                gc.tri(center.x, center.y, center.z, a.x, a.y, a.z, b.x, b.y, b.z);
            }
        }
        gc.setLineStyle(L"-");
    }
    //=============================================================================
    // The notch marks the median confidence interval (median +/- 1.57 IQR /
    // sqrt(n)). When that interval reaches past a quartile the drawn endpoint is
    // reflected back inside the box instead of being clamped flat onto the
    // quartile, which keeps a short vertical edge between the quartile and the
    // notch shoulder rather than tapering the side straight from the corner.
    static double
    reflectedNotchEndpoint(double endpoint, double bound, double median)
    {
        double drawn = endpoint;
        if ((bound > median && endpoint > bound) || (bound < median && endpoint < bound)) {
            drawn = 2. * bound - endpoint;
        }
        double low = std::min(bound, median);
        double high = std::max(bound, median);
        return std::clamp(drawn, low, high);
    }
    //=============================================================================
    static double
    clampedNotchLow(const BoxChartStats& stats)
    {
        return reflectedNotchEndpoint(stats.notchLow, stats.q1, stats.median);
    }
    //=============================================================================
    static double
    clampedNotchHigh(const BoxChartStats& stats)
    {
        return reflectedNotchEndpoint(stats.notchHigh, stats.q3, stats.median);
    }
    //=============================================================================
    static void
    drawVerticalBox(RenderInterface& gc, const BoxChartItem& item, double boxWidth, double capWidth,
        bool notch, const std::vector<double>& faceColor, const std::vector<double>& edgeColor,
        const std::vector<double>& medianColor, const std::vector<double>& whiskerColor)
    {
        double half = boxWidth / 2.;
        double capHalf = capWidth / 2.;
        double left = item.position - half;
        double right = item.position + half;
        const BoxChartStats& stats = item.stats;
        double notchLow = clampedNotchLow(stats);
        double notchHigh = clampedNotchHigh(stats);
        double midLeft = item.position - half * 0.75;
        double midRight = item.position + half * 0.75;
        if (isDrawableColor(faceColor)) {
            gc.color(faceColor);
            if (notch) {
                drawFilledPolygon(gc, { item.position, stats.median, 0. },
                    { { left, stats.q1, 0. }, { left, notchLow, 0. }, { midLeft, stats.median, 0. },
                        { left, notchHigh, 0. }, { left, stats.q3, 0. }, { right, stats.q3, 0. },
                        { right, notchHigh, 0. }, { midRight, stats.median, 0. },
                        { right, notchLow, 0. }, { right, stats.q1, 0. } },
                    faceColor);
            } else {
                gc.quad(left, stats.q1, 0., right, stats.q1, 0., right, stats.q3, 0., left,
                    stats.q3, 0.);
            }
        }
        if (isDrawableColor(edgeColor)) {
            gc.color(edgeColor);
            gc.setLineStyle(L"-");
            if (notch) {
                gc.line(left, stats.q1, 0., left, notchLow, 0.);
                gc.line(left, notchLow, 0., midLeft, stats.median, 0.);
                gc.line(midLeft, stats.median, 0., left, notchHigh, 0.);
                gc.line(left, notchHigh, 0., left, stats.q3, 0.);
                gc.line(left, stats.q3, 0., right, stats.q3, 0.);
                gc.line(right, stats.q3, 0., right, notchHigh, 0.);
                gc.line(right, notchHigh, 0., midRight, stats.median, 0.);
                gc.line(midRight, stats.median, 0., right, notchLow, 0.);
                gc.line(right, notchLow, 0., right, stats.q1, 0.);
                gc.line(right, stats.q1, 0., left, stats.q1, 0.);
            } else {
                gc.quadline(left, stats.q1, 0., right, stats.q1, 0., right, stats.q3, 0., left,
                    stats.q3, 0.);
            }
        }
        if (isDrawableColor(medianColor)) {
            gc.color(medianColor);
            gc.setLineStyle(L"-");
            if (notch) {
                gc.line(midLeft, stats.median, 0., midRight, stats.median, 0.);
            } else {
                gc.line(left, stats.median, 0., right, stats.median, 0.);
            }
        }
        if (isDrawableColor(whiskerColor)) {
            gc.color(whiskerColor);
            if (stats.whiskerHigh > stats.q3) {
                gc.line(item.position, stats.q3, 0., item.position, stats.whiskerHigh, 0.);
                gc.line(item.position - capHalf, stats.whiskerHigh, 0., item.position + capHalf,
                    stats.whiskerHigh, 0.);
            }
            if (stats.whiskerLow < stats.q1) {
                gc.line(item.position, stats.q1, 0., item.position, stats.whiskerLow, 0.);
                gc.line(item.position - capHalf, stats.whiskerLow, 0., item.position + capHalf,
                    stats.whiskerLow, 0.);
            }
        }
    }
    //=============================================================================
    static void
    drawHorizontalBox(RenderInterface& gc, const BoxChartItem& item, double boxWidth,
        double capWidth, bool notch, const std::vector<double>& faceColor,
        const std::vector<double>& edgeColor, const std::vector<double>& medianColor,
        const std::vector<double>& whiskerColor)
    {
        double half = boxWidth / 2.;
        double capHalf = capWidth / 2.;
        double bottom = item.position - half;
        double top = item.position + half;
        const BoxChartStats& stats = item.stats;
        double notchLow = clampedNotchLow(stats);
        double notchHigh = clampedNotchHigh(stats);
        double midBottom = item.position - half * 0.75;
        double midTop = item.position + half * 0.75;
        if (isDrawableColor(faceColor)) {
            gc.color(faceColor);
            if (notch) {
                drawFilledPolygon(gc, { stats.median, item.position, 0. },
                    { { stats.q1, bottom, 0. }, { notchLow, bottom, 0. },
                        { stats.median, midBottom, 0. }, { notchHigh, bottom, 0. },
                        { stats.q3, bottom, 0. }, { stats.q3, top, 0. }, { notchHigh, top, 0. },
                        { stats.median, midTop, 0. }, { notchLow, top, 0. },
                        { stats.q1, top, 0. } },
                    faceColor);
            } else {
                gc.quad(stats.q1, bottom, 0., stats.q3, bottom, 0., stats.q3, top, 0., stats.q1,
                    top, 0.);
            }
        }
        if (isDrawableColor(edgeColor)) {
            gc.color(edgeColor);
            gc.setLineStyle(L"-");
            if (notch) {
                gc.line(stats.q1, bottom, 0., notchLow, bottom, 0.);
                gc.line(notchLow, bottom, 0., stats.median, midBottom, 0.);
                gc.line(stats.median, midBottom, 0., notchHigh, bottom, 0.);
                gc.line(notchHigh, bottom, 0., stats.q3, bottom, 0.);
                gc.line(stats.q3, bottom, 0., stats.q3, top, 0.);
                gc.line(stats.q3, top, 0., notchHigh, top, 0.);
                gc.line(notchHigh, top, 0., stats.median, midTop, 0.);
                gc.line(stats.median, midTop, 0., notchLow, top, 0.);
                gc.line(notchLow, top, 0., stats.q1, top, 0.);
                gc.line(stats.q1, top, 0., stats.q1, bottom, 0.);
            } else {
                gc.quadline(stats.q1, bottom, 0., stats.q3, bottom, 0., stats.q3, top, 0., stats.q1,
                    top, 0.);
            }
        }
        if (isDrawableColor(medianColor)) {
            gc.color(medianColor);
            gc.setLineStyle(L"-");
            if (notch) {
                gc.line(stats.median, midBottom, 0., stats.median, midTop, 0.);
            } else {
                gc.line(stats.median, bottom, 0., stats.median, top, 0.);
            }
        }
        if (isDrawableColor(whiskerColor)) {
            gc.color(whiskerColor);
            if (stats.whiskerHigh > stats.q3) {
                gc.line(stats.q3, item.position, 0., stats.whiskerHigh, item.position, 0.);
                gc.line(stats.whiskerHigh, item.position - capHalf, 0., stats.whiskerHigh,
                    item.position + capHalf, 0.);
            }
            if (stats.whiskerLow < stats.q1) {
                gc.line(stats.q1, item.position, 0., stats.whiskerLow, item.position, 0.);
                gc.line(stats.whiskerLow, item.position - capHalf, 0., stats.whiskerLow,
                    item.position + capHalf, 0.);
            }
        }
    }
    //=============================================================================
    static void
    validatePositive(GraphicsObject* object, const std::wstring& name)
    {
        double value = object->findScalarDoubleProperty(name);
        if (!std::isfinite(value) || value <= 0.) {
            Error(L"Nelson:graphics:mustBePositiveFinite", L"%s must be a positive finite scalar.",
                name);
        }
    }
    //=============================================================================
    static void
    validateRange(GraphicsObject* object, const std::wstring& name, double low, double high)
    {
        double value = object->findScalarDoubleProperty(name);
        if (!std::isfinite(value) || value < low || value > high) {
            Error(L"Nelson:boxchart:outOfRange", L"%s is outside its valid range.", name);
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
GOBoxChart::GOBoxChart()
{
    addProperty(this, new GOArrayOfProperty, GO_X_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_X_DATA_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_Y_DATA_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_X_VARIABLE_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_Y_VARIABLE_PROPERTY_NAME_STR);
    addProperty(this, new GOSourceTableProperty, GO_SOURCE_TABLE_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_BOX_WIDTH_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoFlatColorProperty, GO_BOX_FACE_COLOR_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoFlatColorProperty, GO_BOX_EDGE_COLOR_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoFlatColorProperty, GO_BOX_MEDIAN_LINE_COLOR_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty(0., 1.), GO_BOX_FACE_ALPHA_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_CAP_WIDTH_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoFlatColorProperty, GO_WHISKER_LINE_COLOR_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    addProperty(this, new GOLineStyleProperty, GO_WHISKER_LINE_STYLE_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoFlatColorProperty, GO_MARKER_COLOR_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_MARKER_SIZE_PROPERTY_NAME_STR);
    addProperty(this, new GOSymbolProperty, GO_MARKER_STYLE_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_CLIPPING_PROPERTY_NAME_STR);
    addProperty(
        this, new GORestrictedStringProperty(ORIENTATION_VALUES), GO_ORIENTATION_PROPERTY_NAME_STR);
    addProperty(this, new GORestrictedStringProperty(COLOR_GROUP_LAYOUT_VALUES),
        GO_COLOR_GROUP_LAYOUT_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_NOTCH_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_JITTER_OUTLIERS_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty(0., 1.), GO_COLOR_GROUP_WIDTH_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_BOX_FACE_COLOR_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_BOX_EDGE_COLOR_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_BOX_MEDIAN_LINE_COLOR_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_CAP_WIDTH_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_MARKER_COLOR_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_COLOR_GROUP_WIDTH_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR, false);
    addProperty(this, new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    addProperty(this, new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    addProperty(this, new GOLazyDecoratorProperty(GODecoratorKind::AnnotationEventData),
        GO_ANNOTATION_PROPERTY_NAME_STR);
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
    addProperty(this, new GOSeriesIndexProperty, GO_SERIES_INDEX_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_NUM_PEERS_PROPERTY_NAME_STR, true, false);
    addProperty(this, new GOLazyDecoratorProperty(GODecoratorKind::DataTipTemplate),
        GO_DATA_TIP_TEMPLATE_PROPERTY_NAME_STR, false);

    setRestrictedStringDefault(GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Y_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    setStringDefault(GO_X_VARIABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setStringDefault(GO_Y_VARIABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setScalarDoubleDefault(GO_BOX_WIDTH_PROPERTY_NAME_STR, 0.5);
    setColorSpec(this, GO_BOX_FACE_COLOR_PROPERTY_NAME_STR, defaultSeriesColor());
    setColorSpec(this, GO_BOX_EDGE_COLOR_PROPERTY_NAME_STR, defaultSeriesColor());
    setColorSpec(this, GO_BOX_MEDIAN_LINE_COLOR_PROPERTY_NAME_STR, defaultSeriesColor());
    setScalarDoubleDefault(GO_BOX_FACE_ALPHA_PROPERTY_NAME_STR, 0.2);
    setScalarDoubleDefault(GO_CAP_WIDTH_PROPERTY_NAME_STR, 0.25);
    setColorSpec(this, GO_WHISKER_LINE_COLOR_PROPERTY_NAME_STR, defaultWhiskerColor());
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 1.);
    setRestrictedStringDefault(GO_WHISKER_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setColorSpec(this, GO_MARKER_COLOR_PROPERTY_NAME_STR, defaultSeriesColor());
    setScalarDoubleDefault(GO_MARKER_SIZE_PROPERTY_NAME_STR, 6.);
    setRestrictedStringDefault(GO_MARKER_STYLE_PROPERTY_NAME_STR, L"o");
    setRestrictedStringDefault(GO_CLIPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_ORIENTATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VERTICAL_STR);
    setRestrictedStringDefault(GO_COLOR_GROUP_LAYOUT_PROPERTY_NAME_STR, L"grouped");
    setRestrictedStringDefault(GO_NOTCH_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_JITTER_OUTLIERS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setScalarDoubleDefault(GO_COLOR_GROUP_WIDTH_PROPERTY_NAME_STR, 1.);
    setRestrictedStringDefault(
        GO_BOX_FACE_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(
        GO_BOX_EDGE_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(
        GO_BOX_MEDIAN_LINE_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_CAP_WIDTH_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_MARKER_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(
        GO_COLOR_GROUP_WIDTH_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
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
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOX_CHART_STR);
    setStringDefault(GO_TAG_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    auto* seriesIndex
        = static_cast<GOSeriesIndexProperty*>(findProperty(GO_SERIES_INDEX_PROPERTY_NAME_STR));
    seriesIndex->setValue(1.);
    seriesIndex->clearModified();
    setScalarDoubleDefault(GO_NUM_PEERS_PROPERTY_NAME_STR, 1.);
    clearAllChanged();
    sortProperties();
}
//=============================================================================
GOBoxChart::~GOBoxChart() = default;
//=============================================================================
std::wstring
GOBoxChart::getType()
{
    return GO_PROPERTY_VALUE_BOX_CHART_STR;
}
//=============================================================================
void
GOBoxChart::updateState()
{
    if (hasChanged(GO_X_DATA_PROPERTY_NAME_STR) && !hasChanged(GO_X_DATA_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_X_DATA_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_DATA_PROPERTY_NAME_STR) && !hasChanged(GO_Y_DATA_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_Y_DATA_MODE_PROPERTY_NAME_STR);
    }
    syncColorMode(
        this, GO_BOX_FACE_COLOR_PROPERTY_NAME_STR, GO_BOX_FACE_COLOR_MODE_PROPERTY_NAME_STR);
    syncColorMode(
        this, GO_BOX_EDGE_COLOR_PROPERTY_NAME_STR, GO_BOX_EDGE_COLOR_MODE_PROPERTY_NAME_STR);
    syncColorMode(this, GO_BOX_MEDIAN_LINE_COLOR_PROPERTY_NAME_STR,
        GO_BOX_MEDIAN_LINE_COLOR_MODE_PROPERTY_NAME_STR);
    syncColorMode(this, GO_MARKER_COLOR_PROPERTY_NAME_STR, GO_MARKER_COLOR_MODE_PROPERTY_NAME_STR);
    if (hasChanged(GO_CAP_WIDTH_PROPERTY_NAME_STR)
        && !hasChanged(GO_CAP_WIDTH_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_CAP_WIDTH_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_COLOR_GROUP_WIDTH_PROPERTY_NAME_STR)
        && !hasChanged(GO_COLOR_GROUP_WIDTH_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_COLOR_GROUP_WIDTH_MODE_PROPERTY_NAME_STR);
    }

    validatePositive(this, GO_BOX_WIDTH_PROPERTY_NAME_STR);
    validatePositive(this, GO_CAP_WIDTH_PROPERTY_NAME_STR);
    validatePositive(this, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    validatePositive(this, GO_MARKER_SIZE_PROPERTY_NAME_STR);
    validateRange(this, GO_BOX_FACE_ALPHA_PROPERTY_NAME_STR, 0., 1.);
    validateRange(this, GO_COLOR_GROUP_WIDTH_PROPERTY_NAME_STR, 0., 1.);

    std::vector<double> color = automaticColor(this);
    if (isAuto(GO_BOX_FACE_COLOR_MODE_PROPERTY_NAME_STR)) {
        setColorSpec(this, GO_BOX_FACE_COLOR_PROPERTY_NAME_STR, color);
    }
    if (isAuto(GO_BOX_EDGE_COLOR_MODE_PROPERTY_NAME_STR)) {
        setColorSpec(this, GO_BOX_EDGE_COLOR_PROPERTY_NAME_STR, color);
    }
    if (isAuto(GO_BOX_MEDIAN_LINE_COLOR_MODE_PROPERTY_NAME_STR)) {
        setColorSpec(this, GO_BOX_MEDIAN_LINE_COLOR_PROPERTY_NAME_STR, color);
    }
    if (isAuto(GO_MARKER_COLOR_MODE_PROPERTY_NAME_STR)) {
        setColorSpec(this, GO_MARKER_COLOR_PROPERTY_NAME_STR, color);
    }
    if (isAuto(GO_CAP_WIDTH_MODE_PROPERTY_NAME_STR)) {
        setScalarDoubleDefault(GO_CAP_WIDTH_PROPERTY_NAME_STR,
            findScalarDoubleProperty(GO_BOX_WIDTH_PROPERTY_NAME_STR) / 2.);
        findProperty(GO_CAP_WIDTH_PROPERTY_NAME_STR)->clearModified();
    }
    if (isAuto(GO_COLOR_GROUP_WIDTH_MODE_PROPERTY_NAME_STR)) {
        setScalarDoubleDefault(
            GO_COLOR_GROUP_WIDTH_PROPERTY_NAME_STR, automaticColorGroupWidth(numPeersValue(this)));
        findProperty(GO_COLOR_GROUP_WIDTH_PROPERTY_NAME_STR)->clearModified();
    }

    items = buildItems(this);
    limitsDirty = true;
    clearChanged({ GO_X_DATA_PROPERTY_NAME_STR, GO_Y_DATA_PROPERTY_NAME_STR,
        GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_Y_DATA_MODE_PROPERTY_NAME_STR,
        GO_BOX_WIDTH_PROPERTY_NAME_STR, GO_BOX_FACE_COLOR_PROPERTY_NAME_STR,
        GO_BOX_EDGE_COLOR_PROPERTY_NAME_STR, GO_BOX_MEDIAN_LINE_COLOR_PROPERTY_NAME_STR,
        GO_BOX_FACE_ALPHA_PROPERTY_NAME_STR, GO_CAP_WIDTH_PROPERTY_NAME_STR,
        GO_WHISKER_LINE_COLOR_PROPERTY_NAME_STR, GO_LINE_WIDTH_PROPERTY_NAME_STR,
        GO_WHISKER_LINE_STYLE_PROPERTY_NAME_STR, GO_MARKER_COLOR_PROPERTY_NAME_STR,
        GO_MARKER_SIZE_PROPERTY_NAME_STR, GO_MARKER_STYLE_PROPERTY_NAME_STR,
        GO_ORIENTATION_PROPERTY_NAME_STR, GO_COLOR_GROUP_LAYOUT_PROPERTY_NAME_STR,
        GO_NOTCH_PROPERTY_NAME_STR, GO_JITTER_OUTLIERS_PROPERTY_NAME_STR,
        GO_COLOR_GROUP_WIDTH_PROPERTY_NAME_STR, GO_BOX_FACE_COLOR_MODE_PROPERTY_NAME_STR,
        GO_BOX_EDGE_COLOR_MODE_PROPERTY_NAME_STR, GO_BOX_MEDIAN_LINE_COLOR_MODE_PROPERTY_NAME_STR,
        GO_CAP_WIDTH_MODE_PROPERTY_NAME_STR, GO_MARKER_COLOR_MODE_PROPERTY_NAME_STR,
        GO_COLOR_GROUP_WIDTH_MODE_PROPERTY_NAME_STR, GO_SERIES_INDEX_PROPERTY_NAME_STR,
        GO_NUM_PEERS_PROPERTY_NAME_STR });
}
//=============================================================================
void
GOBoxChart::paintMe(RenderInterface& gc)
{
    updateState();
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR) || items.empty()) {
        return;
    }
    std::vector<double> autoColor = automaticColor(this);
    std::vector<double> faceColor = resolvedColor(this, GO_BOX_FACE_COLOR_PROPERTY_NAME_STR,
        GO_BOX_FACE_COLOR_MODE_PROPERTY_NAME_STR, autoColor);
    std::vector<double> edgeColor = resolvedColor(this, GO_BOX_EDGE_COLOR_PROPERTY_NAME_STR,
        GO_BOX_EDGE_COLOR_MODE_PROPERTY_NAME_STR, autoColor);
    std::vector<double> medianColor
        = resolvedColor(this, GO_BOX_MEDIAN_LINE_COLOR_PROPERTY_NAME_STR,
            GO_BOX_MEDIAN_LINE_COLOR_MODE_PROPERTY_NAME_STR, autoColor);
    std::vector<double> whiskerColor = resolvedUnmodedColor(
        this, GO_WHISKER_LINE_COLOR_PROPERTY_NAME_STR, defaultWhiskerColor());
    std::vector<double> markerColor = resolvedColor(
        this, GO_MARKER_COLOR_PROPERTY_NAME_STR, GO_MARKER_COLOR_MODE_PROPERTY_NAME_STR, autoColor);
    appendAlpha(faceColor, findScalarDoubleProperty(GO_BOX_FACE_ALPHA_PROPERTY_NAME_STR));
    double width = findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR);
    double baseBoxWidth = findScalarDoubleProperty(GO_BOX_WIDTH_PROPERTY_NAME_STR);
    double unitWidth = positionUnitWidth(items, 2. * baseBoxWidth);
    double boxWidth = drawBoxWidth(this);
    double capWidth = findScalarDoubleProperty(GO_CAP_WIDTH_PROPERTY_NAME_STR);
    bool horizontal
        = findStringProperty(GO_ORIENTATION_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_HORIZONTAL_STR;
    bool notch = findStringProperty(GO_NOTCH_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_ON_STR;
    double positionOffset = colorGroupOffset(this, unitWidth);
    gc.lineWidth(width);
    for (const BoxChartItem& item : items) {
        BoxChartItem drawItem = shiftedItem(item, positionOffset);
        gc.setLineStyle(L"-");
        if (horizontal) {
            drawHorizontalBox(gc, drawItem, boxWidth, capWidth, notch, faceColor, edgeColor,
                medianColor, whiskerColor);
        } else {
            drawVerticalBox(gc, drawItem, boxWidth, capWidth, notch, faceColor, edgeColor,
                medianColor, whiskerColor);
        }
    }
    if (!isDrawableColor(markerColor)) {
        return;
    }
    RenderInterface::SymbolType marker
        = StringToSymbol(findStringProperty(GO_MARKER_STYLE_PROPERTY_NAME_STR));
    if (marker == RenderInterface::None) {
        return;
    }
    bool jitter
        = findStringProperty(GO_JITTER_OUTLIERS_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_ON_STR;
    GOAxis* axis = static_cast<GOAxis*>(getParentAxis());
    std::vector<point> markerPixels;
    for (const BoxChartItem& item : items) {
        BoxChartItem drawItem = shiftedItem(item, positionOffset);
        for (size_t k = 0; k < item.stats.outliers.size(); ++k) {
            double offset = jitter ? jitterOffset(k, boxWidth) : 0.;
            double x = horizontal ? item.stats.outliers[k] : drawItem.position + offset;
            double y = horizontal ? drawItem.position + offset : item.stats.outliers[k];
            double px = 0.;
            double py = 0.;
            if (mapToPixel(axis, gc, x, y, px, py)) {
                markerPixels.emplace_back(px, py, 0.);
            }
        }
    }
    if (markerPixels.empty()) {
        return;
    }
    gc.setupDirectDraw();
    gc.lineWidth(width);
    gc.setLineStyle(L"-");
    double markerSize = findScalarDoubleProperty(GO_MARKER_SIZE_PROPERTY_NAME_STR);
    std::vector<double> ux;
    std::vector<double> uy;
    ux.reserve(markerPixels.size());
    uy.reserve(markerPixels.size());
    for (const point& markerPixel : markerPixels) {
        ux.push_back(markerPixel.x);
        uy.push_back(markerPixel.y);
    }
    // One batched primitive (RenderWeb) for the outlier markers; Qt/OpenGL return
    // false and the per-point loop runs.
    if (!DrawSymbolBatchUniform(
            gc, marker, ux, uy, markerSize, markerColor, { -1., -1., -1. }, width)) {
        for (const point& markerPixel : markerPixels) {
            DrawSymbol(gc, marker, markerPixel.x, markerPixel.y, 0., markerSize, markerColor,
                { -1., -1., -1. }, width);
        }
    }
    gc.releaseDirectDraw();
}
//=============================================================================
std::vector<double>
GOBoxChart::getLimits()
{
    if (!limitsDirty) {
        return limits;
    }
    if (items.empty()) {
        limits = { 0.5, 1.5, 0., 1., 0., 0., 0., 1., 1., 1. };
        limitsDirty = false;
        return limits;
    }
    bool automaticPositions = isAuto(GO_X_DATA_MODE_PROPERTY_NAME_STR);
    double boxHalf = findScalarDoubleProperty(GO_BOX_WIDTH_PROPERTY_NAME_STR) / 2.;
    double capHalf = findScalarDoubleProperty(GO_CAP_WIDTH_PROPERTY_NAME_STR) / 2.;
    double posPad = automaticPositions ? 0. : std::max(boxHalf, capHalf);
    double minPosition = std::numeric_limits<double>::infinity();
    double maxPosition = -std::numeric_limits<double>::infinity();
    double minValue = std::numeric_limits<double>::infinity();
    double maxValue = -std::numeric_limits<double>::infinity();
    auto appendValue = [&](double value) {
        if (std::isfinite(value)) {
            minValue = std::min(minValue, value);
            maxValue = std::max(maxValue, value);
        }
    };
    for (const BoxChartItem& item : items) {
        minPosition = std::min(minPosition, item.position - posPad);
        maxPosition = std::max(maxPosition, item.position + posPad);
        appendValue(item.stats.q1);
        appendValue(item.stats.median);
        appendValue(item.stats.q3);
        appendValue(item.stats.whiskerLow);
        appendValue(item.stats.whiskerHigh);
        for (double value : item.stats.outliers) {
            appendValue(value);
        }
    }
    if (!std::isfinite(minValue) || !std::isfinite(maxValue)) {
        minValue = 0.;
        maxValue = 1.;
    }
    if (minValue == maxValue) {
        minValue -= 0.5;
        maxValue += 0.5;
    } else {
        expandToNiceValueLimits(minValue, maxValue);
    }
    if (minPosition == maxPosition) {
        minPosition -= 0.5;
        maxPosition += 0.5;
    }
    bool horizontal
        = findStringProperty(GO_ORIENTATION_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_HORIZONTAL_STR;
    if (horizontal) {
        limits = { minValue, maxValue, minPosition, maxPosition, 0., 0., 0., 1., 1., 1. };
    } else {
        limits = { minPosition, maxPosition, minValue, maxValue, 0., 0., 0., 1., 1., 1. };
    }
    limitsDirty = false;
    return limits;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
