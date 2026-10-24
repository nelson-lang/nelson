//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOHistogram2.hpp"
#include "GOHistogramBinning.hpp"
#include "GOChartDecorators.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOAxis.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOEdgeAlphaProperty.hpp"
#include "GOFaceAlphaProperty.hpp"
#include "GOFigure.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOPickablePartsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOSeriesIndexProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "RenderInterface.hpp"
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
    static const std::vector<std::wstring> BIN_METHOD_VALUES
        = { L"auto", L"manual", L"scott", L"fd", L"integers" };
    static const std::vector<std::wstring> NORMALIZATION_VALUES
        = { L"count", L"probability", L"countdensity", L"pdf", L"cumcount", L"cdf" };
    static const std::vector<std::wstring> DISPLAY_STYLE_VALUES = { L"bar3", L"tile" };
    static const std::vector<std::wstring> FACE_LIGHTING_VALUES
        = { GO_PROPERTY_VALUE_NONE_STR, GO_PROPERTY_VALUE_FLAT_STR, GO_PROPERTY_VALUE_LIT_STR };
    //=============================================================================
    static void
    addProperty(GraphicsObject* go, GOGenericProperty* property, const std::wstring& name,
        bool writable = true)
    {
        go->registerProperty(property, name, writable);
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
    static std::vector<double>
    numericVector(const ArrayOf& value)
    {
        if (value.isEmpty()) {
            return {};
        }
        if (!value.isNumeric()) {
            Error("Nelson:graphics:numericExpected", "Numeric data expected.");
        }
        ArrayOf asDouble(value);
        asDouble.promoteType(NLS_DOUBLE);
        return asDouble.asDoubleVector();
    }
    //=============================================================================
    static std::vector<double>
    validatedEdges(const std::vector<double>& values, const std::wstring& name)
    {
        if (values.size() < 2) {
            Error(
                L"Nelson:histogram2:binEdgesTooFew", L"%s must have at least two elements.", name);
        }
        for (size_t k = 1; k < values.size(); ++k) {
            if (!std::isfinite(values[k]) || values[k] < values[k - 1]) {
                Error(L"Nelson:histogram2:binEdgesNotMonotonic",
                    L"%s must be a nondecreasing finite vector.", name);
            }
        }
        return values;
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
        if (nBins < 1) {
            nBins = 1;
        }
        double width = (high - low) / static_cast<double>(nBins);
        if (!std::isfinite(width) || width <= 0.) {
            width = 1.;
        }
        std::vector<double> edges(nBins + 1);
        for (size_t k = 0; k <= nBins; ++k) {
            edges[k] = low + width * static_cast<double>(k);
        }
        edges[nBins] = high;
        return edges;
    }
    //=============================================================================
    static double
    roundScale(int decimals)
    {
        return std::pow(10., static_cast<double>(std::max(0, decimals)));
    }
    //=============================================================================
    static double
    roundDownTo(double value, int decimals)
    {
        double scale = roundScale(decimals);
        return std::floor(value * scale) / scale;
    }
    //=============================================================================
    static double
    roundUpTo(double value, int decimals)
    {
        double scale = roundScale(decimals);
        return std::ceil(value * scale - 1e-12) / scale;
    }
    //=============================================================================
    static double
    roundStepUpTo(double value, int decimals)
    {
        double scale = roundScale(decimals);
        return std::ceil(value * scale - 1e-12) / scale;
    }
    //=============================================================================
    static int
    edgeDecimalsForWidth(double width)
    {
        if (width >= 0.75) {
            return 1;
        }
        if (width >= 0.1) {
            return 2;
        }
        return static_cast<int>(std::ceil(-std::log10(width))) + 1;
    }
    //=============================================================================
    static std::vector<double>
    numBinsEdges(double low, double high, size_t nBins)
    {
        if (nBins < 1) {
            nBins = 1;
        }
        if (low == high) {
            low -= 0.5;
            high += 0.5;
        }
        double range = high - low;
        double rawWidth = range / static_cast<double>(nBins);
        if (!std::isfinite(rawWidth) || rawWidth <= 0.) {
            return uniformEdges(low, high, nBins);
        }
        if (std::fabs(rawWidth * static_cast<double>(nBins) - range)
            <= std::max(1., std::fabs(range)) * 1e-12) {
            double rawScale = std::pow(10., std::ceil(-std::log10(rawWidth)) + 2.);
            double roundedWidth = std::round(rawWidth * rawScale) / rawScale;
            if (std::fabs(roundedWidth - rawWidth) <= std::max(1., rawWidth) * 1e-12) {
                return uniformEdges(low, high, nBins);
            }
        }

        int decimals = edgeDecimalsForWidth(rawWidth);
        double first = low;
        double step = rawWidth;
        if (low < 0. && high > 0.) {
            if (rawWidth >= 0.5 && nBins > 10) {
                first = std::floor(low * 2.) / 2.;
                double upper = roundUpTo(high, decimals);
                step = roundStepUpTo((upper - first) / static_cast<double>(nBins), decimals);
            } else {
                double upper = roundUpTo(high, decimals);
                step = roundStepUpTo((upper - low) / static_cast<double>(nBins), decimals);
                first = upper - step * static_cast<double>(nBins);
            }
        } else {
            step = roundStepUpTo(rawWidth, decimals);
            if (low >= 0.) {
                first = roundDownTo(low, decimals);
            } else {
                double upper = roundUpTo(high, decimals);
                first = upper - step * static_cast<double>(nBins);
            }
        }

        if (!std::isfinite(step) || step <= 0.) {
            return uniformEdges(low, high, nBins);
        }
        std::vector<double> edges(nBins + 1);
        for (size_t k = 0; k <= nBins; ++k) {
            edges[k] = first + step * static_cast<double>(k);
        }
        return edges;
    }
    //=============================================================================
    static std::vector<double>
    edgesFromWidth(double low, double high, double width)
    {
        if (!std::isfinite(width) || width <= 0.) {
            Error("Nelson:histogram2:invalidBinWidth",
                "BinWidth must contain positive finite values.");
        }
        size_t nBins = static_cast<size_t>(std::ceil((high - low) / width));
        if (nBins < 1) {
            nBins = 1;
        }
        std::vector<double> edges(nBins + 1);
        for (size_t k = 0; k <= nBins; ++k) {
            edges[k] = low + width * static_cast<double>(k);
        }
        if (edges.back() < high) {
            edges.push_back(edges.back() + width);
        }
        return edges;
    }
    //=============================================================================
    static std::vector<double>
    currentEdges(
        GraphicsObject* object, const std::wstring& propertyName, const std::wstring& label)
    {
        ArrayOf value = object->findArrayOfProperty(propertyName);
        if (value.isEmpty()) {
            return {};
        }
        return validatedEdges(numericVector(value), label);
    }
    //=============================================================================
    static std::vector<double>
    computedEdges(GraphicsObject* object, const std::vector<double>& data,
        const std::wstring& edgesProperty, const std::wstring& limitsProperty,
        const std::wstring& limitsModeProperty, bool& explicitEdges, double width, double nBins,
        const std::wstring& label)
    {
        if (object->isAuto(limitsModeProperty)) {
            setArray(object, limitsProperty, rowVector(limitsFromData(data)));
        }
        std::vector<double> limits = validatedLimits(
            numericVector(object->findArrayOfProperty(limitsProperty)), limitsProperty);
        std::vector<double> edges = currentEdges(object, edgesProperty, label);
        if (object->hasChanged(edgesProperty) && !edges.empty()) {
            explicitEdges = true;
            return edges;
        }
        if (explicitEdges && !edges.empty()) {
            return edges;
        }
        if (object->hasChanged(GO_BIN_WIDTH_PROPERTY_NAME_STR)) {
            return edgesFromWidth(limits[0], limits[1], width);
        }
        // Automatic binning (BinMethod auto/scott/fd/integers) when neither the bin count nor
        // the limits were requested: pick a 'nice' bin width per dimension from the data.
        if (!object->hasChanged(GO_NUM_BINS_PROPERTY_NAME_STR)
            && object->isAuto(limitsModeProperty)) {
            std::vector<double> autoEdges = HistogramBinning::autoEdges2D(
                data, object->findStringProperty(GO_BIN_METHOD_PROPERTY_NAME_STR));
            if (autoEdges.size() >= 2) {
                return autoEdges;
            }
        }
        return numBinsEdges(
            limits[0], limits[1], static_cast<size_t>(std::max(1., std::floor(nBins))));
    }
    //=============================================================================
    static void
    dataColumns(const ArrayOf& value, std::vector<double>& x, std::vector<double>& y)
    {
        x.clear();
        y.clear();
        if (value.isEmpty()) {
            return;
        }
        ArrayOf asDouble(value);
        asDouble.promoteType(NLS_DOUBLE);
        if (!asDouble.isNumeric() || asDouble.isComplex()) {
            Error("Nelson:histogram2:invalidDataType", "Data must be a real numeric matrix.");
        }
        size_t rows = static_cast<size_t>(asDouble.rows());
        size_t columns = static_cast<size_t>(asDouble.columns());
        std::vector<double> values = asDouble.asDoubleVector();
        if (columns == 2) {
            x.reserve(rows);
            y.reserve(rows);
            for (size_t k = 0; k < rows; ++k) {
                x.push_back(values[k]);
                y.push_back(values[k + rows]);
            }
            return;
        }
        if (rows == 2) {
            x.reserve(columns);
            y.reserve(columns);
            for (size_t k = 0; k < columns; ++k) {
                x.push_back(values[2 * k]);
                y.push_back(values[2 * k + 1]);
            }
            return;
        }
        Error("Nelson:histogram2:invalidDataColumns", "Data must have two columns.");
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
            size_t xi = 0;
            if (xv == xEdges.back()) {
                xi = xBins - 1;
            } else {
                xi = static_cast<size_t>(std::distance(
                         xEdges.begin(), std::upper_bound(xEdges.begin(), xEdges.end(), xv)))
                    - 1;
            }
            size_t yi = 0;
            if (yv == yEdges.back()) {
                yi = yBins - 1;
            } else {
                yi = static_cast<size_t>(std::distance(
                         yEdges.begin(), std::upper_bound(yEdges.begin(), yEdges.end(), yv)))
                    - 1;
            }
            if (xi < xBins && yi < yBins) {
                counts[xi + xBins * yi] += 1.;
            }
        }
        return counts;
    }
    //=============================================================================
    static std::vector<double>
    normalizedValues(const std::vector<double>& counts, const std::vector<double>& xEdges,
        const std::vector<double>& yEdges, const std::wstring& normalization)
    {
        std::vector<double> values(counts);
        double total = 0.;
        for (double count : counts) {
            total += count;
        }
        if (normalization == L"probability" || normalization == L"pdf" || normalization == L"cdf") {
            double denom = total > 0. ? total : 1.;
            for (double& value : values) {
                value /= denom;
            }
        }
        if (normalization == L"countdensity" || normalization == L"pdf") {
            size_t xBins = xEdges.size() > 1 ? xEdges.size() - 1 : 0;
            size_t yBins = yEdges.size() > 1 ? yEdges.size() - 1 : 0;
            for (size_t yi = 0; yi < yBins; ++yi) {
                for (size_t xi = 0; xi < xBins; ++xi) {
                    double area = (xEdges[xi + 1] - xEdges[xi]) * (yEdges[yi + 1] - yEdges[yi]);
                    if (area > 0.) {
                        values[xi + xBins * yi] /= area;
                    }
                }
            }
        }
        if (normalization == L"cumcount" || normalization == L"cdf") {
            size_t xBins = xEdges.size() > 1 ? xEdges.size() - 1 : 0;
            size_t yBins = yEdges.size() > 1 ? yEdges.size() - 1 : 0;
            for (size_t yi = 0; yi < yBins; ++yi) {
                for (size_t xi = 0; xi < xBins; ++xi) {
                    double cumulative = 0.;
                    for (size_t yj = 0; yj <= yi; ++yj) {
                        for (size_t xj = 0; xj <= xi; ++xj) {
                            cumulative += values[xj + xBins * yj];
                        }
                    }
                    values[xi + xBins * yi] = cumulative;
                }
            }
        }
        return values;
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
    propertyColor(GraphicsObject* object, const std::wstring& propertyName,
        const std::vector<double>& defaultColor)
    {
        auto* property
            = static_cast<GORestrictedStringColorProperty*>(object->findProperty(propertyName));
        std::wstring value = object->findStringProperty(propertyName);
        if (value == GO_PROPERTY_VALUE_NONE_STR) {
            return { -1. };
        }
        if (value == GO_PROPERTY_VALUE_COLORSPEC_STR) {
            return property->colorSpec();
        }
        return defaultColor;
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
    static bool
    isDrawableColor(const std::vector<double>& color)
    {
        return color.size() >= 3 && color[0] >= 0.;
    }
    //=============================================================================
    struct CuboidTopEdges
    {
        bool yLow = true;
        bool xHigh = true;
        bool yHigh = true;
        bool xLow = true;
    };
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
    static std::vector<double>
    shadedFaceColor(const std::vector<double>& color, double factor)
    {
        std::vector<double> shaded(color);
        if (shaded.size() >= 3) {
            shaded[0] = std::clamp(shaded[0] * factor, 0., 1.);
            shaded[1] = std::clamp(shaded[1] * factor, 0., 1.);
            shaded[2] = std::clamp(shaded[2] * factor, 0., 1.);
        }
        return shaded;
    }
    //=============================================================================
    static void
    drawCuboid(RenderInterface& gc, double x0, double x1, double y0, double y1, double z,
        const std::vector<double>& faceColor, const std::vector<double>& edgeColor, bool lit,
        const CuboidTopEdges& topEdges)
    {
        if (z == 0.) {
            return;
        }
        double z0 = std::min(0., z);
        double z1 = std::max(0., z);
        if (isDrawableColor(faceColor)) {
            gc.color(faceColor);
            gc.quad(x0, y0, z1, x1, y0, z1, x1, y1, z1, x0, y1, z1);
            gc.color(lit ? shadedFaceColor(faceColor, 0.75) : faceColor);
            gc.quad(x0, y0, z0, x1, y0, z0, x1, y0, z1, x0, y0, z1);
            gc.color(lit ? shadedFaceColor(faceColor, 0.65) : faceColor);
            gc.quad(x1, y0, z0, x1, y1, z0, x1, y1, z1, x1, y0, z1);
            gc.color(lit ? shadedFaceColor(faceColor, 0.75) : faceColor);
            gc.quad(x1, y1, z0, x0, y1, z0, x0, y1, z1, x1, y1, z1);
            gc.color(lit ? shadedFaceColor(faceColor, 0.65) : faceColor);
            gc.quad(x0, y1, z0, x0, y0, z0, x0, y0, z1, x0, y1, z1);
        }
        if (isDrawableColor(edgeColor)) {
            gc.color(edgeColor);
            if (topEdges.yLow) {
                gc.line(x0, y0, z1, x1, y0, z1);
            }
            if (topEdges.xHigh) {
                gc.line(x1, y0, z1, x1, y1, z1);
            }
            if (topEdges.yHigh) {
                gc.line(x1, y1, z1, x0, y1, z1);
            }
            if (topEdges.xLow) {
                gc.line(x0, y1, z1, x0, y0, z1);
            }
            gc.quadline(x0, y0, z0, x1, y0, z0, x1, y1, z0, x0, y1, z0);
            gc.line(x0, y0, z0, x0, y0, z1);
            gc.line(x1, y0, z0, x1, y0, z1);
            gc.line(x1, y1, z0, x1, y1, z1);
            gc.line(x0, y1, z0, x0, y1, z1);
        }
    }
    //=============================================================================
    static void
    drawHistogramCuboid(RenderInterface& gc, double x0, double x1, double y0, double y1, double z,
        const std::vector<double>& faceColor, const std::vector<double>& edgeColor, bool lit,
        double paddingFraction, const CuboidTopEdges& topEdges)
    {
        double xPadding = (x1 - x0) * paddingFraction;
        double yPadding = (y1 - y0) * paddingFraction;
        drawCuboid(gc, x0 + xPadding, x1 - xPadding, y0 + yPadding, y1 - yPadding, z, faceColor,
            edgeColor, lit, topEdges);
    }
    //=============================================================================
    static void
    drawTile(RenderInterface& gc, double x0, double x1, double y0, double y1,
        const std::vector<double>& faceColor, const std::vector<double>& edgeColor)
    {
        if (isDrawableColor(faceColor)) {
            gc.color(faceColor);
            gc.quad(x0, y0, 0., x1, y0, 0., x1, y1, 0., x0, y1, 0.);
        }
        if (isDrawableColor(edgeColor)) {
            gc.color(edgeColor);
            gc.quadline(x0, y0, 0., x1, y0, 0., x1, y1, 0., x0, y1, 0.);
        }
    }
    //=============================================================================
    static bool
    topEdgeCoveredByNeighbor(double value, double neighborValue)
    {
        if (value > 0.) {
            return neighborValue >= value;
        }
        if (value < 0.) {
            return neighborValue <= value;
        }
        return false;
    }
    //=============================================================================
} // namespace
//=============================================================================
GOHistogram2::GOHistogram2()
{
    addProperty(this, new GOArrayOfProperty, GO_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_BIN_COUNTS_PROPERTY_NAME_STR);
    addProperty(this, new GOTwoVectorProperty, GO_NUM_BINS_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_X_BIN_EDGES_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_Y_BIN_EDGES_PROPERTY_NAME_STR);
    addProperty(this, new GOTwoVectorProperty, GO_BIN_WIDTH_PROPERTY_NAME_STR);
    addProperty(
        this, new GORestrictedStringProperty(BIN_METHOD_VALUES), GO_BIN_METHOD_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_X_BIN_LIMITS_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_Y_BIN_LIMITS_PROPERTY_NAME_STR);
    addProperty(this, new GORestrictedStringProperty(NORMALIZATION_VALUES),
        GO_NORMALIZATION_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoFlatColorProperty, GO_FACE_COLOR_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoFlatColorProperty, GO_EDGE_COLOR_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    addProperty(this, new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    addProperty(this, new GORestrictedStringProperty(FACE_LIGHTING_VALUES),
        GO_FACE_LIGHTING_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_BIN_COUNTS_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_X_BIN_LIMITS_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_Y_BIN_LIMITS_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GORestrictedStringProperty(DISPLAY_STYLE_VALUES),
        GO_DISPLAY_STYLE_PROPERTY_NAME_STR);
    addProperty(this, new GOFaceAlphaProperty, GO_FACE_ALPHA_PROPERTY_NAME_STR);
    addProperty(this, new GOEdgeAlphaProperty, GO_EDGE_ALPHA_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_SHOW_EMPTY_BINS_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_VALUES_PROPERTY_NAME_STR, false);
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
    addProperty(this, new GOSeriesIndexProperty, GO_SERIES_INDEX_PROPERTY_NAME_STR);

    setTwoVectorDefault(GO_NUM_BINS_PROPERTY_NAME_STR, 10., 10.);
    setTwoVectorDefault(GO_BIN_WIDTH_PROPERTY_NAME_STR, 1., 1.);
    setArray(this, GO_X_BIN_LIMITS_PROPERTY_NAME_STR, rowVector({ 0., 1. }));
    setArray(this, GO_Y_BIN_LIMITS_PROPERTY_NAME_STR, rowVector({ 0., 1. }));
    setRestrictedStringDefault(GO_BIN_METHOD_PROPERTY_NAME_STR, L"auto");
    setRestrictedStringDefault(GO_NORMALIZATION_PROPERTY_NAME_STR, L"count");
    setRestrictedStringColorDefault(
        GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR, 0.066, 0.443, 0.745);
    setRestrictedStringColorDefault(GO_EDGE_COLOR_PROPERTY_NAME_STR,
        GO_PROPERTY_VALUE_COLORSPEC_STR, 0.129411764705882, 0.129411764705882, 0.129411764705882);
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setRestrictedStringDefault(GO_FACE_LIGHTING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LIT_STR);
    setRestrictedStringDefault(GO_BIN_COUNTS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_X_BIN_LIMITS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Y_BIN_LIMITS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_DISPLAY_STYLE_PROPERTY_NAME_STR, L"bar3");
    setRestrictedStringScalarDefault(
        GO_FACE_ALPHA_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALAR_STR, 1.);
    setRestrictedStringScalarDefault(
        GO_EDGE_ALPHA_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALAR_STR, 1.);
    setRestrictedStringDefault(GO_SHOW_EMPTY_BINS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
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
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_HISTOGRAM2_STR);
    setStringDefault(GO_TAG_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    auto* seriesIndex
        = static_cast<GOSeriesIndexProperty*>(findProperty(GO_SERIES_INDEX_PROPERTY_NAME_STR));
    seriesIndex->setValue(1.);
    seriesIndex->clearModified();
    clearAllChanged();
    sortProperties();
}
//=============================================================================
GOHistogram2::~GOHistogram2() = default;
//=============================================================================
std::wstring
GOHistogram2::getType()
{
    return GO_PROPERTY_VALUE_HISTOGRAM2_STR;
}
//=============================================================================
void
GOHistogram2::updateState()
{
    bool displayStyleChanged = hasChanged(GO_DISPLAY_STYLE_PROPERTY_NAME_STR);
    bool faceLightingChanged = hasChanged(GO_FACE_LIGHTING_PROPERTY_NAME_STR);
    std::wstring displayStyle = findStringProperty(GO_DISPLAY_STYLE_PROPERTY_NAME_STR);
    if (displayStyle == L"tile") {
        if (faceLightingChanged
            && findStringProperty(GO_FACE_LIGHTING_PROPERTY_NAME_STR)
                != GO_PROPERTY_VALUE_NONE_STR) {
            setRestrictedStringDefault(
                GO_FACE_LIGHTING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
            clearChanged(GO_FACE_LIGHTING_PROPERTY_NAME_STR);
            Error("Nelson:histogram2:invalidFaceLighting",
                "FaceLighting must be 'none' when DisplayStyle is 'tile'.");
        }
        if (displayStyleChanged && !faceLightingChanged) {
            setRestrictedStringDefault(
                GO_FACE_LIGHTING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
        }
    } else if (displayStyleChanged && !faceLightingChanged) {
        setRestrictedStringDefault(GO_FACE_LIGHTING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LIT_STR);
    }
    if (displayStyleChanged) {
        clearChanged(GO_DISPLAY_STYLE_PROPERTY_NAME_STR);
    }
    if (faceLightingChanged) {
        clearChanged(GO_FACE_LIGHTING_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_BIN_COUNTS_PROPERTY_NAME_STR)) {
        toManual(GO_BIN_COUNTS_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_X_BIN_LIMITS_PROPERTY_NAME_STR)) {
        toManual(GO_X_BIN_LIMITS_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_BIN_LIMITS_PROPERTY_NAME_STR)) {
        toManual(GO_Y_BIN_LIMITS_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_NUM_BINS_PROPERTY_NAME_STR) || hasChanged(GO_BIN_WIDTH_PROPERTY_NAME_STR)
        || hasChanged(GO_BIN_METHOD_PROPERTY_NAME_STR)) {
        xEdgesExplicit = false;
        yEdgesExplicit = false;
    }

    bool needsUpdate = hasChanged(GO_DATA_PROPERTY_NAME_STR)
        || hasChanged(GO_BIN_COUNTS_PROPERTY_NAME_STR) || hasChanged(GO_NUM_BINS_PROPERTY_NAME_STR)
        || hasChanged(GO_X_BIN_EDGES_PROPERTY_NAME_STR)
        || hasChanged(GO_Y_BIN_EDGES_PROPERTY_NAME_STR)
        || hasChanged(GO_BIN_WIDTH_PROPERTY_NAME_STR)
        || hasChanged(GO_X_BIN_LIMITS_PROPERTY_NAME_STR)
        || hasChanged(GO_Y_BIN_LIMITS_PROPERTY_NAME_STR)
        || hasChanged(GO_BIN_METHOD_PROPERTY_NAME_STR)
        || hasChanged(GO_BIN_COUNTS_MODE_PROPERTY_NAME_STR)
        || hasChanged(GO_X_BIN_LIMITS_MODE_PROPERTY_NAME_STR)
        || hasChanged(GO_Y_BIN_LIMITS_MODE_PROPERTY_NAME_STR)
        || hasChanged(GO_NORMALIZATION_PROPERTY_NAME_STR);
    if (!needsUpdate) {
        return;
    }

    std::vector<double> xEdges;
    std::vector<double> yEdges;
    std::vector<double> counts;
    size_t xBins = 0;
    size_t yBins = 0;
    if (!isAuto(GO_BIN_COUNTS_MODE_PROPERTY_NAME_STR)
        && !findArrayOfProperty(GO_BIN_COUNTS_PROPERTY_NAME_STR).isEmpty()) {
        xEdges = currentEdges(this, GO_X_BIN_EDGES_PROPERTY_NAME_STR, L"XBinEdges");
        yEdges = currentEdges(this, GO_Y_BIN_EDGES_PROPERTY_NAME_STR, L"YBinEdges");
        if (xEdges.size() < 2 || yEdges.size() < 2) {
            Error("Nelson:histogram2:binCountsRequiresEdges",
                "BinCounts requires XBinEdges and YBinEdges.");
        }
        xBins = xEdges.size() - 1;
        yBins = yEdges.size() - 1;
        counts = numericVector(findArrayOfProperty(GO_BIN_COUNTS_PROPERTY_NAME_STR));
        if (counts.size() != xBins * yBins) {
            Error("Nelson:histogram2:binCountsSizeMismatch",
                "BinCounts size must match XBinEdges and YBinEdges.");
        }
    } else {
        std::vector<double> xData;
        std::vector<double> yData;
        dataColumns(findArrayOfProperty(GO_DATA_PROPERTY_NAME_STR), xData, yData);
        std::vector<double> binWidth = findVectorDoubleProperty(GO_BIN_WIDTH_PROPERTY_NAME_STR);
        std::vector<double> nBins = findVectorDoubleProperty(GO_NUM_BINS_PROPERTY_NAME_STR);
        xEdges = computedEdges(this, xData, GO_X_BIN_EDGES_PROPERTY_NAME_STR,
            GO_X_BIN_LIMITS_PROPERTY_NAME_STR, GO_X_BIN_LIMITS_MODE_PROPERTY_NAME_STR,
            xEdgesExplicit, binWidth[0], nBins[0], L"XBinEdges");
        yEdges = computedEdges(this, yData, GO_Y_BIN_EDGES_PROPERTY_NAME_STR,
            GO_Y_BIN_LIMITS_PROPERTY_NAME_STR, GO_Y_BIN_LIMITS_MODE_PROPERTY_NAME_STR,
            yEdgesExplicit, binWidth[1], nBins[1], L"YBinEdges");
        xBins = xEdges.size() - 1;
        yBins = yEdges.size() - 1;
        counts = countsForEdges(xData, yData, xEdges, yEdges);
        setArray(this, GO_BIN_COUNTS_PROPERTY_NAME_STR, matrixArray(xBins, yBins, counts));
        setArray(this, GO_X_BIN_EDGES_PROPERTY_NAME_STR, rowVector(xEdges));
        setArray(this, GO_Y_BIN_EDGES_PROPERTY_NAME_STR, rowVector(yEdges));
    }

    setTwoVectorDefault(
        GO_NUM_BINS_PROPERTY_NAME_STR, static_cast<double>(xBins), static_cast<double>(yBins));
    if (xEdges.size() > 1 && yEdges.size() > 1) {
        setTwoVectorDefault(
            GO_BIN_WIDTH_PROPERTY_NAME_STR, xEdges[1] - xEdges[0], yEdges[1] - yEdges[0]);
    }
    setArray(this, GO_X_BIN_LIMITS_PROPERTY_NAME_STR, rowVector({ xEdges.front(), xEdges.back() }));
    setArray(this, GO_Y_BIN_LIMITS_PROPERTY_NAME_STR, rowVector({ yEdges.front(), yEdges.back() }));
    if (xEdgesExplicit || yEdgesExplicit || hasChanged(GO_X_BIN_EDGES_PROPERTY_NAME_STR)
        || hasChanged(GO_Y_BIN_EDGES_PROPERTY_NAME_STR)) {
        setRestrictedStringDefault(GO_BIN_METHOD_PROPERTY_NAME_STR, L"manual");
    }
    if (xEdgesExplicit || hasChanged(GO_X_BIN_EDGES_PROPERTY_NAME_STR)) {
        toManual(GO_X_BIN_LIMITS_MODE_PROPERTY_NAME_STR);
    }
    if (yEdgesExplicit || hasChanged(GO_Y_BIN_EDGES_PROPERTY_NAME_STR)) {
        toManual(GO_Y_BIN_LIMITS_MODE_PROPERTY_NAME_STR);
    }
    std::vector<double> values = normalizedValues(
        counts, xEdges, yEdges, findStringProperty(GO_NORMALIZATION_PROPERTY_NAME_STR));
    setArray(this, GO_VALUES_PROPERTY_NAME_STR, matrixArray(xBins, yBins, values));
    limitsDirty = true;
    clearChanged(
        { GO_DATA_PROPERTY_NAME_STR, GO_BIN_COUNTS_PROPERTY_NAME_STR, GO_NUM_BINS_PROPERTY_NAME_STR,
            GO_X_BIN_EDGES_PROPERTY_NAME_STR, GO_Y_BIN_EDGES_PROPERTY_NAME_STR,
            GO_BIN_WIDTH_PROPERTY_NAME_STR, GO_X_BIN_LIMITS_PROPERTY_NAME_STR,
            GO_Y_BIN_LIMITS_PROPERTY_NAME_STR, GO_BIN_METHOD_PROPERTY_NAME_STR,
            GO_BIN_COUNTS_MODE_PROPERTY_NAME_STR, GO_X_BIN_LIMITS_MODE_PROPERTY_NAME_STR,
            GO_Y_BIN_LIMITS_MODE_PROPERTY_NAME_STR, GO_NORMALIZATION_PROPERTY_NAME_STR });
}
//=============================================================================
void
GOHistogram2::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    std::vector<double> xEdges
        = numericVector(findArrayOfProperty(GO_X_BIN_EDGES_PROPERTY_NAME_STR));
    std::vector<double> yEdges
        = numericVector(findArrayOfProperty(GO_Y_BIN_EDGES_PROPERTY_NAME_STR));
    std::vector<double> values = numericVector(findArrayOfProperty(GO_VALUES_PROPERTY_NAME_STR));
    size_t xBins = xEdges.size() > 1 ? xEdges.size() - 1 : 0;
    size_t yBins = yEdges.size() > 1 ? yEdges.size() - 1 : 0;
    if (xBins == 0 || yBins == 0 || values.size() != xBins * yBins) {
        return;
    }

    std::vector<double> faceColor
        = propertyColor(this, GO_FACE_COLOR_PROPERTY_NAME_STR, { 0.066, 0.443, 0.745 });
    std::vector<double> edgeColor = propertyColor(this, GO_EDGE_COLOR_PROPERTY_NAME_STR,
        { 0.129411764705882, 0.129411764705882, 0.129411764705882 });
    appendAlpha(faceColor, numericScalarProperty(this, GO_FACE_ALPHA_PROPERTY_NAME_STR, 1.));
    appendAlpha(edgeColor, numericScalarProperty(this, GO_EDGE_ALPHA_PROPERTY_NAME_STR, 1.));
    gc.lineWidth(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    gc.setLineStyle(findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR));

    bool showEmptyBins
        = findStringProperty(GO_SHOW_EMPTY_BINS_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_ON_STR;
    bool tile = findStringProperty(GO_DISPLAY_STYLE_PROPERTY_NAME_STR) == L"tile";
    bool lit = findStringProperty(GO_FACE_LIGHTING_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_LIT_STR;
    std::wstring faceColorMode = findStringProperty(GO_FACE_COLOR_PROPERTY_NAME_STR);
    double barPaddingFraction = (xEdgesExplicit || yEdgesExplicit) ? 0. : 0.05;
    if (tile) {
        for (size_t yi = 0; yi < yBins; ++yi) {
            for (size_t xi = 0; xi < xBins; ++xi) {
                double value = values[xi + xBins * yi];
                if (!showEmptyBins && value == 0.) {
                    continue;
                }
                std::vector<double> tileFaceColor = faceColor;
                if (faceColorMode == GO_PROPERTY_VALUE_AUTO_STR
                    || faceColorMode == GO_PROPERTY_VALUE_FLAT_STR) {
                    tileFaceColor = colormapColor(this, value,
                        numericScalarProperty(this, GO_FACE_ALPHA_PROPERTY_NAME_STR, 1.));
                }
                drawTile(gc, xEdges[xi], xEdges[xi + 1], yEdges[yi], yEdges[yi + 1], tileFaceColor,
                    edgeColor);
            }
        }
    } else {
        gc.beginSurfaceGroup();
        for (size_t yi = 0; yi < yBins; ++yi) {
            for (size_t xi = 0; xi < xBins; ++xi) {
                double value = values[xi + xBins * yi];
                if (!showEmptyBins && value == 0.) {
                    continue;
                }
                CuboidTopEdges topEdges;
                if (xi > 0) {
                    topEdges.xLow = !topEdgeCoveredByNeighbor(value, values[xi - 1 + xBins * yi]);
                }
                if (xi + 1 < xBins) {
                    topEdges.xHigh = !topEdgeCoveredByNeighbor(value, values[xi + 1 + xBins * yi]);
                }
                if (yi > 0) {
                    topEdges.yLow = !topEdgeCoveredByNeighbor(value, values[xi + xBins * (yi - 1)]);
                }
                if (yi + 1 < yBins) {
                    topEdges.yHigh
                        = !topEdgeCoveredByNeighbor(value, values[xi + xBins * (yi + 1)]);
                }
                drawHistogramCuboid(gc, xEdges[xi], xEdges[xi + 1], yEdges[yi], yEdges[yi + 1],
                    value, faceColor, edgeColor, lit, barPaddingFraction, topEdges);
            }
        }
        gc.endSurfaceGroup();
    }
}
//=============================================================================
std::vector<double>
GOHistogram2::getLimits()
{
    if (!limitsDirty) {
        return limits;
    }
    std::vector<double> xEdges
        = numericVector(findArrayOfProperty(GO_X_BIN_EDGES_PROPERTY_NAME_STR));
    std::vector<double> yEdges
        = numericVector(findArrayOfProperty(GO_Y_BIN_EDGES_PROPERTY_NAME_STR));
    std::vector<double> values = numericVector(findArrayOfProperty(GO_VALUES_PROPERTY_NAME_STR));
    if (xEdges.empty()) {
        xEdges = { 0., 1. };
    }
    if (yEdges.empty()) {
        yEdges = { 0., 1. };
    }
    double maxValue = 1.;
    if (!values.empty()) {
        maxValue = *std::max_element(values.begin(), values.end());
        if (!std::isfinite(maxValue) || maxValue <= 0.) {
            maxValue = 1.;
        }
    }
    limits = { xEdges.front(), xEdges.back(), yEdges.front(), yEdges.back(), 0., maxValue, 0.,
        maxValue, 1., 1. };
    limitsDirty = false;
    return limits;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
