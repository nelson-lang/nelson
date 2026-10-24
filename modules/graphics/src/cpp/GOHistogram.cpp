//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOHistogram.hpp"
#include "GOHistogramBinning.hpp"
#include "GOChartDecorators.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOAxis.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOEdgeAlphaProperty.hpp"
#include "GOFaceAlphaProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOPickablePartsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOScalarPositiveIntegerValueProperty.hpp"
#include "GOSeriesIndexProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOStringVectorProperty.hpp"
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
        = { L"auto", L"manual", L"scott", L"fd", L"integers", L"sturges", L"sqrt" };
    static const std::vector<std::wstring> NORMALIZATION_VALUES
        = { L"count", L"probability", L"countdensity", L"pdf", L"cdf", L"cumcount" };
    static const std::vector<std::wstring> ORIENTATION_VALUES = { L"vertical", L"horizontal" };
    static const std::vector<std::wstring> DISPLAY_STYLE_VALUES = { L"bar", L"stairs" };
    //=============================================================================
    static void
    addHistogramProperty(GraphicsObject* go, GOGenericProperty* property, const std::wstring& name,
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
    finiteData(const ArrayOf& value)
    {
        std::vector<double> values = numericVector(value);
        std::vector<double> finiteValues;
        finiteValues.reserve(values.size());
        for (double value : values) {
            if (std::isfinite(value)) {
                finiteValues.push_back(value);
            }
        }
        return finiteValues;
    }
    //=============================================================================
    static std::vector<double>
    computedEdges(GraphicsObject* object, const std::vector<double>& data, bool& binEdgesExplicit)
    {
        if (object->isAuto(GO_BIN_LIMITS_MODE_PROPERTY_NAME_STR)) {
            std::vector<double> limits = HistogramBinning::defaultBinLimits(data);
            setArray(object, GO_BIN_LIMITS_PROPERTY_NAME_STR, rowVector(limits));
        }
        std::vector<double> limits = HistogramBinning::validateBinLimits(
            numericVector(object->findArrayOfProperty(GO_BIN_LIMITS_PROPERTY_NAME_STR)));

        std::vector<double> currentEdges;
        if (!object->findArrayOfProperty(GO_BIN_EDGES_PROPERTY_NAME_STR).isEmpty()) {
            currentEdges = HistogramBinning::validateEdges(
                numericVector(object->findArrayOfProperty(GO_BIN_EDGES_PROPERTY_NAME_STR)));
        }
        if (object->hasChanged(GO_BIN_EDGES_PROPERTY_NAME_STR) && !currentEdges.empty()) {
            binEdgesExplicit = true;
            return currentEdges;
        }
        if (binEdgesExplicit && !currentEdges.empty()) {
            bool keepExplicitEdges = true;
            if (object->hasChanged(GO_BIN_WIDTH_PROPERTY_NAME_STR)) {
                double width = object->findScalarDoubleProperty(GO_BIN_WIDTH_PROPERTY_NAME_STR);
                double currentWidth
                    = currentEdges.size() > 1 ? currentEdges[1] - currentEdges[0] : width;
                keepExplicitEdges
                    = std::fabs(width - currentWidth) <= std::numeric_limits<double>::epsilon();
            }
            if (object->hasChanged(GO_NUM_BINS_PROPERTY_NAME_STR)) {
                auto* numBinsProperty = static_cast<GOScalarPositiveIntegerValueProperty*>(
                    object->findProperty(GO_NUM_BINS_PROPERTY_NAME_STR));
                keepExplicitEdges = keepExplicitEdges
                    && (numBinsProperty->data() == static_cast<double>(currentEdges.size() - 1));
            }
            if (object->hasChanged(GO_BIN_LIMITS_PROPERTY_NAME_STR)) {
                keepExplicitEdges = keepExplicitEdges && limits.size() == 2
                    && std::fabs(limits[0] - currentEdges.front())
                        <= std::numeric_limits<double>::epsilon()
                    && std::fabs(limits[1] - currentEdges.back())
                        <= std::numeric_limits<double>::epsilon();
            }
            if (keepExplicitEdges) {
                return currentEdges;
            }
            binEdgesExplicit = false;
        }
        if (object->hasChanged(GO_BIN_WIDTH_PROPERTY_NAME_STR)) {
            double width = object->findScalarDoubleProperty(GO_BIN_WIDTH_PROPERTY_NAME_STR);
            return HistogramBinning::edgesFromWidth(limits[0], limits[1], width);
        }
        // The automatic rules operate on the raw data range in auto BinLimitsMode
        // (limits carries the widened value only for constant data); with hard bin
        // limits they operate on those limits.
        const bool autoLimits = object->isAuto(GO_BIN_LIMITS_MODE_PROPERTY_NAME_STR);
        double xmin = limits[0];
        double xmax = limits[1];
        if (autoLimits && !data.empty()) {
            auto [mnIt, mxIt] = std::minmax_element(data.begin(), data.end());
            xmin = *mnIt;
            xmax = *mxIt;
        }
        const double xrange = xmax - xmin;
        const bool numBinsRequested = object->hasChanged(GO_NUM_BINS_PROPERTY_NAME_STR);
        std::wstring binMethod = object->findStringProperty(GO_BIN_METHOD_PROPERTY_NAME_STR);
        // Integer rule: always for 'integers'; for 'auto' only when the data are integers
        // spanning at most 50 values. Beyond that 'auto' uses Scott's rule, so
        // histogram(1:100) gives a few wide bins.
        const bool integerAuto = binMethod == L"auto" && !numBinsRequested
            && HistogramBinning::allIntegers(data) && xrange <= 50.;
        if (binMethod == L"integers" || integerAuto) {
            std::vector<double> edges = HistogramBinning::integerRuleEdges(xmin, xmax);
            if (!edges.empty()) {
                return edges;
            }
        }
        // Scott's rule with a 'nice' bin width, only when the bin count was not requested.
        if ((binMethod == L"auto" || binMethod == L"scott") && !numBinsRequested) {
            if (HistogramBinning::isNearlyConstant(xmin, xmax)) {
                return HistogramBinning::nearlyConstantEdges(xmin, xmax);
            }
            double niceWidth = HistogramBinning::scottNiceBinWidth(data, xmin, xmax);
            if (niceWidth > 0.) {
                return HistogramBinning::edgesFromWidth(xmin, xmax, niceWidth);
            }
        }
        auto* numBinsProperty = static_cast<GOScalarPositiveIntegerValueProperty*>(
            object->findProperty(GO_NUM_BINS_PROPERTY_NAME_STR));
        double nBinsValue = numBinsProperty->data();
        size_t nBins = static_cast<size_t>(std::max(1., std::floor(nBinsValue)));
        if (autoLimits) {
            // Edges on a 'nice' grid that may extend past the data range, with exactly nBins bins.
            return HistogramBinning::niceNumBinsEdges(xmin, xmax, nBins);
        }
        return HistogramBinning::uniformEdges(limits[0], limits[1], nBins);
    }
    //=============================================================================
    static std::vector<double>
    countsForEdges(const std::vector<double>& data, const std::vector<double>& edges)
    {
        std::vector<double> counts(edges.size() > 1 ? edges.size() - 1 : 0, 0.);
        if (counts.empty()) {
            return counts;
        }
        for (double value : data) {
            if (value < edges.front() || value > edges.back()) {
                continue;
            }
            if (value == edges.back()) {
                counts.back() += 1.;
                continue;
            }
            auto it = std::upper_bound(edges.begin(), edges.end(), value);
            size_t index = static_cast<size_t>(std::distance(edges.begin(), it));
            if (index > 0 && index <= counts.size()) {
                counts[index - 1] += 1.;
            }
        }
        return counts;
    }
    //=============================================================================
    static std::vector<double>
    binWidths(const std::vector<double>& edges)
    {
        std::vector<double> widths;
        widths.reserve(edges.size() > 1 ? edges.size() - 1 : 0);
        for (size_t k = 0; k + 1 < edges.size(); ++k) {
            widths.push_back(edges[k + 1] - edges[k]);
        }
        return widths;
    }
    //=============================================================================
    static std::vector<double>
    normalizedValues(const std::vector<double>& counts, const std::vector<double>& edges,
        const std::wstring& normalization)
    {
        std::vector<double> values(counts);
        double total = 0.;
        for (double count : counts) {
            total += count;
        }
        std::vector<double> widths = binWidths(edges);
        if (normalization == L"probability" || normalization == L"pdf" || normalization == L"cdf") {
            double denom = total > 0. ? total : 1.;
            for (double& value : values) {
                value /= denom;
            }
        }
        if (normalization == L"countdensity" || normalization == L"pdf") {
            for (size_t k = 0; k < values.size(); ++k) {
                double width = (k < widths.size() && widths[k] > 0.) ? widths[k] : 1.;
                values[k] /= width;
            }
        }
        if (normalization == L"cdf" || normalization == L"cumcount") {
            double cumulative = 0.;
            for (double& value : values) {
                cumulative += value;
                value = cumulative;
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
    static void
    drawVerticalBar(RenderInterface& gc, double left, double right, double value,
        const std::vector<double>& faceColor, const std::vector<double>& edgeColor)
    {
        double bottom = std::min(0., value);
        double top = std::max(0., value);
        if (isDrawableColor(faceColor)) {
            gc.color(faceColor);
            gc.quad(left, bottom, 0., right, bottom, 0., right, top, 0., left, top, 0.);
        }
        if (isDrawableColor(edgeColor)) {
            gc.color(edgeColor);
            gc.quadline(left, bottom, 0., right, bottom, 0., right, top, 0., left, top, 0.);
        }
    }
    //=============================================================================
    static void
    drawHorizontalBar(RenderInterface& gc, double bottom, double top, double value,
        const std::vector<double>& faceColor, const std::vector<double>& edgeColor)
    {
        double left = std::min(0., value);
        double right = std::max(0., value);
        if (isDrawableColor(faceColor)) {
            gc.color(faceColor);
            gc.quad(left, bottom, 0., right, bottom, 0., right, top, 0., left, top, 0.);
        }
        if (isDrawableColor(edgeColor)) {
            gc.color(edgeColor);
            gc.quadline(left, bottom, 0., right, bottom, 0., right, top, 0., left, top, 0.);
        }
    }
    //=============================================================================
    // Single connected staircase outline: horizontal tops alternate with vertical risers.
    // Only the two outer ends touch the baseline. For the horizontal orientation x and y
    // are swapped.
    static void
    buildStairsOutline(const std::vector<double>& edges, const std::vector<double>& values,
        bool horizontal, std::vector<double>& xs, std::vector<double>& ys, std::vector<double>& zs)
    {
        size_t n = values.size();
        std::vector<double> primary;
        std::vector<double> secondary;
        primary.reserve(2 * n + 2);
        secondary.reserve(2 * n + 2);
        // primary follows the bin edges, secondary follows the bin heights.
        primary.push_back(edges[0]);
        secondary.push_back(0.);
        for (size_t k = 0; k < n; ++k) {
            primary.push_back(edges[k]);
            secondary.push_back(values[k]);
            primary.push_back(edges[k + 1]);
            secondary.push_back(values[k]);
        }
        primary.push_back(edges[n]);
        secondary.push_back(0.);
        size_t count = primary.size();
        xs.assign(count, 0.);
        ys.assign(count, 0.);
        zs.assign(count, 0.);
        for (size_t i = 0; i < count; ++i) {
            if (horizontal) {
                xs[i] = secondary[i];
                ys[i] = primary[i];
            } else {
                xs[i] = primary[i];
                ys[i] = secondary[i];
            }
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
GOHistogram::GOHistogram()
{
    addHistogramProperty(this, new GOArrayOfProperty, GO_DATA_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOArrayOfProperty, GO_BIN_COUNTS_PROPERTY_NAME_STR);
    addHistogramProperty(
        this, new GOScalarPositiveIntegerValueProperty, GO_NUM_BINS_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOArrayOfProperty, GO_BIN_EDGES_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOScalarProperty, GO_BIN_WIDTH_PROPERTY_NAME_STR);
    addHistogramProperty(
        this, new GORestrictedStringProperty(BIN_METHOD_VALUES), GO_BIN_METHOD_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOArrayOfProperty, GO_BIN_LIMITS_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GORestrictedStringProperty(NORMALIZATION_VALUES),
        GO_NORMALIZATION_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOAutoFlatColorProperty, GO_FACE_COLOR_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOAutoFlatColorProperty, GO_EDGE_COLOR_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOAutoManualProperty, GO_BIN_COUNTS_MODE_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOAutoManualProperty, GO_BIN_LIMITS_MODE_PROPERTY_NAME_STR);
    addHistogramProperty(
        this, new GORestrictedStringProperty(ORIENTATION_VALUES), GO_ORIENTATION_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GORestrictedStringProperty(DISPLAY_STYLE_VALUES),
        GO_DISPLAY_STYLE_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOFaceAlphaProperty, GO_FACE_ALPHA_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOEdgeAlphaProperty, GO_EDGE_ALPHA_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOArrayOfProperty, GO_VALUES_PROPERTY_NAME_STR, false);
    addHistogramProperty(
        this, new GOStringVectorProperty(true, false, true), GO_CATEGORIES_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOScalarProperty, GO_BAR_WIDTH_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR, false);
    addHistogramProperty(this, new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    addHistogramProperty(
        this, new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOLazyDecoratorProperty(GODecoratorKind::AnnotationEventData),
        GO_ANNOTATION_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOLazyDecoratorProperty(GODecoratorKind::DataTipTemplate),
        GO_DATA_TIP_TEMPLATE_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOOnOffProperty, GO_SELECTED_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOOnOffProperty, GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOOnOffProperty, GO_HIT_TEST_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOPickablePartsProperty, GO_PICKABLE_PARTS_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOCallbackProperty, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOGObjectsProperty, GO_CONTEXT_MENU_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    addHistogramProperty(this, new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    addHistogramProperty(this, new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    addHistogramProperty(this, new GOSeriesIndexProperty, GO_SERIES_INDEX_PROPERTY_NAME_STR);

    setScalarPositiveIntegerValueDefault(GO_NUM_BINS_PROPERTY_NAME_STR, 10);
    setScalarDoubleDefault(GO_BIN_WIDTH_PROPERTY_NAME_STR, 1.);
    setArray(this, GO_BIN_LIMITS_PROPERTY_NAME_STR, rowVector({ 0., 1. }));
    setRestrictedStringDefault(GO_BIN_METHOD_PROPERTY_NAME_STR, L"auto");
    setRestrictedStringDefault(GO_NORMALIZATION_PROPERTY_NAME_STR, L"count");
    setRestrictedStringDefault(GO_BIN_COUNTS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_BIN_LIMITS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_ORIENTATION_PROPERTY_NAME_STR, L"vertical");
    setRestrictedStringDefault(GO_DISPLAY_STYLE_PROPERTY_NAME_STR, L"bar");
    setRestrictedStringColorDefault(
        GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR, 0., 0.447, 0.741);
    setRestrictedStringColorDefault(GO_EDGE_COLOR_PROPERTY_NAME_STR,
        GO_PROPERTY_VALUE_COLORSPEC_STR, 0.129411764705882, 0.129411764705882, 0.129411764705882);
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setScalarDoubleDefault(GO_BAR_WIDTH_PROPERTY_NAME_STR, 0.9);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setRestrictedStringScalarDefault(
        GO_FACE_ALPHA_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALAR_STR, 0.6);
    setRestrictedStringScalarDefault(
        GO_EDGE_ALPHA_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALAR_STR, 1.);
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
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_HISTOGRAM_STR);
    setStringDefault(GO_TAG_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    auto* seriesIndex
        = static_cast<GOSeriesIndexProperty*>(findProperty(GO_SERIES_INDEX_PROPERTY_NAME_STR));
    seriesIndex->setValue(1.);
    seriesIndex->clearModified();
    clearAllChanged();
    sortProperties();
}
//=============================================================================
GOHistogram::~GOHistogram() = default;
//=============================================================================
std::wstring
GOHistogram::getType()
{
    return GO_PROPERTY_VALUE_HISTOGRAM_STR;
}
//=============================================================================
bool
GOHistogram::preservesAutoDataLimits()
{
    return true;
}
//=============================================================================
void
GOHistogram::updateState()
{
    if (hasChanged(GO_BIN_COUNTS_PROPERTY_NAME_STR)) {
        toManual(GO_BIN_COUNTS_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_BIN_LIMITS_PROPERTY_NAME_STR)) {
        toManual(GO_BIN_LIMITS_MODE_PROPERTY_NAME_STR);
    }

    bool needsUpdate = hasChanged(GO_DATA_PROPERTY_NAME_STR)
        || hasChanged(GO_BIN_COUNTS_PROPERTY_NAME_STR) || hasChanged(GO_NUM_BINS_PROPERTY_NAME_STR)
        || hasChanged(GO_BIN_EDGES_PROPERTY_NAME_STR) || hasChanged(GO_BIN_WIDTH_PROPERTY_NAME_STR)
        || hasChanged(GO_BIN_LIMITS_PROPERTY_NAME_STR)
        || hasChanged(GO_BIN_METHOD_PROPERTY_NAME_STR)
        || hasChanged(GO_BIN_COUNTS_MODE_PROPERTY_NAME_STR)
        || hasChanged(GO_BIN_LIMITS_MODE_PROPERTY_NAME_STR)
        || hasChanged(GO_NORMALIZATION_PROPERTY_NAME_STR);
    if (!needsUpdate) {
        return;
    }

    std::vector<double> edges;
    std::vector<double> counts;
    if (!isAuto(GO_BIN_COUNTS_MODE_PROPERTY_NAME_STR)
        && !findArrayOfProperty(GO_BIN_COUNTS_PROPERTY_NAME_STR).isEmpty()) {
        counts = numericVector(findArrayOfProperty(GO_BIN_COUNTS_PROPERTY_NAME_STR));
        edges = HistogramBinning::validateEdges(
            numericVector(findArrayOfProperty(GO_BIN_EDGES_PROPERTY_NAME_STR)));
        if (edges.size() != counts.size() + 1) {
            Error("Nelson:histogram:binEdgesCountMismatch",
                "BinEdges must have one more element than BinCounts.");
        }
    } else {
        std::vector<double> data = finiteData(findArrayOfProperty(GO_DATA_PROPERTY_NAME_STR));
        edges = computedEdges(this, data, binEdgesExplicit);
        counts = countsForEdges(data, edges);
        setArray(this, GO_BIN_COUNTS_PROPERTY_NAME_STR, rowVector(counts));
        setArray(this, GO_BIN_EDGES_PROPERTY_NAME_STR, rowVector(edges));
    }
    setScalarPositiveIntegerValueDefault(
        GO_NUM_BINS_PROPERTY_NAME_STR, static_cast<double>(counts.size()));
    std::vector<double> widths = binWidths(edges);
    if (!widths.empty()) {
        setScalarDoubleDefault(GO_BIN_WIDTH_PROPERTY_NAME_STR, widths[0]);
    }
    std::vector<double> values
        = normalizedValues(counts, edges, findStringProperty(GO_NORMALIZATION_PROPERTY_NAME_STR));
    setArray(this, GO_VALUES_PROPERTY_NAME_STR, rowVector(values));
    if (!edges.empty()) {
        setArray(this, GO_BIN_LIMITS_PROPERTY_NAME_STR, rowVector({ edges.front(), edges.back() }));
        if (binEdgesExplicit || hasChanged(GO_BIN_EDGES_PROPERTY_NAME_STR)) {
            setRestrictedStringDefault(GO_BIN_METHOD_PROPERTY_NAME_STR, L"manual");
            toManual(GO_BIN_LIMITS_MODE_PROPERTY_NAME_STR);
        }
    }
    limitsDirty = true;
    clearChanged({ GO_DATA_PROPERTY_NAME_STR, GO_BIN_COUNTS_PROPERTY_NAME_STR,
        GO_NUM_BINS_PROPERTY_NAME_STR, GO_BIN_EDGES_PROPERTY_NAME_STR,
        GO_BIN_WIDTH_PROPERTY_NAME_STR, GO_BIN_LIMITS_PROPERTY_NAME_STR,
        GO_BIN_METHOD_PROPERTY_NAME_STR, GO_BIN_COUNTS_MODE_PROPERTY_NAME_STR,
        GO_BIN_LIMITS_MODE_PROPERTY_NAME_STR, GO_NORMALIZATION_PROPERTY_NAME_STR });
}
//=============================================================================
void
GOHistogram::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    GOAxis* axis = getParentAxis();
    if (axis && axis->stringCheck(GO_POLAR_AXES_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
        return;
    }
    std::vector<double> edges = numericVector(findArrayOfProperty(GO_BIN_EDGES_PROPERTY_NAME_STR));
    std::vector<double> values = numericVector(findArrayOfProperty(GO_VALUES_PROPERTY_NAME_STR));
    if (edges.size() != values.size() + 1 || values.empty()) {
        return;
    }
    std::vector<double> faceColor
        = propertyColor(this, GO_FACE_COLOR_PROPERTY_NAME_STR, { 0., 0.447, 0.741 });
    std::vector<double> edgeColor
        = propertyColor(this, GO_EDGE_COLOR_PROPERTY_NAME_STR, { 0., 0., 0. });
    appendAlpha(faceColor, numericScalarProperty(this, GO_FACE_ALPHA_PROPERTY_NAME_STR, 0.6));
    appendAlpha(edgeColor, numericScalarProperty(this, GO_EDGE_ALPHA_PROPERTY_NAME_STR, 1.));
    double lineWidth = findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR);
    gc.lineWidth(lineWidth);
    gc.setLineStyle(findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR));

    bool horizontal = findStringProperty(GO_ORIENTATION_PROPERTY_NAME_STR) == L"horizontal";
    bool stairs = findStringProperty(GO_DISPLAY_STYLE_PROPERTY_NAME_STR) == L"stairs";

    // Draw all bars as one batched "rects" primitive (RenderWeb). A backend that cannot
    // batch (Qt/OpenGL) returns false and the per-bar loop runs. e0/e1 are the position
    // extents, the value gives the other.
    const bool categorical = !findStringVectorProperty(GO_CATEGORIES_PROPERTY_NAME_STR).empty();
    if (categorical || !stairs) {
        std::vector<double> e0s;
        std::vector<double> e1s;
        std::vector<double> vals;
        e0s.reserve(values.size());
        e1s.reserve(values.size());
        vals.reserve(values.size());
        if (categorical) {
            double barWidth = findScalarDoubleProperty(GO_BAR_WIDTH_PROPERTY_NAME_STR);
            if (!std::isfinite(barWidth) || barWidth <= 0.) {
                barWidth = 0.9;
            }
            double half = barWidth / 2.;
            for (size_t k = 0; k < values.size(); ++k) {
                double center = static_cast<double>(k) + 1.;
                e0s.push_back(center - half);
                e1s.push_back(center + half);
                vals.push_back(values[k]);
            }
        } else {
            for (size_t k = 0; k < values.size(); ++k) {
                e0s.push_back(edges[k]);
                e1s.push_back(edges[k + 1]);
                vals.push_back(values[k]);
            }
        }
        std::vector<double> corners;
        corners.reserve(vals.size() * 4);
        for (size_t k = 0; k < vals.size(); ++k) {
            double lo = std::min(0., vals[k]);
            double hi = std::max(0., vals[k]);
            if (horizontal) {
                corners.push_back(lo);
                corners.push_back(e0s[k]);
                corners.push_back(hi);
                corners.push_back(e1s[k]);
            } else {
                corners.push_back(e0s[k]);
                corners.push_back(lo);
                corners.push_back(e1s[k]);
                corners.push_back(hi);
            }
        }
        if (!gc.rectBatch(corners, faceColor, edgeColor, lineWidth)) {
            for (size_t k = 0; k < vals.size(); ++k) {
                if (horizontal) {
                    drawHorizontalBar(gc, e0s[k], e1s[k], vals[k], faceColor, edgeColor);
                } else {
                    drawVerticalBar(gc, e0s[k], e1s[k], vals[k], faceColor, edgeColor);
                }
            }
        }
        return;
    }

    if (!isDrawableColor(edgeColor)) {
        return;
    }
    gc.color(edgeColor);
    std::vector<double> stairX;
    std::vector<double> stairY;
    std::vector<double> stairZ;
    buildStairsOutline(edges, values, horizontal, stairX, stairY, stairZ);
    gc.lineSeries(stairX, stairY, stairZ);
}
//=============================================================================
std::vector<double>
GOHistogram::getLimits()
{
    // Values are filled by updateState(), which otherwise runs at paint time. Without this,
    // a limits query before the first paint (drawnow() right after creation) sees empty
    // Values and YLim falls back to [0 1].
    updateState();
    if (!limitsDirty) {
        return limits;
    }
    std::vector<double> edges = numericVector(findArrayOfProperty(GO_BIN_EDGES_PROPERTY_NAME_STR));
    std::vector<double> values = numericVector(findArrayOfProperty(GO_VALUES_PROPERTY_NAME_STR));
    if (edges.empty()) {
        edges = { 0., 1. };
    }
    // Categorical histogram: tight [0.5, N+0.5] position range, no edge padding.
    bool categorical = !findStringVectorProperty(GO_CATEGORIES_PROPERTY_NAME_STR).empty();
    double low = edges.front();
    double high = edges.back();
    double edgeRange = high - low;
    double edgePadding
        = (!categorical && std::isfinite(edgeRange) && edgeRange > 0.) ? 0.05 * edgeRange : 0.;
    double maxValue = 1.;
    if (!values.empty()) {
        maxValue = *std::max_element(values.begin(), values.end());
        if (!std::isfinite(maxValue) || maxValue <= 0.) {
            maxValue = 1.;
        }
    }
    bool horizontal = findStringProperty(GO_ORIENTATION_PROPERTY_NAME_STR) == L"horizontal";
    if (horizontal) {
        limits = { 0., maxValue, low - edgePadding, high + edgePadding, 0., 1., 0., 1., 1., 1. };
    } else {
        limits = { low - edgePadding, high + edgePadding, 0., maxValue, 0., 1., 0., 1., 1., 1. };
    }
    limitsDirty = false;
    return limits;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
