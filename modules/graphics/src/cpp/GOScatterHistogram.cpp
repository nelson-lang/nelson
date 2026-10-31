//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOScatterHistogram.hpp"
#include "GOChartDecorators.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOVectorProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPositionHelpers.hpp"
#include <algorithm>
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
static GraphicsObject*
graphicsChild(int64 handle)
{
    if (handle < HANDLE_OFFSET_OBJECT) {
        return nullptr;
    }
    return findGraphicsObject(handle, false);
}
//=============================================================================
static std::vector<int64>
childrenOf(GraphicsObject* object)
{
    if (!object) {
        return {};
    }
    GOGObjectsProperty* children = dynamic_cast<GOGObjectsProperty*>(
        object->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return {};
    }
    return children->data();
}
//=============================================================================
static void
setPropertyIfPresent(GraphicsObject* object, const std::wstring& name, const ArrayOf& value)
{
    if (!object) {
        return;
    }
    GOGenericProperty* property = object->findProperty(name, false);
    if (property) {
        property->set(value);
    }
}
//=============================================================================
static ArrayOf
scalarDouble(double value)
{
    return ArrayOf::doubleScalar(value);
}
//=============================================================================
static double
histogramNumBinsValue(const ArrayOf& value, bool yHistogram)
{
    if (!value.isNumeric() || value.elementCount() == 0) {
        return 10.;
    }
    ArrayOf asDouble(value);
    asDouble.promoteType(NLS_DOUBLE);
    std::vector<double> values = asDouble.asDoubleVector();
    if (values.empty()) {
        return 10.;
    }
    size_t index = yHistogram && values.size() > 1 ? 1 : 0;
    return values[index];
}
//=============================================================================
static void
syncAxisLimits(GraphicsObject* axis, const std::wstring& axisProperty, const ArrayOf& value)
{
    setPropertyIfPresent(axis, axisProperty, value);
}
//=============================================================================
static void
syncScatterChild(GraphicsObject* scatter, GOScatterHistogram* chart)
{
    if (!scatter || !scatter->isType(GO_PROPERTY_VALUE_SCATTER_STR)) {
        return;
    }
    ArrayOf color = chart->findProperty(GO_COLOR_PROPERTY_NAME_STR)->get();
    setPropertyIfPresent(scatter, GO_C_DATA_PROPERTY_NAME_STR, color);
    setPropertyIfPresent(scatter, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, color);
    if (chart->stringCheck(GO_MARKER_FILLED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
        setPropertyIfPresent(scatter, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR,
            ArrayOf::characterArray(GO_PROPERTY_VALUE_FLAT_STR));
    } else {
        setPropertyIfPresent(scatter, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR,
            ArrayOf::characterArray(GO_PROPERTY_VALUE_NONE_STR));
    }
    setPropertyIfPresent(scatter, GO_MARKER_PROPERTY_NAME_STR,
        chart->findProperty(GO_MARKER_STYLE_PROPERTY_NAME_STR)->get());
    setPropertyIfPresent(scatter, GO_SIZE_DATA_PROPERTY_NAME_STR,
        chart->findProperty(GO_MARKER_SIZE_PROPERTY_NAME_STR)->get());
    ArrayOf alpha = chart->findProperty(GO_MARKER_ALPHA_PROPERTY_NAME_STR)->get();
    setPropertyIfPresent(scatter, GO_MARKER_EDGE_ALPHA_PROPERTY_NAME_STR, alpha);
    setPropertyIfPresent(scatter, GO_MARKER_FACE_ALPHA_PROPERTY_NAME_STR, alpha);
    scatter->updateState();
}
//=============================================================================
static ArrayOf
markerQuantizedColor(const ArrayOf& color)
{
    // The SVG export rounds marker colours but truncates stroke colours, so a stroke
    // set to the exact series colour is up to one 8-bit level darker per channel.
    // Snap each channel to the centre of its 8-bit cell so stroke and markers use
    // the same byte.
    if (!color.isNumeric() || color.elementCount() < 3) {
        return color;
    }
    ArrayOf asDouble(color);
    asDouble.promoteType(NLS_DOUBLE);
    std::vector<double> values = asDouble.asDoubleVector();
    for (size_t k = 0; k < values.size() && k < 3; ++k) {
        double centred = (std::round(values[k] * 255.0) + 0.5) / 255.0;
        values[k] = std::min(1.0, std::max(0.0, centred));
    }
    double* data = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, values.size()));
    for (size_t k = 0; k < values.size(); ++k) {
        data[k] = values[k];
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(1, values.size()), data);
}
//=============================================================================
static void
syncHistogramChild(GraphicsObject* histogram, GOScatterHistogram* chart, bool yHistogram)
{
    if (!histogram || !histogram->isType(GO_PROPERTY_VALUE_HISTOGRAM_STR)) {
        return;
    }
    ArrayOf numBins = chart->findProperty(GO_NUM_BINS_PROPERTY_NAME_STR)->get();
    setPropertyIfPresent(histogram, GO_NUM_BINS_PROPERTY_NAME_STR,
        scalarDouble(histogramNumBinsValue(numBins, yHistogram)));
    std::wstring style = chart->findStringProperty(GO_HISTOGRAM_DISPLAY_STYLE_PROPERTY_NAME_STR);
    if (style == L"bar" || style == L"stairs") {
        setPropertyIfPresent(
            histogram, GO_DISPLAY_STYLE_PROPERTY_NAME_STR, ArrayOf::characterArray(style));
    }
    setPropertyIfPresent(histogram, GO_EDGE_COLOR_PROPERTY_NAME_STR,
        markerQuantizedColor(chart->findProperty(GO_COLOR_PROPERTY_NAME_STR)->get()));
    setPropertyIfPresent(histogram, GO_LINE_STYLE_PROPERTY_NAME_STR,
        chart->findProperty(GO_LINE_STYLE_PROPERTY_NAME_STR)->get());
    setPropertyIfPresent(histogram, GO_LINE_WIDTH_PROPERTY_NAME_STR,
        chart->findProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR)->get());
    histogram->updateState();
}
//=============================================================================
static ArrayOf
twoElementColumnVector(double first, double second)
{
    double* data = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 2));
    data[0] = first;
    data[1] = second;
    return ArrayOf(NLS_DOUBLE, Dimensions(2, 1), data);
}
//=============================================================================
GOScatterHistogram::GOScatterHistogram() : GOGroup()
{
    registerProperty(new GOSourceTableProperty, GO_SOURCE_TABLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_X_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_Y_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_GROUP_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_X_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_GROUP_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_X_LIMITS_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_Y_LIMITS_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_FONT_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_NUM_BINS_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_BIN_WIDTHS_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_SCATTER_PLOT_LOCATION_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_X_HISTOGRAM_DIRECTION_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_Y_HISTOGRAM_DIRECTION_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_HISTOGRAM_DISPLAY_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_LEGEND_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_MARKER_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_MARKER_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_MARKER_FILLED_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty(0, 1), GO_MARKER_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty(0, 1), GO_SCATTER_PLOT_PROPORTION_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TITLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_X_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_Y_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_LEGEND_TITLE_PROPERTY_NAME_STR);
    registerProperty(new GOUnitsProperty, GO_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_OUTER_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_INNER_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR);
    registerProperty(new GOLayoutOptionsProperty, GO_LAYOUT_PROPERTY_NAME_STR);
    sortProperties();

    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    findProperty(GO_SOURCE_TABLE_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_X_VARIABLE_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_Y_VARIABLE_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_GROUP_VARIABLE_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_X_DATA_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_Y_DATA_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_GROUP_DATA_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 0.066, 0.443, 0.745);
    setTwoVectorDefault(GO_X_LIMITS_PROPERTY_NAME_STR, 0, 1);
    setTwoVectorDefault(GO_Y_LIMITS_PROPERTY_NAME_STR, 0, 1);
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 10);
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, L"");
    findProperty(GO_NUM_BINS_PROPERTY_NAME_STR)->set(twoElementColumnVector(10, 10));
    setTwoVectorDefault(GO_BIN_WIDTHS_PROPERTY_NAME_STR, 1, 1);
    setStringDefault(GO_SCATTER_PLOT_LOCATION_PROPERTY_NAME_STR, L"SouthWest");
    setStringDefault(GO_X_HISTOGRAM_DIRECTION_PROPERTY_NAME_STR, L"up");
    setStringDefault(GO_Y_HISTOGRAM_DIRECTION_PROPERTY_NAME_STR, L"right");
    setStringDefault(GO_HISTOGRAM_DISPLAY_STYLE_PROPERTY_NAME_STR, L"stairs");
    setRestrictedStringDefault(GO_LEGEND_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"none");
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setStringDefault(GO_MARKER_STYLE_PROPERTY_NAME_STR, L"o");
    setScalarDoubleDefault(GO_MARKER_SIZE_PROPERTY_NAME_STR, 36);
    setRestrictedStringDefault(GO_MARKER_FILLED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setScalarDoubleDefault(GO_MARKER_ALPHA_PROPERTY_NAME_STR, 1);
    setScalarDoubleDefault(GO_SCATTER_PLOT_PROPORTION_PROPERTY_NAME_STR, 0.75);
    setStringDefault(GO_TITLE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_X_LABEL_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_Y_LABEL_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_LEGEND_TITLE_PROPERTY_NAME_STR, L"");
    setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMALIZED_STR);
    setFourVectorDefault(GO_OUTER_POSITION_PROPERTY_NAME_STR, 0, 0, 1, 1);
    setFourVectorDefault(GO_INNER_POSITION_PROPERTY_NAME_STR, 0.13, 0.11, 0.775, 0.815);
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0.13, 0.11, 0.775, 0.815);
    setStringDefault(GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OUTERPOSITION_STR);
    clearAllChanged();
}
//=============================================================================
std::wstring
GOScatterHistogram::getType()
{
    return GO_PROPERTY_VALUE_SCATTER_HISTOGRAM_STR;
}
//=============================================================================
void
GOScatterHistogram::updateState()
{
    syncChartPositionUnits(this);
    GOGroup::updateState();
    bool scatterPropertiesChanged = hasChanged(GO_COLOR_PROPERTY_NAME_STR)
        || hasChanged(GO_MARKER_STYLE_PROPERTY_NAME_STR)
        || hasChanged(GO_MARKER_SIZE_PROPERTY_NAME_STR)
        || hasChanged(GO_MARKER_FILLED_PROPERTY_NAME_STR)
        || hasChanged(GO_MARKER_ALPHA_PROPERTY_NAME_STR);
    bool histogramPropertiesChanged = hasChanged(GO_COLOR_PROPERTY_NAME_STR)
        || hasChanged(GO_NUM_BINS_PROPERTY_NAME_STR)
        || hasChanged(GO_HISTOGRAM_DISPLAY_STYLE_PROPERTY_NAME_STR)
        || hasChanged(GO_LINE_STYLE_PROPERTY_NAME_STR)
        || hasChanged(GO_LINE_WIDTH_PROPERTY_NAME_STR);
    std::vector<int64> chartChildren = childrenOf(this);
    if (chartChildren.size() < 3) {
        return;
    }
    GraphicsObject* scatterAxis = graphicsChild(chartChildren[0]);
    GraphicsObject* xHistogramAxis = graphicsChild(chartChildren[1]);
    GraphicsObject* yHistogramAxis = graphicsChild(chartChildren[2]);
    if (hasChanged(GO_X_LIMITS_PROPERTY_NAME_STR)) {
        ArrayOf xLimits = findProperty(GO_X_LIMITS_PROPERTY_NAME_STR)->get();
        syncAxisLimits(scatterAxis, GO_X_LIM_PROPERTY_NAME_STR, xLimits);
        syncAxisLimits(xHistogramAxis, GO_X_LIM_PROPERTY_NAME_STR, xLimits);
    }
    if (hasChanged(GO_Y_LIMITS_PROPERTY_NAME_STR)) {
        ArrayOf yLimits = findProperty(GO_Y_LIMITS_PROPERTY_NAME_STR)->get();
        syncAxisLimits(scatterAxis, GO_Y_LIM_PROPERTY_NAME_STR, yLimits);
        syncAxisLimits(yHistogramAxis, GO_Y_LIM_PROPERTY_NAME_STR, yLimits);
    }
    if (scatterPropertiesChanged) {
        for (int64 childHandle : childrenOf(scatterAxis)) {
            syncScatterChild(graphicsChild(childHandle), this);
        }
    }
    if (histogramPropertiesChanged) {
        for (int64 childHandle : childrenOf(xHistogramAxis)) {
            syncHistogramChild(graphicsChild(childHandle), this, false);
        }
        for (int64 childHandle : childrenOf(yHistogramAxis)) {
            syncHistogramChild(graphicsChild(childHandle), this, true);
        }
    }
    clearChanged({ GO_COLOR_PROPERTY_NAME_STR, GO_MARKER_STYLE_PROPERTY_NAME_STR,
        GO_MARKER_SIZE_PROPERTY_NAME_STR, GO_MARKER_FILLED_PROPERTY_NAME_STR,
        GO_MARKER_ALPHA_PROPERTY_NAME_STR, GO_NUM_BINS_PROPERTY_NAME_STR,
        GO_HISTOGRAM_DISPLAY_STYLE_PROPERTY_NAME_STR, GO_LINE_STYLE_PROPERTY_NAME_STR,
        GO_LINE_WIDTH_PROPERTY_NAME_STR, GO_X_LIMITS_PROPERTY_NAME_STR,
        GO_Y_LIMITS_PROPERTY_NAME_STR });
}
//=============================================================================
} // namespace Nelson
//=============================================================================
