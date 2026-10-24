//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _USE_MATH_DEFINES
#include "GOPieChart.hpp"
#include "GOChartDecorators.hpp"
#include "GOFontSpecHelpers.hpp"
#include "Error.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOColorVectorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GraphicsObject.hpp"
#include "RenderInterface.hpp"
#include "TexToUnicode.hpp"
#include "GOPositionHelpers.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <sstream>
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* LABEL_STYLE_DICT[4]
    = { GO_PROPERTY_VALUE_PERCENT_STR, GO_PROPERTY_VALUE_DATA_STR, GO_PROPERTY_VALUE_NONE_STR, 0 };
static const wchar_t* DIRECTION_DICT[3]
    = { GO_PROPERTY_VALUE_CLOCKWISE_STR, GO_PROPERTY_VALUE_COUNTERCLOCKWISE_STR, 0 };
static const wchar_t* DISPLAY_ORDER_DICT[4]
    = { GO_PROPERTY_VALUE_DATA_STR, L"ascend", L"descend", 0 };
static const wchar_t* POSITION_CONSTRAINT_DICT[3]
    = { GO_PROPERTY_VALUE_OUTERPOSITION_STR, GO_PROPERTY_VALUE_INNERPOSITION_STR, 0 };
//=============================================================================
static std::wstring
formatDouble(double value)
{
    std::wostringstream stream;
    if (std::fabs(value - std::round(value)) < 1e-10) {
        stream << static_cast<long long>(std::llround(value));
    } else {
        stream.precision(4);
        stream << std::noshowpoint << value;
    }
    return stream.str();
}
//=============================================================================
static std::wstring
formatPercent(double value)
{
    double percent = value * 100.0;
    std::wostringstream stream;
    if (std::fabs(percent - std::round(percent)) < 1e-10) {
        stream << static_cast<long long>(std::llround(percent));
    } else {
        stream.setf(std::ios::fixed);
        stream.precision(1);
        stream << percent;
    }
    stream << L"%";
    return stream.str();
}
//=============================================================================
static std::vector<double>
numericVector(ArrayOf value)
{
    std::vector<double> result;
    if (!value.isNumeric() && !value.isLogical()) {
        return result;
    }
    value.promoteType(NLS_DOUBLE);
    const double* data = static_cast<const double*>(value.dataPointer());
    indexType count = value.elementCount();
    result.reserve(count);
    for (indexType k = 0; k < count; ++k) {
        result.push_back(data[k]);
    }
    return result;
}
//=============================================================================
static wstringVector
stringVectorFromArray(const ArrayOf& value)
{
    if (value.isEmpty()) {
        return {};
    }
    if (value.isRowVectorCharacterArray() || value.isStringArray()
        || value.isCellArrayOfCharacterVectors()) {
        return value.asWideStringVector(false);
    }
    return {};
}
//=============================================================================
static ArrayOf
stringRowArray(const wstringVector& values)
{
    Dimensions dims(1, static_cast<indexType>(values.size()));
    return ArrayOf::stringArray(values, dims);
}
//=============================================================================
static std::vector<double>
withAlpha(std::vector<double> color, double alpha)
{
    if (color.size() < 3) {
        return { -1, -1, -1, 0 };
    }
    color.push_back(alpha);
    return color;
}
//=============================================================================
static std::vector<double>
colorFromOrder(const std::vector<double>& colorOrder, size_t index)
{
    if (colorOrder.size() < 3) {
        return { 0.0660, 0.4430, 0.7450 };
    }
    size_t rows = colorOrder.size() / 3;
    size_t row = index % rows;
    size_t base = 3 * row;
    return { colorOrder[base], colorOrder[base + 1], colorOrder[base + 2] };
}
//=============================================================================
static std::vector<double>
positionInPixels(GraphicsObject* object, double figureWidth, double figureHeight)
{
    std::vector<double> position = object->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (position.size() < 4) {
        return { 0, 0, figureWidth, figureHeight };
    }
    std::wstring units = object->findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
    if (units == GO_PROPERTY_VALUE_NORMALIZED_STR) {
        position[0] *= figureWidth;
        position[1] *= figureHeight;
        position[2] *= figureWidth;
        position[3] *= figureHeight;
    }
    return position;
}
//=============================================================================
static std::vector<double>
pieInnerFromOuter(const std::vector<double>& outer)
{
    if (outer.size() != 4) {
        return {};
    }
    return { outer[0] + outer[2] * 0.13, outer[1] + outer[3] * 0.11, outer[2] * 0.775,
        outer[3] * 0.815 };
}
//=============================================================================
static std::vector<double>
pieOuterFromInner(const std::vector<double>& inner)
{
    if (inner.size() != 4) {
        return {};
    }
    double width = inner[2] / 0.775;
    double height = inner[3] / 0.815;
    return { inner[0] - width * 0.13, inner[1] - height * 0.11, width, height };
}
//=============================================================================
GOPieChart::GOPieChart()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOPieChart::getType()
{
    return GO_PROPERTY_VALUE_PIE_CHART_STR;
}
//=============================================================================
void
GOPieChart::constructProperties()
{
    registerProperty(new GOArrayOfProperty, GO_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_NAMES_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_DATA_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_NAMES_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_LABELS_PROPERTY_NAME_STR);
    registerProperty(
        new GORestrictedStringProperty(LABEL_STYLE_DICT), GO_LABEL_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_START_ANGLE_PROPERTY_NAME_STR);
    registerProperty(
        new GORestrictedStringProperty(DIRECTION_DICT), GO_DIRECTION_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_EXPLODED_WEDGES_PROPERTY_NAME_STR);
    registerProperty(new GOColorVectorProperty, GO_COLOR_ORDER_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_FACE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty(0, 1), GO_FACE_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_FONT_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_FONT_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_INTERPRETER_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_LEGEND_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_LEGEND_TITLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TITLE_PROPERTY_NAME_STR);
    registerProperty(
        new GORestrictedStringProperty(DISPLAY_ORDER_DICT), GO_DISPLAY_ORDER_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_NUM_DISPLAY_WEDGES_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_SHOW_OTHERS_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_PROPORTIONS_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_CATEGORY_COUNTS_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_WEDGE_DISPLAY_DATA_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_WEDGE_DISPLAY_NAMES_PROPERTY_NAME_STR, false);
    registerProperty(new GOAutoManualProperty, GO_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_FONT_SIZE_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_LABELS_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_LABEL_STYLE_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_NAMES_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR, false, false);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    registerProperty(new GOUnitsProperty, GO_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_OUTER_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_INNER_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty(POSITION_CONSTRAINT_DICT),
        GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR);
    registerProperty(new GOLayoutOptionsProperty, GO_LAYOUT_PROPERTY_NAME_STR);
    registerProperty(new GOSourceTableProperty, GO_SOURCE_TABLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerExtraProperties();
    sortProperties();
}
//=============================================================================
void
GOPieChart::setupDefaults()
{
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PIE_CHART_STR);
    findProperty(GO_DATA_PROPERTY_NAME_STR)->set(ArrayOf::doubleRowVector({ 1, 2, 3 }));
    setStringDefault(GO_DATA_VARIABLE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_NAMES_VARIABLE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_LABEL_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PERCENT_STR);
    setScalarDoubleDefault(GO_START_ANGLE_PROPERTY_NAME_STR, 0);
    setStringDefault(GO_DIRECTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CLOCKWISE_STR);
    setVectorDoubleDefault(GO_COLOR_ORDER_PROPERTY_NAME_STR,
        { 0.0660, 0.4430, 0.7450, 0.8660, 0.3290, 0.0000, 0.9290, 0.6940, 0.1250, 0.5210, 0.0860,
            0.8190, 0.2310, 0.6660, 0.1960, 0.1840, 0.7450, 0.9370, 0.8190, 0.0150, 0.5450 });
    setRestrictedStringColorDefault(
        GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR, 0, 0, 0);
    setThreeVectorDefault(
        GO_EDGE_COLOR_PROPERTY_NAME_STR, 33.0 / 255.0, 33.0 / 255.0, 33.0 / 255.0);
    setScalarDoubleDefault(GO_FACE_ALPHA_PROPERTY_NAME_STR, 0.6);
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 10);
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_HELVETICA_STR);
    setThreeVectorDefault(
        GO_FONT_COLOR_PROPERTY_NAME_STR, 33.0 / 255.0, 33.0 / 255.0, 33.0 / 255.0);
    setStringDefault(GO_INTERPRETER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEX_STR);
    setRestrictedStringDefault(GO_LEGEND_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setStringDefault(GO_LEGEND_TITLE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_TITLE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_DISPLAY_ORDER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_DATA_STR);
    setScalarDoubleDefault(
        GO_NUM_DISPLAY_WEDGES_PROPERTY_NAME_STR, std::numeric_limits<double>::infinity());
    setRestrictedStringDefault(GO_SHOW_OTHERS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    setRestrictedStringDefault(GO_FONT_SIZE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_LABELS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_LABEL_STYLE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_NAMES_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMALIZED_STR);
    setFourVectorDefault(GO_OUTER_POSITION_PROPERTY_NAME_STR, 0, 0, 1, 1);
    setFourVectorDefault(GO_INNER_POSITION_PROPERTY_NAME_STR, 0.13, 0.11, 0.775, 0.815);
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0.13, 0.11, 0.775, 0.815);
    setStringDefault(GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OUTERPOSITION_STR);
    setupExtraDefaults();
    updateDerivedProperties();
    clearAllChanged();
}
//=============================================================================
void
GOPieChart::registerExtraProperties()
{
}
//=============================================================================
void
GOPieChart::setupExtraDefaults()
{
}
//=============================================================================
void
GOPieChart::updateModes()
{
    bool outerPositionChanged = hasChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR);
    syncOuterDrivenPositionTriplet(this);
    if (outerPositionChanged) {
        std::vector<double> position
            = pieInnerFromOuter(findVectorDoubleProperty(GO_OUTER_POSITION_PROPERTY_NAME_STR));
        if (position.size() == 4) {
            setFourVectorDefault(
                GO_POSITION_PROPERTY_NAME_STR, position[0], position[1], position[2], position[3]);
            setFourVectorDefault(GO_INNER_POSITION_PROPERTY_NAME_STR, position[0], position[1],
                position[2], position[3]);
            clearChanged(GO_POSITION_PROPERTY_NAME_STR);
            clearChanged(GO_INNER_POSITION_PROPERTY_NAME_STR);
        }
    } else if (syncInnerDrivenPositionTriplet(this)) {
        std::vector<double> outer
            = pieOuterFromInner(findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
        if (outer.size() == 4) {
            setFourVectorDefault(
                GO_OUTER_POSITION_PROPERTY_NAME_STR, outer[0], outer[1], outer[2], outer[3]);
            clearChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR);
        }
    }
    if (hasChanged(GO_DATA_PROPERTY_NAME_STR)) {
        toManual(GO_DATA_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_NAMES_PROPERTY_NAME_STR)) {
        toManual(GO_NAMES_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_LABELS_PROPERTY_NAME_STR)) {
        toManual(GO_LABELS_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_LABEL_STYLE_PROPERTY_NAME_STR)) {
        toManual(GO_LABEL_STYLE_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_FONT_SIZE_PROPERTY_NAME_STR)) {
        toManual(GO_FONT_SIZE_MODE_PROPERTY_NAME_STR);
    }
    if (haveProperty(GO_CENTER_LABEL_FONT_SIZE_PROPERTY_NAME_STR)
        && hasChanged(GO_CENTER_LABEL_FONT_SIZE_PROPERTY_NAME_STR)) {
        toManual(GO_CENTER_LABEL_FONT_SIZE_MODE_PROPERTY_NAME_STR);
    }
}
//=============================================================================
void
GOPieChart::updateDerivedProperties()
{
    std::vector<double> data = numericVector(findArrayOfProperty(GO_DATA_PROPERTY_NAME_STR));
    wstringVector names = stringVectorFromArray(findArrayOfProperty(GO_NAMES_PROPERTY_NAME_STR));
    if (names.size() != data.size()) {
        names.clear();
        for (size_t k = 0; k < data.size(); ++k) {
            names.push_back(L"data" + std::to_wstring(k + 1));
        }
        static_cast<GOArrayOfProperty*>(findProperty(GO_NAMES_PROPERTY_NAME_STR))
            ->data(stringRowArray(names));
    }

    double total = 0;
    for (double& value : data) {
        if (!std::isfinite(value) || value < 0) {
            value = 0;
        }
        total += value;
    }
    if (total <= 0) {
        total = 1;
    }

    std::vector<size_t> order(data.size());
    std::iota(order.begin(), order.end(), 0);
    std::wstring displayOrder = findStringProperty(GO_DISPLAY_ORDER_PROPERTY_NAME_STR);
    if (displayOrder == L"ascend") {
        std::stable_sort(
            order.begin(), order.end(), [&](size_t a, size_t b) { return data[a] < data[b]; });
    } else if (displayOrder == L"descend") {
        std::stable_sort(
            order.begin(), order.end(), [&](size_t a, size_t b) { return data[a] > data[b]; });
    }

    double maxDisplay = findScalarDoubleProperty(GO_NUM_DISPLAY_WEDGES_PROPERTY_NAME_STR);
    size_t keep = std::isfinite(maxDisplay)
        ? static_cast<size_t>(std::max(0.0, std::floor(maxDisplay)))
        : order.size();
    keep = std::min(keep, order.size());
    bool showOthers = stringCheck(GO_SHOW_OTHERS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);

    m_wedges.clear();
    for (size_t k = 0; k < keep; ++k) {
        size_t idx = order[k];
        m_wedges.push_back({ data[idx], data[idx] / total, names[idx], L"", idx });
    }
    if (showOthers && keep < order.size()) {
        double other = 0;
        for (size_t k = keep; k < order.size(); ++k) {
            other += data[order[k]];
        }
        if (other > 0) {
            m_wedges.push_back({ other, other / total, L"Others", L"", order.size() });
        }
    }

    wstringVector labels = stringVectorFromArray(findArrayOfProperty(GO_LABELS_PROPERTY_NAME_STR));
    std::wstring labelStyle = findStringProperty(GO_LABEL_STYLE_PROPERTY_NAME_STR);
    bool useManualLabels
        = findStringProperty(GO_LABELS_MODE_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_MANUAL_STR
        && labels.size() == m_wedges.size();
    // When names are supplied and the label style is the default ('auto'), each wedge is labelled
    // "<name> (<value>)". An explicit LabelStyle keeps the bare value/percent text.
    bool prefixWithName
        = findStringProperty(GO_NAMES_MODE_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_MANUAL_STR
        && findStringProperty(GO_LABEL_STYLE_MODE_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_AUTO_STR;
    for (size_t k = 0; k < m_wedges.size(); ++k) {
        if (useManualLabels) {
            m_wedges[k].label = labels[k];
            continue;
        }
        std::wstring value;
        if (labelStyle == GO_PROPERTY_VALUE_DATA_STR) {
            value = formatDouble(m_wedges[k].value);
        } else if (labelStyle == GO_PROPERTY_VALUE_NONE_STR) {
            value = L"";
        } else {
            value = formatPercent(m_wedges[k].proportion);
        }
        if (prefixWithName && !value.empty()) {
            m_wedges[k].label = m_wedges[k].name + L" (" + value + L")";
        } else {
            m_wedges[k].label = value;
        }
    }

    std::vector<double> proportions;
    std::vector<double> displayData;
    wstringVector displayNames;
    for (const auto& wedge : m_wedges) {
        proportions.push_back(wedge.proportion);
        displayData.push_back(wedge.value);
        displayNames.push_back(wedge.name);
    }
    static_cast<GOArrayOfProperty*>(findProperty(GO_PROPORTIONS_PROPERTY_NAME_STR))
        ->data(ArrayOf::doubleRowVector(proportions));
    static_cast<GOArrayOfProperty*>(findProperty(GO_CATEGORY_COUNTS_PROPERTY_NAME_STR))
        ->data(ArrayOf::empty());
    static_cast<GOArrayOfProperty*>(findProperty(GO_WEDGE_DISPLAY_DATA_PROPERTY_NAME_STR))
        ->data(ArrayOf::doubleRowVector(displayData));
    static_cast<GOArrayOfProperty*>(findProperty(GO_WEDGE_DISPLAY_NAMES_PROPERTY_NAME_STR))
        ->data(stringRowArray(displayNames));
    if (!useManualLabels) {
        wstringVector autoLabels;
        for (const auto& wedge : m_wedges) {
            autoLabels.push_back(wedge.label);
        }
        static_cast<GOArrayOfProperty*>(findProperty(GO_LABELS_PROPERTY_NAME_STR))
            ->data(stringRowArray(autoLabels));
    }
}
//=============================================================================
void
GOPieChart::updateState()
{
    updateModes();
    updateDerivedProperties();
    m_font = FontSpec {};
    m_font.family = findStringProperty(GO_FONT_NAME_PROPERTY_NAME_STR);
    m_font.pointSizeF = static_cast<double>(
        static_cast<int>(findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR)));
    m_font.antialias = true;
    clearAllChanged();
}
//=============================================================================
double
GOPieChart::innerRadiusRatio()
{
    return 0.0;
}
//=============================================================================
void
GOPieChart::paintCenterLabel(RenderInterface&, double, double, double, double)
{
}
//=============================================================================
void
GOPieChart::paintMe(RenderInterface& gc)
{
    updateState();
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR) || m_wedges.empty()) {
        return;
    }

    std::vector<double> position = positionInPixels(this, gc.width(), gc.height());
    double left = position[0];
    double bottom = position[1];
    double width = position[2];
    double height = position[3];
    double top = gc.height() - bottom - height;
    double legendWidth = stringCheck(GO_LEGEND_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)
        ? std::min(160.0, width * 0.32)
        : 0.0;
    double plotWidth = std::max(1.0, width - legendWidth);
    double radius = std::max(1.0, std::min(plotWidth, height) * 0.49);
    double centerX = left + plotWidth * 0.48;
    double centerYTop = top + height * 0.52;
    double centerYBottom = gc.height() - centerYTop;
    double innerRadius = radius * innerRadiusRatio();

    GOAutoFlatColorProperty* face
        = static_cast<GOAutoFlatColorProperty*>(findProperty(GO_FACE_COLOR_PROPERTY_NAME_STR));
    std::vector<double> edge
        = static_cast<GOColorProperty*>(findProperty(GO_EDGE_COLOR_PROPERTY_NAME_STR))->data();
    double alpha = findScalarDoubleProperty(GO_FACE_ALPHA_PROPERTY_NAME_STR);
    double lineWidth = findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR);
    std::vector<double> colorOrder = findVectorDoubleProperty(GO_COLOR_ORDER_PROPERTY_NAME_STR);
    std::vector<double> exploded
        = numericVector(findArrayOfProperty(GO_EXPLODED_WEDGES_PROPERTY_NAME_STR));
    bool clockwise
        = findStringProperty(GO_DIRECTION_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_CLOCKWISE_STR;

    gc.setupDirectDraw();
    double angle = findScalarDoubleProperty(GO_START_ANGLE_PROPERTY_NAME_STR);
    for (size_t k = 0; k < m_wedges.size(); ++k) {
        double span = m_wedges[k].proportion * 360.0;
        double mid = clockwise ? angle + span / 2.0 : angle - span / 2.0;
        bool isExploded = std::find_if(exploded.begin(), exploded.end(), [&](double value) {
            return static_cast<size_t>(value) == k + 1;
        }) != exploded.end();
        double offset = isExploded ? radius * 0.08 : 0.0;
        double wedgeX = centerX + offset * std::sin(mid * M_PI / 180.0);
        double wedgeY = centerYTop - offset * std::cos(mid * M_PI / 180.0);
        double qStart = 90.0 - angle;
        double qSpan = clockwise ? -span : span;
        std::vector<double> faceColor = face->isEqual(GO_PROPERTY_VALUE_NONE_STR)
            ? std::vector<double> { -1, -1, -1, 0 }
            : withAlpha(face->isEqual(GO_PROPERTY_VALUE_COLORSPEC_STR)
                      ? face->colorSpec()
                      : colorFromOrder(colorOrder, k),
                  alpha);
        gc.drawPieSlice(
            wedgeX, wedgeY, radius, innerRadius, qStart, qSpan, faceColor, edge, lineWidth);
        if (!m_wedges[k].label.empty()) {
            double labelX = centerX + radius * 1.17 * std::sin(mid * M_PI / 180.0);
            double labelYTop = centerYTop - radius * 1.17 * std::cos(mid * M_PI / 180.0);
            RenderInterface::AlignmentFlag align = RenderInterface::Mean;
            if (labelX > centerX + 2) {
                align = RenderInterface::Min;
            } else if (labelX < centerX - 2) {
                align = RenderInterface::Max;
            }
            std::wstring text
                = findStringProperty(GO_INTERPRETER_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_TEX_STR
                ? texToUnicode(m_wedges[k].label)
                : m_wedges[k].label;
            gc.putText(labelX, gc.height() - labelYTop, text,
                findVectorDoubleProperty(GO_FONT_COLOR_PROPERTY_NAME_STR), align,
                RenderInterface::Mean, m_font, 0);
        }
        angle += clockwise ? span : -span;
    }

    std::wstring title = findStringProperty(GO_TITLE_PROPERTY_NAME_STR);
    if (!title.empty()) {
        gc.putText(left + plotWidth / 2.0, gc.height() - (top + 18), title,
            findVectorDoubleProperty(GO_FONT_COLOR_PROPERTY_NAME_STR), RenderInterface::Mean,
            RenderInterface::Mean, m_font, 0);
    }
    if (stringCheck(GO_LEGEND_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
        double lx = left + plotWidth + 12;
        double ly = top + height * 0.25;
        std::wstring legendTitle = findStringProperty(GO_LEGEND_TITLE_PROPERTY_NAME_STR);
        if (!legendTitle.empty()) {
            gc.putText(lx, gc.height() - ly, legendTitle,
                findVectorDoubleProperty(GO_FONT_COLOR_PROPERTY_NAME_STR), RenderInterface::Min,
                RenderInterface::Mean, m_font, 0);
            ly += 22;
        }
        for (size_t k = 0; k < m_wedges.size(); ++k) {
            std::vector<double> c = withAlpha(colorFromOrder(colorOrder, k), alpha);
            gc.drawPieSlice(lx + 6, ly + 6, 6, 0, 0, 360, c, edge, 0.5);
            gc.putText(lx + 18, gc.height() - (ly + 6), m_wedges[k].name,
                findVectorDoubleProperty(GO_FONT_COLOR_PROPERTY_NAME_STR), RenderInterface::Min,
                RenderInterface::Mean, m_font, 0);
            ly += 18;
        }
    }
    paintCenterLabel(gc, centerX, centerYBottom, gc.height(), innerRadius);
    gc.releaseDirectDraw();
}
//=============================================================================
GODonutChart::GODonutChart() : GOPieChart()
{
    registerExtraProperties();
    setupExtraDefaults();
    sortProperties();
    updateState();
}
//=============================================================================
std::wstring
GODonutChart::getType()
{
    return GO_PROPERTY_VALUE_DONUT_CHART_STR;
}
//=============================================================================
void
GODonutChart::registerExtraProperties()
{
    if (!haveProperty(GO_INNER_RADIUS_PROPERTY_NAME_STR)) {
        registerProperty(new GOScalarProperty(0, 1), GO_INNER_RADIUS_PROPERTY_NAME_STR);
        registerProperty(new GOStringProperty, GO_CENTER_LABEL_PROPERTY_NAME_STR);
        registerProperty(new GOScalarProperty, GO_CENTER_LABEL_FONT_SIZE_PROPERTY_NAME_STR);
        registerProperty(
            new GOAutoManualProperty, GO_CENTER_LABEL_FONT_SIZE_MODE_PROPERTY_NAME_STR);
    }
}
//=============================================================================
void
GODonutChart::setupExtraDefaults()
{
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_DONUT_CHART_STR);
    setScalarDoubleDefault(GO_INNER_RADIUS_PROPERTY_NAME_STR, 0.6);
    setStringDefault(GO_CENTER_LABEL_PROPERTY_NAME_STR, L"");
    setScalarDoubleDefault(GO_CENTER_LABEL_FONT_SIZE_PROPERTY_NAME_STR, 10);
    setRestrictedStringDefault(
        GO_CENTER_LABEL_FONT_SIZE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
}
//=============================================================================
double
GODonutChart::innerRadiusRatio()
{
    return findScalarDoubleProperty(GO_INNER_RADIUS_PROPERTY_NAME_STR);
}
//=============================================================================
void
GODonutChart::paintCenterLabel(
    RenderInterface& gc, double centerX, double centerY, double figureHeight, double innerRadius)
{
    std::wstring text = findStringProperty(GO_CENTER_LABEL_PROPERTY_NAME_STR);
    if (text.empty()) {
        return;
    }
    FontSpec centerFont;
    centerFont.family = findStringProperty(GO_FONT_NAME_PROPERTY_NAME_STR);
    bool autoSize = findStringProperty(GO_CENTER_LABEL_FONT_SIZE_MODE_PROPERTY_NAME_STR)
        == GO_PROPERTY_VALUE_AUTO_STR;
    if (autoSize && innerRadius > 0) {
        // Grow the label to fill the donut hole: the height target keeps a
        // single glyph at ~65% of the hole, and the width target shrinks longer
        // labels so they stay inside the ring.
        double heightPixels = 1.3 * innerRadius;
        size_t glyphs = std::max<size_t>(text.size(), 1);
        double widthPixels = 2.9 * innerRadius / static_cast<double>(glyphs);
        double pixelSize = std::max(6.0, std::min(heightPixels, widthPixels));
        centerFont.pixelSize = pixelSize;
        // Mirror the computed size into CenterLabelFontSize (points) so it is
        // observable; data() leaves the font-size mode untouched (stays 'auto').
        setScalarDoubleDefault(
            GO_CENTER_LABEL_FONT_SIZE_PROPERTY_NAME_STR, pixelSize * 72.0 / 96.0);
    } else {
        centerFont.pointSizeF = static_cast<double>(static_cast<int>(
            findScalarDoubleProperty(GO_CENTER_LABEL_FONT_SIZE_PROPERTY_NAME_STR)));
    }
    gc.putText(centerX, centerY, text, findVectorDoubleProperty(GO_FONT_COLOR_PROPERTY_NAME_STR),
        RenderInterface::Mean, RenderInterface::Mean, centerFont, 0);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
