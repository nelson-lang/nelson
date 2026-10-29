//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOBar.hpp"
#include "GOChartDecorators.hpp"
#include "GOFontSpecHelpers.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOAxis.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCommonProperties.hpp"
#include "GOEdgeAlphaProperty.hpp"
#include "GOFaceAlphaProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOPickablePartsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GORestrictedStringColorProperty.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOSeriesIndexProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "RenderInterface.hpp"
#include "TexToUnicode.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <sstream>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    static const std::vector<std::wstring> BAR_LAYOUT_VALUES = { L"grouped", L"stacked" };
    static const std::vector<std::wstring> LABEL_LOCATION_VALUES = { L"end", L"center", L"base" };
    //=============================================================================
    static void
    addProperty(GraphicsObject* go, GOGenericProperty* property, const std::wstring& name,
        bool writable = true, bool visible = true)
    {
        go->registerProperty(property, name, writable, visible);
    }
    //=============================================================================
    static void
    setArray(GraphicsObject* go, const std::wstring& name, const std::vector<double>& values)
    {
        auto* property = static_cast<GOArrayOfProperty*>(go->findProperty(name));
        property->data(ArrayOf::doubleRowVector(values));
        property->clearModified();
    }
    //=============================================================================
    static std::vector<double>
    numericVector(const ArrayOf& value)
    {
        if (value.isEmpty() || !value.isNumeric()) {
            return {};
        }
        ArrayOf asDouble(value);
        asDouble.promoteType(NLS_DOUBLE);
        return asDouble.asDoubleVector();
    }
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
    static wstringVector
    labelsFromArray(const ArrayOf& value)
    {
        if (value.isEmpty()) {
            return {};
        }
        if (value.isRowVectorCharacterArray() || value.isStringArray()
            || value.isCellArrayOfCharacterVectors()) {
            return value.asWideStringVector(false);
        }
        if (value.isNumeric() || value.isLogical()) {
            ArrayOf asDouble(value);
            asDouble.promoteType(NLS_DOUBLE);
            std::vector<double> values = asDouble.asDoubleVector();
            wstringVector labels;
            labels.reserve(values.size());
            for (double item : values) {
                labels.push_back(formatDouble(item));
            }
            return labels;
        }
        return {};
    }
    //=============================================================================
    static double
    scalarValue(GraphicsObject* object, const std::wstring& propertyName, double fallback)
    {
        ArrayOf value = object->findProperty(propertyName)->get();
        if (!value.isNumeric() || value.isEmpty()) {
            return fallback;
        }
        return value.asDoubleScalar();
    }
    //=============================================================================
    static std::vector<double>
    propertyColor(GraphicsObject* object, const std::wstring& propertyName,
        const std::vector<double>& fallback)
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
        return fallback;
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
    static double
    minPositiveStep(const std::vector<double>& x)
    {
        std::vector<double> values;
        values.reserve(x.size());
        for (double value : x) {
            if (std::isfinite(value)) {
                values.push_back(value);
            }
        }
        std::sort(values.begin(), values.end());
        values.erase(std::unique(values.begin(), values.end()), values.end());
        double step = std::numeric_limits<double>::infinity();
        for (size_t k = 1; k < values.size(); ++k) {
            double dx = values[k] - values[k - 1];
            if (dx > 0. && std::isfinite(dx)) {
                step = std::min(step, dx);
            }
        }
        return std::isfinite(step) ? step : 1.;
    }
    //=============================================================================
    static std::vector<double>
    localSteps(const std::vector<double>& x)
    {
        std::vector<double> steps(x.size(), 1.);
        if (x.size() < 2) {
            return steps;
        }
        double fallback = minPositiveStep(x);
        for (size_t k = 0; k < x.size(); ++k) {
            double dx = k + 1 < x.size() ? x[k + 1] - x[k] : x[k] - x[k - 1];
            steps[k] = (dx > 0. && std::isfinite(dx)) ? dx : fallback;
        }
        return steps;
    }
    //=============================================================================
    static double
    automaticGroupWidth(double barWidth, int peers, bool stacked)
    {
        if (stacked || peers <= 1) {
            return barWidth;
        }
        return std::min(barWidth, static_cast<double>(peers) / (static_cast<double>(peers) + 1.5));
    }
    //=============================================================================
    static int
    seriesIndex(GraphicsObject* object)
    {
        auto* property = static_cast<GOSeriesIndexProperty*>(
            object->findProperty(GO_SERIES_INDEX_PROPERTY_NAME_STR, false));
        if (!property || property->isNone()) {
            return 1;
        }
        return std::max(1, static_cast<int>(std::round(property->value())));
    }
    //=============================================================================
    static double
    barEndPoint(double base, double value, bool stacked)
    {
        return stacked ? base + value : value;
    }
    //=============================================================================
    static void
    drawBar(RenderInterface& gc, bool horizontal, double left, double right, double base,
        double top, const std::vector<double>& faceColor, const std::vector<double>& edgeColor)
    {
        if (horizontal) {
            if (isDrawableColor(faceColor)) {
                gc.color(faceColor);
                gc.quad(base, left, 0., top, left, 0., top, right, 0., base, right, 0.);
            }
            if (isDrawableColor(edgeColor)) {
                gc.color(edgeColor);
                gc.quadline(base, left, 0., top, left, 0., top, right, 0., base, right, 0.);
            }
            return;
        }
        if (isDrawableColor(faceColor)) {
            gc.color(faceColor);
            gc.quad(left, base, 0., left, top, 0., right, top, 0., right, base, 0.);
        }
        if (isDrawableColor(edgeColor)) {
            gc.color(edgeColor);
            gc.quadline(left, base, 0., left, top, 0., right, top, 0., right, base, 0.);
        }
    }
    //=============================================================================
    static void
    drawBaseLine(RenderInterface& gc, GraphicsObject* object, bool horizontal,
        const std::vector<double>& color)
    {
        if (!isDrawableColor(color)
            || object->stringCheck(
                GO_SHOW_BASE_LINE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
            return;
        }
        GOAxis* axis = object->getParentAxis();
        if (!axis) {
            return;
        }
        std::vector<double> xlim = axis->findVectorDoubleProperty(GO_X_LIM_PROPERTY_NAME_STR);
        std::vector<double> ylim = axis->findVectorDoubleProperty(GO_Y_LIM_PROPERTY_NAME_STR);
        if (xlim.size() < 2 || ylim.size() < 2) {
            return;
        }
        double baseValue = object->findScalarDoubleProperty(GO_BASE_VALUE_PROPERTY_NAME_STR);
        if (!std::isfinite(baseValue)) {
            return;
        }
        gc.color(color);
        if (horizontal) {
            gc.line(baseValue, ylim[0], 0., baseValue, ylim[1], 0.);
        } else {
            gc.line(xlim[0], baseValue, 0., xlim[1], baseValue, 0.);
        }
    }
    //=============================================================================
    static FontSpec
    barLabelFont(GraphicsObject* object)
    {
        FontSpec font;
        font.family = object->findStringProperty(GO_FONT_NAME_PROPERTY_NAME_STR);
        font.pointSizeF = static_cast<double>(
            static_cast<int>(object->findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR)));
        std::wstring weight = object->findStringProperty(GO_FONT_WEIGHT_PROPERTY_NAME_STR);
        if (weight == GO_PROPERTY_VALUE_BOLD_STR) {
            font.setWeight(FontSpec::WeightBold);
        } else if (weight == GO_PROPERTY_VALUE_LIGHT_STR) {
            font.setWeight(FontSpec::WeightLight);
        }
        std::wstring angle = object->findStringProperty(GO_FONT_ANGLE_PROPERTY_NAME_STR);
        font.italic
            = angle == GO_PROPERTY_VALUE_ITALIC_STR || angle == GO_PROPERTY_VALUE_OBLIQUE_STR;
        font.antialias = true;
        return font;
    }
    //=============================================================================
    static void
    drawBarLabels(RenderInterface& gc, GraphicsObject* object, bool horizontal,
        const std::vector<double>& x, const std::vector<double>& y, const std::vector<double>& base,
        double centerOffset)
    {
        wstringVector labels
            = labelsFromArray(object->findArrayOfProperty(GO_LABELS_PROPERTY_NAME_STR));
        if (labels.empty()) {
            return;
        }
        std::vector<double> labelColor
            = propertyColor(object, GO_LABEL_COLOR_PROPERTY_NAME_STR, { 0., 0., 0. });
        if (!isDrawableColor(labelColor)) {
            return;
        }
        FontSpec font = barLabelFont(object);
        bool useTex = object->findStringProperty(GO_INTERPRETER_PROPERTY_NAME_STR)
            == GO_PROPERTY_VALUE_TEX_STR;
        std::wstring location = object->findStringProperty(GO_LABEL_LOCATION_PROPERTY_NAME_STR);
        gc.setupDirectDraw();
        bool stacked = object->findStringProperty(GO_BAR_LAYOUT_PROPERTY_NAME_STR) == L"stacked";
        for (size_t k = 0; k < x.size() && k < y.size(); ++k) {
            if (!std::isfinite(x[k]) || !std::isfinite(y[k]) || !std::isfinite(base[k])) {
                continue;
            }
            double center = x[k] + centerOffset;
            double top = barEndPoint(base[k], y[k], stacked);
            double labelValue = top;
            if (location == L"center") {
                labelValue = (base[k] + top) / 2.;
            } else if (location == L"base") {
                labelValue = base[k];
            }
            double pxData = horizontal ? labelValue : center;
            double pyData = horizontal ? center : labelValue;
            int px = 0;
            int py = 0;
            gc.toPixels(pxData, pyData, 0., px, py);
            std::wstring text = labels[k % labels.size()];
            if (useTex) {
                text = texToUnicode(text);
            }
            RenderInterface::AlignmentFlag xAlign = RenderInterface::Mean;
            RenderInterface::AlignmentFlag yAlign = RenderInterface::Mean;
            if (location == L"end") {
                if (horizontal) {
                    xAlign = top >= base[k] ? RenderInterface::Min : RenderInterface::Max;
                } else {
                    yAlign = top >= base[k] ? RenderInterface::Min : RenderInterface::Max;
                }
            }
            gc.putText(px, py, text, labelColor, xAlign, yAlign, font, 0.);
        }
        gc.releaseDirectDraw();
    }
    //=============================================================================
} // namespace
//=============================================================================
GOBar::GOBar()
{
    addProperty(
        this, new GORestrictedStringProperty(BAR_LAYOUT_VALUES), GO_BAR_LAYOUT_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_BAR_WIDTH_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_GROUP_WIDTH_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_GROUP_WIDTH_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOLazyDecoratorProperty(GODecoratorKind::Baseline),
        GO_BASE_LINE_PROPERTY_NAME_STR, false);
    addProperty(this, new GOScalarProperty, GO_BASE_VALUE_PROPERTY_NAME_STR);
    addProperty(this, new GOEdgeAlphaProperty, GO_EDGE_ALPHA_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoFlatColorProperty, GO_EDGE_COLOR_PROPERTY_NAME_STR);
    addProperty(this, new GOFaceAlphaProperty, GO_FACE_ALPHA_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoFlatColorProperty, GO_FACE_COLOR_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_FACE_COLOR_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_C_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_C_DATA_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_HORIZONTAL_PROPERTY_NAME_STR);
    addProperty(this, new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_CLIPPING_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_X_END_POINTS_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_Y_END_POINTS_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_LABELS_PROPERTY_NAME_STR);
    addProperty(this, new GORestrictedStringProperty(LABEL_LOCATION_VALUES),
        GO_LABEL_LOCATION_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_LABEL_LOCATION_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoFlatColorProperty, GO_LABEL_COLOR_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_LABEL_COLOR_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_FONT_SIZE_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_FONT_ANGLE_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_INTERPRETER_PROPERTY_NAME_STR);
    addProperty(this, new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR, false);
    addProperty(this, new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    addProperty(this, new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    addProperty(this, new GOCallbackProperty, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    addProperty(this, new GOGObjectsProperty, GO_CONTEXT_MENU_PROPERTY_NAME_STR);
    addProperty(this, new GOLazyDecoratorProperty(GODecoratorKind::AnnotationEventData),
        GO_ANNOTATION_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_SELECTED_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_HIT_TEST_PROPERTY_NAME_STR);
    addProperty(this, new GOPickablePartsProperty, GO_PICKABLE_PARTS_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    addProperty(this, new GOLazyDecoratorProperty(GODecoratorKind::DataTipTemplate),
        GO_DATA_TIP_TEMPLATE_PROPERTY_NAME_STR);
    addProperty(this, new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    addProperty(this, new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    addProperty(this, new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    addProperty(this, new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    addProperty(this, new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_X_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_X_DATA_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_X_DATA_SOURCE_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_Y_DATA_SOURCE_PROPERTY_NAME_STR);
    addProperty(this, new GOSeriesIndexProperty, GO_SERIES_INDEX_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_SHOW_BASE_LINE_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_NUM_PEERS_PROPERTY_NAME_STR, true, false);
    addProperty(this, new GOArrayOfProperty, GO_BAR_BASE_DATA_PROPERTY_NAME_STR, true, false);
    registerCommonProperties(this);

    setRestrictedStringDefault(GO_BAR_LAYOUT_PROPERTY_NAME_STR, L"grouped");
    setScalarDoubleDefault(GO_BAR_WIDTH_PROPERTY_NAME_STR, 0.8);
    setScalarDoubleDefault(GO_GROUP_WIDTH_PROPERTY_NAME_STR, 0.8);
    setRestrictedStringDefault(GO_GROUP_WIDTH_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_BASE_VALUE_PROPERTY_NAME_STR, 0.);
    setRestrictedStringScalarDefault(
        GO_EDGE_ALPHA_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALAR_STR, 1.);
    setRestrictedStringColorDefault(GO_EDGE_COLOR_PROPERTY_NAME_STR,
        GO_PROPERTY_VALUE_COLORSPEC_STR, 33. / 255., 33. / 255., 33. / 255.);
    setRestrictedStringScalarDefault(
        GO_FACE_ALPHA_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALAR_STR, 1.);
    setRestrictedStringColorDefault(
        GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR, 0., 0.447, 0.741);
    setRestrictedStringDefault(GO_FACE_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_C_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_HORIZONTAL_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_CLIPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_LABEL_LOCATION_PROPERTY_NAME_STR, L"end");
    setRestrictedStringDefault(
        GO_LABEL_LOCATION_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringColorDefault(
        GO_LABEL_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR, 0., 0., 0.);
    setRestrictedStringDefault(GO_LABEL_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_HELVETICA_STR);
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 10.);
    setStringDefault(GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setStringDefault(GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setStringDefault(GO_INTERPRETER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEX_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_SELECTED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HIT_TEST_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_PICKABLE_PARTS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VISIBLE_STR);
    setStringDefault(GO_DISPLAY_NAME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setRestrictedStringDefault(GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setStringDefault(GO_X_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setStringDefault(GO_Y_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setRestrictedStringDefault(GO_SHOW_BASE_LINE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setScalarDoubleDefault(GO_NUM_PEERS_PROPERTY_NAME_STR, 1.);
    static_cast<GOArrayOfProperty*>(findProperty(GO_LABELS_PROPERTY_NAME_STR))
        ->data(ArrayOf::stringArray(wstringVector(), Dimensions(1, 0)));
    auto* seriesIndex
        = static_cast<GOSeriesIndexProperty*>(findProperty(GO_SERIES_INDEX_PROPERTY_NAME_STR));
    seriesIndex->setValue(1.);
    seriesIndex->clearModified();
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BAR_STR);
    initializeCommonProperties(this);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BAR_STR);
    clearAllChanged();
    sortProperties();
}
//=============================================================================
GOBar::~GOBar() = default;
//=============================================================================
std::wstring
GOBar::getType()
{
    return GO_PROPERTY_VALUE_BAR_STR;
}
//=============================================================================
void
GOBar::updateState()
{
    std::vector<double> x = numericVector(findArrayOfProperty(GO_X_DATA_PROPERTY_NAME_STR));
    std::vector<double> y = numericVector(findArrayOfProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    if (y.empty()) {
        setArray(this, GO_X_END_POINTS_PROPERTY_NAME_STR, std::vector<double> {});
        setArray(this, GO_Y_END_POINTS_PROPERTY_NAME_STR, std::vector<double> {});
        limitsDirty = true;
        return;
    }
    if (x.empty()) {
        x.resize(y.size());
        for (size_t k = 0; k < x.size(); ++k) {
            x[k] = static_cast<double>(k + 1);
        }
        setArray(this, GO_X_DATA_PROPERTY_NAME_STR, x);
    }
    size_t n = std::min(x.size(), y.size());
    x.resize(n);
    y.resize(n);

    bool stacked = findStringProperty(GO_BAR_LAYOUT_PROPERTY_NAME_STR) == L"stacked";
    int peers = std::max(
        1, static_cast<int>(std::round(findScalarDoubleProperty(GO_NUM_PEERS_PROPERTY_NAME_STR))));
    int index = std::min(seriesIndex(this), peers);
    double barWidth = std::max(0., findScalarDoubleProperty(GO_BAR_WIDTH_PROPERTY_NAME_STR));
    double groupWidth = findScalarDoubleProperty(GO_GROUP_WIDTH_PROPERTY_NAME_STR);
    if (isAuto(GO_GROUP_WIDTH_MODE_PROPERTY_NAME_STR)) {
        groupWidth = automaticGroupWidth(barWidth, peers, stacked);
        setScalarDoubleDefault(GO_GROUP_WIDTH_PROPERTY_NAME_STR, groupWidth);
    }

    std::vector<double> base
        = numericVector(findArrayOfProperty(GO_BAR_BASE_DATA_PROPERTY_NAME_STR));
    if (base.size() < n) {
        base.assign(n, findScalarDoubleProperty(GO_BASE_VALUE_PROPERTY_NAME_STR));
    }
    std::vector<double> centers(n);
    std::vector<double> endpoints(n);
    double offset = 0.;
    if (!stacked && peers > 1) {
        offset = (static_cast<double>(index) - (static_cast<double>(peers) + 1.) / 2.) * groupWidth
            / static_cast<double>(peers);
    }
    // The step sorts the positions: compute it once, not once per bar.
    const double centerOffset = offset * minPositiveStep(x);
    for (size_t k = 0; k < n; ++k) {
        centers[k] = x[k] + centerOffset;
        endpoints[k] = barEndPoint(base[k], y[k], stacked);
    }
    setArray(this, GO_X_END_POINTS_PROPERTY_NAME_STR, centers);
    setArray(this, GO_Y_END_POINTS_PROPERTY_NAME_STR, endpoints);
    limitsDirty = true;
    clearChanged({ GO_X_DATA_PROPERTY_NAME_STR, GO_Y_DATA_PROPERTY_NAME_STR,
        GO_BAR_BASE_DATA_PROPERTY_NAME_STR, GO_BAR_LAYOUT_PROPERTY_NAME_STR,
        GO_BAR_WIDTH_PROPERTY_NAME_STR, GO_GROUP_WIDTH_PROPERTY_NAME_STR,
        GO_GROUP_WIDTH_MODE_PROPERTY_NAME_STR, GO_NUM_PEERS_PROPERTY_NAME_STR,
        GO_BASE_VALUE_PROPERTY_NAME_STR, GO_SERIES_INDEX_PROPERTY_NAME_STR });
}
//=============================================================================
void
GOBar::paintMe(RenderInterface& gc)
{
    updateState();
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    std::vector<double> x = numericVector(findArrayOfProperty(GO_X_DATA_PROPERTY_NAME_STR));
    std::vector<double> y = numericVector(findArrayOfProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    if (x.empty() || y.empty()) {
        return;
    }
    size_t n = std::min(x.size(), y.size());
    x.resize(n);
    y.resize(n);
    bool horizontal = stringCheck(GO_HORIZONTAL_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    bool stacked = findStringProperty(GO_BAR_LAYOUT_PROPERTY_NAME_STR) == L"stacked";
    int peers = std::max(
        1, static_cast<int>(std::round(findScalarDoubleProperty(GO_NUM_PEERS_PROPERTY_NAME_STR))));
    int index = std::min(seriesIndex(this), peers);
    double barWidth = std::max(0., findScalarDoubleProperty(GO_BAR_WIDTH_PROPERTY_NAME_STR));
    double groupWidth = findScalarDoubleProperty(GO_GROUP_WIDTH_PROPERTY_NAME_STR);
    double span = barWidth;
    if (!stacked && peers > 1) {
        span = groupWidth * barWidth / static_cast<double>(peers);
    }
    double offset = 0.;
    if (!stacked && peers > 1) {
        offset = (static_cast<double>(index) - (static_cast<double>(peers) + 1.) / 2.) * groupWidth
            / static_cast<double>(peers);
    }
    std::vector<double> base
        = numericVector(findArrayOfProperty(GO_BAR_BASE_DATA_PROPERTY_NAME_STR));
    if (base.size() < n) {
        base.assign(n, findScalarDoubleProperty(GO_BASE_VALUE_PROPERTY_NAME_STR));
    }
    std::vector<double> steps = localSteps(x);
    const double centerOffset = offset * minPositiveStep(x);
    std::vector<double> faceColor
        = propertyColor(this, GO_FACE_COLOR_PROPERTY_NAME_STR, { 0., 0.447, 0.741 });
    std::vector<double> edgeColor = propertyColor(
        this, GO_EDGE_COLOR_PROPERTY_NAME_STR, { 33. / 255., 33. / 255., 33. / 255. });
    appendAlpha(faceColor, scalarValue(this, GO_FACE_ALPHA_PROPERTY_NAME_STR, 1.));
    appendAlpha(edgeColor, scalarValue(this, GO_EDGE_ALPHA_PROPERTY_NAME_STR, 1.));
    double lineWidth = findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR);
    gc.lineWidth(lineWidth);
    gc.setLineStyle(findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR));
    if (index == 1) {
        drawBaseLine(gc, this, horizontal, edgeColor);
    }
    // Collect the bars once, then draw them as a single batched "rects" primitive
    // (RenderWeb); a backend that cannot batch (Qt/OpenGL) returns false and the
    // per-bar loop runs. lefts/rights/bases/tops are the bar extents in data
    // coordinates.
    std::vector<double> lefts;
    std::vector<double> rights;
    std::vector<double> bases;
    std::vector<double> tops;
    lefts.reserve(n);
    rights.reserve(n);
    bases.reserve(n);
    tops.reserve(n);
    for (size_t k = 0; k < n; ++k) {
        if (!std::isfinite(x[k]) || !std::isfinite(y[k]) || !std::isfinite(base[k])) {
            continue;
        }
        double center = x[k] + centerOffset;
        double half = steps[k] * span / 2.;
        lefts.push_back(center - half);
        rights.push_back(center + half);
        bases.push_back(base[k]);
        tops.push_back(barEndPoint(base[k], y[k], stacked));
    }
    std::vector<double> corners;
    corners.reserve(lefts.size() * 4);
    for (size_t k = 0; k < lefts.size(); ++k) {
        // Two opposite corners; horizontal bars swap the value and position axes.
        if (horizontal) {
            corners.push_back(bases[k]);
            corners.push_back(lefts[k]);
            corners.push_back(tops[k]);
            corners.push_back(rights[k]);
        } else {
            corners.push_back(lefts[k]);
            corners.push_back(bases[k]);
            corners.push_back(rights[k]);
            corners.push_back(tops[k]);
        }
    }
    if (!gc.rectBatch(corners, faceColor, edgeColor, lineWidth)) {
        for (size_t k = 0; k < lefts.size(); ++k) {
            drawBar(gc, horizontal, lefts[k], rights[k], bases[k], tops[k], faceColor, edgeColor);
        }
    }
    drawBarLabels(gc, this, horizontal, x, y, base, centerOffset);
}
//=============================================================================
std::vector<double>
GOBar::getLimits()
{
    if (!limitsDirty) {
        return limits;
    }
    updateState();
    std::vector<double> x = numericVector(findArrayOfProperty(GO_X_DATA_PROPERTY_NAME_STR));
    std::vector<double> y = numericVector(findArrayOfProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    if (x.empty() || y.empty()) {
        limits = { 0., 1., 0., 1., 0., 0., 0., 1., 1., 1. };
        limitsDirty = false;
        return limits;
    }
    size_t n = std::min(x.size(), y.size());
    x.resize(n);
    y.resize(n);
    std::vector<double> base
        = numericVector(findArrayOfProperty(GO_BAR_BASE_DATA_PROPERTY_NAME_STR));
    if (base.size() < n) {
        base.assign(n, findScalarDoubleProperty(GO_BASE_VALUE_PROPERTY_NAME_STR));
    }
    bool stacked = findStringProperty(GO_BAR_LAYOUT_PROPERTY_NAME_STR) == L"stacked";
    double step = minPositiveStep(x);
    double posMin = std::numeric_limits<double>::infinity();
    double posMax = -std::numeric_limits<double>::infinity();
    double valueMin = std::min(0., findScalarDoubleProperty(GO_BASE_VALUE_PROPERTY_NAME_STR));
    double valueMax = std::max(0., findScalarDoubleProperty(GO_BASE_VALUE_PROPERTY_NAME_STR));
    for (size_t k = 0; k < n; ++k) {
        if (std::isfinite(x[k])) {
            posMin = std::min(posMin, x[k]);
            posMax = std::max(posMax, x[k]);
        }
        if (std::isfinite(base[k]) && std::isfinite(y[k])) {
            double top = barEndPoint(base[k], y[k], stacked);
            valueMin = std::min(valueMin, std::min(base[k], top));
            valueMax = std::max(valueMax, std::max(base[k], top));
        }
    }
    if (!std::isfinite(posMin) || !std::isfinite(posMax)) {
        posMin = 0.;
        posMax = 1.;
    } else {
        double barWidth = findScalarDoubleProperty(GO_BAR_WIDTH_PROPERTY_NAME_STR);
        double groupWidth = findScalarDoubleProperty(GO_GROUP_WIDTH_PROPERTY_NAME_STR);
        bool grouped = findStringProperty(GO_BAR_LAYOUT_PROPERTY_NAME_STR) == L"grouped";
        int peers = std::max(1,
            static_cast<int>(std::round(findScalarDoubleProperty(GO_NUM_PEERS_PROPERTY_NAME_STR))));
        double pad = (0.8 + barWidth / 2.) * step;
        if (grouped && peers > 1) {
            pad = (groupWidth / 2. + std::max(0., 1. - barWidth)) * step;
        }
        posMin -= pad;
        posMax += pad;
    }
    bool horizontal = stringCheck(GO_HORIZONTAL_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    if (horizontal) {
        limits = { valueMin, valueMax, posMin, posMax, 0., 0., 0., 1., 1., 1. };
    } else {
        limits = { posMin, posMax, valueMin, valueMax, 0., 0., 0., 1., 1., 1. };
    }
    limitsDirty = false;
    return limits;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
