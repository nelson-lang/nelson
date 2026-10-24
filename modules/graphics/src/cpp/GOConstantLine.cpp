//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <limits>
#include "GOConstantLine.hpp"
#include "GOAxis.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOAlignHorizProperty.hpp"
#include "GOFontAngleProperty.hpp"
#include "GOFontWeightProperty.hpp"
#include "GOTextInterpreterProperty.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOCommonProperties.hpp"
#include "RenderInterface.hpp"
#include "TexToUnicode.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Dark grey (33/255) shared with the default line/label colour.
static constexpr double CONSTANT_LINE_GREY = 0.129411764705882;
//=============================================================================
GOConstantLine::GOConstantLine()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
GOConstantLine::~GOConstantLine() = default;
//=============================================================================
std::wstring
GOConstantLine::getType()
{
    return GO_PROPERTY_VALUE_CONSTANTLINE_STR;
}
//=============================================================================
void
GOConstantLine::constructProperties()
{
    registerProperty(new GOScalarProperty, GO_VALUE_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty(std::vector<std::wstring> {
                         GO_PROPERTY_VALUE_X_STR, GO_PROPERTY_VALUE_Y_STR }),
        GO_INTERCEPT_AXIS_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOFontAngleProperty, GO_FONT_ANGLE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_FONT_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOFontWeightProperty, GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    registerProperty(new GOTextInterpreterProperty, GO_INTERPRETER_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty(std::vector<std::wstring> {
                         GO_PROPERTY_VALUE_ALIGNED_STR, GO_PROPERTY_VALUE_HORIZONTAL_STR }),
        GO_LABEL_ORIENTATION_PROPERTY_NAME_STR);
    registerProperty(new GOAlignHorizProperty, GO_LABEL_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
    registerProperty(
        new GORestrictedStringProperty(std::vector<std::wstring> { GO_PROPERTY_VALUE_TOP_STR,
            GO_PROPERTY_VALUE_MIDDLE_STR, GO_PROPERTY_VALUE_BOTTOM_STR }),
        GO_LABEL_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty(0, 1), GO_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_LABEL_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty(std::vector<std::wstring> {
                         GO_PROPERTY_VALUE_BOTTOM_STR, GO_PROPERTY_VALUE_TOP_STR }),
        GO_LAYER_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_COLOR_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_LABEL_COLOR_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_LINE_STYLE_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);

    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
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
GOConstantLine::setupDefaults()
{
    setScalarDoubleDefault(GO_VALUE_PROPERTY_NAME_STR, 1.);
    setRestrictedStringDefault(GO_INTERCEPT_AXIS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_X_STR);
    setStringDefault(GO_LABEL_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setRestrictedStringDefault(GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 10.);
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, L"Helvetica");
    setRestrictedStringDefault(GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(GO_INTERPRETER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEX_STR);
    setRestrictedStringDefault(
        GO_LABEL_ORIENTATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ALIGNED_STR);
    setRestrictedStringDefault(
        GO_LABEL_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RIGHT_STR);
    setRestrictedStringDefault(
        GO_LABEL_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOP_STR);
    setScalarDoubleDefault(GO_ALPHA_PROPERTY_NAME_STR, 0.7);
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setThreeVectorDefault(
        GO_COLOR_PROPERTY_NAME_STR, CONSTANT_LINE_GREY, CONSTANT_LINE_GREY, CONSTANT_LINE_GREY);
    setThreeVectorDefault(GO_LABEL_COLOR_PROPERTY_NAME_STR, CONSTANT_LINE_GREY, CONSTANT_LINE_GREY,
        CONSTANT_LINE_GREY);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setRestrictedStringDefault(GO_LAYER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOP_STR);
    setRestrictedStringDefault(GO_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_LABEL_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_LINE_STYLE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setStringDefault(GO_DISPLAY_NAME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);

    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_TAG_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CONSTANTLINE_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);

    initializeCommonProperties(this);
    clearAllChanged();
}
//=============================================================================
std::vector<double>
GOConstantLine::getLimits()
{
    const double value = findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR);
    if (!std::isfinite(value)) {
        return {};
    }
    // The axes autoscale to include the constant value on the InterceptAxis
    // only. The spanning axis (and z / color / alpha) get a neutral, inverted
    // range [+inf, -inf] so the line never drives those limits; the axis merge
    // keeps the current limits for any dimension left inverted (see
    // GOAxis::updateLimits).
    const double inf = std::numeric_limits<double>::infinity();
    const bool vertical
        = findStringProperty(GO_INTERCEPT_AXIS_PROPERTY_NAME_STR) != GO_PROPERTY_VALUE_Y_STR;
    std::vector<double> limits(10);
    limits[0] = vertical ? value : inf;
    limits[1] = vertical ? value : -inf;
    limits[2] = vertical ? inf : value;
    limits[3] = vertical ? -inf : value;
    limits[4] = inf;
    limits[5] = -inf;
    limits[6] = inf;
    limits[7] = -inf;
    limits[8] = inf;
    limits[9] = -inf;
    return limits;
}
//=============================================================================
void
GOConstantLine::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    GOAxis* parent = static_cast<GOAxis*>(getParentAxis());
    if (!parent) {
        return;
    }
    std::vector<double> xLimits(parent->findVectorDoubleProperty(GO_X_LIM_PROPERTY_NAME_STR));
    std::vector<double> yLimits(parent->findVectorDoubleProperty(GO_Y_LIM_PROPERTY_NAME_STR));
    if (xLimits.size() < 2 || yLimits.size() < 2) {
        return;
    }
    const bool vertical
        = findStringProperty(GO_INTERCEPT_AXIS_PROPERTY_NAME_STR) != GO_PROPERTY_VALUE_Y_STR;
    const double value = findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR);
    if (!std::isfinite(value)) {
        return;
    }

    std::vector<double> xs(2);
    std::vector<double> ys(2);
    std::vector<double> zs(2, 0.);
    if (vertical) {
        // xline: vertical segment at x = value spanning the full y range.
        xs[0] = value;
        xs[1] = value;
        ys[0] = yLimits[0];
        ys[1] = yLimits[1];
    } else {
        // yline: horizontal segment at y = value spanning the full x range.
        xs[0] = xLimits[0];
        xs[1] = xLimits[1];
        ys[0] = value;
        ys[1] = value;
    }

    std::vector<double> mxs;
    std::vector<double> mys;
    std::vector<double> mzs;
    parent->reMap(xs, ys, zs, mxs, mys, mzs);

    std::wstring lineStyle(findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR));
    GOColorProperty* colorProperty
        = static_cast<GOColorProperty*>(findProperty(GO_COLOR_PROPERTY_NAME_STR));
    if (!colorProperty->isNone() && lineStyle != GO_PROPERTY_VALUE_NONE_STR) {
        gc.lineWidth(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
        gc.color(colorProperty->data());
        gc.setLineStyle(lineStyle);
        gc.lineSeries(mxs, mys, mzs);
    }
    paintLabel(
        gc, vertical, mxs.empty() ? 0. : mxs[0], mys.empty() ? 0. : mys[0], xLimits, yLimits);
}
//=============================================================================
void
GOConstantLine::paintLabel(RenderInterface& gc, bool vertical, double mappedX, double mappedY,
    const std::vector<double>& xLimits, const std::vector<double>& yLimits)
{
    std::wstring label(findStringProperty(GO_LABEL_PROPERTY_NAME_STR));
    if (label.empty()) {
        return;
    }
    GOColorProperty* labelColorProperty
        = static_cast<GOColorProperty*>(findProperty(GO_LABEL_COLOR_PROPERTY_NAME_STR));
    if (labelColorProperty->isNone()) {
        return;
    }
    GOAxis* parent = static_cast<GOAxis*>(getParentAxis());
    if (!parent) {
        return;
    }
    const double value = findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR);

    // Anchor the label at the corner selected by the alignment properties: along
    // the line the vertical alignment picks an end, across it the horizontal one.
    std::wstring vAlign(findStringProperty(GO_LABEL_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR));
    std::wstring hAlign(findStringProperty(GO_LABEL_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR));
    std::vector<double> anchorX(1);
    std::vector<double> anchorY(1);
    std::vector<double> anchorZ(1, 0.);
    if (vertical) {
        anchorX[0] = value;
        anchorY[0] = (vAlign == GO_PROPERTY_VALUE_BOTTOM_STR)
            ? yLimits[0]
            : ((vAlign == GO_PROPERTY_VALUE_MIDDLE_STR) ? (yLimits[0] + yLimits[1]) / 2.
                                                        : yLimits[1]);
    } else {
        anchorY[0] = value;
        anchorX[0] = (hAlign == GO_PROPERTY_VALUE_LEFT_STR)
            ? xLimits[0]
            : ((hAlign == GO_PROPERTY_VALUE_CENTER_STR) ? (xLimits[0] + xLimits[1]) / 2.
                                                        : xLimits[1]);
    }
    std::vector<double> mx;
    std::vector<double> my;
    std::vector<double> mz;
    parent->reMap(anchorX, anchorY, anchorZ, mx, my, mz);
    if (mx.empty()) {
        return;
    }
    double px = 0.;
    double py = 0.;
    gc.toPixels(mx[0], my[0], mz[0], px, py);

    FontSpec font;
    font.family = findStringProperty(GO_FONT_NAME_PROPERTY_NAME_STR);
    font.pointSizeF = findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR);
    if (findStringProperty(GO_FONT_WEIGHT_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_BOLD_STR) {
        font.setWeight(FontSpec::WeightBold);
    }
    std::wstring angle(findStringProperty(GO_FONT_ANGLE_PROPERTY_NAME_STR));
    font.italic = angle == GO_PROPERTY_VALUE_ITALIC_STR || angle == GO_PROPERTY_VALUE_OBLIQUE_STR;
    font.antialias = true;

    std::wstring text(label);
    if (findStringProperty(GO_INTERPRETER_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_TEX_STR) {
        text = texToUnicode(text);
    }

    RenderInterface::AlignmentFlag xFlag = (hAlign == GO_PROPERTY_VALUE_LEFT_STR)
        ? RenderInterface::Min
        : ((hAlign == GO_PROPERTY_VALUE_CENTER_STR) ? RenderInterface::Mean : RenderInterface::Max);
    RenderInterface::AlignmentFlag yFlag = (vAlign == GO_PROPERTY_VALUE_BOTTOM_STR)
        ? RenderInterface::Max
        : ((vAlign == GO_PROPERTY_VALUE_MIDDLE_STR) ? RenderInterface::Mean : RenderInterface::Min);

    gc.setupDirectDraw();
    gc.putText(px, py, text, labelColorProperty->data(), xFlag, yFlag, font, 0.);
    gc.releaseDirectDraw();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
