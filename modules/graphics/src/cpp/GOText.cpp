//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _USE_MATH_DEFINES
#include <cmath>
#include <QtGui/QFontMetrics>
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOText.hpp"
#include "GOAxis.hpp"
#include "GOList.hpp"
#include "GOAlignVertProperty.hpp"
#include "GOAlignHorizProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOFontWeightProperty.hpp"
#include "GOFontUnitsProperty.hpp"
#include "GOFontAngleProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOVectorThreeDoubleProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "QStringConverter.hpp"
#include "GOTextInterpreterProperty.hpp"
#include "TexToUnicode.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
GOText::getType()
{
    return GO_PROPERTY_VALUE_TEXT_STR;
}
//=============================================================================
GOText::GOText()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
GOText::~GOText() { }
//=============================================================================
int
GOText::getTextHeightInPixels()
{
    QFontMetrics fm(fnt);
    QRect sze(fm.boundingRect("|"));
    return sze.height();
}
//=============================================================================
void
GOText::constructProperties()
{
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOFourVectorProperty, GO_BOUNDING_BOX_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_STRING_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_EXTENT_PROPERTY_NAME_STR);
    registerProperty(new GOAlignHorizProperty, GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
    registerProperty(new GOThreeVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_ROTATION_PROPERTY_NAME_STR);
    registerProperty(new GOUnitsProperty, GO_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOAlignVertProperty, GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_BACKGROUND_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_MARGIN_PROPERTY_NAME_STR);
    registerProperty(new GOFontAngleProperty, GO_FONT_ANGLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_FONT_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOFontUnitsProperty, GO_FONT_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOFontWeightProperty, GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOTextInterpreterProperty, GO_INTERPRETER_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_FONT_SMOOTHING_PROPERTY_NAME_STR);

    sortProperties();
}
//=============================================================================
void
GOText::setupDefaults()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(
        GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LEFT_STR);
    setThreeVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0, 0, 0);
    setScalarDoubleDefault(GO_ROTATION_PROPERTY_NAME_STR, 0);
    setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_DATA_STR);
    setRestrictedStringDefault(
        GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MIDDLE_STR);
    setThreeVectorDefault(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, -1, -1, -1);
    setThreeVectorDefault(GO_EDGE_COLOR_PROPERTY_NAME_STR, -1, -1, -1);
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setScalarDoubleDefault(GO_MARGIN_PROPERTY_NAME_STR, 0);
    setRestrictedStringDefault(GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, L"helvetica");
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 11);
    setRestrictedStringDefault(GO_FONT_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POINTS_STR);
    setRestrictedStringDefault(GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 0, 0, 0);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setStringDefault(GO_INTERPRETER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEX_STR);
    setRestrictedStringDefault(GO_FONT_SMOOTHING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    fnt.setStyleStrategy(QFont::PreferAntialias);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
}
//=============================================================================
void
GOText::paintMe(RenderInterface& gc)
{
    updateState();
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR))
        return;
    GOAxis* axis = getParentAxis();
    if (!axis) {
        return;
    }
    int x, y;
    std::vector<double> pos(findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
    std::vector<double> mapped(axis->reMap(pos, true));
    gc.toPixels(mapped[0], mapped[1], mapped[2], x, y);
    gc.setupDirectDraw();
    double margin(findScalarDoubleProperty(GO_MARGIN_PROPERTY_NAME_STR));
    margin = margin + 1;
    RenderInterface::AlignmentFlag xalign, yalign;
    GOAlignVertProperty* hv
        = static_cast<GOAlignVertProperty*>(findProperty(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR));
    if (hv->isEqual(GO_PROPERTY_VALUE_TOP_STR)) {
        yalign = RenderInterface::Max;
    } else if (hv->isEqual(GO_PROPERTY_VALUE_MIDDLE_STR)) {
        yalign = RenderInterface::Mean;
    } else {
        yalign = RenderInterface::Min;
    }
    GOAlignHorizProperty* hh = static_cast<GOAlignHorizProperty*>(
        findProperty(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR));
    if (hh->isEqual(GO_PROPERTY_VALUE_LEFT_STR)) {
        xalign = RenderInterface::Min;
    } else if (hh->isEqual(GO_PROPERTY_VALUE_CENTER_STR)) {
        xalign = RenderInterface::Mean;
    } else {
        xalign = RenderInterface::Max;
    }
    int textwidth;
    int textheight;
    int textxoffset;
    int textyoffset;
    gc.measureText(text, fnt, xalign, yalign, textwidth, textheight, textxoffset, textyoffset);
    double rotation = findScalarDoubleProperty(GO_ROTATION_PROPERTY_NAME_STR);
    double costheta = cos(-rotation * M_PI / 180.0);
    double sintheta = sin(-rotation * M_PI / 180.0);
    int x1 = (int)(x + (textxoffset - margin) * costheta + (textyoffset - margin) * sintheta);
    int y1 = (int)(y - (textxoffset - margin) * sintheta + (textyoffset - margin) * costheta);
    double hdelx = (textwidth + 2 * margin) * costheta;
    double hdely = -(textwidth + 2 * margin) * sintheta;
    double vdelx = (textheight + 2 * margin) * sintheta;
    double vdely = (textheight + 2 * margin) * costheta;
    GOColorProperty* bc
        = static_cast<GOColorProperty*>(findProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR));
    if (!bc->isNone()) {
        gc.color(bc->data());
        gc.quad(x1, y1, 0, x1 + hdelx, y1 + hdely, 0, x1 + hdelx + vdelx, y1 + hdely + vdely, 0,
            x1 + vdelx, y1 + vdely, 0);
    }
    GOColorProperty* ec
        = static_cast<GOColorProperty*>(findProperty(GO_EDGE_COLOR_PROPERTY_NAME_STR));
    if (!ec->isNone()) {
        gc.color(ec->data());
        gc.setLineStyle(
            (static_cast<GOLineStyleProperty*>(findProperty(GO_LINE_STYLE_PROPERTY_NAME_STR)))
                ->data());
        gc.lineWidth(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
        gc.quadline(x1, y1, 0, x1 + hdelx, y1 + hdely, 0, x1 + hdelx + vdelx, y1 + hdely + vdely, 0,
            x1 + vdelx, y1 + vdely, 0);
    }
    GOColorProperty* tc = static_cast<GOColorProperty*>(findProperty(GO_COLOR_PROPERTY_NAME_STR));
    gc.putText(x, y, text, tc->data(), xalign, yalign, fnt, rotation);
    gc.releaseDirectDraw();
}
//=============================================================================
void
GOText::updateState()
{
    QFont::Style fstyle = QFont::StyleNormal;
    QFont::Weight fweight = QFont::Normal;
    GOStringProperty* fontname = (GOStringProperty*)findProperty(GO_FONT_NAME_PROPERTY_NAME_STR);
    GOFontAngleProperty* fontangle
        = (GOFontAngleProperty*)findProperty(GO_FONT_ANGLE_PROPERTY_NAME_STR);
    GOFontWeightProperty* fontweight
        = (GOFontWeightProperty*)findProperty(GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    GOScalarProperty* fontsize = (GOScalarProperty*)findProperty(GO_FONT_SIZE_PROPERTY_NAME_STR);
    if (fontangle->isEqual(GO_PROPERTY_VALUE_NORMAL_STR)) {
        fstyle = QFont::StyleNormal;
    }
    if (fontangle->isEqual(GO_PROPERTY_VALUE_ITALIC_STR)) {
        fstyle = QFont::StyleItalic;
    }
    if (fontangle->isEqual(GO_PROPERTY_VALUE_OBLIQUE_STR)) {
        fstyle = QFont::StyleOblique;
    }
    if (fontweight->isEqual(GO_PROPERTY_VALUE_NORMAL_STR)) {
        fweight = QFont::Normal;
    }
    if (fontweight->isEqual(GO_PROPERTY_VALUE_BOLD_STR)) {
        fweight = QFont::Bold;
    }
    if (fontweight->isEqual(GO_PROPERTY_VALUE_LIGHT_STR)) {
        fweight = QFont::Light;
    }
    if (fontweight->isEqual(GO_PROPERTY_VALUE_DEMI_STR)) {
        fweight = QFont::DemiBold;
    }
    fnt = QFont(Nelson::wstringToQString(fontname->data()), (int)(fontsize->data()));
    GOOnOffProperty* fontSmoothingProperty
        = static_cast<GOOnOffProperty*>(findProperty(GO_FONT_SMOOTHING_PROPERTY_NAME_STR));
    fnt.setStyleStrategy(
        fontSmoothingProperty->asBool() ? QFont::PreferAntialias : QFont::NoAntialias);
    fnt.setStyle(fstyle);
    fnt.setWeight(fweight);
    GOStringProperty* txt
        = static_cast<GOStringProperty*>(findProperty(GO_STRING_PROPERTY_NAME_STR));
    GOTextInterpreterProperty* textInterpreter
        = static_cast<GOTextInterpreterProperty*>(findProperty(GO_INTERPRETER_PROPERTY_NAME_STR));
    if (textInterpreter->getAsEnum() == TEXT_INTERPRETER_FORMAT::TEX_MARKUP) {
        text = texToUnicode(txt->data());
    } else {
        text = txt->data();
    }
    QFontMetrics fm(fnt);
    QRect sze(fm.boundingRect(wstringToQString(text)));
    setFourVectorDefault(
        GO_BOUNDING_BOX_PROPERTY_NAME_STR, sze.left(), sze.bottom(), sze.width(), sze.height());
}
//=============================================================================
}
//=============================================================================
