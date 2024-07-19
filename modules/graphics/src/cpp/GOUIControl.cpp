//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QPushButton>
#include <QtWidgets/QRadioButton>
#include "QStringConverter.hpp"
#include "GOUIControl.h"
#include "GOPropertyValues.hpp"
#include "GOPropertyNames.hpp"
#include "GOColorProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOFontAngleProperty.hpp"
#include "GOFontUnitsProperty.hpp"
#include "GOFontWeightProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOAlignHorizProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOVectorProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "GOHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOUIControl::GOUIControl()
{
    widget = nullptr;
    constructProperties();
    setupDefaults();
}
//=============================================================================
GOUIControl::~GOUIControl()
{
    if (widget) {
        delete widget;
        widget = nullptr;
    }
}
//=============================================================================
std::wstring
GOUIControl::getType()
{
    return GO_PROPERTY_VALUE_UICONTROL_STR;
}
//=============================================================================
void
GOUIControl::updateState()
{
    if (hasChanged(GO_VISIBLE_PROPERTY_NAME_STR) && widget) {
        if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
            widget->hide();
        } else {
            widget->show();
        }
    }

    bool createWidget = false;
    if (hasChanged(GO_STYLE_PROPERTY_NAME_STR)) {
        if (widget) {
            delete widget;
            widget = nullptr;
            disconnect(widget, SIGNAL(clicked()), nullptr, nullptr);
        }
        if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PUSHBUTTON_STR)) {
            widget = new QPushButton(parentWidget);
            connect(widget, SIGNAL(clicked()), this, SLOT(clicked()));
        }

        if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIOBUTTON_STR)) {
            widget = new QRadioButton(parentWidget);
            ((QRadioButton*)widget)->setAutoExclusive(false);
            connect(widget, SIGNAL(clicked()), this, SLOT(clicked()));
        }

        if (widget) {
            widget->show();
            createWidget = true;
        }
    }

    if (hasChanged(GO_STRING_PROPERTY_NAME_STR) || createWidget) {
        if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PUSHBUTTON_STR)) {
            std::wstring str = findStringProperty(GO_STRING_PROPERTY_NAME_STR);
            ((QPushButton*)widget)->setText(wstringToQString(str));
        }
        if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIOBUTTON_STR)) {
            std::wstring str = findStringProperty(GO_STRING_PROPERTY_NAME_STR);
            ((QRadioButton*)widget)->setText(wstringToQString(str));
        }
    }
    if (hasChanged(GO_POSITION_PROPERTY_NAME_STR) || createWidget) {
        std::vector<double> sizevec(findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
        QPoint pt((int)sizevec[0], (int)sizevec[1]);
        pt = convertToBottomLeft(pt);
        widget->setGeometry(pt.x(), pt.y(), (int)sizevec[2], (int)sizevec[3]);
        clearChanged(GO_POSITION_PROPERTY_NAME_STR);
    }

    if (parentWidget->getGOFigure()->hasChanged(GO_POSITION_PROPERTY_NAME_STR)) {
        std::vector<double> sizevec(findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
        QPoint pt((int)sizevec[0], (int)sizevec[1]);
        pt = convertToBottomLeft(pt);
        widget->setGeometry(pt.x(), pt.y(), (int)sizevec[2], (int)sizevec[3]);
    }

    GOColorProperty* bc
        = static_cast<GOColorProperty*>(findProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR));

    QString styleSheet;
    if (bc && !bc->isNone()) {
        std::vector<double> color = bc->data();
        QColor qColor = QColor::fromRgb(color[0] * 255, color[1] * 255, color[2] * 255);
        styleSheet = QString("background-color: %1;").arg(qColor.name());
    }

    GOColorProperty* fc
        = static_cast<GOColorProperty*>(findProperty(GO_FOREGROUND_COLOR_PROPERTY_NAME_STR));
    if (fc && !fc->isNone()) {
        std::vector<double> color = fc->data();
        QColor qColor = QColor::fromRgb(color[0] * 255, color[1] * 255, color[2] * 255);
        styleSheet += QString("color: %1;").arg(qColor.name());
    }

    GOScalarProperty* fSize
        = static_cast<GOScalarProperty*>(findProperty(GO_FONT_SIZE_PROPERTY_NAME_STR));
    if (fSize) {
        int fontSize = (int)fSize->data();
        GOFontUnitsProperty* fUnits
            = static_cast<GOFontUnitsProperty*>(findProperty(GO_FONT_UNITS_PROPERTY_NAME_STR));
        if (fUnits) {
            std::wstring unit = fUnits->data();
            if (unit == GO_PROPERTY_VALUE_PIXELS_STR) {
                styleSheet += QString("font-size: %1px;").arg(fontSize);
            } else if (unit == GO_PROPERTY_VALUE_POINTS_STR) {
                styleSheet += QString("font-size: %1pt;").arg(fontSize);
            } else if (unit == GO_PROPERTY_VALUE_CENTIMETERS_STR) {
                // Convert cm to points (1 cm = 28.3465 points)
                int fontSizeInPoints = fontSize * 28.3465;
                styleSheet += QString("font-size: %1pt;").arg(fontSizeInPoints);
            } else if (unit == GO_PROPERTY_VALUE_INCHES_STR) {
                // Convert inches to points (1 inch = 72 points)
                int fontSizeInPoints = fontSize * 72;
                styleSheet += QString("font-size: %1pt;").arg(fontSizeInPoints);
            } else {
                styleSheet += "px;";
            }
        }
    }
    GOStringProperty* fontname
        = static_cast<GOStringProperty*>(findProperty(GO_FONT_NAME_PROPERTY_NAME_STR));
    if (fontname) {
        styleSheet += QString("font-family: %1;").arg(wstringToQString(fontname->data()));
    }

    GOFontAngleProperty* fontangle
        = (GOFontAngleProperty*)findProperty(GO_FONT_ANGLE_PROPERTY_NAME_STR);
    if (fontangle) {
        if (fontangle->isEqual(GO_PROPERTY_VALUE_NORMAL_STR)) {
            styleSheet += "font-style: normal;";
        } else if (fontangle->isEqual(GO_PROPERTY_VALUE_ITALIC_STR)) {
            styleSheet += "font-style: italic;";
        } else if (fontangle->isEqual(GO_PROPERTY_VALUE_OBLIQUE_STR)) {
            styleSheet += "font-style: oblique;";
        }
    }

    GOFontWeightProperty* fontweight
        = (GOFontWeightProperty*)findProperty(GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    if (fontweight) {
        if (fontweight->isEqual(GO_PROPERTY_VALUE_NORMAL_STR)) {
            styleSheet += "font-weight: normal;";
        } else if (fontweight->isEqual(GO_PROPERTY_VALUE_BOLD_STR)) {
            styleSheet += "font-weight: bold;";
        } else if (fontweight->isEqual(GO_PROPERTY_VALUE_LIGHT_STR)) {
            styleSheet += "font-weight: 300;";
        } else if (fontweight->isEqual(GO_PROPERTY_VALUE_DEMI_STR)) {
            styleSheet += "font-weight: 600;";
        }
    }

    GOAlignHorizProperty* textAlign = static_cast<GOAlignHorizProperty*>(
        findProperty(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR));
    if (textAlign) {
        if (textAlign->isEqual(GO_PROPERTY_VALUE_LEFT_STR)) {
            styleSheet += "text-align: left;";
        } else if (textAlign->isEqual(GO_PROPERTY_VALUE_CENTER_STR)) {
            styleSheet += "text-align: center;";
        } else if (textAlign->isEqual(GO_PROPERTY_VALUE_RIGHT_STR)) {
            styleSheet += "text-align: right;";
        }
    }

    if (!styleSheet.isEmpty() && widget) {
        widget->setStyleSheet(styleSheet);
    }
    if (widget) {
        widget->setEnabled(stringCheck(GO_ENABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR));
    }

    if (hasChanged(GO_TOOLTIPS_STRING_PROPERTY_NAME_STR) || widget) {
        std::wstring str = findStringProperty(GO_TOOLTIPS_STRING_PROPERTY_NAME_STR);
        widget->setToolTip(wstringToQString(str));
        clearChanged(GO_TOOLTIPS_STRING_PROPERTY_NAME_STR);
    }
}
//=============================================================================
void
GOUIControl::constructProperties()
{
    registerProperty(new GOCallbackProperty, GO_CALLBACK_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_STRING_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_C_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_BACKGROUND_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_FOREGROUND_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_FONT_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOFontAngleProperty, GO_FONT_ANGLE_PROPERTY_NAME_STR);
    registerProperty(new GOFontWeightProperty, GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    registerProperty(new GOFontUnitsProperty, GO_FONT_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOAlignHorizProperty, GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_ENABLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TOOLTIPS_STRING_PROPERTY_NAME_STR);

    sortProperties();
}
//=============================================================================
void
GOUIControl::setupDefaults()
{
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 20, 20, 60, 20);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setThreeVectorDefault(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, 0.9400, 0.9400, 0.9400);
    setThreeVectorDefault(GO_FOREGROUND_COLOR_PROPERTY_NAME_STR, 0, 0, 0);
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 8);
    setStringDefault(GO_FONT_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POINTS_STR);
#ifdef _MSC_VER
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, L"MS Sans Serif");
#else
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, L"Helvetica");
#endif
    setStringDefault(GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setStringDefault(GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(
        GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CENTER_STR);
    setRestrictedStringDefault(GO_ENABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PUSHBUTTON_STR);
    setStringDefault(GO_TOOLTIPS_STRING_PROPERTY_NAME_STR, L"");
}
//=============================================================================
void
GOUIControl::paintMe(RenderInterface& gc)
{
}
//=============================================================================
void
GOUIControl::buildWidget(GOWindow* f)
{
    parentWidget = f;
}
//=============================================================================
QPoint
GOUIControl::convertToBottomLeft(const QPoint& topLeftPos)
{
    int x = topLeftPos.x();
    int y = parentWidget->height() - topLeftPos.y() - widget->height();
    return QPoint(x, y);
}
//=============================================================================
QPoint
GOUIControl::convertToTopLeft(const QPoint& bottomLeftPos)
{
    int x = bottomLeftPos.x();
    int y = parentWidget->height() - bottomLeftPos.y() - widget->height();
    return QPoint(x, y);
}
//=============================================================================
void
GOUIControl::clicked()
{
    if (!widget || !getEvaluator()) {
        return;
    }
    GOCallbackProperty* goCallback
        = (GOCallbackProperty*)findProperty(GO_CALLBACK_PROPERTY_NAME_STR);
    if (goCallback) {
        goCallback->pushEvent(this, L"ActionData", L"Action");
    }
}
//=============================================================================
}
//=============================================================================
