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
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QSlider>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QListWidget>
#include <QtGui/QImage>
#include <QtGui/QPainter>
#include <QtGui/QPixmap>
#include "QMultiLineEdit.h"
#include "QStringConverter.hpp"
#include "GOUIControl.h"
#include "GOPropertyValues.hpp"
#include "GOPropertyNames.hpp"
#include "GOColorProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOStringVectorProperty.hpp"
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
#include "GOControlStyleProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "GOHelpers.hpp"
#include "QMultiLineEditWidget.h"
#include "QLineEditWidget.h"
#include "Error.hpp"
#include "i18n.hpp"
#include "QPushButtonWidget.h"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOUIControl::constructProperties()
{
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_CALLBACK_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOStringVectorProperty, GO_STRING_PROPERTY_NAME_STR);
    registerProperty(new GOControlStyleProperty, GO_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_C_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_BACKGROUND_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_FOREGROUND_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty(1.0, (double)std::numeric_limits<int>::max()),
        GO_FONT_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOFontAngleProperty, GO_FONT_ANGLE_PROPERTY_NAME_STR);
    registerProperty(new GOFontWeightProperty, GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    registerProperty(new GOFontUnitsProperty, GO_FONT_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOAlignHorizProperty, GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_ENABLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TOOLTIP_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_MIN_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_MAX_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty(0., (double)std::numeric_limits<int>::max()),
        GO_VALUE_PROPERTY_NAME_STR);
    registerProperty(new GOFixedVectorProperty(2), GO_SLIDER_STEP_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LISTBOX_TOP_PROPERTY_NAME_STR);

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
    setStringDefault(GO_TOOLTIP_PROPERTY_NAME_STR, L"");
    setScalarDoubleDefault(GO_MIN_PROPERTY_NAME_STR, 0);
    setScalarDoubleDefault(GO_MAX_PROPERTY_NAME_STR, 1);
    setScalarDoubleDefault(GO_VALUE_PROPERTY_NAME_STR, 0);
    setTwoVectorDefault(GO_SLIDER_STEP_PROPERTY_NAME_STR, 0.0100, 0.1000);
    setScalarDoubleDefault(GO_LISTBOX_TOP_PROPERTY_NAME_STR, 1);
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
}
//=============================================================================
void
GOUIControl::onPositionPropertyChanged(bool newWidget)
{
    bool needContinue = hasChanged(GO_POSITION_PROPERTY_NAME_STR) || newWidget;
    if (!needContinue) {
        return;
    }
    std::vector<double> sizevec(findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
    QPoint pt((int)sizevec[0], (int)sizevec[1]);
    pt = convertToBottomLeft(pt);
    widget->setGeometry(pt.x(), pt.y(), (int)sizevec[2], (int)sizevec[3]);
    widget->setFixedSize((int)sizevec[2], (int)sizevec[3]);
    clearChanged(GO_POSITION_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOUIControl::onVisiblePropertyChanged(bool newWidget)
{
    if (hasChanged(GO_VISIBLE_PROPERTY_NAME_STR) && !newWidget) {
        return;
    }
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        hide();
    } else {
        show();
    }
    clearChanged(GO_VISIBLE_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOUIControl::onBackgroundPropertyChanged(bool newWidget)
{
    if (!hasChanged(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR) && !newWidget) {
        return;
    }
    GOColorProperty* bc
        = static_cast<GOColorProperty*>(findProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR));

    if (bc && !bc->isNone() && findArrayOfProperty(GO_C_DATA_PROPERTY_NAME_STR).isEmpty()) {
        QString styleSheet;
        std::vector<double> color = bc->data();
        QColor qColor = QColor::fromRgb(color[0] * 255, color[1] * 255, color[2] * 255);
        styleSheet = QString("background-color: %1;").arg(qColor.name());
        widget->setStyleSheet(styleSheet);
    }
    clearChanged(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOUIControl::onFontNameChanged(bool newWidget)
{
    if (!hasChanged(GO_FONT_NAME_PROPERTY_NAME_STR) && !newWidget) {
        return;
    }

    QFont font = widget->font();
    font.setStyleStrategy(QFont::PreferAntialias);
    widget->setFont(font);

    GOStringProperty* fontname
        = static_cast<GOStringProperty*>(findProperty(GO_FONT_NAME_PROPERTY_NAME_STR));
    if (fontname) {
        QString styleSheet;
        styleSheet += QString("font-family: %1;").arg(wstringToQString(fontname->data()));
        widget->setStyleSheet(styleSheet);
    }
    clearChanged(GO_FONT_NAME_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOUIControl::onFontSizeOrUnitChanged(bool newWidget)
{
    if (!hasChanged(GO_FONT_SIZE_PROPERTY_NAME_STR) && !hasChanged(GO_FONT_UNITS_PROPERTY_NAME_STR)
        && !newWidget) {
        return;
    }

    QString styleSheet;
    GOScalarProperty* fSize
        = static_cast<GOScalarProperty*>(findProperty(GO_FONT_SIZE_PROPERTY_NAME_STR));
    if (fSize) {
        double fontSize = fSize->data();
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
        widget->setStyleSheet(styleSheet);
    }
    clearChanged(GO_FONT_SIZE_PROPERTY_NAME_STR);
    clearChanged(GO_FONT_UNITS_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOUIControl::onFontAngleChanged(bool newWidget)
{
    if (!hasChanged(GO_FONT_ANGLE_PROPERTY_NAME_STR) && !newWidget) {
        return;
    }
    GOFontAngleProperty* fontangle
        = (GOFontAngleProperty*)findProperty(GO_FONT_ANGLE_PROPERTY_NAME_STR);
    if (fontangle) {
        QString styleSheet;
        if (fontangle->isEqual(GO_PROPERTY_VALUE_NORMAL_STR)) {
            styleSheet += "font-style: normal;";
        } else if (fontangle->isEqual(GO_PROPERTY_VALUE_ITALIC_STR)) {
            styleSheet += "font-style: italic;";
        } else if (fontangle->isEqual(GO_PROPERTY_VALUE_OBLIQUE_STR)) {
            styleSheet += "font-style: oblique;";
        }
        widget->setStyleSheet(styleSheet);
    }
    clearChanged(GO_FONT_ANGLE_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOUIControl::onFontWeightChanged(bool newWidget)
{
    if (!hasChanged(GO_FONT_WEIGHT_PROPERTY_NAME_STR) && !newWidget) {
        return;
    }
    GOFontWeightProperty* fontweight
        = (GOFontWeightProperty*)findProperty(GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    if (fontweight) {
        QString styleSheet;
        if (fontweight->isEqual(GO_PROPERTY_VALUE_NORMAL_STR)) {
            styleSheet += "font-weight: normal;";
        } else if (fontweight->isEqual(GO_PROPERTY_VALUE_BOLD_STR)) {
            styleSheet += "font-weight: bold;";
        } else if (fontweight->isEqual(GO_PROPERTY_VALUE_LIGHT_STR)) {
            styleSheet += "font-weight: 300;";
        } else if (fontweight->isEqual(GO_PROPERTY_VALUE_DEMI_STR)) {
            styleSheet += "font-weight: 600;";
        }
        widget->setStyleSheet(styleSheet);
    }
    clearChanged(GO_FONT_WEIGHT_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOUIControl::onEnableChanged(bool newWidget)
{
    if (!hasChanged(GO_ENABLE_PROPERTY_NAME_STR) && !newWidget) {
        return;
    }
    if (widget) {
        widget->setEnabled(stringCheck(GO_ENABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR));
    }
    clearChanged(GO_ENABLE_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOUIControl::onHAlignmentChanged(bool newWidget)
{
    if (!hasChanged(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR) && !newWidget) {
        return;
    }

    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEXT_STR)
        || stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EDIT_STR)) {
        GOAlignHorizProperty* textAlign = static_cast<GOAlignHorizProperty*>(
            findProperty(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR));
        if (textAlign) {
            QString styleSheet;
            if (textAlign->isEqual(GO_PROPERTY_VALUE_LEFT_STR)) {
                styleSheet += "text-align: left;";
            } else if (textAlign->isEqual(GO_PROPERTY_VALUE_CENTER_STR)) {
                styleSheet += "text-align: center;";
            } else if (textAlign->isEqual(GO_PROPERTY_VALUE_RIGHT_STR)) {
                styleSheet += "text-align: right;";
            }
            widget->setStyleSheet(styleSheet);
        }
        clearChanged(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
    }
}
//=============================================================================
void
GOUIControl::onEditPropertyChanged(bool newWidget)
{
    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EDIT_STR)
        && (hasChanged(GO_STRING_PROPERTY_NAME_STR) || hasChanged(GO_MIN_PROPERTY_NAME_STR)
            || hasChanged(GO_MAX_PROPERTY_NAME_STR))) {
        deleteWidget();
        clearChanged(GO_MIN_PROPERTY_NAME_STR);
        clearChanged(GO_MAX_PROPERTY_NAME_STR);
        clearChanged(GO_STRING_PROPERTY_NAME_STR);
        GOGenericProperty* hp = findProperty(GO_STYLE_PROPERTY_NAME_STR);
        hp->setModified(true);
        updateState();
    }
}
//=============================================================================
void
GOUIControl::onToggleButtonChanged(bool newWidget)
{
    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOGGLEBUTTON_STR)
        && (hasChanged(GO_VALUE_PROPERTY_NAME_STR) || hasChanged(GO_MIN_PROPERTY_NAME_STR)
            || hasChanged(GO_MAX_PROPERTY_NAME_STR) || newWidget)) {
        double min(findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR));
        double max(findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR));
        double value(findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR));
        if (value == min) {
            ((QPushButton*)widget)->setChecked(false);
        } else if (value == max) {
            ((QPushButton*)widget)->setChecked(true);
        }
        clearChanged(GO_MIN_PROPERTY_NAME_STR);
        clearChanged(GO_MAX_PROPERTY_NAME_STR);
        clearChanged(GO_VALUE_PROPERTY_NAME_STR);
    }
}
//=============================================================================
void
GOUIControl::onSliderChanged(bool newWidget)
{
    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SLIDER_STR)
        && (hasChanged(GO_MIN_PROPERTY_NAME_STR) || hasChanged(GO_MAX_PROPERTY_NAME_STR)
            || hasChanged(GO_SLIDER_STEP_PROPERTY_NAME_STR) || newWidget)) {
        double min(findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR));
        double max(findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR));
        std::vector<double> steps(findVectorDoubleProperty(GO_SLIDER_STEP_PROPERTY_NAME_STR));
        ((QSlider*)widget)->setMinimum(0);
        ((QSlider*)widget)->setMaximum((int)((max - min) / steps[0]));
        ((QSlider*)widget)->setSingleStep(1);
        ((QSlider*)widget)->setPageStep((int)(steps[1]));
        if (newWidget) {
            if (hasChanged(GO_VALUE_PROPERTY_NAME_STR)) {
                double value(findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR));
                ((QSlider*)widget)->setValue((int)((value - min) / steps[0]));
                clearChanged(GO_VALUE_PROPERTY_NAME_STR);
            } else {
                findProperty(GO_VALUE_PROPERTY_NAME_STR)
                    ->set(ArrayOf::doubleConstructor(min + steps[0] * ((QSlider*)widget)->value()));
            }
        } else {
            findProperty(GO_VALUE_PROPERTY_NAME_STR)
                ->set(ArrayOf::doubleConstructor(min + steps[0] * ((QSlider*)widget)->value()));
        }
        clearChanged(GO_MIN_PROPERTY_NAME_STR);
        clearChanged(GO_MAX_PROPERTY_NAME_STR);
        clearChanged(GO_SLIDER_STEP_PROPERTY_NAME_STR);
    }
}
//=============================================================================
void
GOUIControl::onCheckBoxChanged(bool newWidget)
{
    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CHECKBOX_STR)
        && (hasChanged(GO_VALUE_PROPERTY_NAME_STR) || hasChanged(GO_MIN_PROPERTY_NAME_STR)
            || hasChanged(GO_MAX_PROPERTY_NAME_STR) || newWidget)) {
        double min(findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR));
        double max(findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR));
        double value(findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR));
        if (value == min) {
            ((QCheckBox*)widget)->setChecked(false);
        } else if (value == max) {
            ((QCheckBox*)widget)->setChecked(true);
        }
        clearChanged(GO_MIN_PROPERTY_NAME_STR);
        clearChanged(GO_MAX_PROPERTY_NAME_STR);
        clearChanged(GO_VALUE_PROPERTY_NAME_STR);
    }
}
//=============================================================================
void
GOUIControl::onRadioButtonChanged(bool newWidget)
{
    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIOBUTTON_STR)
        && (hasChanged(GO_VALUE_PROPERTY_NAME_STR) || hasChanged(GO_MIN_PROPERTY_NAME_STR)
            || hasChanged(GO_MAX_PROPERTY_NAME_STR) || newWidget)) {
        double min(findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR));
        double max(findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR));
        double value(findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR));
        if (value == min) {
            ((QRadioButton*)widget)->setChecked(false);
        } else if (value == max) {
            ((QRadioButton*)widget)->setChecked(true);
        }
        clearChanged(GO_MIN_PROPERTY_NAME_STR);
        clearChanged(GO_MAX_PROPERTY_NAME_STR);
        clearChanged(GO_VALUE_PROPERTY_NAME_STR);
    }
}
//=============================================================================
void
GOUIControl::onToolTipsChanged(bool newWidget)
{
    if (hasChanged(GO_TOOLTIP_PROPERTY_NAME_STR) || newWidget) {
        std::wstring str = findStringProperty(GO_TOOLTIP_PROPERTY_NAME_STR);
        widget->setToolTip(wstringToQString(str));
        clearChanged(GO_TOOLTIP_PROPERTY_NAME_STR);
    }
}
//=============================================================================
void
GOUIControl::onListTopBoxChanged(bool newWidget)
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LISTBOX_STR)) {
        return;
    }
    if (hasChanged(GO_STRING_PROPERTY_NAME_STR)) {
        ((QListWidget*)widget)->clear();
        ArrayOf value = findProperty(GO_STRING_PROPERTY_NAME_STR)->get();
        wstringVector values;
        if (value.isScalarStringArray()) {
            tokenize(value.getContentAsWideString(), values, L"|");
        } else if (value.isStringArray()) {
            values = value.getContentAsWideStringVector();
        } else if (value.isCellArrayOfCharacterVectors()) {
            values = value.getContentAsWideStringVector();
        } else if (value.isCharacterArray()) {
            tokenize(value.getContentAsWideString(), values, L"|");
        }
        for (size_t k = 0; k < values.size(); k++) {
            ((QListWidget*)widget)->addItem(wstringToQString(values[k]));
        }
        if (values.size() > 0) {
            ((QListWidget*)widget)->setCurrentRow((int)1);
        }
        if (!hasChanged(GO_VALUE_PROPERTY_NAME_STR) && !values.empty()
            && findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR) == 0) {
            setScalarDoubleDefault(GO_VALUE_PROPERTY_NAME_STR, 1);
        }
    }

    if (hasChanged(GO_MIN_PROPERTY_NAME_STR) || hasChanged(GO_MAX_PROPERTY_NAME_STR) || newWidget) {
        double min(findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR));
        double max(findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR));
        if (max - min > 1) {
            ((QListWidget*)widget)->setSelectionMode(QAbstractItemView::MultiSelection);
        } else {

            ((QListWidget*)widget)->setSelectionMode(QAbstractItemView::SingleSelection);
        }
        clearChanged(GO_MIN_PROPERTY_NAME_STR);
        clearChanged(GO_MAX_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_LISTBOX_TOP_PROPERTY_NAME_STR) || newWidget) {
        int listTop((int)findScalarDoubleProperty(GO_LISTBOX_TOP_PROPERTY_NAME_STR));
        ((QListWidget*)widget)
            ->scrollToItem(
                ((QListWidget*)widget)->item(listTop - 1), QAbstractItemView::PositionAtTop);
        clearChanged(GO_LISTBOX_TOP_PROPERTY_NAME_STR);
    }

    if (hasChanged(GO_VALUE_PROPERTY_NAME_STR)) {
        double v = findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR);
        ((QListWidget*)widget)->setCurrentRow((int)v - 1);

        clearChanged(GO_VALUE_PROPERTY_NAME_STR);
    }
}
//=============================================================================
void
GOUIControl::onPopupMenuChanged(bool newWidget)
{
    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POPUPMENU_STR)) {
        ((QComboBox*)widget)->clear();
        ArrayOf value = findProperty(GO_STRING_PROPERTY_NAME_STR)->get();
        wstringVector values;
        if (value.isScalarStringArray()) {
            tokenize(value.getContentAsWideString(), values, L"|");
        } else if (value.isStringArray()) {
            values = value.getContentAsWideStringVector();
        } else if (value.isCellArrayOfCharacterVectors()) {
            values = value.getContentAsWideStringVector();
        } else if (value.isCharacterArray()) {
            tokenize(value.getContentAsWideString(), values, L"|");
        }
        for (size_t k = 0; k < values.size(); k++) {
            ((QComboBox*)widget)->addItem(wstringToQString(values[k]));
        }

        double indexPosition = findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR);

        if (!values.empty() && indexPosition == 0) {
            setScalarDoubleDefault(GO_VALUE_PROPERTY_NAME_STR, 1);
            indexPosition = 1;
        }
        ((QComboBox*)widget)->setCurrentIndex((int)(indexPosition - 1));
    }
}
//=============================================================================
void
GOUIControl::onCDataPropertyChanged(bool newWidget)
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PUSHBUTTON_STR)) {
        return;
    }
    if (newWidget || hasChanged(GO_C_DATA_PROPERTY_NAME_STR)) {
        ArrayOf cdata(findArrayOfProperty(GO_C_DATA_PROPERTY_NAME_STR));
        if (cdata.isEmpty()) {
            QPushButtonWidget* button = static_cast<QPushButtonWidget*>(widget);
            QImage img;
            button->setImage(img);
            clearChanged(GO_C_DATA_PROPERTY_NAME_STR);
            return;
        }
        bool isValid = (cdata.getDimensions().getLength() == 3)
            && (cdata.getDimensionLength(2) == 3) && cdata.isNumeric();
        if (!isValid) {
            Error(_("Invalid image format: expected a 3D array with 3 color channels."));
        }

        QPushButtonWidget* button = static_cast<QPushButtonWidget*>(widget);
        QImage img;
        if (cdata.getDataClass() == NLS_UINT8) {
            bool isValid
                = (cdata.getDimensions().getLength() == 3) && (cdata.getDimensionLength(2) == 3);
            if (!isValid) {
                Error(_("Invalid image format: expected a 3D array with 3 color channels."));
            }
            indexType rows = cdata.getRows();
            indexType cols = cdata.getColumns();
            uint8* dp = (uint8*)cdata.getDataPointer();
            img = QImage((int)cols, (int)rows, QImage::Format_ARGB32);
            for (indexType i = 0; i < rows; i++) {
                QRgb* ibits = (QRgb*)img.scanLine((int)i);
                for (indexType j = 0; j < cols; j++) {
                    int r = (int)(dp[(i + j * rows)]);
                    int g = (int)(dp[(i + j * rows) + rows * cols]);
                    int b = (int)(dp[(i + j * rows) + 2 * rows * cols]);
                    int a = (int)(255);
                    ibits[j] = qRgba(r, g, b, a);
                }
            }
        } else {
            cdata.promoteType(NLS_DOUBLE);
            double* dp = (double*)cdata.getDataPointer();
            bool isValid = true;
            for (indexType k = 0; k < cdata.getElementCount(); ++k) {
                if (dp[k] < 0.0 || dp[k] > 1.0) {
                    isValid = false;
                    break;
                }
            }
            if (!isValid) {
                Error(_("Invalid image format: array with values in [0. 1.] expected."));
            }
            indexType rows = cdata.getRows();
            indexType cols = cdata.getColumns();

            img = QImage((int)cols, (int)rows, QImage::Format_ARGB32);
            for (indexType i = 0; i < rows; i++) {
                QRgb* ibits = (QRgb*)img.scanLine((int)i);
                for (indexType j = 0; j < cols; j++) {
                    int r = (int)(dp[(i + j * rows)] * 255);
                    int g = (int)(dp[(i + j * rows) + rows * cols] * 255);
                    int b = (int)(dp[(i + j * rows) + 2 * rows * cols] * 255);
                    int a = 255;
                    ibits[j] = qRgba(r, g, b, a);
                }
            }
        }
        clearChanged(GO_C_DATA_PROPERTY_NAME_STR);
        button->setImage(img);
        button->repaint();
    }
}
//=============================================================================
bool
GOUIControl::onStylePropertyChanged()
{
    bool createWidget = false;
    if (hasChanged(GO_STYLE_PROPERTY_NAME_STR)) {
        deleteWidget();
        createPushButton();
        createToggleButton();
        createRadioButton();
        createCheckBox();
        createSlider();
        createText();
        createEdit();
        createListBox();
        createPopupMenu();

        if (widget) {
            widget->show();
            widget->raise();
            createWidget = true;
            clearChanged(GO_STYLE_PROPERTY_NAME_STR);
        }
    }
    return createWidget;
}
//=============================================================================
void
GOUIControl::onStringChangedPushButton(bool newWidget)
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PUSHBUTTON_STR)) {
        return;
    }
    if (!hasChanged(GO_STRING_PROPERTY_NAME_STR) && !newWidget) {
        return;
    }
    wstringVector vstr = findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
    std::wstring str = vstr.size() > 0 ? vstr[0] : L"";
    ((QPushButtonWidget*)widget)->setText(str);
    clearChanged(GO_STRING_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOUIControl::onStringChangedToggleButton(bool newWidget)
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOGGLEBUTTON_STR)) {
        return;
    }
    if (!hasChanged(GO_STRING_PROPERTY_NAME_STR) && !newWidget) {
        return;
    }
    wstringVector vstr = findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
    std::wstring str = vstr.size() > 0 ? vstr[0] : L"";
    ((QPushButton*)widget)->setText(wstringToQString(str));
    clearChanged(GO_STRING_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOUIControl::onStringChangedRadioButton(bool newWidget)
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIOBUTTON_STR)) {
        return;
    }
    if (!hasChanged(GO_STRING_PROPERTY_NAME_STR) && !newWidget) {
        return;
    }
    wstringVector vstr = findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
    std::wstring str = vstr.size() > 0 ? vstr[0] : L"";
    ((QRadioButton*)widget)->setText(wstringToQString(str));
    clearChanged(GO_STRING_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOUIControl::onStringChangedCheckBox(bool newWidget)
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CHECKBOX_STR)) {
        return;
    }
    if (!hasChanged(GO_STRING_PROPERTY_NAME_STR) && !newWidget) {
        return;
    }
    wstringVector vstr = findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
    std::wstring str = vstr.size() > 0 ? vstr[0] : L"";
    ((QCheckBox*)widget)->setText(wstringToQString(str));
    clearChanged(GO_STRING_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOUIControl::onStringChangedText(bool newWidget)
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEXT_STR)) {
        return;
    }
    if (!hasChanged(GO_STRING_PROPERTY_NAME_STR) && !newWidget) {
        return;
    }
    wstringVector vstr = findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
    std::wstring str = vstr.size() > 0 ? vstr[0] : L"";
    ((QLabel*)widget)->setText(wstringToQString(str));
    clearChanged(GO_STRING_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOUIControl::onStringChangedEdit(bool newWidget)
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EDIT_STR)) {
        return;
    }
    if (!hasChanged(GO_STRING_PROPERTY_NAME_STR) && !newWidget) {
        return;
    }
    wstringVector vstr = findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
    if (widget->objectName() == "QMultiLineEditWidget") {
        std::wstring line;
        for (size_t i = 0; i < vstr.size(); ++i) {
            line += vstr[i];
            if (i < vstr.size() - 1) {
                line += L'\n';
            }
        }
        ((QMultiLineEditWidget*)widget)->setPlainText(wstringToQString(line));
    } else {
        std::wstring str = vstr.size() > 0 ? vstr[0] : L"";
        ((QLineEditWidget*)widget)->setText(wstringToQString(str));
    }
    clearChanged(GO_STRING_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOUIControl::onStringChangedListBox(bool newWidget)
{
    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LISTBOX_STR)
        && (hasChanged(GO_STRING_PROPERTY_NAME_STR) || newWidget)) {
        ((QListWidget*)widget)->clear();
        ArrayOf value = findProperty(GO_STRING_PROPERTY_NAME_STR)->get();
        wstringVector values;
        if (value.isScalarStringArray()) {
            tokenize(value.getContentAsWideString(), values, L"|");
        } else if (value.isStringArray()) {
            values = value.getContentAsWideStringVector();
        } else if (value.isCellArrayOfCharacterVectors()) {
            values = value.getContentAsWideStringVector();
        } else if (value.isCharacterArray()) {
            tokenize(value.getContentAsWideString(), values, L"|");
        }
        for (size_t k = 0; k < values.size(); k++) {
            ((QListWidget*)widget)->addItem(wstringToQString(values[k]));
        }
        if (values.size() > 0) {
            setScalarDoubleDefault(GO_VALUE_PROPERTY_NAME_STR, 1);
            ((QListWidget*)widget)->setCurrentRow(0);
        }
        clearChanged(GO_STRING_PROPERTY_NAME_STR);
    }
}
//=============================================================================
void
GOUIControl::onStringChangedPopupMenu(bool newWidget)
{
    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POPUPMENU_STR)
        && (hasChanged(GO_STRING_PROPERTY_NAME_STR) || newWidget)) {
        ((QComboBox*)widget)->clear();
        ArrayOf value = findProperty(GO_STRING_PROPERTY_NAME_STR)->get();
        wstringVector values;
        if (value.isScalarStringArray()) {
            tokenize(value.getContentAsWideString(), values, L"|");
        } else if (value.isStringArray()) {
            values = value.getContentAsWideStringVector();
        } else if (value.isCellArrayOfCharacterVectors()) {
            values = value.getContentAsWideStringVector();
        } else if (value.isCharacterArray()) {
            tokenize(value.getContentAsWideString(), values, L"|");
        }
        for (size_t k = 0; k < values.size(); k++) {
            ((QComboBox*)widget)->addItem(wstringToQString(values[k]));
        }
        if (!hasChanged(GO_VALUE_PROPERTY_NAME_STR)) {
            findProperty(GO_VALUE_PROPERTY_NAME_STR)->setModified(true);
        }
    }
}
//=============================================================================
void
GOUIControl::onStringPropertyChanged(bool newWidget)
{
    onStringChangedPushButton(newWidget);
    onStringChangedToggleButton(newWidget);
    onStringChangedRadioButton(newWidget);
    onStringChangedCheckBox(newWidget);
    onStringChangedText(newWidget);
    onStringChangedEdit(newWidget);
    onStringChangedListBox(newWidget);
    onStringChangedPopupMenu(newWidget);
}
//=============================================================================
void
GOUIControl::onParentPositionChanged(bool newWidget)
{
    bool mustChange
        = parentGoWindow->getGOFigure()->hasChanged(GO_POSITION_PROPERTY_NAME_STR) || newWidget;
    if (!mustChange) {
        return;
    }
    std::vector<double> sizevec(findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
    QPoint pt((int)sizevec[0], (int)sizevec[1]);
    pt = convertToBottomLeft(pt);
    widget->setGeometry(pt.x(), pt.y(), (int)sizevec[2], (int)sizevec[3]);
}
//=============================================================================
void
GOUIControl::onValueChanged(bool newWidget)
{
    if (!widget) {
        return;
    }
    if (hasChanged(GO_VALUE_PROPERTY_NAME_STR) || newWidget) {
        if (widget->objectName() == "QSliderWidget") {
            double min(findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR));
            std::vector<double> steps(findVectorDoubleProperty(GO_SLIDER_STEP_PROPERTY_NAME_STR));
            double value(findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR));
            ((QSlider*)widget)->setValue((int)((value - min) / steps[0]));
        }
        if (widget->objectName() == "QListBoxWigdet") {
            double value(findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR));
            ((QListWidget*)widget)->setCurrentRow((int)value - 1);
        }

        clearChanged(GO_VALUE_PROPERTY_NAME_STR);
    }
}
//=============================================================================
}
//=============================================================================
