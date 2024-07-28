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
#include "AnonymousMacroFunctionDef.hpp"
#include "GOHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
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
GOUIControl::~GOUIControl() { deleteWidget(); }
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
            hide();
        } else {
            show();
        }
        clearChanged(GO_VISIBLE_PROPERTY_NAME_STR);
    }

    bool createWidget = false;
    if (hasChanged(GO_STYLE_PROPERTY_NAME_STR)) {
        deleteWidget();
        if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PUSHBUTTON_STR)) {
            widget = new QPushButton(getParentWidget());
            connect(widget, SIGNAL(clicked()), this, SLOT(clicked()));
            ((QPushButton*)widget)->setFlat(false);
            ((QPushButton*)widget)->setStyleSheet(QString("QPushButton {border: 0px;}"));

            ArrayOf cdata(findArrayOfProperty(GO_C_DATA_PROPERTY_NAME_STR));
            if (!cdata.isEmpty() && cdata.getDataClass() == NLS_UINT8) {
                if ((cdata.getDimensions().getLength() == 3)
                    && (cdata.getDimensionLength(2) == 3)) {
                    indexType rows = cdata.getRows();
                    indexType cols = cdata.getColumns();
                    uint8* dp = (uint8*)cdata.getDataPointer();
                    QImage img = QImage((int)cols, (int)rows, QImage::Format_ARGB32);
                    for (indexType i = 0; i < rows; i++) {
                        QRgb* ibits = (QRgb*)img.scanLine((int)i);
                        for (indexType j = 0; j < cols; j++) {
                            ibits[j] = qRgba((int)(dp[(i + j * rows)]),
                                (int)(dp[(i + j * rows) + rows * cols]),
                                (int)(dp[(i + j * rows) + 2 * rows * cols]), (int)(255 * 1));
                        }
                    }
                    QPixmap pixMap = QPixmap::fromImage(img);
                    QPalette palette;
                    palette.setBrush(((QPushButton*)widget)->backgroundRole(), QBrush(pixMap));
                    ((QPushButton*)widget)->setAutoFillBackground(true);
                    ((QPushButton*)widget)->setPalette(palette);
                } else {
                    Error(_("Wrong image format."));
                }
            }
        }

        if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOGGLEBUTTON_STR)) {
            widget = new QPushButton(getParentWidget());
            ((QPushButton*)widget)->setCheckable(true);
            connect(widget, SIGNAL(clicked()), this, SLOT(clicked()));
        }

        if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIOBUTTON_STR)) {
            widget = new QRadioButton(getParentWidget());
            ((QRadioButton*)widget)->setAutoExclusive(false);
            connect(widget, SIGNAL(clicked()), this, SLOT(clicked()));
        }

        if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CHECKBOX_STR)) {
            widget = new QCheckBox(getParentWidget());
            connect(widget, SIGNAL(clicked()), this, SLOT(clicked()));
        }

        if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SLIDER_STR)) {
            std::vector<double> sizevec(findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
            if (sizevec[3] > sizevec[2]) {
                widget = new QSlider(Qt::Vertical, getParentWidget());
            } else {
                widget = new QSlider(Qt::Horizontal, getParentWidget());
            }
            ((QSlider*)widget)->setTracking(false);
            connect(widget, SIGNAL(valueChanged(int)), this, SLOT(clicked()));
        }

        if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEXT_STR)) {
            widget = new QLabel(getParentWidget());
            ((QLabel*)widget)->setWordWrap(true);
        }
        if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EDIT_STR)) {
            double min(findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR));
            double max(findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR));
            if ((max - min) > 1) {
                widget = new QMultiLineEdit(getParentWidget());
                widget->setObjectName("multiLine");
            } else {
                widget = new QLineEdit(getParentWidget());
                widget->setObjectName("singleLine");
            }
            connect(widget, SIGNAL(editingFinished()), this, SLOT(clicked()));
        }

        if (widget) {
            widget->show();
            widget->raise();
            createWidget = true;
            clearChanged(GO_STYLE_PROPERTY_NAME_STR);
        }
    }
    if (!widget) {
        clearAllChanged();
        return;
    }

    if (hasChanged(GO_STRING_PROPERTY_NAME_STR) || createWidget) {
        if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PUSHBUTTON_STR)) {
            wstringVector vstr = findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
            std::wstring str = vstr.size() > 0 ? vstr[0] : L"";
            ((QPushButton*)widget)->setText(wstringToQString(str));
        }
        if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOGGLEBUTTON_STR)) {
            wstringVector vstr = findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
            std::wstring str = vstr.size() > 0 ? vstr[0] : L"";
            ((QPushButton*)widget)->setText(wstringToQString(str));
        }
        if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIOBUTTON_STR)) {
            wstringVector vstr = findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
            std::wstring str = vstr.size() > 0 ? vstr[0] : L"";
            ((QRadioButton*)widget)->setText(wstringToQString(str));
        }
        if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CHECKBOX_STR)) {
            wstringVector vstr = findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
            std::wstring str = vstr.size() > 0 ? vstr[0] : L"";
            ((QCheckBox*)widget)->setText(wstringToQString(str));
        }
        if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEXT_STR)) {
            wstringVector vstr = findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
            std::wstring str = vstr.size() > 0 ? vstr[0] : L"";
            ((QLabel*)widget)->setText(wstringToQString(str));
        }
        if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EDIT_STR)) {
            wstringVector vstr = findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
            std::wstring str = vstr.size() > 0 ? vstr[0] : L"";
            if (widget->objectName() == "multiLine") {
                ((QMultiLineEdit*)widget)->setPlainText(wstringToQString(str));
            } else {
                ((QLineEdit*)widget)->setText(wstringToQString(str));
            }
        }
        clearChanged(GO_STRING_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_POSITION_PROPERTY_NAME_STR) || createWidget) {
        std::vector<double> sizevec(findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
        QPoint pt((int)sizevec[0], (int)sizevec[1]);
        pt = convertToBottomLeft(pt);
        widget->setGeometry(pt.x(), pt.y(), (int)sizevec[2], (int)sizevec[3]);
        clearChanged(GO_POSITION_PROPERTY_NAME_STR);
    }

    if (parentGoWindow->getGOFigure()->hasChanged(GO_POSITION_PROPERTY_NAME_STR)) {
        std::vector<double> sizevec(findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
        QPoint pt((int)sizevec[0], (int)sizevec[1]);
        pt = convertToBottomLeft(pt);
        widget->setGeometry(pt.x(), pt.y(), (int)sizevec[2], (int)sizevec[3]);
    }

    GOColorProperty* bc
        = static_cast<GOColorProperty*>(findProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR));

    QString styleSheet;
    if (bc && !bc->isNone() && findArrayOfProperty(GO_C_DATA_PROPERTY_NAME_STR).isEmpty()) {
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

    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEXT_STR)
        || stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EDIT_STR)) {
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
    }

    if (!styleSheet.isEmpty() && widget) {
        widget->setStyleSheet(styleSheet);
    }
    if (widget) {
        widget->setEnabled(stringCheck(GO_ENABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR));
    }

    if (hasChanged(GO_TOOLTIPS_STRING_PROPERTY_NAME_STR) || createWidget) {
        std::wstring str = findStringProperty(GO_TOOLTIPS_STRING_PROPERTY_NAME_STR);
        widget->setToolTip(wstringToQString(str));
        clearChanged(GO_TOOLTIPS_STRING_PROPERTY_NAME_STR);
    }

    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIOBUTTON_STR)
        && (hasChanged(GO_VALUE_PROPERTY_NAME_STR) || hasChanged(GO_MIN_PROPERTY_NAME_STR)
            || hasChanged(GO_MAX_PROPERTY_NAME_STR) || createWidget)) {
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

    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CHECKBOX_STR)
        && (hasChanged(GO_VALUE_PROPERTY_NAME_STR) || hasChanged(GO_MIN_PROPERTY_NAME_STR)
            || hasChanged(GO_MAX_PROPERTY_NAME_STR) || createWidget)) {
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

    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SLIDER_STR)
        && (hasChanged(GO_MIN_PROPERTY_NAME_STR) || hasChanged(GO_MAX_PROPERTY_NAME_STR)
            || hasChanged(GO_SLIDER_STEP_NAME_STR) || createWidget)) {
        double min(findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR));
        double max(findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR));
        std::vector<double> steps(findVectorDoubleProperty(GO_SLIDER_STEP_NAME_STR));
        ((QSlider*)widget)->setMinimum(0);
        ((QSlider*)widget)->setMaximum((int)((max - min) / steps[0]));
        ((QSlider*)widget)->setSingleStep(1);
        ((QSlider*)widget)->setPageStep((int)(steps[1]));
        findProperty(GO_VALUE_PROPERTY_NAME_STR)
            ->set(ArrayOf::doubleConstructor(min + steps[0] * ((QSlider*)widget)->value()));
        clearChanged(GO_MIN_PROPERTY_NAME_STR);
        clearChanged(GO_MAX_PROPERTY_NAME_STR);
        clearChanged(GO_SLIDER_STEP_NAME_STR);
    }

    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOGGLEBUTTON_STR)
        && (hasChanged(GO_VALUE_PROPERTY_NAME_STR) || hasChanged(GO_MIN_PROPERTY_NAME_STR)
            || hasChanged(GO_MAX_PROPERTY_NAME_STR) || createWidget)) {
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

    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PUSHBUTTON_STR)
        && (hasChanged(GO_C_DATA_PROPERTY_NAME_STR))) {
        deleteWidget();
        clearChanged(GO_C_DATA_PROPERTY_NAME_STR);
        GOGenericProperty* hp = findProperty(GO_STYLE_PROPERTY_NAME_STR);
        hp->setModified(true);
        updateState();
    }
    if (widget) {
        widget->raise();
    }
}
//=============================================================================
void
GOUIControl::constructProperties()
{
    registerProperty(new GOCallbackProperty, GO_CALLBACK_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOStringVectorProperty, GO_STRING_PROPERTY_NAME_STR);
    registerProperty(new GOControlStyleProperty, GO_STYLE_PROPERTY_NAME_STR);
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
    registerProperty(new GOScalarProperty, GO_MIN_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_MAX_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_VALUE_PROPERTY_NAME_STR);
    registerProperty(new GOFixedVectorProperty(2), GO_SLIDER_STEP_NAME_STR);

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
    setScalarDoubleDefault(GO_MIN_PROPERTY_NAME_STR, 0);
    setScalarDoubleDefault(GO_MAX_PROPERTY_NAME_STR, 1);
    setScalarDoubleDefault(GO_VALUE_PROPERTY_NAME_STR, 0);
    setTwoVectorDefault(GO_SLIDER_STEP_NAME_STR, 0.0100, 0.1000);
}
//=============================================================================
void
GOUIControl::paintMe(RenderInterface& gc)
{
    if (widget && gc.getRenderName() != L"GL") {
        QImage image(widget->size(), QImage::Format_ARGB32);
        QPainter painter(&image);
        widget->render(&painter);
        std::vector<double> sizevec(findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
        gc.drawImage(sizevec[0], sizevec[1], image);
    }
}
//=============================================================================
void
GOUIControl::buildWidget(GOWindow* f)
{
    parentGoWindow = f;
}
//=============================================================================
QWidget*
GOUIControl::getParentWidget()
{
    return parentGoWindow->getMainQWigdet();
}
//=============================================================================
QPoint
GOUIControl::convertToBottomLeft(const QPoint& topLeftPos)
{
    int x = topLeftPos.x();
    int y = getParentWidget()->height() - topLeftPos.y() - widget->height();
    return QPoint(x, y);
}
//=============================================================================
void
GOUIControl::clicked()
{
    if (!widget) {
        return;
    }

    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOGGLEBUTTON_STR)) {
        if (((QPushButton*)widget)->isChecked()) {
            findProperty(GO_VALUE_PROPERTY_NAME_STR)
                ->set(
                    ArrayOf::doubleConstructor(findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR)));
        } else {
            findProperty(GO_VALUE_PROPERTY_NAME_STR)
                ->set(
                    ArrayOf::doubleConstructor(findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR)));
        }
    }

    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SLIDER_STR)) {
        double min(findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR));
        std::vector<double> steps(findVectorDoubleProperty(GO_SLIDER_STEP_NAME_STR));
        findProperty(GO_VALUE_PROPERTY_NAME_STR)
            ->set(ArrayOf::doubleConstructor(min + steps[0] * ((QSlider*)widget)->value()));
        clearChanged(GO_VALUE_PROPERTY_NAME_STR);
    }

    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIOBUTTON_STR)) {
        if (((QRadioButton*)widget)->isChecked()) {
            findProperty(GO_VALUE_PROPERTY_NAME_STR)
                ->set(findProperty(GO_MAX_PROPERTY_NAME_STR)->get());
        } else {
            findProperty(GO_VALUE_PROPERTY_NAME_STR)
                ->set(findProperty(GO_MIN_PROPERTY_NAME_STR)->get());
        }
    }

    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CHECKBOX_STR)) {
        if (((QCheckBox*)widget)->isChecked()) {
            findProperty(GO_VALUE_PROPERTY_NAME_STR)
                ->set(findProperty(GO_MAX_PROPERTY_NAME_STR)->get());
        } else {
            findProperty(GO_VALUE_PROPERTY_NAME_STR)
                ->set(findProperty(GO_MIN_PROPERTY_NAME_STR)->get());
        }
    }

    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EDIT_STR)) {
        QString qstr;
        if (widget->objectName() == "multiLine") {
            qstr = ((QMultiLineEdit*)widget)->toPlainText();
        } else {
            qstr = ((QLineEdit*)widget)->text();
        }
        setStringDefault(GO_STRING_PROPERTY_NAME_STR, QStringTowstring(qstr));
    }

    GOCallbackProperty* goCallback
        = (GOCallbackProperty*)findProperty(GO_CALLBACK_PROPERTY_NAME_STR);
    if (goCallback) {
        goCallback->pushEvent(this, L"ActionData", L"Action");
    }
}
//=============================================================================
void
GOUIControl::show()
{
    if (widget) {
        widget->show();
    }
}
//=============================================================================
void
GOUIControl::hide()
{
    if (widget) {
        widget->hide();
    }
}
//=============================================================================
void
GOUIControl::deleteWidget()
{
    if (widget) {
        widget->hide();
        disconnect(widget, SIGNAL(clicked()), nullptr, nullptr);
        delete widget;
        widget = nullptr;
    }
}
//=============================================================================
}
//=============================================================================
