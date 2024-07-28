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
#include "AnonymousMacroFunctionDef.hpp"
#include "GOHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOUIControl::createPushButton()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PUSHBUTTON_STR)) {
        return;
    }
    widget = new QPushButton(getParentWidget());
    connect(widget, SIGNAL(clicked()), this, SLOT(clicked()));
    ((QPushButton*)widget)->setFlat(false);
    ((QPushButton*)widget)->setStyleSheet(QString("QPushButton {border: 0px;}"));

    ArrayOf cdata(findArrayOfProperty(GO_C_DATA_PROPERTY_NAME_STR));
    if (!cdata.isEmpty() && cdata.getDataClass() == NLS_UINT8) {
        if ((cdata.getDimensions().getLength() == 3) && (cdata.getDimensionLength(2) == 3)) {
            indexType rows = cdata.getRows();
            indexType cols = cdata.getColumns();
            uint8* dp = (uint8*)cdata.getDataPointer();
            QImage img = QImage((int)cols, (int)rows, QImage::Format_ARGB32);
            for (indexType i = 0; i < rows; i++) {
                QRgb* ibits = (QRgb*)img.scanLine((int)i);
                for (indexType j = 0; j < cols; j++) {
                    ibits[j]
                        = qRgba((int)(dp[(i + j * rows)]), (int)(dp[(i + j * rows) + rows * cols]),
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
//=============================================================================
void
GOUIControl::createToggleButton()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOGGLEBUTTON_STR)) {
        return;
    }
    widget = new QPushButton(getParentWidget());
    ((QPushButton*)widget)->setCheckable(true);
    connect(widget, SIGNAL(clicked()), this, SLOT(clicked()));
}
//=============================================================================
void
GOUIControl::createRadioButton()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIOBUTTON_STR)) {
        return;
    }
    widget = new QRadioButton(getParentWidget());
    ((QRadioButton*)widget)->setAutoExclusive(false);
    connect(widget, SIGNAL(clicked()), this, SLOT(clicked()));
}
//=============================================================================
void
GOUIControl::createCheckBox()
{
    if (stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CHECKBOX_STR)) {
        widget = new QCheckBox(getParentWidget());
        connect(widget, SIGNAL(clicked()), this, SLOT(clicked()));
    }
}
//=============================================================================
void
GOUIControl::createSlider()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SLIDER_STR)) {
        return;
    }
    std::vector<double> sizevec(findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
    if (sizevec[3] > sizevec[2]) {
        widget = new QSlider(Qt::Vertical, getParentWidget());
    } else {
        widget = new QSlider(Qt::Horizontal, getParentWidget());
    }
    ((QSlider*)widget)->setTracking(false);
    connect(widget, SIGNAL(valueChanged(int)), this, SLOT(clicked()));
}
//=============================================================================
void
GOUIControl::createText()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEXT_STR)) {
        return;
    }
    widget = new QLabel(getParentWidget());
    ((QLabel*)widget)->setWordWrap(true);
}
//=============================================================================
void
GOUIControl::createEdit()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EDIT_STR)) {
        return;
    }
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
//=============================================================================
void
GOUIControl::createListBox()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LISTBOX_STR)) {
        return;
    }
    widget = new QListWidget(getParentWidget());
    connect(widget, SIGNAL(itemSelectionChanged()), this, SLOT(clicked()));
}
//=============================================================================
void
GOUIControl::createPopupMenu()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POPUPMENU_STR)) {
        return;
    }
    widget = new QComboBox(getParentWidget());
    connect(widget, SIGNAL(activated(int)), this, SLOT(clicked()));
    ((QComboBox*)widget)->setEditable(false);
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
