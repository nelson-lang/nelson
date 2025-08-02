//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
#include "QPushButtonWidget.h"
#include "QToggleButtonWidget.h"
#include "QCheckBoxWidget.h"
#include "QRadioButtonWidget.h"
#include "QSliderWidget.h"
#include "QLabelWidget.h"
#include "QPopupMenuWidget.h"
#include "QLineEditWidget.h"
#include "QMultiLineEditWidget.h"
#include "QListBoxWidget.h"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOUIControl::deleteWidget()
{
    if (widget) {
        widget->hide();
        delete widget;
        widget = nullptr;
    }
}
//=============================================================================
void
GOUIControl::createPushButton()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PUSHBUTTON_STR)) {
        return;
    }
    widget = new QPushButtonWidget(this, getParentWidget());
}
//=============================================================================
void
GOUIControl::createToggleButton()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOGGLEBUTTON_STR)) {
        return;
    }
    widget = new QToggleButtonWidget(this, getParentWidget());
}
//=============================================================================
void
GOUIControl::createRadioButton()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIOBUTTON_STR)) {
        return;
    }
    widget = new QRadioButtonWidget(this, getParentWidget());
}
//=============================================================================
void
GOUIControl::createCheckBox()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CHECKBOX_STR)) {
        return;
    }
    widget = new QCheckBoxWidget(this, getParentWidget());
}
//=============================================================================
void
GOUIControl::createSlider()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SLIDER_STR)) {
        return;
    }
    std::vector<double> sizevec(findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
    widget = new QSliderWidget(sizevec[3] > sizevec[2], this, getParentWidget());
}
//=============================================================================
void
GOUIControl::createText()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEXT_STR)) {
        return;
    }
    widget = new QLabelWidget(this, getParentWidget());
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
    bool isMultiLine = (max - min) > 1;
    if (isMultiLine) {
        widget = new QMultiLineEditWidget(this, getParentWidget());
    } else {
        widget = new QLineEditWidget(this, getParentWidget());
    }
}
//=============================================================================
void
GOUIControl::createListBox()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LISTBOX_STR)) {
        return;
    }
    widget = new QListBoxWidget(this, getParentWidget());
}
//=============================================================================
void
GOUIControl::createPopupMenu()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POPUPMENU_STR)) {
        return;
    }
    widget = new QPopupMenuWidget(this, getParentWidget());
}
//=============================================================================
}
//=============================================================================
