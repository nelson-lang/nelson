//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QApplication>
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
GOUIControl::keyPressEvent(QKeyEvent* event)
{
}
void
GOUIControl::keyReleaseEvent(QKeyEvent* event)
{
}

void
GOUIControl::clicked()
{
    if (!widget) {
        return;
    }
    pushButtonClicked();
    toggleButtonClicked();
    sliderClicked();
    radioButtonClicked();
    checkboxClicked();
    editClicked();
    listboxClicked();
    popupmenuClicked();

    GOCallbackProperty* goCallback
        = (GOCallbackProperty*)findProperty(GO_CALLBACK_PROPERTY_NAME_STR);
    if (goCallback) {
        goCallback->pushEvent(this, L"ActionData", L"Action");
    }
}
//=============================================================================
void
GOUIControl::pushButtonClicked()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PUSHBUTTON_STR)) {
        return;
    }
}
//=============================================================================
void
GOUIControl::toggleButtonClicked()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOGGLEBUTTON_STR)) {
        return;
    }
    if (((QPushButton*)widget)->isChecked()) {
        findProperty(GO_VALUE_PROPERTY_NAME_STR)
            ->set(ArrayOf::doubleConstructor(findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR)));
    } else {
        findProperty(GO_VALUE_PROPERTY_NAME_STR)
            ->set(ArrayOf::doubleConstructor(findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR)));
    }
}
//=============================================================================
void
GOUIControl::sliderClicked()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SLIDER_STR)) {
        return;
    }
    double min(findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR));
    std::vector<double> steps(findVectorDoubleProperty(GO_SLIDER_STEP_NAME_STR));
    findProperty(GO_VALUE_PROPERTY_NAME_STR)
        ->set(ArrayOf::doubleConstructor(min + steps[0] * ((QSlider*)widget)->value()));
    clearChanged(GO_VALUE_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOUIControl::radioButtonClicked()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIOBUTTON_STR)) {
        return;
    }
    if (((QRadioButton*)widget)->isChecked()) {
        findProperty(GO_VALUE_PROPERTY_NAME_STR)
            ->set(findProperty(GO_MAX_PROPERTY_NAME_STR)->get());
    } else {
        findProperty(GO_VALUE_PROPERTY_NAME_STR)
            ->set(findProperty(GO_MIN_PROPERTY_NAME_STR)->get());
    }
}
//=============================================================================
void
GOUIControl::checkboxClicked()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CHECKBOX_STR)) {
        return;
    }
    if (((QCheckBox*)widget)->isChecked()) {
        findProperty(GO_VALUE_PROPERTY_NAME_STR)
            ->set(findProperty(GO_MAX_PROPERTY_NAME_STR)->get());
    } else {
        findProperty(GO_VALUE_PROPERTY_NAME_STR)
            ->set(findProperty(GO_MIN_PROPERTY_NAME_STR)->get());
    }
    findProperty(GO_VALUE_PROPERTY_NAME_STR)->setModified(true);
}
//=============================================================================
void
GOUIControl::editClicked()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EDIT_STR)) {
        return;
    }
    QString qstr;
    if (widget->objectName() == "QMultiLineEditWidget") {
        qstr = ((QMultiLineEdit*)widget)->toPlainText();
    } else {
        qstr = ((QLineEdit*)widget)->text();
    }
    setStringDefault(GO_STRING_PROPERTY_NAME_STR, QStringTowstring(qstr));
}
//=============================================================================
void
GOUIControl::listboxClicked()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LISTBOX_STR)) {
        return;
    }
    QList<QListWidgetItem*> selection(((QListWidget*)widget)->selectedItems());
    std::vector<double> selected;
    for (int i = 0; i < selection.size(); i++) {
        selected.push_back(((QListWidget*)widget)->row(selection[i]) + 1);
    }
    ((GOVectorProperty*)findProperty(GO_VALUE_PROPERTY_NAME_STR))->data(selected);
    clearChanged(GO_VALUE_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOUIControl::popupmenuClicked()
{
    if (!stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POPUPMENU_STR)) {
        return;
    }
    int value = ((QComboBox*)widget)->currentIndex() + 1;
    findProperty(GO_VALUE_PROPERTY_NAME_STR)->set(ArrayOf::doubleConstructor(value));
    clearChanged(GO_VALUE_PROPERTY_NAME_STR);
}
//=============================================================================
}
//=============================================================================
