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
GOUIControl::GOUIControl()
{
    widget = nullptr;
    constructProperties();
    setupDefaults();
}
//=============================================================================
GOUIControl::~GOUIControl()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    deleteWidget();
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
    bool createWidget = onStylePropertyChanged();

    if (!widget) {
        clearAllChanged();
        return;
    }

    onStringPropertyChanged(createWidget);
    onParentPositionChanged(createWidget);
    onVisiblePropertyChanged(createWidget);
    onBackgroundPropertyChanged(createWidget);

    onFontNameChanged(createWidget);
    onFontSizeOrUnitChanged(createWidget);
    onFontAngleChanged(createWidget);
    onFontWeightChanged(createWidget);
    onHAlignmentChanged(createWidget);

    onEnableChanged(createWidget);
    onToolTipsChanged(createWidget);
    onRadioButtonChanged(createWidget);
    onCheckBoxChanged(createWidget);
    onSliderChanged(createWidget);
    onToggleButtonChanged(createWidget);
    onEditPropertyChanged(createWidget);
    onCDataPropertyChanged(createWidget);
    onListTopBoxChanged(createWidget);
    onPopupMenuChanged(createWidget);

    onValueChanged(createWidget);
    onPositionPropertyChanged(createWidget);

    if (widget) {
        widget->raise();
        widget->setFocus();
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
bool
GOUIControl::handleKeyEvent(
    QEvent* event, const std::wstring& callbackPropertyStr, const std::wstring& eventType)
{
    GOCallbackProperty* goCallback = (GOCallbackProperty*)findProperty(callbackPropertyStr);
    ArrayOf callbackArrayOf = goCallback->get();
    if (callbackArrayOf.isEmpty()) {
        return true;
    }

    QKeyEvent* keyEvent = static_cast<QKeyEvent*>(event);
    wstringVector modifiersWstringVector = getModifiers(keyEvent);
    std::wstring key = getKeyString(keyEvent);
    std::wstring character = getCharacterString(keyEvent);

    goCallback->pushKeyEvent(
        this, L"UIClientComponentKeyEvent", eventType, character, key, modifiersWstringVector);
    return true;
}
//=============================================================================
bool
GOUIControl::handleMouseEvent(
    QEvent* event, const std::wstring& callbackPropertyStr, const std::wstring& eventType)
{
    GOCallbackProperty* goCallback = (GOCallbackProperty*)findProperty(callbackPropertyStr);
    ArrayOf callbackArrayOf = goCallback->get();
    if (callbackArrayOf.isEmpty()) {
        return true;
    }
    goCallback->pushEvent(this, L"MouseData", eventType);
    return true;
}
//=============================================================================
wstringVector
GOUIControl::getModifiers(QKeyEvent* keyEvent)
{
    wstringVector modifiersWstringVector;
    modifiersWstringVector.reserve(4);
    Qt::KeyboardModifiers modifiers = keyEvent->modifiers();
    if (modifiers & Qt::ShiftModifier) {
        modifiersWstringVector.push_back(L"shift");
    }
    if (modifiers & Qt::ControlModifier) {
        modifiersWstringVector.push_back(L"control");
    }
    if (modifiers & Qt::AltModifier) {
        modifiersWstringVector.push_back(L"alt");
    }
    if (modifiers & Qt::MetaModifier) {
        modifiersWstringVector.push_back(L"meta");
    }
    return modifiersWstringVector;
}
//=============================================================================
std::wstring
GOUIControl::getKeyString(QKeyEvent* keyEvent)
{
    return QStringTowstring(QKeySequence(keyEvent->key()).toString());
}
//=============================================================================
std::wstring
GOUIControl::getCharacterString(QKeyEvent* keyEvent)
{
    return QStringTowstring(keyEvent->text());
}
//=============================================================================
}
//=============================================================================
