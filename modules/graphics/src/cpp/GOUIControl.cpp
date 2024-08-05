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

    // Initialize the elapsed timers
    keyPressElapsedTimer.invalidate();
    keyReleaseElapsedTimer.invalidate();
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

    if (createWidget && widget) {
        widget->installEventFilter(this);
    }

    onStringPropertyChanged(createWidget);
    onPositionPropertyChanged(createWidget);
    onParentPositionChanged();
    onVisiblePropertyChanged();
    onBackgroundPropertyChanged();

    onFontNameChanged();
    onFontSizeOrUnitChanged();
    onFontAngleChanged();
    onFontWeightChanged();
    onHAlignmentChanged();

    onEnableChanged();
    onToolTipsChanged(createWidget);
    onRadioButtonChanged(createWidget);
    onCheckBoxChanged(createWidget);
    onSliderChanged(createWidget);
    onToggleButtonChanged(createWidget);
    onEditPropertyChanged();
    onCDataPropertyChanged();
    onListTopBoxChanged(createWidget);
    onPopupMenuChanged(createWidget);
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
GOUIControl::eventFilter(QObject* obj, QEvent* event)
{
    if (obj == widget) {
        const int DEBOUNCE_TIME_MS = 80;
        if (event->type() == QEvent::KeyPress) {
            if (keyPressElapsedTimer.isValid()
                && keyPressElapsedTimer.elapsed() < DEBOUNCE_TIME_MS) {
                return true;
            }
            keyReleaseElapsedTimer.invalidate();
            keyPressElapsedTimer.start();
            return handleKeyEvent(event, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR, L"KeyPress");
        } else if (event->type() == QEvent::KeyRelease) {
            if (keyReleaseElapsedTimer.isValid()
                && keyReleaseElapsedTimer.elapsed() < DEBOUNCE_TIME_MS) {
                return true;
            }
            keyReleaseElapsedTimer.invalidate();
            keyReleaseElapsedTimer.start();
            return handleKeyEvent(event, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR, L"KeyRelease");
        } else if (event->type() == QEvent::MouseButtonPress) {
        } else if (event->type() == QEvent::MouseButtonRelease) {
            QMouseEvent* mouseEvent = static_cast<QMouseEvent*>(event);
            if (mouseEvent->button() == Qt::LeftButton) {
                clicked();
                return true;
            } else {
                return handleMouseEvent(event, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"ButtonDown");
            }
        }
    }
    return QObject::eventFilter(obj, event);
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
