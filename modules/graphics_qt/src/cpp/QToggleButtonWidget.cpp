//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QToggleButtonWidget.h"
#include "UIControlBinding.hpp"
#include "GOPropertyNames.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
QToggleButtonWidget::QToggleButtonWidget(GOUIControl* gouiParent, QWidget* parent)
    : goParent(gouiParent)
{
    setObjectName("QToggleButtonWidget");
    this->setParent(parent);
    setAutoRepeat(false);
    setFlat(false);
    setCheckable(true);
    setStyleSheet(QString("QPushButton {border: 0px;}"));
    setFocusPolicy(Qt::ClickFocus);
}
//=============================================================================
void
QToggleButtonWidget::keyPressEvent(QKeyEvent* event)
{
    UIControlHandleKeyEvent(goParent, event, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR, L"KeyPress");
    QPushButton::keyPressEvent(event);
}
//=============================================================================
void
QToggleButtonWidget::keyReleaseEvent(QKeyEvent* event)
{
    UIControlHandleKeyEvent(goParent, event, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR, L"KeyRelease");
    bool activation = UIControlIsActivationKey(event);
    if (activation && goParent && goParent->isEnableInactive()) {
        // looks normal but must not toggle
        setDown(false);
        return;
    }
    QPushButton::keyReleaseEvent(event);
    // Space toggles like a click does (the base class toggled the Qt state:
    // realign it on the model flag, then report)
    if (activation && goParent) {
        setChecked(!isChecked());
        UIControlSyncValueFromWidget(goParent, this);
        goParent->clicked();
    }
}
//=============================================================================
bool
QToggleButtonWidget::focusNextPrevChild(bool next)
{
    // Tab and Shift+Tab follow the uicontrol order of the figure
    return UIControlFocusNextPrevious(goParent, next) || QPushButton::focusNextPrevChild(next);
}
//=============================================================================
void
QToggleButtonWidget::mousePressEvent(QMouseEvent* event)
{
    UIControlHandleMouseEvent(goParent, event, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"ButtonDown");
    if (goParent && goParent->isEnableInactive()) {
        return;
    }
    QPushButton::mousePressEvent(event);
}
//=============================================================================
void
QToggleButtonWidget::mouseReleaseEvent(QMouseEvent* event)
{
    if (goParent && goParent->isEnableInactive()) {
        return;
    }
    if (event->button() == Qt::LeftButton) {
        QPushButton::mouseReleaseEvent(event);
        setChecked(!isChecked());
        UIControlSyncValueFromWidget(goParent, this);
        goParent->clicked();
    }
}
//=============================================================================
void
QToggleButtonWidget::setChecked(bool state)
{
    checked = state;
    QPushButton::setChecked(state);
}
//=============================================================================
bool
QToggleButtonWidget::isChecked()
{
    return checked;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
