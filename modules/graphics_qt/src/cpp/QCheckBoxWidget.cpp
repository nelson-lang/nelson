//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QCheckBoxWidget.h"
#include "UIControlBinding.hpp"
#include "QStringConverter.hpp"
#include "GOPropertyNames.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
QCheckBoxWidget::QCheckBoxWidget(GOUIControl* gouiParent, QWidget* parent) : goParent(gouiParent)
{
    setObjectName("QCheckBoxWidget");
    this->setParent(parent);
    setAutoRepeat(false);
    setEnabled(true);
    setFocusPolicy(Qt::ClickFocus);
}
//=============================================================================
void
QCheckBoxWidget::keyPressEvent(QKeyEvent* event)
{
    UIControlHandleKeyEvent(goParent, event, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR, L"KeyPress");
    QWidget::keyPressEvent(event);
}
//=============================================================================
void
QCheckBoxWidget::keyReleaseEvent(QKeyEvent* event)
{
    // Space selects like a click does, Callback included
    bool activate = UIControlIsActivationKey(event) && goParent && !goParent->isEnableInactive();
    if (activate) {
        setChecked(!isChecked());
    }
    UIControlHandleKeyEvent(goParent, event, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR, L"KeyRelease");
    QWidget::keyReleaseEvent(event);
    if (activate) {
        UIControlSyncValueFromWidget(goParent, this);
        goParent->clicked();
    }
}
//=============================================================================
bool
QCheckBoxWidget::focusNextPrevChild(bool next)
{
    // Tab and Shift+Tab follow the uicontrol order of the figure
    return UIControlFocusNextPrevious(goParent, next) || QCheckBox::focusNextPrevChild(next);
}
//=============================================================================
void
QCheckBoxWidget::mousePressEvent(QMouseEvent* event)
{
    UIControlHandleMouseEvent(goParent, event, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"ButtonDown");
    if (goParent && goParent->isEnableInactive()) {
        return;
    }
    QWidget::mousePressEvent(event);
}
//=============================================================================
void
QCheckBoxWidget::mouseReleaseEvent(QMouseEvent* event)
{
    if (goParent && goParent->isEnableInactive()) {
        return;
    }
    if (event->button() == Qt::LeftButton) {
        QWidget::mouseReleaseEvent(event);
        setChecked(!isChecked());
        UIControlSyncValueFromWidget(goParent, this);
        goParent->clicked();
    }
}
//=============================================================================
void
QCheckBoxWidget::setChecked(bool state)
{
    checked = state;
    QCheckBox::setChecked(state);
}
//=============================================================================
bool
QCheckBoxWidget::isChecked()
{
    return checked;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
