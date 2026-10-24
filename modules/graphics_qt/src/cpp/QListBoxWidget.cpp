//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QListBoxWidget.h"
#include "UIControlBinding.hpp"
#include "QStringConverter.hpp"
#include "GOPropertyNames.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
QListBoxWidget::QListBoxWidget(GOUIControl* gouiParent, QWidget* parent) : goParent(gouiParent)
{
    setObjectName("QListBoxWidget");
    this->setParent(parent);
    setFocusPolicy(Qt::ClickFocus);
}
//=============================================================================
void
QListBoxWidget::keyPressEvent(QKeyEvent* event)
{
    UIControlHandleKeyEvent(goParent, event, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR, L"KeyPress");
    if (goParent && goParent->isEnableInactive()) {
        event->accept();
        return;
    }
    // a key that changes the selection runs the Callback, as a click does
    QList<QListWidgetItem*> previousSelection = selectedItems();
    QListWidget::keyPressEvent(event);
    if (goParent && selectedItems() != previousSelection) {
        UIControlSyncValueFromWidget(goParent, this);
        goParent->clicked();
    }
}
//=============================================================================
void
QListBoxWidget::keyReleaseEvent(QKeyEvent* event)
{
    UIControlHandleKeyEvent(goParent, event, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR, L"KeyRelease");
    QListWidget::keyReleaseEvent(event);
}
//=============================================================================
bool
QListBoxWidget::focusNextPrevChild(bool next)
{
    // Tab and Shift+Tab follow the uicontrol order of the figure
    return UIControlFocusNextPrevious(goParent, next) || QListWidget::focusNextPrevChild(next);
}
//=============================================================================
void
QListBoxWidget::mousePressEvent(QMouseEvent* event)
{
    UIControlHandleMouseEvent(goParent, event, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"ButtonDown");
    if (goParent && goParent->isEnableInactive()) {
        return;
    }
    QListWidget::mousePressEvent(event);
}
//=============================================================================
void
QListBoxWidget::mouseReleaseEvent(QMouseEvent* event)
{
    if (goParent && goParent->isEnableInactive()) {
        return;
    }
    QListWidget::mouseReleaseEvent(event);
    UIControlSyncValueFromWidget(goParent, this);
    goParent->clicked();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
