//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QPopupMenuWidget.h"
#include "UIControlBinding.hpp"
#include "QStringConverter.hpp"
#include "GOPropertyNames.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
QPopupMenuWidget::QPopupMenuWidget(GOUIControl* gouiParent, QWidget* parent) : goParent(gouiParent)
{
    setObjectName("QPopupMenuWidget");
    this->setParent(parent);
    setEditable(false);
    setFocusPolicy(Qt::ClickFocus);
    connect(this, SIGNAL(activated(int)), this, SLOT(onActivated(int)));
}
//=============================================================================
void
QPopupMenuWidget::keyPressEvent(QKeyEvent* event)
{
    UIControlHandleKeyEvent(goParent, event, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR, L"KeyPress");
    QComboBox::keyPressEvent(event);
}
//=============================================================================
void
QPopupMenuWidget::keyReleaseEvent(QKeyEvent* event)
{
    UIControlHandleKeyEvent(goParent, event, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR, L"KeyRelease");
    QComboBox::keyReleaseEvent(event);
}
//=============================================================================
bool
QPopupMenuWidget::focusNextPrevChild(bool next)
{
    // Tab and Shift+Tab follow the uicontrol order of the figure
    return UIControlFocusNextPrevious(goParent, next) || QComboBox::focusNextPrevChild(next);
}
//=============================================================================
void
QPopupMenuWidget::mousePressEvent(QMouseEvent* event)
{
    UIControlHandleMouseEvent(goParent, event, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"ButtonDown");
    if (goParent && goParent->isEnableInactive()) {
        return;
    }
    QComboBox::mousePressEvent(event);
}
//=============================================================================
void
QPopupMenuWidget::mouseReleaseEvent(QMouseEvent* event)
{
    if (goParent && goParent->isEnableInactive()) {
        return;
    }
    if (event->button() == Qt::LeftButton) {
        QComboBox::mouseReleaseEvent(event);
    }
}
//=============================================================================
void
QPopupMenuWidget::onActivated(int pos)
{
    if (goParent && goParent->isEnableInactive()) {
        return;
    }
    UIControlSyncValueFromWidget(goParent, this);
    goParent->clicked();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
