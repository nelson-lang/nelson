//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QLineEditWidget.h"
#include "UIControlBinding.hpp"
#include "QStringConverter.hpp"
#include "GOPropertyNames.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
QLineEditWidget::QLineEditWidget(GOUIControl* gouiParent, QWidget* parent) : goParent(gouiParent)
{
    setObjectName("QLineEditWidget");
    this->setParent(parent);
    setFocusPolicy(Qt::ClickFocus);
}
//=============================================================================
void
QLineEditWidget::keyPressEvent(QKeyEvent* event)
{
    UIControlHandleKeyEvent(goParent, event, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR, L"KeyPress");
    QLineEdit::keyPressEvent(event);
}
//=============================================================================
void
QLineEditWidget::keyReleaseEvent(QKeyEvent* event)
{
    UIControlHandleKeyEvent(goParent, event, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR, L"KeyRelease");
    QLineEdit::keyReleaseEvent(event);
}
//=============================================================================
bool
QLineEditWidget::focusNextPrevChild(bool next)
{
    // Tab and Shift+Tab follow the uicontrol order of the figure
    return UIControlFocusNextPrevious(goParent, next) || QLineEdit::focusNextPrevChild(next);
}
//=============================================================================
void
QLineEditWidget::mousePressEvent(QMouseEvent* event)
{
    UIControlHandleMouseEvent(goParent, event, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"ButtonDown");
    QLineEdit::mousePressEvent(event);
}
//=============================================================================
void
QLineEditWidget::mouseReleaseEvent(QMouseEvent* event)
{
    if (goParent && goParent->isEnableInactive()) {
        // Read-only edit: allow cursor/selection but no value sync or callback.
        QLineEdit::mouseReleaseEvent(event);
        return;
    }
    QLineEdit::mouseReleaseEvent(event);
    UIControlSyncValueFromWidget(goParent, this);
    goParent->clicked();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
