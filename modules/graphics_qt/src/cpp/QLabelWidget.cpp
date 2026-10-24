//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QLabelWidget.h"
#include "UIControlBinding.hpp"
#include "QStringConverter.hpp"
#include "GOPropertyNames.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
QLabelWidget::QLabelWidget(GOUIControl* gouiParent, QWidget* parent) : goParent(gouiParent)
{
    setObjectName("QLabelWidget");
    this->setParent(parent);
    setWordWrap(true);
    setFocusPolicy(Qt::ClickFocus);
}
//=============================================================================
void
QLabelWidget::keyPressEvent(QKeyEvent* event)
{
    UIControlHandleKeyEvent(goParent, event, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR, L"KeyPress");
    QLabel::keyPressEvent(event);
}
//=============================================================================
void
QLabelWidget::keyReleaseEvent(QKeyEvent* event)
{
    UIControlHandleKeyEvent(goParent, event, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR, L"KeyRelease");
    QLabel::keyReleaseEvent(event);
}
//=============================================================================
void
QLabelWidget::mousePressEvent(QMouseEvent* event)
{
    UIControlHandleMouseEvent(goParent, event, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"ButtonDown");
    QLabel::mousePressEvent(event);
}
//=============================================================================
void
QLabelWidget::mouseReleaseEvent(QMouseEvent* event)
{
    if (goParent && goParent->isEnableInactive()) {
        return;
    }
    if (event->button() == Qt::LeftButton) {
        QLabel::mouseReleaseEvent(event);
        UIControlSyncValueFromWidget(goParent, this);
        goParent->clicked();
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
