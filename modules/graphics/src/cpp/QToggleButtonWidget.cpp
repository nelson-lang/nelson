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
    goParent->handleKeyEvent(event, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR, L"KeyPress");
    QPushButton::keyPressEvent(event);
}
//=============================================================================
void
QToggleButtonWidget::keyReleaseEvent(QKeyEvent* event)
{
    goParent->handleKeyEvent(event, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR, L"KeyRelease");
    QPushButton::keyReleaseEvent(event);
}
//=============================================================================
void
QToggleButtonWidget::mousePressEvent(QMouseEvent* event)
{
    goParent->handleMouseEvent(event, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"ButtonDown");
    QPushButton::mousePressEvent(event);
}
//=============================================================================
void
QToggleButtonWidget::mouseReleaseEvent(QMouseEvent* event)
{
    if (event->button() == Qt::LeftButton) {
        QPushButton::mouseReleaseEvent(event);
        setChecked(!isChecked());
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
}
//=============================================================================
