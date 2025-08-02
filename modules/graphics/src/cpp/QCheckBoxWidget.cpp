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
    goParent->handleKeyEvent(event, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR, L"KeyPress");
    QWidget::keyPressEvent(event);
}
//=============================================================================
void
QCheckBoxWidget::keyReleaseEvent(QKeyEvent* event)
{
    if (event->key() == Qt::Key_Space) {
        setChecked(!isChecked());
    }
    goParent->handleKeyEvent(event, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR, L"KeyRelease");
    QWidget::keyReleaseEvent(event);
}
//=============================================================================
void
QCheckBoxWidget::mousePressEvent(QMouseEvent* event)
{
    goParent->handleMouseEvent(event, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"ButtonDown");
    QWidget::mousePressEvent(event);
}
//=============================================================================
void
QCheckBoxWidget::mouseReleaseEvent(QMouseEvent* event)
{
    if (event->button() == Qt::LeftButton) {
        QWidget::mouseReleaseEvent(event);
        setChecked(!isChecked());
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
}
//=============================================================================
