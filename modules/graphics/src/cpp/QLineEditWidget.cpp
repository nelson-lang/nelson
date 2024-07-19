//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QLineEditWidget.h"
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
    goParent->handleKeyEvent(event, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR, L"KeyPress");
    QLineEdit::keyPressEvent(event);
}
//=============================================================================
void
QLineEditWidget::keyReleaseEvent(QKeyEvent* event)
{
    goParent->handleKeyEvent(event, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR, L"KeyRelease");
    QLineEdit::keyReleaseEvent(event);
}
//=============================================================================
void
QLineEditWidget::mousePressEvent(QMouseEvent* event)
{
    goParent->handleMouseEvent(event, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"ButtonDown");
    QLineEdit::mousePressEvent(event);
}
//=============================================================================
void
QLineEditWidget::mouseReleaseEvent(QMouseEvent* event)
{
    QLineEdit::mouseReleaseEvent(event);
    goParent->clicked();
}
//=============================================================================
}
//=============================================================================
