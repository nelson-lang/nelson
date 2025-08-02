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
    goParent->handleKeyEvent(event, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR, L"KeyPress");
    QListWidget::keyPressEvent(event);
}
//=============================================================================
void
QListBoxWidget::keyReleaseEvent(QKeyEvent* event)
{
    goParent->handleKeyEvent(event, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR, L"KeyRelease");
    QListWidget::keyReleaseEvent(event);
}
//=============================================================================
void
QListBoxWidget::mousePressEvent(QMouseEvent* event)
{
    goParent->handleMouseEvent(event, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"ButtonDown");
    QListWidget::mousePressEvent(event);
}
//=============================================================================
void
QListBoxWidget::mouseReleaseEvent(QMouseEvent* event)
{
    QListWidget::mouseReleaseEvent(event);
    goParent->clicked();
}
//=============================================================================
}
//=============================================================================
