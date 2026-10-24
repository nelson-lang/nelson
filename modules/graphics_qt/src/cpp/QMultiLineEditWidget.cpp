//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QMultiLineEditWidget.h"
#include "UIControlBinding.hpp"
#include "QStringConverter.hpp"
#include "GOPropertyNames.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
QMultiLineEditWidget::QMultiLineEditWidget(GOUIControl* gouiParent, QWidget* parent)
    : QMultiLineEdit(parent), goParent(gouiParent)
{
    // The uicontrol dispatch (UIControlBinding) tells the multi-line edit from the
    // single-line one by objectName, so it must stay "QMultiLineEditWidget". Overwriting
    // it sends the dispatch to the single-line branch, which casts this QTextEdit-derived
    // widget to a QLineEdit and crashes on setText.
    setObjectName("QMultiLineEditWidget");
    this->setParent(parent);
    setFocusPolicy(Qt::ClickFocus);
}
//=============================================================================
void
QMultiLineEditWidget::keyPressEvent(QKeyEvent* event)
{
    UIControlHandleKeyEvent(goParent, event, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR, L"KeyPress");
    QMultiLineEdit::keyPressEvent(event);
}
//=============================================================================
void
QMultiLineEditWidget::keyReleaseEvent(QKeyEvent* event)
{
    UIControlHandleKeyEvent(goParent, event, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR, L"KeyRelease");
    QMultiLineEdit::keyReleaseEvent(event);
}
//=============================================================================
void
QMultiLineEditWidget::mousePressEvent(QMouseEvent* event)
{
    UIControlHandleMouseEvent(goParent, event, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"ButtonDown");
    QMultiLineEdit::mousePressEvent(event);
    // A QTextEdit is a scroll area: the press lands on its viewport and the
    // click-to-focus walk does not reliably reach this frame widget, so the event bubbles
    // up to the figure window (GOWindow::mousePressEvent), which grabs the keyboard focus
    // and leaves the edit unable to receive typed characters. Claim focus and consume the
    // press here, as the single-line QLineEdit widget does.
    setFocus(Qt::MouseFocusReason);
    event->accept();
}
//=============================================================================
void
QMultiLineEditWidget::mouseReleaseEvent(QMouseEvent* event)
{
    if (goParent && goParent->isEnableInactive()) {
        // Read-only edit: allow cursor/selection but no value sync or callback.
        QMultiLineEdit::mouseReleaseEvent(event);
        return;
    }
    QMultiLineEdit::mouseReleaseEvent(event);
    UIControlSyncValueFromWidget(goParent, this);
    goParent->clicked();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
