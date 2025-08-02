//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QMultiLineEdit.h"
//=============================================================================
QMultiLineEdit::QMultiLineEdit(QWidget* parent) : QTextEdit(parent) { }
//=============================================================================
void
QMultiLineEdit::focusOutEvent(QFocusEvent* event)
{
    emit editingFinished();
    QTextEdit::focusOutEvent(event);
}
//=============================================================================
void
QMultiLineEdit::keyPressEvent(QKeyEvent* event)
{
    if ((event->key() == Qt::Key_Return) && ((event->modifiers() & Qt::AltModifier) != 0)) {
        emit editingFinished();
    } else {
        QTextEdit::keyPressEvent(event);
    }
}
//=============================================================================
