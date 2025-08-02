//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtTextIndent.h"
#include <QtGui/QTextDocumentFragment>
//=============================================================================
QtTextIndent::QtTextIndent() = default;
//=============================================================================
QtTextIndent::~QtTextIndent() = default;
//=============================================================================
void
QtTextIndent::setDocument(QtTextEdit* te)
{
    m_te = te;
}
//=============================================================================
QtTextEdit*
QtTextIndent::document() const
{
    return m_te;
}
//=============================================================================
QString
indentLine(QString toIndent, QStringList priorText)
{
    return toIndent;
}
//=============================================================================
void
QtTextIndent::update()
{
    QTextCursor cursor(m_te->textCursor());
    QTextCursor save(cursor);
    QTextCursor final(cursor);
    // Get the current cursor position relative to the start of the line
    final.movePosition(QTextCursor::StartOfLine, QTextCursor::KeepAnchor);
    int curpos = final.selectedText().length();
    cursor.movePosition(QTextCursor::StartOfLine);
    cursor.movePosition(QTextCursor::EndOfLine, QTextCursor::KeepAnchor);
    QString toIndent(cursor.selectedText());
    cursor.movePosition(QTextCursor::Up);
    cursor.movePosition(QTextCursor::EndOfLine);
    cursor.movePosition(QTextCursor::Start, QTextCursor::KeepAnchor);
    QStringList priorlines(cursor.selection().toPlainText().split("\n"));
    QString indented(indentLine(toIndent, priorlines));
    save.movePosition(QTextCursor::StartOfLine);
    save.movePosition(QTextCursor::EndOfLine, QTextCursor::KeepAnchor);
    save.insertText(indented);
    // Move the cursor to where it was relative to the original text
    // The number of characters inserted
    int orig_length = toIndent.length();
    int new_length = indented.length();
    int new_pos = std::max(curpos + new_length - orig_length, 0);
    final.movePosition(QTextCursor::StartOfLine, QTextCursor::MoveAnchor);
    final.movePosition(QTextCursor::Right, QTextCursor::MoveAnchor, new_pos);
    m_te->setTextCursor(final);
}
//=============================================================================
