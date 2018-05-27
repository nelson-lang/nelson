//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "QtTextIndent.h"
#include <QtGui/QTextDocumentFragment>
//=============================================================================
QtTextIndent::QtTextIndent() {}
//=============================================================================
QtTextIndent::~QtTextIndent() {}
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
    int new_pos = qMax(curpos + new_length - orig_length, 0);
    final.movePosition(QTextCursor::StartOfLine, QTextCursor::MoveAnchor);
    final.movePosition(QTextCursor::Right, QTextCursor::MoveAnchor, new_pos);
    m_te->setTextCursor(final);
}
//=============================================================================
