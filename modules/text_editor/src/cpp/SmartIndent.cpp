//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QtGlobal>
#include <algorithm>
#include <QtCore/QStringList>
#include <QtGui/QTextBlock>
#include <QtGui/QTextDocument>
#include "NelsonCodeIndent.hpp"
#include "SmartIndent.hpp"
#include "QStringConverter.hpp"
#include "SmartIndent.h"
//=============================================================================
// replaces the whole document content without clearing the undo stack
static void
replaceDocumentContent(QTextEdit* textEdit, const QString& newContent, int cursorPosition)
{
    QTextDocument* document = textEdit->document();
    QTextCursor replaceCursor(document);
    replaceCursor.beginEditBlock();
    replaceCursor.select(QTextCursor::Document);
    replaceCursor.insertText(newContent);
    replaceCursor.endEditBlock();
    QTextCursor restoredCursor(document);
    const int lastPosition = std::max(document->characterCount() - 1, 0);
    restoredCursor.setPosition(std::min(std::max(cursorPosition, 0), lastPosition));
    textEdit->setTextCursor(restoredCursor);
}
//=============================================================================
void
smartIndent(QTextEdit* textEdit, int tabSize)
{
    if (textEdit == nullptr) {
        return;
    }
    QTextCursor originalCursor(textEdit->textCursor());
    const QString content = textEdit->toPlainText();
    const QString indentedContent = Nelson::wstringToQString(
        Nelson::smartIndentCode(Nelson::QStringTowstring(content), tabSize));
    if (!originalCursor.hasSelection()) {
        if (content != indentedContent) {
            replaceDocumentContent(textEdit, indentedContent, originalCursor.position());
        }
        return;
    }

    const int selectionStart = std::min(originalCursor.position(), originalCursor.anchor());
    const int selectionEnd = std::max(originalCursor.position(), originalCursor.anchor());
    QTextDocument* document = textEdit->document();
    QTextBlock startBlock = document->findBlock(selectionStart);
    QTextBlock endBlock = document->findBlock(selectionEnd);
    if (selectionEnd > selectionStart && endBlock.isValid()
        && selectionEnd == endBlock.position()) {
        endBlock = endBlock.previous();
    }
    if (!startBlock.isValid() || !endBlock.isValid()) {
        if (content != indentedContent) {
            replaceDocumentContent(textEdit, indentedContent, originalCursor.position());
        }
        return;
    }

    const QStringList indentedLines = indentedContent.split('\n');
    QString replacement;
    for (int blockNumber = startBlock.blockNumber(); blockNumber <= endBlock.blockNumber();
        ++blockNumber) {
        if (blockNumber > startBlock.blockNumber()) {
            replacement += '\n';
        }
        if (blockNumber < indentedLines.size()) {
            replacement += indentedLines[blockNumber];
        }
    }
    QTextCursor replaceCursor(document);
    replaceCursor.setPosition(startBlock.position());
    replaceCursor.setPosition(endBlock.position() + endBlock.length() - 1, QTextCursor::KeepAnchor);
    replaceCursor.beginEditBlock();
    replaceCursor.insertText(replacement);
    replaceCursor.endEditBlock();
    textEdit->setTextCursor(originalCursor);
}
//=============================================================================
namespace Nelson {
//=============================================================================
void
smartIndent(const std::wstring& filename, int tabsize, bool doBackup, bool fullFormat)
{
    smartIndentFile(filename, tabsize, doBackup, fullFormat);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
