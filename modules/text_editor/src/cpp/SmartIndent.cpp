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
#include "SmartIndent.hpp"
#include "QStringConverter.hpp"
#include "SmartIndent.h"
#include <QtCore/QFileInfo>
#include <QtCore/QRegExp>
#include <QtCore/QStringList>
#include <QtCore/QTextStream>
#include <QtGui/QTextDocumentFragment>
//=============================================================================
QString
setIndentSpace(QString lineToIndent, int leadingSpace)
{
    QRegExp whitespace("^\\s*");
    int k;
    if ((k = lineToIndent.indexOf(whitespace, 0)) != -1) {
        lineToIndent.remove(0, whitespace.matchedLength());
    }
    lineToIndent = QString(leadingSpace, ' ') + lineToIndent;
    return lineToIndent;
}
//=============================================================================
void
removeMatch(QString& line, QRegExp& pattern)
{
    int j = 0;
    while ((j = line.indexOf(pattern, j)) != -1) {
        for (int i = 0; i < pattern.matchedLength(); i++) {
            line.replace(j + i, 1, 'X');
        }
        j += pattern.matchedLength();
    }
}
//=============================================================================
int
countMatches(QString line, QRegExp& pattern)
{
    int matchCount = 0;
    int i = 0;
    while ((i = line.indexOf(pattern, i)) != -1) {
        matchCount++;
        i += pattern.matchedLength();
    }
    return matchCount;
}
//=============================================================================
QString
stripLine(QString line)
{
    QRegExp literal("\'([^\']*)\'");
    removeMatch(line, literal);
    QRegExp endparenthese("\\([^\\)]*(\\b(end|endfunction)\\b)[^\\)]*\\)");
    removeMatch(line, endparenthese);
    QRegExp endbracket("\\{[^\\}]*(\\b(end|endfunction)\\b)[^\\}]*\\}");
    removeMatch(line, endbracket);
    QRegExp commentPercent("%.*");
    removeMatch(line, commentPercent);
    QRegExp commentSlash("//.*");
    removeMatch(line, commentSlash);
    return line;
}
//=============================================================================
int
computeIndexIncrement(QString a)
{
    QRegExp keywordIn("\\b(if|for|function|try|while|switch)\\b");
    QRegExp keywordOut("\\b(end|endfunction)\\b");
    return countMatches(a, keywordIn) - countMatches(a, keywordOut);
}
//=============================================================================
QString
smartIndentLine(QString lineToIndent, QStringList previousText, int indentSize)
{
    bool continueToRemoveLast = !previousText.empty();
    while (continueToRemoveLast) {
        if (previousText.empty()) {
            break;
        }
        int idx = previousText.size() - 1;
        if (idx >= 0) {
            QString last = previousText[idx];
            QString lastTrimmed = last.trimmed();
            continueToRemoveLast = (lastTrimmed.isEmpty() || lastTrimmed.at(0) == '%')
                || ((lastTrimmed.at(0) == '/') && (lastTrimmed.at(1) == '/'));
            if (continueToRemoveLast) {
                previousText.removeLast();
            }
        }
    }
    if (previousText.empty()) {
        return setIndentSpace(lineToIndent, 0);
    }
    QString last = previousText[previousText.size() - 1];
    last = stripLine(last);
    int indentIncrement = computeIndexIncrement(last);
    QString stripped = stripLine(lineToIndent);
    QRegExp keyword_adjust("^\\s*\\b(endfunction|end|else|elseif|catch)\\b");
    if (stripped.indexOf(keyword_adjust) >= 0) {
        indentIncrement--;
    }
    if (last.indexOf(keyword_adjust) >= 0) {
        indentIncrement++;
    }
    QRegExp keyword_case("^\\s*\\b(case|otherwise)\\b");
    QRegExp keyword_switch("^\\s*\\b(switch)\\b");
    QRegExp keyword_end("^\\s*\\b(end)\\b");
    if (stripped.indexOf(keyword_case) >= 0 && last.indexOf(keyword_switch) < 0) {
        indentIncrement--;
    }
    if (last.indexOf(keyword_case) >= 0 && stripped.indexOf(keyword_end) < 0) {
        indentIncrement++;
    }
    QRegExp function_det("^\\s*\\b(function)\\b");
    if (stripped.indexOf(function_det) >= 0) {
        return setIndentSpace(lineToIndent, 0);
    }
    QRegExp whitespace("^\\s*");
    int leadingSpace = 0;
    int i = 0;
    if ((i = last.indexOf(whitespace, 0)) != -1) {
        leadingSpace = whitespace.matchedLength();
    }
    leadingSpace += indentIncrement * indentSize;
    leadingSpace = qMax(leadingSpace, 0);
    return setIndentSpace(lineToIndent, leadingSpace);
}
//=============================================================================
void
smartIndent(QTextEdit* textEdit, int tabSize)
{
    QTextCursor cursor(textEdit->textCursor());
    QTextCursor line1(cursor);
    QTextCursor line2(cursor);
    int startPos;
    if (cursor.position() < cursor.anchor()) {
        line2.setPosition(cursor.anchor());
        startPos = cursor.position();
    } else {
        line1.setPosition(cursor.anchor());
        startPos = cursor.anchor();
    }
    line1.movePosition(QTextCursor::StartOfLine, QTextCursor::MoveAnchor);
    QTextCursor line2Copy(line2);
    line2.movePosition(QTextCursor::StartOfLine, QTextCursor::MoveAnchor);
    if (line2.position() == line2Copy.position()) {
        line2.movePosition(QTextCursor::Up, QTextCursor::MoveAnchor);
    }
    QTextCursor pos(line1);
    pos.beginEditBlock();
    while (pos.position() < line2.position()) {
        pos.movePosition(QTextCursor::StartOfLine, QTextCursor::MoveAnchor);
        pos.movePosition(QTextCursor::Down, QTextCursor::MoveAnchor);
        QTextCursor cursor(pos);
        QTextCursor save(cursor);
        QTextCursor final(cursor);
        final.movePosition(QTextCursor::StartOfLine, QTextCursor::KeepAnchor);
        cursor.movePosition(QTextCursor::StartOfLine);
        cursor.movePosition(QTextCursor::EndOfLine, QTextCursor::KeepAnchor);
        QString toIndent(cursor.selectedText());
        QString indented;
        cursor.movePosition(QTextCursor::StartOfLine);
        cursor.setPosition(startPos, QTextCursor::KeepAnchor);
        QString prior = cursor.selection().toPlainText();
        QStringList priorlines(prior.split("\n"));
        indented = smartIndentLine(toIndent, priorlines, tabSize);
        save.movePosition(QTextCursor::StartOfLine);
        save.movePosition(QTextCursor::EndOfLine, QTextCursor::KeepAnchor);
        save.insertText(indented);
    }
    pos.endEditBlock();
}
//=============================================================================
namespace Nelson {
//=============================================================================
void
smartIndent(std::wstring filename, int tabsize, bool doBackup)
{
    QFile file(wstringToQString(filename));
    if (file.open(QFile::ReadOnly | QFile::Text)) {
        QTextStream in(&file);
        in.setCodec("UTF-8");
        QString content = in.readAll();
        file.close();
        if (doBackup) {
            std::wstring backupFilename = filename + L".bak";
            file.copy(wstringToQString(backupFilename));
        }
        QTextEdit* textEdit = new QTextEdit();
        if (textEdit) {
            textEdit->setText(content);
            textEdit->selectAll();
            smartIndent(textEdit, tabsize);
            if (file.open(QFile::WriteOnly | QFile::Text)) {
                QTextStream out(&file);
                out.setCodec("UTF-8");
                out << textEdit->toPlainText();
                file.close();
            }
            delete textEdit;
        }
    }
}
//=============================================================================
}
//=============================================================================
