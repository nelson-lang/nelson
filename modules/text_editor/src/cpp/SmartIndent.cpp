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
#include <QtCore/QFileInfo>
#include <QtCore/QStringList>
#include <QtCore/QTextStream>
#include <QtCore/QRegularExpression>
#include <QtGui/QTextDocumentFragment>
#include "SmartIndent.hpp"
#include "QStringConverter.hpp"
#include "SmartIndent.h"
//=============================================================================
QString
setIndentSpace(QString lineToIndent, int leadingSpace)
{
    QRegularExpression regWhitespace("^\\s*");
    int k;
    if ((k = lineToIndent.indexOf(regWhitespace, 0)) != -1) {
        QRegularExpressionMatch match = regWhitespace.match(lineToIndent);
        if (match.hasMatch()) {
            lineToIndent.remove(0, match.capturedLength());
        }
    }
    lineToIndent = QString(leadingSpace, ' ') + lineToIndent;
    return lineToIndent;
}
//=============================================================================
void
removeMatch(QString& line, QString& patternStr)
{
    QRegularExpression pattern(patternStr);
    int j = 0;
    while ((j = line.indexOf(pattern, j)) != -1) {
        QRegularExpressionMatch match = pattern.match(line);
        if (match.hasMatch()) {
            int len = match.capturedLength();
            for (int i = 0; i < len; i++) {
                line.replace(j + i, 1, 'X');
            }
            j += len;
        }
    }
}
//=============================================================================
int
countMatches(QString line, QString& patternStr)
{
    QRegularExpression pattern(patternStr);
    int matchCount = 0;
    int i = 0;
    while ((i = line.indexOf(pattern, i)) != -1) {
        matchCount++;
        QRegularExpressionMatch match = pattern.match(line);
        i += match.capturedLength();
    }
    return matchCount;
}
//=============================================================================
QString
stripLine(QString line)
{
    QString literal(R"('([^']*)')");
    removeMatch(line, literal);
    QString endparenthese(R"(\([^\)]*(\b(end|endfunction)\b)[^\)]*\))");
    removeMatch(line, endparenthese);
    QString endbracket(R"(\{[^\}]*(\b(end|endfunction)\b)[^\}]*\})");
    removeMatch(line, endbracket);
    QString commentPercent("%.*");
    removeMatch(line, commentPercent);
    QString commentSlash("//.*");
    removeMatch(line, commentSlash);
    return line;
}
//=============================================================================
int
computeIndexIncrement(QString a)
{
    QString keywordIn("\\b(if|for|function|try|while|switch)\\b");
    QString keywordOut("\\b(end|endfunction)\\b");
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
    QRegularExpression keyword_adjust(R"(^\s*\b(endfunction|end|else|elseif|catch)\b)");
    if (stripped.indexOf(keyword_adjust) >= 0) {
        indentIncrement--;
    }
    if (last.indexOf(keyword_adjust) >= 0) {
        indentIncrement++;
    }
    QRegularExpression keyword_case(R"(^\s*\b(case|otherwise)\b)");
    QRegularExpression keyword_switch(R"(^\s*\b(switch)\b)");
    QRegularExpression keyword_end(R"(^\s*\b(end)\b)");
    if (stripped.indexOf(keyword_case) >= 0 && last.indexOf(keyword_switch) < 0) {
        indentIncrement--;
    }
    if (last.indexOf(keyword_case) >= 0 && stripped.indexOf(keyword_end) < 0) {
        indentIncrement++;
    }
    QRegularExpression function_det(R"(^\s*\b(function)\b)");
    if (stripped.indexOf(function_det) >= 0) {
        return setIndentSpace(lineToIndent, 0);
    }
    QRegularExpression whitespace("^\\s*");
    int leadingSpace = 0;
    int i = 0;
    if ((i = last.indexOf(whitespace, 0)) != -1) {
        QRegularExpressionMatch match = whitespace.match(last);
        leadingSpace = match.capturedLength();
    }
    leadingSpace += indentIncrement * indentSize;
    leadingSpace = std::max(leadingSpace, 0);
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
smartIndent(const std::wstring& filename, int tabsize, bool doBackup)
{
    QFile file(wstringToQString(filename));
    if (file.open(QFile::ReadOnly | QFile::Text)) {
        QTextStream in(&file);
#if QT_VERSION < QT_VERSION_CHECK(6, 0, 0)
        in.setCodec("UTF-8");
#endif
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
#if QT_VERSION < QT_VERSION_CHECK(6, 0, 0)
                out.setCodec("UTF-8");
#endif
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
