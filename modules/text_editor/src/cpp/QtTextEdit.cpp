//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtTextEdit.h"
#include "CompleterHelper.hpp"
#include "QStringConverter.hpp"
#include "i18n.hpp"
#include <QtCore/QStringListModel>
#include <QtGui/QKeyEvent>
#include <QtGui/QMouseEvent>
#include <QtGui/QTextCharFormat>
#include <QtGui/QTextBlock>
#include <QtWidgets/QAbstractItemView>
#include <QtWidgets/QApplication>
#include <QtWidgets/QCompleter>
#include <QtWidgets/QScrollBar>
#include <QtWidgets/QToolTip>
#include <algorithm>
//=============================================================================
using namespace Nelson;
//=============================================================================
QtTextEdit::QtTextEdit()
{
    setLineWrapMode(QTextEdit::NoWrap);
    setAcceptDrops(false);
    setMouseTracking(true);
}
//=============================================================================
QtTextEdit::~QtTextEdit() = default;
//=============================================================================
void
QtTextEdit::setCompleter(QCompleter* completer)
{
    qCompleter = completer;
    if (!qCompleter) {
        return;
    }
    qCompleter->setWidget(this);
    qCompleter->setModelSorting(QCompleter::UnsortedModel);
    qCompleter->setCompletionMode(QCompleter::UnfilteredPopupCompletion);
    qCompleter->setCaseSensitivity(Qt::CaseSensitive);
    qCompleter->setWrapAround(false);
    QObject::connect(qCompleter, SIGNAL(activated(QString)), this, SLOT(insertCompletion(QString)));
}
//=============================================================================
void
QtTextEdit::updateModel(const std::wstring& prefix, const wstringVector& filesList,
    const wstringVector& builtinList, const wstringVector& macroList,
    const wstringVector& variableList, const wstringVector& fieldList,
    const wstringVector& propertyList, const wstringVector& methodList)
{
    if (qCompleter != nullptr) {
        qCompleter->setModel(modelFromNelson(
            filesList, builtinList, macroList, variableList, fieldList, propertyList, methodList));
        qCompleter->setCompletionPrefix(wstringToQString(prefix));
    }
}
//=============================================================================
QAbstractItemModel*
QtTextEdit::modelFromNelson(const wstringVector& filesList, const wstringVector& builtinList,
    const wstringVector& macroList, const wstringVector& variableList,
    const wstringVector& fieldList, const wstringVector& propertyList,
    const wstringVector& methodList)
{
    QStringList words;

    auto appendItemsWithPostfix = [&](const wstringVector& list, const std::wstring& postfix) {
        for (const auto& k : list) {
            words.append(
                wstringToQString(k) + QString(" (") + wstringToQString(postfix) + QString(")"));
        }
    };

    appendItemsWithPostfix(filesList, POSTFIX_FILES);
    appendItemsWithPostfix(builtinList, POSTFIX_BUILTIN);
    appendItemsWithPostfix(macroList, POSTFIX_MACRO);
    appendItemsWithPostfix(variableList, POSTFIX_VARIABLE);
    appendItemsWithPostfix(fieldList, POSTFIX_FIELD);
    appendItemsWithPostfix(methodList, POSTFIX_METHOD);
    appendItemsWithPostfix(propertyList, POSTFIX_PROPERTY);
    words.sort();
    return new QStringListModel(words, qCompleter);
}
//=============================================================================
void
QtTextEdit::keyPressEvent(QKeyEvent* e)
{
    bool tab = false;
    int keycode = e->key();
    if ((keycode == Qt::Key_S) && (e->modifiers() & Qt::ControlModifier)) {
        e->accept();
    } else {
        if (qCompleter && qCompleter->popup()->isVisible()) {
            switch (e->key()) {
            case Qt::Key_Enter:
            case Qt::Key_Return:
            case Qt::Key_Escape:
            case Qt::Key_Tab:
            case Qt::Key_Backtab:
                e->ignore();
                return;
            default:
                break;
            }
            QTextEdit::keyPressEvent(e);
            QString completionPrefix = textUnderCursor();
            complete(completionPrefix);
        } else {
            if ((e->key() == Qt::Key_Tab)
                || ((e->modifiers() & Qt::ControlModifier) && (e->key() == Qt::Key_Space))) {
                QString completionPrefix = textUnderCursor();
                complete(completionPrefix);
            } else {
                if ((e->key() == Qt::Key_A) && (e->modifiers() & Qt::ControlModifier)) {
                    QTextEdit::keyPressEvent(e);
                    return;
                }
                if (e->key() == Qt::Key_Up || e->key() == Qt::Key_Down || e->key() == Qt::Key_Left
                    || e->key() == Qt::Key_Right || e->key() == Qt::Key_End
                    || e->key() == Qt::Key_Home || e->key() == Qt::Key_PageUp
                    || e->key() == Qt::Key_PageDown) {
                    QTextEdit::keyPressEvent(e);
                } else {
                    QTextCursor cursor = textCursor();
                    cursor.beginEditBlock();
                    QTextEdit::keyPressEvent(e);
                    // automatic indentation is emitted inside the edit block so
                    // that the keystroke and the indentation share a single undo
                    // step; the indenter places the cursor itself.
                    const bool autoIndent = isAutoIndentTrigger(e);
                    if (autoIndent) {
                        emit indent();
                    }
                    cursor.endEditBlock();
                    if (!autoIndent) {
                        setTextCursor(cursor);
                    }
                }
            }
        }
    }
}
//=============================================================================
bool
QtTextEdit::isAutoIndentTrigger(QKeyEvent* event) const
{
    if (event == nullptr) {
        return false;
    }
    const Qt::KeyboardModifiers modifiers
        = event->modifiers() & ~(Qt::ShiftModifier | Qt::KeypadModifier);
    if (modifiers != Qt::NoModifier) {
        return false;
    }
    if ((event->key() == Qt::Key_Return) || (event->key() == Qt::Key_Enter)) {
        return true;
    }
    if (event->text().isEmpty()) {
        return false;
    }
    // re-indent as soon as a block keyword is the only thing typed on the line
    static const QStringList deindentKeywords
        = { QStringLiteral("end"), QStringLiteral("else"), QStringLiteral("elseif"),
              QStringLiteral("catch"), QStringLiteral("case"), QStringLiteral("otherwise") };
    QTextCursor cursor(textCursor());
    if (cursor.hasSelection()) {
        return false;
    }
    const QString typed = cursor.block().text().left(cursor.positionInBlock()).trimmed();
    return deindentKeywords.contains(typed);
}
//=============================================================================
void
QtTextEdit::complete(QString prefix)
{
    if (prefix.isEmpty()) {
        return;
    }

    std::wstring line = QStringTowstring(prefix);
    std::wstring completionPrefix;
    wstringVector files;
    wstringVector builtin;
    wstringVector macros;
    wstringVector variables;
    wstringVector fields;
    wstringVector properties;
    wstringVector methods;

    bool showpopup = computeCompletion(
        line, completionPrefix, files, builtin, macros, variables, fields, properties, methods);

    if (showpopup) {
        updateModel(
            completionPrefix, files, builtin, macros, variables, fields, properties, methods);
        QRect cr = cursorRect();
        cr.setWidth(qCompleter->popup()->sizeHintForColumn(0)
            + qCompleter->popup()->verticalScrollBar()->sizeHint().width());
        cr.setHeight(10);
        qCompleter->complete(cr);
        qCompleter->setCurrentRow(0);
        qCompleter->popup()->setCurrentIndex(qCompleter->completionModel()->index(0, 0));
    } else {
        if (qCompleter->popup()->isVisible()) {
            qCompleter->popup()->close();
        }
    }
}
//=============================================================================
void
QtTextEdit::contextMenuEvent(QContextMenuEvent* e)
{
    e->ignore();
}
//=============================================================================
void
QtTextEdit::insertCompletion(const QString& completion)
{
    QString FILE_OR_DIR = QString(" (") + wstringToQString(POSTFIX_FILES) + QString(")");
    bool isPathCompletion = completion.endsWith(FILE_OR_DIR);
    wstringVector postfixStrings;
    postfixStrings.push_back(POSTFIX_FILES);
    postfixStrings.push_back(POSTFIX_BUILTIN);
    postfixStrings.push_back(POSTFIX_MACRO);
    postfixStrings.push_back(POSTFIX_VARIABLE);
    postfixStrings.push_back(POSTFIX_FIELD);
    postfixStrings.push_back(POSTFIX_PROPERTY);
    postfixStrings.push_back(POSTFIX_METHOD);

    // Strip the decoration defensively: lastIndexOf returns -1 when the
    // postfix is absent and must never be fed to remove().
    QString cleanedCompletion = completion;
    for (const std::wstring& postfix : postfixStrings) {
        const QString decoration
            = wstringToQString(std::wstring(L" (") + postfix + std::wstring(L")"));
        const qsizetype index = cleanedCompletion.lastIndexOf(decoration);
        if (index >= 0) {
            cleanedCompletion.remove(index, decoration.size());
        }
    }

    if (qCompleter->widget() != this) {
        return;
    }
    QTextCursor tc = textCursor();
    std::wstring currentLineW = QStringTowstring(textUnderCursor());

    std::wstring cleanedCompletionW = QStringTowstring(cleanedCompletion);
    std::wstring newLine = completerLine(currentLineW, cleanedCompletionW, isPathCompletion);
    tc.beginEditBlock();
    tc.select(QTextCursor::LineUnderCursor);
    tc.removeSelectedText();
    tc.insertText(wstringToQString(newLine));
    tc.endEditBlock();
    setTextCursor(tc);
}
//=============================================================================
void
QtTextEdit::focusInEvent(QFocusEvent* e)
{
    if (qCompleter) {
        qCompleter->setWidget(this);
    }
    QTextEdit::focusInEvent(e);
}
//=============================================================================
QString
QtTextEdit::textUnderCursor() const
{
    QTextCursor tc = textCursor();
    tc.select(QTextCursor::LineUnderCursor);
    return tc.selectedText();
}
//=============================================================================
void
QtTextEdit::comment()
{
    QTextCursor textCursor(this->textCursor());
    QTextCursor lineOne(textCursor);
    QTextCursor lineTwo(textCursor);
    if (textCursor.position() < textCursor.anchor()) {
        lineTwo.setPosition(textCursor.anchor());
    } else {
        lineOne.setPosition(textCursor.anchor());
    }
    lineOne.movePosition(QTextCursor::StartOfLine, QTextCursor::MoveAnchor);
    QTextCursor lineTwoCopy(lineTwo);
    lineTwo.movePosition(QTextCursor::StartOfLine, QTextCursor::MoveAnchor);
    if (lineTwo.position() == lineTwoCopy.position()) {
        lineTwo.movePosition(QTextCursor::Up, QTextCursor::MoveAnchor);
    }
    QTextCursor textPosition(lineOne);
    textPosition.beginEditBlock();
    while (textPosition.position() < lineTwo.position()) {
        textPosition.insertText("% ");
        textPosition.movePosition(QTextCursor::Down, QTextCursor::MoveAnchor);
    }
    textPosition.insertText("% ");
    textPosition.endEditBlock();
}
//=============================================================================
void
QtTextEdit::uncomment()
{
    QTextCursor textCursor(this->textCursor());
    QTextCursor lineOne(textCursor);
    QTextCursor lineTwo(textCursor);
    if (textCursor.position() < textCursor.anchor()) {
        lineTwo.setPosition(textCursor.anchor());
    } else {
        lineOne.setPosition(textCursor.anchor());
    }
    lineOne.movePosition(QTextCursor::StartOfLine, QTextCursor::MoveAnchor);
    QTextCursor lineTwoCopy(lineTwo);
    lineTwo.movePosition(QTextCursor::StartOfLine, QTextCursor::MoveAnchor);
    if (lineTwo.position() == lineTwoCopy.position()) {
        lineTwo.movePosition(QTextCursor::Up, QTextCursor::MoveAnchor);
    }
    QTextCursor textPosition(lineOne);
    textPosition.beginEditBlock();
    while (textPosition.position() <= lineTwo.position()) {
        while (!textPosition.atBlockEnd()) {
            textPosition.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor);
            if (textPosition.selectedText() == " ") {
                textPosition.clearSelection();
                continue;
            }
            if (textPosition.selectedText() == "%") {
                textPosition.deleteChar();
                textPosition.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor);
                if (textPosition.selectedText() == " ") {
                    textPosition.deleteChar();
                }
                break;
            }
            break;
        }
        if (textPosition.position() < lineTwo.position()) {
            textPosition.movePosition(QTextCursor::Down, QTextCursor::MoveAnchor);
            textPosition.movePosition(QTextCursor::StartOfLine, QTextCursor::MoveAnchor);
        }
    }
    textPosition.endEditBlock();
}
//=============================================================================
bool
QtTextEdit::canInsertFromMimeData(const QMimeData* source) const
{
    return source->hasText();
}
//=============================================================================
void
QtTextEdit::insertFromMimeData(const QMimeData* source)
{
    if (source->hasText()) {
        QTextCursor cursor(textCursor());
        cursor.insertText(source->text());
        setTextCursor(cursor);
    }
}
//=============================================================================
void
QtTextEdit::wheelEvent(QWheelEvent* wheelEvent)
{
    if (wheelEvent->modifiers() == Qt::ControlModifier) {
        if (wheelEvent->angleDelta().y() > 0) {
            this->zoomIn();
        } else {
            this->zoomOut();
        }
        wheelEvent->accept();
    } else {
        QTextEdit::wheelEvent(wheelEvent);
    }
}
//=============================================================================
void
QtTextEdit::setDebugLine(int line)
{
    currentDebugLine = line;
    updateExtraSelections();
}
//=============================================================================
void
QtTextEdit::clearDebugLine()
{
    currentDebugLine = -1;
    updateExtraSelections();
}
//=============================================================================
int
QtTextEdit::getDebugLine() const
{
    return currentDebugLine;
}
//=============================================================================
void
QtTextEdit::setDebugLineColor(const QColor& color)
{
    debugLineColor = color;
    if (currentDebugLine > 0) {
        updateExtraSelections();
    }
}
//=============================================================================
void
QtTextEdit::setCodeAnalyzerDiagnostics(
    const std::vector<Nelson::CodeAnalyzerDiagnostic>& diagnostics)
{
    codeAnalyzerDiagnostics = diagnostics;
    updateExtraSelections();
}
//=============================================================================
std::vector<Nelson::CodeAnalyzerDiagnostic>
QtTextEdit::getCodeAnalyzerDiagnostics() const
{
    return codeAnalyzerDiagnostics;
}
//=============================================================================
void
QtTextEdit::setSearchHighlights(const QList<QPair<int, int>>& ranges, int currentIndex)
{
    searchHighlights = ranges;
    currentSearchHighlight = currentIndex;
    updateExtraSelections();
}
//=============================================================================
void
QtTextEdit::clearSearchHighlights()
{
    if (searchHighlights.isEmpty() && (currentSearchHighlight == -1)) {
        return;
    }
    searchHighlights.clear();
    currentSearchHighlight = -1;
    updateExtraSelections();
}
//=============================================================================
void
QtTextEdit::updateExtraSelections()
{
    QList<QTextEdit::ExtraSelection> extraSelections;

    if (currentDebugLine > 0) {
        QTextEdit::ExtraSelection selection;

        selection.format.setBackground(debugLineColor);
        selection.format.setProperty(QTextFormat::FullWidthSelection, true);

        QTextBlock block = document()->findBlockByLineNumber(currentDebugLine - 1);
        if (block.isValid()) {
            QTextCursor cursor(block);
            selection.cursor = cursor;
            selection.cursor.clearSelection();
            extraSelections.append(selection);
        }
    }
    // search occurrences are appended after the debug line and before the code
    // analyzer diagnostics: the wave underlines stay visible on top of them.
    if (!searchHighlights.isEmpty()) {
        const int lastPosition = std::max(document()->characterCount() - 1, 0);
        const QColor matchColor(255, 213, 79, 140);
        const QColor currentMatchColor(255, 145, 0, 190);
        int index = 0;
        for (const auto& range : searchHighlights) {
            const int start = range.first;
            const int length = range.second;
            if ((length <= 0) || (start < 0) || (start >= lastPosition)) {
                ++index;
                continue;
            }
            QTextEdit::ExtraSelection selection;
            selection.format.setBackground(
                (index == currentSearchHighlight) ? currentMatchColor : matchColor);
            QTextCursor cursor(document());
            cursor.setPosition(start);
            cursor.setPosition(std::min(start + length, lastPosition), QTextCursor::KeepAnchor);
            selection.cursor = cursor;
            extraSelections.append(selection);
            ++index;
        }
    }
    for (const auto& diagnostic : codeAnalyzerDiagnostics) {
        if (diagnostic.suppressed || diagnostic.lineStart <= 0) {
            continue;
        }
        QTextBlock block = document()->findBlockByLineNumber(diagnostic.lineStart - 1);
        if (!block.isValid()) {
            continue;
        }
        QTextEdit::ExtraSelection selection;
        QColor color = QColor(74, 144, 226);
        if (diagnostic.severity == "error") {
            color = QColor(206, 51, 51);
        } else if (diagnostic.severity == "warning") {
            color = QColor(222, 143, 35);
        }
        QTextCharFormat format;
        format.setUnderlineColor(color);
        format.setUnderlineStyle(QTextCharFormat::WaveUnderline);
        selection.format = format;
        QTextCursor cursor(block);
        int column = std::max(diagnostic.columnStart - 1, 0);
        cursor.movePosition(QTextCursor::Right, QTextCursor::MoveAnchor, column);
        int width = std::max(diagnostic.columnEnd - diagnostic.columnStart + 1, 1);
        cursor.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor, width);
        selection.cursor = cursor;
        extraSelections.append(selection);
    }

    setExtraSelections(extraSelections);
    viewport()->update();
}
//=============================================================================
void
QtTextEdit::mouseMoveEvent(QMouseEvent* event)
{
    QTextCursor cursor = cursorForPosition(event->pos());
    int line = cursor.blockNumber() + 1;
    QStringList messages;
    for (const auto& diagnostic : codeAnalyzerDiagnostics) {
        if (!diagnostic.suppressed && diagnostic.lineStart == line) {
            messages << QString::fromStdString(
                diagnostic.checkId + " " + diagnostic.severity + ": " + diagnostic.message);
        }
    }
    if (!messages.isEmpty()) {
        QToolTip::showText(event->globalPosition().toPoint(), messages.join("\n"), this);
    } else {
        QToolTip::hideText();
    }
    QTextEdit::mouseMoveEvent(event);
}
//=============================================================================
