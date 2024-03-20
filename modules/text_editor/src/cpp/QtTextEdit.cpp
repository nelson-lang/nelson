//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
#include <QtWidgets/QAbstractItemView>
#include <QtWidgets/QApplication>
#include <QtWidgets/QCompleter>
#include <QtWidgets/QScrollBar>
//=============================================================================
using namespace Nelson;
//=============================================================================
QtTextEdit::QtTextEdit()
{
    setLineWrapMode(QTextEdit::NoWrap);
    setAcceptDrops(false);
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
    if ((keycode == Qt::Key_S) && QApplication::keyboardModifiers() & Qt::ControlModifier) {
        e->accept();
    } else {
        if (qCompleter && qCompleter->popup()->isVisible()) {
            // The following keys are forwarded by the completer to the widget
            switch (e->key()) {
            case Qt::Key_Enter:
            case Qt::Key_Return:
            case Qt::Key_Escape:
            case Qt::Key_Tab:
            case Qt::Key_Backtab:
                e->ignore();
                return; // let the completer do default behavior
            default:
                break;
            }
            QTextEdit::keyPressEvent(e);
            QString completionPrefix = textUnderCursor();
            complete(completionPrefix);
        } else {
            if ((e->key() == Qt::Key_Tab)
                || (QApplication::keyboardModifiers() && Qt::ControlModifier
                    && (e->key() == Qt::Key_Space))) {
                QString completionPrefix = textUnderCursor();
                complete(completionPrefix);
            } else {
                if ((e->key() == Qt::Key_A) && Qt::ControlModifier) {
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
                    cursor.endEditBlock();
                    setTextCursor(cursor);
                }
            }
        }
    }
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
    bool isPathCompletion = (completion.lastIndexOf(FILE_OR_DIR) != -1);
    wstringVector postfixStrings;
    postfixStrings.push_back(POSTFIX_BUILTIN);
    postfixStrings.push_back(POSTFIX_MACRO);
    postfixStrings.push_back(POSTFIX_VARIABLE);
    postfixStrings.push_back(POSTFIX_FIELD);
    postfixStrings.push_back(POSTFIX_PROPERTY);
    postfixStrings.push_back(POSTFIX_METHOD);

    QString cleanedCompletion = completion;
    for (const std::wstring& postfix : postfixStrings) {
        QString beforeString = wstringToQString(std::wstring(L" (") + postfix + std::wstring(L")"));
        cleanedCompletion = cleanedCompletion.replace(
            cleanedCompletion.lastIndexOf(beforeString), beforeString.size(), QString());
    }
    cleanedCompletion = cleanedCompletion.replace(
        cleanedCompletion.lastIndexOf(FILE_OR_DIR), FILE_OR_DIR.size(), QString());

    if (qCompleter->widget() != this) {
        return;
    }
    QTextCursor tc = textCursor();
    QString completionPrefix = qCompleter->completionPrefix();
    std::wstring currentLineW = QStringTowstring(textUnderCursor());

    std::wstring cleanedCompletionW = QStringTowstring(cleanedCompletion);
    std::wstring fileSearchedPattern = getPartialLineAsPath(currentLineW);
    std::wstring searchedPattern = getPartialLine(currentLineW);
    std::wstring newLine = completerLine(
        currentLineW, cleanedCompletionW, fileSearchedPattern, searchedPattern, isPathCompletion);
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
        textPosition.insertText("// ");
        textPosition.movePosition(QTextCursor::Down, QTextCursor::MoveAnchor);
    }
    textPosition.insertText("// ");
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
