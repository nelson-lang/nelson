//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include <QtWidgets/QAbstractItemView>
#include <QtWidgets/QApplication>
#include <QtWidgets/QCompleter>
#include <QtWidgets/QScrollBar>
#include <QtGui/QKeyEvent>
#include <QtCore/QStringListModel>
#include "QtTextEdit.h"
#include "QStringConverter.hpp"
#include "FileCompleter.hpp"
#include "MacroCompleter.hpp"
#include "BuiltinCompleter.hpp"
#include "VariableCompleter.hpp"
#include "CompleterHelper.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
QtTextEdit::QtTextEdit()
{
    setLineWrapMode(QTextEdit::NoWrap);
}
//=============================================================================
void QtTextEdit::setCompleter(QCompleter *completer)
{
    qCompleter = completer;
    if (!qCompleter)
    {
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
void QtTextEdit::updateModel(QString prefix, bool withPath)
{
    if (qCompleter != nullptr)
    {
        qCompleter->setModel(modelFromNelson(prefix, withPath));
        qCompleter->setCompletionPrefix(prefix);
    }
}
//=============================================================================
QtTextEdit::~QtTextEdit()
{
}
//=============================================================================
QAbstractItemModel *QtTextEdit::modelFromNelson(QString prefix, bool withPath)
{
    QStringList words;
    if (!prefix.isEmpty())
    {
        if (withPath)
        {
            wstringVector files = FileCompleter(QStringTowstring(prefix));
            for (size_t k = 0; k < files.size(); ++k)
            {
                words.append(wstringToQString(files[k]) + QString(" (") + wstringToQString(POSTFIX_FILES) + QString(")"));
            }
        }
        wstringVector builtin = BuiltinCompleter(QStringTowstring(prefix));
        for (size_t k = 0; k < builtin.size(); ++k)
        {
            words.append(wstringToQString(builtin[k]) + QString(" (") + wstringToQString(POSTFIX_BUILTIN) + QString(")"));
        }
        wstringVector macros = MacroCompleter(QStringTowstring(prefix));
        for (size_t k = 0; k < macros.size(); ++k)
        {
            words.append(wstringToQString(macros[k]) + QString(" (") + wstringToQString(POSTFIX_MACRO) + QString(")"));
        }
        wstringVector variables = VariableCompleter(QStringTowstring(prefix));
        for (size_t k = 0; k < variables.size(); ++k)
        {
            words.append(wstringToQString(variables[k]) + QString(" (") + wstringToQString(POSTFIX_VARIABLE) + QString(")"));
        }
        words.sort();
    }
    return new QStringListModel(words, qCompleter);
}
//=============================================================================
void QtTextEdit::keyPressEvent(QKeyEvent *e)
{
    bool tab = false;
    int keycode = e->key();
    if ((keycode == Qt::Key_S) && QApplication::keyboardModifiers() && Qt::ControlModifier)
    {
        e->accept();
    }
    else
    {
        if (qCompleter && qCompleter->popup()->isVisible())
        {
            // The following keys are forwarded by the completer to the widget
            switch (e->key())
            {
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
        }
        else
        {
            if (e->key() == Qt::Key_Tab)
            {
                QString completionPrefix = textUnderCursor();
                complete(completionPrefix);
            }
            else
            {
                QTextEdit::keyPressEvent(e);
            }
        }
        /*
        if (keycode)
        {
        	QByteArray p(e->text().toUtf8());
        	char key;
        	if (!e->text().isEmpty())
        	{
        		key = p[0];
        	}
        	else
        	{
        		key = 0;
        	}
        	if (key == 0x09)
        	{
        		tab = true;
        		emit indent();
        	}
        }
        if (!tab)
        {
        	QTextEdit::keyPressEvent(e);
        }
        else
        {
        	e->accept();
        }
        */
    }
}
//=============================================================================
void QtTextEdit::complete(QString prefix)
{
    if (!prefix.isEmpty())
    {
        std::wstring completionPrefixW = QStringTowstring(prefix);
        std::wstring filepart = getPartialLineAsPath(completionPrefixW);
        std::wstring textpart = getPartialLine(completionPrefixW);
        if (!filepart.empty() || !textpart.empty())
        {
            if (!filepart.empty())
            {
                updateModel(wstringToQString(filepart), true);
            }
            else
            {
                updateModel(wstringToQString(textpart), false);
            }
        }
        QRect cr = cursorRect();
        cr.setWidth(qCompleter->popup()->sizeHintForColumn(0) + qCompleter->popup()->verticalScrollBar()->sizeHint().width());
        cr.setHeight(10);
        qCompleter->complete(cr);
        qCompleter->setCurrentRow(0);
        qCompleter->popup()->setCurrentIndex(qCompleter->completionModel()->index(0, 0));
    }
}
//=============================================================================
void QtTextEdit::contextMenuEvent(QContextMenuEvent* e)
{
    e->ignore();
}
//=============================================================================
void QtTextEdit::insertCompletion(const QString& completion)
{
    QString FILE_OR_DIR = QString(" (") + wstringToQString(POSTFIX_FILES) + QString(")");
    bool isPathCompletion = (completion.lastIndexOf(FILE_OR_DIR) != -1);
    QString cleanedCompletion = completion;
    QString beforeString = QString(" (") + wstringToQString(POSTFIX_BUILTIN) + QString(")");
    cleanedCompletion = cleanedCompletion.replace(cleanedCompletion.lastIndexOf(beforeString), beforeString.size(), QString());
    beforeString = QString(" (") + wstringToQString(POSTFIX_MACRO) + QString(")");
    cleanedCompletion = cleanedCompletion.replace(cleanedCompletion.lastIndexOf(beforeString), beforeString.size(), QString());
    beforeString = QString(" (") + wstringToQString(POSTFIX_VARIABLE) + QString(")");
    cleanedCompletion = cleanedCompletion.replace(cleanedCompletion.lastIndexOf(beforeString), beforeString.size(), QString());
    cleanedCompletion = cleanedCompletion.replace(cleanedCompletion.lastIndexOf(FILE_OR_DIR), FILE_OR_DIR.size(), QString());
    if (qCompleter->widget() != this)
    {
        return;
    }
    QTextCursor tc = textCursor();
    QString completionPrefix = qCompleter->completionPrefix();
    std::wstring currentLineW = QStringTowstring(textUnderCursor());
    std::wstring completionPrefixW = QStringTowstring(completionPrefix);
    std::wstring cleanedCompletionW = QStringTowstring(cleanedCompletion);
    std::wstring fileSearchedPattern = getPartialLineAsPath(currentLineW);
    std::wstring searchedPattern = getPartialLine(currentLineW);
    std::wstring newLine = completerLine(currentLineW, cleanedCompletionW, fileSearchedPattern, searchedPattern, isPathCompletion);
    tc.select(QTextCursor::LineUnderCursor);
    tc.removeSelectedText();
    tc.insertText(wstringToQString(newLine));
    setTextCursor(tc);
}
//=============================================================================
void QtTextEdit::focusInEvent(QFocusEvent *e)
{
    if (qCompleter)
    {
        qCompleter->setWidget(this);
    }
    QTextEdit::focusInEvent(e);
}
//=============================================================================
QString QtTextEdit::textUnderCursor() const
{
    QTextCursor tc = textCursor();
    tc.select(QTextCursor::LineUnderCursor);
    return tc.selectedText();
}
//=============================================================================
void QtTextEdit::comment()
{
    QTextCursor textCursor(textCursor());
    QTextCursor lineOne(textCursor);
    QTextCursor lineTwo(textCursor);
    if (textCursor.position() < textCursor.anchor())
    {
        lineTwo.setPosition(textCursor.anchor());
    }
    else
    {
        lineOne.setPosition(textCursor.anchor());
    }
    lineOne.movePosition(QTextCursor::StartOfLine, QTextCursor::MoveAnchor);
    QTextCursor lineTwoCopy(lineTwo);
    lineTwo.movePosition(QTextCursor::StartOfLine, QTextCursor::MoveAnchor);
    if (lineTwo.position() == lineTwoCopy.position())
    {
        lineTwo.movePosition(QTextCursor::Up, QTextCursor::MoveAnchor);
    }
    QTextCursor textPosition(lineOne);
    textPosition.beginEditBlock();
    while (textPosition.position() < lineTwo.position())
    {
        textPosition.insertText("// ");
        textPosition.movePosition(QTextCursor::Down, QTextCursor::MoveAnchor);
    }
    textPosition.insertText("// ");
    textPosition.endEditBlock();
}
//=============================================================================
void QtTextEdit::uncomment()
{
    QTextCursor textCursor(textCursor());
    QTextCursor lineOne(textCursor);
    QTextCursor lineTwo(textCursor);
    if (textCursor.position() < textCursor.anchor())
    {
        lineTwo.setPosition(textCursor.anchor());
    }
    else
    {
        lineOne.setPosition(textCursor.anchor());
    }
    lineOne.movePosition(QTextCursor::StartOfLine, QTextCursor::MoveAnchor);
    QTextCursor lineTwoCopy(lineTwo);
    lineTwo.movePosition(QTextCursor::StartOfLine, QTextCursor::MoveAnchor);
    if (lineTwo.position() == lineTwoCopy.position())
    {
        lineTwo.movePosition(QTextCursor::Up, QTextCursor::MoveAnchor);
    }
    QTextCursor textPosition(lineOne);
    textPosition.beginEditBlock();
    while (textPosition.position() <= lineTwo.position())
    {
        while (!textPosition.atBlockEnd())
        {
            textPosition.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor);
            if (textPosition.selectedText() == " ")
            {
                textPosition.clearSelection();
                continue;
            }
            else if (textPosition.selectedText() == "%")
            {
                textPosition.deleteChar();
                textPosition.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor);
                if (textPosition.selectedText() == " ")
                {
                    textPosition.deleteChar();
                }
                break;
            }
            else if (textPosition.selectedText() == "/")
            {
                textPosition.movePosition(QTextCursor::NextCharacter, QTextCursor::KeepAnchor);
                if (textPosition.selectedText() == "//")
                {
                    textPosition.deletePreviousChar();
                    textPosition.deleteChar();
                    textPosition.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor);
                    if (textPosition.selectedText() == " ")
                    {
                        textPosition.deleteChar();
                    }
                    break;
                }
            }
            else
            {
                break;
            }
        }
        if (textPosition.position() < lineTwo.position())
        {
            textPosition.movePosition(QTextCursor::Down, QTextCursor::MoveAnchor);
            textPosition.movePosition(QTextCursor::StartOfLine, QTextCursor::MoveAnchor);
        }
    }
    textPosition.endEditBlock();
}
//=============================================================================
