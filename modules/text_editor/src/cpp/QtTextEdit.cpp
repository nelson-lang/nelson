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
#include "What.hpp"
#include "QStringConverter.hpp"
#include "FilesCompleter.hpp"
#include "CompleterHelper.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
#define POSTFIX_BUILTIN _W("Builtin")
#define POSTFIX_MACRO _W("Macro")
#define POSTFIX_VARIABLE _W("Variable")
#define POSTFIX_FILES _W("File or directory")
//=============================================================================
QtTextEdit::QtTextEdit()
{
    setLineWrapMode(QTextEdit::NoWrap);
	if (qCompleter == nullptr)
	{
		qCompleter = new QCompleter(this);
		qCompleter->setModelSorting(QCompleter::UnsortedModel);
		qCompleter->setCompletionMode(QCompleter::UnfilteredPopupCompletion);
		qCompleter->setCaseSensitivity(Qt::CaseSensitive);
		qCompleter->setWrapAround(false);
		QObject::connect(qCompleter, SIGNAL(activated(QString)), this, SLOT(insertCompletion(QString)));
	}
}
//=============================================================================
void QtTextEdit::updateModel(QString prefix)
{
	if (qCompleter != nullptr)
	{
		qCompleter->setModel(modelFromNelson(prefix));
		qCompleter->setCompletionPrefix(prefix);
	}
}
//=============================================================================
QtTextEdit::~QtTextEdit()
{
}
//=============================================================================
QAbstractItemModel *QtTextEdit::modelFromNelson(QString prefix)
{
	QStringList words;
	if (!prefix.isEmpty())
		{
			wstringVector files = FilesCompleter(QStringTowstring(prefix), true);
			for (size_t k = 0; k < files.size(); ++k)
			{
				words.append(wstringToQString(files[k]) + QString(" (") + wstringToQString(POSTFIX_FILES) + QString(")"));
			}

			wstringVector builtin = WhatListOfBuiltin();
			for (size_t k = 0; k < builtin.size(); ++k)
			{
				if (wstringToQString(builtin[k]).startsWith(prefix))
				{
					words.append(wstringToQString(builtin[k]) + QString(" (") + wstringToQString(POSTFIX_BUILTIN) + QString(")"));
				}
			}
			wstringVector macros = WhatListOfMacro();
			for (size_t k = 0; k < macros.size(); ++k)
			{
				if (wstringToQString(macros[k]).startsWith(prefix))
				{
					words.append(wstringToQString(macros[k]) + QString(" (") + wstringToQString(POSTFIX_MACRO) + QString(")"));
				}
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
			if (!completionPrefix.isEmpty())
			{
				std::wstring completionPrefixW = QStringTowstring(completionPrefix);
				std::wstring filepart = getPartialLineAsPath(completionPrefixW);
				std::wstring textpart = getPartialLine(completionPrefixW);
				if (!filepart.empty() || !textpart.empty())
				{
					if (!filepart.empty())
					{
						updateModel(wstringToQString(filepart));
					}
					else
					{
						updateModel(wstringToQString(textpart));
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
		else
		{
			if (e->key() == Qt::Key_Tab) {
				QString completionPrefix = textUnderCursor();
				if (!completionPrefix.isEmpty())
				{
					std::wstring completionPrefixW = QStringTowstring(completionPrefix);
					std::wstring filepart = getPartialLineAsPath(completionPrefixW);
					std::wstring textpart = getPartialLine(completionPrefixW);
					if (!filepart.empty() || !textpart.empty())
					{
						if (!filepart.empty())
						{
							updateModel(wstringToQString(filepart));
						}
						else
						{
							updateModel(wstringToQString(textpart));
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
