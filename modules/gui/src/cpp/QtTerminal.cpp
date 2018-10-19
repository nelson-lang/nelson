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
#include <QtCore/QMimeData>
#include <QtGui/QClipboard>
#include <QtGui/QImageReader>
#include <QtGui/QKeyEvent>
#include <QtGui/QTextBlock>
#include <QtGui/QTextCursor>
#include <QtGui/QTextDocument>
#include <QtGui/QTextImageFormat>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QMenu>
#include <QtWidgets/QScrollBar>
#include <boost/algorithm/string.hpp>
#include "QtTerminal.h"
#include "Evaluator.hpp"
#include "GetNelsonMainEvaluatorDynamicFunction.hpp"
#include "GetNelsonPath.hpp"
#include "NelsonHistory.hpp"
#include "ProcessEvents.hpp"
#include "QStringConverter.hpp"
#include "QtTranslation.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static Nelson::Evaluator* eval = nullptr;
//=============================================================================
QtTerminal::QtTerminal(QWidget* parent) : QTextBrowser(parent)
{
    mCommandLineReady = false;
    QLocale us(QLocale::English, QLocale::UnitedStates);
    QLocale::setDefault(us);
    QPalette p = palette();
    p.setColor(QPalette::Active, QPalette::Base, Qt::white);
    p.setColor(QPalette::Inactive, QPalette::Base, Qt::white);
    p.setColor(QPalette::Active, QPalette::Text, Qt::black);
    p.setColor(QPalette::Inactive, QPalette::Text, Qt::black);
    setPalette(p);
#ifdef __APPLE__
    QFont f("Monaco");
#else
    QFont f("Monospace");
#endif
    f.setStyleHint(QFont::TypeWriter);
    f.setPointSize(10);
    f.setFixedPitch(true);
    setFont(f);
    setCursorWidth(QFontMetrics(font()).width(QChar('x')));
    setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    setUndoRedoEnabled(false);
    setWordWrapMode(QTextOption::NoWrap);
    setFrameStyle(QFrame::NoFrame);
    setTabStopWidth(40);
    setAcceptDrops(false);
    warningColor = QColor(255, 128, 0);
    inputColor = QColor(0, 0, 255);
    errorColor = QColor(255, 0, 0);
    outputColor = QColor(0, 0, 0);
    lineToSend.clear();
    // disable cursor
    setCursorWidth(0);
    setTextInteractionFlags(Qt::TextSelectableByMouse | Qt::TextSelectableByKeyboard
        | Qt::LinksAccessibleByMouse | Qt::LinksAccessibleByKeyboard | Qt::TextEditable);
    setOpenExternalLinks(true);
    mCommandLineReady = true;
    document()->setMaximumBlockCount(0);
    nelsonPath = Nelson::wstringToQString(Nelson::GetRootPath());
    helpOnSelectionAction = nullptr;
    cutAction = nullptr;
    copyAction = nullptr;
    pasteAction = nullptr;
    selectAllAction = nullptr;
    clcAction = nullptr;
    stopAction = nullptr;
    contextMenu = nullptr;
}
//=============================================================================
QtTerminal::~QtTerminal()
{
    if (cutAction) {
        delete cutAction;
        cutAction = nullptr;
    }
    if (copyAction) {
        delete copyAction;
        copyAction = nullptr;
    }
    if (pasteAction) {
        delete pasteAction;
        pasteAction = nullptr;
    }
    if (selectAllAction) {
        delete selectAllAction;
        selectAllAction = nullptr;
    }
    if (clcAction) {
        delete clcAction;
        clcAction = nullptr;
    }
    if (stopAction) {
        delete stopAction;
        stopAction = nullptr;
    }
    if (contextMenu) {
        delete contextMenu;
        contextMenu = nullptr;
    }
}
//=============================================================================
void
QtTerminal::banner()
{
    mCommandLineReady = false;
    QString nelsonPath = Nelson::wstringToQString(Nelson::GetRootPath());
    QString fileName = nelsonPath + "/resources/banner_nelson.png";
    textCursor().insertBlock();
    QFile qfile(fileName);
    if (qfile.exists()) {
        QImageReader reader(fileName);
        if (reader.canRead()) {
            QImage image = reader.read();
            if (!image.isNull()) {
                QTextImageFormat imageFormat;
                imageFormat.setName(fileName);
                textCursor().insertImage(imageFormat);
                textCursor().insertBlock();
            }
            repaint();
        }
    }
}
//=============================================================================
void
QtTerminal::printPrompt(QString prompt)
{
    mPrompt = prompt;
    QTextCursor cur(document()->lastBlock());
    if (!document()->lastBlock().text().isEmpty()) {
        cur.movePosition(QTextCursor::EndOfBlock);
        cur.insertBlock();
    }
    QTextCharFormat fmt;
    fmt.setForeground(inputColor);
    cur.setCharFormat(fmt);
    cur.insertText(mPrompt);
    cur.setCharFormat(QTextCharFormat());
    setTextCursor(cur);
    mCommandLineReady = true;
}
//=============================================================================
bool
QtTerminal::isInEditionZone()
{
    QTextBlock qtb = document()->lastBlock();
    int promptp = promptBlock.position();
    int curp = textCursor().position();
    return mCommandLineReady && ((curp - mPrompt.size()) >= promptp);
}
//=============================================================================
void
QtTerminal::sendReturnKey()
{
    lineToSend = L"\n";
}
//=============================================================================
std::wstring
QtTerminal::getLine(std::wstring prompt)
{
    printPrompt(Nelson::wstringToQString(prompt));
    promptBlock = document()->lastBlock();
    // enable cursor text
    setCursorWidth(QFontMetrics(font()).width(QChar('x')));
    // restore default icon cursor
    QApplication::restoreOverrideCursor();
    if (eval == nullptr) {
        void* veval = GetNelsonMainEvaluatorDynamicFunction();
        eval = (Nelson::Evaluator*)veval;
    }
    bool wasInterruptByAction = false;
    while (lineToSend.empty()) {
        Nelson::ProcessEvents(true);
        if (!eval->commandQueue.isEmpty()) {
            wasInterruptByAction = true;
            break;
        }
    }
    std::wstring line;
    if (wasInterruptByAction) {
        clearLine();
        line = L"\n";
    } else {
        line = lineToSend;
    }
    if (!wasInterruptByAction) {
        while (lineToSend.empty()) {
            Nelson::ProcessEvents(true);
        }
    }
    lineToSend.clear();
    Nelson::ProcessEvents();
    // disable cursor text
    setCursorWidth(0);
    // change icon cursor to wait (computation)
    QApplication::setOverrideCursor(Qt::WaitCursor);
    return line;
}
//=============================================================================
size_t
QtTerminal::getTerminalWidth()
{
    size_t chSize = QFontMetrics(font()).width(QChar('M'));
    QScrollBar* vsb = verticalScrollBar();
    size_t res = (document()->textWidth() - vsb->width()) / chSize;
    return res;
}
//=============================================================================
void
QtTerminal::outputMessage(std::wstring msg)
{
    printMessage(Nelson::wstringToQString(msg), STDOUT_DISP);
    Nelson::ProcessEvents();
}
//=============================================================================
void
QtTerminal::errorMessage(std::wstring msg)
{
    printMessage(Nelson::wstringToQString(msg), STDERR_DISP);
    Nelson::ProcessEvents();
}
//=============================================================================
void
QtTerminal::warningMessage(std::wstring msg)
{
    printMessage(Nelson::wstringToQString(msg), WARNING_DISP);
    Nelson::ProcessEvents();
}
//=============================================================================
void
QtTerminal::clearTerminal()
{
    this->clear();
    if (mCommandLineReady) {
        QTextCursor cur(document()->lastBlock());
        cur.movePosition(QTextCursor::EndOfBlock);
        cur.insertBlock();
        setTextCursor(cur);
        printPrompt(mPrompt);
        promptBlock = document()->lastBlock();
    }
}
//=============================================================================
bool
QtTerminal::handlePreviousCharKeyPress()
{
    QTextCursor cur = textCursor();
    const int col = cur.columnNumber();
    if ((promptBlock == cur.block()) && (col == mPrompt.size())) {
        return true;
    }
    return false;
}
//=============================================================================
bool
QtTerminal::handleBackspaceKeyPress()
{
    QTextCursor cur = textCursor();
    const int col = cur.columnNumber();
    if ((promptBlock == cur.block()) && (col == mPrompt.size())) {
        return true;
    }
    return false;
}
//=============================================================================
bool
QtTerminal::handleHomePress()
{
    if (isInEditionZone()) {
        QTextCursor cursor = QTextCursor(promptBlock);
        cursor.movePosition(QTextCursor::StartOfBlock);
        cursor.movePosition(QTextCursor::Right, QTextCursor::MoveAnchor, mPrompt.length());
        setTextCursor(cursor);
        return true;
    }
    return false;
}
//=============================================================================
bool
QtTerminal::handleUpKeyPress()
{
    std::wstring line = Nelson::History::getPreviousLine();
    return replaceCurrentCommandLine(line);
}
//=============================================================================
bool
QtTerminal::handleDownKeyPress()
{
    std::wstring line = Nelson::History::getNextLine();
    return replaceCurrentCommandLine(line);
}
//=============================================================================
void
QtTerminal::sendKeyEvent(QKeyEvent* event)
{
    keyPressEvent(event);
}
//=============================================================================
void
QtTerminal::keyPressEvent(QKeyEvent* event)
{
    if (!isInEditionZone()) {
        QTextCursor cur = textCursor();
        cur.movePosition(QTextCursor::End);
        setTextCursor(cur);
        if ((event->key() == Qt::Key_Backspace) || (event->key() == Qt::Key_Delete)
            || event->matches(QKeySequence::MoveToPreviousLine)
            || event->matches(QKeySequence::MoveToNextLine)) {
            return;
        }
    }
    if (event->matches(QKeySequence::MoveToStartOfLine)) {
        handleHomePress();
        return;
    } else if (event->matches(QKeySequence::MoveToPreviousLine)) {
        handleUpKeyPress();
        return;
    } else if (event->matches(QKeySequence::MoveToNextLine)) {
        handleDownKeyPress();
        return;
    } else if (event->matches(QKeySequence::MoveToPreviousChar)) {
        if (handleBackspaceKeyPress()) {
            return;
        }
    } else if (event->key() == Qt::Key_Backspace) {
        if (handleBackspaceKeyPress()) {
            return;
        }
        updateHistoryToken();
    } else if (event->key() == Qt::Key_Delete) {
        updateHistoryToken();
        if (!isInEditionZone()) {
            return;
        }
    } else if (event->key() == Qt::Key_Return || event->key() == Qt::Key_Enter) {
        if (isInEditionZone()) {
            QString cmd = getCurrentCommandLine();
            lineToSend = Nelson::QStringTowstring(cmd) + L"\n";
            QTextCursor cur(document()->lastBlock());
            cur.movePosition(QTextCursor::EndOfBlock);
            cur.insertBlock();
            setTextCursor(cur);
        }
        return;
    } else if (event->matches(QKeySequence::Copy)) {
        if (!mCommandLineReady) {
            if (eval == nullptr) {
                void* veval = GetNelsonMainEvaluatorDynamicFunction();
                eval = (Nelson::Evaluator*)veval;
            }
            if (eval) {
                NelsonConfiguration::getInstance()->setInterruptPending(true);
            }
        }
    }
    if (isInEditionZone()) {
        QTextBrowser::keyPressEvent(event);
    }
    updateHistoryToken();
}
//=============================================================================
QString
QtTerminal::getCurrentCommandLine()
{
    QString command;
    QTextBlock curBlock = promptBlock;
    while (curBlock != document()->lastBlock()) {
        if (!curBlock.isValid()) {
            command.clear();
            break;
        }
        command = command.append(curBlock.text());
        command = command.append("\n");
        curBlock = curBlock.next();
    }
    if (!command.isEmpty()) {
        command = command.append("\n");
    }
    command = command.append(curBlock.text());
    command.replace(QString(QChar(8233)), QString("\n"));
    if (command.startsWith(mPrompt)) {
        command.remove(0, mPrompt.length());
    }
    if (!command.isEmpty()) {
        mCommandLineReady = false;
    }
    return command;
}
//=============================================================================
bool
QtTerminal::replaceCurrentCommandLine(std::wstring newline)
{
    QTextCursor cursor = textCursor();
    cursor.movePosition(QTextCursor::StartOfLine);
    cursor.movePosition(QTextCursor::Right, QTextCursor::MoveAnchor, mPrompt.length());
    cursor.movePosition(QTextCursor::EndOfLine, QTextCursor::KeepAnchor);
    cursor.insertText(Nelson::wstringToQString(newline));
    return true;
}
//=============================================================================
void
QtTerminal::printNewLine()
{
    QTextCursor cursor = textCursor();
    cursor.insertBlock();
    setTextCursor(cursor);
}
//=============================================================================
void
QtTerminal::printMessage(QString msg, DISP_MODE mode)
{
    mCommandLineReady = false;
    QTextCursor cur(document()->lastBlock());
    QTextCharFormat format = cur.charFormat();
    switch (mode) {
    case WARNING_DISP: {
        format.setForeground(warningColor);
    } break;
    case STDOUT_DISP: {
        format.setForeground(outputColor);
    } break;
    case STDERR_DISP: {
        format.setForeground(errorColor);
    } break;
    case STDIN_DISP: {
        format.setForeground(inputColor);
    } break;
    }
    cur.movePosition(QTextCursor::EndOfBlock, QTextCursor::MoveAnchor, 1);
    cur.insertText(msg, format);
    setTextCursor(cur);
}
//=============================================================================
void
QtTerminal::closeEvent(QCloseEvent* event)
{
    if (eval == nullptr) {
        void* veval = GetNelsonMainEvaluatorDynamicFunction();
        eval = (Nelson::Evaluator*)veval;
    }
    if (eval) {
        NelsonConfiguration::getInstance()->setInterruptPending(true);
    }
    lineToSend = L"exit";
}
//=============================================================================
void
QtTerminal::insertFromMimeData(const QMimeData* source)
{
    if (!isInEditionZone()) {
        QTextCursor cur = textCursor();
        cur.movePosition(QTextCursor::End);
        setTextCursor(cur);
    }
    if (source->hasText()) {
        QTextBrowser::insertPlainText(source->text());
    }
    updateHistoryToken();
}
//=============================================================================
void
QtTerminal::insertHtml(std::wstring msg)
{
    textCursor().insertHtml(Nelson::wstringToQString(msg));
    textCursor().insertBlock();
    updateHistoryToken();
}
//=============================================================================
void
QtTerminal::cut()
{
    if (isInEditionZone()) {
        QTextEdit::cut();
        updateHistoryToken();
        return;
    }
}
//=============================================================================
void
QtTerminal::copy()
{
    QTextBrowser::copy();
}
//=============================================================================
void
QtTerminal::helpOnSelection()
{
    QTextCursor cur = textCursor();
    if (cur.hasSelection()) {
        QString textSelected = cur.selectedText();
        if (!textSelected.isEmpty()) {
            std::wstring text = QStringTowstring(textSelected);
            boost::algorithm::replace_all(text, "'", "\"");
            std::wstring cmd = L"doc('" + text + L"');";
            if (eval == nullptr) {
                void* veval = GetNelsonMainEvaluatorDynamicFunction();
                eval = (Nelson::Evaluator*)veval;
            }
            try {
                eval->evaluateString(cmd, true);
            } catch (const Exception&) {
            }
        }
    }
}
//=============================================================================
void
QtTerminal::paste()
{
    if (!isInEditionZone()) {
        QTextCursor cur = textCursor();
        cur.movePosition(QTextCursor::End);
        setTextCursor(cur);
    }
    QTextBrowser::paste();
    updateHistoryToken();
}
//=============================================================================
void
QtTerminal::selectAll()
{
    QTextBrowser::selectAll();
}
//=============================================================================
void
QtTerminal::clc()
{
    clearTerminal();
    updateHistoryToken();
}
//=============================================================================
void
QtTerminal::clearLine()
{
    QTextCursor cursor = textCursor();
    cursor.movePosition(QTextCursor::StartOfLine);
    cursor.movePosition(QTextCursor::Right, QTextCursor::MoveAnchor, 0);
    cursor.movePosition(QTextCursor::EndOfLine, QTextCursor::KeepAnchor);
    cursor.insertText(Nelson::wstringToQString(L""));
}
//=============================================================================
void
QtTerminal::stopRun()
{
    if (eval == nullptr) {
        void* veval = GetNelsonMainEvaluatorDynamicFunction();
        eval = (Nelson::Evaluator*)veval;
    }
    if (eval) {
        NelsonConfiguration::getInstance()->setInterruptPending(true);
    }
}
//=============================================================================
void
QtTerminal::contextMenuEvent(QContextMenuEvent* event)
{
    if (contextMenu == nullptr) {
        try {
            contextMenu = new QMenu(this);
        } catch (const std::bad_alloc&) {
            contextMenu = nullptr;
        }
        QString fileNameIcon;
        if (helpOnSelectionAction == nullptr) {
            fileNameIcon = nelsonPath + QString("/resources/help-icon.svg");
            helpOnSelectionAction = new QAction(QIcon(fileNameIcon), TR("Help on Selection"), this);
            helpOnSelectionAction->setShortcuts(QKeySequence::HelpContents);
            contextMenu->addAction(helpOnSelectionAction);
            connect(helpOnSelectionAction, SIGNAL(triggered()), this, SLOT(helpOnSelection()));
            contextMenu->addSeparator();
        }
        if (cutAction == nullptr) {
            fileNameIcon = nelsonPath + QString("/resources/edit-cut.svg");
            cutAction = new QAction(QIcon(fileNameIcon), TR("Cut"), this);
            cutAction->setShortcut(TR("Ctrl + X"));
            contextMenu->addAction(cutAction);
            connect(cutAction, SIGNAL(triggered()), this, SLOT(cut()));
        }
        if (copyAction == nullptr) {
            fileNameIcon = nelsonPath + QString("/resources/edit-copy.svg");
            copyAction = new QAction(QIcon(fileNameIcon), TR("Copy"), this);
            copyAction->setShortcut(TR("Ctrl + Ins"));
            contextMenu->addAction(copyAction);
            connect(copyAction, SIGNAL(triggered()), this, SLOT(copy()));
        }
        if (pasteAction == nullptr) {
            fileNameIcon = nelsonPath + QString("/resources/edit-paste.svg");
            pasteAction = new QAction(QIcon(fileNameIcon), TR("Paste"), this);
            pasteAction->setShortcut(TR("Ctrl + V"));
            contextMenu->addAction(pasteAction);
            connect(pasteAction, SIGNAL(triggered()), this, SLOT(paste()));
        }
        if (selectAllAction == nullptr) {
            fileNameIcon = nelsonPath + QString("/resources/edit-select-all.svg");
            selectAllAction = new QAction(QIcon(fileNameIcon), TR("Select All"), this);
            selectAllAction->setShortcut(TR("Ctrl + A"));
            contextMenu->addAction(selectAllAction);
            connect(selectAllAction, SIGNAL(triggered()), this, SLOT(selectAll()));
        }
        contextMenu->addSeparator();
        if (clcAction == nullptr) {
            fileNameIcon = nelsonPath + QString("/resources/console-clear.svg");
            clcAction = new QAction(QIcon(fileNameIcon), TR("Clear Command Window"), this);
            contextMenu->addAction(clcAction);
            connect(clcAction, SIGNAL(triggered()), this, SLOT(clc()));
        }
        contextMenu->addSeparator();
        if (stopAction == nullptr) {
            fileNameIcon = nelsonPath + QString("/resources/stop-interpreter.svg");
            stopAction = new QAction(QIcon(fileNameIcon), TR("Stop execution"), this);
            contextMenu->addSeparator();
            contextMenu->addAction(stopAction);
            connect(stopAction, SIGNAL(triggered()), this, SLOT(stopRun()));
        }
    }
    QTextCursor cur = textCursor();
    if (cur.hasSelection()) {
        helpOnSelectionAction->setEnabled(true);
    } else {
        helpOnSelectionAction->setEnabled(false);
    }
    if (cur.hasSelection() && isInEditionZone()) {
        cutAction->setEnabled(true);
    } else {
        cutAction->setEnabled(false);
    }
    if (cur.hasSelection()) {
        copyAction->setEnabled(true);
    } else {
        copyAction->setEnabled(false);
    }
    QClipboard* Clipboard = QApplication::clipboard();
    QString clipboardText = Clipboard->text();
    if (clipboardText.isEmpty()) {
        pasteAction->setEnabled(false);
    } else {
        pasteAction->setEnabled(true);
    }
    if (!isAtPrompt()) {
        stopAction->setVisible(true);
    } else {
        stopAction->setVisible(false);
    }
    contextMenu->exec(event->globalPos());
}
//=============================================================================
bool
QtTerminal::isAtPrompt()
{
    return mCommandLineReady;
}
//=============================================================================
int
QtTerminal::setMaxBlockCount(int newMax)
{
    int previous = getMaxBlockCount();
    document()->setMaximumBlockCount(newMax);
    return previous;
}
//=============================================================================
int
QtTerminal::getMaxBlockCount()
{
    return document()->maximumBlockCount();
}
//=============================================================================
bool
QtTerminal::updateHistoryToken()
{
    if (isInEditionZone()) {
        bool bBackup = mCommandLineReady;
        QString cmd = getCurrentCommandLine();
        mCommandLineReady = bBackup;
        std::wstring currentline = Nelson::QStringTowstring(cmd);
        Nelson::History::setToken(currentline);
        return true;
    }
    return false;
}
//=============================================================================
