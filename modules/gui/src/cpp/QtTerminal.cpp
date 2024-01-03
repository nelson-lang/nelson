//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QtGlobal>
#include <QtCore/QLocale>
#include <QtCore/QMimeData>
#include <QtCore/QFile>
#include <QtGui/QClipboard>
#include <QtGui/QImageReader>
#include <QtGui/QKeyEvent>
#include <QtGui/QTextBlock>
#include <QtGui/QTextCursor>
#include <QtGui/QTextDocument>
#include <QtGui/QTextImageFormat>
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
#include <QtGui/QAction>
#else
#include <QtWidgets/QAction>
#endif
#include <QtWidgets/QApplication>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QMenu>
#include <QtWidgets/QScrollBar>
#include <QtCore/QStringListModel>
#include <QtWidgets/QAbstractItemView>
#include "QtTerminal.h"
#include "Evaluator.hpp"
#include "NelsonHistory.hpp"
#include "ProcessEvents.hpp"
#include "QStringConverter.hpp"
#include "QtTranslation.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
#include "PostCommand.hpp"
#include "NelsonPalette.hpp"
#include "NelsonColors.hpp"
#include "DefaultFont.hpp"
#include "StringHelpers.hpp"

#include "BuiltinCompleter.hpp"
#include "CompleterHelper.hpp"
#include "FileCompleter.hpp"
#include "MacroCompleter.hpp"
#include "VariableCompleter.hpp"
#include "QStringConverter.hpp"

//=============================================================================
using namespace Nelson;
//=============================================================================
QtTerminal::QtTerminal(QWidget* parent) : QTextBrowser(parent)
{
    mCommandLineReady = false;
    qCompleter = nullptr;
    QLocale us(QLocale::English, QLocale::UnitedStates);
    QLocale::setDefault(us);
    setPalette(getNelsonPalette());
    QFont f(wstringToQString(Nelson::getDefaultFontName()));
    f.setStyleHint(QFont::TypeWriter);
    f.setPointSize(10);
    f.setFixedPitch(true);
    setFont(f);
    setCursorWidth(QFontMetrics(font()).horizontalAdvance(QChar('x')));
    setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    setUndoRedoEnabled(false);
    setWordWrapMode(QTextOption::NoWrap);
    setFrameStyle(QFrame::NoFrame);
    setTabStopDistance(40);
    setAcceptDrops(false);
    lineToSend.clear();
    // disable cursor
    setCursorWidth(0);
    setTextInteractionFlags(Qt::TextSelectableByMouse | Qt::TextSelectableByKeyboard
        | Qt::LinksAccessibleByMouse | Qt::LinksAccessibleByKeyboard | Qt::TextEditable);
    setOpenExternalLinks(true);
    document()->setMaximumBlockCount(0);
    nelsonPath = Nelson::wstringToQString(
        Nelson::NelsonConfiguration::getInstance()->getNelsonRootDirectory());
    helpOnSelectionAction = nullptr;
    cutAction = nullptr;
    copyAction = nullptr;
    pasteAction = nullptr;
    selectAllAction = nullptr;
    clcAction = nullptr;
    stopAction = nullptr;
    contextMenu = nullptr;
    mCommandLineReady = true;
}
//=============================================================================
void
QtTerminal::createCompleter()
{
    if (!qCompleter) {
        qCompleter = new QCompleter(this);

        qCompleter->setWidget(this);
        qCompleter->setModelSorting(QCompleter::UnsortedModel);
        qCompleter->setCompletionMode(QCompleter::UnfilteredPopupCompletion);
        qCompleter->setCaseSensitivity(Qt::CaseSensitive);
        qCompleter->setWrapAround(false);
        QObject::connect(
            qCompleter, SIGNAL(activated(QString)), this, SLOT(insertCompletion(QString)));
    }
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
    if (qCompleter) {
        delete qCompleter;
        qCompleter = nullptr;
    }
}
//=============================================================================
void
QtTerminal::banner()
{
    mCommandLineReady = false;
    QString _nelsonPath = Nelson::wstringToQString(
        Nelson::NelsonConfiguration::getInstance()->getNelsonRootDirectory());
    QString fileName = _nelsonPath + "/resources/banner_nelson.png";
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
    fmt.setForeground(getInputColor());
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
QtTerminal::getLine(const std::wstring& prompt)
{
    printPrompt(Nelson::wstringToQString(prompt));
    promptBlock = document()->lastBlock();
    // enable cursor text
    setCursorWidth(QFontMetrics(font()).horizontalAdvance(QChar('x')));
    // restore default icon cursor
    QApplication::restoreOverrideCursor();
    void* veval = NelsonConfiguration::getInstance()->getMainEvaluator();
    Nelson::Evaluator* eval = (Nelson::Evaluator*)veval;
    eval->commandQueue.clear();
    bool wasInterruptByAction = false;
    do {
        Nelson::ProcessEvents(true);
        if (!eval->commandQueue.isEmpty()) {
            wasInterruptByAction = true;
            break;
        }
    } while (!wasInterruptByAction && lineToSend.empty());
    std::wstring line;
    if (wasInterruptByAction) {
        clearLine();
        line = L"\n";
    } else {
        line = lineToSend;
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
    size_t chSize = QFontMetrics(font()).horizontalAdvance(QChar('M'));
    QScrollBar* vsb = verticalScrollBar();
    size_t res = (document()->textWidth() - vsb->width()) / chSize;
    return res;
}
//=============================================================================
size_t
QtTerminal::getTerminalHeight()
{
    size_t chSize = QFontMetrics(font()).height();
    size_t res = document()->size().height() / chSize;
    return res;
}
//=============================================================================
void
QtTerminal::outputMessage(const std::wstring& msg)
{
    printMessage(Nelson::wstringToQString(msg), STDOUT_DISP);
    Nelson::ProcessEvents();
}
//=============================================================================
void
QtTerminal::errorMessage(const std::wstring& msg)
{
    printMessage(Nelson::wstringToQString(msg), STDERR_DISP);
    Nelson::ProcessEvents();
}
//=============================================================================
void
QtTerminal::warningMessage(const std::wstring& msg)
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
void
QtTerminal::ensureInputColor()
{
    // force to restore input color
    if (isInEditionZone()) {
        QTextCursor cur = textCursor();
        setTextColor(getInputColor());
        QTextCharFormat fmt;
        fmt.setForeground(getInputColor());
        cur.setCharFormat(fmt);
        cur.setCharFormat(QTextCharFormat());
        setTextCursor(cur);
    }
}
//=============================================================================
bool
QtTerminal::handlePreviousCharKeyPress()
{
    QTextCursor cur = textCursor();
    const int col = cur.columnNumber();
    ensureInputColor();

    if ((promptBlock == cur.block()) && (col == mPrompt.size())) {
        return true;
    }
    return false;
}
//=============================================================================
bool
QtTerminal::handleBackspaceKeyPress()
{
    return handlePreviousCharKeyPress();
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
    if (qCompleter) {
        if (qCompleter->popup()->isVisible()) {
            // The following keys are forwarded by the completer to the widget
            switch (event->key()) {
            case Qt::Key_Enter:
            case Qt::Key_Return:
            case Qt::Key_Escape:
            case Qt::Key_Tab:
            case Qt::Key_Backtab:
                event->ignore();
                return; // let the completer do default behavior
            default:
                qCompleter->popup()->hide();
                break;
            }
        }
    }
    if (event->matches(QKeySequence::MoveToStartOfLine)) {
        handleHomePress();
        return;
    }
    if (event->matches(QKeySequence::MoveToPreviousLine)) {
        handleUpKeyPress();
        return;
    }
    if (event->matches(QKeySequence::MoveToNextLine)) {
        handleDownKeyPress();
        return;
    }
    if (event->matches(QKeySequence::MoveToPreviousChar)) {
        if (handleBackspaceKeyPress()) {
            return;
        }
    } else if (event->matches(QKeySequence::Copy)) {
        if (!mCommandLineReady) {
            NelsonConfiguration::getInstance()->setInterruptPending(true);
        }
    } else {
        switch (event->key()) {
        case Qt::Key_Tab: {
            if (isInEditionZone()) {
                bool backup = mCommandLineReady;
                createCompleter();
                QString completionPrefix = getCurrentCommandLine();
                mCommandLineReady = backup;
                complete(completionPrefix);
                event->accept();
                return;
            }
        } break;
        case Qt::Key_Backspace: {
            if (handleBackspaceKeyPress()) {
                return;
            }
            updateHistoryToken();

        } break;
        case Qt::Key_Delete: {
            updateHistoryToken();
            if (!isInEditionZone()) {
                return;
            }
            ensureInputColor();
        } break;
        case Qt::Key_Return:
        case Qt::Key_Enter: {
            if (isInEditionZone()) {
                QString cmd = getCurrentCommandLine();
                lineToSend = Nelson::QStringTowstring(cmd) + L"\n";
                QTextCursor cur(document()->lastBlock());
                cur.movePosition(QTextCursor::EndOfBlock);
                cur.insertBlock();
                setTextCursor(cur);
            }
            return;
        } break;
        default: {

        } break;
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
QtTerminal::replaceCurrentCommandLine(const std::wstring& newline)
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
static bool doOnce = false;
void
QtTerminal::printMessage(QString msg, DISP_MODE mode)
{
    mCommandLineReady = false;
    this->setUpdatesEnabled(false);
    QTextCursor cur(document()->lastBlock());
    QTextCharFormat format = cur.charFormat();

    switch (mode) {
    case WARNING_DISP: {
        format.setForeground(getWarningColor());
    } break;
    case STDOUT_DISP: {
        format.setForeground(getOutputColor());
    } break;
    case STDERR_DISP: {
        format.setForeground(getErrorColor());
    } break;
    case STDIN_DISP: {
        format.setForeground(getInputColor());
    } break;
    }
    cur.movePosition(QTextCursor::EndOfBlock, QTextCursor::MoveAnchor, 1);

    if (msg.contains("\b")) {
        for (auto c : msg) {
            if (c == '\b') {
                cur.deletePreviousChar();
            } else {
                cur.insertText(c, format);
            }
        }
    } else {
        cur.insertText(msg, format);
    }
    setTextCursor(cur);
    this->setUpdatesEnabled(true);
}
//=============================================================================
void
QtTerminal::closeEvent(QCloseEvent* event)
{
    NelsonConfiguration::getInstance()->setInterruptPending(true);
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
QtTerminal::insertHtml(const std::wstring& msg)
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
            StringHelpers::replace_all(text, L"'", L"\"");
            std::wstring cmd = L"doc('" + text + L"');";
            postCommand(cmd);
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
    NelsonConfiguration::getInstance()->setInterruptPending(true);
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
void
QtTerminal::complete(QString prefix)
{
    if (prefix.isEmpty()) {
        return;
    }
    bool showpopup = false;
    if (!prefix.isEmpty()) {
        std::wstring completionPrefixW = QStringTowstring(prefix);
        std::wstring filepart = getPartialLineAsPath(completionPrefixW);
        wstringVector files;
        bool doFullSearch = true;
        if (!filepart.empty()) {
            files = FileCompleter(filepart);
            if (!files.empty()) {
                updateModel(
                    completionPrefixW, files, wstringVector(), wstringVector(), wstringVector());
                showpopup = true;
                doFullSearch = false;
            }
        }
        if (doFullSearch) {
            std::wstring textpart = getPartialLine(completionPrefixW);
            if (!textpart.empty()) {
                wstringVector builtin = BuiltinCompleter(textpart);
                wstringVector macros = MacroCompleter(textpart);
                wstringVector variables = VariableCompleter(textpart);
                if (!files.empty() || !builtin.empty() || !macros.empty() || !variables.empty()) {
                    updateModel(textpart, files, builtin, macros, variables);
                    showpopup = true;
                }
            }
        }
    }

    if (showpopup) {
        QRect cr = cursorRect();
        cr.setWidth(qCompleter->popup()->sizeHintForColumn(0)
            + qCompleter->popup()->verticalScrollBar()->sizeHint().width());
        cr.setHeight(10);
        qCompleter->complete(cr);
        qCompleter->setCurrentRow(0);
        qCompleter->popup()->setCurrentIndex(qCompleter->completionModel()->index(0, 0));
        qCompleter->popup()->setVisible(true);
    } else {
        if (qCompleter->popup()->isVisible()) {
            qCompleter->popup()->close();
        }
    }
}
//=============================================================================
void
QtTerminal::updateModel(const std::wstring& prefix, const wstringVector& filesList,
    const wstringVector& builtinList, const wstringVector& macroList,
    const wstringVector& variableList)
{

    if (qCompleter != nullptr) {
        qCompleter->setModel(modelFromNelson(filesList, builtinList, macroList, variableList));
        qCompleter->setCompletionPrefix(wstringToQString(prefix));
    }
}
//=============================================================================
QAbstractItemModel*
QtTerminal::modelFromNelson(const wstringVector& filesList, const wstringVector& builtinList,
    const wstringVector& macroList, const wstringVector& variableList)
{
    QStringList words;
    for (const auto& k : filesList) {
        words.append(
            wstringToQString(k) + QString(" (") + wstringToQString(POSTFIX_FILES) + QString(")"));
    }
    for (const auto& k : builtinList) {
        words.append(
            wstringToQString(k) + QString(" (") + wstringToQString(POSTFIX_BUILTIN) + QString(")"));
    }
    for (const auto& k : macroList) {
        words.append(
            wstringToQString(k) + QString(" (") + wstringToQString(POSTFIX_MACRO) + QString(")"));
    }
    for (const auto& k : variableList) {
        words.append(wstringToQString(k) + QString(" (") + wstringToQString(POSTFIX_VARIABLE)
            + QString(")"));
    }
    words.sort();
    return new QStringListModel(words, qCompleter);
}
//=============================================================================
void
QtTerminal::insertCompletion(const QString& completion)
{
    QString FILE_OR_DIR = QString(" (") + wstringToQString(POSTFIX_FILES) + QString(")");
    bool isPathCompletion = (completion.lastIndexOf(FILE_OR_DIR) != -1);
    QString cleanedCompletion = completion;
    QString beforeString = QString(" (") + wstringToQString(POSTFIX_BUILTIN) + QString(")");
    cleanedCompletion = cleanedCompletion.replace(
        cleanedCompletion.lastIndexOf(beforeString), beforeString.size(), QString());
    beforeString = QString(" (") + wstringToQString(POSTFIX_MACRO) + QString(")");
    cleanedCompletion = cleanedCompletion.replace(
        cleanedCompletion.lastIndexOf(beforeString), beforeString.size(), QString());
    beforeString = QString(" (") + wstringToQString(POSTFIX_VARIABLE) + QString(")");
    cleanedCompletion = cleanedCompletion.replace(
        cleanedCompletion.lastIndexOf(beforeString), beforeString.size(), QString());
    cleanedCompletion = cleanedCompletion.replace(
        cleanedCompletion.lastIndexOf(FILE_OR_DIR), FILE_OR_DIR.size(), QString());
    if (qCompleter->widget() != this) {
        return;
    }
    QTextCursor tc = textCursor();
    QString completionPrefix = qCompleter->completionPrefix();
    bool backup = mCommandLineReady;
    std::wstring currentLineW = QStringTowstring(getCurrentCommandLine());
    mCommandLineReady = backup;

    std::wstring cleanedCompletionW = QStringTowstring(cleanedCompletion);
    std::wstring fileSearchedPattern = getPartialLineAsPath(currentLineW);
    std::wstring searchedPattern = getPartialLine(currentLineW);
    std::wstring newLine = completerLine(
        currentLineW, cleanedCompletionW, fileSearchedPattern, searchedPattern, isPathCompletion);

    replaceCurrentCommandLine(newLine);
    updateHistoryToken();
}
//=============================================================================
