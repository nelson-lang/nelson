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
#include <QtCore/QLocale>
#include <QtCore/QMimeData>
#include <QtCore/QDir>
#include <QtCore/QFile>
#include <QtCore/QSettings>
#include <QtCore/QUrl>
#include <QtCore/QUrlQuery>
#include <QtGui/QClipboard>
#include <QtGui/QDragEnterEvent>
#include <QtGui/QDropEvent>
#include <QtGui/QDesktopServices>
#include <QtGui/QImageReader>
#include <QtGui/QKeyEvent>
#include <QtGui/QTextBlock>
#include <QtGui/QTextBlockFormat>
#include <QtGui/QTextCharFormat>
#include <QtGui/QTextCursor>
#include <QtGui/QTextDocument>
#include <QtGui/QTextImageFormat>
#include <QtGui/QTextFormat>
#include <QtGui/QFontDatabase>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QInputDialog>
#include <QtWidgets/QLineEdit>
#include <QtGui/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QMenu>
#include <QtCore/QStringListModel>
#include <QtWidgets/QAbstractItemView>
#include <QtWidgets/QScrollBar>
#include <QtGui/QTextDocumentFragment>
#include <QtGui/QPainter>
#include <QtPrintSupport/QPrinter>
#include <vector>
#include "Nelson_VERSION.h"
#include "NelsonSettingsScope.hpp"
#include "QtTerminal.h"
#include "Evaluator.hpp"
#include "NelsonHistory.hpp"
#include "ProcessEvents.hpp"
#include "QStringConverter.hpp"
#include "QtTranslation.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
#include "QtFileDialogHistory.hpp"
#include "PostCommand.hpp"
#include "NelsonPalette.hpp"
#include "NelsonColors.hpp"
#include "DefaultFont.hpp"
#include "StringHelpers.hpp"
#include "CompleterHelper.hpp"
#include "HistoryBrowser.hpp"
#include "QtHistoryBrowser.h"
#include "FileBrowser.hpp"
#include "QtFileBrowser.h"
#include "WorkspaceBrowser.hpp"
#include "QtWorkspaceBrowser.h"
#include "CallbackQueue.hpp"
#include "EventQueue.hpp"
#include "MainThreadTaskQueue.hpp"
#include "VariablesEditor.hpp"
#include "QtTerminalCompleter.h"
#include "QtWaitCursor.hpp"
#include "AnsiSgr.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
constexpr int LATEX_IMAGE_BASE_WIDTH_PROPERTY = QTextFormat::UserProperty + 1201;
constexpr int LATEX_IMAGE_BASE_HEIGHT_PROPERTY = QTextFormat::UserProperty + 1202;
constexpr double LATEX_IMAGE_ZOOM_FACTOR = 1.1;
bool
isLatexImageFormat(const QTextImageFormat& imageFormat)
{
    return imageFormat.name().startsWith("data:image/svg+xml", Qt::CaseInsensitive);
}
//=============================================================================
} // namespace
//=============================================================================
const QString SETTING_COMMAND_WINDOW_TRUNCATE_MATRICES = "command_window_truncate_matrices";
const QString SETTING_COMMAND_WINDOW_FONT_SIZE = "command_window_font_size";
const QString SETTING_COMMAND_WINDOW_WRAP_LINES = "command_window_wrap_lines";
//=============================================================================
namespace {
constexpr int COMMAND_WINDOW_DEFAULT_FONT_SIZE = 10;
constexpr int COMMAND_WINDOW_MIN_FONT_SIZE = 4;
constexpr int COMMAND_WINDOW_MAX_FONT_SIZE = 72;
//=============================================================================
} // namespace
//=============================================================================
QtTerminal::QtTerminal(QWidget* parent) : QTextBrowser(parent)
{
    mCommandLineReady = false;
    latexImageScale = 1.0;
    expandableTextCounter = 0;
    completionDisabled = true;
    isFirstPrompt = true;
    completerImpl = nullptr;

    QLocale us(QLocale::English, QLocale::UnitedStates);
    QLocale::setDefault(us);
    setPalette(getNelsonPalette());
    defaultFontPointSize = COMMAND_WINDOW_DEFAULT_FONT_SIZE;
    wrapLinesEnabled = false;
    wrapLinesAction = nullptr;

    QSettings terminalSettings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    int savedFontSize
        = terminalSettings.value(SETTING_COMMAND_WINDOW_FONT_SIZE, defaultFontPointSize).toInt();
    if (savedFontSize < COMMAND_WINDOW_MIN_FONT_SIZE
        || savedFontSize > COMMAND_WINDOW_MAX_FONT_SIZE) {
        savedFontSize = defaultFontPointSize;
    }

    QFont f(wstringToQString(Nelson::getDefaultFontName()));
    f.setStyleHint(QFont::Monospace);
    f.setPointSize(savedFontSize);
    f.setFixedPitch(true);
    setFont(f);
    setCursorWidth(QFontMetrics(font()).horizontalAdvance(QChar('x')));
    setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    setUndoRedoEnabled(false);
    wrapLinesEnabled = terminalSettings.value(SETTING_COMMAND_WINDOW_WRAP_LINES, false).toBool();
    applyWrapLines(wrapLinesEnabled);
    setFrameStyle(QFrame::NoFrame);
    setTabStopDistance(40);
    setAcceptDrops(true);
    viewport()->setAcceptDrops(true);
    lineToSend.clear();
    setCursorWidth(0);
    setTextInteractionFlags(Qt::TextSelectableByMouse | Qt::TextSelectableByKeyboard
        | Qt::LinksAccessibleByMouse | Qt::TextEditable);
    setOpenLinks(false);
    setOpenExternalLinks(false);
    connect(this, &QTextBrowser::anchorClicked, this, &QtTerminal::handleAnchorClicked);
    // Bound the scrollback from construction: a command that floods the console
    // (a directory with tens of thousands of files) would otherwise grow the
    // document without limit and freeze the UI. 0 means unlimited.
    document()->setMaximumBlockCount(DEFAULT_CONSOLE_MAX_LINE_VISIBLE);
    nelsonPath = Nelson::wstringToQString(
        Nelson::NelsonConfiguration::getInstance()->getNelsonRootDirectory());
    helpOnSelectionAction = nullptr;
    cutAction = nullptr;
    copyAction = nullptr;
    pasteAction = nullptr;
    selectAllAction = nullptr;
    clcAction = nullptr;
    stopAction = nullptr;
    truncateMatricesAction = nullptr;
    openVariableAction = nullptr;
    contextMenu = nullptr;

    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    NelsonConfiguration::getInstance()->setTruncateMatricesDisplay(
        settings.value(SETTING_COMMAND_WINDOW_TRUNCATE_MATRICES, true).toBool());

    lastSearch.clear();
    lastSearchCaseSensitive = false;

    findAction = new QAction(TR("Find..."), this);
    findAction->setShortcut(QKeySequence::Find);
    connect(findAction, SIGNAL(triggered()), this, SLOT(showFindDialog()));

    findNextAction = new QAction(TR("Find Next"), this);
    findNextAction->setShortcut(QKeySequence::FindNext);
    connect(findNextAction, SIGNAL(triggered()), this, SLOT(findNext()));
    findNextAction->setEnabled(!lastSearch.isEmpty());

    findPreviousAction = new QAction(TR("Find Previous"), this);
    findPreviousAction->setShortcut(QKeySequence::FindPrevious);
    connect(findPreviousAction, SIGNAL(triggered()), this, SLOT(findPrevious()));
    findPreviousAction->setEnabled(!lastSearch.isEmpty());

    completionDisabled = false;
    mCommandLineReady = true;
}
//=============================================================================
void
QtTerminal::createCompleter()
{
    if (!completerImpl) {
        completerImpl = new QtTerminalCompleter(this);
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
    if (exportContentAction) {
        delete exportContentAction;
        exportContentAction = nullptr;
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
    if (completerImpl) {
        delete completerImpl;
        completerImpl = nullptr;
    }
}
//=============================================================================
void
QtTerminal::banner()
{
    mCommandLineReady = false;
    QString _nelsonPath = Nelson::wstringToQString(
        Nelson::NelsonConfiguration::getInstance()->getNelsonRootDirectory());
    QString fileName = _nelsonPath + "/resources/banner_nelson_small.png";
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
#ifdef _MSC_VER
    fprintf(stdout, "Nelson %s - Copyright (c) 2016-present Allan CORNET (Nelson)\n",
        NELSON_VERSION_STRING);
#endif
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

    cur.beginEditBlock();
    QTextCharFormat fmt;
    fmt.setForeground(getInputColor());
    cur.setCharFormat(fmt);
    cur.insertText(mPrompt);
    cur.setCharFormat(QTextCharFormat());
    cur.endEditBlock();

    setTextCursor(cur);
    ensureCursorVisible();

    if (isFirstPrompt) {
        QtHistoryBrowser* qtHistoryBrowser = (QtHistoryBrowser*)HistoryBrowser::getHistoryBrowser();
        QtFileBrowser* qtFileBrowser = (QtFileBrowser*)FileBrowser::getFileBrowser();
        QtWorkspaceBrowser* qtWorkspaceBrowser
            = (QtWorkspaceBrowser*)WorkspaceBrowser::getWorkspaceBrowser();

        QObject::connect(qtHistoryBrowser, SIGNAL(sendCommands(const QStringList&)), this,
            SLOT(onCommandsReceived(const QStringList&)));
        QObject::connect(qtHistoryBrowser, SIGNAL(sendToCommandLine(const QStringList&)), this,
            SLOT(onCommandsToCommandLineReceived(const QStringList&)));
        QObject::connect(
            qtHistoryBrowser, SIGNAL(sendToTextEditor()), this, SLOT(onToTextEditorReceived()));

        QObject::connect(qtFileBrowser, SIGNAL(postCommand(const QString&)), this,
            SLOT(onPostCommandReceived(const QString&)));

        QObject::connect(qtWorkspaceBrowser, SIGNAL(postCommand(const QString&)), this,
            SLOT(onPostCommandReceived(const QString&)));

        QObject::connect(qtWorkspaceBrowser, SIGNAL(sendCommands(const QStringList&)), this,
            SLOT(onCommandsReceived(const QStringList&)));
    }
    isFirstPrompt = false;
    mCommandLineReady = true;
    emit promptDisplayed();
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
QtTerminal::moveCursorToPrompt()
{
    QTextCursor cur;
    if (mCommandLineReady && promptBlock.isValid()) {
        cur = QTextCursor(promptBlock);
        cur.movePosition(QTextCursor::EndOfBlock);
    } else {
        cur = QTextCursor(document());
        cur.movePosition(QTextCursor::End);
    }
    setTextCursor(cur);
    ensureInputColor();
    ensureCursorVisible();
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
    VariablesEditor::updateVariablesEditor();
    FileBrowser::updateFileBrowser();
    promptBlock = document()->lastBlock();

    setCursorWidth(QFontMetrics(document()->defaultFont()).horizontalAdvance(QChar('x')));
    WorkspaceBrowser::updateWorkspaceBrowser();
    Nelson::restoreApplicationWaitCursor();
    Nelson::Evaluator* eval
        = static_cast<Nelson::Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
    if (!eval) {
        return L"\n";
    }
    eval->commandQueue.clear();
    Nelson::CallbackQueue::getInstance()->clear();
    Nelson::EventQueue::getInstance()->clear();
    auto* taskQueue = Nelson::MainThreadTaskQueue::getInstance();
    taskQueue->setInterpreterIdle(true);
    struct IdleReset
    {
        Nelson::MainThreadTaskQueue* queue;
        ~IdleReset() { queue->setInterpreterIdle(false); }
    } idleReset { taskQueue };
    bool wasInterruptedByAction = false;
    while (true) {
        try {
            Nelson::ProcessEvents(true);
        } catch (const NelsonException& e) {
            e.printMe(eval->getInterface());
            wasInterruptedByAction = true;
            break;
        }

        if (taskQueue->processTasks(eval)) {
            continue;
        }
        if (!eval->commandQueue.isEmpty() || !Nelson::CallbackQueue::getInstance()->isEmpty()
            || !Nelson::EventQueue::getInstance()->isEmpty()) {
            wasInterruptedByAction = true;
            break;
        }

        if (!lineToSend.empty() || !lineBuffer.isEmpty()) {
            break;
        }
    }

    std::wstring line;
    if (wasInterruptedByAction) {
        clearLine();
        line = L"\n";
    } else {
        if (!lineBuffer.isEmpty()) {
            line = QStringTowstring(lineBuffer);
            lineBuffer.clear();
        } else {
            line = lineToSend;
        }
    }

    while (lineToSend.empty() && !wasInterruptedByAction) {
        Nelson::ProcessEvents(true);
    }

    lineToSend.clear();
    Nelson::ProcessEvents();

    setCursorWidth(0);

    Nelson::setApplicationWaitCursor();
    return line;
}
//=============================================================================
size_t
QtTerminal::getTerminalWidth()
{
    // the zoom applies to the document default font, use it to stay in sync
    int chSize = QFontMetrics(document()->defaultFont()).horizontalAdvance(QChar('M'));
    if (chSize <= 0) {
        return DEFAULT_CONSOLE_WIDTH;
    }
    int availableWidth = viewport() ? viewport()->width() : width();
    if (availableWidth <= 0) {
        return DEFAULT_CONSOLE_WIDTH;
    }
    return static_cast<size_t>(availableWidth / chSize);
}
//=============================================================================
size_t
QtTerminal::getTerminalHeight()
{
    int chSize = QFontMetrics(document()->defaultFont()).height();
    if (chSize <= 0) {
        return DEFAULT_CONSOLE_HEIGHT;
    }
    int availableHeight = viewport() ? viewport()->height() : height();
    if (availableHeight <= 0) {
        return DEFAULT_CONSOLE_HEIGHT;
    }
    return static_cast<size_t>(availableHeight / chSize);
}
//=============================================================================
void
QtTerminal::outputMessage(const std::wstring& msg)
{
    printMessage(Nelson::wstringToQString(msg), STDOUT_DISP);
}
//=============================================================================
void
QtTerminal::outputStyledText(const std::wstring& msg, const TextStyle& style)
{
    printStyledMessage(Nelson::wstringToQString(msg), style);
}
//=============================================================================
void
QtTerminal::outputAnsiText(const std::wstring& msg, bool asError)
{
    DISP_MODE mode = asError ? STDERR_DISP : STDOUT_DISP;
    if (!containsAnsiEscape(msg)) {
        printMessage(Nelson::wstringToQString(msg), mode);
        return;
    }
    printAnsiMessage(msg, mode);
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
QtTerminal::applyForegroundToCursor(
    QTextCursor& cursor, const QColor& color, bool merge, bool makeCurrent)
{
    QTextCharFormat fmt = cursor.charFormat();
    fmt.setForeground(color);
    if (merge) {
        cursor.mergeCharFormat(fmt);
    } else {
        cursor.setCharFormat(fmt);
    }
    if (makeCurrent) {
        setCurrentCharFormat(fmt);
        setTextColor(color);
    }
}
//=============================================================================
void
QtTerminal::ensureInputColor()
{
    if (!isInEditionZone()) {
        return;
    }

    QTextCursor cur = textCursor();

    // Apply input color to current cursor/selection and make it the current
    // char format so subsequent typing uses it.
    applyForegroundToCursor(cur, getInputColor(), true, true);

    setTextCursor(cur);
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
bool
QtTerminal::event(QEvent* event)
{
    if (event->type() == QEvent::KeyPress) {
        auto* keyEvent = static_cast<QKeyEvent*>(event);
        if (keyEvent->key() == Qt::Key_Tab && keyEvent->modifiers() == Qt::NoModifier) {
            if (completerImpl && completerImpl->completer()->popup()->isVisible()
                && !completionDisabled) {
                return QTextBrowser::event(event);
            }
            if (handleTabCompletion()) {
                keyEvent->accept();
                return true;
            }
        }
    }
    return QTextBrowser::event(event);
}
//=============================================================================
bool
QtTerminal::handleTabCompletion()
{
    if (completionDisabled) {
        return true;
    }
    if (!isInEditionZone()) {
        moveCursorToPrompt();
    }
    if (!isInEditionZone()) {
        return true;
    }
    bool backup = mCommandLineReady;
    createCompleter();
    QString completionPrefix = getCurrentCommandLine();
    mCommandLineReady = backup;
    complete(completionPrefix);
    return true;
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
    if ((event->modifiers() & Qt::ControlModifier) && event->key() == Qt::Key_D) {
        if (openTruncatedMatrixAtCursor()) {
            event->accept();
            return;
        }
    }
    if (event->matches(QKeySequence::Find)) {
        showFindDialog();
        event->accept();
        return;
    }
    if (event->matches(QKeySequence::FindNext)) {
        findNext();
        event->accept();
        return;
    }
    if (event->matches(QKeySequence::FindPrevious)) {
        findPrevious();
        event->accept();
        return;
    }

    if (event->modifiers() & Qt::ControlModifier) {
        if (event->key() == Qt::Key_Plus || event->key() == Qt::Key_Equal) {
            zoomTerminalIn();
            event->accept();
            return;
        } else if (event->key() == Qt::Key_Minus) {
            zoomTerminalOut();
            event->accept();
            return;
        } else if (event->key() == Qt::Key_0) {
            zoomTerminalReset();
            event->accept();
            return;
        }
    }

    // Copy must be handled before any cursor move: a selection made outside the edition
    // zone would be lost otherwise. With a selection Ctrl+C copies, without it interrupts.
    if (event->matches(QKeySequence::Copy)) {
        if (textCursor().hasSelection()) {
            copy();
        } else if (!mCommandLineReady) {
            NelsonConfiguration::getInstance()->setInterruptPending(true);
        }
        event->accept();
        return;
    }

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
    if (completerImpl) {
        if (completerImpl->completer()->popup()->isVisible()) {
            if (completionDisabled) {
                completerImpl->completer()->popup()->hide();
            } else {
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
                    completerImpl->completer()->popup()->hide();
                    break;
                }
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
    } else {
        switch (event->key()) {
        case Qt::Key_Tab: {
            if (handleTabCompletion()) {
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
QTextCharFormat
QtTerminal::createCharFormatWithForeground(const QColor& color)
{
    QTextCharFormat fmt;
    fmt.setForeground(color);
    return fmt;
}
//=============================================================================
QColor
QtTerminal::colorForMode(DISP_MODE mode)
{
    switch (mode) {
    case WARNING_DISP:
        return getWarningColor();
    case STDOUT_DISP:
        return getOutputColor();
    case STDERR_DISP:
        return getErrorColor();
    case STDIN_DISP:
    default:
        return getInputColor();
    }
}
//=============================================================================
QTextCharFormat
QtTerminal::createCharFormat(const TextStyle& style, DISP_MODE fallbackMode)
{
    QColor color = style.hasForeground ? QColor(style.red, style.green, style.blue)
                                       : colorForMode(fallbackMode);
    QTextCharFormat format = createCharFormatWithForeground(color);
    if (style.bold) {
        format.setFontWeight(QFont::Bold);
    }
    if (style.underline) {
        format.setFontUnderline(true);
    }
    if (style.italic) {
        format.setFontItalic(true);
    }
    if (style.strikethrough) {
        format.setFontStrikeOut(true);
    }
    if (style.hasBackground) {
        format.setBackground(
            QColor(style.backgroundRed, style.backgroundGreen, style.backgroundBlue));
    }
    return format;
}
//=============================================================================
void
QtTerminal::insertFormattedText(QTextCursor& cursor, const QString& text, DISP_MODE mode)
{
    QTextCharFormat format = createCharFormatWithForeground(colorForMode(mode));
    insertFormattedText(cursor, text, format);
}
//=============================================================================
void
QtTerminal::insertFormattedText(
    QTextCursor& cursor, const QString& text, const QTextCharFormat& format)
{
    QString normalized = text;
    normalized.replace("\r\n", "\n");
    normalized.replace('\r', '\n');
    QStringList lines = normalized.split('\n');
    for (qsizetype k = 0; k < lines.size(); ++k) {
        if (k > 0) {
            cursor.insertBlock();
        }
        if (!lines[k].isEmpty()) {
            cursor.insertText(lines[k], format);
        }
    }
}
//=============================================================================
bool
QtTerminal::isScrolledToBottom() const
{
    const QScrollBar* vScrollBar = verticalScrollBar();
    if (vScrollBar == nullptr) {
        return true;
    }
    if (!vScrollBar->isVisible() && vScrollBar->maximum() == vScrollBar->minimum()) {
        return true;
    }
    // small tolerance: a partially visible last line must still count as 'at bottom'
    return vScrollBar->value() >= (vScrollBar->maximum() - 4);
}
//=============================================================================
void
QtTerminal::finishOutputScroll(QTextCursor& cursor, bool wasAtBottom, int previousScrollValue)
{
    // Never steal an active user selection: moving the text cursor would clear it.
    if (!textCursor().hasSelection()) {
        // setTextCursor() implicitly makes the cursor visible: the scroll position is
        // restored just after when the user had scrolled away from the bottom.
        setTextCursor(cursor);
    }
    QScrollBar* vScrollBar = verticalScrollBar();
    if (vScrollBar == nullptr) {
        return;
    }
    if (wasAtBottom) {
        vScrollBar->setValue(vScrollBar->maximum());
    } else {
        vScrollBar->setValue(previousScrollValue);
    }
}
//=============================================================================
void
QtTerminal::printMessage(QString msg, DISP_MODE mode)
{
    mCommandLineReady = false;
    const bool wasAtBottom = isScrolledToBottom();
    const int previousScrollValue = verticalScrollBar() ? verticalScrollBar()->value() : 0;
    this->setUpdatesEnabled(false);

    QTextBlock lastBlock = document()->lastBlock();
    if (!lastBlock.isValid()) {
        QTextCursor cursor(document());
        cursor.movePosition(QTextCursor::End);
        cursor.insertBlock();
        lastBlock = document()->lastBlock();
    }

    QTextCursor cur(lastBlock);
    if (!cur.isNull()) {
        cur.movePosition(QTextCursor::EndOfBlock, QTextCursor::MoveAnchor, 1);
    } else {
        cur = QTextCursor(document());
        cur.movePosition(QTextCursor::End);
    }

    // Batch document edits to reduce layout/paint work
    cur.beginEditBlock();
    if (msg.contains(QStringLiteral("\x1b]8;;"))) {
        insertHyperlinkedText(cur, msg, mode);
    } else if (msg.contains("\b")) {
        QString buffer;
        buffer.reserve(msg.size());
        for (auto c : msg) {
            if (c == '\b') {
                if (!buffer.isEmpty()) {
                    insertFormattedText(cur, buffer, mode);
                    buffer.clear();
                }
                cur.deletePreviousChar();
            } else {
                buffer.append(c);
            }
        }
        if (!buffer.isEmpty()) {
            insertFormattedText(cur, buffer, mode);
        }
    } else {
        insertFormattedText(cur, msg, mode);
    }
    cur.endEditBlock();

    this->setUpdatesEnabled(true);
    finishOutputScroll(cur, wasAtBottom, previousScrollValue);
}
//=============================================================================
void
QtTerminal::insertHyperlinkedText(QTextCursor& cursor, const QString& text, DISP_MODE mode)
{
    const QString oscStart = QStringLiteral("\x1b]8;;");
    // Find the OSC string terminator (ESC '\' or BEL) at/after `from`; returns its
    // index and sets `afterTerminator` to the index just past it, or -1 if absent.
    auto findTerminator = [&text](int from, int& afterTerminator) -> int {
        for (int j = from; j < text.size(); ++j) {
            if (text[j] == QChar(0x07)) {
                afterTerminator = j + 1;
                return j;
            }
            if (text[j] == QChar(0x1b) && j + 1 < text.size() && text[j + 1] == QChar('\\')) {
                afterTerminator = j + 2;
                return j;
            }
        }
        return -1;
    };

    int i = 0;
    const int n = text.size();
    while (i < n) {
        int linkStart = text.indexOf(oscStart, i);
        if (linkStart < 0) {
            insertFormattedText(cursor, text.mid(i), mode);
            return;
        }
        if (linkStart > i) {
            insertFormattedText(cursor, text.mid(i, linkStart - i), mode);
        }
        int afterUriTerm = 0;
        int uriTerm = findTerminator(linkStart + oscStart.size(), afterUriTerm);
        if (uriTerm < 0) {
            insertFormattedText(cursor, text.mid(linkStart), mode);
            return;
        }
        const QString uri
            = text.mid(linkStart + oscStart.size(), uriTerm - linkStart - oscStart.size());
        const int labelStart = afterUriTerm;
        int closeStart = text.indexOf(oscStart, labelStart);
        if (closeStart < 0) {
            insertFormattedText(cursor, text.mid(labelStart), mode);
            return;
        }
        const QString label = text.mid(labelStart, closeStart - labelStart);
        int afterCloseTerm = 0;
        int closeTerm = findTerminator(closeStart + oscStart.size(), afterCloseTerm);
        const QString anchor = QStringLiteral("<a href=\"%1\">%2</a>")
                                   .arg(uri.toHtmlEscaped(), label.toHtmlEscaped());
        cursor.insertHtml(anchor);
        // insertHtml leaves the anchor char format current; restore the plain mode
        // colour so trailing text is not rendered as part of the link.
        cursor.setCharFormat(createCharFormatWithForeground(colorForMode(mode)));
        i = (closeTerm < 0) ? n : afterCloseTerm;
    }
}
//=============================================================================
void
QtTerminal::printStyledMessage(QString msg, const TextStyle& style)
{
    mCommandLineReady = false;
    const bool wasAtBottom = isScrolledToBottom();
    const int previousScrollValue = verticalScrollBar() ? verticalScrollBar()->value() : 0;
    this->setUpdatesEnabled(false);

    QTextBlock lastBlock = document()->lastBlock();
    if (!lastBlock.isValid()) {
        QTextCursor cursor(document());
        cursor.movePosition(QTextCursor::End);
        cursor.insertBlock();
        lastBlock = document()->lastBlock();
    }

    QTextCursor cur(lastBlock);
    if (!cur.isNull()) {
        cur.movePosition(QTextCursor::EndOfBlock, QTextCursor::MoveAnchor, 1);
    } else {
        cur = QTextCursor(document());
        cur.movePosition(QTextCursor::End);
    }

    QTextCharFormat format = createCharFormat(style, STDOUT_DISP);
    cur.beginEditBlock();
    if (msg.contains("\b")) {
        QString buffer;
        buffer.reserve(msg.size());
        for (auto c : msg) {
            if (c == '\b') {
                if (!buffer.isEmpty()) {
                    insertFormattedText(cur, buffer, format);
                    buffer.clear();
                }
                cur.deletePreviousChar();
            } else {
                buffer.append(c);
            }
        }
        if (!buffer.isEmpty()) {
            insertFormattedText(cur, buffer, format);
        }
    } else {
        insertFormattedText(cur, msg, format);
    }
    cur.endEditBlock();

    this->setUpdatesEnabled(true);
    finishOutputScroll(cur, wasAtBottom, previousScrollValue);
}
//=============================================================================
void
QtTerminal::printAnsiMessage(const std::wstring& msg, DISP_MODE mode)
{
    mCommandLineReady = false;
    const bool wasAtBottom = isScrolledToBottom();
    const int previousScrollValue = verticalScrollBar() ? verticalScrollBar()->value() : 0;
    this->setUpdatesEnabled(false);

    QTextBlock lastBlock = document()->lastBlock();
    if (!lastBlock.isValid()) {
        QTextCursor cursor(document());
        cursor.movePosition(QTextCursor::End);
        cursor.insertBlock();
        lastBlock = document()->lastBlock();
    }

    QTextCursor cur(lastBlock);
    if (!cur.isNull()) {
        cur.movePosition(QTextCursor::EndOfBlock, QTextCursor::MoveAnchor, 1);
    } else {
        cur = QTextCursor(document());
        cur.movePosition(QTextCursor::End);
    }

    std::vector<AnsiStyledSpan> spans = parseAnsiStyledSpans(msg);
    cur.beginEditBlock();
    for (const AnsiStyledSpan& span : spans) {
        if (span.length == 0) {
            continue;
        }
        QTextCharFormat format = createCharFormat(span.style, mode);
        insertFormattedText(
            cur, Nelson::wstringToQString(msg.substr(span.first, span.length)), format);
    }
    cur.endEditBlock();

    this->setUpdatesEnabled(true);
    finishOutputScroll(cur, wasAtBottom, previousScrollValue);
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
QString
QtTerminal::quotePathForNelson(const QString& path)
{
    QString escaped = path;
    // Nelson single quoted strings escape an embedded quote by doubling it.
    escaped.replace(QString("'"), QString("''"));
    return QString("'") + escaped + QString("'");
}
//=============================================================================
void
QtTerminal::dragEnterEvent(QDragEnterEvent* event)
{
    if (event->mimeData() != nullptr && event->mimeData()->hasUrls()) {
        event->acceptProposedAction();
        return;
    }
    QTextBrowser::dragEnterEvent(event);
}
//=============================================================================
void
QtTerminal::dragMoveEvent(QDragMoveEvent* event)
{
    if (event->mimeData() != nullptr && event->mimeData()->hasUrls()) {
        event->acceptProposedAction();
        return;
    }
    QTextBrowser::dragMoveEvent(event);
}
//=============================================================================
void
QtTerminal::dropEvent(QDropEvent* event)
{
    const QMimeData* mimeData = event->mimeData();
    if (mimeData == nullptr || !mimeData->hasUrls()) {
        QTextBrowser::dropEvent(event);
        return;
    }
    QStringList quotedPaths;
    const QList<QUrl> urls = mimeData->urls();
    for (const QUrl& url : urls) {
        if (!url.isLocalFile()) {
            continue;
        }
        const QString localFile = QDir::toNativeSeparators(url.toLocalFile());
        if (localFile.isEmpty()) {
            continue;
        }
        quotedPaths.append(quotePathForNelson(localFile));
    }
    if (quotedPaths.isEmpty()) {
        event->ignore();
        return;
    }
    if (!isInEditionZone()) {
        moveCursorToPrompt();
    }
    if (isInEditionZone()) {
        QTextCursor cur = textCursor();
        cur.insertText(quotedPaths.join(QString(" ")));
        setTextCursor(cur);
        ensureInputColor();
        updateHistoryToken();
    }
    event->acceptProposedAction();
    setFocus();
}
//=============================================================================
void
QtTerminal::applyWrapLines(bool wrap)
{
    wrapLinesEnabled = wrap;
    if (wrap) {
        setWordWrapMode(QTextOption::WrapAtWordBoundaryOrAnywhere);
        setLineWrapMode(QTextEdit::WidgetWidth);
        setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    } else {
        setWordWrapMode(QTextOption::NoWrap);
        setLineWrapMode(QTextEdit::NoWrap);
        setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    }
}
//=============================================================================
void
QtTerminal::reloadPreferences()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);

    int savedFontSize
        = settings.value(SETTING_COMMAND_WINDOW_FONT_SIZE, defaultFontPointSize).toInt();
    if (savedFontSize < COMMAND_WINDOW_MIN_FONT_SIZE
        || savedFontSize > COMMAND_WINDOW_MAX_FONT_SIZE) {
        savedFontSize = defaultFontPointSize;
    }
    QFont f(wstringToQString(Nelson::getDefaultFontName()));
    f.setStyleHint(QFont::Monospace);
    f.setPointSize(savedFontSize);
    f.setFixedPitch(true);
    setFont(f);
    document()->setDefaultFont(f);

    bool wrap = settings.value(SETTING_COMMAND_WINDOW_WRAP_LINES, false).toBool();
    applyWrapLines(wrap);
    if (wrapLinesAction) {
        wrapLinesAction->setChecked(wrap);
    }

    bool truncate = settings.value(SETTING_COMMAND_WINDOW_TRUNCATE_MATRICES, true).toBool();
    NelsonConfiguration::getInstance()->setTruncateMatricesDisplay(truncate);
    if (truncateMatricesAction) {
        truncateMatricesAction->setChecked(truncate);
    }
}
//=============================================================================
void
QtTerminal::tagLatexImages(int startPosition, int endPosition)
{
    QTextCursor cursor(document());
    int position = startPosition;
    while (position < endPosition) {
        cursor.setPosition(position);
        if (!cursor.movePosition(QTextCursor::NextCharacter, QTextCursor::KeepAnchor)) {
            break;
        }
        QTextCharFormat charFormat = cursor.charFormat();
        if (charFormat.isImageFormat()) {
            QTextImageFormat imageFormat = charFormat.toImageFormat();
            if (isLatexImageFormat(imageFormat)) {
                if (!imageFormat.hasProperty(LATEX_IMAGE_BASE_WIDTH_PROPERTY)) {
                    imageFormat.setProperty(LATEX_IMAGE_BASE_WIDTH_PROPERTY, imageFormat.width());
                    imageFormat.setProperty(LATEX_IMAGE_BASE_HEIGHT_PROPERTY, imageFormat.height());
                }
                double baseWidth = imageFormat.property(LATEX_IMAGE_BASE_WIDTH_PROPERTY).toDouble();
                double baseHeight
                    = imageFormat.property(LATEX_IMAGE_BASE_HEIGHT_PROPERTY).toDouble();
                if (baseWidth > 0) {
                    imageFormat.setWidth(baseWidth * latexImageScale);
                }
                if (baseHeight > 0) {
                    imageFormat.setHeight(baseHeight * latexImageScale);
                }
                cursor.mergeCharFormat(imageFormat);
            }
        }
        position = cursor.position();
    }
}
//=============================================================================
void
QtTerminal::updateLatexImageSizes()
{
    tagLatexImages(0, document()->characterCount() - 1);
}
//=============================================================================
void
QtTerminal::saveFontSizeSetting()
{
    // zoomIn()/zoomOut() act on the document default font, not on the widget font.
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    settings.setValue(SETTING_COMMAND_WINDOW_FONT_SIZE, document()->defaultFont().pointSize());
}
//=============================================================================
void
QtTerminal::zoomTerminalIn()
{
    if (document()->defaultFont().pointSize() >= COMMAND_WINDOW_MAX_FONT_SIZE) {
        return;
    }
    QTextBrowser::zoomIn();
    latexImageScale *= LATEX_IMAGE_ZOOM_FACTOR;
    updateLatexImageSizes();
    saveFontSizeSetting();
}
//=============================================================================
void
QtTerminal::zoomTerminalOut()
{
    if (document()->defaultFont().pointSize() <= COMMAND_WINDOW_MIN_FONT_SIZE) {
        return;
    }
    QTextBrowser::zoomOut();
    latexImageScale /= LATEX_IMAGE_ZOOM_FACTOR;
    updateLatexImageSizes();
    saveFontSizeSetting();
}
//=============================================================================
void
QtTerminal::zoomTerminalReset()
{
    QFont f = document()->defaultFont();
    if (f.pointSize() == defaultFontPointSize) {
        return;
    }
    f.setPointSize(defaultFontPointSize);
    // Both must be updated: setFont() alone is a no-op when the widget font already
    // has the default size, and the document would keep the zoomed one.
    document()->setDefaultFont(f);
    setFont(f);
    latexImageScale = 1.0;
    updateLatexImageSizes();
    saveFontSizeSetting();
}
//=============================================================================
void
QtTerminal::insertHtml(const std::wstring& msg)
{
    QTextCursor cur = textCursor();
    const int htmlStart = cur.position();
    cur.insertHtml(Nelson::wstringToQString(msg));
    const int htmlEnd = cur.position();
    tagLatexImages(htmlStart, htmlEnd);
    cur.insertBlock(QTextBlockFormat(), QTextCharFormat());
    cur.setBlockFormat(QTextBlockFormat());
    cur.setCharFormat(QTextCharFormat());
    setTextCursor(cur);
    updateHistoryToken();
}
//=============================================================================
void
QtTerminal::insertExpandableText(const std::wstring& label, const std::wstring& expandedText,
    const std::wstring& contextName, long long contextValue)
{
    mCommandLineReady = false;
    QTextCursor cur(document()->lastBlock());
    cur.movePosition(QTextCursor::EndOfBlock);

    QString id = QString::number(++expandableTextCounter);
    ExpandableTextItem item;
    item.text = Nelson::wstringToQString(expandedText);
    item.blockPosition = cur.block().position();
    item.firstBlockPosition = item.blockPosition;
    item.lastBlockPosition = item.blockPosition;
    item.contextName = Nelson::wstringToQString(contextName);
    item.contextValue = contextValue;
    expandableTextItems.insert(id, item);

    QString linkText = Nelson::wstringToQString(label).toHtmlEscaped();
    QString href = QString("nelson-display:%1").arg(id);
    cur.insertHtml(QString("<a href=\"%1\">%2</a>").arg(href, linkText));
    cur.insertBlock();
    setTextCursor(cur);
    ensureCursorVisible();
    updateHistoryToken();
}
//=============================================================================
void
QtTerminal::insertTruncatedMatrix(const std::wstring& truncatedText, const std::wstring& label,
    const std::wstring& expandedText, const std::wstring& variableName)
{
    mCommandLineReady = false;
    QTextCursor cur(document()->lastBlock());
    cur.movePosition(QTextCursor::EndOfBlock);
    int firstBlockPosition = cur.block().position();

    this->setUpdatesEnabled(false);
    cur.beginEditBlock();
    insertFormattedText(cur, Nelson::wstringToQString(truncatedText), STDOUT_DISP);
    if (!cur.block().text().isEmpty()) {
        cur.insertBlock();
    }

    QString id = QString::number(++expandableTextCounter);
    ExpandableTextItem item;
    item.text = Nelson::wstringToQString(expandedText);
    item.blockPosition = cur.block().position();
    item.firstBlockPosition = firstBlockPosition;
    item.lastBlockPosition = item.blockPosition;
    item.variableName = Nelson::wstringToQString(variableName);
    item.matrixDisplay = true;

    QString linkText = Nelson::wstringToQString(label).toHtmlEscaped();
    QString href = QString("nelson-display:%1").arg(id);
    insertFormattedText(cur, QString("    "), STDOUT_DISP);
    cur.insertHtml(QString("<a href=\"%1\">%2</a>").arg(href, linkText));
    expandableTextItems.insert(id, item);
    cur.insertBlock();
    if (NelsonConfiguration::getInstance()->getLineSpacingDisplay() == NLS_LINE_SPACING_LOOSE) {
        cur.insertBlock();
    }
    cur.endEditBlock();
    this->setUpdatesEnabled(true);

    setTextCursor(cur);
    ensureCursorVisible();
    updateHistoryToken();
}
//=============================================================================
void
QtTerminal::handleAnchorClicked(const QUrl& url)
{
    if (url.scheme() == "nelson-open") {
        openFileAtLineFromUrl(url);
        // Clicking the anchor parks the text cursor on the clicked output line;
        // restore it to the prompt so the posted editor() command's prompt cycle
        // does not disturb that line.
        moveCursorToPrompt();
        return;
    }
    if (url.scheme() != "nelson-display") {
        QDesktopServices::openUrl(url);
        return;
    }

    QString id = url.toString();
    id.remove(QString("nelson-display:"));
    if (id.startsWith('/')) {
        id.remove(0, 1);
    }

    auto it = expandableTextItems.find(id);
    if (it == expandableTextItems.end()) {
        moveCursorToPrompt();
        return;
    }
    if (it.value().expanded) {
        moveCursorToPrompt();
        return;
    }
    QTextBlock block = document()->findBlock(it.value().blockPosition);
    if (!block.isValid()) {
        moveCursorToPrompt();
        return;
    }

    if (it.value().matrixDisplay && !it.value().variableName.isEmpty()
        && !VariablesEditor::isVariableExisting(
            Nelson::QStringTowstring(it.value().variableName))) {
        QTextCursor cur(block);
        cur.movePosition(QTextCursor::EndOfBlock);
        this->setUpdatesEnabled(false);
        cur.beginEditBlock();
        cur.insertBlock();
        QString msg = QString("Unable to display variable '%1' because it no longer exists.")
                          .arg(it.value().variableName);
        insertFormattedText(cur, msg, STDERR_DISP);
        cur.endEditBlock();
        this->setUpdatesEnabled(true);
        setTextCursor(cur);
        ensureCursorVisible();
        updateHistoryToken();
        return;
    }
    if (it.value().contextName == QString("graphics_object")
        && !VariablesEditor::isGraphicsObjectExisting(it.value().contextValue)) {
        QTextCursor cur(block);
        cur.movePosition(QTextCursor::EndOfBlock);
        this->setUpdatesEnabled(false);
        cur.beginEditBlock();
        cur.insertBlock();
        insertFormattedText(cur,
            QString("Unable to display properties because the graphics object no longer exists."),
            STDERR_DISP);
        cur.endEditBlock();
        this->setUpdatesEnabled(true);
        setTextCursor(cur);
        ensureCursorVisible();
        updateHistoryToken();
        return;
    }
    it.value().expanded = true;
    QTextCursor cur(block);
    cur.movePosition(QTextCursor::EndOfBlock);

    this->setUpdatesEnabled(false);
    cur.beginEditBlock();
    cur.insertBlock();
    insertFormattedText(cur, it.value().text, STDOUT_DISP);
    it.value().lastBlockPosition = cur.block().position();
    cur.endEditBlock();
    this->setUpdatesEnabled(true);

    moveCursorToPrompt();
}
//=============================================================================
void
QtTerminal::openFileAtLineFromUrl(const QUrl& url)
{
    // nelson-open:?line=<N>&file=<percent-encoded-path>
    QUrlQuery query(url);
    bool ok = false;
    int line = query.queryItemValue("line").toInt(&ok);
    QString path = query.queryItemValue("file", QUrl::FullyDecoded);
    if (!ok || line <= 0 || path.isEmpty()) {
        return;
    }
    std::wstring wpath = QStringTowstring(path);
    StringHelpers::replace_all(wpath, L"'", L"''");
    std::wstring cmd = L"editor('" + wpath + L"', " + std::to_wstring(line) + L");";
    postCommand(cmd);
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
    completionDisabled = true;
    QTextBrowser::paste();
    updateHistoryToken();
    completionDisabled = false;
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
QtTerminal::onExportContentAction()
{
    QString defaultFilePath
        = qtDialogInitialSavePath(L"desktop.save", QString(), QStringLiteral("document"));

    bool haveTextSelected = textCursor().hasSelection();
    if (haveTextSelected) {
        QTextCursor cursor = textCursor();
        QString textSelected = cursor.selectedText();
        if (textSelected.isEmpty()) {
            return;
        }
    }
    QString exportTypeMessage
        = haveTextSelected ? TR("Export selected text to ...") : TR("Export console text to ...");
    QString filePath = QFileDialog::getSaveFileName(
        this, exportTypeMessage, defaultFilePath, TR("PDF Files (*.pdf);;HTML Files (*.html)"));

    if (filePath.isEmpty()) {
        return;
    }
    qtRecordDialogSelection(L"desktop.save", QStringLiteral("save"), filePath);
    QString fileExtension = QFileInfo(filePath).suffix();
    bool isPDF = (fileExtension.toLower() == "pdf");
    if (isPDF) {
        exportToPdf(filePath);
    } else {
        exportToHtml(filePath);
    }
}
//=============================================================================
void
QtTerminal::exportToPdf(const QString& filename)
{
    QPrinter printer(QPrinter::PrinterResolution);
    printer.setOutputFormat(QPrinter::PdfFormat);
    printer.setOutputFileName(filename);
    QTextDocument doc;
    if (textCursor().hasSelection()) {
        QTextCursor cursor = textCursor();
        QTextDocumentFragment fragment = cursor.selection();
        doc.setHtml(fragment.toHtml());
    } else {
        doc.setHtml(toHtml());
    }
    QPainter painter;
    if (painter.begin(&printer)) {
        doc.drawContents(&painter);
        painter.end();
    }
}
//=============================================================================
void
QtTerminal::exportToHtml(const QString& filename)
{
    QTextCursor cursor = textCursor();
    QString htmlContent;
    if (textCursor().hasSelection()) {
        QTextDocumentFragment fragment = cursor.selection();
        htmlContent = fragment.toHtml();
    } else {
        htmlContent = toHtml();
    }

    QFileInfo fileInfo(filename);
    QString directoryPath = fileInfo.absolutePath();

    QRegularExpression imgSrcRegex(
        "src=[\"'](.+?)[\"']", QRegularExpression::CaseInsensitiveOption);

    QRegularExpressionMatchIterator matchIter = imgSrcRegex.globalMatch(htmlContent);
    while (matchIter.hasNext()) {
        QRegularExpressionMatch match = matchIter.next();
        QString originalImagePath = match.captured(1);
        if (originalImagePath.startsWith("qrc:")) {
            originalImagePath = originalImagePath.remove(0, 3);
            originalImagePath = ":/" + originalImagePath;
        }
        QImage image(originalImagePath);
        QString imageName = QFileInfo(originalImagePath).fileName();
        QString newImagePath = directoryPath + "/" + imageName;
        image.save(newImagePath);
        htmlContent.replace(originalImagePath, newImagePath);
    }

    QFile file(filename);
    if (file.open(QIODevice::WriteOnly | QIODevice::Text)) {
        QTextStream out(&file);
        out << htmlContent;
        file.close();
    }
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
        if (findAction != nullptr) {
            contextMenu->addAction(findAction);
        }
        if (findNextAction != nullptr) {
            contextMenu->addAction(findNextAction);
        }
        if (findPreviousAction != nullptr) {
            contextMenu->addAction(findPreviousAction);
        }
        if (cutAction == nullptr) {
            fileNameIcon = nelsonPath + QString("/resources/edit-cut.svg");
            cutAction = new QAction(QIcon(fileNameIcon), TR("Cut"), this);
            cutAction->setShortcut(QStringLiteral("Ctrl + X"));
            contextMenu->addAction(cutAction);
            connect(cutAction, SIGNAL(triggered()), this, SLOT(cut()));
        }
        if (copyAction == nullptr) {
            fileNameIcon = nelsonPath + QString("/resources/edit-copy.svg");
            copyAction = new QAction(QIcon(fileNameIcon), TR("Copy"), this);
            copyAction->setShortcut(QStringLiteral("Ctrl + Ins"));
            contextMenu->addAction(copyAction);
            connect(copyAction, SIGNAL(triggered()), this, SLOT(copy()));
        }
        if (pasteAction == nullptr) {
            fileNameIcon = nelsonPath + QString("/resources/edit-paste.svg");
            pasteAction = new QAction(QIcon(fileNameIcon), TR("Paste"), this);
            pasteAction->setShortcut(QStringLiteral("Ctrl + V"));
            contextMenu->addAction(pasteAction);
            connect(pasteAction, SIGNAL(triggered()), this, SLOT(paste()));
        }
        if (selectAllAction == nullptr) {
            fileNameIcon = nelsonPath + QString("/resources/edit-select-all.svg");
            selectAllAction = new QAction(QIcon(fileNameIcon), TR("Select All"), this);
            selectAllAction->setShortcut(QStringLiteral("Ctrl + A"));
            contextMenu->addAction(selectAllAction);
            connect(selectAllAction, SIGNAL(triggered()), this, SLOT(selectAll()));
        }
        if (openVariableAction == nullptr) {
            openVariableAction = new QAction(TR("Open Variable"), this);
            openVariableAction->setShortcut(QStringLiteral("Ctrl + D"));
            contextMenu->addAction(openVariableAction);
            connect(openVariableAction, &QAction::triggered, this, [this](bool) {
                QString variableName = openVariableAction->data().toString();
                if (!variableName.isEmpty()) {
                    VariablesEditor::openVariable(Nelson::QStringTowstring(variableName));
                }
            });
        }
        contextMenu->addSeparator();
        if (truncateMatricesAction == nullptr) {
            truncateMatricesAction = new QAction(TR("Truncate large matrices"), this);
            truncateMatricesAction->setCheckable(true);
            contextMenu->addAction(truncateMatricesAction);
            connect(truncateMatricesAction, &QAction::triggered, this, [](bool checked) {
                NelsonConfiguration::getInstance()->setTruncateMatricesDisplay(checked);
                QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
                settings.setValue(SETTING_COMMAND_WINDOW_TRUNCATE_MATRICES, checked);
            });
        }
        if (wrapLinesAction == nullptr) {
            wrapLinesAction = new QAction(TR("Wrap lines"), this);
            wrapLinesAction->setCheckable(true);
            contextMenu->addAction(wrapLinesAction);
            connect(wrapLinesAction, &QAction::triggered, this, [this](bool checked) {
                applyWrapLines(checked);
                QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
                settings.setValue(SETTING_COMMAND_WINDOW_WRAP_LINES, checked);
            });
        }
        contextMenu->addSeparator();
        if (exportContentAction == nullptr) {
            fileNameIcon = nelsonPath + QString("/resources/export-to.svg");
            exportContentAction = new QAction(QIcon(fileNameIcon), TR("Export to ..."), this);
            contextMenu->addAction(exportContentAction);
            connect(exportContentAction, SIGNAL(triggered()), this, SLOT(onExportContentAction()));
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
    QTextCursor eventCursor = cursorForPosition(event->pos());
    QString truncatedMatrixVariableName
        = truncatedMatrixVariableForBlock(eventCursor.block().position());
    openVariableAction->setData(truncatedMatrixVariableName);
    openVariableAction->setEnabled(!truncatedMatrixVariableName.isEmpty());
    truncateMatricesAction->setChecked(
        NelsonConfiguration::getInstance()->getTruncateMatricesDisplay());
    if (wrapLinesAction != nullptr) {
        wrapLinesAction->setChecked(wrapLinesEnabled);
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
    if (findNextAction != nullptr) {
        findNextAction->setEnabled(!lastSearch.isEmpty());
    }
    if (findPreviousAction != nullptr) {
        findPreviousAction->setEnabled(!lastSearch.isEmpty());
    }
    contextMenu->exec(event->globalPos());
}
//=============================================================================
QString
QtTerminal::truncatedMatrixVariableForBlock(int blockPosition) const
{
    for (auto it = expandableTextItems.constBegin(); it != expandableTextItems.constEnd(); ++it) {
        const ExpandableTextItem& item = it.value();
        if (!item.matrixDisplay || item.variableName.isEmpty()) {
            continue;
        }
        if (blockPosition >= item.firstBlockPosition && blockPosition <= item.lastBlockPosition) {
            return item.variableName;
        }
    }
    return QString();
}
//=============================================================================
QString
QtTerminal::truncatedMatrixVariableAtCursor()
{
    return truncatedMatrixVariableForBlock(textCursor().block().position());
}
//=============================================================================
bool
QtTerminal::openTruncatedMatrixAtCursor()
{
    QString variableName = truncatedMatrixVariableAtCursor();
    if (variableName.isEmpty()) {
        return false;
    }
    return VariablesEditor::openVariable(Nelson::QStringTowstring(variableName));
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
    int previous = maxBlockCount();
    document()->setMaximumBlockCount(newMax);
    return previous;
}
//=============================================================================
int
QtTerminal::maxBlockCount()
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
    if (!completerImpl) {
        return;
    }
    completerImpl->complete(prefix);
}
//=============================================================================
void
QtTerminal::updateModel(const std::wstring& prefix, const wstringVector& filesList,
    const wstringVector& builtinList, const wstringVector& macroList,
    const wstringVector& variableList, const wstringVector& fieldList,
    const wstringVector& propertyList, const wstringVector& methodList)
{
    if (completerImpl != nullptr) {
        completerImpl->updateModel(prefix, filesList, builtinList, macroList, variableList,
            fieldList, propertyList, methodList);
    }
}
//=============================================================================
QAbstractItemModel*
QtTerminal::modelFromNelson(const wstringVector& filesList, const wstringVector& builtinList,
    const wstringVector& macroList, const wstringVector& variableList,
    const wstringVector& fieldList, const wstringVector& propertyList,
    const wstringVector& methodList)
{
    if (completerImpl) {
        return completerImpl->modelFromNelson(
            filesList, builtinList, macroList, variableList, fieldList, propertyList, methodList);
    }
    QStringList words;

    auto appendItemsWithPostfix = [&](const wstringVector& list, const std::wstring& postfix) {
        for (const auto& k : list) {
            words.append(
                wstringToQString(k) + QString(" (") + wstringToQString(postfix) + QString(")"));
        }
    };

    appendItemsWithPostfix(variableList, POSTFIX_VARIABLE);
    appendItemsWithPostfix(fieldList, POSTFIX_FIELD);
    appendItemsWithPostfix(propertyList, POSTFIX_PROPERTY);
    appendItemsWithPostfix(methodList, POSTFIX_METHOD);
    appendItemsWithPostfix(macroList, POSTFIX_MACRO);
    appendItemsWithPostfix(builtinList, POSTFIX_BUILTIN);
    appendItemsWithPostfix(filesList, POSTFIX_FILES);
    return new QStringListModel(words, completerImpl ? completerImpl->completer() : nullptr);
}
//=============================================================================
void
QtTerminal::insertCompletion(const QString& completion)
{
    // Decorated form: "name (type)". Strip the decoration defensively: lastIndexOf
    // returns -1 when the postfix is absent and must never be fed to replace().
    const QString FILE_OR_DIR = QString(" (") + wstringToQString(POSTFIX_FILES) + QString(")");
    wstringVector postfixStrings;
    postfixStrings.push_back(POSTFIX_FILES);
    postfixStrings.push_back(POSTFIX_BUILTIN);
    postfixStrings.push_back(POSTFIX_MACRO);
    postfixStrings.push_back(POSTFIX_VARIABLE);
    postfixStrings.push_back(POSTFIX_FIELD);
    postfixStrings.push_back(POSTFIX_PROPERTY);
    postfixStrings.push_back(POSTFIX_METHOD);

    const bool isPathCompletion = completion.endsWith(FILE_OR_DIR);
    QString cleanedCompletion = completion;
    for (const std::wstring& postfix : postfixStrings) {
        const QString decoration
            = wstringToQString(std::wstring(L" (") + postfix + std::wstring(L")"));
        const qsizetype index = cleanedCompletion.lastIndexOf(decoration);
        if (index >= 0) {
            cleanedCompletion.remove(index, decoration.size());
        }
    }
    insertCompletionItem(cleanedCompletion, isPathCompletion);
}
//=============================================================================
void
QtTerminal::insertCompletionItem(const QString& rawName, bool isPathCompletion)
{
    QString cleanedCompletion = rawName;
    if (completerImpl && completerImpl->completer()) {
        if (completerImpl->completer()->widget() != this) {
            return;
        }
    }
    bool backup = mCommandLineReady;
    std::wstring currentLineW = QStringTowstring(getCurrentCommandLine());
    mCommandLineReady = backup;

    std::wstring cleanedCompletionW = QStringTowstring(cleanedCompletion);
    std::wstring newLine = completerLine(currentLineW, cleanedCompletionW, isPathCompletion);

    replaceCurrentCommandLine(newLine);
    updateHistoryToken();
}
//=============================================================================
void
QtTerminal::onPostCommandReceived(const QString& command)
{
    postCommand(QStringTowstring(command));
}
//=============================================================================
void
QtTerminal::onCommandsReceived(const QStringList& commands)
{
    if (isAtPrompt() && !isInEditionZone()) {
        QTextCursor cur(document()->lastBlock());
        cur.movePosition(QTextCursor::EndOfBlock);
        setTextCursor(cur);
    }
    if (isInEditionZone()) {
        std::wstring line;
        for (auto command : commands) {
            line = line + QStringTowstring(command) + L"\n";
        }
        line.pop_back();
        QTextCursor tc = textCursor();
        bool backup = mCommandLineReady;
        replaceCurrentCommandLine(line);
        mCommandLineReady = backup;
        QKeyEvent keyPressEvent(QEvent::KeyPress, Qt::Key_Return, Qt::NoModifier);
        sendKeyEvent(&keyPressEvent);
    }
}
//=============================================================================
void
QtTerminal::onCommandsToCommandLineReceived(const QStringList& commands)
{
    if (commands.isEmpty()) {
        return;
    }
    if (isAtPrompt() && !isInEditionZone()) {
        QTextCursor cur(document()->lastBlock());
        cur.movePosition(QTextCursor::EndOfBlock);
        setTextCursor(cur);
    }
    if (isInEditionZone()) {
        std::wstring line;
        for (const auto& command : commands) {
            line = line + QStringTowstring(command) + L"\n";
        }
        line.pop_back();
        bool backup = mCommandLineReady;
        replaceCurrentCommandLine(line);
        mCommandLineReady = backup;
        setFocus();
    }
}
//=============================================================================
void
QtTerminal::onToTextEditorReceived()
{
    postCommand(L"editor('new_file', [])");
}
//=============================================================================
void
QtTerminal::wheelEvent(QWheelEvent* wheelEvent)
{
    if (wheelEvent->modifiers() == Qt::ControlModifier) {
        if (wheelEvent->angleDelta().y() > 0) {
            zoomTerminalIn();
        } else {
            zoomTerminalOut();
        }
        wheelEvent->accept();
    } else {
        QTextEdit::wheelEvent(wheelEvent);
    }
}
//=============================================================================
void
QtTerminal::showFindDialog()
{
    bool ok = false;
    QString text
        = QInputDialog::getText(this, TR("Find"), TR("Find:"), QLineEdit::Normal, lastSearch, &ok);
    if (ok) {
        lastSearch = text;
        lastSearchCaseSensitive = false;
        findNext();
    }
}
//=============================================================================
void
QtTerminal::findPrevious()
{
    if (lastSearch.isEmpty()) {
        return;
    }

    QTextDocument::FindFlags flags = QTextDocument::FindBackward;
    if (lastSearchCaseSensitive) {
        flags |= QTextDocument::FindCaseSensitively;
    }

    bool found = this->find(lastSearch, flags);
    if (!found) {
        QTextCursor c = this->textCursor();
        c.movePosition(QTextCursor::End);
        this->setTextCursor(c);
        found = this->find(lastSearch, flags);
        if (!found) {
            QApplication::beep();
        }
    }
}
//=============================================================================
void
QtTerminal::findNext()
{
    if (lastSearch.isEmpty()) {
        return;
    }
    QTextDocument::FindFlags flags;
    if (lastSearchCaseSensitive) {
        flags |= QTextDocument::FindCaseSensitively;
    }
    bool found = this->find(lastSearch, flags);
    if (!found) {
        QTextCursor c = this->textCursor();
        c.movePosition(QTextCursor::Start);
        this->setTextCursor(c);
        found = this->find(lastSearch, flags);
        if (!found) {
            QApplication::beep();
        }
    }
}
//=============================================================================
void
QtTerminal::insertCompletionImpl(const QString& completion)
{
    insertCompletion(completion);
}
//=============================================================================
