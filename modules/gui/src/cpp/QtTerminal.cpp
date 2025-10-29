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
#include <QtCore/QFile>
#include <QtCore/QSettings>
#include <QtGui/QClipboard>
#include <QtGui/QImageReader>
#include <QtGui/QKeyEvent>
#include <QtGui/QTextBlock>
#include <QtGui/QTextCursor>
#include <QtGui/QTextDocument>
#include <QtGui/QTextImageFormat>
#include <QtGui/QFontDatabase>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QInputDialog>
#include <QtWidgets/QLineEdit>

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
#include <QtGui/QTextDocumentFragment>
#include <QtGui/QPainter>
#include <QtPrintSupport/QPrinter>
#include "Nelson_VERSION.h"
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
#include "CompleterHelper.hpp"
#include "HistoryBrowser.hpp"
#include "QtHistoryBrowser.h"
#include "FileBrowser.hpp"
#include "QtFileBrowser.h"
#include "WorkspaceBrowser.hpp"
#include "QtWorkspaceBrowser.h"
#include "CallbackQueue.hpp"
#include "VariablesEditor.hpp"
#include "QtTerminalCompleter.h"
//=============================================================================
using namespace Nelson;
//=============================================================================
QtTerminal::QtTerminal(QWidget* parent) : QTextBrowser(parent)
{
    mCommandLineReady = false;
    completionDisabled = true;
    isFirstPrompt = true;
    completerImpl = nullptr;
    // qCompleter removed; use completerImpl

    QLocale us(QLocale::English, QLocale::UnitedStates);
    QLocale::setDefault(us);
    setPalette(getNelsonPalette());
    QFont f(wstringToQString(Nelson::getDefaultFontName()));
    f.setStyleHint(QFont::Monospace);
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

    lastSearch.clear();
    lastSearchCaseSensitive = false;

    // Find actions
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

    // Batch changes to avoid repeated layout/paint work
    cur.beginEditBlock();
    QTextCharFormat fmt;
    fmt.setForeground(getInputColor());
    cur.setCharFormat(fmt);
    cur.insertText(mPrompt);
    cur.setCharFormat(QTextCharFormat());
    cur.endEditBlock();

    setTextCursor(cur);

    if (isFirstPrompt) {
        QtHistoryBrowser* qtHistoryBrowser = (QtHistoryBrowser*)HistoryBrowser::getHistoryBrowser();
        QtFileBrowser* qtFileBrowser = (QtFileBrowser*)FileBrowser::getFileBrowser();
        QtWorkspaceBrowser* qtWorkspaceBrowser
            = (QtWorkspaceBrowser*)WorkspaceBrowser::getWorkspaceBrowser();

        QObject::connect(qtHistoryBrowser, SIGNAL(sendCommands(const QStringList&)), this,
            SLOT(onCommandsReceived(const QStringList&)));
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
    VariablesEditor::updateVariablesEditor();
    FileBrowser::updateFileBrowser();
    promptBlock = document()->lastBlock();

    // Enable cursor text
    setCursorWidth(QFontMetrics(font()).horizontalAdvance(QChar('x')));

    // Restore default icon cursor
    WorkspaceBrowser::updateWorkspaceBrowser();
    QApplication::restoreOverrideCursor();

    // Prepare evaluator and queues

    Nelson::Evaluator* eval
        = static_cast<Nelson::Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
    if (!eval) {
        return L"\n";
    }
    eval->commandQueue.clear();
    Nelson::CallbackQueue::getInstance()->clear();

    bool wasInterruptedByAction = false;

    // Main event processing loop
    while (true) {
        try {
            Nelson::ProcessEvents(true);
        } catch (const Exception& e) {
            e.printMe(eval->getInterface());
            wasInterruptedByAction = true;
            break;
        }

        if (!eval->commandQueue.isEmpty() || !Nelson::CallbackQueue::getInstance()->isEmpty()) {
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

    // Wait until lineToSend is not empty if it was empty before
    while (lineToSend.empty() && !wasInterruptedByAction) {
        Nelson::ProcessEvents(true);
    }

    lineToSend.clear();
    Nelson::ProcessEvents();

    // Disable cursor text
    setCursorWidth(0);

    // Change icon cursor to wait (computation)
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
    // force to restore input color
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
void
QtTerminal::sendKeyEvent(QKeyEvent* event)
{
    keyPressEvent(event);
}
//=============================================================================
void
QtTerminal::keyPressEvent(QKeyEvent* event)
{
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
            this->zoomOut();
            event->accept();
            return;
        } else if (event->key() == Qt::Key_Minus) {
            this->zoomIn();
            event->accept();
            return;
        }
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
    } else if (event->matches(QKeySequence::Copy)) {
        if (!mCommandLineReady) {
            NelsonConfiguration::getInstance()->setInterruptPending(true);
        }
    } else {
        switch (event->key()) {
        case Qt::Key_Tab: {
            if (completionDisabled) {
                event->accept();
                return;
            }
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
void
QtTerminal::printMessage(QString msg, DISP_MODE mode)
{
    mCommandLineReady = false;
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

    QColor col = colorForMode(mode);
    QTextCharFormat format = createCharFormatWithForeground(col);

    // Batch document edits to reduce layout/paint work
    cur.beginEditBlock();
    if (msg.contains("\b")) {
        QString buffer;
        buffer.reserve(msg.size());
        for (auto c : msg) {
            if (c == '\b') {
                if (!buffer.isEmpty()) {
                    cur.insertText(buffer, format);
                    buffer.clear();
                }
                cur.deletePreviousChar();
            } else {
                buffer.append(c);
            }
        }
        if (!buffer.isEmpty()) {
            cur.insertText(buffer, format);
        }
    } else {
        cur.insertText(msg, format);
    }
    cur.endEditBlock();

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
#define PREFERED_DIRECTORY_EXPORT_TO_PDF_HTML "preferedDirectoryExportPdfHtmlTo"
    QSettings settings(NELSON_PRODUCT_NAME, NELSON_SEMANTIC_VERSION_STRING);

    QString currentDir = QDir::currentPath();
    if (settings.contains(PREFERED_DIRECTORY_EXPORT_TO_PDF_HTML)) {
        currentDir = settings.value(PREFERED_DIRECTORY_EXPORT_TO_PDF_HTML).toString();
        QFileInfo fileinfo(currentDir);
        if (!fileinfo.isDir()) {
            currentDir = QDir::homePath();
        }
    } else {
        currentDir = QDir::homePath();
    }
    QString defaultFilePath = currentDir + "/document";

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
    settings.setValue(PREFERED_DIRECTORY_EXPORT_TO_PDF_HTML, QFileInfo(filePath).absolutePath());

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
        // Add find actions to context menu
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
    // Forward to completer impl for now (kept for compatibility)
    if (completerImpl) {
        return completerImpl->modelFromNelson(
            filesList, builtinList, macroList, variableList, fieldList, propertyList, methodList);
    }
    // fallback original implementation
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
    return new QStringListModel(words, completerImpl ? completerImpl->completer() : nullptr);
}
//=============================================================================
void
QtTerminal::insertCompletion(const QString& completion)
{
    // original implementation kept
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

    if (completerImpl && completerImpl->completer()) {
        if (completerImpl->completer()->widget() != this) {
            return;
        }
    }
    QTextCursor tc = textCursor();
    QString completionPrefix
        = completerImpl ? completerImpl->completer()->completionPrefix() : QString();
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
QtTerminal::showFindDialog()
{
    bool ok = false;
    QString text
        = QInputDialog::getText(this, TR("Find"), TR("Find:"), QLineEdit::Normal, lastSearch, &ok);
    if (ok) {
        lastSearch = text;
        lastSearchCaseSensitive = false; // default
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
        // wrap around
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
    // Forward vers l'implémentation publique existante.
    insertCompletion(completion);
}
//=============================================================================
