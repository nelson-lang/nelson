//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtShellTerminal.h"
#include "QtDockTitleBar.h"
#include <QtCore/QDir>
#include <QtCore/QFileInfo>
#include <QtGui/QClipboard>
#include <QtGui/QCloseEvent>
#include <QtGui/QIcon>
#include <QtGui/QKeyEvent>
#include <QtGui/QPainter>
#include <QtGui/QPaintEvent>
#include <QtGui/QTextBlock>
#include <QtGui/QTextCursor>
#include <QtGui/QTextLayout>
#include <QtWidgets/QApplication>
#include <QtWidgets/QMessageBox>
#include <string>
#ifdef _MSC_VER
#include <Windows.h>
#endif
#include "NelsonConfiguration.hpp"
#include "NelsonPalette.hpp"
#include "QStringConverter.hpp"
#include "QtTranslation.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
// The transcript keeps the whole session in memory: bound it so a long lived
// terminal does not grow without limit.
constexpr qsizetype MAX_TRANSCRIPT_LENGTH = 1000000;
constexpr qsizetype TRANSCRIPT_KEPT_LENGTH = 800000;
constexpr int MAX_TRANSCRIPT_BLOCKS = 5000;
constexpr int MAX_HISTORY_ENTRIES = 200;
constexpr qsizetype PASTE_CONFIRMATION_LINE_THRESHOLD = 5;
//=============================================================================
#ifdef _MSC_VER
bool
decodeWindowsBytes(
    const QByteArray& data, unsigned int codePage, unsigned long flags, QString& text)
{
    if (data.isEmpty()) {
        text.clear();
        return true;
    }
    int requiredSize = MultiByteToWideChar(
        codePage, flags, data.constData(), static_cast<int>(data.size()), nullptr, 0);
    if (requiredSize <= 0) {
        return false;
    }
    std::wstring buffer(static_cast<size_t>(requiredSize), L'\0');
    int convertedSize = MultiByteToWideChar(codePage, flags, data.constData(),
        static_cast<int>(data.size()), buffer.data(), requiredSize);
    if (convertedSize <= 0) {
        return false;
    }
    text = QString::fromWCharArray(buffer.data(), convertedSize);
    return true;
}
//=============================================================================
QString
decodeProcessOutput(const QByteArray& data)
{
    QString text;
    if (decodeWindowsBytes(data, CP_UTF8, MB_ERR_INVALID_CHARS, text)) {
        return text;
    }
    if (decodeWindowsBytes(data, GetOEMCP(), 0, text)) {
        return text;
    }
    return QString::fromLocal8Bit(data);
}
//=============================================================================
QByteArray
encodeProcessInput(const QString& text)
{
    if (text.isEmpty()) {
        return QByteArray();
    }
    std::wstring wideText = text.toStdWString();
    int requiredSize = WideCharToMultiByte(GetOEMCP(), 0, wideText.data(),
        static_cast<int>(wideText.size()), nullptr, 0, nullptr, nullptr);
    if (requiredSize <= 0) {
        return text.toLocal8Bit();
    }
    QByteArray data(requiredSize, Qt::Uninitialized);
    int convertedSize = WideCharToMultiByte(GetOEMCP(), 0, wideText.data(),
        static_cast<int>(wideText.size()), data.data(), requiredSize, nullptr, nullptr);
    if (convertedSize <= 0) {
        return text.toLocal8Bit();
    }
    return data;
}
#else
QString
decodeProcessOutput(const QByteArray& data)
{
    return QString::fromLocal8Bit(data);
}
//=============================================================================
QByteArray
encodeProcessInput(const QString& text)
{
    return text.toLocal8Bit();
}
#endif
//=============================================================================
QString
stripControlSequences(const QString& text)
{
    QString result;
    result.reserve(text.size());
    for (qsizetype k = 0; k < text.size(); ++k) {
        const QChar ch = text.at(k);
        const ushort unicode = ch.unicode();
        if (unicode == 0x1B) {
            if (k + 1 < text.size() && text.at(k + 1) == QChar('[')) {
                k += 2;
                while (k < text.size()) {
                    const ushort finalChar = text.at(k).unicode();
                    if (finalChar >= 0x40 && finalChar <= 0x7E) {
                        break;
                    }
                    ++k;
                }
                continue;
            }
            if (k + 1 < text.size() && text.at(k + 1) == QChar(']')) {
                k += 2;
                while (k < text.size() && text.at(k).unicode() != 0x07) {
                    if (text.at(k).unicode() == 0x1B && k + 1 < text.size()
                        && text.at(k + 1) == QChar('\\')) {
                        ++k;
                        break;
                    }
                    ++k;
                }
                continue;
            }
            continue;
        }
        if (unicode == '\r') {
            result.append('\n');
            if (k + 1 < text.size() && text.at(k + 1).unicode() == '\n') {
                ++k;
            }
            continue;
        }
        if ((unicode < 0x20 && unicode != '\n' && unicode != '\t') || unicode == 0x7F) {
            continue;
        }
        result.append(ch);
    }
    return result;
}
//=============================================================================
} // namespace
//=============================================================================
QtShellTerminalEdit::QtShellTerminalEdit(QtShellTerminal* terminal, QWidget* parent)
    : QPlainTextEdit(parent), m_terminal(terminal), m_cursorVisible(true)
{
    setReadOnly(true);
    setUndoRedoEnabled(false);
    setMaximumBlockCount(MAX_TRANSCRIPT_BLOCKS);
    setLineWrapMode(QPlainTextEdit::NoWrap);
    setFocusPolicy(Qt::StrongFocus);
    viewport()->setFocusPolicy(Qt::StrongFocus);
    setCursorWidth(0);
    m_cursorTimer.setInterval(530);
    connect(&m_cursorTimer, &QTimer::timeout, this, [this]() {
        m_cursorVisible = !m_cursorVisible;
        viewport()->update();
    });
    m_cursorTimer.start();
}
//=============================================================================
void
QtShellTerminalEdit::keyPressEvent(QKeyEvent* event)
{
    const Qt::KeyboardModifiers modifiers = event->modifiers() & ~Qt::KeypadModifier;
    if (modifiers == (Qt::ControlModifier | Qt::ShiftModifier)) {
        if (event->key() == Qt::Key_C) {
            copy();
            event->accept();
            return;
        }
        if (event->key() == Qt::Key_V) {
            m_terminal->pasteUserText(QApplication::clipboard()->text());
            event->accept();
            return;
        }
    }
    if (event->key() == Qt::Key_Return || event->key() == Qt::Key_Enter) {
        m_terminal->submitUserInput();
        event->accept();
        return;
    }
    if (event->key() == Qt::Key_Backspace) {
        m_terminal->backspaceUserInput();
        event->accept();
        return;
    }
    if (event->key() == Qt::Key_Tab) {
        m_terminal->insertUserText("\t");
        event->accept();
        return;
    }
    if (modifiers == Qt::NoModifier && event->key() == Qt::Key_Up) {
        m_terminal->historyPrevious();
        event->accept();
        return;
    }
    if (modifiers == Qt::NoModifier && event->key() == Qt::Key_Down) {
        m_terminal->historyNext();
        event->accept();
        return;
    }
    if (modifiers == Qt::ControlModifier && event->key() == Qt::Key_C) {
        m_terminal->writeText(QString(QChar(0x03)));
        event->accept();
        return;
    }
    const QString text = event->text();
    if (!text.isEmpty() && (modifiers == Qt::NoModifier || modifiers == Qt::ShiftModifier)) {
        m_terminal->insertUserText(text);
        event->accept();
        return;
    }
    QPlainTextEdit::keyPressEvent(event);
}
//=============================================================================
void
QtShellTerminalEdit::paintEvent(QPaintEvent* event)
{
    QPlainTextEdit::paintEvent(event);
    if (!m_cursorVisible || !m_terminal->isRunning()) {
        return;
    }
    QTextCursor cursor = textCursor();
    cursor.movePosition(QTextCursor::End);
    QRect cursorRectangle = cursorRect(cursor);
    if (cursorRectangle.isValid() && cursorRectangle.top() <= viewport()->height()
        && cursorRectangle.bottom() >= 0) {
        QPainter painter(viewport());
        painter.fillRect(QRect(cursorRectangle.left(), cursorRectangle.top() + 2, 2,
                             qMax(2, cursorRectangle.height() - 4)),
            palette().color(QPalette::Text));
        return;
    }

    QTextBlock block = document()->lastBlock();
    if (!block.isValid()) {
        return;
    }
    const QRectF blockRectangle = blockBoundingGeometry(block).translated(contentOffset());
    const QString lineText = block.text();
    const QFontMetrics metrics(font());
    int cursorX = qRound(blockRectangle.left()) + metrics.horizontalAdvance(lineText);
    QTextLayout* layout = block.layout();
    if (layout != nullptr && layout->lineCount() > 0) {
        QTextLine line = layout->lineAt(layout->lineCount() - 1);
        cursorX = qRound(blockRectangle.left() + line.cursorToX(lineText.size()));
    }
    const int cursorY = qRound(blockRectangle.top());
    const int cursorHeight = qMax(2, metrics.height());
    if (cursorY + cursorHeight < 0 || cursorY > viewport()->height()) {
        return;
    }
    QPainter painter(viewport());
    painter.fillRect(QRect(cursorX, cursorY, 2, cursorHeight), palette().color(QPalette::Text));
}
//=============================================================================
void
QtShellTerminalEdit::focusInEvent(QFocusEvent* event)
{
    m_cursorVisible = true;
    viewport()->update();
    QPlainTextEdit::focusInEvent(event);
}
//=============================================================================
void
QtShellTerminalEdit::focusOutEvent(QFocusEvent* event)
{
    m_cursorVisible = false;
    viewport()->update();
    QPlainTextEdit::focusOutEvent(event);
}
//=============================================================================
QtShellTerminal::QtShellTerminal(QWidget* parent, const Nelson::Terminal::TerminalOptions& options)
    : QDockWidget(parent)
    , m_process(new QProcess(this))
    , m_textEdit(new QtShellTerminalEdit(this, this))
    , m_windowStyle(Nelson::wstringToQString(options.windowStyle))
    , m_theme(Nelson::wstringToQString(options.theme))
    , m_historyIndex(0)
    , m_shellEchoesInput(false)
    , m_promptRestorePending(false)
    , m_commandAwaitingPrompt(false)
    , m_exitCode(-1)
{
    m_shell = resolvedShell(Nelson::wstringToQString(options.shell));
    // An interactive shell reading from a pipe prints its own prompt and echoes back each
    // command line: local echo and prompt restoration would then show them twice.
    m_shellEchoesInput = shellArguments().contains("-i");
    QString title = Nelson::wstringToQString(options.name);
    if (title.isEmpty()) {
        title = TR("Terminal");
    }
    setTerminalName(title);
    const QString iconPath = Nelson::wstringToQString(
        NelsonConfiguration::getInstance()->getNelsonRootDirectory() + L"/resources/console.svg");
    setWindowIcon(QIcon(iconPath));
    setObjectName(QString("QtShellTerminal_%1").arg(reinterpret_cast<quintptr>(this)));
    setWidget(m_textEdit);
    // Collapse chevron in the title bar. An empty settings prefix disables persisting the
    // collapsed state: this dock is created on demand and not restored at startup.
    setTitleBarWidget(new QtDockTitleBar(this, QString()));
    setAllowedAreas(Qt::AllDockWidgetAreas);
    applyTheme();
    m_promptRestoreTimer.setSingleShot(true);
    m_promptRestoreTimer.setInterval(120);
    connect(&m_promptRestoreTimer, &QTimer::timeout, this, [this]() { restorePromptIfNeeded(); });

    connect(m_process, &QProcess::readyReadStandardOutput, this,
        [this]() { appendOutput(m_process->readAllStandardOutput()); });
    connect(m_process, &QProcess::readyReadStandardError, this,
        [this]() { appendOutput(m_process->readAllStandardError()); });
    connect(m_process, QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished), this,
        [this](int code, QProcess::ExitStatus) {
            m_exitCode = code;
            appendText(QString("\n[%1 %2]\n").arg(TR("Process exited with code")).arg(code));
        });

    const QString startupCommand = Nelson::wstringToQString(options.startupCommand);
    connect(m_process, &QProcess::started, this, [this, startupCommand]() {
        if (!startupCommand.isEmpty()) {
            writeText(startupCommand);
#ifdef _MSC_VER
            writeText("\r\n");
#else
            writeText("\n");
#endif
        }
        writeProcessPrompt();
    });
}
//=============================================================================
QtShellTerminal::~QtShellTerminal() { closeTerminal(); }
//=============================================================================
QString
QtShellTerminal::resolvedShell(const QString& requestedShell) const
{
    if (!requestedShell.isEmpty()) {
        return requestedShell;
    }
#ifdef _MSC_VER
    QString shell = QString::fromLocal8Bit(qgetenv("COMSPEC"));
    return shell.isEmpty() ? QString("cmd.exe") : shell;
#else
    QString shell = QString::fromLocal8Bit(qgetenv("SHELL"));
    return shell.isEmpty() ? QString("/bin/sh") : shell;
#endif
}
//=============================================================================
QStringList
QtShellTerminal::shellArguments() const
{
#ifdef _MSC_VER
    return QStringList();
#else
    // QProcess connects the child to pipes, not to a tty: a POSIX shell then starts in
    // non-interactive mode and never emits its prompt. '-i' forces interactive mode so
    // PS1 is printed (on stderr, which is captured too).
    static const QStringList interactiveShells
        = { "sh", "bash", "zsh", "ksh", "dash", "fish", "csh", "tcsh" };
    const QString shellName = QFileInfo(m_shell).fileName();
#ifdef Q_OS_MACOS
    if (shellName == "zsh") {
        return QStringList();
    }
#endif
    if (interactiveShells.contains(shellName)) {
        return QStringList() << "-i";
    }
    return QStringList();
#endif
}
//=============================================================================
bool
QtShellTerminal::start()
{
    m_process->setWorkingDirectory(QDir::currentPath());
    m_process->setProcessChannelMode(QProcess::SeparateChannels);
    m_process->start(m_shell, shellArguments());
    return m_process->waitForStarted(1000);
}
//=============================================================================
bool
QtShellTerminal::usesProcessManagedPrompt() const
{
#ifdef Q_OS_MACOS
    return QFileInfo(m_shell).fileName() == "zsh";
#else
    return false;
#endif
}
//=============================================================================
QString
QtShellTerminal::shellPromptCommand() const
{
    return QStringLiteral("d=${PWD##*/}; [ -n \"$d\" ] || d=/; printf '%s %% ' \"$d\"\n");
}
//=============================================================================
void
QtShellTerminal::writeRawText(const QString& text)
{
    if (m_process->state() == QProcess::NotRunning || text.isEmpty()) {
        return;
    }
    m_process->write(encodeProcessInput(text));
}
//=============================================================================
void
QtShellTerminal::writeProcessPrompt()
{
    if (!usesProcessManagedPrompt()) {
        return;
    }
    writeRawText(shellPromptCommand());
}
//=============================================================================
void
QtShellTerminal::writeText(const QString& text)
{
    if (m_process->state() == QProcess::NotRunning) {
        return;
    }
    if ((text.contains(QChar('\n')) || text.contains(QChar('\r'))) && !text.trimmed().isEmpty()) {
        m_commandAwaitingPrompt = true;
        m_promptRestorePending = false;
        m_promptRestoreTimer.stop();
    }
    writeRawText(text);
}
//=============================================================================
void
QtShellTerminal::insertUserText(const QString& text)
{
    if (m_process->state() == QProcess::NotRunning || text.isEmpty()) {
        return;
    }
    restorePromptIfNeeded();
    QString normalizedText = text;
    normalizedText.remove(QChar('\r'));
    normalizedText.remove(QChar('\n'));
    if (normalizedText.isEmpty()) {
        return;
    }
    m_pendingInput.append(normalizedText);
    appendText(normalizedText);
}
//=============================================================================
void
QtShellTerminal::pasteUserText(const QString& text)
{
    QString normalizedText = text;
    normalizedText.replace("\r\n", "\n");
    normalizedText.replace(QChar('\r'), QChar('\n'));
    const QStringList lines = normalizedText.split(QChar('\n'));
    if (lines.size() > PASTE_CONFIRMATION_LINE_THRESHOLD) {
        // Each line but the last is executed immediately: ask before a bulk paste.
        const QMessageBox::StandardButton answer = QMessageBox::question(this, TR("Terminal"),
            TR("The clipboard contains %1 lines that will be executed. Do you want to continue?")
                .arg(lines.size()),
            QMessageBox::Yes | QMessageBox::No, QMessageBox::No);
        if (answer != QMessageBox::Yes) {
            return;
        }
    }
    for (qsizetype k = 0; k < lines.size(); ++k) {
        insertUserText(lines.at(k));
        if (k + 1 < lines.size()) {
            submitUserInput();
        }
    }
}
//=============================================================================
void
QtShellTerminal::submitUserInput()
{
    if (m_process->state() == QProcess::NotRunning) {
        return;
    }
    const bool hasCommand = !m_pendingInput.trimmed().isEmpty();
    if (m_shellEchoesInput && hasCommand) {
        m_pendingEcho = m_pendingInput.trimmed();
    }
    if (hasCommand) {
        addToHistory(m_pendingInput.trimmed());
    }
    m_historyIndex = m_history.size();
    appendText("\n");
    writeText(m_pendingInput);
#ifdef _MSC_VER
    writeText("\r\n");
#else
    writeText("\n");
#endif
    writeProcessPrompt();
    m_pendingInput.clear();
    m_promptRestorePending = false;
    m_commandAwaitingPrompt = hasCommand;
    m_promptRestoreTimer.stop();
}
//=============================================================================
void
QtShellTerminal::backspaceUserInput()
{
    if (m_pendingInput.isEmpty()) {
        return;
    }
    m_pendingInput.chop(1);
    if (!m_transcript.isEmpty()) {
        m_transcript.chop(1);
    }
    QTextCursor cursor = m_textEdit->textCursor();
    cursor.movePosition(QTextCursor::End);
    cursor.deletePreviousChar();
    m_textEdit->setTextCursor(cursor);
}
//=============================================================================
void
QtShellTerminal::addToHistory(const QString& command)
{
    if (command.isEmpty()) {
        return;
    }
    if (!m_history.isEmpty() && m_history.constLast() == command) {
        return;
    }
    m_history.append(command);
    while (m_history.size() > MAX_HISTORY_ENTRIES) {
        m_history.removeFirst();
    }
}
//=============================================================================
void
QtShellTerminal::replacePendingInput(const QString& text)
{
    while (!m_pendingInput.isEmpty()) {
        backspaceUserInput();
    }
    if (!text.isEmpty()) {
        insertUserText(text);
    }
}
//=============================================================================
void
QtShellTerminal::historyPrevious()
{
    if (m_history.isEmpty()) {
        return;
    }
    if (m_historyIndex > m_history.size()) {
        m_historyIndex = m_history.size();
    }
    if (m_historyIndex == 0) {
        return;
    }
    --m_historyIndex;
    replacePendingInput(m_history.at(m_historyIndex));
}
//=============================================================================
void
QtShellTerminal::historyNext()
{
    if (m_history.isEmpty()) {
        return;
    }
    if (m_historyIndex >= m_history.size() - 1) {
        m_historyIndex = m_history.size();
        replacePendingInput(QString());
        return;
    }
    ++m_historyIndex;
    replacePendingInput(m_history.at(m_historyIndex));
}
//=============================================================================
QString
QtShellTerminal::readText()
{
    restorePromptIfNeeded();
    return m_transcript;
}
//=============================================================================
void
QtShellTerminal::clearTerminal()
{
    m_transcript.clear();
    m_pendingInput.clear();
    m_pendingEcho.clear();
    m_promptRestorePending = false;
    m_commandAwaitingPrompt = false;
    m_promptRestoreTimer.stop();
    m_textEdit->clear();
}
//=============================================================================
void
QtShellTerminal::focusTerminal()
{
    show();
    raise();
    activateWindow();
    m_textEdit->setFocus();
}
//=============================================================================
void
QtShellTerminal::closeTerminal()
{
    if (m_process && m_process->state() != QProcess::NotRunning) {
        m_process->terminate();
        if (!m_process->waitForFinished(1000)) {
            m_process->kill();
            m_process->waitForFinished(1000);
        }
    }
    hide();
}
//=============================================================================
bool
QtShellTerminal::isRunning() const
{
    return m_process->state() != QProcess::NotRunning;
}
//=============================================================================
int
QtShellTerminal::exitCode() const
{
    return isRunning() ? -1 : m_exitCode;
}
//=============================================================================
qint64
QtShellTerminal::processId() const
{
    return m_process->processId();
}
//=============================================================================
QString
QtShellTerminal::terminalShell() const
{
    return m_shell;
}
//=============================================================================
QString
QtShellTerminal::windowStyle() const
{
    return m_windowStyle;
}
//=============================================================================
QString
QtShellTerminal::theme() const
{
    return m_theme;
}
//=============================================================================
void
QtShellTerminal::setTerminalName(const QString& name)
{
    setWindowTitle(name);
}
//=============================================================================
QString
QtShellTerminal::terminalName() const
{
    return windowTitle();
}
//=============================================================================
void
QtShellTerminal::setTerminalTheme(const QString& theme)
{
    m_theme = theme;
    applyTheme();
}
//=============================================================================
void
QtShellTerminal::closeEvent(QCloseEvent* event)
{
    closeTerminal();
    event->accept();
}
//=============================================================================
void
QtShellTerminal::appendOutput(const QByteArray& data)
{
    QString text = stripControlSequences(decodeProcessOutput(data));
    if (!m_pendingEcho.isEmpty()) {
        const qsizetype echoIndex = text.indexOf(m_pendingEcho);
        if (echoIndex >= 0) {
            qsizetype echoSize = m_pendingEcho.size();
            if (echoIndex + echoSize < text.size()
                && text.at(echoIndex + echoSize) == QChar('\n')) {
                ++echoSize;
            }
            text.remove(echoIndex, echoSize);
            m_pendingEcho.clear();
        }
    }
    if (text.isEmpty()) {
        return;
    }
    appendText(text);
    if (m_shellEchoesInput) {
        return;
    }
    const bool promptSeen = updateLastPrompt(m_transcript.right(4096));
    if (promptSeen && !m_lastPrompt.isEmpty()) {
        if (m_transcript.endsWith(m_lastPrompt)) {
            m_commandAwaitingPrompt = false;
            m_promptRestorePending = false;
            m_promptRestoreTimer.stop();
            return;
        }
        m_promptRestorePending = true;
        m_promptRestoreTimer.start();
    }
}
//=============================================================================
void
QtShellTerminal::appendText(const QString& text)
{
    m_transcript.append(text);
    if (m_transcript.size() > MAX_TRANSCRIPT_LENGTH) {
        // Drop the oldest part: the transcript is only used for readText() and for
        // prompt detection, both of which look at the tail.
        m_transcript.remove(0, m_transcript.size() - TRANSCRIPT_KEPT_LENGTH);
    }
    QTextCursor cursor = m_textEdit->textCursor();
    cursor.movePosition(QTextCursor::End);
    cursor.insertText(text);
    m_textEdit->setTextCursor(cursor);
    m_textEdit->ensureCursorVisible();
    m_textEdit->viewport()->update();
}
//=============================================================================
bool
QtShellTerminal::updateLastPrompt(const QString& text)
{
    bool promptSeen = false;
    const QStringList lines = text.split(QChar('\n'));
    for (const QString& line : lines) {
        const qsizetype promptEnd = line.indexOf(QChar('>'));
        if (promptEnd > 0) {
            const QString candidate = line.left(promptEnd + 1);
            if (candidate.contains(":\\") || candidate.contains(":/")
                || candidate.startsWith("CLINK ")) {
                m_lastPrompt = candidate;
                promptSeen = true;
                continue;
            }
        }

        const QString candidate = line.trimmed();
        if (candidate.isEmpty() || candidate.size() > 160) {
            continue;
        }
        const QChar promptChar = candidate.at(candidate.size() - 1);
        if (promptChar == QChar('$') || promptChar == QChar('#') || promptChar == QChar('%')) {
            m_lastPrompt = candidate;
            if (usesProcessManagedPrompt() && !m_lastPrompt.endsWith(QChar(' '))) {
                m_lastPrompt.append(QChar(' '));
            }
            promptSeen = true;
        }
    }
    return promptSeen;
}
//=============================================================================
void
QtShellTerminal::restorePromptIfNeeded()
{
    if (m_shellEchoesInput || !m_promptRestorePending || m_lastPrompt.isEmpty() || !isRunning()) {
        return;
    }
    if (!m_transcript.endsWith('\n') && !m_transcript.endsWith(m_lastPrompt)) {
        appendText("\n");
    }
    if (!m_transcript.endsWith(m_lastPrompt)) {
        appendText(m_lastPrompt);
    }
    m_promptRestorePending = false;
    m_commandAwaitingPrompt = false;
}
//=============================================================================
void
QtShellTerminal::applyTheme()
{
    if (m_theme.compare("dark", Qt::CaseInsensitive) == 0) {
        m_textEdit->setStyleSheet("QPlainTextEdit { background: #1f1f1f; color: #f0f0f0; }");
    } else if (m_theme.compare("light", Qt::CaseInsensitive) == 0) {
        m_textEdit->setStyleSheet("QPlainTextEdit { background: #ffffff; color: #202020; }");
    } else {
        m_textEdit->setStyleSheet("");
        m_textEdit->setPalette(getNelsonPalette());
    }
}
//=============================================================================
