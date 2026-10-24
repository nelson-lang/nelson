//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <QtCore/QProcess>
#include <QtCore/QStringList>
#include <QtCore/QTimer>
#include <QtWidgets/QDockWidget>
#include <QtWidgets/QPlainTextEdit>
#include "Terminal.hpp"
//=============================================================================
class QFocusEvent;
class QPaintEvent;
class QtShellTerminal;
//=============================================================================
class QtShellTerminalEdit : public QPlainTextEdit
{
public:
    explicit QtShellTerminalEdit(QtShellTerminal* terminal, QWidget* parent = nullptr);

protected:
    void
    keyPressEvent(QKeyEvent* event) override;
    void
    paintEvent(QPaintEvent* event) override;
    void
    focusInEvent(QFocusEvent* event) override;
    void
    focusOutEvent(QFocusEvent* event) override;

private:
    QtShellTerminal* m_terminal;
    QTimer m_cursorTimer;
    bool m_cursorVisible;
};
//=============================================================================
class QtShellTerminal : public QDockWidget
{
public:
    explicit QtShellTerminal(QWidget* parent, const Nelson::Terminal::TerminalOptions& options);
    ~QtShellTerminal() override;

    bool
    start();
    void
    writeText(const QString& text);
    void
    insertUserText(const QString& text);
    void
    pasteUserText(const QString& text);
    void
    submitUserInput();
    void
    backspaceUserInput();
    QString
    readText();
    void
    clearTerminal();
    void
    focusTerminal();
    void
    closeTerminal();
    bool
    isRunning() const;
    int
    exitCode() const;
    qint64
    processId() const;
    QString
    terminalShell() const;
    QString
    windowStyle() const;
    QString
    theme() const;
    void
    setTerminalName(const QString& name);
    QString
    terminalName() const;
    void
    setTerminalTheme(const QString& theme);
    void
    historyPrevious();
    void
    historyNext();

protected:
    void
    closeEvent(QCloseEvent* event) override;

private:
    QString
    resolvedShell(const QString& requestedShell) const;
    QStringList
    shellArguments() const;
    bool
    usesProcessManagedPrompt() const;
    QString
    shellPromptCommand() const;
    void
    writeRawText(const QString& text);
    void
    writeProcessPrompt();
    void
    appendOutput(const QByteArray& data);
    void
    appendText(const QString& text);
    bool
    updateLastPrompt(const QString& text);
    void
    restorePromptIfNeeded();
    void
    applyTheme();
    void
    replacePendingInput(const QString& text);
    void
    addToHistory(const QString& command);

    QProcess* m_process;
    QtShellTerminalEdit* m_textEdit;
    QString m_transcript;
    QString m_pendingInput;
    QString m_lastPrompt;
    QString m_pendingEcho;
    QString m_shell;
    QString m_windowStyle;
    QString m_theme;
    QStringList m_history;
    qsizetype m_historyIndex;
    QTimer m_promptRestoreTimer;
    bool m_shellEchoesInput;
    bool m_promptRestorePending;
    bool m_commandAwaitingPrompt;
    int m_exitCode;
};
//=============================================================================
