//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "nlsGui_exports.h"
#include <QtGui/QTextBlock>
#include <QtWidgets/QTextBrowser>
#include <QtWidgets/QCompleter>
#include <string>
#include "Types.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
class NLSGUI_IMPEXP QtTerminal : public QTextBrowser
{
    Q_OBJECT
public:
    explicit QtTerminal(QWidget* parent = nullptr);
    ~QtTerminal() override;
    void
    closeEvent(QCloseEvent* event) override;

public slots:
    std::wstring
    getLine(const std::wstring& prompt);
    size_t
    getTerminalWidth();
    size_t
    getTerminalHeight();
    void
    outputMessage(const std::wstring& msg);
    void
    errorMessage(const std::wstring& msg);
    void
    warningMessage(const std::wstring& msg);
    void
    clearTerminal();
    void
    clearLine();
    void
    banner();
    void
    insertHtml(const std::wstring& msg);

    bool
    isAtPrompt();
    int
    setMaxBlockCount(int newMax);
    int
    getMaxBlockCount();

    void
    sendKeyEvent(QKeyEvent* event);

private slots:
    void
    insertCompletion(const QString& completion);

private:
    enum DISP_MODE
    {
        WARNING_DISP,
        STDOUT_DISP,
        STDERR_DISP,
        STDIN_DISP,
    };
    QString lineBuffer;

    QString nelsonPath;
    bool isFirstPrompt;
    void
    keyPressEvent(QKeyEvent* event) override;
    void
    insertFromMimeData(const QMimeData* source) override;
    void
    contextMenuEvent(QContextMenuEvent* event) override;

    QString
    getCurrentCommandLine();
    void
    printMessage(QString msg, DISP_MODE mode);
    void
    printPrompt(QString prompt);
    bool mCommandLineReady;
    QString mPrompt;
    std::wstring lineToSend;
    void
    printNewLine();

    QTextBlock promptBlock;
    bool
    isInEditionZone();
    bool
    handleBackspaceKeyPress();
    bool
    handlePreviousCharKeyPress();
    bool
    handleUpKeyPress();
    bool
    handleDownKeyPress();
    bool
    handleHomePress();

    bool
    replaceCurrentCommandLine(const std::wstring& newline);
    bool
    updateHistoryToken();

    void
    ensureInputColor();

    QMenu* contextMenu;
    QAction* helpOnSelectionAction;
    QAction* cutAction;
    QAction* copyAction;
    QAction* pasteAction;
    QAction* selectAllAction;
    QAction* clcAction;
    QAction* stopAction;

    QCompleter* qCompleter;

    void
    complete(QString prefix);
    void
    updateModel(const std::wstring& prefix, const wstringVector& filesList,
        const wstringVector& builtinList, const wstringVector& macroList,
        const wstringVector& variableList);

    QAbstractItemModel*
    modelFromNelson(const wstringVector& filesList, const wstringVector& builtinList,
        const wstringVector& macroList, const wstringVector& variableList);

    void
    createCompleter();

public Q_SLOTS:
    void
    cut();
    void
    copy();
    void
    paste();
    void
    selectAll();
    void
    clc();
    void
    stopRun();
    void
    sendReturnKey();
    void
    helpOnSelection();

    void
    onCommandsReceived(const QStringList& commands);
    void
    onPostCommandReceived(const QString& command);

    void
    onToTextEditorReceived();
};
//=============================================================================
