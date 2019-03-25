//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
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
#pragma once
//=============================================================================
#include "nlsGui_exports.h"
#include <QtGui/QTextBlock>
#include <QtWidgets/QTextBrowser>
#include <string>
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
    getLine(const std::wstring &prompt);
    size_t
    getTerminalWidth();
    void
    outputMessage(const std::wstring &msg);
    void
    errorMessage(const std::wstring &msg);
    void
    warningMessage(const std::wstring &msg);
    void
    clearTerminal();
    void
    clearLine();
    void
    banner();
    void
    insertHtml(const std::wstring &msg);

    bool
    isAtPrompt();
    int
    setMaxBlockCount(int newMax);
    int
    getMaxBlockCount();

    void
    sendKeyEvent(QKeyEvent* event);

private:
    typedef enum
    {
        WARNING_DISP,
        STDOUT_DISP,
        STDERR_DISP,
        STDIN_DISP,
    } DISP_MODE;
    QString nelsonPath;
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
    QColor warningColor;
    QColor inputColor;
    QColor errorColor;
    QColor outputColor;
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
    replaceCurrentCommandLine(const std::wstring &newline);
    bool
    updateHistoryToken();

    QMenu* contextMenu;
    QAction* helpOnSelectionAction;
    QAction* cutAction;
    QAction* copyAction;
    QAction* pasteAction;
    QAction* selectAllAction;
    QAction* clcAction;
    QAction* stopAction;

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
};
//=============================================================================
