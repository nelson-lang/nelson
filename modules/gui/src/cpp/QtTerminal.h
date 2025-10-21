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
#include "nlsGui_exports.h"
#include <QtGui/QTextBlock>
#include <QtGui/QTextCursor>
#include <QtGui/QColor>
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

    // Find feature slots
    void
    showFindDialog();
    void
    findNext();
    void
    findPrevious();

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

    // Helper to apply a foreground color to a cursor/selection and optionally
    // merge with existing formats and make the format current for subsequent typing.
    void
    applyForegroundToCursor(
        QTextCursor& cursor, const QColor& color, bool merge = false, bool makeCurrent = false);

    // Create/modify a char format to set foreground color
    QTextCharFormat
    createCharFormatWithForeground(const QColor& color);

    // Map DISP_MODE to QColor
    QColor
    colorForMode(DISP_MODE mode);

    QMenu* contextMenu;
    QAction* helpOnSelectionAction;
    QAction* cutAction;
    QAction* copyAction;
    QAction* pasteAction;
    QAction* selectAllAction;
    QAction* clcAction;
    QAction* exportContentAction;
    QAction* stopAction;

    QCompleter* qCompleter;

    bool completionDisabled;

    void
    complete(QString prefix);
    void
    updateModel(const std::wstring& prefix, const wstringVector& filesList,
        const wstringVector& builtinList, const wstringVector& macroList,
        const wstringVector& variableList, const wstringVector& fieldList,
        const wstringVector& propertyList, const wstringVector& methodList);

    QAbstractItemModel*
    modelFromNelson(const wstringVector& filesList, const wstringVector& builtinList,
        const wstringVector& macroList, const wstringVector& variableList,
        const wstringVector& fieldList, const wstringVector& propertyList,
        const wstringVector& methodList);

    void
    createCompleter();
    void
    exportToPdf(const QString& filename);
    void
    exportToHtml(const QString& filename);

    // Find feature members
    QAction* findAction;
    QAction* findNextAction;
    QAction* findPreviousAction;
    QString lastSearch;
    bool lastSearchCaseSensitive;

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
    onExportContentAction();

    void
    onCommandsReceived(const QStringList& commands);
    void
    onPostCommandReceived(const QString& command);

    void
    onToTextEditorReceived();

    void
    wheelEvent(QWheelEvent* wheelEvent);
};
//=============================================================================
