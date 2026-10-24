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
#include <QtCore/QHash>
#include <QtCore/QUrl>
#include <QtGui/QTextBlock>
#include <QtGui/QTextCursor>
#include <QtGui/QColor>
#include <QtWidgets/QTextBrowser>
#include <QtWidgets/QCompleter>
#include <string>
#include "Types.hpp"
#include "Interface.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
class QtTerminalCompleter;
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
    outputStyledText(const std::wstring& msg, const TextStyle& style);
    void
    outputAnsiText(const std::wstring& msg, bool asError);
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
    void
    insertExpandableText(const std::wstring& label, const std::wstring& expandedText,
        const std::wstring& contextName = L"", long long contextValue = 0);
    void
    insertTruncatedMatrix(const std::wstring& truncatedText, const std::wstring& label,
        const std::wstring& expandedText, const std::wstring& variableName);

    bool
    isAtPrompt();
    // True when the prompt kept on screen by a silent callback serves the next getLine as is.
    bool
    canReusePrompt(const std::wstring& prompt) const;
    // Re-read the command window preferences (font size, wrap, truncate) from
    // QSettings and apply them live. Called by the preferences dialog.
    void
    reloadPreferences();
    int
    setMaxBlockCount(int newMax);
    int
    maxBlockCount();

    void
    sendKeyEvent(QKeyEvent* event);

    void
    insertCompletion(const QString& completion);

public:
    // Insert a completion using the raw candidate name (without the " (type)" decoration).
    void
    insertCompletionItem(const QString& rawName, bool isPathCompletion);

signals:
    void
    promptDisplayed();

private slots:
    void
    insertCompletionImpl(const QString& completion);
    void
    handleAnchorClicked(const QUrl& url);

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
    bool
    event(QEvent* event) override;
    void
    keyPressEvent(QKeyEvent* event) override;
    void
    insertFromMimeData(const QMimeData* source) override;
    void
    contextMenuEvent(QContextMenuEvent* event) override;
    void
    dragEnterEvent(QDragEnterEvent* event) override;
    void
    dragMoveEvent(QDragMoveEvent* event) override;
    void
    dropEvent(QDropEvent* event) override;

    QString
    getCurrentCommandLine();
    void
    printMessage(QString msg, DISP_MODE mode);
    // Render a message that may embed OSC 8 hyperlinks, turning each linked run
    // into a clickable anchor (used by the error/warning location links).
    void
    insertHyperlinkedText(QTextCursor& cursor, const QString& text, DISP_MODE mode);
    // Handle a clicked nelson-open:<line>:<path> link by opening the editor there.
    void
    openFileAtLineFromUrl(const QUrl& url);
    void
    printStyledMessage(QString msg, const TextStyle& style);
    void
    printAnsiMessage(const std::wstring& msg, DISP_MODE mode);
    void
    insertFormattedText(QTextCursor& cursor, const QString& text, DISP_MODE mode);
    void
    insertFormattedText(QTextCursor& cursor, const QString& text, const QTextCharFormat& format);
    void
    printPrompt(QString prompt);
    bool mCommandLineReady;
    QString mPrompt;
    std::wstring lineToSend;
    void
    printNewLine();

    QTextBlock promptBlock;
    // Set when getLine returns for a queued callback and leaves the prompt line untouched.
    bool promptKeptAlive = false;
    // True while the wait cursor pushed at the end of getLine is still to be popped.
    bool promptWaitCursorSet = false;
    // Output is coming while the prompt line is on screen: empty that line first.
    void
    releaseKeptPrompt(bool messageStartsWithNewline = false);
    bool
    isInEditionZone();
    void
    moveCursorToPrompt();
    bool
    handleBackspaceKeyPress();
    bool
    handlePreviousCharKeyPress();
    bool
    handleUpKeyPress();
    bool
    handleDownKeyPress();
    bool
    handleTabCompletion();
    bool
    handleHomePress();

    bool
    replaceCurrentCommandLine(const std::wstring& newline);
    bool
    updateHistoryToken();

    void
    ensureInputColor();

    double latexImageScale;
    void
    tagLatexImages(int startPosition, int endPosition);
    void
    updateLatexImageSizes();
    void
    zoomTerminalIn();
    void
    zoomTerminalOut();
    void
    zoomTerminalReset();
    void
    saveFontSizeSetting();
    int defaultFontPointSize;

    // Auto-scroll helpers: keep the user selection/scroll position untouched when
    // output is appended while the view is not at the bottom.
    bool
    isScrolledToBottom() const;
    void
    finishOutputScroll(QTextCursor& cursor, bool wasAtBottom, int previousScrollValue);

    void
    applyWrapLines(bool wrap);
    bool wrapLinesEnabled;

    static QString
    quotePathForNelson(const QString& path);

    // Helper to apply a foreground color to a cursor/selection and optionally
    // merge with existing formats and make the format current for subsequent typing.
    void
    applyForegroundToCursor(
        QTextCursor& cursor, const QColor& color, bool merge = false, bool makeCurrent = false);

    QTextCharFormat
    createCharFormatWithForeground(const QColor& color);
    QTextCharFormat
    createCharFormat(const TextStyle& style, DISP_MODE fallbackMode);

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
    QAction* truncateMatricesAction;
    QAction* openVariableAction;
    QAction* wrapLinesAction;

    QtTerminalCompleter* completerImpl;

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

    QAction* findAction;
    QAction* findNextAction;
    QAction* findPreviousAction;
    QString lastSearch;
    bool lastSearchCaseSensitive;

    struct ExpandableTextItem
    {
        QString text;
        int blockPosition = -1;
        int firstBlockPosition = -1;
        int lastBlockPosition = -1;
        QString variableName;
        QString contextName;
        long long contextValue = 0;
        bool expanded = false;
        bool matrixDisplay = false;
    };

    QHash<QString, ExpandableTextItem> expandableTextItems;
    int expandableTextCounter;

    QString
    truncatedMatrixVariableForBlock(int blockPosition) const;
    QString
    truncatedMatrixVariableAtCursor();
    bool
    openTruncatedMatrixAtCursor();

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
    onCommandsToCommandLineReceived(const QStringList& commands);
    void
    onPostCommandReceived(const QString& command);

    void
    onToTextEditorReceived();

    void
    wheelEvent(QWheelEvent* wheelEvent) override;
};
//=============================================================================
