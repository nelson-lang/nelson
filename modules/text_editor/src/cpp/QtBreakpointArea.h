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
#include <QtWidgets/QWidget>
#include <QtCore/QSet>
#include <QtCore/QList>
#include <QtGui/QTextBlock>
#include "QtTextEdit.h"
#include "Evaluator.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// a breakpoint anchored on a text block; lastKnownLine is the evaluator line (0 before save)
struct EditorBreakpoint
{
    QTextBlock block;
    int lastKnownLine = 0;
    bool enabled = true;
    QString condition;

    int
    currentLine() const
    {
        return block.isValid() ? block.blockNumber() + 1 : 0;
    }
};
//=============================================================================
class QtBreakpointArea : public QWidget
{
    Q_OBJECT
public:
    QtBreakpointArea(QtTextEdit* textEditor, Evaluator* eval);

    void
    setFilename(const QString& filename);

    QString
    getFilename() const;

    // reloads the markers from the evaluator (the file as saved on disk)
    void
    refreshBreakpoints();

    void
    toggleBreakpoint(int line);

    bool
    hasBreakpoint(int line) const;

    bool
    isValidBreakpointLine(int lineNumber);

    int
    findNextValidBreakpointLine(int lineNumber);

    QSet<int>
    getBreakpointLines() const;

    const EditorBreakpoint*
    breakpointAt(int line) const;

    bool
    setBreakpointEnabled(int line, bool enabled);

    // returns false with the message when the condition does not parse
    bool
    setBreakpointCondition(int line, const QString& condition, QString& errorMessage);

    void
    setAllBreakpointsEnabled(bool enabled);

    void
    clearAllBreakpoints();

    // unsaved edits: the markers are drawn as invalid until the file is saved
    void
    setDocumentModified(bool modified);

    bool
    isDocumentModified() const;

    // the file was saved: moves, adds and removes are pushed to the evaluator
    void
    syncAfterSave();

    void
    setExecutionLine(int line);

    void
    clearExecutionLine();

    int
    getExecutionLine() const;

    void
    setErrorStop(bool errorStop, const QString& message);

    // marker colours
    static QColor
    activeColor();
    static QColor
    disabledColor();
    static QColor
    invalidColor();
    static QColor
    conditionalColor();
    static QColor
    executionColor();

protected:
    void
    paintEvent(QPaintEvent* paintEvent) override;

    void
    mousePressEvent(QMouseEvent* event) override;

    void
    contextMenuEvent(QContextMenuEvent* event) override;

    bool
    event(QEvent* event) override;

private:
    QtTextEdit* tEditor;
    Evaluator* nlsEvaluator;
    QString currentFilename;
    QList<EditorBreakpoint> breakpoints;
    // evaluator lines of markers removed while the document was modified
    QList<int> pendingRemovals;
    bool documentModified = false;
    int currentExecutionLine = -1;
    bool executionIsError = false;
    QString executionMessage;

    int
    lineNumberAtPosition(int y) const;

    bool
    isCommentOnlyLine(int lineNumber) const;

    QTextBlock
    blockForLine(int line) const;

    int
    indexOfLine(int line) const;

    void
    addBreakpointAt(int line);

    void
    removeBreakpointAt(int index);

    bool
    editConditionAt(int line);

    QString
    tooltipForLine(int line) const;

    QColor
    markerColor(const EditorBreakpoint& bp) const;

signals:
    void
    breakpointToggled(int line, bool added);
    void
    breakpointsChanged();
};
//=============================================================================
