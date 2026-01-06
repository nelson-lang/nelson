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
#include "QtTextEdit.h"
#include "Evaluator.hpp"
//=============================================================================
using namespace Nelson;
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

    void
    refreshBreakpoints();

    void
    toggleBreakpoint(int line);

    bool
    hasBreakpoint(int line) const;

    QSet<int>
    getBreakpointLines() const;

    void
    setExecutionLine(int line);

    void
    clearExecutionLine();

    int
    getExecutionLine() const;

protected:
    void
    paintEvent(QPaintEvent* paintEvent) override;

    void
    mousePressEvent(QMouseEvent* event) override;

private:
    QtTextEdit* tEditor;
    Evaluator* nlsEvaluator;
    QString currentFilename;
    QSet<int> breakpointLines;
    int currentExecutionLine = -1;

    int
    lineNumberAtPosition(int y) const;

signals:
    void
    breakpointToggled(int line, bool added);
};
//=============================================================================
