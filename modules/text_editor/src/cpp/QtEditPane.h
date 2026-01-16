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
#include <QtWidgets/QCompleter>
#include <QtWidgets/QWidget>
#include "QtHighlighter.h"
#include "QtTextEdit.h"
#include "Evaluator.hpp"
//=============================================================================
class QCompleter;
class QtBreakpointArea;
//=============================================================================
using namespace Nelson;
//=============================================================================
class QtEditPane : public QWidget
{
    Q_OBJECT
    QtTextEdit* textEditor;
    QString currentFilename;
    QString currentEncoding;

public:
    QtEditPane(Evaluator* eval = nullptr);
    ~QtEditPane();
    QtTextEdit*
    getEditor();
    void
    setFileName(const QString& filename);
    QString
    getFileName();
    QString
    getEncoding();
    void
    setEncoding(const QString& encoding);

    QtBreakpointArea*
    getBreakpointArea();

    void
    refreshBreakpoints();

    void
    setDebugLine(int line);

    void
    clearDebugLine();

    int
    getDebugLine() const;

    void
    setExecutionLine(int line);

    void
    clearExecutionLine();

    int
    getExecutionLine() const;

private:
    Highlighter* highlight;
    QCompleter* completer;
    QtBreakpointArea* breakpointArea;
    Evaluator* nlsEvaluator;
};
//=============================================================================
