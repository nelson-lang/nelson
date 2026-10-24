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
class QtCodeAnalyzerMarkerArea;
class QtTextIndent;
//=============================================================================
#define EOL_STYLE_CRLF "CRLF"
#define EOL_STYLE_LF "LF"
#define EOL_STYLE_CR "CR"
//=============================================================================
using namespace Nelson;
//=============================================================================
class QtEditPane : public QWidget
{
    Q_OBJECT
    QtTextEdit* textEditor;
    QString currentFilename;
    QString currentEncoding;
    QString currentEol;

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

    QString
    getEol();
    void
    setEol(const QString& eol);
    static QString
    defaultEol();
    static QString
    eolSequence(const QString& eol);

    QtBreakpointArea*
    getBreakpointArea();
    QtCodeAnalyzerMarkerArea*
    getCodeAnalyzerMarkerArea();
    void
    setCodeAnalyzerDiagnostics(const std::vector<Nelson::CodeAnalyzerDiagnostic>& diagnostics);
    std::vector<Nelson::CodeAnalyzerDiagnostic>
    getCodeAnalyzerDiagnostics() const;

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
    QtTextIndent* textIndent;
    QCompleter* completer;
    QtBreakpointArea* breakpointArea;
    QtCodeAnalyzerMarkerArea* codeAnalyzerMarkerArea;
    Evaluator* nlsEvaluator;
};
//=============================================================================
