//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtEditPane.h"
#include "QtBreakpointArea.h"
#include "QtCodeAnalyzerMarkerArea.h"
#include "QtHighlighter.h"
#include "QtLineNumber.h"
#include "QtTextIndent.h"
#include "TextEditorIndentPreferences.h"
#include <QtWidgets/QHBoxLayout>
//=============================================================================
QtEditPane::QtEditPane(Evaluator* eval)
    : currentEncoding("UTF-8"), currentEol(QtEditPane::defaultEol()), nlsEvaluator(eval)
{
    textEditor = new QtTextEdit();
    completer = new QCompleter(this);
    textEditor->setCompleter(completer);

    breakpointArea = new QtBreakpointArea(textEditor, nlsEvaluator);
    QtLineNumber* tLN = new QtLineNumber(textEditor);
    codeAnalyzerMarkerArea = new QtCodeAnalyzerMarkerArea(textEditor);

    QHBoxLayout* layout = new QHBoxLayout;
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);
    layout->addWidget(breakpointArea);
    layout->addWidget(tLN);
    layout->addWidget(codeAnalyzerMarkerArea);
    layout->addWidget(textEditor);
    setLayout(layout);

    textIndent = new QtTextIndent(this);
    textIndent->setIndentSize(TextEditorGetIndentSize());
    connect(textEditor, SIGNAL(indent()), textIndent, SLOT(update()));
    highlight = new Highlighter(textEditor->document());
    textIndent->setDocument(textEditor);
}
//=============================================================================
QtEditPane::~QtEditPane()
{
    if (highlight) {
        delete highlight;
        highlight = nullptr;
    }
}
//=============================================================================
QtTextEdit*
QtEditPane::getEditor()
{
    return textEditor;
}
//=============================================================================
void
QtEditPane::setFileName(const QString& filename)
{
    currentFilename = filename;
    // an unnamed tab (new file) is also highlighted as a Nelson script
    bool isNelsonScript = filename.isEmpty() || filename.endsWith(".m", Qt::CaseInsensitive);
    if (highlight && (highlight->getEnable() != isNelsonScript)) {
        highlight->setEnable(isNelsonScript);
        highlight->rehighlight();
    }
    // automatic indentation only applies to Nelson scripts
    if (textIndent) {
        textIndent->setEnabled(isNelsonScript);
    }
    if (breakpointArea) {
        breakpointArea->setFilename(filename);
    }
}
//=============================================================================
QString
QtEditPane::getFileName()
{
    return currentFilename;
}
//=============================================================================
void
QtEditPane::setEncoding(const QString& encoding)
{
    currentEncoding = encoding;
}
//=============================================================================
QString
QtEditPane::getEncoding()
{
    return currentEncoding;
}
//=============================================================================
QString
QtEditPane::defaultEol()
{
#if defined(_WIN32)
    return QString(EOL_STYLE_CRLF);
#else
    return QString(EOL_STYLE_LF);
#endif
}
//=============================================================================
QString
QtEditPane::eolSequence(const QString& eol)
{
    if (eol == EOL_STYLE_CRLF) {
        return QString("\r\n");
    }
    if (eol == EOL_STYLE_CR) {
        return QString("\r");
    }
    return QString("\n");
}
//=============================================================================
void
QtEditPane::setEol(const QString& eol)
{
    if ((eol == EOL_STYLE_CRLF) || (eol == EOL_STYLE_LF) || (eol == EOL_STYLE_CR)) {
        currentEol = eol;
    }
}
//=============================================================================
QString
QtEditPane::getEol()
{
    if (currentEol.isEmpty()) {
        currentEol = QtEditPane::defaultEol();
    }
    return currentEol;
}
//=============================================================================
QtBreakpointArea*
QtEditPane::getBreakpointArea()
{
    return breakpointArea;
}
//=============================================================================
QtCodeAnalyzerMarkerArea*
QtEditPane::getCodeAnalyzerMarkerArea()
{
    return codeAnalyzerMarkerArea;
}
//=============================================================================
void
QtEditPane::setCodeAnalyzerDiagnostics(
    const std::vector<Nelson::CodeAnalyzerDiagnostic>& diagnostics)
{
    if (textEditor) {
        textEditor->setCodeAnalyzerDiagnostics(diagnostics);
    }
    if (codeAnalyzerMarkerArea) {
        codeAnalyzerMarkerArea->setDiagnostics(diagnostics);
    }
}
//=============================================================================
std::vector<Nelson::CodeAnalyzerDiagnostic>
QtEditPane::getCodeAnalyzerDiagnostics() const
{
    if (textEditor) {
        return textEditor->getCodeAnalyzerDiagnostics();
    }
    return {};
}
//=============================================================================
void
QtEditPane::refreshBreakpoints()
{
    if (breakpointArea) {
        breakpointArea->refreshBreakpoints();
    }
}
//=============================================================================
void
QtEditPane::setDebugLine(int line)
{
    if (textEditor) {
        textEditor->setDebugLine(line);
    }
}
//=============================================================================
void
QtEditPane::clearDebugLine()
{
    if (textEditor) {
        textEditor->clearDebugLine();
    }
}
//=============================================================================
int
QtEditPane::getDebugLine() const
{
    if (textEditor) {
        return textEditor->getDebugLine();
    }
    return -1;
}
//=============================================================================
void
QtEditPane::setExecutionLine(int line)
{
    if (breakpointArea) {
        breakpointArea->setExecutionLine(line);
    }
}
//=============================================================================
void
QtEditPane::clearExecutionLine()
{
    if (breakpointArea) {
        breakpointArea->clearExecutionLine();
    }
}
//=============================================================================
int
QtEditPane::getExecutionLine() const
{
    if (breakpointArea) {
        return breakpointArea->getExecutionLine();
    }
    return -1;
}
//=============================================================================
