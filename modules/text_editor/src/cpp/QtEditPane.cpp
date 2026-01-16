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
#include "QtHighlighter.h"
#include "QtLineNumber.h"
#include "QtTextIndent.h"
#include <QtWidgets/QHBoxLayout>
//=============================================================================
QtEditPane::QtEditPane(Evaluator* eval) : currentEncoding("UTF-8"), nlsEvaluator(eval)
{
    textEditor = new QtTextEdit();
    completer = new QCompleter(this);
    textEditor->setCompleter(completer);

    breakpointArea = new QtBreakpointArea(textEditor, nlsEvaluator);
    QtLineNumber* tLN = new QtLineNumber(textEditor);

    QHBoxLayout* layout = new QHBoxLayout;
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);
    layout->addWidget(breakpointArea);
    layout->addWidget(tLN);
    layout->addWidget(textEditor);
    setLayout(layout);

    QtTextIndent* ind = new QtTextIndent();
    connect(textEditor, SIGNAL(indent()), ind, SLOT(update()));
    highlight = new Highlighter(textEditor->document());
    ind->setDocument(textEditor);
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
    if (filename.endsWith(".m")) {
        highlight->setEnable(true);
    } else {
        highlight->setEnable(false);
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
QtBreakpointArea*
QtEditPane::getBreakpointArea()
{
    return breakpointArea;
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
