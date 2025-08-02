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
#include "QtHighlighter.h"
#include "QtLineNumber.h"
#include "QtTextIndent.h"
#include <QtWidgets/QHBoxLayout>
//=============================================================================
QtEditPane::QtEditPane() : currentEncoding("UTF-8")
{
    textEditor = new QtTextEdit();
    completer = new QCompleter(this);
    textEditor->setCompleter(completer);
    QtLineNumber* tLN = new QtLineNumber(textEditor);
    QHBoxLayout* layout = new QHBoxLayout;
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
