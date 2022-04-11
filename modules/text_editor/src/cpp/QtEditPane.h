//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "QtHighlighter.h"
#include "QtTextEdit.h"
#include <QtWidgets/QCompleter>
#include <QtWidgets/QWidget>
//=============================================================================
class QCompleter;
//=============================================================================
class QtEditPane : public QWidget
{
    Q_OBJECT
    QtTextEdit* textEditor;
    QString currentFilename;
    QString currentEncoding;

public:
    QtEditPane();
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

private:
    Highlighter* highlight;
    QCompleter* completer;
};
//=============================================================================
