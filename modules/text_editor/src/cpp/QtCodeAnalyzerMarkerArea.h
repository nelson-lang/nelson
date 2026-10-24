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
#include "CodeAnalyzer.hpp"
#include "QtTextEdit.h"
#include <QtWidgets/QWidget>
//=============================================================================
class QtCodeAnalyzerMarkerArea : public QWidget
{
    Q_OBJECT
public:
    explicit QtCodeAnalyzerMarkerArea(QtTextEdit* textEditor);
    void
    setDiagnostics(const std::vector<Nelson::CodeAnalyzerDiagnostic>& diagnostics);

signals:
    void
    issueMarkerClicked(int line);

protected:
    void
    paintEvent(QPaintEvent* event) override;
    void
    mousePressEvent(QMouseEvent* event) override;

private:
    QtTextEdit* tEditor = nullptr;
    std::vector<Nelson::CodeAnalyzerDiagnostic> diagnostics;
    int
    lineNumberAtPosition(int y) const;
    std::string
    severityForLine(int line) const;
};
//=============================================================================
