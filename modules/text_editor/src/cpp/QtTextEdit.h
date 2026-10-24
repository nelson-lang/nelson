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
#include "Types.hpp"
#include "CodeAnalyzer.hpp"
#include <QtCore/QList>
#include <QtCore/QMimeData>
#include <QtCore/QPair>
#include <QtWidgets/QTextEdit>
//=============================================================================
class QCompleter;
class QAbstractItemModel;
class QMouseEvent;
//=============================================================================
using namespace Nelson;
//=============================================================================
class QtTextEdit : public QTextEdit
{
    Q_OBJECT
public:
    QtTextEdit();
    ~QtTextEdit() override;
    void
    keyPressEvent(QKeyEvent* event) override;
    void
    contextMenuEvent(QContextMenuEvent* event) override;
    void
    focusInEvent(QFocusEvent* e) override;
    void
    setCompleter(QCompleter* completer);
    void
    comment();
    void
    uncomment();
    bool
    canInsertFromMimeData(const QMimeData* source) const override;
    void
    insertFromMimeData(const QMimeData* source) override;

    void
    setDebugLine(int line);
    void
    clearDebugLine();
    int
    getDebugLine() const;
    void
    setDebugLineColor(const QColor& color);
    void
    setCodeAnalyzerDiagnostics(const std::vector<Nelson::CodeAnalyzerDiagnostic>& diagnostics);
    std::vector<Nelson::CodeAnalyzerDiagnostic>
    getCodeAnalyzerDiagnostics() const;

    // Find/Replace bar highlighting: ranges are (absolute position, length).
    // They are merged with the debug line and the code analyzer diagnostics
    // by updateExtraSelections().
    void
    setSearchHighlights(const QList<QPair<int, int>>& ranges, int currentIndex);
    void
    clearSearchHighlights();

private:
    QCompleter* qCompleter;
    int currentDebugLine = -1;
    QColor debugLineColor = QColor(255, 255, 0, 128);
    std::vector<Nelson::CodeAnalyzerDiagnostic> codeAnalyzerDiagnostics;
    QList<QPair<int, int>> searchHighlights;
    int currentSearchHighlight = -1;

    [[nodiscard]] bool
    isAutoIndentTrigger(QKeyEvent* event) const;

    void
    updateExtraSelections();
    void
    mouseMoveEvent(QMouseEvent* event) override;

    [[nodiscard]] QString
    textUnderCursor() const;
    QAbstractItemModel*
    modelFromNelson(const wstringVector& filesList, const wstringVector& builtinList,
        const wstringVector& macroList, const wstringVector& variableList,
        const wstringVector& fieldList, const wstringVector& propertyList,
        const wstringVector& methodList);
    void
    updateModel(const std::wstring& prefix, const wstringVector& filesList,
        const wstringVector& builtinList, const wstringVector& macroList,
        const wstringVector& variableList, const wstringVector& fieldList,
        const wstringVector& propertyList, const wstringVector& methodList);
    void
    complete(QString prefix);

private slots:
    void
    insertCompletion(const QString& completion);

    void
    wheelEvent(QWheelEvent* wheelEvent) override;

signals:
    void
    indent();
};
//=============================================================================
