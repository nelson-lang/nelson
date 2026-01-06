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
#include <QtCore/QMimeData>
#include <QtWidgets/QTextEdit>
//=============================================================================
class QCompleter;
class QAbstractItemModel;
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

    // Debug line highlighting
    void
    setDebugLine(int line);
    void
    clearDebugLine();
    int
    getDebugLine() const;
    void
    setDebugLineColor(const QColor& color);

private:
    QCompleter* qCompleter;
    int currentDebugLine = -1;
    QColor debugLineColor = QColor(255, 255, 0, 128); // Default semi-transparent yellow

    void
    updateDebugLineHighlight();

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
    wheelEvent(QWheelEvent* wheelEvent);

signals:
    void
    indent();
};
//=============================================================================
