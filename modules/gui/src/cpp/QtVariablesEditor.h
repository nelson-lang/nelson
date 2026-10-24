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
#include <QtCore/QtGlobal>
#include <QtGui/QAction>
#include <QtWidgets/QDockWidget>
#include <QtWidgets/QTableView>
#include <QtWidgets/QTabWidget>
#include <QtWidgets/QTableWidget>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QToolBar>
#include <QtWidgets/QTextEdit>
#include <QtCore/QMap>
#include <QtGui/QCloseEvent>
#include "nlsGui_exports.h"
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
class QtVariablesEditor : public QDockWidget
{
    Q_OBJECT

public:
    explicit QtVariablesEditor(QWidget* parent = nullptr);
    ~QtVariablesEditor() override;

    void
    setEvaluator(Evaluator* eval);
    bool
    openVariable(const QString& variableName);
    bool
    isVariableExisting(const QString& variableName) const;
    bool
    isGraphicsObjectExisting(long long handle) const;
    void
    updateVariables();
    void
    refreshAllVariables();
    void
    restorePosition();
    void
    savePosition();

signals:
    void
    closeVariablesEditor();

protected:
    void
    closeEvent(QCloseEvent* event) override;

    bool
    eventFilter(QObject* obj, QEvent* event) override;

private slots:
    void
    refreshCurrentVariable();
    void
    createVariableFromSelection();
    void
    copySelectedCells();
    void
    pasteDataFromClipboard();
    void
    pasteExcelDataFromClipboard();
    void
    closeTab(int index);

    void
    insertRowAbove();
    void
    insertRowBelow();
    void
    insertColumnLeft();
    void
    insertColumnRight();
    void
    deleteRow();
    void
    deleteColumn();

    void
    onModelSelectionCreated(const QString& suggestedName, const ArrayOf& newArray);
    void
    onModelError(const QString& message);

    void
    replaceByEmpty();

    void
    undo();
    void
    redo();

    void
    updateEditActions();

private:
    void
    setupUI();
    void
    connectModelSignals(QObject* model);
    void
    showStatusMessage(const QString& message, bool isError = false);
    void
    createTableContextMenu(QTableView* tableView);
    void
    createTextEditContextMenu(QTextEdit* textEdit);

    void
    closeVariableTab(const QString& variableName);
    void
    updateTabTitle(const QString& variableName);
    void
    refreshVariable(const QString& variableName);
    QString
    getVariableContentAsString(const ArrayOf& variable);
    bool
    shouldDisplayAsText(const ArrayOf& value);
    bool
    shouldDisplayAsTable(const ArrayOf& value);

    QString
    preprocessExcelData(const QString& rawData);

    QTabWidget* m_tabWidget = nullptr;
    QToolBar* m_toolBar = nullptr;
    QStatusBar* m_statusBar = nullptr;

    QAction* m_undoAction = nullptr;
    QAction* m_redoAction = nullptr;

    QAction* m_refreshAction = nullptr;
    QAction* m_createVariableAction = nullptr;
    QAction* m_copyAction = nullptr;
    QAction* m_pasteAction = nullptr;
    QAction* m_pasteExcelDataAction = nullptr;
    QAction* m_insertRowAboveAction = nullptr;
    QAction* m_insertRowBelowAction = nullptr;
    QAction* m_insertColLeftAction = nullptr;
    QAction* m_insertColRightAction = nullptr;
    QAction* m_deleteRowAction = nullptr;
    QAction* m_deleteColAction = nullptr;
    QAction* m_replaceByEmptyAction = nullptr;

    Evaluator* m_evaluator = nullptr;
    QHash<QString, QWidget*> m_openedVariables;
};
//=============================================================================
