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
#include "nlsGui_exports.h"
#include "Context.hpp"
#include <QtCore/QtGlobal>
#include <QtGui/QAction>
#include <QtGui/QContextMenuEvent>
#include <QtWidgets/QTableView>
#include <QtWidgets/QDockWidget>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QVBoxLayout>
#include <QtGui/QStandardItemModel>
#include <QtCore/QSortFilterProxyModel>
//=============================================================================
using namespace Nelson;
//=============================================================================
// role holding a numeric sort key, used to sort numeric columns numerically
// instead of lexicographically ("10x10" must come after "9x9").
#define WORKSPACE_SORT_ROLE (Qt::UserRole + 1)
//=============================================================================
class WorkspaceSortFilterProxyModel : public QSortFilterProxyModel
{
    Q_OBJECT

public:
    WorkspaceSortFilterProxyModel(QObject* parent = nullptr) : QSortFilterProxyModel(parent) { }

protected:
    bool
    lessThan(const QModelIndex& left, const QModelIndex& right) const override;
};
//=============================================================================
class QtWorkspaceBrowser : public QDockWidget
{
    Q_OBJECT
    QTableView* m_tableView;
    QStandardItemModel* m_model;
    WorkspaceSortFilterProxyModel* m_proxyModel;
    QLineEdit* m_filterLineEdit;
    Context* m_context;

private:
    QString
    getPlotCommand(const QString& plotType, const QString& variableName);
    QString
    getVariableAsQString(const ArrayOf* variable);
    template <typename T>
    QString
    handleNumericVariable(const ArrayOf* variable) const;
    template <typename T>
    QString
    handleComplexVariable(const ArrayOf* variable) const;
    QString
    handleLogicalVariable(const ArrayOf* variable) const;
    QString
    handleCharVariable(const ArrayOf* variable) const;
    QString
    handleCellArray(const ArrayOf* variable) const;
    QString
    handleStringArray(const ArrayOf* variable) const;
    QString
    handleFunctionHandle(const ArrayOf* variable) const;
    QString
    handleDefaultCase(const ArrayOf* variable) const;
    QString
    getCurrentVariableNameSelected();
    QStringList
    getSelectedVariableNames();
    bool
    isGlobalVariable(const QString& name);

    void
    setupModel();
    void
    setupHeaders();
    void
    setupFilterBar(QVBoxLayout* mainLayout);
    void
    applyVariableFilter();
    void
    restoreSelectionAndScroll(const QStringList& selectedNames, int scrollPosition);

    enum ColumnIndex
    {
        NAME_COLUMN = 0,
        VALUE_COLUMN = 1,
        CLASS_COLUMN = 2,
        SIZE_COLUMN = 3,
        BYTES_COLUMN = 4,
        SCOPE_COLUMN = 5,
        COLUMN_COUNT = 6
    };

protected:
    void
    keyPressEvent(QKeyEvent* event) override;

    void
    contextMenuEvent(QContextMenuEvent* e) override;

    void
    onDeleteAction();
    void
    onRenameAction();
    void
    onSaveAsAction();

    void
    onPlotAction(const QString& plotCommand);

public slots:
    void
    doubleClicked(const QModelIndex& index);
    void
    showHeaderContextMenu(const QPoint& pos);

    void
    updateVariables();

    void
    closeEvent(QCloseEvent*) override;

public:
    QtWorkspaceBrowser(QWidget* parent);
    ~QtWorkspaceBrowser();

    void
    restorePosition();
    void
    restoreVisibility();
    void
    savePositionAndVisibility();

    void
    setContext(Context* context);

signals:
    void
    postCommand(const QString& t);
    void
    sendCommands(const QStringList& t);

    void
    closeWorkspaceBrowser();
};
//=============================================================================
