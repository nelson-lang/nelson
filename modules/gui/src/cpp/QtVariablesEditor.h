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
#include "nlsGui_exports.h"
#include "VariableTableModel.h"
#include "Evaluator.hpp"
#include "Context.hpp"
#include "ArrayOf.hpp"
#include <QtCore/QtGlobal>
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
#include <QtGui/QAction>
#else
#include <QtWidgets/QAction>
#endif
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QSpinBox>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QSplitter>
#include <QtWidgets/QTextEdit>
#include <QtWidgets/QMenuBar>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QToolBar>
#include <QtWidgets/QDockWidget>
#include <QtWidgets/QSizeGrip>
#include <QtWidgets/QTableView>
#include <QtWidgets/QTabWidget>
#include <QtWidgets/QTableWidget>
#include <QtGui/QCloseEvent>
#include <QtCore/QMap>
#include <QtCore/QTimer>
//=============================================================================
using namespace Nelson;
//=============================================================================
class QtVariablesEditor : public QDockWidget
{
    Q_OBJECT

    Evaluator* m_evaluator;

public:
    QtVariablesEditor(QWidget* parent);
    ~QtVariablesEditor();

    void
    setEvaluator(Evaluator* eval);
    void
    restorePosition();
    void
    savePosition();

    void
    updateVariables();

    bool
    openVariable(const QString& variableName);

public slots:
    void
    closeEvent(QCloseEvent* event);

private:
    QTabWidget* m_tabWidget;
    QMap<QString, QWidget*> m_openedVariables; // Stores QTableView* widgets
    QSizeGrip* m_sizeGrip;

    QAction* m_copyAction;
    QAction* m_pasteAction;
    QAction* m_refreshAction;
    QAction* m_createVariableAction;

    QToolBar* m_toolBar;

    void
    setupUI();

    void
    closeTab(int index);

    void
    createTableContextMenu(QTableView* tableView);

    void
    closeVariableTab(const QString& variableName);

    QString
    getVariableClassAsQString(const ArrayOf& variable);
    QString
    getVariableDimensionsQString(const ArrayOf& variable);

    void
    createTextEditContextMenu(QTextEdit* textEdit);
    QString
    getVariableContentAsString(const ArrayOf& variable);

    void
    refreshAllVariables();

    void
    refreshVariable(const QString& variableName);

    bool
    shouldDisplayAsText(const ArrayOf& value);

    bool
    shouldDisplayAsTable(const ArrayOf& value);

    void
    updateTabTitle(const QString& variableName, const Nelson::ArrayOf& value);

public slots:
    void
    handleDockLocationChanged(Qt::DockWidgetArea area);

private slots:
    void
    copySelectedCells();
    void
    pasteDataFromClipboard();

    void
    refreshCurrentVariable();

    void
    createVariableFromSelection();

signals:
    void
    closeVariablesEditor();
};
//=============================================================================
