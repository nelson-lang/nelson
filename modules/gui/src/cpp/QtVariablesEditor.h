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
#include <QtCore/QTimer>
#include <QtGui/QCloseEvent>
#include <QtWidgets/QDockWidget>

#include <QtWidgets/QTabWidget>
#include <QtWidgets/QTableWidget>
#include <QtCore/QMap>
//=============================================================================
using namespace Nelson;
//=============================================================================
class QtVariablesEditor : public QDockWidget
{
    Q_OBJECT

    Context* m_context;

public:
    QtVariablesEditor(QWidget* parent);
    ~QtVariablesEditor();

    void
    setContext(Context* context);
    void
    restorePosition();
    void
    restoreVisibility();
    void
    savePositionAndVisibility();

    void
    updateVariables();

    bool
    openVariable(const QString& variableName);

private:
    QTabWidget* m_tabWidget;
    QMap<QString, QWidget*> m_openedVariables;

    void
    setupUI();

    void
    closeTab(int index);

    void
    closeEvent(QCloseEvent* event);
};
//=============================================================================
