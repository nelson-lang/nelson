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
#include <QtCore/QtGlobal>
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
#include <QtGui/QAction>
#else
#include <QtWidgets/QAction>
#endif
#include <QtGui/QContextMenuEvent>
#include <QtWidgets/QTableWidget>
#include <QtWidgets/QDockWidget>
#include <QtWidgets/QSizeGrip>
//=============================================================================
using namespace Nelson;
//=============================================================================
class QtWorkspaceBrowser : public QDockWidget
{
    Q_OBJECT
    QTableWidget* m_tableWidget;
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

    QSizeGrip* sizeGrip;

protected:
    void
    keyPressEvent(QKeyEvent* event) override;

    void
    contextMenuEvent(QContextMenuEvent* e);

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
    handleDockLocationChanged(Qt::DockWidgetArea area);

    void
    closeEvent(QCloseEvent*);

public:
    QtWorkspaceBrowser(QWidget* parent);

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
