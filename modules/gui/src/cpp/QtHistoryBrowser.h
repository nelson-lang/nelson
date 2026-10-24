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
#include <string>
#include "nlsGui_exports.h"
#include <QtCore/QtGlobal>
#include <QtGui/QAction>
#include <QtWidgets/QListWidget>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QDockWidget>
//=============================================================================
class QtHistoryBrowser : public QDockWidget
{
    Q_OBJECT
public:
    QtHistoryBrowser(QWidget* parent);

    enum SYNC_DIRECTION
    {
        SYNC_HB_TO_HM,
        SYNC_HM_TO_HB
    };

    bool
    synchronizeHistoryManager(SYNC_DIRECTION direction);

    void
    restorePosition();
    void
    restoreVisibility();

    void
    savePositionAndVisibility();

private:
    QListWidget* m_listWidget;
    QLineEdit* m_searchLineEdit;
    QMenu* m_popup;
    QAction *m_execute, *m_clearall, *m_copy, *m_delete, *m_toEditor, *m_toCommandLine;

    QStatusBar* statusBar;

    void
    createContextMenu();

    void
    applySearchFilter();

    QStringList
    selectedCommands();

protected:
    void
    keyPressEvent(QKeyEvent* event) override;

    void
    contextMenuEvent(QContextMenuEvent* e) override;
    void
    closeEvent(QCloseEvent*) override;
public slots:
    void
    addCommand(const QStringList& lines);
    void
    addCommand(const QString& text);
    void
    doubleClicked(QListWidgetItem* item);
    void
    execute();
    void
    clearAll();
    void
    copy();
    void
    deleteItem();
    void
    toTextEditor();
    void
    toCommandLine();

signals:
    void
    sendCommands(const QStringList& t);
    void
    sendToCommandLine(const QStringList& t);
    void
    sendToTextEditor();
    void
    closeHistoryBrowser();
};
//=============================================================================
