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
#include <string>
#include "nlsGui_exports.h"
#include <QtCore/QtGlobal>
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
#include <QtGui/QAction>
#else
#include <QtWidgets/QAction>
#endif
#include <QtWidgets/QListWidget>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QDockWidget>
#include <QtWidgets/QSizeGrip>
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
    QMenu* m_popup;
    QAction *m_execute, *m_clearall, *m_copy, *m_delete, *m_toEditor;

    QStatusBar* statusBar;
    QSizeGrip* sizeGrip;

    void
    createContextMenu();

protected:
    void
    keyPressEvent(QKeyEvent* event) override;

    void
    contextMenuEvent(QContextMenuEvent* e);
    void
    closeEvent(QCloseEvent*);
public slots:
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
    handleDockLocationChanged(Qt::DockWidgetArea area);

signals:
    void
    sendCommands(const QStringList& t);
    void
    sendToTextEditor();
    void
    closeHistoryBrowser();
};
//=============================================================================
