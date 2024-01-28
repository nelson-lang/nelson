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
#include <QtCore/QtGlobal>
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
#include <QtGui/QAction>
#include <QtGui/QFileSystemModel>
#else
#include <QtWidgets/QAction>
#include <QtWidgets/QFileSystemModel>
#endif
#include <QtWidgets/QTreeView>
#include <QtGui/QContextMenuEvent>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QCompleter>
#include <QtCore/QVariant>
#include <QtWidgets/QDockWidget>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QSizeGrip>

//=============================================================================
class CustomFileSystemModel : public QFileSystemModel
{
    Q_OBJECT

public:
    CustomFileSystemModel(QObject* parent = nullptr) : QFileSystemModel(parent) { }

    QVariant
    data(const QModelIndex& index, int role = Qt::DisplayRole) const override;

    bool
    isEnabled()
    {
        return _isOn;
    }

    void
    setEnabled(bool isOn)
    {
        _isOn = isOn;
    }

private:
    bool _isOn = true;
};
//=============================================================================
class QtFileBrowser : public QDockWidget
{
    Q_OBJECT
    CustomFileSystemModel* model;
    QFileSystemModel* modelBar;
    QTreeView* tree;
    QLineEdit* pathLineEdit;
    QCompleter* completer;
    QSizeGrip* sizeGrip;

private:
    bool
    isAnyItemExpanded(QTreeView* treeView);
    bool
    isDescendantExpanded(const QModelIndex& parentIndex, QTreeView* treeView);

protected:
    void
    keyPressEvent(QKeyEvent* event) override;

    void
    contextMenuEvent(QContextMenuEvent* e);

public slots:
    void
    doubleClicked(const QModelIndex& index);
    void
    showHeaderContextMenu(const QPoint& pos);

    void
    updateCurrentPath();

    void
    deleteSelected();
    void
    editSelectedFile();
    void
    runSelectedFile();
    void
    renameSelectedFile();

    void
    collapseAll();

    void
    handleReturnPressed();

    void
    showInExplorer();

    void
    createNewDirectory();
    void
    createNewFileScript();
    void
    createNewFileFunction();

    void
    addToPath();
    void
    removeToPath();

    void
    handleDockLocationChanged(Qt::DockWidgetArea area);

    void
    closeEvent(QCloseEvent* event);

public:
    QtFileBrowser(QWidget* parent);

    void
    restoreVisibility();

    void
    restorePosition();

    void
    savePositionAndVisibility();

private:
    void
    saveHeaderSettings();

    void
    loadHeaderSettings();

    void
    openWithDefaultApplication();

    void
    createTopBar(QVBoxLayout* mainlayout);

    void
    updatePathFromLineEdit();

    void
    indicatesNotInPath();

signals:
    void
    postCommand(const QString& t);
    void
    closeFileBrowser();
};
//=============================================================================
