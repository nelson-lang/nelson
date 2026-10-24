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
#include <QtCore/QtGlobal>
#include <QtGui/QAction>
#include <QtGui/QFileSystemModel>
#include <QtWidgets/QTreeView>
#include <QtGui/QContextMenuEvent>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QCompleter>
#include <QtCore/QVariant>
#include <QtWidgets/QDockWidget>
#include <QtWidgets/QVBoxLayout>
#include "QtFileBrowserIconProvider.h"
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
    // Shared by both models; they do not own their icon provider.
    QtFileBrowserIconProvider iconProvider;
    CustomFileSystemModel* model;
    QFileSystemModel* modelBar;
    QTreeView* tree;
    QLineEdit* pathLineEdit;
    QLineEdit* filterLineEdit;
    QCompleter* completer;
    QString lastKnownPath;
    bool showHiddenFiles = false;

private:
    bool
    isAnyItemExpanded(QTreeView* treeView);
    bool
    isDescendantExpanded(const QModelIndex& parentIndex, QTreeView* treeView);

protected:
    void
    keyPressEvent(QKeyEvent* event) override;

    void
    contextMenuEvent(QContextMenuEvent* e) override;

public slots:
    void
    doubleClicked(const QModelIndex& index);
    void
    showHeaderContextMenu(const QPoint& pos);

    void
    updateCurrentPath();

    void
    refreshView();

    void
    copyFullPath();

    void
    setAsCurrentFolder();

    void
    toggleShowHiddenFiles();

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
    closeEvent(QCloseEvent* event) override;

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

    void
    applyPath(const QString& path);

    void
    updateModelFilter();

    void
    applyNameFilter();

    void
    saveBrowserSettings();

    void
    loadBrowserSettings();

signals:
    void
    postCommand(const QString& t);
    void
    closeFileBrowser();
};
//=============================================================================
