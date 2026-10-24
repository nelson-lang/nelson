//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtFileBrowser.h"
#include "QtDockTitleBar.h"
#include "QtTranslation.hpp"
#include "Nelson_VERSION.h"
#include "NelsonSettingsScope.hpp"
#include "HistoryBrowser.hpp"
#include "ForceWindowsTitleBarToDark.hpp"
#include "QStringConverter.hpp"
#include "ErrorID.hpp"
#include "NelsonConfiguration.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "IsValidVariableName.hpp"
#include <QtCore/QSettings>
#include <QtCore/QTextStream>
#include <QtCore/QRegularExpression>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QMenu>
#include <QtWidgets/QApplication>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QInputDialog>
#include <QtWidgets/QMessageBox>
#include <QtGui/QDesktopServices>
#include <QtGui/QStandardItemModel>
#include <QtGui/QClipboard>
#include <QtGui/QPalette>
//=============================================================================
const QString SETTING_FILE_BROWSER_VISIBILITY = "fb_Visibility";
const QString SETTING_FILE_BROWSER_GEOMETRY = "fb_Geometry";
const QString SETTING_FILE_BROWSER_HEADER_SIZE = "fb_HeaderSectionSize_%1";
const QString SETTING_FILE_BROWSER_HEADER_SECTION = "fb_HeaderSection_%1";
const QString SETTING_FILE_BROWSER_SHOW_HIDDEN = "fb_ShowHiddenFiles";
const QString SETTING_FILE_BROWSER_NAME_FILTER = "fb_NameFilter";
//=============================================================================
using namespace Nelson;
//=============================================================================
QVariant
CustomFileSystemModel::data(const QModelIndex& index, int role) const
{
    if (role == Qt::ForegroundRole && this->_isOn && QFileSystemModel::isDir(index)) {
        // This runs for every painted cell, so it must stay cheap: isDir() reuses the
        // model's cached node type (no filesystem stat), and isAvailablePath() tests
        // membership without copying the whole path vector.
        const std::wstring wFilePath = QStringTowstring(QFileSystemModel::filePath(index));
        if (PathFunctionIndexerManager::getInstance()->isAvailablePath(wFilePath)) {
            // directory available in path: keep the theme default color
            return QVariant();
        }
        // directory not in path: dimmed color derived from the current palette
        // so that it stays readable with both light and dark themes.
        const QPalette palette = QApplication::palette();
        QColor dimmed = palette.color(QPalette::Disabled, QPalette::WindowText);
        if (!dimmed.isValid()) {
            dimmed = palette.color(QPalette::WindowText);
        }
        return QVariant::fromValue(dimmed);
    }

    return QFileSystemModel::data(index, role);
}
//=============================================================================
QtFileBrowser::QtFileBrowser(QWidget* parent) : QDockWidget(TR("File Browser"), parent)
{
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(this->winId());
#endif
    model = new CustomFileSystemModel(this);
    modelBar = new QFileSystemModel(this);
    // Both models are asked for icons and types on the GUI thread for every
    // entry of a changed directory: answer from the suffix, never the MIME
    // database or the shell (see QtFileBrowserIconProvider).
    model->setIconProvider(&iconProvider);
    modelBar->setIconProvider(&iconProvider);
    model->setOption(QFileSystemModel::DontUseCustomDirectoryIcons);
    modelBar->setOption(QFileSystemModel::DontUseCustomDirectoryIcons);

    model->setRootPath(QDir::currentPath());
    lastKnownPath = QDir::currentPath();
    updateModelFilter();
    model->setNameFilterDisables(false);

    modelBar->setRootPath(QDir::currentPath());
    modelBar->setFilter(QDir::AllDirs | QDir::NoDotAndDotDot | QDir::Drives);

    tree = new QTreeView;
    tree->setRootIndex(model->index(QDir::currentPath()));
    tree->setSelectionMode(QAbstractItemView::SingleSelection);
    // Every row in a file listing has the same height: telling the view so lets it
    // skip per-row size measurement, which keeps painting responsive when a directory
    // holds a very large number of entries.
    tree->setUniformRowHeights(true);
    tree->setSortingEnabled(true);
    QHeaderView* header = tree->header();
    header->resizeSection(0, 64);

    header->setContextMenuPolicy(Qt::CustomContextMenu);

    QVBoxLayout* mainLayout = new QVBoxLayout;
    createTopBar(mainLayout);
    loadBrowserSettings();
    mainLayout->addWidget(tree);

    setLayout(mainLayout);

    setFocusPolicy(Qt::StrongFocus);
    tree->setModel(model);
    connect(tree, SIGNAL(doubleClicked(const QModelIndex&)), this,
        SLOT(doubleClicked(const QModelIndex&)));
    setObjectName("fileBrowser");

    QWidget* centralWidget = new QWidget(this);
    centralWidget->setLayout(mainLayout);
    setWidget(centralWidget);

    connect(header, SIGNAL(customContextMenuRequested(const QPoint&)), this,
        SLOT(showHeaderContextMenu(const QPoint&)));

    setMinimumSize(50, 50);
    setTitleBarWidget(new QtDockTitleBar(this, "fb"));
}
//=============================================================================
void
QtFileBrowser::createTopBar(QVBoxLayout* mainlayout)
{

    pathLineEdit = new QLineEdit(this);
    pathLineEdit->setClearButtonEnabled(true);

    QString folderIconName = Nelson::wstringToQString(
        Nelson::NelsonConfiguration::getInstance()->getNelsonRootDirectory()
        + L"/resources/folder.svg");

    pathLineEdit->addAction(QPixmap(folderIconName), QLineEdit::LeadingPosition);

    completer = new QCompleter(this);
    completer->setCaseSensitivity(Qt::CaseInsensitive);
    pathLineEdit->setCompleter(completer);

    completer->setModel(modelBar);

    connect(pathLineEdit, &QLineEdit::returnPressed, this, &QtFileBrowser::updatePathFromLineEdit);

    mainlayout->addWidget(pathLineEdit);

    filterLineEdit = new QLineEdit(this);
    filterLineEdit->setClearButtonEnabled(true);
    filterLineEdit->setPlaceholderText(TR("Filter (e.g. *.m;*.nh5)"));
    filterLineEdit->setToolTip(TR("Filter displayed files, patterns separated by ';'"));
    connect(filterLineEdit, &QLineEdit::textChanged, this, &QtFileBrowser::applyNameFilter);

    mainlayout->addWidget(filterLineEdit);
}
//=============================================================================
void
QtFileBrowser::updateModelFilter()
{
    QDir::Filters filters = QDir::AllEntries | QDir::NoDot | QDir::Drives;
    if (showHiddenFiles) {
        filters |= QDir::Hidden;
    }
    model->setFilter(filters);
}
//=============================================================================
void
QtFileBrowser::applyNameFilter()
{
    QStringList patterns;
    const QString text = filterLineEdit->text().trimmed();
    if (!text.isEmpty()) {
        const QStringList splitted = text.split(QRegularExpression("[;,\\s]+"), Qt::SkipEmptyParts);
        for (const QString& pattern : splitted) {
            patterns << pattern;
        }
    }
    model->setNameFilterDisables(false);
    model->setNameFilters(patterns);
}
//=============================================================================
void
QtFileBrowser::toggleShowHiddenFiles()
{
    showHiddenFiles = !showHiddenFiles;
    updateModelFilter();
    saveBrowserSettings();
}
//=============================================================================
void
QtFileBrowser::saveBrowserSettings()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    settings.setValue(SETTING_FILE_BROWSER_SHOW_HIDDEN, showHiddenFiles);
    if (filterLineEdit) {
        settings.setValue(SETTING_FILE_BROWSER_NAME_FILTER, filterLineEdit->text());
    }
}
//=============================================================================
void
QtFileBrowser::loadBrowserSettings()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    showHiddenFiles = settings.value(SETTING_FILE_BROWSER_SHOW_HIDDEN, false).toBool();
    updateModelFilter();
    if (filterLineEdit) {
        filterLineEdit->setText(settings.value(SETTING_FILE_BROWSER_NAME_FILTER, "").toString());
        applyNameFilter();
    }
}
//=============================================================================
void
QtFileBrowser::updatePathFromLineEdit()
{
    QString newPath = pathLineEdit->text();
    if (QDir(newPath).exists()) {
        model->setRootPath(newPath);
        modelBar->setRootPath(newPath);
        tree->setRootIndex(model->index(newPath));
        if (QFileInfo(newPath).isDir()) {
            emit postCommand("cd('" + newPath + "')");
        }
    }
}
//=============================================================================
void
QtFileBrowser::restorePosition()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    restoreGeometry(settings.value(SETTING_FILE_BROWSER_GEOMETRY).toByteArray());
    loadHeaderSettings();
}
//=============================================================================
void
QtFileBrowser::restoreVisibility()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    bool savedVisibility = settings.value(SETTING_FILE_BROWSER_VISIBILITY, true).toBool();
    setVisible(savedVisibility);
}
//=============================================================================
void
QtFileBrowser::savePositionAndVisibility()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    settings.setValue(SETTING_FILE_BROWSER_VISIBILITY, isVisible());
    settings.setValue(SETTING_FILE_BROWSER_GEOMETRY, saveGeometry());

    saveHeaderSettings();
    saveBrowserSettings();
}
//=============================================================================
void
QtFileBrowser::doubleClicked(const QModelIndex& index)
{
    QString newPath = model->filePath(index);
    if (QFileInfo(newPath).isDir()) {
        emit postCommand("cd('" + newPath + "')");
        if (newPath == QDir::rootPath()) {
            tree->setRootIndex(model->index(newPath));
            model->setRootPath(newPath);
            pathLineEdit->setText(newPath);
            modelBar->setRootPath(newPath);
        } else {
            tree->setRootIndex(model->index(newPath));
            model->setRootPath(newPath);

            pathLineEdit->setText(newPath);
            modelBar->setRootPath(newPath);

            tree->selectionModel()->setCurrentIndex(
                model->index(newPath), QItemSelectionModel::ClearAndSelect);
        }
    }
    if (QFileInfo(newPath).isFile()) {
        if (QFileInfo(newPath).suffix() == "m") {
            emit postCommand("edit('" + newPath + "')");
        } else {
            QDesktopServices::openUrl(QUrl::fromLocalFile(newPath));
        }
    }
}
//=============================================================================
bool
QtFileBrowser::isAnyItemExpanded(QTreeView* treeView)
{
    QAbstractItemModel* model = treeView->model();

    for (int row = 0; row < model->rowCount(); ++row) {
        QModelIndex topLevelIndex = model->index(row, 0);
        if (treeView->isExpanded(topLevelIndex)) {
            return true;
        }
        if (isDescendantExpanded(topLevelIndex, treeView)) {
            return true;
        }
    }

    return false;
}
//=============================================================================
bool
QtFileBrowser::isDescendantExpanded(const QModelIndex& parentIndex, QTreeView* treeView)
{
    QAbstractItemModel* model = treeView->model();
    int rowCount = model->rowCount(parentIndex);

    for (int row = 0; row < rowCount; ++row) {
        QModelIndex childIndex = model->index(row, 0, parentIndex);
        if (treeView->isExpanded(childIndex)) {
            return true;
        }
        if (isDescendantExpanded(childIndex, treeView)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
void
QtFileBrowser::contextMenuEvent(QContextMenuEvent* event)
{
    QMenu contextMenu(this);

    QModelIndex qModelIndex = tree->currentIndex();
    QString currentPath = model->filePath(qModelIndex);
    QFileInfo fileInfo(currentPath);

    bool hasSelection = qModelIndex.isValid() && fileInfo.exists();
    bool isFile = (fileInfo.exists() && fileInfo.isFile());
    bool isDir = (fileInfo.exists() && fileInfo.isDir());
    QString fileNameIcon;
    if (!isFile) {
        fileNameIcon = Nelson::wstringToQString(
            Nelson::NelsonConfiguration::getInstance()->getNelsonRootDirectory()
            + L"/resources/mat.ico");

        QMenu* newMenu = contextMenu.addMenu(TR("New"));
        QAction* newFileAction = newMenu->addAction(TR("Script"));

        newFileAction->setIcon(QPixmap(fileNameIcon));

        connect(newFileAction, &QAction::triggered, this, &QtFileBrowser::createNewFileScript);
        QAction* newFunctionAction = newMenu->addAction(TR("Function"));
        newFunctionAction->setIcon(QPixmap(fileNameIcon));

        connect(
            newFunctionAction, &QAction::triggered, this, &QtFileBrowser::createNewFileFunction);
        QAction* newDirAction = newMenu->addAction(TR("Directory"));

        fileNameIcon = Nelson::wstringToQString(
            Nelson::NelsonConfiguration::getInstance()->getNelsonRootDirectory()
            + L"/resources/folder.svg");
        newDirAction->setIcon(QPixmap(fileNameIcon));
        connect(newDirAction, &QAction::triggered, this, &QtFileBrowser::createNewDirectory);
    }

    QAction* showInExplorerAction = new QAction(TR("Show in explorer"), this);
    connect(showInExplorerAction, &QAction::triggered, this, &QtFileBrowser::showInExplorer);
    contextMenu.addAction(showInExplorerAction);

    QAction* deleteAction = new QAction(TR("Delete"), this);
    fileNameIcon = Nelson::wstringToQString(
        Nelson::NelsonConfiguration::getInstance()->getNelsonRootDirectory()
        + L"/resources/user-trash.svg");
    deleteAction->setIcon(QPixmap(fileNameIcon));

    connect(deleteAction, &QAction::triggered, this, &QtFileBrowser::deleteSelected);
    contextMenu.addAction(deleteAction);
    deleteAction->setEnabled(hasSelection);

    QAction* editFileAction = new QAction(TR("Edit"), this);
    connect(editFileAction, &QAction::triggered, this, &QtFileBrowser::editSelectedFile);
    contextMenu.addAction(editFileAction);
    editFileAction->setEnabled(isFile);

    if (isFile && fileInfo.suffix() == "m"
        && IsValidVariableName(QStringTowstring(fileInfo.baseName()))) {
        QAction* runFileAction = new QAction(TR("Run"), this);
        connect(runFileAction, &QAction::triggered, this, &QtFileBrowser::runSelectedFile);
        contextMenu.addAction(runFileAction);
        runFileAction->setEnabled(isFile);
    }

    QAction* renameAction = new QAction(TR("Rename"), this);
    connect(renameAction, &QAction::triggered, this, &QtFileBrowser::renameSelectedFile);
    contextMenu.addAction(renameAction);
    renameAction->setEnabled(hasSelection);

    contextMenu.addSeparator();

    QAction* copyFullPathAction = new QAction(TR("Copy full path"), this);
    connect(copyFullPathAction, &QAction::triggered, this, &QtFileBrowser::copyFullPath);
    contextMenu.addAction(copyFullPathAction);
    copyFullPathAction->setEnabled(hasSelection);

    QAction* setAsCurrentFolderAction = new QAction(TR("Set as Current Folder"), this);
    connect(
        setAsCurrentFolderAction, &QAction::triggered, this, &QtFileBrowser::setAsCurrentFolder);
    contextMenu.addAction(setAsCurrentFolderAction);
    setAsCurrentFolderAction->setEnabled(hasSelection);

    contextMenu.addSeparator();

    QAction* refreshAction = new QAction(TR("Refresh"), this);
    refreshAction->setShortcut(QKeySequence(Qt::Key_F5));
    connect(refreshAction, &QAction::triggered, this, &QtFileBrowser::refreshView);

    contextMenu.addAction(refreshAction);

    QAction* openDefaultAction = new QAction(TR("Open with Default Application"), this);
    connect(
        openDefaultAction, &QAction::triggered, this, &QtFileBrowser::openWithDefaultApplication);
    contextMenu.addAction(openDefaultAction);

    contextMenu.addSeparator();

    QAction* indicateIsInPathAction = new QAction(TR("Indicate files not in path"), this);
    connect(indicateIsInPathAction, &QAction::triggered, this, &QtFileBrowser::indicatesNotInPath);
    contextMenu.addAction(indicateIsInPathAction);
    indicateIsInPathAction->setCheckable(true);
    indicateIsInPathAction->setChecked(model->isEnabled());

    QAction* showHiddenFilesAction = new QAction(TR("Show hidden files"), this);
    connect(
        showHiddenFilesAction, &QAction::triggered, this, &QtFileBrowser::toggleShowHiddenFiles);
    contextMenu.addAction(showHiddenFilesAction);
    showHiddenFilesAction->setCheckable(true);
    showHiddenFilesAction->setChecked(showHiddenFiles);

    if (isDir) {
        bool isInPath = PathFunctionIndexerManager::getInstance()->isAvailablePath(
            QStringTowstring(currentPath));
        if (!isInPath) {
            QAction* addToPathAction = new QAction(TR("Add to Path"), this);
            connect(addToPathAction, &QAction::triggered, this, &QtFileBrowser::addToPath);
            contextMenu.addAction(addToPathAction);
        } else {
            QAction* removeToPathAction = new QAction(TR("Remove to Path"), this);
            connect(removeToPathAction, &QAction::triggered, this, &QtFileBrowser::removeToPath);
            contextMenu.addAction(removeToPathAction);
        }
    }

    if (isAnyItemExpanded(tree)) {
        QAction* collapseAllAction = new QAction(TR("Collapse All"), this);
        connect(collapseAllAction, &QAction::triggered, this, &QtFileBrowser::collapseAll);
        contextMenu.addAction(collapseAllAction);
    }
    contextMenu.exec(event->globalPos());
}
//=============================================================================
void
QtFileBrowser::showHeaderContextMenu(const QPoint& pos)
{
    QHeaderView* header = tree->header();

    QMenu contextMenu(this);

    for (int i = 0; i < header->count(); ++i) {
        QAction* toggleAction
            = new QAction(header->model()->headerData(i, Qt::Horizontal).toString(), this);
        toggleAction->setCheckable(true);
        toggleAction->setChecked(!header->isSectionHidden(i));

        connect(toggleAction, &QAction::triggered,
            [=]() { header->setSectionHidden(i, !toggleAction->isChecked()); });

        contextMenu.addAction(toggleAction);
    }
    contextMenu.exec(header->mapToGlobal(pos));
}
//=============================================================================
void
QtFileBrowser::saveHeaderSettings()
{
    QHeaderView* header = tree->header();
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);

    for (int i = 0; i < header->count(); ++i) {
        settings.setValue(
            QString(SETTING_FILE_BROWSER_HEADER_SECTION).arg(i), !header->isSectionHidden(i));
        settings.setValue(QString(SETTING_FILE_BROWSER_HEADER_SIZE).arg(i), header->sectionSize(i));
    }
}
//=============================================================================
void
QtFileBrowser::loadHeaderSettings()
{
    QHeaderView* header = tree->header();
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);

    for (int i = 0; i < header->count(); ++i) {
        bool isVisible
            = settings.value(QString(SETTING_FILE_BROWSER_HEADER_SECTION).arg(i), true).toBool();
        header->setSectionHidden(i, !isVisible);

        int size = settings.value(QString(SETTING_FILE_BROWSER_HEADER_SIZE).arg(i), -1).toInt();
        if (size != -1) {
            header->resizeSection(i, size);
        }
    }
}
//=============================================================================
void
QtFileBrowser::applyPath(const QString& path)
{
    lastKnownPath = path;
    model->setRootPath(path);
    modelBar->setRootPath(path);
    tree->setRootIndex(model->index(path));
    pathLineEdit->setText(path);
}
//=============================================================================
void
QtFileBrowser::updateCurrentPath()
{
    // called on each prompt: do nothing when the current directory did not change,
    // to preserve the selection and the expanded state of the tree.
    const QString currentPath = QDir::currentPath();
    if (currentPath == lastKnownPath) {
        return;
    }
    applyPath(currentPath);
}
//=============================================================================
void
QtFileBrowser::refreshView()
{
    applyPath(QDir::currentPath());
}
//=============================================================================
void
QtFileBrowser::copyFullPath()
{
    QModelIndex currentIndex = tree->currentIndex();
    if (currentIndex.isValid()) {
        QApplication::clipboard()->setText(QDir::toNativeSeparators(model->filePath(currentIndex)));
    }
}
//=============================================================================
void
QtFileBrowser::setAsCurrentFolder()
{
    QModelIndex currentIndex = tree->currentIndex();
    if (!currentIndex.isValid()) {
        return;
    }
    QString selectedPath = model->filePath(currentIndex);
    QFileInfo fileInfo(selectedPath);
    if (fileInfo.isFile()) {
        selectedPath = fileInfo.absolutePath();
    }
    if (QDir(selectedPath).exists()) {
        emit postCommand("cd('" + selectedPath + "')");
    }
}
//=============================================================================
void
QtFileBrowser::deleteSelected()
{
    QModelIndex currentIndex = tree->currentIndex();
    if (!currentIndex.isValid()) {
        return;
    }
    QString currentPath = model->filePath(currentIndex);
    QFileInfo fileInfo(currentPath);
    if (!fileInfo.exists()) {
        return;
    }
    const bool isDir = fileInfo.isDir();

    QMessageBox msgBox(this);
    msgBox.setIcon(QMessageBox::Warning);
    msgBox.setWindowTitle(isDir ? TR("Delete Directory") : TR("Delete File"));
    msgBox.setText(isDir
            ? TR("Are you certain you wish to delete the directory '%1'?").arg(fileInfo.fileName())
            : TR("Are you certain you wish to delete the file '%1'?").arg(fileInfo.fileName()));
    msgBox.setInformativeText(isDir
            ? TR("The directory and all its content will be moved to the trash.")
            : TR("The file will be moved to the trash."));
    msgBox.setStandardButtons(QMessageBox::Yes | QMessageBox::No);
    msgBox.setDefaultButton(QMessageBox::No);
    if (msgBox.exec() != QMessageBox::Yes) {
        return;
    }

    bool removed = QFile::moveToTrash(currentPath);
    if (!removed) {
        // trash not available: fallback on a permanent removal
        if (isDir) {
            QDir qDir(currentPath);
            removed = qDir.removeRecursively();
        } else {
            QFile file(currentPath);
            removed = file.remove();
        }
    }
    if (!removed) {
        QMessageBox::warning(this, isDir ? TR("Delete Directory") : TR("Delete File"),
            wstringToQString(Nelson::resolveMessageTemplate(
                                 L"Nelson:gui:unableDeletePath", L"Unable to delete '%1'."))
                .arg(QDir::toNativeSeparators(currentPath)));
        return;
    }
    refreshView();
}
//=============================================================================
void
QtFileBrowser::runSelectedFile()
{
    QModelIndex qModelIndex = tree->currentIndex();
    QString currentPath = model->filePath(qModelIndex);
    QFileInfo fileInfo(currentPath);
    QString fileName = fileInfo.baseName();
    QString pathName = fileInfo.path();
    QString cmd = "cd('" + pathName + "');" + fileName;
    emit postCommand(cmd);
}
//=============================================================================
void
QtFileBrowser::editSelectedFile()
{
    QModelIndex qModelIndex = tree->currentIndex();
    QString currentPath = model->filePath(qModelIndex);
    QString cmd = "edit('" + currentPath + "');";
    emit postCommand(cmd);
}
//=============================================================================
static QString
getParentDirectory(const QString& path)
{
    QFileInfo fileInfo(path);
    QDir dir(fileInfo.absolutePath());
    return dir.absolutePath();
}
//=============================================================================
void
QtFileBrowser::renameSelectedFile()
{
    QModelIndex index = tree->currentIndex();
    if (index.isValid()) {
        QString currentPath = model->filePath(index);
        QFileInfo fileInfo(currentPath);
        QString currentFileName;
        bool isDir = false;
        if (fileInfo.isDir()) {
            QDir dir(currentPath);
            currentFileName = dir.dirName();
            isDir = true;
        } else if (fileInfo.isFile()) {
            currentFileName = fileInfo.fileName();
        } else {
            return;
        }

        bool ok;
        QString newFileName
            = QInputDialog::getText(this, isDir ? TR("Rename Directory") : TR("Rename File"),
                TR("New Name:"), QLineEdit::Normal, currentFileName, &ok);
        QString parentPath = getParentDirectory(currentPath);
        QString newPath = parentPath + QDir::separator() + newFileName;

        if (ok && !newFileName.isEmpty()) {

            bool renamed = false;
            if (isDir) {
                QDir dir(currentPath);
                renamed = dir.rename(currentPath, newPath);
            } else {
                renamed = QFile::rename(currentPath, newPath);
            }
            if (renamed) {
                refreshView();
            } else {
                QMessageBox::warning(this, isDir ? TR("Rename Directory") : TR("Rename File"),
                    wstringToQString(Nelson::resolveMessageTemplate(L"Nelson:gui:unableRenamePath",
                                         L"Unable to rename '%1' to '%2'."))
                        .arg(QDir::toNativeSeparators(currentPath),
                            QDir::toNativeSeparators(newPath)));
            }
        }
    }
}
//=============================================================================
void
QtFileBrowser::openWithDefaultApplication()
{
    QModelIndex index = tree->currentIndex();
    if (index.isValid()) {
        QString filePath = model->filePath(index);
        QDesktopServices::openUrl(QUrl::fromLocalFile(filePath));
    }
}
//=============================================================================
void
QtFileBrowser::indicatesNotInPath()
{
    model->setEnabled(!model->isEnabled());
}
//=============================================================================
void
QtFileBrowser::collapseAll()
{
    tree->collapseAll();
}
//=============================================================================
void
QtFileBrowser::keyPressEvent(QKeyEvent* event)
{
    if (event->key() == Qt::Key_Return || event->key() == Qt::Key_Enter) {
        handleReturnPressed();
    } else if (event->key() == Qt::Key_Delete) {
        deleteSelected();
    } else if (event->key() == Qt::Key_F5) {
        refreshView();
    }
    QWidget::keyPressEvent(event);
}
//=============================================================================
void
QtFileBrowser::handleReturnPressed()
{
    QModelIndex currentIndex = tree->currentIndex();
    if (currentIndex.isValid()) {
        doubleClicked(currentIndex);
    }
}
//=============================================================================
void
QtFileBrowser::showInExplorer()
{
    QModelIndex currentIndex = tree->currentIndex();
    if (currentIndex.isValid()) {
        QString filePath = model->filePath(currentIndex);
        if (QFileInfo(filePath).isDir()) {
            QDesktopServices::openUrl(QUrl::fromLocalFile(filePath));
        }
        if (QFileInfo(filePath).isFile()) {
            QString parentPath = QFileInfo(filePath).dir().absolutePath();
            QDesktopServices::openUrl(QUrl::fromLocalFile(parentPath));
        }
    }
}
//=============================================================================
void
QtFileBrowser::createNewDirectory()
{
    QModelIndex currentIndex = tree->currentIndex();
    if (currentIndex.isValid()) {
        QString currentPath = model->filePath(currentIndex);

        QString newDirName
            = QInputDialog::getText(this, TR("Create New Directory"), TR("New Directory Name:"));

        if (!newDirName.isEmpty()) {
            QString newDirPath = currentPath + QDir::separator() + newDirName;
            if (QDir(newDirPath).exists()) {
                QMessageBox::warning(this, TR("Create New Directory"),
                    wstringToQString(
                        Nelson::resolveMessageTemplate(L"Nelson:gui:directoryAlreadyExists",
                            L"The directory '%1' already exists."))
                        .arg(QDir::toNativeSeparators(newDirPath)));
            } else if (QDir().mkdir(newDirPath)) {
                refreshView();
            } else {
                QMessageBox::warning(this, TR("Create New Directory"),
                    wstringToQString(
                        Nelson::resolveMessageTemplate(L"Nelson:gui:unableCreateDirectory",
                            L"Unable to create the directory '%1'."))
                        .arg(QDir::toNativeSeparators(newDirPath)));
            }
        }
    }
}
//=============================================================================
void
QtFileBrowser::createNewFileScript()
{
    QModelIndex currentIndex = tree->currentIndex();
    if (currentIndex.isValid()) {
        QString currentPath = model->filePath(currentIndex);
        QString newScriptFilename = QInputDialog::getText(
            this, TR("Filename"), TR("New script name:"), QLineEdit::Normal, "untitled.m");
        if (!newScriptFilename.isEmpty()) {
            QString fileName = currentPath + QDir::separator() + newScriptFilename;
            QFile file(fileName);
            if (file.open(QIODevice::WriteOnly)) {
                QTextStream stream(&file);
                stream << "%% Nelson script\n";
                stream << "%%\n";
                file.close();
                refreshView();
            } else {
                QMessageBox::warning(this, TR("Filename"),
                    wstringToQString(Nelson::resolveMessageTemplate(L"Nelson:gui:unableCreateFile",
                                         L"Unable to create the file '%1'."))
                        .arg(QDir::toNativeSeparators(fileName)));
            }
        }
    }
}
//=============================================================================
void
QtFileBrowser::createNewFileFunction()
{
    QModelIndex currentIndex = tree->currentIndex();

    if (currentIndex.isValid()) {
        QString currentPath = model->filePath(currentIndex);

        QString newScriptFilename = QInputDialog::getText(
            this, TR("Filename"), TR("New function name:"), QLineEdit::Normal, "untitled.m");

        if (!newScriptFilename.isEmpty()) {
            QFileInfo fileInfo(newScriptFilename);
            QString functionName = fileInfo.baseName();

            QString fileName = currentPath + QDir::separator() + newScriptFilename;

            QFile file(fileName);
            if (file.open(QIODevice::WriteOnly)) {
                QTextStream stream(&file);
                stream << "function varargout = " + functionName + " (varargin)\n";
                stream << "  varargout = {};\n";
                stream << "end\n";

                file.close();

                refreshView();
            } else {
                QMessageBox::warning(this, TR("Filename"),
                    wstringToQString(Nelson::resolveMessageTemplate(L"Nelson:gui:unableCreateFile",
                                         L"Unable to create the file '%1'."))
                        .arg(QDir::toNativeSeparators(fileName)));
            }
        }
    }
}
//=============================================================================
void
QtFileBrowser::addToPath()
{
    QModelIndex currentIndex = tree->currentIndex();
    if (currentIndex.isValid()) {
        QString currentPath = model->filePath(currentIndex);
        PathFunctionIndexerManager::getInstance()->addPath(
            QStringTowstring(currentPath), false, false);
        refreshView();
    }
}
//=============================================================================
void
QtFileBrowser::removeToPath()
{
    QModelIndex currentIndex = tree->currentIndex();
    if (currentIndex.isValid()) {
        QString currentPath = model->filePath(currentIndex);
        PathFunctionIndexerManager::getInstance()->removePath(QStringTowstring(currentPath));
        refreshView();
    }
}
//=============================================================================
void
QtFileBrowser::closeEvent(QCloseEvent* event)
{
    event->accept();
    emit closeFileBrowser();
}
//=============================================================================
