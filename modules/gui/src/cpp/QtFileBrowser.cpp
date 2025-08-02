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
#include "QtTranslation.hpp"
#include "Nelson_VERSION.h"
#include "HistoryBrowser.hpp"
#include "ForceWindowsTitleBarToDark.hpp"
#include "QStringConverter.hpp"
#include "NelsonConfiguration.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "IsValidVariableName.hpp"
#include <QtCore/QSettings>
#include <QtCore/QTextStream>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QMenu>
#include <QtWidgets/QApplication>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QInputDialog>
#include <QtGui/QDesktopServices>
#include <QtGui/QStandardItemModel>
//=============================================================================
const QString SETTING_FILE_BROWSER_VISIBILITY = "fb_Visibility";
const QString SETTING_FILE_BROWSER_GEOMETRY = "fb_Geometry";
const QString SETTING_FILE_BROWSER_HEADER_SIZE = "fb_HeaderSectionSize_%1";
const QString SETTING_FILE_BROWSER_HEADER_SECTION = "fb_HeaderSection_%1";
//=============================================================================
using namespace Nelson;
//=============================================================================
QVariant
CustomFileSystemModel::data(const QModelIndex& index, int role) const
{
    QVariant value = QFileSystemModel::data(index, role);

    if (role == Qt::ForegroundRole && this->_isOn) {
        QString filePath = QFileSystemModel::filePath(index);
        if (QFileInfo(filePath).isDir()) {
            std::wstring wFilePath = QStringTowstring(filePath);
            Nelson::wstringVector paths
                = PathFunctionIndexerManager::getInstance()->getPathNameVector();
            for (auto path : paths) {
                if (path == wFilePath) {
                    return QVariant::fromValue(QColor(Qt::black));
                }
            }
            return QVariant::fromValue(QColor(Qt::gray));
        }
    }

    return value;
}
//=============================================================================
QtFileBrowser::QtFileBrowser(QWidget* parent) : QDockWidget(TR("File Browser"), parent)
{
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(this->winId());
#endif
    model = new CustomFileSystemModel;
    modelBar = new QFileSystemModel;

    model->setRootPath(QDir::currentPath());
    model->setFilter(QDir::AllEntries | QDir::NoDot | QDir::Drives);

    modelBar->setRootPath(QDir::currentPath());
    modelBar->setFilter(QDir::AllDirs | QDir::NoDotAndDotDot | QDir::Drives);

    tree = new QTreeView;
    tree->setRootIndex(model->index(QDir::currentPath()));
    tree->setSelectionMode(QAbstractItemView::SingleSelection);
    tree->setSortingEnabled(true);
    // Create the header
    QHeaderView* header = tree->header();
    header->resizeSection(0, 64);

    // Set the context menu for the header
    header->setContextMenuPolicy(Qt::CustomContextMenu);

    QVBoxLayout* mainLayout = new QVBoxLayout;
    createTopBar(mainLayout);
    mainLayout->addWidget(tree);

    setLayout(mainLayout);

    setFocusPolicy(Qt::StrongFocus);
    sizeGrip = new QSizeGrip(this);
    mainLayout->addWidget(sizeGrip, 0, Qt::AlignBottom | Qt::AlignRight);

    tree->setModel(model);
    connect(tree, SIGNAL(doubleClicked(const QModelIndex&)), this,
        SLOT(doubleClicked(const QModelIndex&)));
    setObjectName("fileBrowser");

    QWidget* centralWidget = new QWidget(this);
    centralWidget->setLayout(mainLayout);
    setWidget(centralWidget);

    connect(header, SIGNAL(customContextMenuRequested(const QPoint&)), this,
        SLOT(showHeaderContextMenu(const QPoint&)));

    connect(
        this, &QDockWidget::dockLocationChanged, this, &QtFileBrowser::handleDockLocationChanged);

    setMinimumSize(50, 50);
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
    QSettings settings(NELSON_PRODUCT_NAME, NELSON_SEMANTIC_VERSION_STRING);
    restoreGeometry(settings.value(SETTING_FILE_BROWSER_GEOMETRY).toByteArray());
    loadHeaderSettings();
}
//=============================================================================
void
QtFileBrowser::restoreVisibility()
{
    QSettings settings(NELSON_PRODUCT_NAME, NELSON_SEMANTIC_VERSION_STRING);
    bool savedVisibility = settings.value(SETTING_FILE_BROWSER_VISIBILITY, true).toBool();
    setVisible(savedVisibility);
}
//=============================================================================
void
QtFileBrowser::savePositionAndVisibility()
{
    QSettings settings(NELSON_PRODUCT_NAME, NELSON_SEMANTIC_VERSION_STRING);
    settings.setValue(SETTING_FILE_BROWSER_VISIBILITY, isVisible());
    settings.setValue(SETTING_FILE_BROWSER_GEOMETRY, saveGeometry());

    saveHeaderSettings();
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

    contextMenu.addSeparator();

    QAction* refreshAction = new QAction(TR("Refresh"), this);
    refreshAction->setShortcut(QKeySequence(Qt::Key_F5));
    connect(refreshAction, &QAction::triggered, this, &QtFileBrowser::updateCurrentPath);

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
    QSettings settings(NELSON_PRODUCT_NAME, NELSON_SEMANTIC_VERSION_STRING);

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
    QSettings settings(NELSON_PRODUCT_NAME, NELSON_SEMANTIC_VERSION_STRING);

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
QtFileBrowser::updateCurrentPath()
{
    tree->setRootIndex(model->index(QDir::currentPath()));
    pathLineEdit->setText(QDir::currentPath());
}
//=============================================================================
void
QtFileBrowser::deleteSelected()
{
    QString currentPath = model->filePath(tree->currentIndex());
    QFileInfo fileInfo(currentPath);
    if (fileInfo.isFile()) {
        QFile file(currentPath);
        file.remove();
    } else if (fileInfo.isDir()) {
        QDir qDir(currentPath);
        qDir.removeRecursively();
    }
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

            if (isDir) {
                QDir dir(currentPath);
                if (dir.rename(currentPath, newPath)) {
                    updateCurrentPath();
                }
            } else {
                if (QFile::rename(currentPath, newPath)) {
                    updateCurrentPath();
                }
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
        updateCurrentPath();
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
            if (!QDir(newDirPath).exists()) {
                if (QDir().mkdir(newDirPath)) {
                    updateCurrentPath();
                }
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
                updateCurrentPath();
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

                updateCurrentPath();
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
        updateCurrentPath();
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
        updateCurrentPath();
    }
}
//=============================================================================
void
QtFileBrowser::handleDockLocationChanged(Qt::DockWidgetArea area)
{
    sizeGrip->setVisible(area == Qt::NoDockWidgetArea);
}
//=============================================================================
void
QtFileBrowser::closeEvent(QCloseEvent* event)
{
    event->accept();
    emit closeFileBrowser();
}
//=============================================================================
