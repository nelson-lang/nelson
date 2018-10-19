//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QFileInfo>
#include <QtCore/QMimeData>
#include <QtCore/QTextStream>
#include <QtGui/QClipboard>
#include <QtGui/QKeyEvent>
#include <QtGui/QTextDocumentFragment>
#include <QtPrintSupport/QPrintDialog>
#include <QtPrintSupport/QPrintPreviewDialog>
#include <QtPrintSupport/QPrinter>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QFontDialog>
#include <QtWidgets/QInputDialog>
#include <QtWidgets/QMenuBar>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QToolBar>
#include <algorithm>
#include <boost/algorithm/string.hpp>
#include "QtTextEditor.h"
#include "ExecuteCommand.hpp"
#include "GetNelsonPath.hpp"
#include "ModulesManager.hpp"
#include "QStringConverter.hpp"
#include "QtLineNumber.h"
#include "QtTextIndent.h"
#include "QtTranslation.hpp"
#include "SmartIndent.h"
#include "TextEditorPreferences.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
#define DEFAULT_FILENAME "untitled.nls"
#define DEFAULT_DELAY_MSG 1500
//=============================================================================
QtTextEditor::QtTextEditor(Evaluator* eval)
{
    setAcceptDrops(true);
    recentFilenames.clear();
    nlsEvaluator = eval;
    textEditorRootPath = Nelson::GetModulePath(L"text_editor");
    QString fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/document-open.svg"));
    setWindowIcon(QPixmap(fileNameIcon));
    prevEdit = nullptr;
    tab = new QTabWidget(this);
    tab->setTabsClosable(true);
    setCentralWidget(tab);
    readSettings();
    createActions();
    createMenus();
    createToolBars();
    createStatusBar();
    connect(tab, SIGNAL(currentChanged(int)), this, SLOT(tabChanged(int)));
    connect(tab, SIGNAL(tabCloseRequested(int)), this, SLOT(closeTab(int)));
    addTab();
}
//=============================================================================
void
QtTextEditor::loadOrCreateFile(const QString& filename)
{
    if (filename.isEmpty()) {
        return;
    }
    QFile ff(filename);
    QFileInfo fileInfo(ff);
    bool isdir = (fileInfo.exists() && fileInfo.isDir());
    if (isdir) {
        QString msg = TR("Cannot edit the directory: %1").arg(fileInfo.absoluteFilePath());
        statusBar()->showMessage(msg);
        return;
    }
    bool isfile = (fileInfo.exists() && fileInfo.isFile());
    if (!isfile) {
        if (QMessageBox::question(this, TR("Nelson"),
                TR("File %1 does not exists. Do you want to create it?")
                    .arg(fileInfo.absoluteFilePath()),
                QMessageBox::Yes | QMessageBox::Default, QMessageBox::No)
            == QMessageBox::No) {
            return;
        } else {
            addTabUntitled();
            setCurrentFile(filename);
            return;
        }
    }
    loadFile(filename);
}
//=============================================================================
void
QtTextEditor::createActions()
{
    QString fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/document-new.svg"));
    newAction = new QAction(QIcon(fileNameIcon), TR("&New"), this);
    connect(newAction, SIGNAL(triggered()), this, SLOT(addTab()));
    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/document-open.svg"));
    openAction = new QAction(QIcon(fileNameIcon), TR("&Open"), this);
    connect(openAction, SIGNAL(triggered()), this, SLOT(open()));
    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/document-save.svg"));
    saveAction = new QAction(QIcon(fileNameIcon), TR("&Save"), this);
    saveAction->setShortcut(Qt::Key_S | Qt::CTRL);
    connect(saveAction, SIGNAL(triggered()), this, SLOT(save()));
    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/document-save-as.svg"));
    saveAsAction = new QAction(QIcon(fileNameIcon), TR("Save &As"), this);
    connect(saveAsAction, SIGNAL(triggered()), this, SLOT(saveAs()));
    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/document-save-all.svg"));
    saveAllAction = new QAction(QIcon(fileNameIcon), TR("Save A&ll"), this);
    saveAllAction->setShortcut(Qt::Key_S | Qt::CTRL | Qt::SHIFT);
    connect(saveAllAction, SIGNAL(triggered()), this, SLOT(saveAll()));
    for (int i = 0; i < MAX_RECENT_FILES; ++i) {
        recentFileActions[i] = new QAction(this);
        recentFileActions[i]->setVisible(false);
        connect(recentFileActions[i], SIGNAL(triggered()), this, SLOT(openRecentFile()));
    }
    closeAction = new QAction(TR("&Close"), this);
    connect(closeAction, SIGNAL(triggered()), this, SLOT(closeTab()));
    closeAllAction = new QAction(TR("Close &All"), this);
    connect(closeAllAction, SIGNAL(triggered()), this, SLOT(closeAllTabs()));
    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/document-exit.svg"));
    quitAction = new QAction(QIcon(fileNameIcon), TR("&Quit Editor"), this);
    saveAllAction->setShortcut(Qt::Key_F4 | Qt::ALT);
    connect(quitAction, SIGNAL(triggered()), this, SLOT(close()));
    fileNameIcon
        = Nelson::wstringToQString(textEditorRootPath + std::wstring(L"/resources/edit-copy.svg"));
    copyAction = new QAction(QIcon(fileNameIcon), TR("&Copy"), this);
    fileNameIcon
        = Nelson::wstringToQString(textEditorRootPath + std::wstring(L"/resources/edit-cut.svg"));
    cutAction = new QAction(QIcon(fileNameIcon), TR("C&ut"), this);
    fileNameIcon
        = Nelson::wstringToQString(textEditorRootPath + std::wstring(L"/resources/edit-paste.svg"));
    pasteAction = new QAction(QIcon(fileNameIcon), TR("&Paste"), this);
    fileNameIcon
        = Nelson::wstringToQString(textEditorRootPath + std::wstring(L"/resources/edit-undo.svg"));
    undoAction = new QAction(QIcon(fileNameIcon), TR("&Undo"), this);
    undoAction->setShortcut(Qt::Key_Z | Qt::CTRL);
    connect(undoAction, SIGNAL(triggered()), this, SLOT(undo()));
    fileNameIcon
        = Nelson::wstringToQString(textEditorRootPath + std::wstring(L"/resources/edit-redo.svg"));
    redoAction = new QAction(QIcon(fileNameIcon), TR("&Redo"), this);
    redoAction->setShortcut(Qt::Key_Y | Qt::CTRL);
    connect(redoAction, SIGNAL(triggered()), this, SLOT(redo()));
    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/format-text-bold.svg"));
    fontAction = new QAction(QIcon(fileNameIcon), TR("&Font"), this);
    connect(fontAction, SIGNAL(triggered()), this, SLOT(font()));
    copyFullPathAction = new QAction(TR("Copy Full Path"), this);
    connect(copyFullPathAction, SIGNAL(triggered()), this, SLOT(copyFullPath()));
    commentAction = new QAction(TR("Comment"), this);
    commentAction->setShortcut(Qt::Key_R | Qt::CTRL);
    connect(commentAction, SIGNAL(triggered()), this, SLOT(comment()));
    uncommentAction = new QAction(TR("Uncomment"), this);
    uncommentAction->setShortcut(Qt::Key_T | Qt::CTRL);
    connect(uncommentAction, SIGNAL(triggered()), this, SLOT(uncomment()));
    gotoLineAction = new QAction(TR("&Go To Line ..."), this);
    gotoLineAction->setShortcut(Qt::Key_G | Qt::CTRL);
    connect(gotoLineAction, SIGNAL(triggered()), this, SLOT(gotoLine()));
    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/run-file-start.svg"));
    runFileAction = new QAction(QIcon(fileNameIcon), TR("&Run file"), this);
    connect(runFileAction, SIGNAL(triggered()), this, SLOT(runFile()));
    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/stop-interpreter.svg"));
    stopRunAction = new QAction(QIcon(fileNameIcon), TR("&Stop execution"), this);
    connect(stopRunAction, SIGNAL(triggered()), this, SLOT(stopRun()));
    fileNameIcon
        = Nelson::wstringToQString(Nelson::GetRootPath()) + QString("/resources/help-icon.svg");
    helpOnSelectionAction = new QAction(QIcon(fileNameIcon), TR("Help on Selection"), this);
    connect(helpOnSelectionAction, SIGNAL(triggered()), this, SLOT(helpOnSelection()));
    smartIndentAction = new QAction(TR("Smart Indent"), this);
    smartIndentAction->setShortcut(Qt::Key_I | Qt::CTRL);
    connect(smartIndentAction, SIGNAL(triggered()), this, SLOT(smartIndent()));
    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/document-print.svg"));
    printAction = new QAction(QIcon(fileNameIcon), TR("Print"), this);
    printAction->setShortcut(Qt::Key_P | Qt::CTRL);
    connect(printAction, SIGNAL(triggered()), this, SLOT(printDocument()));
    evaluateSelectionAction = new QAction(TR("Evaluate selection"), this);
    connect(evaluateSelectionAction, SIGNAL(triggered()), this, SLOT(evaluateSelection()));
}
//=============================================================================
void
QtTextEditor::createMenus()
{
    fileMenu = menuBar()->addMenu(TR("&File"));
    fileMenu->addAction(newAction);
    fileMenu->addAction(openAction);
    fileMenu->addSeparator();
    fileMenu->addAction(saveAction);
    fileMenu->addAction(saveAsAction);
    fileMenu->addAction(saveAllAction);
    fileMenu->addSeparator();
    fileMenu->addAction(printAction);
    fileMenu->addSeparator();
    fileMenu->addAction(closeAction);
    fileMenu->addAction(closeAllAction);
    separatorAction = fileMenu->addSeparator();
    for (int i = 0; i < MAX_RECENT_FILES; ++i) {
        fileMenu->addAction(recentFileActions[i]);
    }
    fileMenu->addSeparator();
    updateRecentFileActions();
    fileMenu->addAction(quitAction);
    editMenu = menuBar()->addMenu(TR("&Edit"));
    editMenu->addAction(gotoLineAction);
    editMenu->addSeparator();
    editMenu->addAction(undoAction);
    editMenu->addAction(redoAction);
    editMenu->addSeparator();
    editMenu->addAction(copyAction);
    editMenu->addAction(pasteAction);
    editMenu->addAction(cutAction);
    editMenu->addSeparator();
    editMenu->addAction(commentAction);
    editMenu->addAction(uncommentAction);
    editMenu->addAction(smartIndentAction);
    editMenu->addSeparator();
    editMenu->addAction(fontAction);
    contextMenu = new QMenu();
    contextMenu->addAction(copyFullPathAction);
    contextMenu->addSeparator();
    contextMenu->addAction(evaluateSelectionAction);
    contextMenu->addAction(helpOnSelectionAction);
    contextMenu->addSeparator();
    contextMenu->addAction(copyAction);
    contextMenu->addAction(cutAction);
    contextMenu->addAction(pasteAction);
    contextMenu->addSeparator();
    contextMenu->addAction(undoAction);
    contextMenu->addAction(redoAction);
    contextMenu->addSeparator();
    contextMenu->addAction(commentAction);
    contextMenu->addAction(uncommentAction);
    contextMenu->addAction(smartIndentAction);
}
//=============================================================================
void
QtTextEditor::createToolBars()
{
    fileToolBar = addToolBar(TR("File"));
    fileToolBar->addAction(newAction);
    fileToolBar->addAction(openAction);
    fileToolBar->addAction(saveAction);
    fileToolBar->addAction(saveAsAction);
    fileToolBar->addAction(saveAllAction);
    editToolBar = addToolBar(TR("Edit"));
    editToolBar->addAction(printAction);
    editToolBar->addSeparator();
    editToolBar->addAction(copyAction);
    editToolBar->addAction(cutAction);
    editToolBar->addAction(pasteAction);
    editToolBar->addSeparator();
    editToolBar->addAction(undoAction);
    editToolBar->addAction(redoAction);
    editToolBar->addSeparator();
    editToolBar->addAction(runFileAction);
    editToolBar->addAction(stopRunAction);
    editToolBar->addSeparator();
    editToolBar->addAction(fontAction);
}
//=============================================================================
void
QtTextEditor::createStatusBar()
{
    statusBar()->showMessage(TR("Ready"));
}
//=============================================================================
void
QtTextEditor::updateRecentFileActions()
{
    int numRecentFiles = std::min((int)recentFilenames.size(), (int)MAX_RECENT_FILES);
    for (int i = 0; i < numRecentFiles; ++i) {
        QString text
            = TR("&%1 %2").arg(i + 1).arg(strippedName(wstringToQString(recentFilenames[i])));
        recentFileActions[i]->setText(text);
        recentFileActions[i]->setData(wstringToQString(recentFilenames[i]));
        recentFileActions[i]->setVisible(true);
    }
    for (int j = numRecentFiles; j < MAX_RECENT_FILES; ++j) {
        recentFileActions[j]->setVisible(false);
    }
    separatorAction->setVisible(numRecentFiles > 0);
}
//=============================================================================
bool
QtTextEditor::maybeSave()
{
    if (currentEditor()->document()->isModified()) {
        int ret = QMessageBox::warning(this, TR("Nelson"),
            TR("The document %1 has been modified.\nDo you want to save your changes ?")
                .arg(shownName()),
            QMessageBox::Yes | QMessageBox::Default, QMessageBox::No,
            QMessageBox::Cancel | QMessageBox::Escape);
        if (ret == QMessageBox::Yes) {
            return save();
        } else if (ret == QMessageBox::Cancel) {
            return false;
        }
    }
    return true;
}
//=============================================================================
void
QtTextEditor::openRecentFile()
{
    QAction* action = qobject_cast<QAction*>(sender());
    QString fileName = action->data().toString();
    loadFile(fileName);
}
//=============================================================================
bool
QtTextEditor::saveFile(const QString& filename)
{
    bool res = false;
    QFile file(filename);
    if (file.open(QFile::WriteOnly | QFile::Text)) {
        QTextStream out(&file);
        out.setCodec("UTF-8");
        QApplication::setOverrideCursor(Qt::WaitCursor);
        out << currentEditor()->toPlainText();
        QApplication::restoreOverrideCursor();
        setCurrentFile(filename);
        statusBar()->showMessage(TR("File saved"), DEFAULT_DELAY_MSG);
        res = true;
        file.close();
    } else {
        QMessageBox::warning(this, TR("Nelson"),
            TR("Cannot write file %1:\n%2.").arg(filename).arg(file.errorString()));
        statusBar()->showMessage(TR("File not saved"), DEFAULT_DELAY_MSG);
        res = false;
    }
    return res;
}
//=============================================================================
void
QtTextEditor::loadFile(const QString& filename)
{
    QFile file(filename);
    if (!file.open(QFile::ReadOnly | QFile::Text)) {
        QMessageBox::warning(this, TR("Nelson"),
            TR("Cannot read file %1:\n%2.").arg(filename).arg(file.errorString()));
        return;
    }
    // Check for one of the editors that might be editing this file already
    for (int i = 0; i < tab->count(); i++) {
        QWidget* widget = tab->widget(i);
        QtEditPane* editPane = qobject_cast<QtEditPane*>(widget);
        if (editPane) {
            if (editPane->getFileName() == filename) {
                tab->setCurrentIndex(i);
                return;
            }
        }
    }
    addTabUntitled();
    if (fileWatcher.files().empty()) {
        connect(&fileWatcher, SIGNAL(fileChanged(const QString)), this,
            SLOT(reloadFile(const QString)));
    }
    fileWatcher.addPath(filename);
    QTextStream in(&file);
    in.setCodec("UTF-8");
    QApplication::setOverrideCursor(Qt::WaitCursor);
    currentEditor()->setPlainText(in.readAll());
    file.close();
    QApplication::restoreOverrideCursor();
    setCurrentFile(filename);
    statusBar()->showMessage(TR("File loaded"), DEFAULT_DELAY_MSG);
    if (filename.endsWith(".nls")) {
        runFileAction->setEnabled(true);
    } else {
        runFileAction->setEnabled(false);
    }
    currentEditor()->setFocus();
}
//=============================================================================
void
QtTextEditor::setCurrentFile(const QString& filename)
{
    setCurrentFilename(filename);
    currentEditor()->document()->setModified(false);
    setWindowModified(false);
    updateTitles();
    std::wstring name = QStringTowstring(filename);
    recentFilenames.erase(
        std::remove(recentFilenames.begin(), recentFilenames.end(), name), recentFilenames.end());
    recentFilenames.insert(recentFilenames.begin(), name);
    while (recentFilenames.size() > MAX_RECENT_FILES) {
        recentFilenames.pop_back();
    }
    writeSettings();
    foreach (QWidget* widget, QApplication::topLevelWidgets()) {
        QtTextEditor* tEditor = qobject_cast<QtTextEditor*>(widget);
        if (tEditor) {
            tEditor->updateRecentFileActions();
        }
    }
}
//=============================================================================
QString
QtTextEditor::strippedName(const QString& fullfilename)
{
    return QFileInfo(fullfilename).fileName();
}
//=============================================================================
QtTextEdit*
QtTextEditor::currentEditor()
{
    QWidget* widget = tab->currentWidget();
    QtEditPane* editPane = qobject_cast<QtEditPane*>(widget);
    if (!editPane) {
        addTab();
        widget = tab->currentWidget();
        editPane = qobject_cast<QtEditPane*>(widget);
    }
    return editPane->getEditor();
}
//=============================================================================
void
QtTextEditor::setCurrentFilename(QString filename)
{
    QWidget* widget = tab->currentWidget();
    QtEditPane* editPane = qobject_cast<QtEditPane*>(widget);
    if (!editPane) {
        addTab();
        widget = tab->currentWidget();
        editPane = qobject_cast<QtEditPane*>(widget);
    }
    editPane->setFileName(filename);
}
//=============================================================================
QString
QtTextEditor::currentFilename()
{
    QWidget* widget = tab->currentWidget();
    QtEditPane* editPane = qobject_cast<QtEditPane*>(widget);
    if (!editPane) {
        addTab();
        widget = tab->currentWidget();
        editPane = qobject_cast<QtEditPane*>(widget);
    }
    return editPane->getFileName();
}
//=============================================================================
QString
QtTextEditor::shownName()
{
    QString sName;
    if (currentFilename().isEmpty()) {
        sName = DEFAULT_FILENAME;
    } else {
        sName = strippedName(currentFilename());
    }
    return sName;
}
//=============================================================================
void
QtTextEditor::updateTitles()
{
    tab->setTabText(tab->currentIndex(), shownName());
    if (currentFilename().isEmpty()) {
        setWindowTitle(TR("Nelson Editor"));
    } else {
        setWindowTitle(QString("%1[*]").arg(currentFilename()) + " - " + TR("Nelson Editor"));
    }
    documentWasModified();
}
//=============================================================================
void
QtTextEditor::readSettings()
{
    QPoint pos;
    QSize size;
    TextEditorLoadPreferences(m_font, pos, size, recentFilenames);
    resize(size);
    move(pos);
    updateFont();
}
//=============================================================================
void
QtTextEditor::writeSettings()
{
    TextEditorSavePreferences(m_font, pos(), size(), recentFilenames);
}
//=============================================================================
void
QtTextEditor::updateFont()
{
    for (int i = 0; i < tab->count(); i++) {
        QWidget* p = tab->widget(i);
        QtEditPane* te = qobject_cast<QtEditPane*>(p);
        te->setFont(m_font);
    }
}
//=============================================================================
bool
QtTextEditor::saveAll()
{
    int backupIndex = tab->currentIndex();
    for (int i = 0; i < tab->count(); i++) {
        tab->setCurrentIndex(i);
        if (currentEditor()->document()->isModified()) {
            save();
        }
    }
    tab->setCurrentIndex(backupIndex);
    return true;
}
//=============================================================================
bool
QtTextEditor::save()
{
    bool res = false;
    if (!currentFilename().isEmpty()) {
        lastFilenameSaved = currentFilename();
        res = saveFile(lastFilenameSaved);
    } else {
        res = saveAs();
    }
    return res;
}
//=============================================================================
bool
QtTextEditor::saveAs()
{
    bool res = false;
    QString fileName = QFileDialog::getSaveFileName(
        this, TR("Save File"), shownName(), TR("Nelson (*.nls *.nlf)"));
    if (!fileName.isEmpty()) {
        for (int i = 0; i < tab->count(); i++) {
            QWidget* w = tab->widget(i);
            QtEditPane* te = qobject_cast<QtEditPane*>(w);
            if (te) {
                if ((te->getFileName() == fileName) && (i != tab->currentIndex())) {
                    QMessageBox::critical(this, TR("Nelson"),
                        "Cannot save to filename\n " + fileName
                            + "\n as this file is open in another tab.\n  Please close the other "
                              "tab and\n then repeat the save operation.");
                    tab->setCurrentIndex(i);
                    return false;
                }
            }
        }
        res = saveFile(fileName);
    } else {
        res = false;
    }
    return res;
}
//=============================================================================
void
QtTextEditor::open()
{
    if (maybeSave()) {
        QStringList fileNames = QFileDialog::getOpenFileNames(this, TR("Open file ..."),
            QDir::currentPath(),
            TR("Nelson (*.nls *.nlf);;Text files (*.txt);;Markdown files (*.md);;All files (*.*)"));
        for (int k = 0; k < fileNames.size(); k++) {
            loadFile(fileNames[k]);
        }
    }
}
//=============================================================================
QtTextEditor::~QtTextEditor()
{
    saveAll();
    if (contextMenu) {
        delete contextMenu;
        contextMenu = nullptr;
    }
}
//=============================================================================
void
QtTextEditor::font()
{
    bool bOk = false;
    QFont new_font = QFontDialog::getFont(&bOk, m_font, this);
    m_font = new_font;
    updateFont();
}
//=============================================================================
void
QtTextEditor::addTabUntitled()
{
    QWidget* widget = tab->currentWidget();
    QtEditPane* editPane = qobject_cast<QtEditPane*>(widget);
    if (!editPane) {
        addTab();
    } else {
        bool foundActive = false;
        for (int i = 0; i < tab->count(); i++) {
            if (tab->tabText(i) == DEFAULT_FILENAME) {
                foundActive = true;
                tab->setCurrentIndex(i);
                break;
            }
        }
        if (!foundActive) {
            addTab();
        }
    }
}
//=============================================================================
void
QtTextEditor::addTab()
{
    QtEditPane* editPane = new QtEditPane();
    if (editPane) {
        editPane->setFileName(DEFAULT_FILENAME);
        tab->addTab(editPane, DEFAULT_FILENAME);
        tab->setCurrentIndex(tab->count() - 1);
        updateFont();
    }
}
//=============================================================================
void
QtTextEditor::closeTab()
{
    if (maybeSave()) {
        QWidget* p = tab->currentWidget();
        tab->removeTab(tab->currentIndex());
        p->deleteLater();
        prevEdit = nullptr;
    }
    fileWatcher.removePath(currentFilename());
}
//=============================================================================
void
QtTextEditor::closeAllTabs()
{
    int nbTabs = tab->count();
    for (int i = 0; i < nbTabs; i++) {
        QWidget* currentWidget = tab->currentWidget();
        maybeSave();
        tab->removeTab(tab->currentIndex());
        currentWidget->deleteLater();
    }
    setWindowTitle(TR("Nelson Editor"));
    fileWatcher.removePaths(fileWatcher.directories());
}
//=============================================================================
void
QtTextEditor::closeTab(int indexTab)
{
    closeTab();
}
//=============================================================================
void
QtTextEditor::tabChanged(int indexTab)
{
    disconnect(copyAction, SIGNAL(triggered()), 0, 0);
    connect(cutAction, SIGNAL(triggered()), currentEditor(), SLOT(cut()));
    connect(copyAction, SIGNAL(triggered()), currentEditor(), SLOT(copy()));
    connect(pasteAction, SIGNAL(triggered()), currentEditor(), SLOT(paste()));
    if (prevEdit) {
        disconnect(prevEdit->document(), SIGNAL(contentsChanged()), 0, 0);
    }
    connect(
        currentEditor()->document(), SIGNAL(contentsChanged()), this, SLOT(documentWasModified()));
    updateTitles();
    prevEdit = currentEditor();
    QString filename = currentFilename();
    if (filename.endsWith(".nls") || filename.isEmpty()) {
        runFileAction->setEnabled(true);
    } else {
        runFileAction->setEnabled(false);
    }
}
//=============================================================================
void
QtTextEditor::documentWasModified()
{
    setWindowModified(currentEditor()->document()->isModified());
    if (currentEditor()->document()->isModified()) {
        QString fileNameIcon = Nelson::wstringToQString(
            textEditorRootPath + std::wstring(L"/resources/document-modified.svg"));
        tab->setTabIcon(tab->currentIndex(), QIcon(fileNameIcon));
        tab->setTabText(tab->currentIndex(), shownName() + "*");
    } else {
        QString fileNameIcon = Nelson::wstringToQString(
            textEditorRootPath + std::wstring(L"/resources/document-new.svg"));
        tab->setTabIcon(tab->currentIndex(), QIcon(fileNameIcon));
        tab->setTabText(tab->currentIndex(), shownName());
    }
}
//=============================================================================
void
QtTextEditor::closeEvent(QCloseEvent* event)
{
    if (!isVisible()) {
        return;
    }
    writeSettings();
    int nbTabs = tab->count();
    while (nbTabs > 0) {
        if (maybeSave()) {
            QWidget* p = tab->currentWidget();
            tab->removeTab(tab->currentIndex());
            p->deleteLater();
            prevEdit = nullptr;
        } else {
            event->ignore();
            return;
        }
        nbTabs--;
    }
    event->accept();
}
//=============================================================================
void
QtTextEditor::contextMenuEvent(QContextMenuEvent* event)
{
    QString selectedText = currentEditor()->textCursor().selectedText();
    selectedText = selectedText.trimmed();
    helpOnSelectionAction->setVisible(!selectedText.isEmpty());
    evaluateSelectionAction->setVisible(!selectedText.isEmpty());
    contextMenu->exec(event->globalPos());
}
//=============================================================================
void
QtTextEditor::copyFullPath()
{
    QClipboard* clipboard = QApplication::clipboard();
    if (!currentFilename().isEmpty()) {
        QString filename = currentFilename();
#ifdef _MSC_VER
        filename.replace("/", "\\");
#endif
        clipboard->setText(filename);
    } else {
        clipboard->setText(DEFAULT_FILENAME);
    }
}
//=============================================================================
void
QtTextEditor::undo()
{
    currentEditor()->undo();
}
//=============================================================================
void
QtTextEditor::redo()
{
    currentEditor()->redo();
}
//=============================================================================
void
QtTextEditor::comment()
{
    currentEditor()->comment();
}
//=============================================================================
void
QtTextEditor::uncomment()
{
    currentEditor()->uncomment();
}
//=============================================================================
int
QtTextEditor::getCurrentLineNumber()
{
    return currentEditor()->textCursor().blockNumber() + 1;
}
//=============================================================================
bool
QtTextEditor::gotoLineNumber(int lineNumber)
{
    QTextCursor text_cursor(currentEditor()->document()->findBlockByLineNumber(lineNumber - 1));
    text_cursor.movePosition(QTextCursor::StartOfLine);
    currentEditor()->setTextCursor(text_cursor);
    return true;
}
//=============================================================================
void
QtTextEditor::gotoLine()
{
    bool ok;
    int line_number
        = QInputDialog::getInt(this, TR("Go To Line ..."), TR("Enter a line number to go to: "), 1,
            1, currentEditor()->document()->blockCount(), 1, &ok);
    if (ok) {
        gotoLineNumber(line_number);
    }
}
//=============================================================================
void
QtTextEditor::runFile()
{
    if (nlsEvaluator->getInterface()->isAtPrompt()) {
        if (currentEditor()->document()->isModified() || currentEditor()->document()->isEmpty()) {
            save();
        }
        std::wstring filename = QStringTowstring(currentFilename());
        executeCommand(std::wstring(L"run('") + filename + std::wstring(L"')"));
    } else {
        QMessageBox::warning(
            this, _("Run file ...").c_str(), _("Interpreter currently runs.").c_str());
    }
}
//=============================================================================
void
QtTextEditor::stopRun()
{
    if (!nlsEvaluator->getInterface()->isAtPrompt()) {
        NelsonConfiguration::getInstance()->setInterruptPending(true);
    }
}
//=============================================================================
void
QtTextEditor::helpOnSelection()
{
    QString selectedText = currentEditor()->textCursor().selectedText();
    selectedText = selectedText.trimmed();
    if (selectedText.startsWith('\'') && selectedText.endsWith('\'')) {
        selectedText.chop(1);
        selectedText.remove(0, 1);
    }
    if (!selectedText.isEmpty()) {
        std::wstring text = QStringTowstring(selectedText);
        boost::algorithm::replace_all(text, "'", "\"");
        std::wstring cmd = L"doc('" + text + L"');";
        try {
            nlsEvaluator->evaluateString(Nelson::wstring_to_utf8(cmd), true);
        } catch (const Exception&) {
        }
    }
}
//=============================================================================
void
QtTextEditor::smartIndent()
{
    int indentSize = 2;
    QTextCursor cursor(currentEditor()->textCursor());
    QTextCursor cursorBackup(currentEditor()->textCursor());
    bool noTextSelected = false;
    if (cursor.selectedText().isEmpty()) {
        noTextSelected = true;
    }
    if (noTextSelected) {
        currentEditor()->selectAll();
        cursor = currentEditor()->textCursor();
    }
    ::smartIndent(currentEditor(), indentSize);
    if (noTextSelected) {
        currentEditor()->setTextCursor(cursorBackup);
        currentEditor()->setFocus();
    }
}
//=============================================================================
void
QtTextEditor::printDocument()
{
    QPrinter printer;
    printer.setPaperSize(QPrinter::A4);
    printer.setOrientation(QPrinter::Portrait);
    QPrintPreviewDialog* printPreview = new QPrintPreviewDialog(
        &printer, this, Qt::WindowCloseButtonHint | Qt::WindowMaximizeButtonHint);
    if (printPreview != nullptr) {
        connect(printPreview, SIGNAL(paintRequested(QPrinter*)), this, SLOT(print(QPrinter*)));
        printPreview->exec();
        delete printPreview;
    }
}
//=============================================================================
void
QtTextEditor::print(QPrinter* p)
{
    currentEditor()->print(p);
}
//=============================================================================
void
QtTextEditor::evaluateSelection()
{
    QString selectedText = currentEditor()->textCursor().selectedText();
    selectedText = selectedText.trimmed();
    if (selectedText.startsWith('\'') && selectedText.endsWith('\'')) {
        selectedText.chop(1);
        selectedText.remove(0, 1);
    }
    if (!selectedText.isEmpty()) {
        std::wstring text = QStringTowstring(selectedText);
        executeCommand(text);
    }
}
//=============================================================================
void
QtTextEditor::dragEnterEvent(QDragEnterEvent* event)
{
    event->mimeData()->hasFormat("text/uri-list") ? event->accept() : event->ignore();
}
//=============================================================================
void
QtTextEditor::dropEvent(QDropEvent* event)
{
    if (event->mimeData()->hasFormat("text/uri-list")) {
        QList<QUrl> urls = event->mimeData()->urls();
        for (int k = 0; k < urls.size(); k++) {
            QFileInfo qmake(QString(urls[k].toLocalFile()));
            if (!urls.isEmpty()) {
                loadOrCreateFile(urls[k].toLocalFile());
            }
        }
        event->accept();
    } else {
        event->ignore();
    }
}
//=============================================================================
void
QtTextEditor::reloadFile(const QString filenameModified)
{
    for (int i = 0; i < tab->count(); i++) {
        QWidget* widget = tab->widget(i);
        QtEditPane* editPane = qobject_cast<QtEditPane*>(widget);
        if (editPane) {
            if (editPane->getFileName() == filenameModified) {
                if (lastFilenameSaved == filenameModified) {
                    filesModifiedMessageDisplayedList.removeAll(filenameModified);
                    return;
                }
                if (filesModifiedMessageDisplayedList.contains(filenameModified)) {
                    return;
                }
                filesModifiedMessageDisplayedList.append(filenameModified);
                if (QMessageBox::question(this, TR("Nelson"),
                        TR("File %1 was modified by an external software.\nDo you want to reopen "
                           "it?")
                            .arg(filenameModified),
                        QMessageBox::Yes | QMessageBox::Default, QMessageBox::No)
                    == QMessageBox::No) {
                    filesModifiedMessageDisplayedList.removeAll(filenameModified);
                    return;
                } else {
                    filesModifiedMessageDisplayedList.removeAll(filenameModified);
                    tab->setCurrentIndex(i);
                    closeTab();
                    loadFile(filenameModified);
                    return;
                }
            }
        }
    }
}
//=============================================================================
