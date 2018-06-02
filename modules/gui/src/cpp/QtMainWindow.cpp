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
#include "QtMainWindow.h"
#include "ExecuteCommand.hpp"
#include "GetNelsonPath.hpp"
#include "Nelson_VERSION.h"
#include "QStringConverter.hpp"
#include "QtTerminal.h"
#include "QtTranslation.hpp"
#include "UiGetDirectory.hpp"
#include "characters_encoding.hpp"
#include <QtCore/QMimeData>
#include <QtGui/QClipboard>
#include <QtGui/QCloseEvent>
#include <QtGui/QDesktopServices>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QMenu>
#include <QtWidgets/QMenuBar>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QVBoxLayout>
//=============================================================================
using namespace Nelson;
//=============================================================================
QtMainWindow::~QtMainWindow()
{
    if (runAction) {
        delete runAction;
        runAction = nullptr;
    }
    if (pwdAction) {
        delete pwdAction;
        pwdAction = nullptr;
    }
    if (exitAction) {
        delete exitAction;
        exitAction = nullptr;
    }
    if (aboutAction) {
        delete aboutAction;
        aboutAction = nullptr;
    }
    if (helpAction) {
        delete helpAction;
        helpAction = nullptr;
    }
    if (cutAction) {
        delete cutAction;
        cutAction = nullptr;
    }
    if (copyAction) {
        delete copyAction;
        copyAction = nullptr;
    }
    if (pasteAction) {
        delete pasteAction;
        pasteAction = nullptr;
    }
    if (selectAllAction) {
        delete selectAllAction;
        selectAllAction = nullptr;
    }
    if (emptyClipboardAction) {
        delete emptyClipboardAction;
        emptyClipboardAction = nullptr;
    }
    if (clearConsoleAction) {
        delete clearConsoleAction;
        clearConsoleAction = nullptr;
    }
    if (editMenu) {
        delete editMenu;
        editMenu = nullptr;
    }
    if (fileMenu) {
        delete fileMenu;
        fileMenu = nullptr;
    }
    if (helpMenu) {
        delete helpMenu;
        helpMenu = nullptr;
    }
    if (mainMenuBar) {
        delete mainMenuBar;
        mainMenuBar = nullptr;
    }
    if (qtTerminal) {
        delete qtTerminal;
        qtTerminal = nullptr;
    }
}
//=============================================================================
QtMainWindow::QtMainWindow()
{
    nelsonPath = Nelson::wstringToQString(Nelson::GetRootPath());
    QWidget* widget = new QWidget;
    setCentralWidget(widget);
    QWidget* topFiller = new QWidget;
    topFiller->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    QWidget* bottomFiller = new QWidget;
    bottomFiller->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    QVBoxLayout* layout = new QVBoxLayout;
    layout->setMargin(5);
    layout->addWidget(topFiller);
    layout->addWidget(bottomFiller);
    layout->setMenuBar(this->menuBar());
    widget->setLayout(layout);
    createMenus();
    createToolbars();
    setWindowTitle(TR("Nelson"));
    setMinimumSize(640, 480);
    resize(840, 600);
    qtTerminal = new QtTerminal(this);
    setFocusProxy(qtTerminal);
    setCentralWidget(qtTerminal);
    statusBar()->showMessage("");
    show();
    qtTerminal->show();
    bClosed = false;
#if defined __APPLE__ || defined _MSC_VER
    QString fileNameIcon = nelsonPath + "/resources/fibonacci.png";
    QIcon icon(fileNameIcon);
    setWindowIcon(icon);
#endif
    setAcceptDrops(true);
}
//=============================================================================
void
QtMainWindow::runFile()
{
    if (qtTerminal) {
        if (!qtTerminal->isAtPrompt()) {
            QMessageBox::warning(
                this, _("Execute...").c_str(), _("Interpreter currently runs.").c_str());
        } else {
            QString qfileName = QFileDialog::getOpenFileName(
                this, TR("Execute..."), QDir::currentPath(), TR("Nelson (*.nls)"));
            if (!qfileName.isEmpty()) {
                std::wstring filename = Nelson::QStringTowstring(qfileName);
                qtTerminal->outputMessage(L"\n");
                qtTerminal->sendReturnKey();
                std::wstring cmd = L"run('" + filename + L"')";
                executeCommand(cmd);
            }
        }
    }
}
//=============================================================================
void
QtMainWindow::about()
{
    std::string version
        = std::string(NELSON_PRODUCT_NAME) + " " + std::string(NELSON_VERSION_STRING);
    std::string aboutText = version + "\n" + "Copyright 2016-2018 Allan CORNET";
    QMessageBox::about(this, _("About Nelson...").c_str(), aboutText.c_str());
}
//=============================================================================
void
QtMainWindow::website()
{
    QString link = "https://nelson-numerical-software.github.io/nelson-website/";
    QDesktopServices::openUrl(QUrl(link));
}
//=============================================================================
void
QtMainWindow::bugAndRequest()
{
    QString link = "https://github.com/Nelson-numerical-software/nelson/issues";
    QDesktopServices::openUrl(QUrl(link));
}
//=============================================================================
void
QtMainWindow::help()
{
    Nelson::executeCommand(L"doc");
}
//=============================================================================
void
QtMainWindow::cutText()
{
    if (qtTerminal) {
        qtTerminal->cut();
    }
}
//=============================================================================
void
QtMainWindow::copyText()
{
    if (qtTerminal) {
        qtTerminal->copy();
    }
}
//=============================================================================
void
QtMainWindow::pasteText()
{
    if (qtTerminal) {
        qtTerminal->paste();
    }
}
//=============================================================================
void
QtMainWindow::selectAllText()
{
    if (qtTerminal) {
        qtTerminal->selectAll();
    }
}
//=============================================================================
void
QtMainWindow::emptyClipboard()
{
    QClipboard* Clipboard = QApplication::clipboard();
    Clipboard->clear(QClipboard::Clipboard);
}
//=============================================================================
void
QtMainWindow::clearConsole()
{
    if (qtTerminal) {
        qtTerminal->clearTerminal();
    }
}
//=============================================================================
void
QtMainWindow::pwdDisplay()
{
    executeCommand(L"disp(pwd())");
}
//=============================================================================
void
QtMainWindow::changeDir()
{
    std::wstring pathSelected;
    bool bCanceled = UiGetDirectory(L"", L"", pathSelected);
    if (!bCanceled) {
        QDir::setCurrent(wstringToQString(pathSelected));
    }
}
//=============================================================================
void
QtMainWindow::editor()
{
    executeCommand(L"edit()");
}
//=============================================================================
void
QtMainWindow::createToolbars()
{
    toolBar = addToolBar(TR("Text editor"));
    toolBar->addAction(editorAction);
    toolBar->addAction(runAction);
    toolBar->addAction(chdirAction);
    toolBar->addSeparator();
    toolBar->addAction(cutAction);
    toolBar->addAction(copyAction);
    toolBar->addAction(pasteAction);
    toolBar->addSeparator();
    toolBar->addAction(helpAction);
    toolBar->addSeparator();
}
//=============================================================================
void
QtMainWindow::createMenus()
{
    QString fileNameIcon;
    mainMenuBar = this->menuBar();
    mainMenuBar->setNativeMenuBar(false);
    fileMenu = mainMenuBar->addMenu(TR("&File"));
    // File menu
    fileNameIcon = nelsonPath + QString("/resources/file-run.svg");
    runAction = new QAction(QIcon(fileNameIcon), TR("&Execute..."), this);
    runAction->setShortcut(QKeySequence("Ctrl+E"));
    runAction->setStatusTip(TR("Execute a .nls file"));
    connect(runAction, SIGNAL(triggered()), this, SLOT(runFile()));
    fileMenu->addAction(runAction);
    // separator
    fileMenu->addSeparator();
    // chdir
    fileNameIcon = nelsonPath + QString("/resources/folder-open.svg");
    chdirAction = new QAction(QIcon(fileNameIcon), TR("&Change current directory"), this);
    chdirAction->setStatusTip(TR("Change current directory"));
    connect(chdirAction, SIGNAL(triggered()), this, SLOT(changeDir()));
    fileMenu->addAction(chdirAction);
    // pwd
    pwdAction = new QAction(TR("&Display current directory"), this);
    pwdAction->setStatusTip(TR("Display current directory"));
    connect(pwdAction, SIGNAL(triggered()), this, SLOT(pwdDisplay()));
    fileMenu->addAction(pwdAction);
    // separator
    fileMenu->addSeparator();
    // exit
    fileNameIcon = nelsonPath + QString("/resources/exit.svg");
    exitAction = new QAction(QIcon(fileNameIcon), TR("E&xit"), this);
    exitAction->setShortcuts(QKeySequence::Quit);
    exitAction->setStatusTip(TR("Exit the application"));
    connect(exitAction, SIGNAL(triggered()), this, SLOT(close()));
    fileMenu->addAction(exitAction);
    //
    // Edit menu
    editMenu = mainMenuBar->addMenu(TR("&Edit"));
    // cut
    fileNameIcon = nelsonPath + QString("/resources/edit-cut.svg");
    cutAction = new QAction(QIcon(fileNameIcon), TR("C&ut"), this);
    cutAction->setShortcuts(QKeySequence::Cut);
    cutAction->setStatusTip(TR("Cut"));
    connect(cutAction, SIGNAL(triggered()), this, SLOT(cutText()));
    editMenu->addAction(cutAction);
    // copy
    fileNameIcon = nelsonPath + QString("/resources/edit-copy.svg");
    copyAction = new QAction(QIcon(fileNameIcon), TR("&Copy"), this);
    copyAction->setShortcuts(QKeySequence::Copy);
    copyAction->setStatusTip(TR("Copy"));
    connect(copyAction, SIGNAL(triggered()), this, SLOT(copyText()));
    editMenu->addAction(copyAction);
    // paste
    fileNameIcon = nelsonPath + QString("/resources/edit-paste.svg");
    pasteAction = new QAction(QIcon(fileNameIcon), TR("&Paste"), this);
    pasteAction->setShortcuts(QKeySequence::Paste);
    pasteAction->setStatusTip(TR("Paste"));
    connect(pasteAction, SIGNAL(triggered()), this, SLOT(pasteText()));
    editMenu->addAction(pasteAction);
    // separator
    editMenu->addSeparator();
    // select all
    fileNameIcon = nelsonPath + QString("/resources/edit-select-all.svg");
    selectAllAction = new QAction(QIcon(fileNameIcon), TR("&Select all"), this);
    selectAllAction->setStatusTip(TR("Select all"));
    connect(selectAllAction, SIGNAL(triggered()), this, SLOT(selectAllText()));
    editMenu->addAction(selectAllAction);
    // separator
    editMenu->addSeparator();
    // empty clipboard
    fileNameIcon = nelsonPath + QString("/resources/clipboard-empty.svg");
    emptyClipboardAction = new QAction(QIcon(fileNameIcon), TR("&Empty clipboard"), this);
    emptyClipboardAction->setStatusTip(TR("Empty clipboard"));
    connect(emptyClipboardAction, SIGNAL(triggered()), this, SLOT(emptyClipboard()));
    editMenu->addAction(emptyClipboardAction);
    // separator
    editMenu->addSeparator();
    // clear console
    fileNameIcon = nelsonPath + QString("/resources/console-clear.svg");
    clearConsoleAction = new QAction(QIcon(fileNameIcon), TR("Clear c&onsole"), this);
    clearConsoleAction->setStatusTip(TR("Clear console"));
    connect(clearConsoleAction, SIGNAL(triggered()), this, SLOT(clearConsole()));
    editMenu->addAction(clearConsoleAction);
    //
    // Help menu
    helpMenu = mainMenuBar->addMenu(TR("&Help"));
    // documentation
    fileNameIcon = nelsonPath + QString("/resources/help-icon.svg");
    helpAction = new QAction(QIcon(fileNameIcon), TR("&Documentation"), this);
    helpAction->setShortcuts(QKeySequence::HelpContents);
    helpAction->setStatusTip(TR("Documentation"));
    connect(helpAction, SIGNAL(triggered()), this, SLOT(help()));
    helpMenu->addAction(helpAction);
    // website
    fileNameIcon = nelsonPath + QString("/resources/fibonacci.png");
    webAction = new QAction(QIcon(fileNameIcon), TR("Nelson &website"), this);
    webAction->setStatusTip(TR("Nelson website"));
    connect(webAction, SIGNAL(triggered()), this, SLOT(website()));
    helpMenu->addAction(webAction);
    // bugs and requests
    fileNameIcon = nelsonPath + QString("/resources/bug-icon.svg");
    bugAction = new QAction(QIcon(fileNameIcon), TR("B&ugs and Requests"), this);
    bugAction->setStatusTip(TR("Bugs and Requests"));
    connect(bugAction, SIGNAL(triggered()), this, SLOT(bugAndRequest()));
    helpMenu->addAction(bugAction);
    // about
    fileNameIcon = nelsonPath + QString("/resources/information-icon.svg");
    aboutAction = new QAction(QIcon(fileNameIcon), TR("&About"), this);
    aboutAction->setStatusTip(TR("About"));
    connect(aboutAction, SIGNAL(triggered()), this, SLOT(about()));
    helpMenu->addAction(aboutAction);
    // editor
    fileNameIcon = nelsonPath + QString("/resources/textedit-icon.svg");
    editorAction = new QAction(QIcon(fileNameIcon), TR("&Text editor"), this);
    editorAction->setStatusTip(TR("Text editor"));
    connect(editorAction, SIGNAL(triggered()), this, SLOT(editor()));
}
//=============================================================================
void
QtMainWindow::closeEvent(QCloseEvent* event)
{
    if (!qtTerminal->isAtPrompt()) {
        if (!bClosed) {
            event->ignore();
            QMessageBox::StandardButton reply
                = QMessageBox::question(this, _("Close confirmation").c_str(),
                    _("Are you sure to quit?").c_str(), QMessageBox::Yes | QMessageBox::No);
            if (reply == QMessageBox::Yes) {
                event->accept();
                QWidget* qwidgetConsole = this->focusProxy();
                QApplication::sendEvent(qwidgetConsole, new QCloseEvent());
                bClosed = true;
                return;
            } else {
                return;
            }
        }
    }
    event->accept();
    QWidget* qwidgetConsole = this->focusProxy();
    QApplication::sendEvent(qwidgetConsole, new QCloseEvent());
}
//=============================================================================
QtTerminal*
QtMainWindow::getQtTerminal()
{
    return qtTerminal;
}
//=============================================================================
void
QtMainWindow::dragEnterEvent(QDragEnterEvent* event)
{
    event->mimeData()->hasFormat("text/uri-list") ? event->accept() : event->ignore();
}
//=============================================================================
void
QtMainWindow::dropEvent(QDropEvent* event)
{
    if (event->mimeData()->hasFormat("text/uri-list")) {
        QList<QUrl> urls = event->mimeData()->urls();
        for (int k = 0; k < urls.size(); k++) {
            QFileInfo qmake(QString(urls[k].toLocalFile()));
            if (!urls.isEmpty()) {
                if (qmake.suffix() == "nls") {
                    if (!qtTerminal->isAtPrompt()) {
                        QMessageBox::warning(this, _("Execute...").c_str(),
                            _("Interpreter currently runs.").c_str());
                    } else {
                        std::wstring filename = Nelson::QStringTowstring(urls[k].toLocalFile());
                        qtTerminal->outputMessage(L"\n");
                        qtTerminal->sendReturnKey();
                        std::wstring cmd = L"run('" + filename + L"')";
                        executeCommand(cmd);
                    }
                } else if (qmake.suffix() == "nlf") {
                    executeCommand(
                        L"edit('" + Nelson::QStringTowstring(urls[k].toLocalFile()) + L"')");
                }
            }
        }
        event->accept();
    } else {
        event->ignore();
    }
}
//=============================================================================
