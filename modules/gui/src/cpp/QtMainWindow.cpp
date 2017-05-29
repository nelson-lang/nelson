//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QApplication>
#include <QtWidgets/QAction>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QMenuBar>
#include <QtWidgets/QMenu>
#include <QtWidgets/QFileDialog>
#include <QtGui/QClipboard>
#include <QtGui/QCloseEvent>
#include <QtGui/QDesktopServices>
#include "QtMainWindow.h"
#include "QStringConverter.hpp"
#include "characters_encoding.hpp"
#include "QtTerminal.h"
#include "GetNelsonPath.hpp"
#include "QtTranslation.hpp"
#include "GetNelsonMainEvaluatorDynamicFunction.hpp"
#include "Evaluator.hpp"
#include "Nelson_VERSION.h"
#include "UiGetDirectory.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
QtMainWindow::~QtMainWindow()
{
    if (runAct)
    {
        delete runAct;
        runAct = nullptr;
    }
    if (pwdAct)
    {
        delete pwdAct;
        pwdAct = nullptr;
    }
    if (exitAct)
    {
        delete exitAct;
        exitAct = nullptr;
    }
    if (aboutAct)
    {
        delete aboutAct;
        aboutAct = nullptr;
    }
    if (helpAct)
    {
        delete helpAct;
        helpAct = nullptr;
    }
    if (cutAct)
    {
        delete cutAct;
        cutAct = nullptr;
    }
    if (copyAct)
    {
        delete copyAct;
        copyAct = nullptr;
    }
    if (pasteAct)
    {
        delete pasteAct;
        pasteAct = nullptr;
    }
    if (selectAllAct)
    {
        delete selectAllAct;
        selectAllAct = nullptr;
    }
    if (emptyClipboardAct)
    {
        delete emptyClipboardAct;
        emptyClipboardAct = nullptr;
    }
    if (clearConsoleAct)
    {
        delete clearConsoleAct;
        clearConsoleAct = nullptr;
    }
    if (editMenu)
    {
        delete editMenu;
        editMenu = nullptr;
    }
    if (fileMenu)
    {
        delete fileMenu;
        fileMenu = nullptr;
    }
    if (helpMenu)
    {
        delete helpMenu;
        helpMenu = nullptr;
    }
    if (mainMenuBar)
    {
        delete mainMenuBar;
        mainMenuBar = nullptr;
    }
	if (qtTerminal)
	{
		delete qtTerminal;
		qtTerminal = nullptr;
	}
}
//=============================================================================
QtMainWindow::QtMainWindow()
{
    QWidget *widget = new QWidget;
    setCentralWidget(widget);
    QWidget *topFiller = new QWidget;
    topFiller->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    QWidget *bottomFiller = new QWidget;
    bottomFiller->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    QVBoxLayout *layout = new QVBoxLayout;
    layout->setMargin(5);
    layout->addWidget(topFiller);
    layout->addWidget(bottomFiller);
    layout->setMenuBar(this->menuBar());
    widget->setLayout(layout);
    createMenus();
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
#if defined  __APPLE__ || defined _MSC_VER
    QString nelsonPath = Nelson::wstringToQString(Nelson::GetRootPath());
    QString fileNameIcon = nelsonPath + "/resources/fibonacci.png";
    QIcon icon(fileNameIcon);
    setWindowIcon(icon);
#endif
}
//=============================================================================
void QtMainWindow::runFile()
{
    if (qtTerminal)
    {
        if (!qtTerminal->isAtPrompt())
        {
            QMessageBox::warning(this, _("Execute...").c_str(), _("Interpreter currently runs.").c_str());
        }
        else
        {
            QString qfileName = QFileDialog::getOpenFileName(this, TR("Execute..."),
                                QDir::currentPath(),
                                TR("Nelson (*.nls)"));
            if (!qfileName.isEmpty())
            {
                std::wstring filename = Nelson::QStringTowstring(qfileName);
                void *veval = GetNelsonMainEvaluatorDynamicFunction();
                Nelson::Evaluator *eval = (Nelson::Evaluator *)veval;
                qtTerminal->outputMessage(L"\n");
                qtTerminal->sendReturnKey();
                std::wstring cmd = L"run('" + filename + L"');";
                eval->addCommandToQueue(cmd, true);
            }
        }
    }
}
//=============================================================================
void QtMainWindow::about()
{
    std::string version = std::string(NELSON_PRODUCT_NAME) + " " + std::string(NELSON_VERSION_STRING);
    std::string aboutText = version + "\n" + "Copyright 2016-2017 Allan CORNET";
    QMessageBox::about(this, _("About Nelson...").c_str(), aboutText.c_str());
}
//=============================================================================
void QtMainWindow::website()
{
    QString link = "https://nelson-numerical-software.github.io/nelson-website/";
    QDesktopServices::openUrl(QUrl(link));
}
//=============================================================================
void QtMainWindow::bugAndRequest()
{
    QString link = "https://github.com/Nelson-numerical-software/nelson/issues";
    QDesktopServices::openUrl(QUrl(link));
}
//=============================================================================
void QtMainWindow::executeCommand(std::wstring cmd)
{
    if (qtTerminal)
    {
        std::wstring _cmd = cmd + L";";
        void *veval = GetNelsonMainEvaluatorDynamicFunction();
        if (veval != nullptr)
        {
            Nelson::Evaluator *eval = (Nelson::Evaluator *)veval;
            if (qtTerminal->isAtPrompt())
            {
                eval->addCommandToQueue(cmd, true);
            }
            else
            {
                std::string ustr = wstring_to_utf8(_cmd);
                eval->evaluateString(ustr + "\n");
            }
        }
    }
}
//=============================================================================
void QtMainWindow::help()
{
    executeCommand(L"doc");
}
//=============================================================================
void QtMainWindow::cutText()
{
    if (qtTerminal)
    {
        qtTerminal->cut();
    }
}
//=============================================================================
void QtMainWindow::copyText()
{
    if (qtTerminal)
    {
        qtTerminal->copy();
    }
}
//=============================================================================
void QtMainWindow::pasteText()
{
    if (qtTerminal)
    {
        qtTerminal->paste();
    }
}
//=============================================================================
void QtMainWindow::selectAllText()
{
    if (qtTerminal)
    {
        qtTerminal->selectAll();
    }
}
//=============================================================================
void QtMainWindow::emptyClipboard()
{
    QClipboard *Clipboard = QApplication::clipboard();
    Clipboard->clear(QClipboard::Clipboard);
}
//=============================================================================
void QtMainWindow::clearConsole()
{
    if (qtTerminal)
    {
        qtTerminal->clearTerminal();
    }
}
//=============================================================================
void QtMainWindow::pwdDisplay()
{
    executeCommand(L"disp(pwd())");
}
//=============================================================================
void QtMainWindow::changeDir()
{
    std::wstring pathSelected;
    bool bCanceled = UiGetDirectory(L"", L"", pathSelected);
    if (!bCanceled)
    {
        QDir::setCurrent(wstringToQString(pathSelected));
    }
}
//=============================================================================
void QtMainWindow::createMenus()
{
    mainMenuBar = this->menuBar();
    mainMenuBar->setNativeMenuBar(false);
    fileMenu = mainMenuBar->addMenu(TR("&File"));
    // File menu
    runAct = new QAction(TR("&Execute..."), this);
    runAct->setShortcut(QKeySequence("Ctrl+E"));
    runAct->setStatusTip(TR("Execute a .nls file"));
    connect(runAct, SIGNAL(triggered()), this, SLOT(runFile()));
    fileMenu->addAction(runAct);
    // separator
    fileMenu->addSeparator();
    // chdir
    chdirAct = new QAction(TR("&Change current directory"), this);
    chdirAct->setStatusTip(TR("Change current directory"));
    connect(chdirAct, SIGNAL(triggered()), this, SLOT(changeDir()));
    fileMenu->addAction(chdirAct);
    // pwd
    pwdAct = new QAction(TR("&Display current directory"), this);
    pwdAct->setStatusTip(TR("Display current directory"));
    connect(pwdAct, SIGNAL(triggered()), this, SLOT(pwdDisplay()));
    fileMenu->addAction(pwdAct);
    // separator
    fileMenu->addSeparator();
    // exit
    exitAct = new QAction(TR("E&xit"), this);
    exitAct->setShortcuts(QKeySequence::Quit);
    exitAct->setStatusTip(TR("Exit the application"));
    connect(exitAct, SIGNAL(triggered()), this, SLOT(close()));
    fileMenu->addAction(exitAct);
    //
    // Edit menu
    editMenu = mainMenuBar->addMenu(TR("&Edit"));
    // cut
    cutAct = new QAction(TR("C&ut"), this);
    cutAct->setShortcuts(QKeySequence::Cut);
    cutAct->setStatusTip(TR("Cut"));
    connect(cutAct, SIGNAL(triggered()), this, SLOT(cutText()));
    editMenu->addAction(cutAct);
    // copy
    copyAct = new QAction(TR("&Copy"), this);
    copyAct->setShortcuts(QKeySequence::Copy);
    copyAct->setStatusTip(TR("Copy"));
    connect(copyAct, SIGNAL(triggered()), this, SLOT(copyText()));
    editMenu->addAction(copyAct);
    // paste
    pasteAct = new QAction(TR("&Paste"), this);
    pasteAct->setShortcuts(QKeySequence::Paste);
    pasteAct->setStatusTip(TR("Paste"));
    connect(pasteAct, SIGNAL(triggered()), this, SLOT(pasteText()));
    editMenu->addAction(pasteAct);
    // separator
    editMenu->addSeparator();
    // select all
    selectAllAct = new QAction(TR("&Select all"), this);
    selectAllAct->setStatusTip(TR("Select all"));
    connect(selectAllAct, SIGNAL(triggered()), this, SLOT(selectAllText()));
    editMenu->addAction(selectAllAct);
    // separator
    editMenu->addSeparator();
    // empty clipboard
    emptyClipboardAct = new QAction(TR("&Empty clipboard"), this);
    emptyClipboardAct->setStatusTip(TR("Empty clipboard"));
    connect(emptyClipboardAct, SIGNAL(triggered()), this, SLOT(emptyClipboard()));
    editMenu->addAction(emptyClipboardAct);
    // separator
    editMenu->addSeparator();
    // clear console
    clearConsoleAct = new QAction(TR("Clear c&onsole"), this);
    clearConsoleAct->setStatusTip(TR("Clear console"));
    connect(clearConsoleAct, SIGNAL(triggered()), this, SLOT(clearConsole()));
    editMenu->addAction(clearConsoleAct);
    //
    // Help menu
    helpMenu = mainMenuBar->addMenu(TR("&Help"));
    // documentation
    helpAct = new QAction(TR("&Documentation"), this);
    helpAct->setShortcuts(QKeySequence::HelpContents);
    helpAct->setStatusTip(TR("Documentation"));
    connect(helpAct, SIGNAL(triggered()), this, SLOT(help()));
    helpMenu->addAction(helpAct);
    // website
    webAct = new QAction(TR("Nelson &website"), this);
    webAct->setStatusTip(TR("Nelson website"));
    connect(webAct, SIGNAL(triggered()), this, SLOT(website()));
    helpMenu->addAction(webAct);
    // bugs and requests
    bugAct = new QAction(TR("B&ugs and Requests"), this);
    bugAct->setStatusTip(TR("Bugs and Requests"));
    connect(bugAct, SIGNAL(triggered()), this, SLOT(bugAndRequest()));
    helpMenu->addAction(bugAct);
    // about
    aboutAct = new QAction(TR("&About"), this);
    aboutAct->setStatusTip(TR("About"));
    connect(aboutAct, SIGNAL(triggered()), this, SLOT(about()));
    helpMenu->addAction(aboutAct);
}
//=============================================================================
void QtMainWindow::closeEvent(QCloseEvent *event)
{
    if (!qtTerminal->isAtPrompt())
    {
        if (!bClosed)
        {
            event->ignore();
            QMessageBox::StandardButton	reply = QMessageBox::question(this, _("Close confirmation").c_str(), _("Are you sure to quit?").c_str(), QMessageBox::Yes | QMessageBox::No);
            if (reply == QMessageBox::Yes)
            {
                event->accept();
                QWidget * qwidgetConsole = this->focusProxy();
                QApplication::sendEvent(qwidgetConsole, new QCloseEvent());
                bClosed = true;
                return;
            }
            else
            {
                return;
            }
        }
    }
    event->accept();
    QWidget * qwidgetConsole = this->focusProxy();
    QApplication::sendEvent(qwidgetConsole, new QCloseEvent());
}
//=============================================================================
QtTerminal *QtMainWindow::getQtTerminal()
{
    return qtTerminal;
}
//=============================================================================
