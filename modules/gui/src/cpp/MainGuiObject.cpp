//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QtGlobal>
#include <QtWidgets/QApplication>
#include <QtWidgets/QStyleFactory>
#include <QtWidgets/QStyle>
#include <QtGui/QPalette>
#include "MainGuiObject.hpp"
#include "AddPathToEnvironmentVariable.hpp"
#include "Console.hpp"
#include "GetQtPath.hpp"
#include "GuiTerminal.hpp"
#include "Warning.hpp"
#include "Nelson_VERSION.h"
#include "QtMainWindow.h"
#include "QStringConverter.hpp"
#include "NelsonPalette.hpp"
#include "DefaultFont.hpp"
#include "Evaluator.hpp"
#include "NelsonConfiguration.hpp"
#include "Error.hpp"
//===================================================================================
class MainGuiObjectSingleton
{
private:
    QApplication* nelSonQtApp = nullptr;
    QtMainWindow* nelSonQtMainWindow = nullptr;
    bool messageVerbose = false;

    MainGuiObjectSingleton() = default;

public:
    MainGuiObjectSingleton(const MainGuiObjectSingleton&) = delete;
    MainGuiObjectSingleton&
    operator=(const MainGuiObjectSingleton&)
        = delete;

    static MainGuiObjectSingleton&
    getInstance()
    {
        static MainGuiObjectSingleton instance;
        return instance;
    }

    QApplication*
    getQtApp() const
    {
        return nelSonQtApp;
    }
    void
    setQtApp(QApplication* app)
    {
        nelSonQtApp = app;
    }

    QtMainWindow*
    getMainWindow() const
    {
        return nelSonQtMainWindow;
    }
    void
    setMainWindow(QtMainWindow* window)
    {
        nelSonQtMainWindow = window;
    }

    bool
    isMessageVerbose() const
    {
        return messageVerbose;
    }
    void
    setMessageVerbose(bool verbose)
    {
        messageVerbose = verbose;
    }

    void
    reset()
    {
        nelSonQtApp = nullptr;
        nelSonQtMainWindow = nullptr;
        messageVerbose = false;
    }
};
//===================================================================================
void
QtMessageVerbose(bool bVerbose)
{
    MainGuiObjectSingleton::getInstance().setMessageVerbose(bVerbose);
}
//===================================================================================
bool
IsQtMessageVerbose()
{
    return MainGuiObjectSingleton::getInstance().isMessageVerbose();
}
//===================================================================================
static void
QtMessageOutput(QtMsgType type, const QMessageLogContext& context, const QString& msg)
{
    QByteArray localMsg = msg.toLocal8Bit();
    std::string str(localMsg);
    if (!MainGuiObjectSingleton::getInstance().isMessageVerbose()) {
        return;
    }
    switch (type) {
    case QtDebugMsg: {
        Warning(str);
    } break;
    case QtInfoMsg:
    case QtWarningMsg: {
        Warning(str);
    } break;
    case QtCriticalMsg:
    case QtFatalMsg: {
        Error(str);
    } break;
    }
}
//===================================================================================
static char execName[7] = "Nelson";
static char* argv[] = { execName, nullptr };
static int argc = 1;
//===================================================================================
void
InitGuiObjects(void)
{
    auto& manager = MainGuiObjectSingleton::getInstance();
    if (manager.getQtApp() != nullptr) {
        return;
    }
    qInstallMessageHandler(QtMessageOutput);

    QApplication* qtApp = nullptr;
    try {
        qtApp = new QApplication(argc, argv);
    } catch (const std::exception&) {
        qtApp = nullptr;
        return;
    } catch (...) {
        qtApp = nullptr;
        return;
    }
    if (!qtApp) {
        return;
    }
    manager.setQtApp(qtApp);
    createNelsonPalette();
    QCoreApplication::setApplicationName("Nelson");
    QCoreApplication::setOrganizationDomain("https://nelson-lang.github.io/nelson-website/");
    AddPathToEnvironmentVariable(std::wstring(L"PATH"), GetQtPath(L"BinariesPath"));
    configureDefaultFont();
}
//===================================================================================
void*
CreateGuiEvaluator(void* vcontext, NELSON_ENGINE_MODE _mode, bool minimizeWindow, size_t ID)
{
    auto& manager = MainGuiObjectSingleton::getInstance();
    CreateConsole();
    QtMainWindow* mainWindow = nullptr;
    try {
        mainWindow = new QtMainWindow(minimizeWindow);
    } catch (std::bad_alloc&) {
        mainWindow = nullptr;
    }

    if (mainWindow) {
        manager.setMainWindow(mainWindow);
        GuiTerminal* nlsTerm = nullptr;
        try {
            nlsTerm = new GuiTerminal((void*)mainWindow);
        } catch (std::bad_alloc&) {
            nlsTerm = nullptr;
        }
        if (nlsTerm) {
            Evaluator* mainEvaluator = nullptr;
            try {
                mainEvaluator = new Evaluator(static_cast<Context*>(vcontext), nlsTerm, true, ID);
            } catch (std::bad_alloc&) {
                mainEvaluator = nullptr;
            }
            if (mainEvaluator) {
                mainWindow->createDockWigdets(mainEvaluator);
                NelsonConfiguration::getInstance()->setMainGuiObject((void*)mainWindow);
            }
            return (void*)mainEvaluator;
        }
    }
    return nullptr;
}
//===================================================================================
void
DestroyMainGuiObject(void* term)
{
    if (!NelsonConfiguration::getInstance()->isClosing()) {
        NelsonConfiguration::getInstance()->setAsClosing(true);

        auto& manager = MainGuiObjectSingleton::getInstance();
        QApplication::closeAllWindows();
        if (term) {
            auto* nlsTerm = static_cast<GuiTerminal*>(term);
            delete nlsTerm;
            nlsTerm = nullptr;
        }
        delete manager.getQtApp();
        manager.reset();
    }
    DestroyConsole();
}
//===================================================================================
void*
GetMainGuiObject(void)
{
    return (void*)MainGuiObjectSingleton::getInstance().getMainWindow();
}
//===================================================================================
bool
QtSetLookAndFeel(const std::wstring& lf)
{
    bool res = false;
    QStyle* qStyle = QStyleFactory::create(wstringToQString(lf));
    if (qStyle) {
        QApplication* qtApp = MainGuiObjectSingleton::getInstance().getQtApp();
        if (qtApp) {
            qtApp->setStyleSheet("");
            qtApp->setStyle(qStyle);
            qtApp->setPalette(getNelsonPalette());
            res = true;
        }
    }
    return res;
}
//===================================================================================
std::wstring
QtGetLookAndFeel()
{
    std::wstring lf;
    QApplication* qtApp = MainGuiObjectSingleton::getInstance().getQtApp();
    if (qtApp) {
        QStyle* qtStyle = qtApp->style();
        if (qtStyle) {
            lf = QStringTowstring(qtStyle->objectName());
        }
    }
    return lf;
}
//===================================================================================
std::wstring
QtGetStyleSheet()
{
    std::wstring styleSheet;
    QApplication* qtApp = MainGuiObjectSingleton::getInstance().getQtApp();
    if (qtApp) {
        styleSheet = QStringTowstring(qtApp->styleSheet());
    }
    return styleSheet;
}
//===================================================================================
void
QtSetStyleSheet(const std::wstring& styleSheet)
{
    QApplication* qtApp = MainGuiObjectSingleton::getInstance().getQtApp();
    if (qtApp) {
        qtApp->setStyleSheet(wstringToQString(styleSheet));
    }
}
//===================================================================================
