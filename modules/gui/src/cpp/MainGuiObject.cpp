//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
#include "Warning.hpp"
//===================================================================================
static QApplication* NelSonQtApp = nullptr;
static QtMainWindow* NelSonQtMainWindow = nullptr;
static bool messageVerbose = false;
//===================================================================================
void
QtMessageVerbose(bool bVerbose)
{
    messageVerbose = bVerbose;
}
//===================================================================================
bool
IsQtMessageVerbose()
{
    return messageVerbose;
}
//===================================================================================
static void
QtMessageOutput(QtMsgType type, const QMessageLogContext& context, const QString& msg)
{
    QByteArray localMsg = msg.toLocal8Bit();
    std::string str(localMsg);
    if (!messageVerbose) {
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
    qInstallMessageHandler(QtMessageOutput);
    if (NelSonQtApp == nullptr) {
        NelSonQtApp = new QApplication(argc, argv);
        createNelsonPalette();
        QCoreApplication::setApplicationName("Nelson");
        QCoreApplication::setOrganizationDomain("https://nelson-lang.github.io/nelson-website/");
        AddPathToEnvironmentVariable(std::wstring(L"PATH"), GetQtPath(L"BinariesPath"));
        configureDefaultFont();
    }
}
//===================================================================================
void*
CreateGuiEvaluator(void* vcontext, NELSON_ENGINE_MODE _mode, bool minimizeWindow, size_t ID)
{
    CreateConsole();
    try {
        NelSonQtMainWindow = new QtMainWindow(minimizeWindow);
    } catch (std::bad_alloc&) {
        NelSonQtMainWindow = nullptr;
    }

    if (NelSonQtMainWindow) {
        GuiTerminal* nlsTerm = nullptr;
        try {
            nlsTerm = new GuiTerminal((void*)NelSonQtMainWindow);
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
                NelSonQtMainWindow->createDockWigdets(mainEvaluator->getContext());
                NelsonConfiguration::getInstance()->setMainGuiObject((void*)NelSonQtMainWindow);
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
    if (NelSonQtApp) {
        if (NelSonQtMainWindow && !NelSonQtMainWindow->isClose()) {
            NelSonQtMainWindow->declareAsClosed();
            QApplication::sendEvent(NelSonQtMainWindow, new QCloseEvent());

            NelSonQtMainWindow->deleteLater();
            NelSonQtMainWindow = nullptr;
        }
        if (term) {
            auto* nlsTerm = static_cast<GuiTerminal*>(term);
            delete nlsTerm;
            nlsTerm = nullptr;
        }
        delete NelSonQtApp;
        NelSonQtApp = nullptr;
    }
    DestroyConsole();
}
//===================================================================================
void*
GetMainGuiObject(void)
{
    return (void*)NelSonQtMainWindow;
}
//===================================================================================
bool
QtSetLookAndFeel(const std::wstring& lf)
{
    bool res = false;
    QStyle* qStyle = QStyleFactory::create(wstringToQString(lf));
    if (qStyle) {
        if (NelSonQtApp) {
            NelSonQtApp->setStyleSheet("");
            NelSonQtApp->setStyle(qStyle);
            NelSonQtApp->setPalette(getNelsonPalette());
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
    if (NelSonQtApp) {
        QStyle* qtStyle = NelSonQtApp->style();
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
    if (NelSonQtApp) {
        styleSheet = QStringTowstring(NelSonQtApp->styleSheet());
    }
    return styleSheet;
}
//===================================================================================
void
QtSetStyleSheet(const std::wstring& styleSheet)
{
    if (NelSonQtApp) {
        NelSonQtApp->setStyleSheet(wstringToQString(styleSheet));
    }
}
//===================================================================================
