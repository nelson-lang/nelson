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
#include "MainGuiObject.hpp"
#include "AddPathToEnvironmentVariable.hpp"
#include "Console.hpp"
#include "Evaluator.hpp"
#include "GetNelsonMainEvaluatorDynamicFunction.hpp"
#include "GetQtPath.hpp"
#include "GuiTerminal.hpp"
#include "Warning.hpp"
#include "Nelson_VERSION.h"
#include "QtMainWindow.h"
#include <QtCore/QtGlobal>
#include <QtWidgets/QApplication>
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
#if QT_VERSION > QT_VERSION_CHECK(5, 5, 0)
    case QtInfoMsg:
#endif
    case QtWarningMsg: {
        Warning(str);
    } break;
    case QtCriticalMsg:
    case QtFatalMsg: {
        Evaluator* eval = (Evaluator*)GetNelsonMainEvaluatorDynamicFunction();
        if (eval) {
            Interface* io = eval->getInterface();
            if (io) {
                io->errorMessage(str);
            }
        }
    } break;
    }
}
//===================================================================================
#if defined _MSC_VER || defined __APPLE__
static char* argv[] = { "Nelson", Q_NULLPTR };
static int argc = sizeof(argv) / sizeof(char*) - 1;
#else
// These variables must be global for linux ...
static char* argv[] = { "Nelson", NULL };
static int argc = 1;
#endif
//===================================================================================
void
InitGuiObjects(void)
{
    qInstallMessageHandler(QtMessageOutput);
    if (NelSonQtApp == nullptr) {
        NelSonQtApp = new QApplication(argc, argv);
        QCoreApplication::setApplicationName("Nelson");
        QCoreApplication::setOrganizationDomain(
            "https://nelson-numerical-software.github.io/nelson-website/");
        AddPathToEnvironmentVariable(std::wstring(L"PATH"), GetQtPath(L"BinariesPath"));
    }
}
//===================================================================================
void*
CreateGuiEvaluator(void* vcontext, NELSON_ENGINE_MODE _mode)
{
    CreateConsole();
    NelSonQtMainWindow = new QtMainWindow();
    if (NelSonQtMainWindow) {
        GuiTerminal* nlsTerm = new GuiTerminal((void*)NelSonQtMainWindow);
        if (nlsTerm) {
            Evaluator* mainEvaluator = new Evaluator((Context*)vcontext, nlsTerm, _mode);
            if (mainEvaluator) {
                mainEvaluator->mainGuiObject = (void*)NelSonQtMainWindow;
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
        if (NelSonQtMainWindow) {
            delete NelSonQtMainWindow;
            NelSonQtMainWindow = nullptr;
        }
        if (term) {
            GuiTerminal* nlsTerm = (GuiTerminal*)term;
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
