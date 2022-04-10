//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
#include "Evaluator.hpp"
#include "GetNelsonMainEvaluatorDynamicFunction.hpp"
#include "GetQtPath.hpp"
#include "GuiTerminal.hpp"
#include "Warning.hpp"
#include "Nelson_VERSION.h"
#include "QtMainWindow.h"
#include "QStringConverter.hpp"
#include "GetNelsonPath.hpp"
#include "NelsonPalette.hpp"
#include "DefaultFont.hpp"
//===================================================================================
static QApplication* NelSonQtApp = nullptr;
static QtMainWindow* NelSonQtMainWindow = nullptr;
static QStyle* defaultStyle = nullptr;
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
        auto* eval = static_cast<Evaluator*>(GetNelsonMainEvaluatorDynamicFunction());
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
static char* argv[] = { "Nelson", nullptr };
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
        QCoreApplication::setOrganizationDomain(
            "https://nelson-numerical-software.github.io/nelson-website/");
        AddPathToEnvironmentVariable(std::wstring(L"PATH"), GetQtPath(L"BinariesPath"));
        configureDefaultFont();
    }
}
//===================================================================================
void*
CreateGuiEvaluator(void* vcontext, NELSON_ENGINE_MODE _mode, bool minimizeWindow)
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
                mainEvaluator = new Evaluator(static_cast<Context*>(vcontext), nlsTerm, _mode);
            } catch (std::bad_alloc&) {
                mainEvaluator = nullptr;
            }
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
        NelSonQtApp->deleteLater();
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
