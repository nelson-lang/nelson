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
#include <QtCore/QFileInfo>
#include <QtCore/QMetaObject>
#include <QtCore/QThread>
#include <QtCore/QTimer>
#include <QtCore/QEventLoop>
#include <QtWidgets/QApplication>
#include <QtWidgets/QStyleFactory>
#include <QtWidgets/QStyle>
#include <QtGui/QGuiApplication>
#include <QtGui/QPalette>
#include <QtGui/QSurfaceFormat>
#include "MainGuiObject.hpp"
#include "NelsonApplication.hpp"
#include "AddPathToEnvironmentVariable.hpp"
#include "Console.hpp"
#include "GetQtPath.hpp"
#include "GuiTerminal.hpp"
#include "Warning.hpp"
#include "Nelson_VERSION.h"
#include "QtMainWindow.h"
#include "RuntimeDispatch.hpp"
#include "ProcessEvents.hpp"
#include "QStringConverter.hpp"
#include "NelsonPalette.hpp"
#include "DefaultFont.hpp"
#include "Evaluator.hpp"
#include "NelsonConfiguration.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "FilesAssociation.hpp"
#include "ScreenInfo.hpp"
//=============================================================================
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
//=============================================================================
void
QtMessageVerbose(bool bVerbose)
{
    MainGuiObjectSingleton::getInstance().setMessageVerbose(bVerbose);
}
//=============================================================================
bool
IsQtMessageVerbose()
{
    return MainGuiObjectSingleton::getInstance().isMessageVerbose();
}
//=============================================================================
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
        RaiseWarning("Nelson:gui:qtDebug", str);
    } break;
    case QtInfoMsg:
    case QtWarningMsg: {
        RaiseWarning("Nelson:gui:qtWarning", str);
    } break;
    case QtCriticalMsg:
    case QtFatalMsg: {
        Error("Nelson:gui:initializationFailed", str);
    } break;
    }
}
//=============================================================================
static char execName[1024] = "Nelson";
static char* argv[] = { execName, nullptr };
static int argc = 1;
//=============================================================================
static void
configureOpenGLDefaults()
{
    static bool configured = false;
    if (configured) {
        return;
    }
    configured = true;
    QCoreApplication::setAttribute(Qt::AA_ShareOpenGLContexts);
#if defined(Q_OS_MACOS)
    QCoreApplication::setAttribute(Qt::AA_DontUseNativeMenuBar);
#endif

    QSurfaceFormat format;
    format.setRenderableType(QSurfaceFormat::OpenGL);
    format.setVersion(2, 1);
    format.setProfile(QSurfaceFormat::CompatibilityProfile);
    format.setDepthBufferSize(24);
    format.setStencilBufferSize(8);
#if defined(Q_OS_MACOS)
    format.setSamples(4);
#else
    format.setSamples(0);
#endif
    // The default format also drives the context that composes GL-backed
    // windows (figure windows host a QOpenGLWidget). A vsync-locked swap there
    // blocks every synchronously presented drawnow frame for up to a display
    // period; the desktop compositor keeps the output tear-free without it.
    format.setSwapInterval(0);
    QSurfaceFormat::setDefaultFormat(format);
}
//=============================================================================
static bool
isCurrentQtGuiThread()
{
    QCoreApplication* qtApp = QCoreApplication::instance();
    return qtApp != nullptr && QThread::currentThread() == qtApp->thread();
}
//=============================================================================
static void
registerQtRuntimeDispatchBackend()
{
    Nelson::RuntimeDispatchBackend backend;
    backend.postToEvaluator = [](Nelson::RuntimeDispatchTask task) {
        if (task) {
            task();
        }
    };
    backend.postToGui = [](Nelson::RuntimeDispatchTask task) {
        QCoreApplication* qtApp = QCoreApplication::instance();
        if (!task) {
            return;
        }
        if (qtApp == nullptr || isCurrentQtGuiThread()) {
            task();
            return;
        }
        QTimer::singleShot(0, qtApp, [task = std::move(task)]() mutable { task(); });
    };
    backend.callGuiSync = [](Nelson::RuntimeDispatchTask task) {
        QCoreApplication* qtApp = QCoreApplication::instance();
        if (!task) {
            return;
        }
        if (qtApp == nullptr || isCurrentQtGuiThread()) {
            task();
            return;
        }
        QMetaObject::invokeMethod(
            qtApp, [task = std::move(task)]() mutable { task(); }, Qt::BlockingQueuedConnection);
    };
    backend.notifyInterrupt = [](size_t) { };
    backend.processGuiEvents = [](bool waitEvents) { Nelson::ProcessEvents(waitEvents); };
#ifndef __APPLE__
    backend.pumpGuiEventsBlocking = [](int maxTimeMs) {
        // Run the event loop for a bounded slice: it services cross-thread
        // blocking calls, timers and input as they arrive and returns after
        // maxTimeMs so the main loop re-checks its exit flag. A sleep(1ms) poll
        // is limited by the OS timer tick (~15.6 ms on Windows). A single short
        // processEvents() slice stops servicing between the bursts of
        // cross-thread calls that a drawnow makes.
        if (QCoreApplication::instance() == nullptr) {
            return;
        }
        QEventLoop loop;
        QTimer::singleShot(maxTimeMs, &loop, &QEventLoop::quit);
        loop.exec();
    };
#else
    // A nested Qt event loop enters NSApplication::run when a window is open,
    // where the slice timer cannot reliably unwind it. The engine uses its
    // non-blocking event pump on this platform instead.
#endif
    backend.hasQtRuntime = []() { return QApplication::instance() != nullptr; };
    backend.hasGuiServices = []() { return QApplication::instance() != nullptr; };
    backend.hasGraphicsServices = []() { return QApplication::instance() != nullptr; };
    backend.hasQmlServices = []() { return QApplication::instance() != nullptr; };
    backend.isGuiThread = []() { return isCurrentQtGuiThread(); };
    backend.isEvaluatorThread = []() { return !isCurrentQtGuiThread(); };
    Nelson::registerRuntimeDispatchBackend(backend);
}
//=============================================================================
void
InitGuiObjects(void)
{
    auto& manager = MainGuiObjectSingleton::getInstance();
    if (manager.getQtApp() != nullptr) {
        return;
    }
    qInstallMessageHandler(QtMessageOutput);
    configureOpenGLDefaults();
    AddPathToEnvironmentVariable(std::wstring(L"PATH"), GetQtPath(L"BinariesPath"));

    // Must be set before the QApplication is instantiated, otherwise Qt rounds the
    // scale factor and the whole UI is oversized at 125% / 150% (Windows).
    QGuiApplication::setHighDpiScaleFactorRoundingPolicy(
        Qt::HighDpiScaleFactorRoundingPolicy::PassThrough);
    // Qt reads the screen factors while the application is constructed.
    const bool disconnectedDisplayScaled = applyDisconnectedDisplayScale();

    NelsonApplication* qtApp = nullptr;
    try {
        qtApp = new NelsonApplication(argc, argv);
    } catch (const std::exception&) {
        qtApp = nullptr;
    } catch (...) {
        qtApp = nullptr;
    }
    restoreDisconnectedDisplayScale(disconnectedDisplayScaled);
    if (!qtApp) {
        return;
    }
    manager.setQtApp(qtApp);
    qtApp->setFileOpenHandler([](const std::wstring& filename) {
        const QString extension = QFileInfo(QString::fromStdWString(filename)).suffix().toLower();
        if (extension != QStringLiteral("m") && extension != QStringLiteral("nh5")
            && extension != QStringLiteral("mat") && extension != QStringLiteral("nflow")) {
            return true;
        }
        if (NelsonConfiguration::getInstance()->getMainEvaluator() == nullptr) {
            return false;
        }
        // Opening a file in the editor only makes sense for the interactive
        // desktop. In other modes (ADVANCED_TERMINAL, used by the standalone
        // compiler) a macOS QFileOpenEvent for a command-line file must not become
        // an 'edit(...)' call: it fails during startup, before the text editor
        // module is available.
        const auto engineMode
            = (NELSON_ENGINE_MODE)NelsonConfiguration::getInstance()->getNelsonEngineMode();
        return Nelson::OpenAssociatedFiles(engineMode, { filename }, false);
    });
    createNelsonPalette();
    QCoreApplication::setApplicationName("Nelson");
    QCoreApplication::setOrganizationDomain("https://nelson-lang.github.io/nelson-website/");
    configureDefaultFont();
    registerQtRuntimeDispatchBackend();
}
//=============================================================================
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
                mainWindow->createDockWidgets(mainEvaluator);
                NelsonConfiguration::getInstance()->setMainGuiObject((void*)mainWindow);
            }
            return (void*)mainEvaluator;
        }
    }
    return nullptr;
}
//=============================================================================
void
DestroyMainGuiObject(void* term)
{
    if (!NelsonConfiguration::getInstance()->isClosing()) {
        NelsonConfiguration::getInstance()->setAsClosing(true);

        auto& manager = MainGuiObjectSingleton::getInstance();
        QApplication::closeAllWindows();
        QApplication::sendPostedEvents(nullptr, QEvent::DeferredDelete);
        QApplication::processEvents(QEventLoop::AllEvents, 50);
        bool deleteQtApplication = term != nullptr;
        if (term) {
            auto* nlsTerm = static_cast<GuiTerminal*>(term);
            delete nlsTerm;
            nlsTerm = nullptr;
        }
        QApplication::sendPostedEvents(nullptr, QEvent::DeferredDelete);
        QApplication::processEvents(QEventLoop::AllEvents, 50);
        if (deleteQtApplication) {
            delete manager.getQtApp();
        }
        manager.reset();
        Nelson::resetRuntimeDispatchBackend();
    }
    DestroyConsole();
}
//=============================================================================
void*
GetMainGuiObject(void)
{
    return (void*)MainGuiObjectSingleton::getInstance().getMainWindow();
}
//=============================================================================
bool
QtSetLookAndFeel(const std::wstring& lf)
{
    bool res = false;
    QStyle* qStyle = QStyleFactory::create(wstringToQString(lf));
    if (qStyle) {
        QApplication* qtApp = MainGuiObjectSingleton::getInstance().getQtApp();
        if (qtApp) {
            qtApp->setStyle(qStyle);
            // changing the QStyle resets the style sheet: the current (dark/light)
            // theme must be applied again, otherwise lookandfeel() silently destroys it.
            refreshNelsonTheme();
            res = true;
        }
    }
    return res;
}
//=============================================================================
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
//=============================================================================
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
//=============================================================================
void
QtSetStyleSheet(const std::wstring& styleSheet)
{
    QApplication* qtApp = MainGuiObjectSingleton::getInstance().getQtApp();
    if (qtApp) {
        qtApp->setStyleSheet(wstringToQString(styleSheet));
    }
}
//=============================================================================
