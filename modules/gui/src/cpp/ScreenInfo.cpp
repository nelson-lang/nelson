//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ScreenInfo.hpp"
#include <QtGui/QGuiApplication>
#include <QtGui/QScreen>
#include <QtWidgets/QApplication>
#include <QtWidgets/QWidget>
#include <QtCore/QEventLoop>
#include <QtCore/QPointer>
#include <QtGui/QCursor>
#include <QtGui/QFontDatabase>
#include "QStringConverter.hpp"
#include <cmath>
#if defined(__linux__)
#include <dlfcn.h>
#endif
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
static const char* const SCREEN_SCALE_FACTORS_ENV = "QT_SCREEN_SCALE_FACTORS";
// The placeholder display has no "\\.\" device prefix; Qt names it the same.
static const wchar_t* const DISCONNECTED_DISPLAY_DEVICE = L"WinDisc";
static const char* const DISCONNECTED_DISPLAY_QT_NAME = "WinDisc";
//=============================================================================
static double
nativeMonitorDpi(void* monitor);
static bool
nativeLogicalScreen(int& width, int& height, double& scale);
//=============================================================================
// ScreenSize and the bottom-up Position origin refer to the primary display,
// never to the screen of whichever window happens to be active. Without a Qt
// screen (web desktop) the operating system answers, in the same units.
bool
currentScreenResolution(int& width, int& height)
{
    QScreen* qScreen = QGuiApplication::primaryScreen();
    if (qScreen) {
        QRect screenSize = qScreen->geometry();
        if (screenSize.width() > 0 && screenSize.height() > 0) {
            width = screenSize.width();
            height = screenSize.height();
            return true;
        }
    }
    double scale = 1.0;
    if (nativeLogicalScreen(width, height, scale) && width > 0 && height > 0) {
        return true;
    }
    // A primary screen with empty geometry (headless or remote sessions expose a placeholder)
    // would give a -1 x -1 ScreenSize. Fall back to a default so screen-derived values stay
    // consistent.
    width = 1920;
    height = 1080;
    return true;
}
//=============================================================================
int
getCurrentScreenHeight()
{
    int w = 0;
    int h = 0;
    currentScreenResolution(w, h);
    return h;
}
//=============================================================================
int
getCurrentScreenWidth()
{
    int w = 0;
    int h = 0;
    currentScreenResolution(w, h);
    return w;
}
//=============================================================================
double
primaryScreenDpi()
{
    QScreen* mainScreen = QGuiApplication::primaryScreen();
    return mainScreen ? mainScreen->logicalDotsPerInch() : 96.0;
}
//=============================================================================
int
primaryScreenDepth()
{
    QScreen* mainScreen = QGuiApplication::primaryScreen();
    return mainScreen ? mainScreen->depth() : 32;
}
//=============================================================================
double
primaryScreenDevicePixelRatio()
{
    QScreen* mainScreen = QGuiApplication::primaryScreen();
    if (mainScreen) {
        return mainScreen->devicePixelRatio();
    }
    int width = 0;
    int height = 0;
    double scale = 1.0;
    return nativeLogicalScreen(width, height, scale) ? scale : 1.0;
}
//=============================================================================
void
pointerScreenPosition(int& x, int& y)
{
    QPoint point = QCursor::pos();
    x = point.x();
    y = point.y();
}
//=============================================================================
void
setPointerScreenPosition(int x, int y)
{
    QCursor::setPos(x, y);
}
//=============================================================================
std::wstring
defaultSansSubstituteFontFamily()
{
    if (qobject_cast<QGuiApplication*>(QCoreApplication::instance()) == nullptr) {
        return L"Helvetica";
    }
    static std::wstring substitute;
    if (substitute.empty()) {
        substitute = L"Helvetica";
        const QStringList families = QFontDatabase::families();
        for (const auto& candidate :
            { QStringLiteral("Liberation Sans"), QStringLiteral("Arial") }) {
            if (families.contains(candidate, Qt::CaseInsensitive)) {
                substitute = QStringTowstring(candidate);
                break;
            }
        }
    }
    return substitute;
}
//=============================================================================
void
flushGuiEvents(int maxTimeMs)
{
    if (qApp) {
        qApp->sendPostedEvents(nullptr, QEvent::DeferredDelete);
        qApp->processEvents(QEventLoop::AllEvents, maxTimeMs);
    }
}
//=============================================================================
bool
platformReportsWindowPosition()
{
    static const bool reportsPosition
        = !QGuiApplication::platformName().startsWith(QStringLiteral("wayland"));
    return reportsPosition;
}
//=============================================================================
static QPointer<QWidget> savedFocusWidget;
//=============================================================================
void
saveGuiFocus()
{
    savedFocusWidget = qApp ? qApp->focusWidget() : nullptr;
}
//=============================================================================
void
restoreGuiFocus()
{
    if (savedFocusWidget) {
        savedFocusWidget->setFocus();
    }
}
//=============================================================================
bool
nativePrimaryDisplay(std::wstring& deviceName, int& x, int& y, int& width, int& height, double& dpi)
{
#ifdef _WIN32
    // Per-monitor awareness for this thread only: physical pixels and the
    // monitor's own dpi instead of values virtualized for the process.
    DPI_AWARENESS_CONTEXT previous
        = SetThreadDpiAwarenessContext(DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2);
    HMONITOR monitor = MonitorFromPoint(POINT { 0, 0 }, MONITOR_DEFAULTTOPRIMARY);
    MONITORINFOEXW info {};
    info.cbSize = sizeof(info);
    const bool found = monitor != nullptr && GetMonitorInfoW(monitor, &info) != FALSE;
    if (found) {
        deviceName = info.szDevice;
        x = info.rcMonitor.left;
        y = info.rcMonitor.top;
        width = info.rcMonitor.right - info.rcMonitor.left;
        height = info.rcMonitor.bottom - info.rcMonitor.top;
        dpi = nativeMonitorDpi(monitor);
    }
    if (previous != nullptr) {
        SetThreadDpiAwarenessContext(previous);
    }
    return found;
#elif defined(__linux__)
    // before Qt opens the display, X11 itself answers: loaded at run time, no link dependency
    // the SONAME shipped by every distribution first, the development link name as a fallback
    void* x11 = dlopen("libX11.so.6", RTLD_LAZY);
    if (x11 == nullptr) {
        x11 = dlopen("libX11.so", RTLD_LAZY);
    }
    if (x11 == nullptr) {
        return false;
    }
    using OpenDisplayFunction = void* (*)(const char*);
    using CloseDisplayFunction = int (*)(void*);
    using DefaultScreenFunction = int (*)(void*);
    using DisplaySizeFunction = int (*)(void*, int);
    auto openDisplay = reinterpret_cast<OpenDisplayFunction>(dlsym(x11, "XOpenDisplay"));
    auto closeDisplay = reinterpret_cast<CloseDisplayFunction>(dlsym(x11, "XCloseDisplay"));
    auto defaultScreen = reinterpret_cast<DefaultScreenFunction>(dlsym(x11, "XDefaultScreen"));
    auto displayWidth = reinterpret_cast<DisplaySizeFunction>(dlsym(x11, "XDisplayWidth"));
    auto displayHeight = reinterpret_cast<DisplaySizeFunction>(dlsym(x11, "XDisplayHeight"));
    bool found = false;
    if (openDisplay && closeDisplay && defaultScreen && displayWidth && displayHeight) {
        void* display = openDisplay(nullptr);
        if (display != nullptr) {
            const int screen = defaultScreen(display);
            width = displayWidth(display, screen);
            height = displayHeight(display, screen);
            closeDisplay(display);
            found = width > 0 && height > 0;
        }
    }
    dlclose(x11);
    if (found) {
        // the same units Qt reports without an Xft.dpi setting: one logical pixel per pixel
        deviceName.clear();
        x = 0;
        y = 0;
        dpi = 96.0;
    }
    return found;
#else
    return false;
#endif
}
//=============================================================================
bool
applyDisconnectedDisplayScale()
{
    // An explicit user choice wins: per-screen factors, or scaling disabled
    // (a screen factor would switch it back on).
    if (qEnvironmentVariableIsSet(SCREEN_SCALE_FACTORS_ENV)
        || qEnvironmentVariable("QT_ENABLE_HIGHDPI_SCALING") == QStringLiteral("0")) {
        return false;
    }
    std::wstring deviceName;
    int x = 0;
    int y = 0;
    int width = 0;
    int height = 0;
    double dpi = 96.0;
    if (!nativePrimaryDisplay(deviceName, x, y, width, height, dpi)
        || deviceName != DISCONNECTED_DISPLAY_DEVICE || dpi == 96.0) {
        return false;
    }
    const QByteArray spec
        = QByteArray(DISCONNECTED_DISPLAY_QT_NAME) + '=' + QByteArray::number(dpi / 96.0, 'g', 6);
    return qputenv(SCREEN_SCALE_FACTORS_ENV, spec);
}
//=============================================================================
void
restoreDisconnectedDisplayScale(bool applied)
{
    if (applied) {
        qunsetenv(SCREEN_SCALE_FACTORS_ENV);
    }
}
//=============================================================================
bool
nativeLogicalScreen(int& width, int& height, double& scale)
{
    std::wstring deviceName;
    int x = 0;
    int y = 0;
    int physicalWidth = 0;
    int physicalHeight = 0;
    double dpi = 96.0;
    if (!nativePrimaryDisplay(deviceName, x, y, physicalWidth, physicalHeight, dpi)
        || physicalWidth <= 0 || physicalHeight <= 0) {
        return false;
    }
    scale = dpi / 96.0;
    width = static_cast<int>(std::lround(physicalWidth / scale));
    height = static_cast<int>(std::lround(physicalHeight / scale));
    return true;
}
//=============================================================================
double
nativeMonitorDpi(void* monitor)
{
#ifdef _WIN32
    // shcore is resolved at run time so the module keeps its link dependencies.
    using GetDpiForMonitorFunction = HRESULT(WINAPI*)(HMONITOR, int, UINT*, UINT*);
    static const auto getDpiForMonitor = reinterpret_cast<GetDpiForMonitorFunction>(
        GetProcAddress(LoadLibraryW(L"shcore.dll"), "GetDpiForMonitor"));
    constexpr int MDT_EFFECTIVE = 0;
    UINT dpiX = 0;
    UINT dpiY = 0;
    if (getDpiForMonitor != nullptr
        && SUCCEEDED(getDpiForMonitor(static_cast<HMONITOR>(monitor), MDT_EFFECTIVE, &dpiX, &dpiY))
        && dpiX > 0) {
        return static_cast<double>(dpiX);
    }
#endif
    return 96.0;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
