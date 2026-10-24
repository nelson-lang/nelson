//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOWindow.hpp"
#include "GOFiguresManager.hpp"
#include "GraphicsQtModule.hpp"
#include "GraphicsEnvironment.hpp"
#include "ProcessEvents.hpp"
#include "RuntimeDispatch.hpp"
#include "ScreenInfo.hpp"
#include "UIComponentBindings.hpp"
#include "UIControlBinding.hpp"
#include "NelSon_engine_mode.h"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
initializeGraphicsQtBackend()
{
}
//=============================================================================
static void
processGraphicsGuiEvents(bool waitEvents)
{
    if (!hasQtRuntime() || isGuiThread()) {
        ProcessEvents(waitEvents);
    }
}
//=============================================================================
// Environment services backed by the gui module (screen, pointer, event
// pump, focus, preferences). libnlsGraphics itself imports none of this: it
// calls the hooks registered here, and falls back to headless defaults when
// this module is not loaded (nelson-webview).
static GraphicsEnvironmentHooks
guiEnvironmentHooks()
{
    GraphicsEnvironmentHooks hooks;
    hooks.primaryScreenDpi = &primaryScreenDpi;
    hooks.frameCaptureDeviceRatio = &primaryScreenDevicePixelRatio;
    hooks.primaryScreenDepth = &primaryScreenDepth;
    hooks.currentScreenResolution = &currentScreenResolution;
    hooks.pointerScreenPosition = &pointerScreenPosition;
    hooks.setPointerScreenPosition = &setPointerScreenPosition;
    hooks.defaultSansSubstituteFontFamily = &defaultSansSubstituteFontFamily;
    hooks.flushGuiEvents = &flushGuiEvents;
    hooks.processGuiEvents = &processGraphicsGuiEvents;
    hooks.saveGuiFocus = &saveGuiFocus;
    hooks.restoreGuiFocus = &restoreGuiFocus;
    hooks.platformReportsWindowPosition = &platformReportsWindowPosition;
    hooks.desktopPrefersDockedFigures = &DesktopPrefersDockedFigures;
    return hooks;
}
//=============================================================================
// The figures manager creates hosts only through the registered factory (the
// model never names a windowing type). Registering at load time is enough: graphics_io
// links this module and the standard module set loads it before any figure exists.
static GOFigureHost*
createGOWindowHost(int64 figureId, bool headless)
{
    return new GOWindow(figureId, headless);
}
//=============================================================================
// Registers on load; on unload it closes every remaining figure FIRST and
// unregisters. This DLL unloads before nlsGraphics (reverse dependency
// order), so if the model's shutdown ran later it would call virtuals on
// vtables that no longer exist (the access violation seen on exit()).
static struct FigureHostFactoryLifetime
{
    bool registered { false };
    bool webCaptureHookOnly { false };

    FigureHostFactoryLifetime()
    {
        if (NelsonConfiguration::getInstance()->getNelsonEngineMode() == WEB_GUI) {
            // Web figures are headless (no GOWindow, no GUI event pump), but
            // the root object still reports desktop screen metrics.
            GraphicsEnvironmentHooks webHooks;
            webHooks.primaryScreenDpi = &primaryScreenDpi;
            webHooks.frameCaptureDeviceRatio = &primaryScreenDevicePixelRatio;
            webHooks.primaryScreenDepth = &primaryScreenDepth;
            webHooks.currentScreenResolution = &currentScreenResolution;
            webHooks.pointerScreenPosition = &pointerScreenPosition;
            webHooks.setPointerScreenPosition = &setPointerScreenPosition;
            webHooks.defaultSansSubstituteFontFamily = &defaultSansSubstituteFontFamily;
            webHooks.platformReportsWindowPosition = &platformReportsWindowPosition;
            setGraphicsEnvironmentHooks(webHooks);
            webCaptureHookOnly = true;
            return;
        }
        setFigureHostFactory(&createGOWindowHost);
        setUIComponentBindingFactory(&createUIComponentBinding);
        setUIControlBindingFactory(&createUIControlBinding);
        setUIControlKeyInjector(&injectUIControlKey);
        setGraphicsEnvironmentHooks(guiEnvironmentHooks());
        registered = true;
    }
    ~FigureHostFactoryLifetime()
    {
        if (webCaptureHookOnly) {
            clearGraphicsEnvironmentHooks();
            return;
        }
        if (!registered) {
            return;
        }
        callGuiSync([]() { shutdownGraphic(); });
        clearGraphicsEnvironmentHooks();
        setUIControlKeyInjector(nullptr);
        setUIControlBindingFactory(nullptr);
        setUIComponentBindingFactory(nullptr);
        setFigureHostFactory(nullptr);
    }
} figureHostFactoryLifetime;
//=============================================================================
} // namespace Nelson
//=============================================================================
