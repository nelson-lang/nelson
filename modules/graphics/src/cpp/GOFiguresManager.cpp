//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <memory>
#include "GOFiguresManager.hpp"
#include "GOFigure.hpp"
#include "HeadlessFigureHost.hpp"
#include "GraphicsEnvironment.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOHelpers.hpp"
#include "GOStringOnOffProperty.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
#include "WebDesktopBridge.hpp"
#include "WebFigureAnimation.hpp"
#include "NelSon_engine_mode.h"
#include "DynamicLibrary.hpp"
#include "RuntimeDispatch.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// The Qt windowing module has its own cleanup object, which can run after this manager is
// destroyed (static destruction order across libraries). The trivially destructible flag
// keeps shutdownGraphic() from touching the manager outside its lifetime.
static bool figuresManagerAlive = false;
//=============================================================================
// Defaults to Normal when the figure has no host yet.
static FigureWindowStyle
figureWindowStyle(int64 figNum);
//=============================================================================
class GOFiguresManagerSingleton
{
private:
    std::map<int64, GOFigureHost*> hFigures;
    int64 GOCurrentFig = NO_FIGURE;

    GOFiguresManagerSingleton() { figuresManagerAlive = true; }
    ~GOFiguresManagerSingleton() { figuresManagerAlive = false; }

public:
    GOFiguresManagerSingleton(const GOFiguresManagerSingleton&) = delete;
    GOFiguresManagerSingleton&
    operator=(const GOFiguresManagerSingleton&)
        = delete;

    static GOFiguresManagerSingleton&
    getInstance()
    {
        static GOFiguresManagerSingleton instance;
        return instance;
    }

    std::map<int64, GOFigureHost*>&
    getFigures()
    {
        return hFigures;
    }
    const std::map<int64, GOFigureHost*>&
    getFigures() const
    {
        return hFigures;
    }

    int64
    getCurrentFig() const
    {
        return GOCurrentFig;
    }
    void
    setCurrentFig(int64 fig)
    {
        GOCurrentFig = fig;
    }

    void
    clear()
    {
        hFigures.clear();
        GOCurrentFig = NO_FIGURE;
    }
};
//=============================================================================
void
saveFocus()
{
    GraphicsEnvironment::saveGuiFocus();
}
//=============================================================================
void
restoreFocus()
{
    GraphicsEnvironment::restoreGuiFocus();
}
//=============================================================================
std::map<int64, GOFigureHost*>
getFigureList()
{
    return GOFiguresManagerSingleton::getInstance().getFigures();
}
//=============================================================================
int64
findAvailableFigureId()
{
    auto& hFigures = GOFiguresManagerSingleton::getInstance().getFigures();
    int64 figureNumber = 0;
    bool figureAvailable = false;
    while ((figureNumber < MAX_FIGS) && !figureAvailable) {
        figureAvailable = (hFigures[figureNumber] == nullptr);
        if (!figureAvailable) {
            figureNumber++;
        } else {
            return figureNumber;
        }
    }
    Error("Nelson:figure:tooManyFigures", "Please close some graphical windows.");
    return NO_FIGURE;
}
//=============================================================================
// No Qt window in the web desktop: the figure is rendered offscreen and streamed to the
// browser, so show() is never called.
static inline bool
isWebGuiMode()
{
    const auto mode = NelsonConfiguration::getInstance()->getNelsonEngineMode();
    return mode == WEB_GUI || mode == WASM_TERMINAL;
}
//=============================================================================
// True when figures use the headless RenderWeb host instead of Qt: web desktop, WebAssembly
// terminal, or advanced terminal started with `--webview`. Server-side figure window
// notifications stay gated on WEB_GUI alone.
static inline bool
useWebGraphics()
{
    return isWebGuiMode() || NelsonConfiguration::getInstance()->isWebGraphicsMode();
}
//=============================================================================
// For the interpreter's statement-boundary pump an interactive web figure counts as an open
// Qt window: the event loop and key/mouse queue must be serviced at every statement
// boundary, or input lags behind a running script. graphics_qt bumps the count for Qt
// windows, this does it for web figures.
static inline void
noteWebFigureOpened()
{
    if (useWebGraphics()) {
        NelsonConfiguration::getInstance()->incrementOpenFigureWindowCount();
    }
}
//=============================================================================
static inline void
noteWebFigureClosed()
{
    if (useWebGraphics()) {
        NelsonConfiguration::getInstance()->decrementOpenFigureWindowCount();
    }
}
//=============================================================================
// Hosts are built through a registered factory so the model never names a windowing type.
// Desktop modes load the Qt adapter on first use.
static FigureHostFactory hostFactory = nullptr;
static library_handle graphicsQtLibrary = nullptr;
//=============================================================================
void
setFigureHostFactory(FigureHostFactory factory)
{
    hostFactory = factory;
}
//=============================================================================
static void
loadQtFigureHostFactory()
{
    if (hostFactory != nullptr || graphicsQtLibrary != nullptr) {
        return;
    }
    const int mode = NelsonConfiguration::getInstance()->getNelsonEngineMode();
    if (mode != NELSON_ENGINE_MODE::GUI && mode != NELSON_ENGINE_MODE::ADVANCED_TERMINAL) {
        return;
    }
    const std::wstring libraryPath = NelsonConfiguration::getInstance()->getNelsonLibraryDirectory()
        + L"/libnlsGraphics_qt" + get_dynamic_library_extensionW();
    // Loading the adapter installs the host factory and the GUI environment hooks. Keep the
    // handle alive: figure hosts contain vtables and Qt objects from this DLL.
    graphicsQtLibrary = load_dynamic_libraryW(libraryPath);
}
//=============================================================================
static GOFigureHost*
createFigureHost(int64 figNum)
{
    if (useWebGraphics()) {
        return createHeadlessFigureHost(figNum);
    }
    if (!hasGuiServices()) {
        return createHeadlessFigureHost(figNum);
    }
    loadQtFigureHostFactory();
    if (hostFactory == nullptr) {
        return createHeadlessFigureHost(figNum);
    }
    return hostFactory(figNum, isWebGuiMode());
}
//=============================================================================
int64
createNewFigure(bool show)
{
    auto& manager = GOFiguresManagerSingleton::getInstance();
    auto& hFigures = manager.getFigures();

    int64 figNum = findAvailableFigureId();
    checkIdValidity(figNum);
    hFigures[figNum] = createFigureHost(figNum);
    if (show && !useWebGraphics()) {
        saveFocus();
        hFigures[figNum]->presentHost();
        restoreFocus();
    }
    manager.setCurrentFig(figNum);
    WebDesktopBridge::getInstance()->pushWebFigureLifecycle(
        figNum, true, figureWindowStyle(figNum));
    noteWebFigureOpened();
    return manager.getCurrentFig();
}
//=============================================================================
int64
selectFigure(int64 fignum, bool show)
{
    auto& manager = GOFiguresManagerSingleton::getInstance();
    auto& hFigures = manager.getFigures();

    checkIdValidity(fignum);
    bool created = false;
    if (hFigures.count(fignum) == 0
        || ((hFigures.count(fignum) == 1) && getFigure(fignum) == nullptr)) {
        hFigures[fignum] = createFigureHost(fignum);
        created = true;
    }
    manager.setCurrentFig(fignum);
    if (created) {
        WebDesktopBridge::getInstance()->pushWebFigureLifecycle(
            fignum, true, figureWindowStyle(fignum));
        noteWebFigureOpened();
    }
    if (show && !useWebGraphics()) {
        saveFocus();
        hFigures[fignum]->presentHost();
        restoreFocus();
    }
    return manager.getCurrentFig();
}
//=============================================================================
GOFigure*
getCurrentGOFigure()
{
    auto& manager = GOFiguresManagerSingleton::getInstance();
    auto& hFigures = manager.getFigures();

    if (manager.getCurrentFig() == NO_FIGURE) {
        createNewFigure();
    }
    return hFigures[manager.getCurrentFig()]->hostFigure();
}
//=============================================================================
GOFigureHost*
getFigure(int64 id)
{
    const auto& hFigures = GOFiguresManagerSingleton::getInstance().getFigures();
    if (hFigures.count(id) == 1 && hFigures.at(id) != nullptr) {
        return hFigures.at(id);
    }
    return nullptr;
}
//=============================================================================
std::vector<int64>
getFigureGraphicsObjects()
{
    const auto& hFigures = GOFiguresManagerSingleton::getInstance().getFigures();
    std::vector<int64> gos;
    for (const auto& it : hFigures) {
        if (it.second) {
            gos.push_back(it.first);
        }
    }
    return gos;
}
//=============================================================================
int64
getCurrentFigure()
{
    return GOFiguresManagerSingleton::getInstance().getCurrentFig();
}
//=============================================================================
void
notifyCurrentFigureChanged(int64 figNum)
{
    GOFiguresManagerSingleton::getInstance().setCurrentFig(figNum);
}
//=============================================================================
GOFigureHost*
getHandleWindow(int64 fignum)
{
    const auto& hFigures = GOFiguresManagerSingleton::getInstance().getFigures();
    if (hFigures.count(fignum) == 1) {
        return hFigures.at(fignum);
    }
    return nullptr;
}
//=============================================================================
void
initializeGraphic()
{
    GOFiguresManagerSingleton::getInstance().clear();
}
//=============================================================================
void
shutdownGraphic()
{
    if (!figuresManagerAlive) {
        return;
    }
    const auto& hFigures = GOFiguresManagerSingleton::getInstance().getFigures();
    std::vector<int64> figureIds;
    figureIds.reserve(hFigures.size());
    for (const auto& it : hFigures) {
        if (it.second) {
            figureIds.push_back(it.first);
        }
    }
    for (int64 figureId : figureIds) {
        if (getFigure(figureId)) {
            deleteFigure(figureId);
            GraphicsEnvironment::flushGuiEvents(50);
        }
    }
}
//=============================================================================
static bool
isDefaultCloseRequest(GOCallbackProperty* property)
{
    if (property == nullptr) {
        return true;
    }
    ArrayOf callback = property->data();
    if (!callback.isRowVectorCharacterArray() && !callback.isScalarStringArray()) {
        return false;
    }
    std::unique_ptr<wchar_t[]> buffer(callback.toAllocatedWideCString());
    return std::wstring(buffer.get()) == GO_PROPERTY_VALUE_CLOSEREQ_STR;
}
//=============================================================================
bool
closeFigure(go_handle fignum, bool forceClose)
{
    GOFigureHost* win = getFigure(fignum);
    if (win == nullptr) {
        return false;
    }
    GOFigure* fig = win->hostFigure();
    if (fig == nullptr) {
        return deleteFigure(fignum, forceClose);
    }

    GOCallbackProperty* goCallback = static_cast<GOCallbackProperty*>(
        fig->findProperty(GO_CLOSE_REQUEST_FCN_PROPERTY_NAME_STR));
    if (isDefaultCloseRequest(goCallback)) {
        return deleteFigure(fignum, forceClose);
    }
    if (goCallback) {
        goCallback->executeNow(fig, L"WindowCloseRequestData", L"Close");
    }
    return getFigure(fignum) == nullptr;
}
//=============================================================================
bool
deleteFigure(go_handle fignum, bool forceClose)
{
    auto& manager = GOFiguresManagerSingleton::getInstance();
    auto& hFigures = manager.getFigures();

    GOFigureHost* win = getFigure(fignum);
    if (win) {
        bool hostPrepared = false;
        GOFigure* fig = win->hostFigure();
        GOFigure* originalFig = fig;
        if (fig) {
            GOOnOffProperty* goOnOff = static_cast<GOOnOffProperty*>(
                fig->findProperty(GO_BEING_DELETED_PROPERTY_NAME_STR, false));
            bool alreadyBeingDeleted
                = fig->stringCheck(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
            if (alreadyBeingDeleted) {
                return true;
            }
            if (goOnOff) {
                goOnOff->data(GO_PROPERTY_VALUE_ON_STR);
            }
            win->prepareHostClose(forceClose);
            hostPrepared = true;
            GOCallbackProperty* goCallback = static_cast<GOCallbackProperty*>(
                fig->findProperty(GO_DELETE_FCN_PROPERTY_NAME_STR, false));
            if (goCallback) {
                goCallback->executeNow(fig, L"EventData", L"ObjectBeingDestroyed");
                fig = win->hostFigure();
                if (fig == nullptr || fig != originalFig) {
                    return true;
                }
            }
            fig->setRenderingStateInvalid(true);
            GOGObjectsProperty* hp
                = (GOGObjectsProperty*)fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
            if (hp) {
                std::vector<int64> children(hp->data());
                if (!children.empty()) {
                    std::stable_sort(children.begin(), children.end(), [](int64 lhs, int64 rhs) {
                        GraphicsObject* left = findGraphicsObject(lhs, false);
                        GraphicsObject* right = findGraphicsObject(rhs, false);
                        bool leftAxes = left
                            && (left->isType(GO_PROPERTY_VALUE_AXES_STR)
                                || left->isType(GO_PROPERTY_VALUE_POLAR_AXES_STR));
                        bool rightAxes = right
                            && (right->isType(GO_PROPERTY_VALUE_AXES_STR)
                                || right->isType(GO_PROPERTY_VALUE_POLAR_AXES_STR));
                        return !leftAxes && rightAxes;
                    });
                    for (auto c : children) {
                        deleteGraphicsObject(c, false, false);
                    }
                    children.clear();
                    hp->data(children);
                }
            }
        }
        if (!hostPrepared) {
            win->prepareHostClose(forceClose);
        }
        hFigures[fignum] = nullptr;
        if (forceClose) {
            win->destroyHost();
        }
        win = nullptr;
        forgetWebFigureAnimation(static_cast<int64_t>(fignum));
        if (manager.getCurrentFig() == fignum) {
            std::vector<int64> nums = getFigureGraphicsObjects();
            if (nums.empty()) {
                manager.setCurrentFig(NO_FIGURE);
            } else {
                manager.setCurrentFig(nums.back());
            }
        }
        WebDesktopBridge::getInstance()->pushWebFigureLifecycle(
            fignum, false, FigureWindowStyle::Normal);
        noteWebFigureClosed();
        return true;
    }
    return false;
}
//=============================================================================
static FigureWindowStyle
figureWindowStyle(int64 figNum)
{
    GOFigureHost* host = getFigure(figNum);
    if (host == nullptr) {
        return FigureWindowStyle::Normal;
    }
    GOFigure* fig = host->hostFigure();
    if (fig == nullptr) {
        return FigureWindowStyle::Normal;
    }
    const std::wstring style = fig->findStringProperty(GO_WINDOW_STYLE_PROPERTY_NAME_STR);
    if (style == GO_PROPERTY_VALUE_DOCKED_STR) {
        return FigureWindowStyle::Docked;
    }
    if (style == GO_PROPERTY_VALUE_MODAL_STR) {
        return FigureWindowStyle::Modal;
    }
    if (style == GO_PROPERTY_VALUE_ALWAYS_ON_TOP_STR) {
        return FigureWindowStyle::AlwaysOnTop;
    }
    return FigureWindowStyle::Normal;
}
//=============================================================================
void
notifyWebFigureWindowStyleChanged(GOFigure* fig)
{
    if (fig == nullptr || !useWebGraphics()) {
        return;
    }
    const auto& hFigures = GOFiguresManagerSingleton::getInstance().getFigures();
    for (const auto& it : hFigures) {
        if (it.second != nullptr && it.second->hostFigure() == fig) {
            WebDesktopBridge::getInstance()->pushWebFigureWindowStyle(
                it.first, figureWindowStyle(it.first));
            return;
        }
    }
}
//=============================================================================
void
notifyWebFigureVisibleChanged(GOFigure* fig)
{
    if (fig == nullptr || !useWebGraphics()) {
        return;
    }
    const auto& hFigures = GOFiguresManagerSingleton::getInstance().getFigures();
    for (const auto& it : hFigures) {
        if (it.second != nullptr && it.second->hostFigure() == fig) {
            const bool visible
                = fig->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
            WebDesktopBridge::getInstance()->pushWebFigureVisible(it.first, visible);
            // Web mode does not consume the changed flag; clear it to avoid a repeat notification.
            fig->clearChanged(GO_VISIBLE_PROPERTY_NAME_STR);
            return;
        }
    }
}
//=============================================================================
std::wstring
computeFigureTitle(int64 figNum)
{
    const std::wstring fallback = L"Figure " + std::to_wstring(figNum + 1);
    GOFigureHost* host = getFigure(figNum);
    if (host == nullptr) {
        return fallback;
    }
    GOFigure* fig = host->hostFigure();
    if (fig == nullptr) {
        return fallback;
    }
    const std::wstring name = fig->findStringProperty(GO_NAME_PROPERTY_NAME_STR);
    if (fig->stringCheck(GO_NUMBER_TITLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
        if (name.empty()) {
            return fallback;
        }
        return fallback + L": " + name;
    }
    return name;
}
//=============================================================================
void
notifyWebFigureTitleChanged(GOFigure* fig)
{
    if (fig == nullptr || !useWebGraphics()) {
        return;
    }
    const auto& hFigures = GOFiguresManagerSingleton::getInstance().getFigures();
    for (const auto& it : hFigures) {
        if (it.second != nullptr && it.second->hostFigure() == fig) {
            const std::string title = wstring_to_utf8(computeFigureTitle(it.first));
            WebDesktopBridge::getInstance()->pushWebFigureTitle(it.first, title.c_str());
            fig->clearChanged(GO_NAME_PROPERTY_NAME_STR);
            fig->clearChanged(GO_NUMBER_TITLE_PROPERTY_NAME_STR);
            return;
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
