//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebGuiRpc.hpp"
#include "ExamplesBridge.hpp"
#include "ExamplesCatalogHost.hpp"
#include "WebToolsHost.hpp"
#include "UiHtmlFileHandler.hpp"
#include "WebGuiJson.hpp"
#include "WebGuiServer.hpp"
#include "WebGuiTerminal.hpp"
#include "MainThreadTaskQueue.hpp"
#include "WebGuiPreferences.hpp"
#include "CompleterHelper.hpp"
#include "NelsonConfiguration.hpp"
#include "WebDesktopBridge.hpp"
#include "RecentDirectories.hpp"
#include "DialogDirectories.hpp"
#include "ModulesManager.hpp"
#include "DynamicLibrary.hpp"
#include "characters_encoding.hpp"
#include "Evaluator.hpp"
#include "Context.hpp"
#include "Scope.hpp"
#include "ArrayOf.hpp"
#include "ClassToString.hpp"
#include "NelsonHistory.hpp"
#include "HistoryManager.hpp"
#include "SystemCommand.hpp"
#include "GuiDialogProvider.hpp"
#include "DebugStack.hpp"
#include "PositionScript.hpp"
#include "EvaluateCommand.hpp"
#include "CodeAnalyzer.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "Profiler.hpp"
#include "PackageUpdatesCount.hpp"
#include "Nelson_VERSION.h"
#include <string>
#include <vector>
#include <filesystem>
#include <algorithm>
#include <fstream>
#include <sstream>
//=============================================================================
namespace Nelson {
static const char*
figureWindowStyleName(int style)
{
    switch (style) {
    case 2:
        return "docked";
    case 1:
        return "modal";
    case 3:
        return "alwaysontop";
    default:
        return "normal";
    }
}
//=============================================================================
nlohmann::json
WebGuiRpc::figureList()
{
    nlohmann::json figures = nlohmann::json::array();
    if (!ensureGraphicsBridge() || m_listFiguresFn == nullptr) {
        return { { "figures", figures } };
    }
    std::vector<long long> handles(256);
    std::vector<int> styles(256, 0);
    std::vector<std::string> titles(256);
    int count = 0;
    // Graphics objects live on the interpreter thread; one hop reads everything.
    runOnInterpreterThread([&] {
        count = m_listFiguresFn(handles.data(), static_cast<int>(handles.size()));
        for (int i = 0; i < count; ++i) {
            if (m_getWindowStyleFn != nullptr) {
                styles[i] = m_getWindowStyleFn(handles[i]);
            }
            if (m_getFigureTitleFn != nullptr) {
                char buffer[512];
                m_getFigureTitleFn(handles[i], buffer, static_cast<int>(sizeof(buffer)));
                titles[i] = buffer;
            }
        }
    });
    // Figures a Notebook cell owns stay hidden from figure.list, otherwise the
    // desktop reconcile would open them as panels.
    const bool capturing = (m_server != nullptr) && m_server->inlineFigureCaptureActive();
    for (int i = 0; i < count; ++i) {
        if (m_server != nullptr && m_server->isNotebookFigure(handles[i])) {
            continue;
        }
        if (capturing && !m_server->isInlineBaselineFigure(handles[i])) {
            continue;
        }
        const std::string title
            = titles[i].empty() ? "Figure " + std::to_string(handles[i] + 1) : titles[i];
        figures.push_back({ { "handle", handles[i] }, { "title", title },
            { "windowStyle", figureWindowStyleName(styles[i]) } });
    }
    return { { "figures", figures } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::figureClose(const nlohmann::json& params)
{
    if (!ensureGraphicsBridge() || m_closeFigureFn == nullptr) {
        throw std::runtime_error("graphics not available");
    }
    const long long handle = params.value("handle", 0LL);
    bool ok = false;
    runOnInterpreterThread([&] { ok = m_closeFigureFn(handle); });
    if (!ok) {
        throw std::runtime_error("figure " + std::to_string(handle) + " not available");
    }
    return { { "handle", handle }, { "ok", true } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::figureGetImage(const nlohmann::json& params)
{
    if (!ensureGraphicsBridge() || m_renderPngFn == nullptr) {
        throw std::runtime_error("graphics not available");
    }
    const long long handle = params.value("handle", 0LL);
    std::string base64;
    bool ok = false;
    runOnInterpreterThread([&] {
        ok = m_renderPngFn(
            handle,
            [](void* ctx, const char* data, int len) {
                static_cast<std::string*>(ctx)->assign(data, static_cast<size_t>(len));
            },
            &base64);
    });
    if (!ok) {
        throw std::runtime_error("figure " + std::to_string(handle) + " not available");
    }
    return {
        { "handle", handle },
        { "image", "data:image/png;base64," + base64 },
        { "width", 0 },
        { "height", 0 },
    };
}
//=============================================================================
nlohmann::json
WebGuiRpc::figureSaveAs(const nlohmann::json& params)
{
    if (!ensureGraphicsBridge() || m_saveFigureFn == nullptr) {
        throw std::runtime_error("figure export not available");
    }
    const long long handle = params.value("handle", 0LL);
    const std::string path = params.value("path", std::string());
    if (path.empty()) {
        throw std::runtime_error("no output path");
    }
    bool ok = false;
    runOnInterpreterThread([&] { ok = m_saveFigureFn(handle, path.c_str()); });
    if (!ok) {
        throw std::runtime_error("could not save figure to '" + path + "'");
    }
    return { { "ok", true }, { "path", path } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::figureGetDisplayList(const nlohmann::json& params)
{
    if (!ensureGraphicsBridge() || m_displayListFn == nullptr) {
        throw std::runtime_error("native figure rendering not available");
    }
    const long long handle = params.value("handle", 0LL);
    const bool softwareFallback = params.value("softwareFallback", true);
    RenderDisplayListFn renderFn = m_displayListFn;
    if (!softwareFallback && m_displayListNoFallbackFn != nullptr) {
        renderFn = m_displayListNoFallbackFn;
    }
    std::string json;
    bool ok = false;
    runOnInterpreterThread([&] {
        ok = renderFn(
            handle,
            [](void* ctx, const char* data, int len) {
                static_cast<std::string*>(ctx)->assign(data, static_cast<size_t>(len));
            },
            &json);
    });
    if (!ok) {
        throw std::runtime_error("figure " + std::to_string(handle) + " not available");
    }
    nlohmann::json dl = nlohmann::json::parse(json, nullptr, false);
    if (dl.is_discarded()) {
        throw std::runtime_error("malformed display list");
    }
    dl["handle"] = handle;
    return dl;
}
//=============================================================================
nlohmann::json
WebGuiRpc::figureSetView(const nlohmann::json& params)
{
    if (params.contains("historyGroup")) {
        auto request = params;
        request["action"] = "view";
        return figureAction({ { "handle", params.at("handle") }, { "request", request } })
            .at("displayList");
    }
    if (!ensureGraphicsBridge() || m_setAxesViewFn == nullptr) {
        throw std::runtime_error("native figure rendering not available");
    }
    const long long handle = params.value("handle", 0LL);
    // axesHandle > 0 rotates a single subplot; 0 rotates every axes together.
    const long long axesHandle = params.value("axesHandle", 0LL);
    const double azimuth = params.value("azimuth", 0.0);
    const double elevation = params.value("elevation", 0.0);
    const bool softwareFallback = params.value("softwareFallback", true);
    const bool compactView = params.value("compactView", false);
    bool ok = false;
    runOnInterpreterThread([&] { ok = m_setAxesViewFn(handle, axesHandle, azimuth, elevation); });
    if (!ok) {
        throw std::runtime_error("figure " + std::to_string(handle) + " has no axes to rotate");
    }
    if (compactView && !softwareFallback && m_displayListInteractiveFrameFn != nullptr
        && m_markViewOnlyFrameFn != nullptr) {
        std::string json;
        bool rendered = false;
        runOnInterpreterThread([&] {
            m_markViewOnlyFrameFn(handle);
            rendered = m_displayListInteractiveFrameFn(
                handle,
                [](void* ctx, const char* data, int len) {
                    static_cast<std::string*>(ctx)->assign(data, static_cast<size_t>(len));
                },
                &json);
        });
        if (rendered) {
            nlohmann::json dl = nlohmann::json::parse(json, nullptr, false);
            if (!dl.is_discarded()) {
                dl["handle"] = handle;
                return dl;
            }
        }
    }
    // Re-rendered display list in the same round-trip keeps rotation responsive.
    return figureGetDisplayList(params);
}
//=============================================================================
nlohmann::json
WebGuiRpc::figureSetSize(const nlohmann::json& params)
{
    if (!ensureGraphicsBridge() || m_setFigureSizeFn == nullptr) {
        throw std::runtime_error("native figure rendering not available");
    }
    const long long handle = params.value("handle", 0LL);
    const int width = params.value("width", 0);
    const int height = params.value("height", 0);
    bool ok = false;
    runOnInterpreterThread([&] { ok = m_setFigureSizeFn(handle, width, height); });
    if (!ok) {
        throw std::runtime_error("figure " + std::to_string(handle) + " could not be resized");
    }
    return figureGetDisplayList(params);
}
//=============================================================================
nlohmann::json
WebGuiRpc::figureSetPosition(const nlohmann::json& params)
{
    // A native figure window was moved/resized: write back the Position property.
    if (!ensureGraphicsBridge() || m_setFigurePositionFn == nullptr) {
        return { { "ok", false } };
    }
    const long long handle = params.value("handle", 0LL);
    const int x = params.value("x", 0);
    const int y = params.value("y", 0);
    const int width = params.value("width", 0);
    const int height = params.value("height", 0);
    bool ok = false;
    runOnInterpreterThread([&] { ok = m_setFigurePositionFn(handle, x, y, width, height); });
    return { { "ok", ok } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::figureSetWindowSize(const nlohmann::json& params)
{
    // Pure window operation (client size computed by the figure page), no
    // interpreter thread involved.
    const long long handle = params.value("handle", 0LL);
    const int width = params.value("width", 0);
    const int height = params.value("height", 0);
    m_server->setFigureWindowClientSize(handle, width, height);
    return { { "ok", true } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::figureUndock(const nlohmann::json& params)
{
    const long long handle = params.value("handle", 0LL);
    int width = 0;
    int height = 0;
    if (ensureGraphicsBridge() && m_getFigureSizeFn != nullptr) {
        runOnInterpreterThread([&] { m_getFigureSizeFn(handle, &width, &height); });
    }
    m_server->openFigureWindow(handle, width, height);
    return { { "ok", true } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::figureDock(const nlohmann::json& params)
{
    const long long handle = params.value("handle", 0LL);
    m_server->closeFigureWindow(handle);
    return { { "ok", true } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::uicontrolAction(const nlohmann::json& params)
{
    if (!ensureGraphicsBridge() || m_uiControlActionFn == nullptr) {
        throw std::runtime_error("native figure rendering not available");
    }
    const long long handle = params.value("handle", 0LL);
    const double value = params.value("value", 0.0);
    const std::string text = params.value("text", std::string());
    bool ok = false;
    runOnInterpreterThread([&] { ok = m_uiControlActionFn(handle, value, text.c_str()); });
    if (!ok) {
        throw std::runtime_error("uicontrol " + std::to_string(handle) + " not found");
    }
    return { { "handle", handle }, { "ok", true } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::uihtmlPull(const nlohmann::json& params)
{
    if (!ensureGraphicsBridge() || m_uiHtmlPullFn == nullptr) {
        throw std::runtime_error("native figure rendering not available");
    }
    const long long handle = params.value("handle", 0LL);
    std::string json;
    bool ok = false;
    runOnInterpreterThread([&] {
        ok = m_uiHtmlPullFn(
            handle,
            [](void* ctx, const char* data, int len) {
                static_cast<std::string*>(ctx)->assign(data, static_cast<size_t>(len));
            },
            &json);
    });
    if (!ok) {
        throw std::runtime_error("uihtml " + std::to_string(handle) + " not found");
    }
    nlohmann::json state = nlohmann::json::parse(json, nullptr, false);
    if (state.is_discarded()) {
        throw std::runtime_error("malformed uihtml state");
    }
    // The file route comes from what the ENGINE reports, never from the client.
    const std::string source = state.value("source", std::string());
    if (!source.empty() && m_server != nullptr) {
        m_server->ensureUiHtmlFileHandler();
    }
    UiHtmlRegisterSource(handle, source);
    state["handle"] = handle;
    return state;
}
//=============================================================================
std::vector<long long>
WebGuiRpc::notebookListFigureHandles()
{
    std::vector<long long> handles;
    if (!ensureGraphicsBridge() || m_listFiguresFn == nullptr) {
        return handles;
    }
    std::vector<long long> buffer(256);
    int count = 0;
    runOnInterpreterThread(
        [&] { count = m_listFiguresFn(buffer.data(), static_cast<int>(buffer.size())); });
    if (count > 0) {
        handles.assign(buffer.begin(), buffer.begin() + std::min<int>(count, 256));
    }
    return handles;
}
//=============================================================================
// Evaluate one Notebook cell against the base workspace; captures the text
// output (or error) and renders the figures it created as data-URL PNGs.
nlohmann::json
WebGuiRpc::notebookRun(const nlohmann::json& params)
{
    const std::string code = params.value("code", std::string());
    const std::wstring wcode = utf8_to_wstring(code);

    // A Wasm page reload creates a new frontend while the shared worker may
    // still hold figures registered by the previous Notebook session. The
    // first run of the new session explicitly transfers and closes those
    // orphaned figures before taking the baseline snapshot.
    if (params.value("resetLiveFigures", false) && m_server != nullptr) {
        for (const long long handle : m_server->takeNotebookFigures()) {
            interactiveCloseFigure(handle);
        }
    }

    // Close the figures of the previous run BEFORE the baseline snapshot so the
    // cell's figure() calls re-grab the same numbers (stable handles on re-run).
    if (params.contains("reuseHandles") && params.at("reuseHandles").is_array()) {
        for (const auto& item : params.at("reuseHandles")) {
            if (!item.is_number_integer()) {
                continue;
            }
            const long long handle = item.get<long long>();
            if (handle < 0) {
                continue;
            }
            interactiveCloseFigure(handle);
            if (m_server != nullptr) {
                m_server->unregisterNotebookFigure(handle);
            }
        }
    }

    // New figures are shown inline: suppress creation/frame pushes and hide them
    // from figure.list for the whole run so the desktop never opens a panel for
    // them. The guard restores everything on any exit.
    const std::vector<long long> before = notebookListFigureHandles();
    if (m_server != nullptr) {
        m_server->setInlineFigureBaseline(before);
    }
    struct InlineCaptureGuard
    {
        WebGuiServer* server;
        explicit InlineCaptureGuard(WebGuiServer* s) : server(s)
        {
            if (server != nullptr) {
                server->beginInlineFigureCapture();
            }
        }
        ~InlineCaptureGuard()
        {
            if (server != nullptr) {
                server->endInlineFigureCapture();
            }
        }
    } inlineCaptureGuard(m_server);

    std::wstring out;
    bool ok = false;
    if (m_evaluator != nullptr) {
        ok = EvaluateConsoleCommandToString(m_evaluator, wcode, out);
    }
    const std::string text = wstring_to_utf8(out);

    // Interactive mode keeps the figures alive for the inline figure panel; the
    // PNG is always produced as the fallback of a saved notebook.
    const bool interactive = params.value("interactiveFigures", false) && m_server != nullptr;

    nlohmann::json figures = nlohmann::json::array();
    if (ok && ensureGraphicsBridge() && m_renderPngFn != nullptr) {
        // Figures the cell created (handles absent before it ran) render inline.
        for (long long handle : notebookListFigureHandles()) {
            if (std::find(before.begin(), before.end(), handle) != before.end()) {
                continue;
            }
            // Own it BEFORE the capture window ends: no gap for a reconcile.
            if (interactive) {
                m_server->registerNotebookFigure(handle);
            }
            std::string base64;
            bool rendered = false;
            runOnInterpreterThread([&] {
                rendered = m_renderPngFn(
                    handle,
                    [](void* ctx, const char* data, int len) {
                        static_cast<std::string*>(ctx)->assign(data, static_cast<size_t>(len));
                    },
                    &base64);
            });
            if (rendered) {
                figures.push_back(
                    { { "handle", handle }, { "image", "data:image/png;base64," + base64 },
                        { "width", 0 }, { "height", 0 }, { "interactive", interactive } });
            } else if (interactive) {
                m_server->unregisterNotebookFigure(handle);
                interactiveCloseFigure(handle);
            }
            if (!interactive && m_closeFigureFn != nullptr) {
                runOnInterpreterThread([&] { m_closeFigureFn(handle); });
            }
        }
    }

    if (interactive && !figures.empty() && m_clearCurrentFigureFn != nullptr) {
        runOnInterpreterThread([&] { m_clearCurrentFigureFn(); });
    }

    return { { "ok", ok }, { "output", ok ? text : std::string() },
        { "errorId", ok ? std::string() : std::string("Nelson:Notebook:error") },
        { "errorMessage", ok ? std::string() : text }, { "figures", figures } };
}
//=============================================================================
void
WebGuiRpc::interactiveCloseFigure(long long handle)
{
    if (m_closeFigureFn != nullptr) {
        runOnInterpreterThread([&] { m_closeFigureFn(handle); });
    }
}
//=============================================================================
// Hand the figure back to the desktop without closing it ("open in figure window").
nlohmann::json
WebGuiRpc::notebookReleaseFigure(const nlohmann::json& params)
{
    const long long handle = params.value("handle", -1LL);
    if (m_server != nullptr && handle >= 0) {
        m_server->unregisterNotebookFigure(handle);
    }
    return { { "ok", true }, { "handle", handle } };
}
//=============================================================================
// Unregister AFTER closing so the closure push stays suppressed.
nlohmann::json
WebGuiRpc::notebookCloseFigures(const nlohmann::json& params)
{
    nlohmann::json closed = nlohmann::json::array();
    if (!params.contains("handles") || !params.at("handles").is_array()) {
        return { { "closed", closed } };
    }
    for (const auto& item : params.at("handles")) {
        const long long handle = item.get<long long>();
        interactiveCloseFigure(handle);
        if (m_server != nullptr) {
            m_server->unregisterNotebookFigure(handle);
        }
        closed.push_back(handle);
    }
    return { { "closed", closed } };
}
//=============================================================================
} // namespace Nelson
//=============================================================================
