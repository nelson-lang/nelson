//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#if _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <string>
#include <map>
#include <mutex>
#include <condition_variable>
#include <functional>
#include <nlohmann/json.hpp>
#include "nlsWeb_gui_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class WebGuiServer;
class WebGuiTerminal;
class Evaluator;
struct GuiDialogRequest;
struct GuiDialogResponse;
//=============================================================================
// Parses the JSON envelope, routes to a handler, and serializes the response.
// See CONTRACT.md for the full method list.
//=============================================================================
class NLSWEB_GUI_IMPEXP WebGuiRpc
{
public:
    WebGuiRpc(WebGuiServer* server, WebGuiTerminal* terminal);
    ~WebGuiRpc();

    void
    setEvaluator(Evaluator* eval);

    // envelope: {id, method, params} -> {id, result} | {id, error}
    std::string
    dispatch(const std::string& envelope);

private:
    WebGuiServer* m_server;
    WebGuiTerminal* m_terminal;
    Evaluator* m_evaluator { nullptr };
    bool m_hostsWebTools { false };

    // GUI dialogs (inputdlg, uialert, uiconfirm): the interpreter thread blocks
    // in the dialog builtin waiting for the browser's dialog.reply. Coordinated
    // by a condition variable; the reply is served on the server thread.
    std::mutex m_dialogMutex;
    std::condition_variable m_dialogCv;
    std::map<long long, nlohmann::json> m_dialogReplies;
    long long m_dialogSeq { 0 };
    void
    registerDialogProvider();
    // Runs on the interpreter thread: pushes dialog.show and blocks until the
    // browser answers with dialog.reply. Returns true (dialog handled).
    bool
    serveDialog(const GuiDialogRequest& request, GuiDialogResponse& response);
    static bool
    dialogProviderThunk(
        void* context, const GuiDialogRequest& request, GuiDialogResponse& response);
    nlohmann::json
    dialogReply(const nlohmann::json& params);

    // Marshal work onto the interpreter thread. Delegates to the shared
    // MainThreadTaskQueue (drained by the bytecode VM and every REPL, including
    // the advanced terminal), so figure-interaction RPCs work whether the
    // console is the browser (WebGuiTerminal) or a real terminal with no
    // WebGuiTerminal at all (nelson-adv-cli --webview).
    bool
    runOnInterpreterThread(const std::function<void()>& fn, int timeoutMs = 15000);

    nlohmann::json
    handle(const std::string& method, const nlohmann::json& params);

    nlohmann::json
    ping();
    nlohmann::json
    appCapabilities();
    nlohmann::json
    diagnosticsInfo();
    nlohmann::json
    preferencesLoad();
    nlohmann::json
    preferencesSave(const nlohmann::json& params);
    nlohmann::json
    termInput(const nlohmann::json& params);
    nlohmann::json
    termInterrupt();
    nlohmann::json
    termResize(const nlohmann::json& params);
    nlohmann::json
    completionRequest(const nlohmann::json& params);
    nlohmann::json
    figureList();
    nlohmann::json
    figureClose(const nlohmann::json& params);
    nlohmann::json
    figureGetImage(const nlohmann::json& params);
    // Notebook: evaluate one cell, capture output + figures it created.
    nlohmann::json
    notebookRun(const nlohmann::json& params);
    // Notebook: release figures a cell kept alive to render interactively.
    nlohmann::json
    notebookCloseFigures(const nlohmann::json& params);
    // Notebook: hand one kept-alive figure back to the desktop (open in window).
    nlohmann::json
    notebookReleaseFigure(const nlohmann::json& params);
    std::vector<long long>
    notebookListFigureHandles();
    void
    interactiveCloseFigure(long long handle);
    nlohmann::json
    figureSaveAs(const nlohmann::json& params);
    nlohmann::json
    figureGetDisplayList(const nlohmann::json& params);
    nlohmann::json
    figureAction(const nlohmann::json& params);
    nlohmann::json
    figureSetView(const nlohmann::json& params);
    nlohmann::json
    figurePan(const nlohmann::json& params);
    nlohmann::json
    figureSetSize(const nlohmann::json& params);
    nlohmann::json
    figureSetPosition(const nlohmann::json& params);
    nlohmann::json
    figureSetWindowSize(const nlohmann::json& params);
    nlohmann::json
    figureKeyEvent(const nlohmann::json& params);
    nlohmann::json
    figureMouseEvent(const nlohmann::json& params);
    nlohmann::json
    figureUndock(const nlohmann::json& params);
    nlohmann::json
    figureDock(const nlohmann::json& params);
    nlohmann::json
    uicontrolAction(const nlohmann::json& params);
    nlohmann::json
    uihtmlPull(const nlohmann::json& params);
    nlohmann::json
    workspaceList();
    nlohmann::json
    historyList();
    nlohmann::json
    historyClear();
    nlohmann::json
    historyRemove(const nlohmann::json& params);
    nlohmann::json
    fsList(const nlohmann::json& params);
    nlohmann::json
    fsCwd();
    nlohmann::json
    fsChdir(const nlohmann::json& params);
    nlohmann::json
    fsDialogStart(const nlohmann::json& params);
    nlohmann::json
    fsDialogNative(const nlohmann::json& params);
    nlohmann::json
    fsDialogAccept(const nlohmann::json& params);
    nlohmann::json
    fsCreate(const nlohmann::json& params);
    nlohmann::json
    fsRename(const nlohmann::json& params);
    nlohmann::json
    fsDelete(const nlohmann::json& params);
    nlohmann::json
    pathList();
    nlohmann::json
    helpText(const nlohmann::json& params);
    nlohmann::json
    examplesList();
    nlohmann::json
    examplesRun(const nlohmann::json& params);
    nlohmann::json
    examplesOpen(const nlohmann::json& params);
    nlohmann::json
    toolDispatch(const nlohmann::json& params);
    nlohmann::json
    profileInfo();
    nlohmann::json
    profileControl(const nlohmann::json& params);
    nlohmann::json
    profileExport(const nlohmann::json& params);
    nlohmann::json
    newsfeedList(const nlohmann::json& params);
    nlohmann::json
    fsUpload(const nlohmann::json& params);
    nlohmann::json
    fsDownload(const nlohmann::json& params);
    nlohmann::json
    varOpen(const nlohmann::json& params);
    nlohmann::json
    varGetBlock(const nlohmann::json& params);
    nlohmann::json
    varSetCell(const nlohmann::json& params);
    nlohmann::json
    varReplace(const nlohmann::json& params);
    nlohmann::json
    shellRun(const nlohmann::json& params);
    nlohmann::json
    varSetNested(const nlohmann::json& params);
    nlohmann::json
    editorOpen(const nlohmann::json& params);
    nlohmann::json
    editorSave(const nlohmann::json& params);
    nlohmann::json
    codeAnalyze(const nlohmann::json& params);
    nlohmann::json
    debuggerGetBreakpoints(const nlohmann::json& params);
    nlohmann::json
    debuggerToggleBreakpoint(const nlohmann::json& params);
    nlohmann::json
    debuggerState();

    // Lazily resolve the nlsGraphics figure bridge (dynamic load, keeps Qt out
    // of nlsWeb_gui). Returns true once the render function is available.
    bool
    ensureGraphicsBridge();

    using ListFiguresFn = int (*)(long long*, int);
    using CloseFigureFn = bool (*)(long long);
    using ClearCurrentFigureFn = void (*)();
    using RenderPngFn = bool (*)(long long, void (*)(void*, const char*, int), void*);
    using RenderDisplayListFn = bool (*)(long long, void (*)(void*, const char*, int), void*);
    using MarkFigureFrameFn = void (*)(long long);
    using SetAxesViewFn = bool (*)(long long, long long, double, double);
    using PanAxesFn = bool (*)(long long, double, double);
    using SetFigureSizeFn = bool (*)(long long, int, int);
    using UIControlActionFn = bool (*)(long long, double, const char*);
    using UiHtmlPullFn = bool (*)(long long, void (*)(void*, const char*, int), void*);
    using FigureKeyEventFn
        = bool (*)(long long, bool, const char*, const char*, bool, bool, bool, bool);
    using FigureMouseEventFn
        = bool (*)(long long, const char*, double, double, bool*, int, bool, bool, bool);
    using SaveFigureFn = bool (*)(long long, const char*);
    using GetWindowStyleFn = int (*)(long long);
    using GetFigureSizeFn = bool (*)(long long, int*, int*);
    using SetFigurePositionFn = bool (*)(long long, int, int, int, int);
    using GetFigureTitleFn = int (*)(long long, char*, int);
    bool m_graphicsBridgeTried { false };
    ListFiguresFn m_listFiguresFn { nullptr };
    CloseFigureFn m_closeFigureFn { nullptr };
    ClearCurrentFigureFn m_clearCurrentFigureFn { nullptr };
    RenderPngFn m_renderPngFn { nullptr };
    RenderDisplayListFn m_displayListFn { nullptr };
    RenderDisplayListFn m_displayListNoFallbackFn { nullptr };
    RenderDisplayListFn m_displayListAnimationFrameFn { nullptr };
    RenderDisplayListFn m_displayListInteractiveFrameFn { nullptr };
    MarkFigureFrameFn m_markViewOnlyFrameFn { nullptr };
    SetAxesViewFn m_setAxesViewFn { nullptr };
    PanAxesFn m_panAxesFn { nullptr };
    SetFigureSizeFn m_setFigureSizeFn { nullptr };
    UIControlActionFn m_uiControlActionFn { nullptr };
    UiHtmlPullFn m_uiHtmlPullFn { nullptr };
    FigureKeyEventFn m_figureKeyEventFn { nullptr };
    FigureMouseEventFn m_figureMouseEventFn { nullptr };
    SaveFigureFn m_saveFigureFn { nullptr };
    GetWindowStyleFn m_getWindowStyleFn { nullptr };
    GetFigureSizeFn m_getFigureSizeFn { nullptr };
    SetFigurePositionFn m_setFigurePositionFn { nullptr };
    GetFigureTitleFn m_getFigureTitleFn { nullptr };
};
//=============================================================================
} // namespace Nelson
//=============================================================================
