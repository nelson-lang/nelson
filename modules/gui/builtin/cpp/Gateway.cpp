//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "NelsonConfiguration.hpp"
#include "NelSon_engine_mode.h"
#include "HtmlToPdfProvider.hpp"
#include "HtmlToPdfWebview.hpp"
#include "RuntimeDispatch.hpp"
#include "UiHtmlHost.hpp"
#include "WebViewer.hpp"
#include "examplesBuiltin.hpp"
#include "filebrowserBuiltin.hpp"
#include "inserthtmlBuiltin.hpp"
#include "historybrowserBuiltin.hpp"
#include "inputdlgBuiltin.hpp"
#include "listdlgBuiltin.hpp"
#include "openvarBuiltin.hpp"
#include "workspacebrowserBuiltin.hpp"
#include "questdlgBuiltin.hpp"
#include "render_htmlBuiltin.hpp"
#include "terminalBuiltin.hpp"
#include "uialertBuiltin.hpp"
#include "uiconfirmBuiltin.hpp"
#include "uigetdirBuiltin.hpp"
#include "uigetfileBuiltin.hpp"
#include "uihtml_spikeBuiltin.hpp"
#include "uiopenBuiltin.hpp"
#include "uiputfileBuiltin.hpp"
#include "uisaveBuiltin.hpp"
#include "uisetcolorBuiltin.hpp"
#include "uisetfontBuiltin.hpp"
#include "webviewBuiltin.hpp"
//=============================================================================
extern "C"
{
#include "webui.h"
}
//=============================================================================
#ifdef _WIN32
#include <cstdlib>
#include <filesystem>
#include <string>
#include <system_error>
#include <vector>
#endif
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"gui";
//=============================================================================
static const nlsGateway gateway[] = {
    { "demo", (ptrBuiltin)Nelson::WebviewGateway::examplesBuiltin, 0, 0 },
    { "__webview__", (ptrBuiltin)Nelson::WebviewGateway::webviewBuiltin, 2, -1 },
    { "__webview_render_html__", (ptrBuiltin)Nelson::WebviewGateway::render_htmlBuiltin, 1, 1 },
    { "__uihtml_spike__", (ptrBuiltin)Nelson::WebviewGateway::uihtml_spikeBuiltin, 2, -1 },
    { "questdlg", (ptrBuiltin)Nelson::WebviewGateway::questdlgBuiltin, 1, 6 },
    { "inputdlg", (ptrBuiltin)Nelson::WebviewGateway::inputdlgBuiltin, 1, 5 },
    { "listdlg", (ptrBuiltin)Nelson::WebviewGateway::listdlgBuiltin, 2, -1 },
    { "uialert", (ptrBuiltin)Nelson::WebviewGateway::uialertBuiltin, 0, -4 },
    { "uiconfirm", (ptrBuiltin)Nelson::WebviewGateway::uiconfirmBuiltin, 1, -4 },
    { "uigetdir", (ptrBuiltin)Nelson::WebviewGateway::uigetdirBuiltin, 1, -1 },
    { "uigetfile", (ptrBuiltin)Nelson::WebviewGateway::uigetfileBuiltin, 3, -1 },
    { "uiputfile", (ptrBuiltin)Nelson::WebviewGateway::uiputfileBuiltin, 3, -1 },
    { "uisetcolor", (ptrBuiltin)Nelson::WebviewGateway::uisetcolorBuiltin, 1, -1 },
    { "uisetfont", (ptrBuiltin)Nelson::WebviewGateway::uisetfontBuiltin, 1, -1 },
    { "uiopen", (ptrBuiltin)Nelson::WebviewGateway::uiopenBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "uisave", (ptrBuiltin)Nelson::WebviewGateway::uisaveBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "workspace", (ptrBuiltin)Nelson::WebviewGateway::workspacebrowserBuiltin, 0, 2 },
    { "commandhistory", (ptrBuiltin)Nelson::WebviewGateway::historybrowserBuiltin, 0, 2 },
    { "filebrowser", (ptrBuiltin)Nelson::WebviewGateway::filebrowserBuiltin, 0, 2 },
    { "openvar", (ptrBuiltin)Nelson::WebviewGateway::openvarBuiltin, 0, 1 },
    { "__terminal__", (ptrBuiltin)Nelson::WebviewGateway::terminalBuiltin, 1, 4 },
    { "inserthtml", (ptrBuiltin)Nelson::WebviewGateway::inserthtmlBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
};
//=============================================================================
#ifdef _WIN32
// NELSON PATCH: launch a detached recursive delete so it outlives this module.
static void
spawnDetachedRemove(const std::wstring& folder)
{
    std::wstring command = L"cmd.exe /c rmdir /s /q \"" + folder + L"\"";
    std::vector<wchar_t> buffer(command.begin(), command.end());
    buffer.push_back(L'\0');
    STARTUPINFOW si;
    PROCESS_INFORMATION pi;
    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    ZeroMemory(&pi, sizeof(pi));
    if (CreateProcessW(nullptr, buffer.data(), nullptr, nullptr, FALSE, CREATE_NO_WINDOW, nullptr,
            nullptr, &si, &pi)) {
        CloseHandle(pi.hThread);
        CloseHandle(pi.hProcess);
    }
}
//=============================================================================
// NELSON PATCH: quarantine and delete WebView2 caches left by dead Nelson runs.
static void
sweepStaleWebViewCacheFolders()
{
    wchar_t* userProfile = nullptr;
    size_t userProfileLen = 0;
    if (_wdupenv_s(&userProfile, &userProfileLen, L"USERPROFILE") != 0 || userProfile == nullptr) {
        return;
    }
    std::filesystem::path base = std::filesystem::path(userProfile) / L".WebUI";
    free(userProfile);
    std::error_code ec;
    if (!std::filesystem::is_directory(base, ec)) {
        return;
    }
    std::vector<std::filesystem::path> candidates;
    std::vector<std::filesystem::path> leftovers;
    // Collect first: moving entries while iterating the same directory is unsafe.
    for (std::filesystem::directory_iterator it(base, ec), end; it != end; it.increment(ec)) {
        if (ec) {
            break;
        }
        const std::wstring name = it->path().filename().wstring();
        if (name.rfind(L"WebUIWebViewCache_", 0) == 0) {
            candidates.push_back(it->path());
        } else if (name.rfind(L".nlsWebUITrash_", 0) == 0) {
            leftovers.push_back(it->path());
        }
    }
    // A leftover quarantine is from an earlier run and always safe to remove.
    for (const std::filesystem::path& old : leftovers) {
        spawnDetachedRemove(old.wstring());
    }
    std::filesystem::path trash
        = base / (L".nlsWebUITrash_" + std::to_wstring(GetCurrentProcessId()));
    bool trashReady = false;
    unsigned int counter = 0;
    for (const std::filesystem::path& entry : candidates) {
        std::error_code localEc;
        if (!std::filesystem::is_directory(entry, localEc)) {
            continue;
        }
        if (!trashReady) {
            std::filesystem::create_directory(trash, localEc);
            trashReady = std::filesystem::is_directory(trash, localEc);
            if (!trashReady) {
                return;
            }
        }
        // Atomic rename fails while a live instance holds the folder, so only dead runs move.
        std::filesystem::rename(entry, trash / std::to_wstring(counter++), localEc);
    }
    if (trashReady) {
        spawnDetachedRemove(trash.wstring());
    }
}
#endif
//=============================================================================
static bool
initializeWebviewModule(Nelson::Evaluator*)
{
#ifdef _WIN32
    // Clean WebView2 user-data folders leaked by previous runs.
    sweepStaleWebViewCacheFolders();
#endif
    // Serve HTML to PDF conversion (htmltopdf) through the nelson_manager seam.
    Nelson::RegisterHtmlToPdfProvider();
    return true;
}
//=============================================================================
NLS_GATEWAY_FUNC_EXTENDED_LAZY(gateway, (void*)initializeWebviewModule)
//=============================================================================
static bool
finishWebviewModule(Nelson::Evaluator*)
{
    Nelson::setHtmlToPdfProvider(nullptr);
    Nelson::UiHtmlCloseAll();
    Nelson::CloseAllWebViewers();
    Nelson::WebviewGateway::closeExamplesWindow();
    // In the desktop (GUI) mode the whole application runs on WebUI: the main
    // thread is still pumping webui_wait_async() while this module unloads on the
    // evaluator thread. Calling webui_clean() here frees WebUI buffers and
    // destroys its mutexes under the main thread (use-after-free, then AV on a
    // destroyed critical section during _webui_wait_clean). The main loop owns
    // the WebUI teardown in that mode, so skip the library-level cleanup.
    if (Nelson::NelsonConfiguration::getInstance()->getNelsonEngineMode()
        != NELSON_ENGINE_MODE::GUI) {
        webui_clean();
    }
    return true;
}
//=============================================================================
NLS_GATEWAY_REMOVE_EXTENDED(gateway, (void*)finishWebviewModule)
//=============================================================================
