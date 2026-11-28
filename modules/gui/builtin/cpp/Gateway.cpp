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
static bool
initializeWebviewModule(Nelson::Evaluator*)
{
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
