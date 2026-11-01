//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebGuiEngine.hpp"
#include "WebGuiRpc.hpp"
#include "WebGuiServer.hpp"
#include "WebGuiTerminal.hpp"
#include "WebDesktopBridge.hpp"
#include "Evaluator.hpp"
#include <cstdio>
#include <memory>
//=============================================================================
namespace Nelson {
void*
NelsonWebGuiCreateEvaluator(void* context, size_t id, const WebUiLaunchOptions& options)
{
    auto server = std::make_unique<WebGuiServer>();
    auto terminal = std::make_unique<WebGuiTerminal>(server.get());
    auto rpc = std::make_unique<WebGuiRpc>(server.get(), terminal.get());
    server->setDispatcher(rpc.get());
    if (!server->start(L"web_gui/frontend/dist", options)) {
        std::fprintf(stderr, "Nelson web launch failed: %s\n", server->launchError().c_str());
        return nullptr;
    }
    // The terminal REPL must drain callbacks even without a desktop event loop.
    auto evaluator
        = std::make_unique<Evaluator>(static_cast<Context*>(context), terminal.get(), true, id);
    rpc->setEvaluator(evaluator.get());
    server.release();
    terminal.release();
    rpc.release();
    return evaluator.release();
}
//=============================================================================
void
NelsonWebGuiStartFigures(void* evaluator)
{
    auto server = std::make_unique<WebGuiServer>();
    auto rpc = std::make_unique<WebGuiRpc>(server.get(), nullptr);
    server->setDispatcher(rpc.get());
    WebUiLaunchOptions options;
    options.figuresOnly = true;
    if (!server->start(L"web_gui/frontend/dist", options)) {
        std::fprintf(
            stderr, "Nelson web figures launch failed: %s\n", server->launchError().c_str());
        return;
    }
    rpc->setEvaluator(static_cast<Evaluator*>(evaluator));
    // Advanced CLI services the server on its evaluator thread through RuntimeDispatch.
    server->setEngineReady();
    server.release();
    rpc.release();
}
//=============================================================================
bool
NelsonWebGuiWaitUntilLaunched(std::string& error)
{
    error.clear();
    auto* server = WebGuiServer::activeServer();
    if (server != nullptr && !server->waitUntilLaunched()) {
        error = server->launchError();
        return false;
    }
    return true;
}
//=============================================================================
void
NelsonWebGuiPumpEvents()
{
    auto* server = WebGuiServer::activeServer();
    if (server != nullptr) {
        server->processMainThreadEvents();
    }
}
//=============================================================================
void
NelsonWebGuiShutdown()
{
    auto* server = WebGuiServer::activeServer();
    if (server != nullptr) {
        server->shutdown();
    }
}
//=============================================================================
// The notebook host lives until process exit, like the other native web windows.
static WebGuiServer* notebookHostServer = nullptr;
static WebGuiRpc* notebookHostRpc = nullptr;
//=============================================================================
bool
NelsonWebGuiOpenNotebook(void* evaluator, const wchar_t* path)
{
    const std::wstring document = path != nullptr ? std::wstring(path) : std::wstring();
    // A web desktop already serves the Notebook editor as a panel.
    if (WebDesktopBridge::getInstance()->openWebNotebook(document)) {
        return true;
    }
    if (WebGuiServer::activeServer() != nullptr || evaluator == nullptr) {
        return false;
    }
    if (notebookHostServer == nullptr) {
        auto server = std::make_unique<WebGuiServer>();
        WebUiLaunchOptions options;
        options.notebookHost = true;
        if (!server->start(L"web_gui/frontend/dist", options)) {
            std::fprintf(
                stderr, "Nelson Notebook window failed: %s\n", server->launchError().c_str());
            return false;
        }
        // Built after start(), because the dispatcher reads the host mode of the server.
        auto rpc = std::make_unique<WebGuiRpc>(server.get(), nullptr);
        rpc->setEvaluator(static_cast<Evaluator*>(evaluator));
        server->setDispatcher(rpc.get());
        server->setEngineReady();
        notebookHostServer = server.release();
        notebookHostRpc = rpc.release();
    }
    // start() registered the notebook opener, the same path as the web desktop.
    return WebDesktopBridge::getInstance()->openWebNotebook(document);
}
//=============================================================================
void
NelsonWebGuiCloseNotebookHost()
{
    if (notebookHostServer != nullptr) {
        notebookHostServer->shutdown();
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
