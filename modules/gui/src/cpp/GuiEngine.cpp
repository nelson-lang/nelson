//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GuiEngine.hpp"
#include "GuiRpc.hpp"
#include "GuiServer.hpp"
#include "GuiTerminal.hpp"
#include "WebDesktopBridge.hpp"
#include "NotebookKernel.hpp"
#include "NotebookKernelTerminal.hpp"
#include "DapServer.hpp"
#include "DapTerminal.hpp"
#include "DapTransport.hpp"
#include "Evaluator.hpp"
#include "NelsonConfiguration.hpp"
#include "MainThreadTaskQueue.hpp"
#include "RuntimeDispatch.hpp"
#include <cstdio>
#include <memory>
#include <string>
//=============================================================================
namespace Nelson {
void*
NelsonGuiCreateEvaluator(void* context, size_t id, const WebUiLaunchOptions& options)
{
    auto server = std::make_unique<GuiServer>();
    auto terminal = std::make_unique<GuiTerminal>(server.get());
    auto rpc = std::make_unique<GuiRpc>(server.get(), terminal.get());
    server->setDispatcher(rpc.get());
    if (!server->start(L"gui/dist", options)) {
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
NelsonGuiStartFigures(void* evaluator)
{
    auto server = std::make_unique<GuiServer>();
    auto rpc = std::make_unique<GuiRpc>(server.get(), nullptr);
    server->setDispatcher(rpc.get());
    WebUiLaunchOptions options;
    options.figuresOnly = true;
    if (!server->start(L"gui/dist", options)) {
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
void*
NelsonGuiCreateNotebookKernelEvaluator(void* context, size_t id)
{
    auto kernel = std::make_unique<NotebookKernel>();
    auto terminal = std::make_unique<NotebookKernelTerminal>(kernel.get());
    auto evaluator
        = std::make_unique<Evaluator>(static_cast<Context*>(context), terminal.get(), true, id);
    // Headless figure RPC server: no window, pushes routed to the kernel channel.
    auto server = std::make_unique<GuiServer>();
    auto rpc = std::make_unique<GuiRpc>(server.get(), nullptr);
    server->setDispatcher(rpc.get());
    NotebookKernel* kernelPtr = kernel.get();
    server->setPushSink([kernelPtr](const std::string& channel, const std::string& payloadJson) {
        kernelPtr->writePush(channel, payloadJson);
    });
    WebUiLaunchOptions options;
    options.headlessRpc = true;
    if (!server->start(L"gui/dist", options)) {
        std::fprintf(stderr, "Nelson notebook kernel figure server failed: %s\n",
            server->launchError().c_str());
        return nullptr;
    }
    rpc->setEvaluator(evaluator.get());
    server->setEngineReady();
    kernel->setServer(server.get());
    // `--dap-port N`: open a second channel, a TCP Debug Adapter server that shares
    // this kernel's interpreter and figure server. A cell that pauses at a
    // breakpoint routes its prompt to the DAP server (NotebookKernel::nextCellCode),
    // so an editor can step through the cell while the kernel keeps running.
    const int dapPort = NelsonConfiguration::getInstance()->getDapPort();
    if (dapPort > 0) {
        auto transport = std::make_unique<TcpFrameTransport>(dapPort);
        auto dap = std::make_unique<DapServer>(std::move(transport), DapMode::Attach);
        dap->setServer(server.get());
        DapServer* dapRaw = dap.get();
        kernelPtr->setDapServer(dapRaw);
        // The breakpoint prompt loop waits on the DAP server's condition variable:
        // a queued debugger command (CommandQueue::add) or a posted interpreter task
        // must wake it. The task-queue hook also wakes the kernel's cell loop, so
        // both prompts are served whichever is active.
        setInterpreterWakeupHook([dapRaw]() { dapRaw->wake(); });
        MainThreadTaskQueue::getInstance()->setWakeupHook([kernelPtr, dapRaw]() {
            kernelPtr->wake();
            dapRaw->wake();
        });
        dap->start();
        dap.release();
    }
    kernel->start();
    // The kernel, terminal, server and dispatcher live for the whole process, like
    // the figures-only desktop.
    server.release();
    rpc.release();
    terminal.release();
    kernel.release();
    return evaluator.release();
}
//=============================================================================
void*
NelsonGuiCreateDapEvaluator(void* context, size_t id)
{
    auto transport = std::make_unique<StdioFrameTransport>();
    auto dap = std::make_unique<DapServer>(std::move(transport));
    auto terminal = std::make_unique<DapTerminal>(dap.get());
    auto evaluator
        = std::make_unique<Evaluator>(static_cast<Context*>(context), terminal.get(), true, id);
    // Headless figure RPC server: no window, debugger.* dispatched on the
    // interpreter thread, pushes unused by the DAP channel.
    auto server = std::make_unique<GuiServer>();
    auto rpc = std::make_unique<GuiRpc>(server.get(), nullptr);
    server->setDispatcher(rpc.get());
    WebUiLaunchOptions options;
    options.headlessRpc = true;
    if (!server->start(L"gui/dist", options)) {
        std::fprintf(stderr, "Nelson debug adapter figure server failed: %s\n",
            server->launchError().c_str());
        return nullptr;
    }
    rpc->setEvaluator(evaluator.get());
    server->setEngineReady();
    dap->setServer(server.get());
    dap->start();
    // The DAP server, terminal, figure server and dispatcher live for the whole
    // process, like the notebook kernel.
    server.release();
    rpc.release();
    terminal.release();
    dap.release();
    return evaluator.release();
}
//=============================================================================
bool
NelsonGuiWaitUntilLaunched(std::string& error)
{
    error.clear();
    auto* server = GuiServer::activeServer();
    if (server != nullptr && !server->waitUntilLaunched()) {
        error = server->launchError();
        return false;
    }
    return true;
}
//=============================================================================
void
NelsonGuiPumpEvents()
{
    auto* server = GuiServer::activeServer();
    if (server != nullptr) {
        server->processMainThreadEvents();
    }
}
//=============================================================================
void
NelsonGuiShutdown()
{
    auto* server = GuiServer::activeServer();
    if (server != nullptr) {
        server->shutdown();
    }
}
//=============================================================================
// The notebook host lives until process exit, like the other native web windows.
static GuiServer* notebookHostServer = nullptr;
static GuiRpc* notebookHostRpc = nullptr;
//=============================================================================
bool
NelsonGuiOpenNotebook(void* evaluator, const wchar_t* path)
{
    const std::wstring document = path != nullptr ? std::wstring(path) : std::wstring();
    // A desktop already serves the Notebook editor as a panel.
    if (WebDesktopBridge::getInstance()->openWebNotebook(document)) {
        return true;
    }
    if (GuiServer::activeServer() != nullptr || evaluator == nullptr) {
        return false;
    }
    if (notebookHostServer == nullptr) {
        auto server = std::make_unique<GuiServer>();
        WebUiLaunchOptions options;
        options.notebookHost = true;
        if (!server->start(L"gui/dist", options)) {
            std::fprintf(
                stderr, "Nelson Notebook window failed: %s\n", server->launchError().c_str());
            return false;
        }
        // Built after start(), because the dispatcher reads the host mode of the server.
        auto rpc = std::make_unique<GuiRpc>(server.get(), nullptr);
        rpc->setEvaluator(static_cast<Evaluator*>(evaluator));
        server->setDispatcher(rpc.get());
        server->setEngineReady();
        notebookHostServer = server.release();
        notebookHostRpc = rpc.release();
    }
    // start() registered the notebook opener, the same path as the desktop.
    return WebDesktopBridge::getInstance()->openWebNotebook(document);
}
//=============================================================================
void
NelsonGuiCloseNotebookHost()
{
    if (notebookHostServer != nullptr) {
        notebookHostServer->shutdown();
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
