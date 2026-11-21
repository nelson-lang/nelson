//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NFlowKernelHost.hpp"
#include "NFlowSimSession.hpp"
#include "NFlowHeadlessDispatch.hpp"
#include "NFlowNelsonBridge.hpp"
#include "NFlowHelpers.hpp"
#include "BackgroundWorkerPool.hpp"
#include "MainThreadTaskQueue.hpp"
#include "characters_encoding.hpp"
#include <nlohmann/json.hpp>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <stdexcept>
#include <string>
//=============================================================================
namespace {
//=============================================================================
// The opaque host: its own worker pool, the shared simulation session, and the
// dispatch context the nflow.* methods route through.
struct NFlowKernelHostImpl
{
    Nelson::NFlowGateway::BackgroundWorkerPool workers;
    std::shared_ptr<Nelson::NFlowGateway::NFlowSimSession> session;
    Nelson::NFlowUiBindings::NFlowDispatchContext context;
};
//=============================================================================
// Newly allocated C string the caller frees with nlsNFlowKernelFreeString (same
// module, so the allocator matches across DLL boundaries on Windows).
static char*
allocCString(const std::string& value)
{
    char* out = static_cast<char*>(std::malloc(value.size() + 1));
    if (out != nullptr) {
        std::memcpy(out, value.c_str(), value.size() + 1);
    }
    return out;
}
//=============================================================================
} // namespace
//=============================================================================
extern "C" {
//=============================================================================
void*
nlsNFlowKernelCreate(void* evalPtr, NFlowKernelPushFn push, void* pushUser)
{
    try {
        Nelson::Evaluator* eval = static_cast<Nelson::Evaluator*>(evalPtr);
        auto host = std::make_unique<NFlowKernelHostImpl>();

        // Push sink shared by the sim streaming and the library.loaded push.
        auto pushSink
            = [push, pushUser](const std::string& channel, const std::string& payloadJson) {
                  if (push != nullptr) {
                      push(pushUser, channel.c_str(), payloadJson.c_str());
                  }
              };
        // Headless clients can read scopes by notification or by poll, so deliver both.
        auto deliveryBoth = []() { return Nelson::NFlowGateway::NFlowSimDelivery::Both; };
        // Install the workspace bridge on the interpreter thread on the first run, so
        // From/To Workspace read and write the kernel base scope and the marshaling
        // thread id is the interpreter, not the detached worker.
        auto prepareInterpreter = [eval]() {
            Nelson::MainThreadTaskQueue::getInstance()->runOnInterpreterThread(
                [eval](Nelson::Evaluator*) {
                    Nelson::NFlowGateway::ensureNelsonWorkspaceBridge(eval);
                },
                30000);
        };
        host->session = std::make_shared<Nelson::NFlowGateway::NFlowSimSession>(
            eval, pushSink, &host->workers, deliveryBoth, prepareInterpreter);

        // Headless callbacks drive the shared session. SSP, FMU, codegen, modelica,
        // callback and help callbacks are absent: the dispatch reports them
        // unavailable, which is fine for the phase-1 kernel surface.
        Nelson::NFlowUiBindings::NFlowRpcCallbacks callbacks;
        auto session = host->session;
        callbacks.runSimulation
            = [session](const std::string& diagramJson) { return session->runSync(diagramJson); };
        callbacks.startSimulation = [session](const std::string& diagramJson) {
            return session->startAsync(diagramJson);
        };
        callbacks.pollSimulation
            = [session](const std::string& runId) { return session->poll(runId); };
        callbacks.pauseSimulation = [session](bool wantPause) { session->pause(wantPause); };
        callbacks.stopSimulation = [session]() { session->stop(); };
        callbacks.isSimulationRunning = [session]() { return session->isRunning(); };
        callbacks.sendEvent = [session](const std::string& name, const std::string& data) {
            session->sendEvent(name, data);
        };
        // Headless: never an embedded WebView, so nflow.fs.nativeDialog degrades to
        // the not_webview response.
        callbacks.isEmbeddedWebView = []() { return false; };

        host->context.push = pushSink;
        host->context.nativeWindowHandle = nullptr;
        host->context.callbacks = std::move(callbacks);
        return host.release();
    } catch (...) {
        return nullptr;
    }
}
//=============================================================================
char*
nlsNFlowKernelDispatch(void* host, const char* method, const char* paramsJson)
{
    nlohmann::json out;
    try {
        auto impl = static_cast<NFlowKernelHostImpl*>(host);
        if (impl == nullptr || method == nullptr) {
            throw std::runtime_error("invalid nflow kernel host");
        }
        nlohmann::json params = nlohmann::json::object();
        if (paramsJson != nullptr && paramsJson[0] != '\0') {
            params = nlohmann::json::parse(paramsJson);
        }
        out["result"] = Nelson::NFlowUiBindings::dispatchNFlowRpc(impl->context, method, params);
    } catch (const std::exception& ex) {
        out = nlohmann::json { { "error", { { "message", ex.what() } } } };
    } catch (...) {
        out = nlohmann::json { { "error", { { "message", "unknown nflow kernel error" } } } };
    }
    return allocCString(out.dump());
}
//=============================================================================
char*
nlsNFlowKernelDistDir(void)
{
    try {
        return allocCString(Nelson::wstring_to_utf8(Nelson::getNFlowDistributionDirectory()));
    } catch (...) {
        return nullptr;
    }
}
//=============================================================================
void
nlsNFlowKernelFreeString(char* s)
{
    std::free(s);
}
//=============================================================================
void
nlsNFlowKernelDestroy(void* host)
{
    auto impl = static_cast<NFlowKernelHostImpl*>(host);
    if (impl == nullptr) {
        return;
    }
    // Cancel any run and join the worker before releasing the session. A worker
    // blocked on an interpreter hand-off unblocks when the kernel drains
    // MainThreadTaskQueue, or after the 30 s bridge timeout.
    if (impl->session) {
        impl->session->shutdown();
    }
    delete impl;
}
//=============================================================================
} // extern "C"
//=============================================================================
