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
#include <functional>
#include <memory>
#include <mutex>
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
    // Installs the workspace bridge on the interpreter thread exactly once, shared by
    // the sim worker and the synchronous SSP/FMU/modelica/codegen dispatches.
    std::function<void()> ensureBridge;
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
extern "C"
{
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
            // Install the workspace bridge on the interpreter thread exactly once, from
            // whichever reaches the interpreter first: a sim worker (through the session's
            // prepareInterpreter) or a synchronous SSP/FMU/modelica/codegen dispatch on the
            // reader thread. From/To Workspace then read and write the kernel base scope and
            // the marshaling thread id is the interpreter, not a detached worker.
            auto bridgeOnce = std::make_shared<std::once_flag>();
            auto ensureBridge = [eval, bridgeOnce]() {
                std::call_once(*bridgeOnce, [eval]() {
                    Nelson::MainThreadTaskQueue::getInstance()->runOnInterpreterThread(
                        [eval](Nelson::Evaluator*) {
                            Nelson::NFlowGateway::ensureNelsonWorkspaceBridge(eval);
                        },
                        30000);
                });
            };
            host->ensureBridge = ensureBridge;
            host->session = std::make_shared<Nelson::NFlowGateway::NFlowSimSession>(
                eval, pushSink, &host->workers, deliveryBoth, ensureBridge);

            // Headless callbacks drive the shared session plus the SSP/FMU/modelica
            // bridges. The callback (dashboard eval) and help callbacks stay absent: the
            // extension serves help from its own UI, and the dispatch reports them
            // unavailable until a surface needs them.
            Nelson::NFlowUiBindings::NFlowRpcCallbacks callbacks;
            auto session = host->session;
            callbacks.runSimulation = [session](const std::string& diagramJson) {
                return session->runSync(diagramJson);
            };
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
            // SSP / FMU / modelica run Nelson code on the interpreter thread through the
            // engine bridge, the same free functions the editor window binds. The reader
            // thread blocks on the synchronous dispatch while the interpreter thread runs
            // the marshalled work (the bridge is installed first by ensureBridge), so these
            // return their result inline.
            callbacks.importSsp = [](const std::string& sspPath) -> std::string {
                std::string errorMessage;
                const std::string nflowPath = Nelson::NFlowGateway::nflowSspImportViaInterpreter(
                    sspPath, std::string {}, errorMessage);
                nlohmann::json out;
                out["success"] = errorMessage.empty() && !nflowPath.empty();
                out["path"] = nflowPath;
                if (!errorMessage.empty()) {
                    out["error"] = errorMessage;
                }
                return out.dump();
            };
            callbacks.exportSsp
                = [](const std::string& nflowContent, const std::string& sspPath) -> std::string {
                std::string errorMessage;
                const std::string produced
                    = Nelson::NFlowGateway::nflowSspExportDiagramViaInterpreter(
                        nflowContent, sspPath, errorMessage);
                nlohmann::json out;
                out["success"] = errorMessage.empty() && !produced.empty();
                out["path"] = produced;
                if (!errorMessage.empty()) {
                    out["error"] = errorMessage;
                }
                return out.dump();
            };
            callbacks.exportFmu
                = [](const std::string& nflowContent, const std::string& fmuPath) -> std::string {
                std::string errorMessage;
                const std::string produced = Nelson::NFlowGateway::nflowExportFmuViaInterpreter(
                    nflowContent, fmuPath, errorMessage);
                nlohmann::json out;
                out["success"] = errorMessage.empty() && !produced.empty();
                out["path"] = produced;
                if (!errorMessage.empty()) {
                    out["error"] = errorMessage;
                }
                return out.dump();
            };
            callbacks.modelicaInfo = []() -> std::string {
                std::string errorMessage;
                const std::string info
                    = Nelson::NFlowGateway::nflowModelicaInfoViaInterpreter(errorMessage);
                if (!errorMessage.empty()) {
                    throw std::runtime_error(errorMessage);
                }
                return info;
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
            // Methods that run Nelson code on the interpreter thread need the workspace
            // bridge installed first. Install it once, here on the reader thread, before
            // the synchronous dispatch marshals its work; sim installs it the same way from
            // its worker and the shared guard makes it a single install. Pure methods
            // (library, router, expr, fmi3.inspect, examples, model registry) skip it.
            const std::string methodName(method);
            const auto needsBridge = [](const std::string& m) {
                return m.rfind("nflow.sim.", 0) == 0 || m.rfind("nflow.codegen.", 0) == 0
                    || m.rfind("nflow.ssp.", 0) == 0 || m.rfind("nflow.fmu.", 0) == 0
                    || m == "nflow.modelica.info";
            };
            if (impl->ensureBridge && needsBridge(methodName)) {
                impl->ensureBridge();
            }
            out["result"]
                = Nelson::NFlowUiBindings::dispatchNFlowRpc(impl->context, methodName, params);
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
