//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nflow_simulateBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
#include "NFlowBuiltinHelpers.hpp"
#include "NFlowNelsonBridge.hpp"
#include "NFlowWorkspaceIO.hpp"
#include "SimEngine.hpp"
#include "SimEventBridge.hpp"
#include "Warning.hpp"
#include "nlohmann/json.hpp"
#ifdef __EMSCRIPTEN__
#include "Evaluator.hpp"
#include "NelsonException.hpp"
#include <cstdlib>
#include <emscripten/emscripten.h>
#include <string>
#include <vector>
#endif
//=============================================================================
using namespace Nelson;
//=============================================================================
#ifdef __EMSCRIPTEN__
static bool
hasWasmNFlowProgressHook()
{
    // clang-format off
    return EM_ASM_INT({ return typeof Module['onNelsonNFlowPartial'] == 'function'; }) != 0;
    // clang-format on
}
//=============================================================================
static void
publishWasmNFlowProgress(const std::string& json)
{
    // clang-format off
    EM_ASM(
        {
            const callback = Module['onNelsonNFlowPartial'];
            if (typeof callback == 'function') {
                callback(UTF8ToString($0, $1));
            }
        },
        json.data(), static_cast<int>(json.size()));
    // clang-format on
}
//=============================================================================
static bool
wasmNFlowShouldCancel()
{
    // clang-format off
    return EM_ASM_INT(
        {
            const callback = Module['onNelsonNFlowShouldCancel'];
            return typeof callback == 'function' && callback() ? 1 : 0;
        }) != 0;
    // clang-format on
}
//=============================================================================
// Drain UI-sent simulation events (e.g. live dashboardSlider tuning) through the
// host. The JS hook returns a JSON array [{name, data}, ...] of pending events, or
// an empty string when nothing changed since the last poll. They feed the shared
// SimEventBridge, so applyQueuedSimEvents applies them between steps as on native.
static std::vector<Nelson::NFlow::SimEvent>
wasmFetchNFlowEvents()
{
    std::vector<Nelson::NFlow::SimEvent> events;
    // Encode the JSON batch in JS and return only its byte length; the C++ side
    // owns the buffer (std::vector) and JS copies the bytes into it with
    // HEAPU8.set. This avoids relying on _malloc / stringToUTF8 being exported
    // to the JS runtime, which they are not in this build.
    // clang-format off
    int len = EM_ASM_INT({
        const callback = Module['onNelsonNFlowFetchEvents'];
        const s = (typeof callback == 'function') ? (callback() || "") : "";
        if (!s) { Module.__nflowFetchBytes = null; return 0; }
        Module.__nflowFetchBytes = new TextEncoder().encode(s);
        return Module.__nflowFetchBytes.length;
    });
    // clang-format on
    if (len <= 0) {
        return events;
    }
    std::vector<char> buffer(static_cast<size_t>(len) + 1, '\0');
    // clang-format off
    EM_ASM({
        HEAPU8.set(Module.__nflowFetchBytes, $0);
        Module.__nflowFetchBytes = null;
    }, buffer.data(), len);
    // clang-format on
    std::string json(buffer.data(), static_cast<size_t>(len));
    if (json.empty()) {
        return events;
    }
    try {
        nlohmann::json batch = nlohmann::json::parse(json);
        if (batch.is_array()) {
            for (const auto& item : batch) {
                std::string name = item.value("name", std::string());
                std::string data = item.value("data", std::string());
                if (!data.empty()) {
                    events.emplace_back(std::move(name), std::move(data));
                }
            }
        }
    } catch (const std::exception&) {
        // Ignore a malformed event batch rather than aborting the run.
    }
    return events;
}
#endif
//=============================================================================
// __nflow_simulate__(diagramJson [, overridesJson]) -> resultJson
// The diagram is passed as its raw JSON string (never round-tripped through
// jsondecode, which would homogenize heterogeneous / nested block struct arrays
// and corrupt a subsystem's inner diagram). Optional scalar run overrides
// (sampleTime, duration, solver, relTol, absTol, maxStep, variables) arrive as a
// small JSON object that is overlaid onto the raw diagram's top-level keys.
ArrayOfVector
Nelson::NFlowGateway::nflow_simulateBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 2);
    std::string diagramJson = wstring_to_utf8(argIn[0].asWideString());
    if (argIn.size() >= 2 && !argIn[1].isEmpty()) {
        try {
            nlohmann::json diagram = nlohmann::json::parse(diagramJson);
            nlohmann::json overrides
                = nlohmann::json::parse(wstring_to_utf8(argIn[1].asWideString()));
            if (overrides.is_object()) {
                for (const auto& [key, value] : overrides.items()) {
                    diagram[key] = value;
                }
                diagramJson = diagram.dump();
            }
        } catch (const std::exception&) {
            // Leave diagramJson unchanged; runSimulation reports the parse error.
        }
    }
    ensureNelsonWorkspaceBridge(eval);
    ArrayOfVector retval;
    std::string errorJson;
    if (!Nelson::NFlow::resolveWorkspaceInputs(diagramJson, errorJson)) {
        retval << ArrayOf::characterArray(utf8_to_wstring(errorJson));
        return retval;
    }
    // Acausal islands under a non-DAE solver go through the linear-island reduction
    // that code generation uses, so a linear SISO island becomes a causal stateSpace
    // block that every loop (fixed-step, ode1/ode4/ode45, CVODES) integrates at full
    // order. Islands that stay (non-linear or non-SISO) take the engine's embedded
    // implicit step under the native steppers, or its typed rejection under the
    // CVODES-session backends. 'dae'/'idas' integrate the island residual directly
    // and stay the accuracy reference.
    if (diagramJson.find("\"physicalIsland\"") != std::string::npos) {
        std::string solverName;
        try {
            nlohmann::json d = nlohmann::json::parse(diagramJson);
            if (d.contains("solver") && d["solver"].is_string()) {
                solverName = d["solver"].get<std::string>();
            }
        } catch (const std::exception&) {
            // Leave diagramJson unchanged; runSimulation reports the parse error.
        }
        if (solverName != "dae" && solverName != "idas") {
            std::string reduceError;
            std::string reduced = Nelson::NFlowGateway::nflowReduceLinearIslandsViaInterpreter(
                diagramJson, reduceError);
            if (reduceError.empty() && !reduced.empty()) {
                diagramJson = reduced;
            }
        }
    }
    // The engine checks each block's numeric parameters against what the
    // block library declares, so the manifests must be loaded first.
    ensureCatalogLoaded();
    Nelson::NFlow::SimProgressCallback progressCallback;
    std::function<bool()> cancellationCallback;
#ifdef __EMSCRIPTEN__
    if (hasWasmNFlowProgressHook()) {
        progressCallback = publishWasmNFlowProgress;
    }
    cancellationCallback = wasmNFlowShouldCancel;
    // Route live UI events (dashboardSlider tuning) into the engine event pipeline
    // for this run. Native/webview installs the equivalent fetcher from nflow_gui,
    // which is absent in the portable build.
    Nelson::NFlow::setSimEventFetcher(wasmFetchNFlowEvents);
    // dashboardCallbackButton: evaluate the Nelson snippet re-entrantly on this
    // (single) thread between steps. The native build dispatches it on its own
    // RPC thread instead; here the worker thread is the interpreter thread.
    Nelson::NFlow::setSimCallbackInvoker(
        [eval](const std::string& code, const std::string& blockId, std::string& err) -> bool {
            (void)blockId;
            try {
                return eval != nullptr && eval->evaluateString(code, true);
            } catch (NelsonException& exception) {
                err = wstring_to_utf8(exception.what());
                return false;
            } catch (const std::exception& exception) {
                err = exception.what();
                return false;
            } catch (...) {
                err = "callback evaluation failed";
                return false;
            }
        });
#endif
    std::string result
        = Nelson::NFlow::runSimulation(diagramJson, 250, progressCallback, cancellationCallback);
#ifdef __EMSCRIPTEN__
    Nelson::NFlow::setSimEventFetcher(std::function<std::vector<Nelson::NFlow::SimEvent>()>());
    Nelson::NFlow::setSimCallbackInvoker(Nelson::NFlow::SimCallbackInvoker());
#endif
    for (const std::string& writeError : Nelson::NFlow::flushWorkspaceOutputs(result)) {
        RaiseWarning(L"Nelson:nflow:workspaceOutputWriteFailed", utf8_to_wstring(writeError));
    }
    retval << ArrayOf::characterArray(utf8_to_wstring(result));
    return retval;
}
//=============================================================================
