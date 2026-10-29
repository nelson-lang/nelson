//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SimEngineEvents.hpp"
#include "DashboardBindingManager.hpp"
#include "SimEventBridge.hpp"
#include <iostream>
#include <nlohmann/json.hpp>
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
static Block*
topLevelBlock(std::vector<Block>& blocks, const std::string& id)
{
    for (Block& block : blocks) {
        if (block.id == id) {
            return &block;
        }
    }
    return nullptr;
}
//=============================================================================
static void
recordDashboardEventError(std::vector<Block>& blocks, std::vector<BlockState>& states,
    const DashboardBindingDiagnostic& error, SimDiagnostics* diagnostics)
{
    Block* block = topLevelBlock(blocks, error.blockId);
    if (block && block->nid >= 0 && block->nid < static_cast<BlockId>(states.size())) {
        states[block->nid].dashboardError = error.message;
        states[block->nid].dashboardTouched = true;
    }
    if (diagnostics) {
        SimDiagnostic warning;
        warning.code = "dashboard_binding";
        warning.blockId = error.blockId;
        warning.message = error.message;
        diagnostics->warn(std::move(warning));
    }
    std::clog << "[NFlow] " << error.message << std::endl;
}
//=============================================================================
static void
applyLegacyEvent(std::vector<Block>& blocks, const json& event, const std::string& channel)
{
    const std::string type = event.value("type", std::string());
    const std::string id = event.value("id", std::string());
    Block* target = topLevelBlock(blocks, id);
    if (!target) {
        return;
    }
    if (type == "setParams" && event.contains("params") && event["params"].is_object()) {
        for (const auto& item : event["params"].items()) {
            asJson(target->params)[item.key()] = item.value();
        }
        return;
    }
    if (type == "setParam" || channel == "nflow:setParam" || channel == "SimControl"
        || channel == "SimEvent") {
        const std::string key = event.value("key", std::string());
        if (!key.empty() && event.contains("value")) {
            asJson(target->params)[key] = event["value"];
        }
    }
}
//=============================================================================
void
applyQueuedSimEvents(std::vector<Block>& blocks, std::vector<BlockState>& blockState,
    ValMap& variables, SimDiagnostics* diagnostics)
{
    std::vector<SimEvent> events;
    try {
        events = fetchSimEvents();
    } catch (const std::exception& exception) {
        std::clog << "[NFlow] failed to fetch simulation events: " << exception.what() << std::endl;
        return;
    }
    for (const SimEvent& event : events) {
        try {
            const json payload = json::parse(event.second);
            const std::string type = payload.value("type", std::string());
            if (type == "setDashboardValue") {
                DashboardBindingDiagnostic error;
                if (!applyDashboardBindingValue(blocks, blockState, variables, payload, error)) {
                    recordDashboardEventError(blocks, blockState, error, diagnostics);
                }
            } else if (type == "invokeCallback") {
                // dashboardCallbackButton: runs a Nelson snippet via the registered
                // invoker (portable runtime only; see SimEventBridge).
                std::string err;
                if (!invokeSimCallback(payload.value("code", std::string()),
                        payload.value("blockId", std::string()), err)
                    && !err.empty()) {
                    DashboardBindingDiagnostic diag;
                    diag.blockId = payload.value("blockId", std::string());
                    diag.message = err;
                    recordDashboardEventError(blocks, blockState, diag, diagnostics);
                }
            } else {
                applyLegacyEvent(blocks, payload, event.first);
            }
        } catch (const std::exception& exception) {
            DashboardBindingDiagnostic error;
            error.message = std::string("invalid simulation event: ") + exception.what();
            recordDashboardEventError(blocks, blockState, error, diagnostics);
        }
    }
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
