//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BlockRegistry.hpp"
#include "SimEngineTypes.hpp"
#include <algorithm>
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
static bool
dashboardPath(const nlohmann::json& binding, std::vector<std::string>& path)
{
    path.clear();
    if (!binding.is_object() || !binding.contains("blockPath")) {
        return false;
    }
    const auto& value = binding["blockPath"];
    if (value.is_string()) {
        const std::string id = value.get<std::string>();
        if (!id.empty()) {
            path.push_back(id);
        }
    } else if (value.is_array()) {
        for (const auto& item : value) {
            if (item.is_string() && !item.get<std::string>().empty()) {
                path.push_back(item.get<std::string>());
            }
        }
    }
    return !path.empty();
}
//=============================================================================
static bool
readSubsystemOutput(const BlockState::SubsystemState& subsystem,
    const std::vector<std::string>& path, size_t pathIndex, int port, std::vector<double>& values,
    std::vector<double>& valuesImag, std::vector<long long>& valuesI64, int& exactType)
{
    if (pathIndex >= path.size()) {
        return false;
    }
    auto found = subsystem.nameToId.find(path[pathIndex]);
    if (found == subsystem.nameToId.end()) {
        return false;
    }
    const BlockId nid = found->second;
    if (nid < 0 || nid >= static_cast<BlockId>(subsystem.blocks.size())
        || nid >= static_cast<BlockId>(subsystem.blockStateVec.size())) {
        return false;
    }
    if (pathIndex + 1 < path.size()) {
        const auto& nested = subsystem.blockStateVec[nid].subsystem;
        return nested
            && readSubsystemOutput(
                *nested, path, pathIndex + 1, port, values, valuesImag, valuesI64, exactType);
    }

    const Block& source = subsystem.blocks[nid];
    if (port < 0 || port >= source.outputs) {
        return false;
    }
    int offset = -1;
    int width = 1;
    exactType = static_cast<int>(SigType::Double);
    if (!subsystem.portBase.empty() && nid < static_cast<BlockId>(subsystem.portBase.size())) {
        const int gpi = subsystem.portBase[nid] + port;
        if (gpi >= 0 && gpi < static_cast<int>(subsystem.sigs.size())) {
            const PortSig& sig = subsystem.sigs[gpi];
            offset = sig.offset;
            width = std::max(1, sig.width());
            exactType = static_cast<int>(sig.type);
            if (sig.isComplex && offset >= 0
                && offset + width <= static_cast<int>(subsystem.outputsImagVec.size())) {
                valuesImag.assign(subsystem.outputsImagVec.begin() + offset,
                    subsystem.outputsImagVec.begin() + offset + width);
            }
            if (portUsesI64(sig) && offset >= 0
                && offset + width <= static_cast<int>(subsystem.outputsI64Vec.size())) {
                valuesI64.assign(subsystem.outputsI64Vec.begin() + offset,
                    subsystem.outputsI64Vec.begin() + offset + width);
            }
        }
    }
    if (offset < 0 && nid < static_cast<BlockId>(subsystem.outputBase.size())) {
        offset = subsystem.outputBase[nid] + port;
    }
    if (offset < 0 || offset + width > static_cast<int>(subsystem.outputsVec.size())) {
        return false;
    }
    values.assign(
        subsystem.outputsVec.begin() + offset, subsystem.outputsVec.begin() + offset + width);
    return true;
}
//=============================================================================
static bool
readDashboardSignal(SimCtx& ctx, const nlohmann::json& binding, std::vector<double>& values,
    std::vector<double>& valuesImag, std::vector<long long>& valuesI64, int& exactType)
{
    if (!binding.is_object() || binding.value("kind", std::string()) != "signal") {
        return false;
    }
    std::vector<std::string> path;
    if (!dashboardPath(binding, path)) {
        return false;
    }
    auto found = ctx.nameToId.find(path.front());
    if (found == ctx.nameToId.end()) {
        return false;
    }
    const BlockId nid = found->second;
    const int port = std::max(1, binding.value("outputPortIndex", 1)) - 1;
    if (nid < 0 || nid >= static_cast<BlockId>(ctx.blocks.size())
        || port >= ctx.blocks[nid].outputs) {
        return false;
    }
    if (path.size() > 1) {
        const auto& subsystem = ctx.blockState[nid].subsystem;
        return subsystem
            && readSubsystemOutput(
                *subsystem, path, 1, port, values, valuesImag, valuesI64, exactType);
    }
    const int offset = slotOf(ctx, nid, port);
    const int width = outputWidth(ctx, nid, port);
    if (offset < 0 || width <= 0 || offset + width > static_cast<int>(ctx.outputs.size())) {
        return false;
    }
    values.assign(ctx.outputs.begin() + offset, ctx.outputs.begin() + offset + width);
    const PortSig* signal = portSigOf(ctx, nid, port);
    exactType = static_cast<int>(signal ? signal->type : SigType::Double);
    if (signal && signal->isComplex && ctx.outputsImag
        && offset + width <= static_cast<int>(ctx.outputsImag->size())) {
        valuesImag.assign(
            ctx.outputsImag->begin() + offset, ctx.outputsImag->begin() + offset + width);
    }
    if (signal && portUsesI64(*signal) && ctx.outputsI64
        && offset + width <= static_cast<int>(ctx.outputsI64->size())) {
        valuesI64.assign(
            ctx.outputsI64->begin() + offset, ctx.outputsI64->begin() + offset + width);
    }
    return true;
}
//=============================================================================
static bool
handleDashboardObserver(SimCtx& ctx, const Block& block, Phase phase)
{
    BlockState& state = getState(ctx, block.nid);
    if (phase == Phase::INIT) {
        state.scalar = 0.0;
        state.scalar2 = 0.0;
        state.displayValues.clear();
        state.displayValuesImag.clear();
        state.displayValuesI64.clear();
        state.scopeSeries.clear();
        state.scopeSeriesImag.clear();
        state.scopeSeriesI64.clear();
        state.xSeries.clear();
        state.scopeExactType = static_cast<int>(SigType::Double);
        state.dashboardTouched = false;
        state.dashboardError.clear();
        return false;
    }
    if (phase != Phase::AFTER_STEP) {
        return false;
    }
    const nlohmann::json binding = asJson(block.params).value("Binding", nlohmann::json());
    std::vector<double> values;
    std::vector<double> valuesImag;
    std::vector<long long> valuesI64;
    int exactType = static_cast<int>(SigType::Double);
    if (!readDashboardSignal(ctx, binding, values, valuesImag, valuesI64, exactType)) {
        state.scalar2 = 0.0;
        return false;
    }
    state.scalar2 = 1.0;
    state.scopeExactType = exactType;
    state.scalar = values.empty() ? 0.0 : values.front();
    state.displayValues = values;
    state.displayValuesImag = valuesImag;
    state.displayValuesI64 = valuesI64;
    state.dashboardTouched = true;
    if (block.type == "dashboardScope") {
        if (state.scopeSeries.size() != values.size()) {
            state.scopeSeries.assign(values.size(), std::vector<double> {});
        }
        if (!valuesImag.empty() && state.scopeSeriesImag.size() != valuesImag.size()) {
            state.scopeSeriesImag.assign(valuesImag.size(), std::vector<double> {});
        }
        if (!valuesI64.empty() && state.scopeSeriesI64.size() != valuesI64.size()) {
            state.scopeSeriesI64.assign(valuesI64.size(), std::vector<long long> {});
        }
        for (size_t i = 0; i < values.size(); ++i) {
            state.scopeSeries[i].push_back(values[i]);
        }
        for (size_t i = 0; i < valuesImag.size(); ++i) {
            state.scopeSeriesImag[i].push_back(valuesImag[i]);
        }
        for (size_t i = 0; i < valuesI64.size(); ++i) {
            state.scopeSeriesI64[i].push_back(valuesI64[i]);
        }
        state.xSeries.push_back(ctx.t);
    }
    return false;
}
//=============================================================================
static bool
handleDashboardControl(SimCtx&, const Block&, Phase)
{
    return false;
}
//=============================================================================
void
registerDashboardHandlers(BlockRegistry& registry)
{
    const char* observers[] = { "dashboardScope", "dashboardDisplay", "dashboardGauge",
        "dashboardHalfGauge", "dashboardQuarterGauge", "dashboardLinearGauge", "dashboardLamp",
        "dashboardMultiStateImage" };
    for (const char* type : observers) {
        registry.registerHandler(
            type, handleDashboardObserver, { Phase::INIT, Phase::AFTER_STEP }, "builtin.dashboard");
    }

    const char* controls[] = { "dashboardEdit", "dashboardKnob", "dashboardSlider",
        "dashboardPushButton", "dashboardRotarySwitch", "dashboardRadioButton", "dashboardComboBox",
        "dashboardCheckBox", "dashboardRockerSwitch", "dashboardSliderSwitch",
        "dashboardToggleSwitch", "dashboardCallbackButton" };
    BlockMetadata metadata;
    metadata.interactive = true;
    for (const char* type : controls) {
        registry.registerHandler(type, handleDashboardControl, {}, "builtin.dashboard", metadata);
    }
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
