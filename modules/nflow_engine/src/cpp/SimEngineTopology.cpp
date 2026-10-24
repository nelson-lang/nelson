//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _USE_MATH_DEFINES
#include "SimEngineTopology.hpp"
#include "SimEngineResult.hpp"
#include "DashboardBindingManager.hpp"
#include "SimEngineBusTypes.hpp"
#include "FieldNames.hpp"
#include <algorithm>
#include <cmath>
#include <utility>
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
std::optional<std::string>
buildModelTopology(const nlohmann::json& diagram, ModelTopology& out)
{
    std::vector<Block> blocks;
    std::unordered_map<std::string, BlockId> nameToId;
    if (diagram.contains(nflow::kBlocks) && diagram[nflow::kBlocks].is_array()) {
        for (const auto& bj : diagram[nflow::kBlocks]) {
            Block b;
            b.id = bj.value(nflow::kId, "");
            b.type = bj.value(nflow::kType, "");
            b.inputs = bj.value(nflow::kInputs, 0);
            b.outputs = bj.value(nflow::kOutputs, 1);
            b.params = bj.value(nflow::kParams, json::object());
            // The editor writes the port count both as a block field and as a
            // parameter. A hand- or script-written model often carries only the
            // parameter; without this a demux keeps the default single port and
            // silently drops every element but the first.
            if (b.type == "demux" && !bj.contains(nflow::kOutputs)) {
                const auto it = b.params.find("Outputs");
                if (it != b.params.end() && it->is_number()) {
                    b.outputs = (std::max)(1, it->get<int>());
                }
            }
            if (!b.id.empty() && !b.type.empty()) {
                b.nid = static_cast<BlockId>(blocks.size());
                nameToId[b.id] = b.nid;
                blocks.push_back(std::move(b));
            }
        }
    }

    bool hasScope = false;
    for (const Block& b : blocks) {
        if (b.type == "scope" || b.type == "xyScope" || b.type == "xyzScope"
            || b.type == nflow::kDisplay || b.type == "toWorkspace") {
            hasScope = true;
            break;
        }
    }
    if (!hasScope) {
        return "{\"status\":\"error\",\"reason\":\"no_scope\",\"scopes\":[]}";
    }

    std::unordered_map<std::string, double> variables;
    variables[nflow::kPi] = M_PI;
    variables[nflow::kE] = M_E;
    if (diagram.contains(nflow::kVariables) && diagram[nflow::kVariables].is_object()) {
        for (const auto& [k, v] : diagram[nflow::kVariables].items()) {
            flattenDashboardVariable(k, v, variables);
        }
    }

    BusRegistry busRegistry;
    {
        std::string busErr;
        if (!parseBusRegistry(diagram, busRegistry, busErr)) {
            const std::vector<double> noTime;
            const std::vector<BlockState> noState(blocks.size());
            std::vector<std::vector<int>> noInputs(blocks.size());
            return buildOutputJson("error", busErr, noTime, blocks, noState, noInputs, "[]");
        }
    }

    double dt = 0.01;
    if (diagram.contains(nflow::kSampleTime) && diagram[nflow::kSampleTime].is_number()) {
        dt = diagram[nflow::kSampleTime].get<double>();
    } else if (variables.count(nflow::kDt)) {
        dt = variables.at(nflow::kDt);
    } else if (variables.count(nflow::kSampleTime)) {
        dt = variables.at(nflow::kSampleTime);
    }
    dt = std::max(1e-6, dt);

    // End time: an explicit `duration` wins (sim() injects it from StopTime), then
    // the diagram's own `runtime` field, which the GUI and every saved .nflow
    // document store. Falls back to 10 s.
    double duration = 10.0;
    if (diagram.contains(nflow::kDuration) && diagram[nflow::kDuration].is_number()) {
        duration = diagram[nflow::kDuration].get<double>();
    } else if (diagram.contains(nflow::kRuntime) && diagram[nflow::kRuntime].is_number()) {
        duration = diagram[nflow::kRuntime].get<double>();
    }
    duration = std::max(duration, dt * 10.0);

    int samples = (int)std::floor(duration / dt);

    std::vector<SignalConn> connList;
    if (diagram.contains(nflow::kConnections) && diagram[nflow::kConnections].is_array()) {
        for (const auto& cj : diagram[nflow::kConnections]) {
            std::string from = cj.value(nflow::kFrom, "");
            std::string to = cj.value(nflow::kTo, "");
            int fromIdx = cj.value(nflow::kFromIndex, 0);
            int toIdx = cj.value(nflow::kToIndex, 0);

            auto toNit = nameToId.find(to);
            auto fromNit = nameToId.find(from);
            if (toNit == nameToId.end() || fromNit == nameToId.end()) {
                continue;
            }
            connList.push_back({ fromNit->second, fromIdx, toNit->second, toIdx });
        }
        // Grow a block's port count to what the wires address. A hand- or
        // script-written model often omits the counts the editor writes, and the
        // block would keep a single port (a two-output subsystem delivered a flat
        // zero for its second output). Inner diagrams already infer this way (see
        // buildSubsystem). The declared count is never reduced.
        for (const SignalConn& c : connList) {
            Block& src = blocks[static_cast<size_t>(c.fromNid)];
            src.outputs = (std::max)(src.outputs, c.fromPort + 1);
            Block& dst = blocks[static_cast<size_t>(c.toNid)];
            dst.inputs = (std::max)(dst.inputs, c.toPort + 1);
        }
    }

    std::unordered_map<std::string, std::string> labelSinks;
    for (const Block& b : blocks) {
        if (b.type != nflow::kLabelSink) {
            continue;
        }
        if (!b.params.contains(nflow::kGotoTag) || !b.params[nflow::kGotoTag].is_string()) {
            continue;
        }
        std::string name = b.params[nflow::kGotoTag].get<std::string>();
        if (!name.empty()) {
            labelSinks[name] = b.id;
        }
    }

    // Signal layout: propagate port dimensions and assign each output port its
    // contiguous slot range in the flat outputs buffer.
    SignalLayout sigLayout = buildSignalLayout(
        blocks, connList, labelSinks, nameToId, variables, nullptr, &busRegistry);
    if (!sigLayout.ok) {
        std::string errBlocks = "\"errorBlocks\":[";
        for (size_t k = 0; k < sigLayout.errorBlocks.size(); ++k) {
            if (k) {
                errBlocks += ",";
            }
            errBlocks += "\"" + escapeJsonString(sigLayout.errorBlocks[k]) + "\"";
        }
        errBlocks += "]";
        const std::vector<double> noTime;
        const std::vector<BlockState> noState(blocks.size());
        std::vector<std::vector<int>> noInputs(blocks.size());
        return buildOutputJson(
            "error", sigLayout.error, noTime, blocks, noState, noInputs, errBlocks);
    }

    // outputBase[nid] = first output slot for that block (port 0 offset)
    std::vector<int> outputBase(blocks.size(), 0);
    for (int i = 0; i < (int)blocks.size(); ++i) {
        outputBase[i] = (blocks[i].outputs > 0) ? sigLayout.sigs[sigLayout.portBase[i]].offset
                                                : sigLayout.totalSlots;
    }
    const int totalSlots = sigLayout.totalSlots;

    // inputIds[nid][portIdx] = source output slot (-1 if unconnected)
    std::vector<std::vector<int>> inputIds(blocks.size());
    for (int i = 0; i < (int)blocks.size(); ++i) {
        inputIds[i].assign(sigLayout.inputSigs[i].size(), -1);
        for (size_t p = 0; p < sigLayout.inputSigs[i].size(); ++p) {
            const int gpi = sigLayout.inputSigs[i][p];
            if (gpi >= 0) {
                inputIds[i][p] = sigLayout.sigs[gpi].offset;
            }
        }
    }

    out.blocks = std::move(blocks);
    out.nameToId = std::move(nameToId);
    out.variables = std::move(variables);
    out.busRegistry = std::move(busRegistry);
    out.dt = dt;
    out.samples = samples;
    out.labelSinks = std::move(labelSinks);
    out.sigLayout = std::move(sigLayout);
    out.outputBase = std::move(outputBase);
    out.inputIds = std::move(inputIds);
    out.totalSlots = totalSlots;
    return std::nullopt;
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
