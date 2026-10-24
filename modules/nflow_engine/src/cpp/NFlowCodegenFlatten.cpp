//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NFlowCodegenFlatten.hpp"
#include "NFlowCodegenHelpers.hpp"
#include "FieldNames.hpp"
#include <cmath>
#include <set>
#include <string>
#include <vector>
//=============================================================================
namespace nflow {
std::string
DiagramFlattener::connKey(const json& conn)
{
    return jstr(conn, nflow::kFrom) + ":" + std::to_string(conn.value(nflow::kFromIndex, 0)) + "->"
        + jstr(conn, nflow::kTo) + ":" + std::to_string(conn.value(nflow::kToIndex, 0));
}
//=============================================================================
json
DiagramFlattener::normalizeConnection(const json& conn)
{
    return { { nflow::kFrom, jstr(conn, nflow::kFrom) }, { nflow::kTo, jstr(conn, nflow::kTo) },
        { nflow::kFromIndex, conn.value(nflow::kFromIndex, 0) },
        { nflow::kToIndex, conn.value(nflow::kToIndex, 0) } };
}
//=============================================================================
json
DiagramFlattener::flattenOneSubsystem(const json& diagram, const json& subsystemBlock)
{
    json spec = jval(jval(subsystemBlock, nflow::kParams), nflow::kSubsystem);
    if (spec.is_null() || !spec.contains(nflow::kBlocks) || !spec[nflow::kBlocks].is_array()
        || !spec.contains(nflow::kConnections) || !spec[nflow::kConnections].is_array()) {
        return diagram;
    }

    std::string subId = jstr(subsystemBlock, nflow::kId);
    std::string prefix = subId + "/";

    auto remapId = [&](const std::string& id) -> std::string { return prefix + id; };

    auto remapInnerBinding = [&](json& block) {
        if (jstr(block, nflow::kType).rfind("dashboard", 0) != 0 || !block.contains(nflow::kParams)
            || !block[nflow::kParams].is_object()) {
            return;
        }
        json& binding = block[nflow::kParams]["Binding"];
        if (!binding.is_object() || !binding.contains("blockPath")
            || !binding["blockPath"].is_array() || binding["blockPath"].empty()
            || !binding["blockPath"][0].is_string()) {
            return;
        }
        binding["blockPath"][0] = remapId(binding["blockPath"][0].get<std::string>());
    };

    // Conditional execution (enable): the control wire would be dropped by the merge
    // below (its toIndex has no data-port fanout). A synthetic `condGate` block receives
    // it, inner blocks are tagged with the gate id chain (params.__nfgGates) so the
    // generators wrap their emission, and external outputs go through a synthetic
    // `condHold` that reproduces the runtime's held outputs. A nested subsystem
    // inherits the parent chain.
    // The host block's params mirror the control port, but a spec exported through the
    // block packager keeps it at its own root or under its own params. Read all three
    // so such a gate is lowered, not dropped.
    json condCp = jval(jval(subsystemBlock, nflow::kParams), "controlPort");
    if (!(condCp.is_object() && !condCp.empty())) {
        condCp = jval(spec, "controlPort");
    }
    if (!(condCp.is_object() && !condCp.empty())) {
        condCp = jval(jval(spec, nflow::kParams), "controlPort");
    }
    const std::string condKind
        = condCp.is_object() ? condCp.value("kind", std::string()) : std::string();
    // reset: members always run (no guard, no hold); the gate only fires the
    // edge that re-seeds their states (see the generators' reset emission).
    const bool resetGate = (condKind == "reset");
    const bool enableTrigger = (condKind == "enableTrigger");
    const bool enableGate = (condKind == "enable" || condKind == "action" || condKind == "trigger"
        || enableTrigger || resetGate);
    const std::string gateId = subId + "__nfg";
    json parentGates = json::array();
    {
        const json pg = jval(jval(subsystemBlock, nflow::kParams), "__nfgGates");
        if (pg.is_array()) {
            parentGates = pg;
        }
    }
    json childGates = parentGates;
    if (enableGate && !resetGate) {
        childGates.push_back(gateId);
    }

    json innerBlocks = json::array();
    for (const auto& block : spec[nflow::kBlocks]) {
        json copy = block;
        copy[nflow::kId] = remapId(jstr(block, nflow::kId));
        remapInnerBinding(copy);
        // Goto/From scoping: the runtime resolves labels per level, so an inner Goto/From
        // pair must not collide with an outer one after flattening (two instances of the
        // same library subsystem would share the last instance's signal). Explicit inner
        // tags get the subsystem path as prefix; untagged labels already fall back to the
        // remapped block id. External-port sinks keep their tag: it names a model output
        // column.
        {
            const std::string btype = jstr(copy, nflow::kType);
            if ((btype == nflow::kLabelSink || btype == nflow::kLabelSource)
                && copy.contains(nflow::kParams) && copy[nflow::kParams].is_object()) {
                json& bp = copy[nflow::kParams];
                const bool externalPort = bp.contains(nflow::kIsExternalPort)
                    && bp[nflow::kIsExternalPort].is_boolean()
                    && bp[nflow::kIsExternalPort].get<bool>();
                if (!externalPort && bp.contains(nflow::kGotoTag) && bp[nflow::kGotoTag].is_string()
                    && !bp[nflow::kGotoTag].get<std::string>().empty()) {
                    bp[nflow::kGotoTag] = prefix + bp[nflow::kGotoTag].get<std::string>();
                }
            }
        }
        if (!childGates.empty() || resetGate) {
            if (!copy.contains(nflow::kParams) || !copy[nflow::kParams].is_object()) {
                copy[nflow::kParams] = json::object();
            }
            if (!childGates.empty()) {
                copy[nflow::kParams]["__nfgGates"] = childGates;
            }
            if (resetGate) {
                copy[nflow::kParams]["__nfgReset"] = gateId;
            }
        }
        innerBlocks.push_back(copy);
    }

    // Mask parameters: a masked or library subsystem carries params.maskParameters
    // (name -> value). Inner block params that depend on a mask parameter are resolved
    // to numeric values here, so every inlined instance has its own values (the
    // generator has no subsystem scope, so an inner "Gain": "K" would collapse to 0).
    // Only params whose value changes under the mask are substituted.
    {
        const json maskParams = jval(jval(subsystemBlock, nflow::kParams), "maskParameters");
        if (maskParams.is_object() && !maskParams.empty()) {
            VarMap maskVars;
            for (auto it = maskParams.begin(); it != maskParams.end(); ++it) {
                if (it.value().is_number()) {
                    maskVars[it.key()] = it.value().get<double>();
                }
            }
            const VarMap baseVars;
            for (auto& blk : innerBlocks) {
                if (!blk.contains(nflow::kParams) || !blk[nflow::kParams].is_object()) {
                    continue;
                }
                json& p = blk[nflow::kParams];
                std::vector<std::string> keys;
                for (auto it = p.begin(); it != p.end(); ++it) {
                    keys.push_back(it.key());
                }
                for (const std::string& pk : keys) {
                    if (!p[pk].is_string()) {
                        continue;
                    }
                    const double vm = resolveNumeric(p[pk], maskVars);
                    const double vb = resolveNumeric(p[pk], baseVars);
                    if (vm != vb && std::isfinite(vm)) {
                        p[pk] = vm;
                    }
                }
            }
        }
    }

    json innerConnections = json::array();
    for (const auto& conn : spec[nflow::kConnections]) {
        innerConnections.push_back({ { nflow::kFrom, remapId(jstr(conn, nflow::kFrom)) },
            { nflow::kTo, remapId(jstr(conn, nflow::kTo)) },
            { nflow::kFromIndex, conn.value(nflow::kFromIndex, 0) },
            { nflow::kToIndex, conn.value(nflow::kToIndex, 0) } });
    }

    std::vector<std::string> extInputIds, extOutputIds;
    if (spec.contains(nflow::kExternalInputs) && spec[nflow::kExternalInputs].is_array()) {
        for (const auto& entry : spec[nflow::kExternalInputs]) {
            extInputIds.push_back(remapId(jstr(entry, nflow::kId)));
        }
    }
    if (spec.contains(nflow::kExternalOutputs) && spec[nflow::kExternalOutputs].is_array()) {
        for (const auto& entry : spec[nflow::kExternalOutputs]) {
            extOutputIds.push_back(remapId(jstr(entry, nflow::kId)));
        }
    }
    std::set<std::string> extInputSet(extInputIds.begin(), extInputIds.end());
    std::set<std::string> extOutputSet(extOutputIds.begin(), extOutputIds.end());

    json outerConnections = json::array();
    if (diagram.contains(nflow::kConnections) && diagram[nflow::kConnections].is_array()) {
        for (const auto& c : diagram[nflow::kConnections]) {
            outerConnections.push_back(normalizeConnection(c));
        }
    }

    json preservedOuter = json::array();
    json inboundOuter = json::array();
    json outboundOuter = json::array();
    for (const auto& conn : outerConnections) {
        std::string from = jstr(conn, nflow::kFrom);
        std::string to = jstr(conn, nflow::kTo);
        if (from == subId) {
            outboundOuter.push_back(conn);
        } else if (to == subId) {
            inboundOuter.push_back(conn);
        } else {
            preservedOuter.push_back(conn);
        }
    }

    // extInputFanouts: for each external input port index --> inner connections from that port
    std::unordered_map<int, std::vector<json>> extInputFanouts;
    for (int idx = 0; idx < static_cast<int>(extInputIds.size()); ++idx) {
        const std::string& eid = extInputIds[idx];
        std::vector<json> fanouts;
        for (const auto& conn : innerConnections) {
            if (jstr(conn, nflow::kFrom) == eid) {
                fanouts.push_back(conn);
            }
        }
        extInputFanouts[idx] = std::move(fanouts);
    }
    std::unordered_map<int, std::vector<json>> extOutputFeeds;
    for (int idx = 0; idx < static_cast<int>(extOutputIds.size()); ++idx) {
        const std::string& eid = extOutputIds[idx];
        std::vector<json> feeds;
        for (const auto& conn : innerConnections) {
            if (jstr(conn, nflow::kTo) == eid) {
                feeds.push_back(conn);
            }
        }
        extOutputFeeds[idx] = std::move(feeds);
    }

    json preservedInner = json::array();
    for (const auto& conn : innerConnections) {
        std::string from = jstr(conn, nflow::kFrom);
        std::string to = jstr(conn, nflow::kTo);
        if (extInputSet.count(from) || extInputSet.count(to) || extOutputSet.count(from)
            || extOutputSet.count(to)) {
            continue;
        }
        preservedInner.push_back(conn);
    }

    json mergedConnections = json::array();
    for (const auto& c : preservedOuter) {
        mergedConnections.push_back(c);
    }
    for (const auto& c : preservedInner) {
        mergedConnections.push_back(c);
    }

    json condBlocks = json::array();
    const std::string latchId = subId + "__nfl";
    if (enableGate) {
        json gp = json::object();
        // Only an enable is a LEVEL: it holds across a whole step. An action
        // branch is the arm an if/switchCase selects from the CURRENT input and
        // a trigger is an edge, so the generators must tell them apart.
        gp["kind"] = condKind;
        if (!parentGates.empty()) {
            gp["__nfgGates"] = parentGates;
        }
        if (condKind == "trigger" || resetGate || enableTrigger) {
            // enableTrigger keeps its own mode: the gate reads TWO wires, the
            // enable level on port 0 and the edge on port 1.
            gp["mode"] = enableTrigger ? "enableTrigger" : "trigger";
            const std::string tt = resetGate
                ? condCp.value("resetTriggerType", std::string("rising"))
                : condCp.value("triggerType", std::string("rising"));
            gp["tt"] = (tt == "falling") ? 1 : ((tt == "either") ? 2 : 0);
            if (resetGate) {
                gp["resetOf"] = gateId; // the generators re-seed tagged states
            }
            // End-of-step latch of the control value (edge memory).
            json lp = json::object();
            lp["gate"] = gateId;
            condBlocks.push_back({ { nflow::kId, latchId }, { nflow::kType, "condLatch" },
                { nflow::kInputs, 1 }, { nflow::kOutputs, 0 }, { nflow::kParams, lp } });
        }
        condBlocks.push_back({ { nflow::kId, gateId }, { nflow::kType, "condGate" },
            { nflow::kInputs, enableTrigger ? 2 : 1 }, { nflow::kOutputs, 1 },
            { nflow::kParams, gp } });
    }
    const int nDataIn = (int)extInputIds.size();
    for (const auto& conn : inboundOuter) {
        int toIndex = conn.value(nflow::kToIndex, 0);
        if (enableGate && toIndex == nDataIn) {
            // The control wire feeds the synthetic gate (and the edge latch).
            // For enableTrigger this first wire is the ENABLE, and the edge is
            // measured against the second one below.
            mergedConnections.push_back({ { nflow::kFrom, jstr(conn, nflow::kFrom) },
                { nflow::kTo, gateId }, { nflow::kFromIndex, conn.value(nflow::kFromIndex, 0) },
                { nflow::kToIndex, 0 } });
            if (condKind == "trigger" || resetGate) {
                mergedConnections.push_back(
                    { { nflow::kFrom, jstr(conn, nflow::kFrom) }, { nflow::kTo, latchId },
                        { nflow::kFromIndex, conn.value(nflow::kFromIndex, 0) },
                        { nflow::kToIndex, 0 } });
            }
            continue;
        }
        if (enableTrigger && toIndex == nDataIn + 1) {
            // The trigger wire: port 1 of the gate, and the edge memory the
            // latch keeps for it.
            mergedConnections.push_back({ { nflow::kFrom, jstr(conn, nflow::kFrom) },
                { nflow::kTo, gateId }, { nflow::kFromIndex, conn.value(nflow::kFromIndex, 0) },
                { nflow::kToIndex, 1 } });
            mergedConnections.push_back({ { nflow::kFrom, jstr(conn, nflow::kFrom) },
                { nflow::kTo, latchId }, { nflow::kFromIndex, conn.value(nflow::kFromIndex, 0) },
                { nflow::kToIndex, 0 } });
            continue;
        }
        auto fit = extInputFanouts.find(toIndex);
        if (fit == extInputFanouts.end()) {
            continue;
        }
        for (const auto& fanout : fit->second) {
            mergedConnections.push_back({ { nflow::kFrom, jstr(conn, nflow::kFrom) },
                { nflow::kTo, jstr(fanout, nflow::kTo) },
                { nflow::kFromIndex, conn.value(nflow::kFromIndex, 0) },
                { nflow::kToIndex, fanout.value(nflow::kToIndex, 0) } });
        }
    }
    // A feed whose source IS an external input (a pass-through wire
    // extInput -> extOutput) has no surviving producer after both port
    // blocks are removed: resolve it to the OUTER sources wired into that
    // input port (cross product), so the wire is not silently dropped.
    std::map<std::string, int> extInputIndexOf;
    for (int idx = 0; idx < (int)extInputIds.size(); ++idx) {
        extInputIndexOf[extInputIds[(size_t)idx]] = idx;
    }
    auto resolveFeedSources = [&](const json& feed) -> std::vector<std::pair<std::string, int>> {
        const std::string ffrom = jstr(feed, nflow::kFrom);
        auto eit = extInputIndexOf.find(ffrom);
        if (eit == extInputIndexOf.end()) {
            return { { ffrom, feed.value(nflow::kFromIndex, 0) } };
        }
        std::vector<std::pair<std::string, int>> srcs;
        for (const auto& conn2 : inboundOuter) {
            if (conn2.value(nflow::kToIndex, 0) == eit->second) {
                srcs.push_back({ jstr(conn2, nflow::kFrom), conn2.value(nflow::kFromIndex, 0) });
            }
        }
        return srcs;
    };
    std::set<int> holdMade;
    for (const auto& conn : outboundOuter) {
        int fromIndex = conn.value(nflow::kFromIndex, 0);
        auto fit = extOutputFeeds.find(fromIndex);
        if (fit == extOutputFeeds.end()) {
            continue;
        }
        if (enableGate && !resetGate) {
            // Route every consumer of this external output through one
            // condHold per port: the hold latches while the gate is open and
            // replays the last value while it is closed (runtime parity). A
            // reset subsystem always runs, so its outputs are never held.
            const std::string holdId = subId + "__nfh" + std::to_string(fromIndex);
            if (!holdMade.count(fromIndex)) {
                holdMade.insert(fromIndex);
                json hp = json::object();
                hp["gate"] = gateId;
                condBlocks.push_back({ { nflow::kId, holdId }, { nflow::kType, "condHold" },
                    { nflow::kInputs, 1 }, { nflow::kOutputs, 1 }, { nflow::kParams, hp } });
                for (const auto& feed : fit->second) {
                    for (const auto& src : resolveFeedSources(feed)) {
                        mergedConnections.push_back(
                            { { nflow::kFrom, src.first }, { nflow::kTo, holdId },
                                { nflow::kFromIndex, src.second }, { nflow::kToIndex, 0 } });
                    }
                }
            }
            mergedConnections.push_back({ { nflow::kFrom, holdId },
                { nflow::kTo, jstr(conn, nflow::kTo) }, { nflow::kFromIndex, 0 },
                { nflow::kToIndex, conn.value(nflow::kToIndex, 0) } });
            continue;
        }
        for (const auto& feed : fit->second) {
            for (const auto& src : resolveFeedSources(feed)) {
                mergedConnections.push_back({ { nflow::kFrom, src.first },
                    { nflow::kTo, jstr(conn, nflow::kTo) }, { nflow::kFromIndex, src.second },
                    { nflow::kToIndex, conn.value(nflow::kToIndex, 0) } });
            }
        }
    }

    json dedupedConnections = json::array();
    std::set<std::string> seen;
    for (const auto& conn : mergedConnections) {
        std::string key = connKey(conn);
        if (seen.count(key)) {
            continue;
        }
        seen.insert(key);
        dedupedConnections.push_back(conn);
    }

    // Build new blocks: outer (minus subsystem) + inner (minus external ports)
    json newBlocks = json::array();
    if (diagram.contains(nflow::kBlocks) && diagram[nflow::kBlocks].is_array()) {
        for (const auto& b : diagram[nflow::kBlocks]) {
            if (jstr(b, nflow::kId) != subId) {
                json copy = b;
                if (jstr(copy, nflow::kType).rfind("dashboard", 0) == 0
                    && copy.contains(nflow::kParams) && copy[nflow::kParams].is_object()) {
                    json& binding = copy[nflow::kParams]["Binding"];
                    if (binding.is_object() && binding.contains("blockPath")
                        && binding["blockPath"].is_array() && binding["blockPath"].size() > 1
                        && binding["blockPath"][0].is_string()
                        && binding["blockPath"][0].get<std::string>() == subId
                        && binding["blockPath"][1].is_string()) {
                        binding["blockPath"][1]
                            = remapId(binding["blockPath"][1].get<std::string>());
                        binding["blockPath"].erase(binding["blockPath"].begin());
                    }
                }
                newBlocks.push_back(std::move(copy));
            }
        }
    }
    for (const auto& b : innerBlocks) {
        std::string bid = jstr(b, nflow::kId);
        if (!extInputSet.count(bid) && !extOutputSet.count(bid)) {
            newBlocks.push_back(b);
        }
    }
    for (const auto& b : condBlocks) {
        newBlocks.push_back(b);
    }

    json result = diagram;
    result[nflow::kBlocks] = newBlocks;
    result[nflow::kConnections] = dedupedConnections;
    return result;
}
//=============================================================================
json
DiagramFlattener::expandForEachSubsystem(
    const json& subsystemBlock, const std::unordered_map<std::string, std::vector<int>>& inWidths)
{
    json result = subsystemBlock;
    json params = jval(subsystemBlock, nflow::kParams);
    json spec = jval(params, nflow::kSubsystem);
    json fe = jval(params, "forEach");

    int N = 1;
    if (fe.is_object() && fe.contains("numIterations") && fe["numIterations"].is_number()) {
        N = static_cast<int>(fe["numIterations"].get<double>());
    }
    // Strip the forEach marker; on a degenerate config return the plain
    // subsystem so the ordinary flattener handles it unchanged.
    if (result.contains(nflow::kParams) && result[nflow::kParams].is_object()) {
        result[nflow::kParams].erase("forEach");
    }
    if (N <= 1 || spec.is_null() || !spec.contains(nflow::kBlocks)
        || !spec[nflow::kBlocks].is_array() || !spec.contains(nflow::kConnections)
        || !spec[nflow::kConnections].is_array()) {
        return result;
    }

    std::set<int> partition;
    if (fe.contains("partition") && fe["partition"].is_array()) {
        for (const auto& p : fe["partition"]) {
            if (p.is_number()) {
                partition.insert(static_cast<int>(p.get<double>()));
            }
        }
    }

    std::string subId = jstr(subsystemBlock, nflow::kId);

    // External input / output block ids, indexed by port.
    std::vector<std::string> extInIds, extOutIds;
    if (spec.contains(nflow::kExternalInputs) && spec[nflow::kExternalInputs].is_array()) {
        for (const auto& e : spec[nflow::kExternalInputs]) {
            extInIds.push_back(jstr(e, nflow::kId));
        }
    }
    if (spec.contains(nflow::kExternalOutputs) && spec[nflow::kExternalOutputs].is_array()) {
        for (const auto& e : spec[nflow::kExternalOutputs]) {
            extOutIds.push_back(jstr(e, nflow::kId));
        }
    }
    std::set<std::string> extInSet(extInIds.begin(), extInIds.end());
    std::set<std::string> extOutSet(extOutIds.begin(), extOutIds.end());
    std::unordered_map<std::string, int> extInPort, extOutPort;
    for (int p = 0; p < static_cast<int>(extInIds.size()); ++p) {
        extInPort[extInIds[p]] = p;
    }
    for (int k = 0; k < static_cast<int>(extOutIds.size()); ++k) {
        extOutPort[extOutIds[k]] = k;
    }

    // Resolved outer width of each input port (the selector slice size = width/N).
    std::vector<int> portWidth(extInIds.size(), N);
    auto wit = inWidths.find(subId);
    if (wit != inWidths.end()) {
        for (int p = 0;
            p < static_cast<int>(extInIds.size()) && p < static_cast<int>(wit->second.size());
            ++p) {
            portWidth[static_cast<size_t>(p)] = (wit->second[p] > 0) ? wit->second[p] : N;
        }
    }

    auto iterPrefix = [](int i) { return "fe" + std::to_string(i) + "/"; };

    json newBlocks = json::array();
    json newConns = json::array();

    // Keep the external input / output boundary blocks (singular).
    for (const auto& b : spec[nflow::kBlocks]) {
        std::string id = jstr(b, nflow::kId);
        if (extInSet.count(id) || extOutSet.count(id)) {
            newBlocks.push_back(b);
        }
    }

    // A `selector` per (partitioned port, iteration) slices the outer input into
    // iteration i's width-w channel (1-based indices).
    for (int p = 0; p < static_cast<int>(extInIds.size()); ++p) {
        if (!partition.count(p)) {
            continue;
        }
        const int w = std::max(1, portWidth[static_cast<size_t>(p)] / N);
        for (int i = 0; i < N; ++i) {
            std::string selId = "fe_sel_" + std::to_string(p) + "_" + std::to_string(i);
            json idxList = json::array();
            for (int e = 0; e < w; ++e) {
                idxList.push_back(i * w + e + 1);
            }
            json sel;
            sel[nflow::kId] = selId;
            sel[nflow::kType] = "selector";
            sel[nflow::kInputs] = 1;
            sel[nflow::kOutputs] = 1;
            sel[nflow::kParams] = { { nflow::kIndices, idxList } };
            newBlocks.push_back(sel);
            newConns.push_back({ { nflow::kFrom, extInIds[p] }, { nflow::kFromIndex, 0 },
                { nflow::kTo, selId }, { nflow::kToIndex, 0 } });
        }
    }

    // A `mux` per output port concatenates the iterations' outputs.
    for (int k = 0; k < static_cast<int>(extOutIds.size()); ++k) {
        std::string mxId = "fe_mux_" + std::to_string(k);
        json mx;
        mx[nflow::kId] = mxId;
        mx[nflow::kType] = "mux";
        mx[nflow::kInputs] = N;
        mx[nflow::kOutputs] = 1;
        mx[nflow::kParams] = json::object();
        newBlocks.push_back(mx);
        newConns.push_back({ { nflow::kFrom, mxId }, { nflow::kFromIndex, 0 },
            { nflow::kTo, extOutIds[k] }, { nflow::kToIndex, 0 } });
    }

    // N independent copies of the body blocks (each carries its own state).
    for (int i = 0; i < N; ++i) {
        for (const auto& b : spec[nflow::kBlocks]) {
            std::string id = jstr(b, nflow::kId);
            if (extInSet.count(id) || extOutSet.count(id)) {
                continue;
            }
            json copy = b;
            copy[nflow::kId] = iterPrefix(i) + id;
            newBlocks.push_back(copy);
        }
    }

    // Re-wire the inner connections per iteration: a partitioned external input
    // becomes that iteration's selector, an unpartitioned one is broadcast from
    // the shared boundary block, and a feed to an external output goes to the
    // mux input for that iteration.
    for (int i = 0; i < N; ++i) {
        for (const auto& c : spec[nflow::kConnections]) {
            std::string from = jstr(c, nflow::kFrom);
            std::string to = jstr(c, nflow::kTo);
            int fi = c.value(nflow::kFromIndex, 0);
            int ti = c.value(nflow::kToIndex, 0);
            std::string nf, nt;
            int nfi = fi, nti = ti;
            if (extInSet.count(from)) {
                const int p = extInPort[from];
                if (partition.count(p)) {
                    nf = "fe_sel_" + std::to_string(p) + "_" + std::to_string(i);
                    nfi = 0;
                } else {
                    nf = from; // broadcast: shared boundary input for every iteration
                    nfi = fi;
                }
            } else {
                nf = iterPrefix(i) + from;
                nfi = fi;
            }
            if (extOutSet.count(to)) {
                nt = "fe_mux_" + std::to_string(extOutPort[to]);
                nti = i; // iteration i -> mux input i
            } else {
                nt = iterPrefix(i) + to;
                nti = ti;
            }
            newConns.push_back({ { nflow::kFrom, nf }, { nflow::kFromIndex, nfi },
                { nflow::kTo, nt }, { nflow::kToIndex, nti } });
        }
    }

    json newSpec = spec;
    newSpec[nflow::kBlocks] = newBlocks;
    newSpec[nflow::kConnections] = newConns;
    result[nflow::kParams][nflow::kSubsystem] = newSpec;
    return result;
}
//=============================================================================
json
DiagramFlattener::flatten(const json& diagram)
{
    return flatten(diagram, nullptr);
}
//=============================================================================
json
DiagramFlattener::flatten(const json& diagram, std::string* error)
{
    json current = diagram;
    if (current.contains(nflow::kBlocks)) {
        current[nflow::kBlocks] = json::parse(current[nflow::kBlocks].dump());
    } else {
        current[nflow::kBlocks] = json::array();
    }
    if (current.contains(nflow::kConnections)) {
        current[nflow::kConnections] = json::parse(current[nflow::kConnections].dump());
    } else {
        current[nflow::kConnections] = json::array();
    }

    for (int guard = 0; guard < 1000; ++guard) {
        // For-each subsystems first: rewrite the next one into an ordinary
        // unrolled subsystem (selector-sliced inputs, N body copies, muxed
        // outputs), then let the loop flatten it normally. Widths are resolved on
        // the current diagram so each iteration's selector slice is exact.
        json* nextForEach = nullptr;
        for (auto& block : current[nflow::kBlocks]) {
            if (jstr(block, nflow::kType) != "subsystem") {
                continue;
            }
            json params = jval(block, nflow::kParams);
            if (params.contains("forEach") && params["forEach"].is_object()
                && params["forEach"].contains("numIterations")
                && params["forEach"]["numIterations"].is_number()
                && params["forEach"]["numIterations"].get<double>() > 1.0) {
                nextForEach = &block;
                break;
            }
        }
        if (nextForEach) {
            CodegenWidths w = computeCodegenWidths(current);
            *nextForEach = expandForEachSubsystem(*nextForEach, w.inWidths);
            continue;
        }

        json* nextSubsystem = nullptr;
        for (auto& block : current[nflow::kBlocks]) {
            if (jstr(block, nflow::kType) != "subsystem") {
                continue;
            }
            json params = jval(block, nflow::kParams);
            json sub = jval(params, nflow::kSubsystem);
            if (sub.is_null()) {
                continue;
            }
            if (!sub.contains(nflow::kBlocks) || !sub[nflow::kBlocks].is_array()) {
                continue;
            }
            if (!sub.contains(nflow::kConnections) || !sub[nflow::kConnections].is_array()) {
                continue;
            }
            nextSubsystem = &block;
            break;
        }
        if (!nextSubsystem) {
            return flattenMuxDemux(current);
        }
        current = flattenOneSubsystem(current, *nextSubsystem);
    }
    if (error != nullptr) {
        *error = "subsystem flattening did not converge after 1000 passes (the model nests "
                 "or contains more than 1000 subsystem instances)";
    }
    return flattenMuxDemux(current);
}
//=============================================================================
json
DiagramFlattener::flattenMuxDemux(const json& diagram)
{
    if (!diagram.contains(nflow::kBlocks) || !diagram.contains(nflow::kConnections)
        || !diagram[nflow::kConnections].is_array()) {
        return diagram;
    }

    std::unordered_map<std::string, json> blocks;
    for (const auto& b : diagram[nflow::kBlocks]) {
        blocks[jstr(b, nflow::kId)] = b;
    }

    std::unordered_map<std::string, std::vector<json>> inbound;
    std::unordered_map<std::string, std::vector<json>> outbound;

    for (const auto& conn : diagram[nflow::kConnections]) {
        std::string from = jstr(conn, nflow::kFrom);
        std::string to = jstr(conn, nflow::kTo);
        outbound[from].push_back(conn);
        inbound[to].push_back(conn);
    }

    json newConnections = json::array();
    std::set<std::string> seen;

    for (const auto& conn : diagram[nflow::kConnections]) {
        std::string from = jstr(conn, nflow::kFrom);
        std::string to = jstr(conn, nflow::kTo);
        int fromIndex = conn.value(nflow::kFromIndex, 0);
        int toIndex = conn.value(nflow::kToIndex, 0);

        auto fromBlockIt = blocks.find(from);
        auto toBlockIt = blocks.find(to);

        bool isFromMux
            = (fromBlockIt != blocks.end() && jstr(fromBlockIt->second, nflow::kType) == "mux");
        bool isToMux
            = (toBlockIt != blocks.end() && jstr(toBlockIt->second, nflow::kType) == "mux");
        bool isFromDemux
            = (fromBlockIt != blocks.end() && jstr(fromBlockIt->second, nflow::kType) == "demux");
        bool isToDemux
            = (toBlockIt != blocks.end() && jstr(toBlockIt->second, nflow::kType) == "demux");

        if (isFromDemux) {
            std::string demuxInputSource = "";
            int demuxInputSourceIndex = 0;

            for (const auto& inConn : inbound[from]) {
                if (inConn.value(nflow::kToIndex, 0) == 0) {
                    demuxInputSource = jstr(inConn, nflow::kFrom);
                    demuxInputSourceIndex = inConn.value(nflow::kFromIndex, 0);
                    break;
                }
            }
            (void)demuxInputSourceIndex;

            // A `mux -> demux` pair is a pure pass-through: rewrite each demux
            // output connection to point straight at the matching mux input, and
            // drop the demux edge. When the demux is fed by a real (non-mux)
            // source (e.g. a vector `constant`), leave the connection in place so
            // the vector-expand pass slices the source: demux output port p,
            // element e <- input element sum(prior out widths) + e.
            bool demuxFedByMux = false;
            if (!demuxInputSource.empty()) {
                auto demuxSrcBlockIt = blocks.find(demuxInputSource);
                if (demuxSrcBlockIt != blocks.end()
                    && jstr(demuxSrcBlockIt->second, nflow::kType) == "mux") {
                    demuxFedByMux = true;
                    std::string muxInputSource = "";
                    int muxInputSourceIndex = 0;
                    for (const auto& muxInConn : inbound[demuxInputSource]) {
                        if (muxInConn.value(nflow::kToIndex, 0) == fromIndex) {
                            muxInputSource = jstr(muxInConn, nflow::kFrom);
                            muxInputSourceIndex = muxInConn.value(nflow::kFromIndex, 0);
                            break;
                        }
                    }

                    if (!muxInputSource.empty()) {
                        json directConn = { { nflow::kFrom, muxInputSource },
                            { nflow::kFromIndex, muxInputSourceIndex }, { nflow::kTo, to },
                            { nflow::kToIndex, toIndex } };
                        std::string k = connKey(directConn);
                        if (!seen.count(k)) {
                            seen.insert(k);
                            newConnections.push_back(directConn);
                        }
                        continue;
                    }
                }
            }

            // Only drop the demux edge for the mux pass-through case. A demux fed
            // by a real source keeps its edge for vector-expand to resolve.
            if (demuxFedByMux) {
                continue;
            }
        }

        if (isFromMux && isToDemux) {
            continue;
        }

        std::string k = connKey(conn);
        if (!seen.count(k)) {
            seen.insert(k);
            newConnections.push_back(conn);
        }
    }

    json result = diagram;
    result[nflow::kConnections] = newConnections;
    return result;
}
//=============================================================================
} // namespace nflow
//=============================================================================
