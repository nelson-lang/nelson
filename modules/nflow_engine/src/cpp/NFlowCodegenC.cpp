//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NFlowCodegenC.hpp"
#include "NFlowCodegenEmitShared.hpp"
#include "NFlowCodegenMainHarness.hpp"
#include "NFlowCodegenSolver.hpp"
#include "NFlowCodegenTyped.hpp"
#include "NFlowCodegenHelpers.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "NFlowCodegenPolynomial.hpp"
#include "NFlowDashboardCodegen.hpp"
#include "FieldNames.hpp"
#include "BlockRegistry.hpp"
#include "ExprEngine.hpp"
#include "SimEngine.hpp"
#include "NFlowVersion.hpp"
#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <functional>
#include <limits>
#include <list>
#include <set>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
namespace nflow {
//=============================================================================
namespace {
    std::string
    ns(double v)
    {
        return numToStr(v);
    }
    //=============================================================================
} // anonymous namespace
//=============================================================================
CodegenCResult
generateC(const json& sourceDiagram, const CodegenOptions& opts)
{
    // Codegen may run before any simulation: make sure the block registry is
    // populated so per-block codegen templates (built-in and plugin) resolve.
    Nelson::NFlow::ensureBuiltinsRegistered();
    DashboardCodegenPlan dashboardPlan = buildDashboardCodegenPlan(sourceDiagram, "c");
    const json& diagram = dashboardPlan.diagram;

    double sampleTime = opts.sampleTime;
    bool includeMain = opts.includeMain;

    json blocksJson = diagram.contains(nflow::kBlocks) ? diagram[nflow::kBlocks] : json::array();
    json connsJson
        = diagram.contains(nflow::kConnections) ? diagram[nflow::kConnections] : json::array();
    json varsJson
        = diagram.contains(nflow::kVariables) ? diagram[nflow::kVariables] : json::object();
    VarMap variables = buildVarMap(varsJson);

    double mainDuration = 10.0;
    if (diagram.contains(nflow::kRuntime)) {
        const auto& rt = diagram[nflow::kRuntime];
        double r = 0;
        if (rt.is_number()) {
            r = rt.get<double>();
        } else if (rt.is_string()) {
            char* end = nullptr;
            r = std::strtod(rt.get<std::string>().c_str(), &end);
        }
        if (std::isfinite(r) && r > 0) {
            mainDuration = r;
        }
    }

    std::vector<json> blocks;
    for (const auto& b : blocksJson) {
        blocks.push_back(b);
    }

    std::vector<json> connections;
    for (const auto& c : connsJson) {
        connections.push_back(c);
    }

    // stopSimulation blocks raise a shared state flag; the generated main()
    // breaks its loop on it (emitted only when the block exists).
    bool hasStopBlock = false;
    for (const auto& b : blocks) {
        if (jstr(b, nflow::kType) == "stopSimulation") {
            hasStopBlock = true;
            break;
        }
    }

    std::unordered_map<std::string, json> byId;
    CodegenInputsMap inputs;
    // outPortCount[id] = number of generated output variables for that block
    // (out_<id>, out_<id>_p1, ...): the declared "outputs" count or, when
    // absent, 1 + the highest fromIndex seen on its outgoing wires.
    std::unordered_map<std::string, int> outPortCount;
    for (const auto& b : blocks) {
        std::string id = jstr(b, nflow::kId);
        byId[id] = b;
        inputs[id] = {};
        outPortCount[id] = std::max(1, jint(b, nflow::kOutputs, 1));
    }
    for (const auto& conn : connections) {
        std::string to = jstr(conn, nflow::kTo);
        int toIndex = jint(conn, nflow::kToIndex, 0);
        std::string from = jstr(conn, nflow::kFrom);
        int fromIndex = jint(conn, nflow::kFromIndex, 0);
        auto& list = inputs[to];
        if (static_cast<int>(list.size()) <= toIndex) {
            list.resize(toIndex + 1);
        }
        list[toIndex] = { from, fromIndex };
        auto oit = outPortCount.find(from);
        if (oit != outPortCount.end()) {
            oit->second = std::max(oit->second, fromIndex + 1);
        }
    }

    // Resolved by the layout pass; exact 64-bit ports get typed C variables
    // (int64_t/uint64_t) and integer-space arithmetic. Everything else stays double.
    nflow::CodegenWidths codegenTypes = nflow::computeCodegenWidths(diagram);
    std::vector<CodegenDiagnostic> diagnostics;
    auto portTypeName = [&codegenTypes](const std::string& rawId, int p) -> std::string {
        if (!codegenTypes.ok) {
            return "double";
        }
        auto it = codegenTypes.outTypes.find(rawId);
        if (it == codegenTypes.outTypes.end() || p < 0 || p >= (int)it->second.size()) {
            return "double";
        }
        return it->second[p];
    };
    auto isExact64Name = [](const std::string& tn) { return tn == "int64" || tn == "uint64"; };
    auto cTypeOf = [&isExact64Name](const std::string& tn) -> std::string {
        if (tn == "int64") {
            return "int64_t";
        }
        if (tn == "uint64") {
            return "uint64_t";
        }
        (void)isExact64Name;
        return "double";
    };
    bool anyExact64 = false;
    if (codegenTypes.ok) {
        for (const auto& kv : codegenTypes.outTypes) {
            for (const auto& tn : kv.second) {
                if (isExact64Name(tn)) {
                    anyExact64 = true;
                    break;
                }
            }
            if (anyExact64) {
                break;
            }
        }
    }
    // single and the small integer types get a generator-side quantization
    // after each producing block (mirroring the simulator's double-lane
    // computation + quantizeToType); the integer form needs a helper in the
    // emitted preamble.
    bool anySmallTyped = false;
    bool anySmallInt = false;
    if (codegenTypes.ok) {
        for (const auto& kv : codegenTypes.outTypes) {
            for (const auto& tn : kv.second) {
                double qlo = 0.0;
                double qhi = 0.0;
                if (tn == "single") {
                    anySmallTyped = true;
                } else if (Nelson::NFlow::codegenSmallIntRange(tn, qlo, qhi)) {
                    anySmallTyped = true;
                    anySmallInt = true;
                }
            }
        }
    }

    std::vector<json> labelSources, labelSinks;
    for (const auto& b : blocks) {
        std::string type = jstr(b, nflow::kType);
        if (type == nflow::kLabelSource) {
            labelSources.push_back(b);
        }
        if (type == nflow::kLabelSink) {
            labelSinks.push_back(b);
        }
    }

    std::unordered_map<std::string, std::string> labelSinkByName;
    for (const auto& b : labelSinks) {
        std::string name = getLabelName(b);
        labelSinkByName[name] = jstr(b, nflow::kId);
    }

    std::vector<json> externalLabelSources;
    for (const auto& b : labelSources) {
        if (labelSinkByName.find(getLabelName(b)) == labelSinkByName.end()) {
            externalLabelSources.push_back(b);
        }
    }

    auto inputLabels = buildUniqueLabelList(externalLabelSources);
    auto outputLabels = buildUniqueLabelList(labelSinks);
    const auto& inputNames = inputLabels.list;
    const auto& outputNames = outputLabels.list;

    // The vector expansion records label sinks fed by a named nonvirtual bus;
    // their per-element fields re-group into one struct-typed ModelOutput
    // field. A group whose element sinks did not all survive as output labels
    // falls back to the flattened per-element fields.
    nflow::BusStructPlan busPlan = nflow::buildBusStructPlan(diagram);
    // element tag -> (index into busPlan.outputs, packed element index)
    std::unordered_map<std::string, std::pair<int, int>> busElemOf;
    {
        std::vector<nflow::BusStructOutput> kept;
        for (auto& o : busPlan.outputs) {
            bool ok = true;
            for (const auto& e : o.elements) {
                if (outputLabels.map.find(e) == outputLabels.map.end()) {
                    ok = false;
                    break;
                }
            }
            if (ok) {
                kept.push_back(std::move(o));
            }
        }
        busPlan.outputs = std::move(kept);
        for (int i = 0; i < (int)busPlan.outputs.size(); ++i) {
            const auto& o = busPlan.outputs[(size_t)i];
            for (int k = 0; k < (int)o.elements.size(); ++k) {
                busElemOf[o.elements[(size_t)k]] = { i, k };
            }
        }
    }
    const bool hasBusOutputs = !busPlan.outputs.empty();

    // Bus-typed external label sources re-group into one struct-typed
    // ModelInput field; a group whose element sources did not all survive as
    // input labels falls back to the flattened per-element fields.
    std::unordered_map<std::string, std::pair<int, int>> busInElemOf;
    {
        std::vector<nflow::BusStructOutput> kept;
        for (auto& o : busPlan.inputs) {
            bool ok = true;
            for (const auto& e : o.elements) {
                if (inputLabels.map.find(e) == inputLabels.map.end()) {
                    ok = false;
                    break;
                }
            }
            if (ok) {
                kept.push_back(std::move(o));
            }
        }
        busPlan.inputs = std::move(kept);
        for (int i = 0; i < (int)busPlan.inputs.size(); ++i) {
            const auto& o = busPlan.inputs[(size_t)i];
            for (int k = 0; k < (int)o.elements.size(); ++k) {
                busInElemOf[o.elements[(size_t)k]] = { i, k };
            }
        }
    }
    const bool hasBusInputs = !busPlan.inputs.empty();

    // Field types of the external inputs follow the label source's port
    // (an exact-64 declared type keeps its integer type).
    std::unordered_map<std::string, std::string> inputSourceByName;
    for (const auto& b : externalLabelSources) {
        const std::string nm = getLabelName(b);
        if (inputSourceByName.find(nm) == inputSourceByName.end()) {
            inputSourceByName[nm] = jstr(b, nflow::kId);
        }
    }

    struct LabelMapping
    {
        std::string id, name, sinkId, sinkInput;
    };
    std::vector<LabelMapping> labelSourceMappings;
    for (const auto& block : labelSources) {
        std::string blockId = jstr(block, nflow::kId);
        std::string name = getLabelName(block);
        std::string sinkId, sinkInput;
        auto sit = labelSinkByName.find(name);
        if (sit != labelSinkByName.end()) {
            sinkId = sit->second;
            const auto& sInputs = inputs[sinkId];
            if (!sInputs.empty() && !sInputs[0].id.empty()) {
                sinkInput = codegenOutVar(sInputs[0].id, sInputs[0].port);
            }
        }
        labelSourceMappings.push_back({ blockId, name, sinkId, sinkInput });
    }

    // Extra edges for label-source --> label-sink wiring
    std::vector<json> extraEdges;
    for (const auto& block : labelSources) {
        std::string blockId = jstr(block, nflow::kId);
        std::string name = getLabelName(block);
        auto sit = labelSinkByName.find(name);
        if (sit == labelSinkByName.end()) {
            continue;
        }
        const auto& sInputs = inputs[sit->second];
        if (!sInputs.empty() && !sInputs[0].id.empty()) {
            extraEdges.push_back({ { nflow::kFrom, sInputs[0].id }, { nflow::kTo, blockId } });
        }
    }
    appendCondExecutionEdges(blocks, extraEdges);

    // Non-feedthrough stateful blocks whose generated output can be pre-loaded
    // from their state ("latched"): a feedback cycle legitimately breaks at
    // them. A block qualifies when its simulation metadata declares no direct
    // feedthrough, it has at least one wired input (a source cannot close a
    // cycle), and its codegen template can emit the output without running the
    // state update (emitOutput, or the RK4 seam's Rk4Output op).
    std::set<std::string> latchedIds;
    for (const auto& b : blocks) {
        const std::string bidRaw = jstr(b, nflow::kId);
        bool hasWiredInput = false;
        auto iit = inputs.find(bidRaw);
        if (iit != inputs.end()) {
            for (const auto& ref : iit->second) {
                if (!ref.id.empty()) {
                    hasWiredInput = true;
                    break;
                }
            }
        }
        if (!hasWiredInput) {
            continue;
        }
        const std::string btype = jstr(b, nflow::kType);
        Nelson::NFlow::BlockCodegenTemplate ctpl;
        if (!Nelson::NFlow::BlockRegistry::instance().getCodegen(btype, "c", ctpl)
            || ctpl.notImplemented || (!ctpl.emitOutput && !ctpl.emitRk4)) {
            continue;
        }
        // Parameter-aware feedthrough: a cycle may only be broken (latched) at a
        // block with NO real feedthrough. The static directFeedthrough flag is a
        // coarse approximation for tf (marked feedthrough to cover the pure-gain
        // case, but a strictly proper plant reads purely from state), pid (marked
        // non-feedthrough for scheduling parity, but P/D terms feed through) and
        // stateSpace (feeds through only when D is nonzero). Latching a
        // feedthrough block would emit its output from a stale, zero input.
        if (nflow::blockHasCodegenFeedthrough(b, variables)) {
            continue;
        }
        latchedIds.insert(bidRaw);
    }

    std::vector<std::string> unresolvedCycleIds;
    auto order
        = buildExecutionOrder(blocks, connections, extraEdges, latchedIds, &unresolvedCycleIds);
    if (!unresolvedCycleIds.empty()) {
        diagnostics.push_back(makeAlgebraicLoopDiagnostic(unresolvedCycleIds));
    }

    // Latched blocks read by a consumer emitted BEFORE them (a broken feedback
    // cycle): their out_<id> local must be pre-loaded from state, otherwise the
    // consumer reads the 0.0 declaration default. Empty for acyclic diagrams.
    std::vector<std::string> preloadIds;
    {
        std::unordered_map<std::string, size_t> orderPos;
        for (size_t i = 0; i < order.size(); ++i) {
            orderPos[order[i]] = i;
        }
        std::set<std::string> need;
        auto consumerBefore = [&](const std::string& producer, const std::string& consumer) {
            auto pit = orderPos.find(producer);
            auto cit = orderPos.find(consumer);
            return pit != orderPos.end() && cit != orderPos.end() && cit->second < pit->second;
        };
        for (const auto& kv : inputs) {
            for (const auto& ref : kv.second) {
                if (!ref.id.empty() && latchedIds.count(ref.id) > 0
                    && consumerBefore(ref.id, kv.first)) {
                    need.insert(ref.id);
                }
            }
        }
        // Label wiring: a labelSource re-emits the variable feeding its sink.
        for (const auto& e : extraEdges) {
            const std::string from = jstr(e, nflow::kFrom);
            if (latchedIds.count(from) > 0 && consumerBefore(from, jstr(e, nflow::kTo))) {
                need.insert(from);
            }
        }
        for (const auto& id : order) {
            if (need.count(id) > 0) {
                preloadIds.push_back(id);
            }
        }
    }

    double dtVal = (sampleTime > 0) ? sampleTime : 0.01;

    std::vector<std::string> stateDecls;
    std::vector<std::string> initLines;
    std::vector<std::string> constLines;
    std::vector<std::string> helperLines;

    auto addState = [&](const std::string& name, const std::string& init = "0.0",
                        const std::string& stype = "double") {
        stateDecls.push_back(stype + " " + name + ";");
        initLines.push_back("  s->" + name + " = " + init + ";");
    };
    for (const auto& tunable : dashboardPlan.tunables) {
        addState(tunable.field, ns(tunable.initialValue));
    }

    struct PidInfo
    {
        json block;
        std::string id;
    };
    struct TfInfo
    {
        json block;
        std::string id;
        std::vector<double> b, a;
        int n = 0;
    };
    struct FilterInfo
    {
        json block;
        std::string id;
    };
    struct DelayInfo
    {
        std::string id;
        int steps = 0;
        double delaySamples = 0;
    };

    std::vector<PidInfo> pidBlocks;
    std::vector<TfInfo> tfBlocks;
    std::vector<FilterInfo> lpfBlocks;
    std::vector<FilterInfo> hpfBlocks;
    std::vector<DelayInfo> delayBlocks;

    // Block types that fall back to the default emission (reported to callers).
    std::set<std::string> unknownTypes;
    unknownTypes.insert(dashboardPlan.errors.begin(), dashboardPlan.errors.end());

    // Shared once-keys for per-block state emitters (e.g. a state field shared
    // by every instance of a type is only declared the first time).
    std::set<std::string> codegenOnceKeys;

    // Reset-subsystem members (params.__nfgReset = gate id): their state
    // fields and initializers, collected during the state pass so the gate's
    // edge can re-seed them (emitted right after the condGate block).
    // resetRaw carries the RAW addInit lines of members whose state is
    // declared via declState (tf, ddelay, ...): the runtime re-runs the whole
    // INIT phase on a reset edge, so those lines are replayed verbatim too.
    std::map<std::string, std::vector<std::pair<std::string, std::string>>> resetFields;
    std::map<std::string, std::vector<std::string>> resetRaw;
    std::string currentResetGate;

    auto makeStateArgs = [&](const std::string& sid, const json* blk, const json* prm) {
        Nelson::NFlow::BlockCodegenStateArgs sa;
        sa.id = sid;
        sa.block = blk;
        sa.params = prm;
        sa.variables = &variables;
        sa.dt = dtVal;
        sa.fmt = [](double v) { return ns(v); };
        sa.addState = [&](const std::string& n, const std::string& init, const std::string& stype) {
            addState(n, init.empty() ? "0.0" : init, stype.empty() ? "double" : stype);
            if (!currentResetGate.empty()) {
                resetFields[currentResetGate].push_back({ n, init.empty() ? "0.0" : init });
            }
        };
        sa.declState = [&](const std::string& l) { stateDecls.push_back(l); };
        sa.addInit = [&](const std::string& l) {
            initLines.push_back(l);
            if (!currentResetGate.empty()) {
                resetRaw[currentResetGate].push_back(l);
            }
        };
        sa.addConst = [&](const std::string& l) { constLines.push_back(l); };
        sa.addHelper = [&](const std::string& l) { helperLines.push_back(l); };
        sa.once = [&](const std::string& key) { return codegenOnceKeys.insert(key).second; };
        sa.allBlocks = &blocks;
        return sa;
    };

    for (const auto& block : blocks) {
        nflow::BlockDescriptor bd(block, variables);
        std::string id = sanitizeId(bd.id());
        std::string type = bd.type();
        json params = jval(block, nflow::kParams);
        if (params.is_null()) {
            params = json::object();
        }

        {
            currentResetGate = params.contains("__nfgReset") && params["__nfgReset"].is_string()
                ? params["__nfgReset"].get<std::string>()
                : std::string();
            Nelson::NFlow::BlockCodegenTemplate ctpl;
            if (Nelson::NFlow::BlockRegistry::instance().getCodegen(type, "c", ctpl)
                && !ctpl.notImplemented && ctpl.emitState) {
                auto sa = makeStateArgs(id, &block, &params);
                ctpl.emitState(sa);
            } else if (Nelson::NFlow::BlockRegistry::instance().getCodegen(type, "c", ctpl)
                && !ctpl.notImplemented && !ctpl.state.empty()) {
                auto resolveInit = [&](const std::string& tok) -> std::string {
                    if (tok == "id") {
                        return id;
                    }
                    if (tok.rfind("param:", 0) == 0) {
                        std::string rest = tok.substr(6);
                        std::string pname = rest;
                        double dflt = 0.0;
                        size_t colon = rest.find(':');
                        if (colon != std::string::npos) {
                            pname = rest.substr(0, colon);
                            try {
                                dflt = std::stod(rest.substr(colon + 1));
                            } catch (...) {
                            }
                        }
                        if (const auto* tunable
                            = dashboardPlan.parameterFor(jstr(block, nflow::kId), pname)) {
                            return "s->" + tunable->field;
                        }
                        if (const auto* tunable = dashboardPlan.variableFor(params, pname)) {
                            return "s->" + tunable->field;
                        }
                        return ns(bd.paramDouble(pname, dflt));
                    }
                    return "{" + tok + "}";
                };
                for (const auto& sv : ctpl.state) {
                    const std::string init
                        = sv.init.empty() ? "0.0" : expandBlockTemplate(sv.init, resolveInit);
                    addState(sv.name + "_" + id, init);
                    if (!currentResetGate.empty()) {
                        resetFields[currentResetGate].push_back({ sv.name + "_" + id, init });
                    }
                }
            }
        }
    }

    // MAX_DIM is defined by tf's shared section; without a tf block, keep a
    // baseline define so generated code is stable.
    {
        bool anyTf = false;
        for (const auto& block : blocks) {
            if (jstr(block, nflow::kType) == "tf") {
                anyTf = true;
                break;
            }
        }
        if (!anyTf) {
            constLines.push_back("#define MAX_DIM 1");
        }
    }

    // Blocks are visited in ascending sharedPriority, then diagram order, so the
    // shared sections stay grouped by type.
    {
        struct SharedItem
        {
            int prio;
            const json* block;
            Nelson::NFlow::BlockCodegenTemplate tpl;
        };
        std::vector<SharedItem> shared;
        for (const auto& block : blocks) {
            std::string btype = jstr(block, nflow::kType);
            Nelson::NFlow::BlockCodegenTemplate st;
            if (Nelson::NFlow::BlockRegistry::instance().getCodegen(btype, "c", st)
                && !st.notImplemented && st.emitShared) {
                shared.push_back({ st.sharedPriority, &block, st });
            }
        }
        std::stable_sort(shared.begin(), shared.end(),
            [](const SharedItem& a, const SharedItem& b) { return a.prio < b.prio; });
        currentResetGate.clear(); // shared-section states never join a reset
        for (auto& item : shared) {
            nflow::BlockDescriptor sbd(*item.block, variables);
            json sparams = jval(*item.block, nflow::kParams);
            if (sparams.is_null()) {
                sparams = json::object();
            }
            auto sa = makeStateArgs(sanitizeId(sbd.id()), item.block, &sparams);
            item.tpl.emitShared(sa);
        }
    }

    //=============================================================================
    std::vector<std::string> lines;
    lines.push_back("/* Generated by NFlow " NFLOW_PRODUCT_VERSION " */");
    // MSVC only defines M_PI (used by the sine block, etc.) when
    // _USE_MATH_DEFINES is set before <math.h> is included.
    lines.push_back("#define _USE_MATH_DEFINES");
    lines.push_back("#include <math.h>");
    lines.push_back("#include <stdint.h>");
    if (includeMain) {
        lines.push_back("#include <stdio.h>");
        lines.push_back("#include <stdlib.h>");
        lines.push_back("#include <string.h>");
    }
    lines.push_back("");

    if (anyExact64) {
        // Exact 64-bit integer helpers: saturation needs explicit overflow
        // checks (signed overflow is UB in C); wrap runs through unsigned
        // arithmetic (well defined). Division rounds per the block's
        // Rounding parameter, floor by default.
        lines.push_back("/* Exact 64-bit integer helpers */");
        lines.push_back("static inline int64_t nflow_i64_sat_add(int64_t a, int64_t b) {");
        lines.push_back("  int64_t r = (int64_t)((uint64_t)a + (uint64_t)b);");
        lines.push_back("  if (a >= 0 && b >= 0 && r < 0) return INT64_MAX;");
        lines.push_back("  if (a < 0 && b < 0 && r >= 0) return INT64_MIN;");
        lines.push_back("  return r;");
        lines.push_back("}");
        lines.push_back("static inline int64_t nflow_i64_sat_sub(int64_t a, int64_t b) {");
        lines.push_back("  int64_t r = (int64_t)((uint64_t)a - (uint64_t)b);");
        lines.push_back("  if (a >= 0 && b < 0 && r < 0) return INT64_MAX;");
        lines.push_back("  if (a < 0 && b >= 0 && r >= 0) return INT64_MIN;");
        lines.push_back("  return r;");
        lines.push_back("}");
        lines.push_back("static inline int64_t nflow_i64_sat_mul(int64_t a, int64_t b) {");
        lines.push_back("  int64_t r = (int64_t)((uint64_t)a * (uint64_t)b);");
        lines.push_back("  if (a != 0 && (r / a != b || (a == -1 && b == INT64_MIN)");
        lines.push_back("        || (b == -1 && a == INT64_MIN)))");
        lines.push_back("    return ((a < 0) != (b < 0)) ? INT64_MIN : INT64_MAX;");
        lines.push_back("  return r;");
        lines.push_back("}");
        lines.push_back("static inline int64_t nflow_i64_div_floor(int64_t a, int64_t b) {");
        lines.push_back("  if (b == 0) return (a == 0) ? 0 : ((a > 0) ? INT64_MAX : INT64_MIN);");
        lines.push_back("  if (a == INT64_MIN && b == -1) return INT64_MAX;");
        lines.push_back("  int64_t q = a / b; int64_t r = a % b;");
        lines.push_back("  if (r != 0 && ((a < 0) != (b < 0))) q -= 1;");
        lines.push_back("  return q;");
        lines.push_back("}");
        lines.push_back("static inline int64_t nflow_i64_div_trunc(int64_t a, int64_t b) {");
        lines.push_back("  if (b == 0) return (a == 0) ? 0 : ((a > 0) ? INT64_MAX : INT64_MIN);");
        lines.push_back("  if (a == INT64_MIN && b == -1) return INT64_MAX;");
        lines.push_back("  return a / b;");
        lines.push_back("}");
        lines.push_back("static inline int64_t nflow_i64_div_ceil(int64_t a, int64_t b) {");
        lines.push_back("  if (b == 0) return (a == 0) ? 0 : ((a > 0) ? INT64_MAX : INT64_MIN);");
        lines.push_back("  if (a == INT64_MIN && b == -1) return INT64_MAX;");
        lines.push_back("  int64_t q = a / b; int64_t r = a % b;");
        lines.push_back("  if (r != 0 && ((a < 0) == (b < 0))) q += 1;");
        lines.push_back("  return q;");
        lines.push_back("}");
        lines.push_back("static inline int64_t nflow_i64_div_round(int64_t a, int64_t b) {");
        lines.push_back("  if (b == 0) return (a == 0) ? 0 : ((a > 0) ? INT64_MAX : INT64_MIN);");
        lines.push_back("  if (a == INT64_MIN && b == -1) return INT64_MAX;");
        lines.push_back("  int64_t q = a / b; int64_t r = a % b;");
        lines.push_back("  if (r != 0) {");
        lines.push_back("    uint64_t ar = (r < 0) ? (uint64_t)(-(r + 1)) + 1u : (uint64_t)r;");
        lines.push_back("    uint64_t ab = (b < 0) ? (uint64_t)(-(b + 1)) + 1u : (uint64_t)b;");
        lines.push_back("    if (2u * ar >= ab) q += ((a < 0) != (b < 0)) ? -1 : 1;");
        lines.push_back("  }");
        lines.push_back("  return q;");
        lines.push_back("}");
        lines.push_back("static inline uint64_t nflow_u64_sat_add(uint64_t a, uint64_t b) {");
        lines.push_back("  uint64_t r = a + b; return (r < a) ? UINT64_MAX : r;");
        lines.push_back("}");
        lines.push_back("static inline uint64_t nflow_u64_sat_sub(uint64_t a, uint64_t b) {");
        lines.push_back("  return (b > a) ? 0 : (a - b);");
        lines.push_back("}");
        lines.push_back("static inline uint64_t nflow_u64_sat_mul(uint64_t a, uint64_t b) {");
        lines.push_back("  uint64_t r = a * b; return (a != 0 && r / a != b) ? UINT64_MAX : r;");
        lines.push_back("}");
        lines.push_back("static inline uint64_t nflow_u64_div_floor(uint64_t a, uint64_t b) {");
        lines.push_back("  return (b == 0) ? ((a == 0) ? 0 : UINT64_MAX) : (a / b);");
        lines.push_back("}");
        lines.push_back("static inline uint64_t nflow_u64_div_ceil(uint64_t a, uint64_t b) {");
        lines.push_back("  if (b == 0) return (a == 0) ? 0 : UINT64_MAX;");
        lines.push_back("  return a / b + ((a % b) ? 1u : 0u);");
        lines.push_back("}");
        lines.push_back("static inline uint64_t nflow_u64_div_round(uint64_t a, uint64_t b) {");
        lines.push_back("  if (b == 0) return (a == 0) ? 0 : UINT64_MAX;");
        lines.push_back("  return a / b + ((2u * (a % b) >= b) ? 1u : 0u);");
        lines.push_back("}");
        lines.push_back("static inline int64_t nflow_d_to_i64_sat(double v) {");
        lines.push_back("  if (!(v == v)) return 0;");
        lines.push_back("  if (v >= 9223372036854775808.0) return INT64_MAX;");
        lines.push_back("  if (v < -9223372036854775808.0) return INT64_MIN;");
        lines.push_back("  return (int64_t)v;");
        lines.push_back("}");
        lines.push_back("static inline uint64_t nflow_d_to_u64_sat(double v) {");
        lines.push_back("  if (!(v == v) || v <= 0.0) return 0;");
        lines.push_back("  if (v >= 18446744073709551616.0) return UINT64_MAX;");
        lines.push_back("  return (uint64_t)v;");
        lines.push_back("}");
        lines.push_back("");
    }
    if (anySmallInt) {
        // Small-integer quantization: NaN -> 0, round to nearest, then clamp
        // (saturate) or wrap modulo the type span, exactly like the
        // simulator's quantizeToType.
        lines.push_back("/* Small integer quantization (round, then saturate or wrap) */");
        lines.push_back("static double nflow_quant_i(double v, double lo, double hi, int sat) {");
        lines.push_back("  double r, span, m;");
        lines.push_back("  if (v != v) return 0.0;");
        lines.push_back("  r = round(v);");
        lines.push_back("  if (sat) return r < lo ? lo : (r > hi ? hi : r);");
        lines.push_back("  span = hi - lo + 1.0;");
        lines.push_back("  m = fmod(r - lo, span);");
        lines.push_back("  if (m < 0.0) m += span;");
        lines.push_back("  return lo + m;");
        lines.push_back("}");
        lines.push_back("");
    }

    for (const auto& l : constLines) {
        lines.push_back(l);
    }
    if (!constLines.empty()) {
        lines.push_back("");
    }

    bool hasVars = false;
    if (varsJson.is_object()) {
        for (auto it = varsJson.begin(); it != varsJson.end(); ++it) {
            const std::string& name = it.key();
            std::string cname = name;
            if (!cname.empty() && cname[0] == '\\') {
                cname = cname.substr(1);
            }
            cname = sanitizeId(cname);
            double val = 0;
            auto vit = variables.find(name);
            if (vit != variables.end()) {
                val = vit->second;
            }
            lines.push_back("static const double " + cname + " = " + ns(val) + ";");
            hasVars = true;
        }
    }
    if (hasVars) {
        lines.push_back("");
    }

    for (const auto& l : helperLines) {
        lines.push_back(l);
    }
    if (!helperLines.empty()) {
        lines.push_back("");
    }

    lines.push_back("typedef struct {");
    for (const auto& l : stateDecls) {
        lines.push_back("  " + l);
    }
    if (stateDecls.empty()) {
        lines.push_back("  double _unused;");
    }
    lines.push_back("} ModelState;");
    lines.push_back("");

    // Emitted before ModelInput/ModelOutput, both of which may embed them.
    if (hasBusOutputs || hasBusInputs) {
        lines.push_back("/* Nonvirtual bus types */");
        for (const auto& bt : busPlan.types) {
            lines.push_back("typedef struct {");
            for (const auto& m : bt.members) {
                if (m.isBus) {
                    lines.push_back("  " + m.busName + "_t " + m.name + ";");
                } else if (m.width > 1) {
                    std::string dimsNote;
                    if (m.dims.size() > 1) {
                        dimsNote = " /* dims ";
                        for (size_t k = 0; k < m.dims.size(); ++k) {
                            if (k > 0) {
                                dimsNote += "x";
                            }
                            dimsNote += std::to_string(m.dims[k]);
                        }
                        dimsNote += ", column-major */";
                    }
                    lines.push_back("  " + cTypeOf(m.typeName) + " " + m.name + "["
                        + std::to_string(m.width) + "];" + dimsNote);
                } else {
                    lines.push_back("  " + cTypeOf(m.typeName) + " " + m.name + ";");
                }
            }
            lines.push_back("} " + bt.name + "_t;");
            lines.push_back("");
        }
    }

    // Field types follow the label source's declared port type (exact-64
    // inputs keep their integer type); a nonvirtual bus group collapses into
    // one struct-typed field placed at its first element's position.
    auto inputFieldType = [&](const std::string& name) -> std::string {
        auto iit = inputSourceByName.find(name);
        if (iit == inputSourceByName.end()) {
            return "double";
        }
        return cTypeOf(portTypeName(iit->second, 0));
    };
    lines.push_back("typedef struct {");
    for (const auto& name : inputNames) {
        auto beIt = busInElemOf.find(name);
        if (beIt != busInElemOf.end()) {
            if (beIt->second.second == 0) {
                const auto& o = busPlan.inputs[(size_t)beIt->second.first];
                lines.push_back("  " + o.busName + "_t " + o.name + ";");
            }
            continue;
        }
        lines.push_back("  " + inputFieldType(name) + " " + name + ";");
    }
    if (inputNames.empty()) {
        lines.push_back("  double _unused;");
    }
    lines.push_back("} ModelInput;");
    lines.push_back("");

    // One entry per scalar column of the input interface: plain inputs map
    // 1:1, a struct input contributes one column per packed element
    // ("u.pos[0]", "u.sub.a"). Drives input_names[] and the CSV filling.
    struct InColumn
    {
        std::string label; // header name (also the access path)
        std::string ctype;
    };
    std::vector<InColumn> inColumns;
    for (const auto& name : inputNames) {
        auto beIt = busInElemOf.find(name);
        if (beIt != busInElemOf.end()) {
            if (beIt->second.second == 0) {
                const auto& o = busPlan.inputs[(size_t)beIt->second.first];
                for (size_t k = 0; k < o.accesses.size(); ++k) {
                    inColumns.push_back({ o.name + o.accesses[k], cTypeOf(o.leafTypes[k]) });
                }
            }
            continue;
        }
        inColumns.push_back({ name, inputFieldType(name) });
    }
    bool anyTypedInput = false;
    for (const auto& col : inColumns) {
        if (col.ctype != "double") {
            anyTypedInput = true;
            break;
        }
    }

    // Field types follow the signal feeding each label sink (exact-64 ports keep
    // their integer type). Nonvirtual bus groups collapse into one struct-typed
    // field placed at their first element's position.
    auto outputFieldType = [&](const std::string& name) -> std::string {
        auto sit = labelSinkByName.find(name);
        if (sit == labelSinkByName.end()) {
            return "double";
        }
        return cTypeOf(portTypeName(sit->second, 0));
    };
    bool anyTypedOutput = false;
    for (const auto& name : outputNames) {
        if (outputFieldType(name) != "double") {
            anyTypedOutput = true;
            break;
        }
    }

    // External interface for the FMU exporter: the exact ModelInput / ModelOutput
    // field names, in order, tagged with their FMI type. A bus/struct field has no
    // scalar FMI type (empty tag); the exporter rejects those.
    auto fmiTypeOfCType = [](const std::string& ct) -> std::string {
        if (ct == "double") {
            return "Float64";
        }
        if (ct == "int64_t") {
            return "Int64";
        }
        if (ct == "uint64_t") {
            return "UInt64";
        }
        return "";
    };
    std::vector<std::pair<std::string, std::string>> ifaceInputs;
    std::vector<std::pair<std::string, std::string>> ifaceOutputs;
    for (const auto& name : inputNames) {
        if (busInElemOf.find(name) != busInElemOf.end()) {
            ifaceInputs.emplace_back(name, ""); // bus element: no scalar FMI type
        } else {
            ifaceInputs.emplace_back(name, fmiTypeOfCType(inputFieldType(name)));
        }
    }
    for (const auto& name : outputNames) {
        if (busElemOf.find(name) != busElemOf.end()) {
            ifaceOutputs.emplace_back(name, "");
        } else {
            ifaceOutputs.emplace_back(name, fmiTypeOfCType(outputFieldType(name)));
        }
    }
    lines.push_back("typedef struct {");
    for (const auto& name : outputNames) {
        auto beIt = busElemOf.find(name);
        if (beIt != busElemOf.end()) {
            if (beIt->second.second == 0) {
                const auto& o = busPlan.outputs[(size_t)beIt->second.first];
                lines.push_back("  " + o.busName + "_t " + o.name + ";");
            }
            continue;
        }
        lines.push_back("  " + outputFieldType(name) + " " + name + ";");
    }
    if (outputNames.empty()) {
        lines.push_back("  double _unused;");
    }
    lines.push_back("} ModelOutput;");
    lines.push_back("");

    // One entry per scalar column of the generated interface: plain outputs
    // map 1:1, a struct output contributes one column per packed element
    // ("y.pos[0]", "y.sub.a"). Drives output_names[] and the CSV printing.
    struct OutColumn
    {
        std::string label; // header name (also the access path)
        std::string ctype;
    };
    std::vector<OutColumn> outColumns;
    for (const auto& name : outputNames) {
        auto beIt = busElemOf.find(name);
        if (beIt != busElemOf.end()) {
            if (beIt->second.second == 0) {
                const auto& o = busPlan.outputs[(size_t)beIt->second.first];
                for (size_t k = 0; k < o.accesses.size(); ++k) {
                    outColumns.push_back({ o.name + o.accesses[k], cTypeOf(o.leafTypes[k]) });
                }
            }
            continue;
        }
        outColumns.push_back({ name, outputFieldType(name) });
    }

    lines.push_back("static const int INPUT_COUNT = " + std::to_string(inColumns.size()) + ";");
    lines.push_back("static const int OUTPUT_COUNT = " + std::to_string(outColumns.size()) + ";");
    lines.push_back("static const char* input_names[] = {");
    for (const auto& col : inColumns) {
        lines.push_back("  \"" + col.label + "\",");
    }
    if (inColumns.empty()) {
        lines.push_back("  \"_unused\",");
    }
    lines.push_back("};");
    lines.push_back("static const char* output_names[] = {");
    for (const auto& col : outColumns) {
        lines.push_back("  \"" + col.label + "\",");
    }
    if (outColumns.empty()) {
        lines.push_back("  \"_unused\",");
    }
    lines.push_back("};");
    lines.push_back("");

    lines.push_back("void nflow_init(ModelState* s) {");
    lines.push_back("  if (!s) return;");
    for (const auto& l : initLines) {
        lines.push_back(l);
    }
    lines.push_back("}");
    lines.push_back("");
    for (const auto& tunable : dashboardPlan.tunables) {
        lines.push_back("int nflow_set_" + tunable.field + "(ModelState* s, double value) {");
        lines.push_back("  if (!s) return 0;");
        lines.push_back("  s->" + tunable.field + " = value;");
        lines.push_back("  return 1;");
        lines.push_back("}");
        lines.push_back("");
    }

    // Multi-rate detection (shared by the Euler and RK4 emission paths). A block
    // whose SampleTime is slower than the base step gates its state advance on
    // the major-step index.
    bool hasMultiRate = false;
    for (const auto& mrBlock : blocks) {
        const json mrParams = jval(mrBlock, nflow::kParams);
        if (mrParams.is_object() && mrParams.contains("SampleTime")
            && mrParams["SampleTime"].is_number() && dtVal > 0.0
            && mrParams["SampleTime"].get<double>() > dtVal * (1.0 + 1e-9)) {
            hasMultiRate = true;
            break;
        }
    }

    // When the model is integrated with a fixed-step RK solver (ode4) and every
    // stateful block declares a continuous-state contribution (continuousWidth /
    // emitRk4), emit a global RK4 scheme (a pure rhs() + a 4-stage step) over the
    // packed continuous state so the generated code matches the simulator's ode4
    // path instead of the per-block forward Euler. Any other shape (another
    // solver, a stateful block that does not join the scheme such as a discrete
    // block or a saturated integrator, or a vector/typed/bus/complex/multi-rate diagram)
    // falls back to the Euler emission.
    std::string solverName;
    if (diagram.contains("solver") && diagram["solver"].is_string()) {
        solverName = diagram["solver"].get<std::string>();
    }
    // ode45 selects the embedded adaptive Dormand-Prince 5(4) stepper instead of
    // the fixed 4-stage RK4; both share the same __nflow_rhs and packed state.
    const bool isOde45 = (solverName == "ode45");
    // Adaptive tolerances / step ceiling, mirroring DormandPrinceBackend's
    // defaults (relTol 1e-6, absTol 1e-8, maxStep unbounded). Read from the same
    // diagram fields the simulator and the GUI solver-tolerance inputs use.
    double ode45RelTol = 1e-6;
    double ode45AbsTol = 1e-8;
    double ode45MaxStep = 0.0; // 0 = unbounded (capped only by the communication step)
    if (diagram.contains("relTol") && diagram["relTol"].is_number()) {
        const double v = diagram["relTol"].get<double>();
        if (v > 0.0) {
            ode45RelTol = v;
        }
    }
    if (diagram.contains("absTol") && diagram["absTol"].is_number()) {
        const double v = diagram["absTol"].get<double>();
        if (v > 0.0) {
            ode45AbsTol = v;
        }
    }
    if (diagram.contains("maxStep") && diagram["maxStep"].is_number()) {
        const double v = diagram["maxStep"].get<double>();
        if (v > 0.0) {
            ode45MaxStep = v;
        }
    }
    // Packed continuous-state layout: each eligible continuous block occupies
    // [offset, offset + width) of the global state vector.
    struct Rk4State
    {
        std::string id; // sanitized block id
        std::string type; // block type (to re-fetch its template)
        int offset;
        int width;
        const json* block;
        const json* params;
    };
    std::vector<Rk4State> rk4Layout;
    std::unordered_map<std::string, size_t> rk4IndexById; // sanitized id -> layout index
    std::unordered_map<std::string, json> rk4ParamsById; // stable params storage for args
    int rk4NX = 0;
    const bool rk4Requested = (solverName == "ode4" || isOde45);
    std::string rk4DowngradeReason;
    bool gatedRk4 = rk4Requested;
    if (gatedRk4 && hasMultiRate) {
        gatedRk4 = false;
        rk4DowngradeReason = "the model uses a per-block SampleTime (multi-rate)";
    } else if (gatedRk4 && !codegenTypes.allScalar) {
        gatedRk4 = false;
        rk4DowngradeReason = "the model carries vector or matrix signals";
    } else if (gatedRk4 && codegenTypes.anyComplex) {
        gatedRk4 = false;
        rk4DowngradeReason = "the model carries complex signals";
    } else if (gatedRk4 && codegenTypes.anyBus) {
        gatedRk4 = false;
        rk4DowngradeReason = "the model carries bus signals";
    } else if (gatedRk4 && (anyTypedInput || anyTypedOutput)) {
        gatedRk4 = false;
        rk4DowngradeReason = "the model has int64/uint64-typed external ports";
    }
    if (gatedRk4) {
        for (const auto& gid : order) {
            auto bit = byId.find(gid);
            if (bit == byId.end()) {
                continue;
            }
            const json& gb = bit->second;
            nflow::BlockDescriptor gbd(gb, variables);
            const std::string gtype = gbd.type();
            Nelson::NFlow::BlockCodegenTemplate gctpl;
            const bool haveTpl
                = Nelson::NFlow::BlockRegistry::instance().getCodegen(gtype, "c", gctpl);
            // A block that declares the RK4 seam joins by its continuousWidth,
            // whatever pass it declares its state in (emitState or emitShared).
            if (haveTpl && gctpl.continuousWidth && gctpl.emitRk4) {
                json gp = jval(gb, nflow::kParams);
                if (gp.is_null()) {
                    gp = json::object();
                }
                rk4ParamsById[gid] = gp; // pointers into an unordered_map stay valid
                Nelson::NFlow::BlockCodegenArgs wa;
                wa.id = sanitizeId(gbd.id());
                wa.block = &gb;
                wa.params = &rk4ParamsById[gid];
                wa.variables = &variables;
                wa.dt = dtVal;
                wa.fmt = [](double v) { return ns(v); };
                const int w = gctpl.continuousWidth(wa);
                if (w < 0) {
                    gatedRk4 = false; // stateful but not RK4-compatible
                    rk4DowngradeReason = "block '" + gid + "' (" + gtype
                        + ") has no RK4-compatible state lowering";
                    break;
                }
                if (w > 0) {
                    Rk4State rs;
                    rs.id = sanitizeId(gbd.id());
                    rs.type = gtype;
                    rs.offset = rk4NX;
                    rs.width = w;
                    rs.block = &gb;
                    rs.params = &rk4ParamsById[gid];
                    rk4IndexById[rs.id] = rk4Layout.size();
                    rk4Layout.push_back(rs);
                    rk4NX += w;
                }
                continue; // w == 0: algebraic despite a state field (pure-gain tf)
            }
            // The synthetic conditional-lowering blocks are RK4-safe despite
            // declaring state: condGate (enable mode) is stateless and re-runs
            // every sub-stage like the simulator's per-DERIVATIVE gate check;
            // condHold's boundary latch is updated once per step (report-gated)
            // by its rhs-mode emission. Neither forces the Euler fallback.
            if (gtype == "condGate" || gtype == "condHold") {
                continue;
            }
            // No RK4 seam: a stateful block (emitState lambda or a declarative
            // `state` vector, e.g. derivative's der_prev) would have its state
            // mutated once per sub-stage inside rhs(), so it forces the Euler
            // fallback; a stateless block is simply skipped.
            if (haveTpl && (gctpl.emitState || !gctpl.state.empty())) {
                gatedRk4 = false;
                rk4DowngradeReason = "stateful block '" + gid + "' (" + gtype
                    + ") does not join the packed RK4 state";
                break;
            }
            continue;
        }
    }
    if (rk4NX == 0) {
        // No continuous state to integrate: fused Euler is numerically identical,
        // so this demotion is not a divergence and raises no warning by itself.
        gatedRk4 = false;
    }
    if (rk4Requested && !gatedRk4 && !rk4DowngradeReason.empty()) {
        CodegenDiagnostic d;
        d.severity = CodegenDiagnostic::Severity::Warning;
        d.gate = "solverDowngrade";
        d.message = "the requested '" + solverName
            + "' solver is not honored by the generated code (" + rk4DowngradeReason
            + "); the generated step falls back to fused forward Euler and can differ "
              "numerically from the simulation.";
        diagnostics.push_back(std::move(d));
    }
    // ode1 (or the Euler fallback of a downgraded model): the simulator's fixed-step
    // solver loop still localizes zero-crossing surfaces at order 1, but the fused
    // Euler emission carries none. Warn when the model has localizable surfaces.
    if (!gatedRk4 && (solverName == "ode1" || rk4Requested)) {
        for (const auto& b : blocks) {
            const std::string ctype = jstr(b, nflow::kType);
            Nelson::NFlow::BlockCodegenTemplate xtpl;
            if (!Nelson::NFlow::BlockRegistry::instance().getCodegen(ctype, "c", xtpl)
                || xtpl.notImplemented || !xtpl.crossingCount || !xtpl.emitRk4) {
                continue;
            }
            json bp = jval(b, nflow::kParams);
            if (!bp.is_object()) {
                bp = json::object();
            }
            Nelson::NFlow::BlockCodegenArgs xa;
            xa.id = sanitizeId(jstr(b, nflow::kId));
            xa.block = &b;
            xa.params = &bp;
            xa.variables = &variables;
            if (xtpl.crossingCount(xa) > 0) {
                CodegenDiagnostic d;
                d.severity = CodegenDiagnostic::Severity::Warning;
                d.gate = "solverDowngrade";
                d.message = "the generated fixed-step code does not localize zero-crossing "
                            "events (block '"
                    + jstr(b, nflow::kId)
                    + "'), while the simulator's fixed-step solver does; crossings land on "
                      "the next sample instead. Use ode4 or ode45 for localized events.";
                diagnostics.push_back(std::move(d));
                break;
            }
        }
    }

    auto emitOutDecls = [&]() {
        for (const auto& block : blocks) {
            const std::string rawId = jstr(block, nflow::kId);
            auto oit = outPortCount.find(rawId);
            const int nOut = (oit != outPortCount.end()) ? oit->second : 1;
            for (int p = 0; p < nOut; ++p) {
                const std::string tn = portTypeName(rawId, p);
                if (isExact64Name(tn)) {
                    lines.push_back("  " + cTypeOf(tn) + " " + codegenOutVar(rawId, p) + " = 0;");
                } else {
                    lines.push_back("  double " + codegenOutVar(rawId, p) + " = 0.0;");
                }
            }
        }
    };

    // Pre-load the outputs of latched blocks read by earlier-emitted consumers
    // (feedback cycles broken at a non-feedthrough stateful block). Emits
    // nothing for acyclic diagrams. Runs at the top of nflow_step AND of the
    // RK4 rhs() (the same ordering hazard exists in both bodies).
    std::unordered_map<std::string, json> preloadParamsById;
    auto emitLatchedPreloads = [&]() {
        for (const auto& rawId : preloadIds) {
            auto bit = byId.find(rawId);
            if (bit == byId.end()) {
                continue;
            }
            const json& block = bit->second;
            const std::string btype = jstr(block, nflow::kType);
            Nelson::NFlow::BlockCodegenTemplate ctpl;
            if (!Nelson::NFlow::BlockRegistry::instance().getCodegen(btype, "c", ctpl)
                || ctpl.notImplemented) {
                continue;
            }
            json bp = jval(block, nflow::kParams);
            if (bp.is_null()) {
                bp = json::object();
            }
            preloadParamsById[rawId] = bp;
            Nelson::NFlow::BlockCodegenArgs a;
            a.id = sanitizeId(rawId);
            // Inputs mirror the block's normal emission so an emitRk4(Rk4Output)
            // fallback that folds a feedthrough term still compiles; at this
            // point they carry the declaration defaults, matching the stale
            // input the interpreter's OUTPUT phase would read.
            std::vector<std::string> tins;
            for (int p = 0; p < 3; ++p) {
                tins.push_back(wrapExpr(getInputExpr(
                    inputs, rawId, p, ctpl.inputDefault.empty() ? "0.0" : ctpl.inputDefault)));
            }
            a.in = tins;
            a.block = &block;
            a.params = &preloadParamsById[rawId];
            a.variables = &variables;
            a.dt = dtVal;
            a.fmt = [](double v) { return ns(v); };
            a.wrap = [](const std::string& e) { return wrapExpr(e); };
            a.sanitize = [](const std::string& s) { return sanitizeId(s); };
            a.line = [&lines](const std::string& l) { lines.push_back("  " + l); };
            if (ctpl.emitOutput) {
                ctpl.emitOutput(a);
            } else if (ctpl.emitRk4) {
                a.rk4Op = Nelson::NFlow::Rk4Output;
                ctpl.emitRk4(a);
            }
        }
    };

    // Topological per-block emission. rhsMode = true emits the body of the pure
    // rhs() used by the RK4 path: a continuous integrator emits its output from
    // the (already scattered) state and its derivative via emitDeriv, instead of
    // the fused forward-Euler state update it emits in the normal step body.
    auto emitOneBlock = [&](bool rhsMode, const std::string& id) {
        auto bit = byId.find(id);
        if (bit == byId.end()) {
            return;
        }
        const json& block = bit->second;
        nflow::BlockDescriptor bd(block, variables);
        std::string bid = sanitizeId(bd.id());
        json params = jval(block, nflow::kParams);
        if (params.is_null()) {
            params = json::object();
        }
        std::string type = bd.type();

        std::string dflt = (type == "mult") ? "1.0" : "0.0";
        // Input expressions for every wired port (no fixed cap), padded to at
        // least 3 entries so native emitters can index a.in[0..2] safely.
        int numInPorts = 0;
        {
            auto iit = inputs.find(jstr(block, nflow::kId));
            if (iit != inputs.end()) {
                numInPorts = static_cast<int>(iit->second.size());
            }
        }
        std::vector<std::string> inExprs;
        inExprs.reserve(std::max(3, numInPorts));
        for (int p = 0; p < std::max(3, numInPorts); ++p) {
            inExprs.push_back(wrapExpr(getInputExpr(inputs, jstr(block, nflow::kId), p, dflt)));
        }
        std::string in0 = getInputExpr(inputs, jstr(block, nflow::kId), 0, dflt);
        std::string in0Expr = inExprs[0];

        // Raw source block ids per port (connectivity tests in native emitters)
        std::vector<std::string> inputsForBlock;
        {
            auto iit = inputs.find(jstr(block, nflow::kId));
            if (iit != inputs.end()) {
                for (const auto& ref : iit->second) {
                    inputsForBlock.push_back(ref.id);
                }
            }
        }

        lines.push_back("  // " + type + " " + jstr(block, nflow::kId));

        // Registered per-block codegen template (built-in or plugin): expand it
        // and skip the native emission chain for this block.
        {
            Nelson::NFlow::BlockCodegenTemplate ctpl;
            if (Nelson::NFlow::BlockRegistry::instance().getCodegen(type, "c", ctpl)
                && !ctpl.notImplemented
                && (ctpl.emitStep || ctpl.emitStepTyped || !ctpl.step.empty())) {
                std::vector<std::string> tins = inExprs;
                if (!ctpl.inputDefault.empty()) {
                    for (int p = 0; p < static_cast<int>(tins.size()); ++p) {
                        tins[p] = wrapExpr(
                            getInputExpr(inputs, jstr(block, nflow::kId), p, ctpl.inputDefault));
                    }
                }
                // Resolved signal types of this block's ports; exact-64
                // routes to the typed emitter when one is declared.
                const std::string outTn = portTypeName(jstr(block, nflow::kId), 0);
                std::vector<std::string> inTns;
                {
                    auto iit = inputs.find(jstr(block, nflow::kId));
                    if (iit != inputs.end()) {
                        for (const auto& ref : iit->second) {
                            inTns.push_back(ref.id.empty() ? std::string("double")
                                                           : portTypeName(ref.id, ref.port));
                        }
                    }
                }
                bool typed64 = isExact64Name(outTn);
                for (const auto& tn : inTns) {
                    typed64 = typed64 || isExact64Name(tn);
                }
                auto buildArgs = [&]() {
                    Nelson::NFlow::BlockCodegenArgs a;
                    a.id = bid;
                    a.in = tins;
                    a.inputIds = inputsForBlock;
                    a.outType = outTn;
                    a.inTypes = inTns;
                    a.block = &block;
                    a.params = &params;
                    a.variables = &variables;
                    a.dt = dtVal;
                    a.fmt = [](double v) { return ns(v); };
                    a.wrap = [](const std::string& e) { return wrapExpr(e); };
                    a.sanitize = [](const std::string& s) { return sanitizeId(s); };
                    a.line = [&lines](const std::string& l) { lines.push_back("  " + l); };
                    return a;
                };
                // RK4 rhs() body: a continuous block emits its output from the
                // scattered global state and writes its derivative into __xdot,
                // instead of the fused forward-Euler update of its normal
                // emitStep.
                if (rhsMode && ctpl.emitRk4) {
                    auto rk4it = rk4IndexById.find(bid);
                    if (rk4it != rk4IndexById.end()) {
                        const Rk4State& rs = rk4Layout[rk4it->second];
                        Nelson::NFlow::BlockCodegenArgs a = buildArgs();
                        a.stateOffset = rs.offset;
                        a.rk4Op = Nelson::NFlow::Rk4Output;
                        ctpl.emitRk4(a);
                        a.rk4Op = Nelson::NFlow::Rk4Deriv;
                        a.rk4Arr = "__xdot";
                        ctpl.emitRk4(a);
                        return;
                    }
                }
                // condHold under RK4: the boundary sample-and-hold latches its
                // input ONCE per major step (the report pass, out != NULL), not
                // on every RK sub-stage. Its output tracks the live member value
                // while the gate is open and holds the latch while it is closed
                // (a disabled member's raw output is garbage, so the ternary
                // never reads it). The fused emitStep is kept for the Euler path.
                // condLatch under RK4: the runtime latches a gate's control
                // ONCE per major step (at UPDATE), so the next step's edge is
                // measured against the value at the step boundary. In rhs mode
                // this body runs at every stage, and latching there would leave
                // the control of the LAST stage - which is the next step's own
                // sample - so no edge would ever be seen again.
                if (rhsMode && type == "condLatch") {
                    lines.push_back("  if (out) {");
                    if (ctpl.emitStep) {
                        Nelson::NFlow::BlockCodegenArgs a = buildArgs();
                        a.line = [&lines](const std::string& l) { lines.push_back("    " + l); };
                        ctpl.emitStep(a);
                    }
                    lines.push_back("  }");
                    return;
                }
                if (rhsMode && type == "condHold") {
                    const std::string gate
                        = sanitizeId(params.contains("gate") && params["gate"].is_string()
                                ? params["gate"].get<std::string>()
                                : std::string());
                    const std::string fld = "s->ch_" + bid;
                    const std::string inx = tins[0];
                    if (gate.empty()) {
                        lines.push_back("  out_" + bid + " = " + inx + ";");
                    } else {
                        lines.push_back("  if (out) { if (out_" + gate + " != 0.0) " + fld + " = "
                            + inx + "; }");
                        lines.push_back("  out_" + bid + " = (out_" + gate + " != 0.0) ? (" + inx
                            + ") : " + fld + ";");
                    }
                    return;
                }
                // condGate (level mode) under RK4: the gate is latched once
                // per major step (the report pass, out != NULL) and read before
                // it is refreshed, so every sub-stage integrates under the same
                // gate and the sample that ends a step answers under the gate
                // that governed it. Read live at stage time it would mix enabled
                // and disabled derivatives inside one step. Trigger mode keeps
                // its own edge memory and its live emission.
                if (rhsMode && type == "condGate"
                    && params.value("kind", std::string()) == "enable") {
                    const std::string lv = "s->nfgl_" + bid;
                    const std::string live = "((" + tins[0] + " > 0.0) ? 1.0 : 0.0)";
                    // out_<id> is the WIDENED gate (see levelGates).
                    lines.push_back("  const double __gp_" + bid + " = " + lv + ";");
                    lines.push_back("  if (out) { " + lv + " = " + live + "; }");
                    lines.push_back("  out_" + bid + " = (" + lv + " != 0.0 || __gp_" + bid
                        + " != 0.0) ? 1.0 : 0.0;");
                    return;
                }
                if (typed64 && ctpl.emitStepTyped) {
                    Nelson::NFlow::BlockCodegenArgs a = buildArgs();
                    ctpl.emitStepTyped(a);
                    return;
                }
                if (ctpl.emitStep) {
                    Nelson::NFlow::BlockCodegenArgs a = buildArgs();
                    ctpl.emitStep(a);
                    return;
                }
                auto resolve = [&](const std::string& tok) -> std::string {
                    if (tok == "id") {
                        return bid;
                    }
                    // Generic {inN} / {inNw} for any port index N.
                    if (tok.rfind("in", 0) == 0 && tok.size() > 2) {
                        std::string numPart = tok.substr(2);
                        if (!numPart.empty() && numPart.back() == 'w') {
                            numPart.pop_back();
                        }
                        if (!numPart.empty()
                            && numPart.find_first_not_of("0123456789") == std::string::npos) {
                            int p = std::atoi(numPart.c_str());
                            if (p >= 0 && p < static_cast<int>(tins.size())) {
                                return tins[p];
                            }
                            return ctpl.inputDefault.empty() ? "0.0" : ctpl.inputDefault;
                        }
                    }
                    if (tok == "dt") {
                        return "dt";
                    }
                    if (tok == "t") {
                        return "t";
                    }
                    if (tok.rfind("state:", 0) == 0) {
                        return "s->" + tok.substr(6) + "_" + bid;
                    }
                    if (tok.rfind("param:", 0) == 0) {
                        std::string rest = tok.substr(6);
                        std::string pname = rest;
                        double dflt = 0.0;
                        size_t colon = rest.find(':');
                        if (colon != std::string::npos) {
                            pname = rest.substr(0, colon);
                            try {
                                dflt = std::stod(rest.substr(colon + 1));
                            } catch (...) {
                            }
                        }
                        if (const auto* tunable
                            = dashboardPlan.parameterFor(jstr(block, nflow::kId), pname)) {
                            return "s->" + tunable->field;
                        }
                        if (const auto* tunable = dashboardPlan.variableFor(params, pname)) {
                            return "s->" + tunable->field;
                        }
                        return ns(bd.paramDouble(pname, dflt));
                    }
                    return "{" + tok + "}";
                };
                std::string body = expandBlockTemplate(ctpl.step, resolve);
                std::stringstream bodyStream(body);
                std::string bodyLine;
                while (std::getline(bodyStream, bodyLine)) {
                    lines.push_back("  " + bodyLine);
                }
                return;
            }
        }

        if (type == nflow::kLabelSource) {
            const LabelMapping* mapping = nullptr;
            for (const auto& m : labelSourceMappings) {
                if (m.id == jstr(block, nflow::kId)) {
                    mapping = &m;
                    break;
                }
            }
            if (mapping && !mapping->sinkId.empty()) {
                if (!mapping->sinkInput.empty()) {
                    // sinkInput already holds the generated variable name.
                    lines.push_back("  out_" + bid + " = " + mapping->sinkInput + ";");
                } else {
                    lines.push_back("  out_" + bid + " = 0.0;");
                }
            } else {
                std::string name = getLabelName(block);
                const bool exactPort = isExact64Name(portTypeName(jstr(block, nflow::kId), 0));
                const std::string zero = exactPort ? "0" : "0.0";
                auto beIt = busInElemOf.find(name);
                if (beIt != busInElemOf.end()) {
                    const auto& o = busPlan.inputs[(size_t)beIt->second.first];
                    lines.push_back("  out_" + bid + " = in ? in->" + o.name
                        + o.accesses[(size_t)beIt->second.second] + " : " + zero + ";");
                } else {
                    lines.push_back("  out_" + bid + " = in ? in->" + name + " : " + zero + ";");
                }
            }
        } else if (type == nflow::kLabelSink) {
            std::string name = getLabelName(block);
            lines.push_back("  out_" + bid + " = " + in0 + ";");
            auto beIt = busElemOf.find(name);
            if (beIt != busElemOf.end()) {
                const auto& o = busPlan.outputs[(size_t)beIt->second.first];
                lines.push_back("  if (out) out->" + o.name
                    + o.accesses[(size_t)beIt->second.second] + " = out_" + bid + ";");
            } else {
                lines.push_back("  if (out) out->" + name + " = out_" + bid + ";");
            }
        } else if (type == "mux") {
            lines.push_back("  out_" + bid + " = " + in0
                + "; /* Virtual Mux (flattened out, but kept as dummy) */");
        } else if (type == "demux") {
            lines.push_back("  out_" + bid + " = " + in0
                + "; /* Virtual Demux (flattened out, but kept as dummy) */");
        } else if (type == "expression") {
            std::string raw = jstr(params, nflow::kExpr, "u");
            std::string expr = sanitizeExpression(raw);
            std::string emitted;
            if (isSafeUserExpression(expr, variables)) {
                emitted = Nelson::NFlow::emitMathExpressionCodegen(expr, "c", in0Expr);
            }
            if (emitted.empty()) {
                // Not a plain math expression (or uses something unemittable) -
                // reject rather than emit the raw string (code-injection risk).
                // The marker keeps the source self-describing, but generation
                // must FAIL: a silent constant 0.0 diverges from the simulation.
                lines.push_back(
                    "  out_" + bid + " = 0.0; /* nflow: rejected non-math expression */");
                CodegenDiagnostic d;
                d.gate = "expressionRejected";
                d.blockId = bid;
                d.blockType = "expression";
                d.message = "the expression on block '" + bid
                    + "' was rejected by the math-only security gate and would emit a constant "
                      "0.0. Rewrite it as a plain math expression of u/t (numbers, u, t, dt, "
                      "+ - * / ^ %, parentheses and math functions), or simulate this model.";
                diagnostics.push_back(std::move(d));
                unknownTypes.insert("expression");
            } else {
                lines.push_back("  out_" + bid + " = " + emitted + ";");
            }
        } else {
            lines.push_back("  out_" + bid + " = 0.0;");
            if (type != "comment" && type != "terminator") {
                unknownTypes.insert(type);
            }
        }
    };

    // The simulator quantizes every produced element of a small-typed signal
    // (double lane + quantizeToType); mirror it by appending the quantization
    // to each small-typed output port right after its producing block
    // (idempotent when a state-holding block re-emits an already-quantized
    // value).
    auto emitSmallTypeQuantize = [&](const std::string& rawId) {
        auto oit = outPortCount.find(rawId);
        const int nOut = (oit != outPortCount.end()) ? oit->second : 1;
        bool saturate = true;
        auto bit = byId.find(rawId);
        if (bit != byId.end()) {
            json p = jval(bit->second, nflow::kParams);
            if (p.is_object() && p.contains(nflow::kSaturateOnOverflow)
                && p[nflow::kSaturateOnOverflow].is_boolean()) {
                saturate = p[nflow::kSaturateOnOverflow].get<bool>();
            }
        }
        for (int p = 0; p < nOut; ++p) {
            const std::string tn = portTypeName(rawId, p);
            const std::string v = codegenOutVar(rawId, p);
            if (tn == "single") {
                lines.push_back("  " + v + " = (double)(float)(" + v + ");");
                continue;
            }
            double qlo = 0.0;
            double qhi = 0.0;
            if (!Nelson::NFlow::codegenSmallIntRange(tn, qlo, qhi)) {
                continue;
            }
            lines.push_back("  " + v + " = nflow_quant_i(" + v + ", " + ns(qlo) + ", " + ns(qhi)
                + ", " + (saturate ? "1" : "0") + ");");
        }
    };

    // A level gate - an enable - publishes a WIDENED gate on out_<id>: besides
    // the step it governs, it covers the sample that CLOSES it, so a member is
    // evaluated once more there and the boundary hold publishes where the step
    // arrived rather than where it started (the simulator gets that for free,
    // its last derivative evaluation of the step having refreshed the held
    // outputs). The gate OF THE STEP stays in the latch, and that is what the
    // derivative mask below reads, so the extra evaluation still freezes.
    std::set<std::string> levelGates;
    for (const auto& kv : byId) {
        if (jstr(kv.second, nflow::kType) != "condGate") {
            continue;
        }
        if (nflow::BlockDescriptor(kv.second, variables).paramStr("kind", "") == "enable") {
            levelGates.insert(sanitizeId(kv.first));
        }
    }
    auto stepGateOf = [&](const std::string& g) -> std::string {
        return levelGates.count(g) ? ("s->nfgl_" + g) : ("out_" + g);
    };
    auto emitBlockBody = [&](bool rhsMode) {
        for (const auto& id : order) {
            // Conditional-subsystem member (V1 enable): wrap the whole
            // emission in the gate guard so a closed gate skips the outputs
            // AND the state updates (runtime parity: a disabled subsystem
            // neither runs nor advances its inner diagram).
            std::vector<std::string> gates;
            {
                auto bit = byId.find(id);
                if (bit != byId.end()) {
                    gates = condGatesOf(bit->second);
                }
            }
            for (const auto& g : gates) {
                lines.push_back("  if (out_" + g + " != 0.0) {");
            }
            emitOneBlock(rhsMode, id);
            if (anySmallTyped) {
                emitSmallTypeQuantize(id);
            }
            for (size_t k = 0; k < gates.size(); ++k) {
                lines.push_back("  }");
            }
            // RK4 rhs masking: a gated continuous member whose gate is closed must freeze.
            // emitOneBlock only wrote its packed derivative inside the open-gate branch, so
            // force the slice to zero when the chain is closed, like the simulator zeroing a
            // disabled subsystem's xdot. Inert on the Euler path (rk4Layout empty) and for
            // stateless members (not in rk4IndexById).
            if (rhsMode && !gates.empty()) {
                auto bit = byId.find(id);
                if (bit != byId.end()) {
                    const std::string sid
                        = sanitizeId(nflow::BlockDescriptor(bit->second, variables).id());
                    auto rk4it = rk4IndexById.find(sid);
                    if (rk4it != rk4IndexById.end()) {
                        const Rk4State& rs = rk4Layout[rk4it->second];
                        std::string cond;
                        for (size_t k = 0; k < gates.size(); ++k) {
                            cond += (k ? " && " : "") + stepGateOf(gates[k]) + " != 0.0";
                        }
                        lines.push_back("  if (!(" + cond + ")) {");
                        for (int i = 0; i < rs.width; ++i) {
                            lines.push_back(
                                "    __xdot[" + std::to_string(rs.offset + i) + "] = 0.0;");
                        }
                        lines.push_back("  }");
                    }
                }
            }
            // Reset gate: on its edge, re-seed every member state to its
            // initial value BEFORE the members run (runtime parity:
            // resetSubsystemStates precedes the step's inner run).
            {
                auto bit = byId.find(id);
                if (bit != byId.end()) {
                    const json ro = jval(jval(bit->second, nflow::kParams), "resetOf");
                    if (ro.is_string()) {
                        auto rit = resetFields.find(ro.get<std::string>());
                        auto rrit = resetRaw.find(ro.get<std::string>());
                        const bool anyField = rit != resetFields.end() && !rit->second.empty();
                        const bool anyRaw = rrit != resetRaw.end() && !rrit->second.empty();
                        if (anyField || anyRaw) {
                            lines.push_back(
                                "  if (out_" + sanitizeId(ro.get<std::string>()) + " != 0.0) {");
                            if (anyField) {
                                for (const auto& fi : rit->second) {
                                    lines.push_back("    s->" + fi.first + " = " + fi.second + ";");
                                }
                            }
                            if (anyRaw) {
                                for (const auto& rl : rrit->second) {
                                    lines.push_back("    " + rl);
                                }
                            }
                            lines.push_back("  }");
                        }
                    }
                }
            }
        }
    };

    if (!gatedRk4) {
        lines.push_back(
            "void nflow_step(ModelState* s, const ModelInput* in, ModelOutput* out, double t) {");
        lines.push_back("  const double dt = " + ns(dtVal) + ";");
        if (hasMultiRate) {
            lines.push_back("  const long __nflow_step = (long)(t / dt + 0.5);");
        }
        emitOutDecls();
        lines.push_back("");
        emitLatchedPreloads();
        emitBlockBody(false);
        lines.push_back("}");
    } else {
        const std::string N = std::to_string(rk4NX);
        // Emit one Rk4Op (gather / scatter) for a continuous block: the block's
        // template maps its native state fields to the packed array `arr`.
        auto rk4EmitOp = [&](const Rk4State& rs, int op, const std::string& arr) {
            Nelson::NFlow::BlockCodegenTemplate ctpl;
            if (!Nelson::NFlow::BlockRegistry::instance().getCodegen(rs.type, "c", ctpl)
                || !ctpl.emitRk4) {
                return;
            }
            Nelson::NFlow::BlockCodegenArgs a;
            a.id = rs.id;
            a.block = rs.block;
            a.params = rs.params;
            a.variables = &variables;
            a.dt = dtVal;
            a.fmt = [](double v) { return ns(v); };
            a.sanitize = [](const std::string& s) { return sanitizeId(s); };
            a.line = [&lines](const std::string& l) { lines.push_back("  " + l); };
            a.stateOffset = rs.offset;
            a.rk4Op = op;
            a.rk4Arr = arr;
            ctpl.emitRk4(a);
        };
        // Zero-crossing surfaces: packed offsets in diagram order.
        // The rhs gains a trailing nullable __g and writes every surface in its tail.
        const CodegenCrossScan crossScan = buildCodegenCrossScan(blocks, "c", variables);
        const auto& crossingList = crossScan.list;
        const auto& crossParams = crossScan.params;
        const int crossingTotal = crossScan.total;
        const bool hasCross = crossingTotal > 0;
        // Pure right-hand side: given the packed continuous state __x and time t,
        // scatter __x into each block's state, evaluate the graph (each continuous
        // block emits its output plus its derivative into __xdot). ModelOutput is
        // written only when out != NULL (the reporting stage), so the RK
        // sub-stages skip it.
        lines.push_back(std::string("static void __nflow_rhs(ModelState* s, const ModelInput* "
                                    "in, ModelOutput* out, double t, const double* __x, double* "
                                    "__xdot")
            + (hasCross ? ", double* __g" : "") + ") {");
        lines.push_back("  const double dt = " + ns(dtVal) + "; (void)dt;");
        for (const auto& rs : rk4Layout) {
            rk4EmitOp(rs, Nelson::NFlow::Rk4Scatter, "__x");
        }
        emitOutDecls();
        lines.push_back("");
        emitLatchedPreloads();
        emitBlockBody(true);
        if (hasCross) {
            // Surface writes at the current scattered state (sim ZERO_CROSSING).
            lines.push_back("  if (__g) {");
            for (size_t ci = 0; ci < crossingList.size(); ++ci) {
                const CodegenCrossScan::Entry& ce = crossingList[ci];
                if (!isOde45 && ce.type == "step") {
                    // Time-only surface, fixed-step path: held constant so the
                    // discontinuity is crossed rather than localised, as in the
                    // simulator's solver loop.
                    lines.push_back("    __g[" + std::to_string(ce.offset) + "] = -1.0;");
                    continue;
                }
                Nelson::NFlow::BlockCodegenTemplate xtpl;
                Nelson::NFlow::BlockRegistry::instance().getCodegen(ce.type, "c", xtpl);
                Nelson::NFlow::BlockCodegenArgs a;
                a.id = sanitizeId(ce.rawId);
                int numInPorts = 0;
                {
                    auto iit = inputs.find(ce.rawId);
                    if (iit != inputs.end()) {
                        numInPorts = static_cast<int>(iit->second.size());
                    }
                }
                for (int p = 0; p < std::max(3, numInPorts); ++p) {
                    a.in.push_back(wrapExpr(getInputExpr(inputs, ce.rawId, p, "0.0")));
                }
                a.block = ce.block;
                a.params = crossParams[ci];
                a.variables = &variables;
                a.dt = dtVal;
                a.fmt = [](double v) { return ns(v); };
                a.sanitize = [](const std::string& s) { return sanitizeId(s); };
                a.line = [&lines](const std::string& l) { lines.push_back("    " + l); };
                a.rk4Op = Nelson::NFlow::Rk4Crossing;
                a.rk4Arr = "__g";
                a.crossingOffset = ce.offset;
                xtpl.emitRk4(a);
            }
            lines.push_back("  }");
        }
        lines.push_back("}");
        lines.push_back("");
        nflow::SolverEmitSpec solverSpec;
        solverSpec.rust = false;
        solverSpec.lines = &lines;
        solverSpec.stateCount = N;
        solverSpec.dtLiteral = ns(dtVal);
        solverSpec.rtolLiteral = ns(ode45RelTol);
        solverSpec.atolLiteral = ns(ode45AbsTol);
        solverSpec.hmaxLiteral = ns(ode45MaxStep);
        solverSpec.emitSeam = [&](int op, const std::string& arr) {
            for (const auto& rs : rk4Layout) {
                rk4EmitOp(rs, op, arr);
            }
        };
        solverSpec.crossingCount = hasCross ? std::to_string(crossingTotal) : std::string();
        solverSpec.resetReseed = !resetFields.empty() || !resetRaw.empty();
        if (!isOde45) {
            // Classic 4th-order Runge-Kutta over the packed continuous state. Output
            // is reported at the pre-step state (stage 1), matching the simulator.
            nflow::emitRk4Stepper(solverSpec);
        } else {
            // Embedded adaptive Dormand-Prince 5(4) (ode45). Integrate each
            // communication step [t, t+dt] with adaptive sub-steps that land
            // exactly on t+dt, reporting the pre-step output at t. Same tableau,
            // error norm and step control as the simulator's DormandPrinceBackend;
            // k1 is recomputed each attempt (no FSAL) to match it exactly. The
            // step is re-initialised per communication step.
            nflow::emitDp45Stepper(solverSpec);
        }
    }
    lines.push_back("");

    // Symmetric teardown hook completing the init / step / terminate API. The
    // generated ModelState owns no heap resources, so the body is empty; a
    // wrapper (e.g. an FMI fmi3FreeInstance) still gets a clean anchor to call.
    lines.push_back("void nflow_terminate(ModelState* s) {");
    lines.push_back("  (void)s;");
    lines.push_back("}");
    lines.push_back("");

    std::vector<std::string> model_lines = lines;

    //=============================================================================
    // Optional main() with CSV I/O - produce as separate main_lines
    //=============================================================================
    std::vector<std::string> main_lines;
    if (includeMain) {
        nflow::CMainSpec ms;
        ms.durationLiteral = ns(mainDuration);
        ms.dtLiteral = ns(dtVal);
        ms.anyTypedInput = anyTypedInput;
        ms.hasBusInputs = hasBusInputs;
        ms.anyTypedOutput = anyTypedOutput;
        ms.hasBusOutputs = hasBusOutputs;
        ms.hasStopBlock = hasStopBlock;
        for (const auto& col : inColumns) {
            ms.inColumns.push_back({ col.label, col.ctype });
        }
        for (const auto& col : outColumns) {
            ms.outColumns.push_back({ col.label, col.ctype });
        }
        main_lines = nflow::emitCMainLines(ms);
    }

    std::string modelSrc;
    for (size_t i = 0; i < model_lines.size(); ++i) {
        if (i > 0) {
            modelSrc += '\n';
        }
        modelSrc += model_lines[i];
    }
    std::string mainSrc;
    if (includeMain) {
        for (size_t i = 0; i < main_lines.size(); ++i) {
            if (i > 0) {
                mainSrc += '\n';
            }
            mainSrc += main_lines[i];
        }
    }

    std::string cmake;
    cmake += "cmake_minimum_required(VERSION 3.15)\n";
    cmake += "project(nflow_model C)\n";
    cmake += "set(CMAKE_C_STANDARD 99)\n";
    // CMP0092 (3.15+) strips MSVC's default /W3, which would leave the
    // generated code compiling blind at /W1: pin a real warning level.
    cmake += "if(MSVC)\n  add_compile_options(/W3)\nelse()\n"
             "  add_compile_options(-Wall -Wextra)\nendif()\n";
    if (includeMain) {
        cmake += "add_executable(nflow_run main.c)\n";
        cmake += "if(NOT WIN32)\n  target_link_libraries(nflow_run PRIVATE m)\nendif()\n";
    } else {
        cmake += "add_library(nflow_model SHARED model.c)\n";
        cmake += "if(NOT WIN32)\n  target_link_libraries(nflow_model PRIVATE m)\nendif()\n";
    }

    CodegenCResult res;
    res.modelSource = std::move(modelSrc);
    res.mainSource = std::move(mainSrc);
    res.cmake = std::move(cmake);
    res.unknownTypes.assign(unknownTypes.begin(), unknownTypes.end());
    res.diagnostics = std::move(diagnostics);
    res.fmuInputs = std::move(ifaceInputs);
    res.fmuOutputs = std::move(ifaceOutputs);
    return res;
}
//=============================================================================
} // namespace nflow
//=============================================================================
