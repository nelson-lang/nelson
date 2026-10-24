//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
//
// NFlowCodegenRust.cpp
// Rust no_std code generator for NFlow diagrams.
//
// Design notes
//=============================================================================
// * Diagram analysis (execution order, label wiring, TF discretisation, delay
//   sizes) is shared with the C generator through NFlowCodegenHelpers,
//   NFlowCodegenPolynomial and BlockDescriptor. Only the emission layer is
//   specific to Rust.
//
// * The generated crate is `#![no_std]`. Math primitives (sin, cos, sqrt, fabs)
//   are emitted as calls to the `libm` crate, so the code compiles on any target
//   that provides libm (bare-metal, WASM, RTOS).
//
// * Delay ring buffers and TF/DTF state arrays use fixed-size arrays `[f64; N]`
//   sized at generation time, as in the C generator. No heap is required.
//
// * The public API mirrors the C API:
//     pub fn nflow_init(s: &mut ModelState)
//     pub fn nflow_step(s: &mut ModelState, inp: &ModelInput,
//                       out: &mut ModelOutput, t: f64)
//
// * An optional `Cargo.toml` snippet is returned with the source in
//   CodegenRustResult.
//=============================================================================
#include "NFlowCodegenRust.hpp" // declares generateRust / CodegenRustResult
#include "NFlowVersion.hpp"
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
#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdlib>
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
// Internal helpers - Rust-specific emission
//=============================================================================
namespace {
    // Format a double as a Rust f64 literal. Rust requires the decimal point.
    std::string
    rf(double v)
    {
        if (std::isinf(v)) {
            return v > 0 ? "f64::INFINITY" : "f64::NEG_INFINITY";
        }
        if (std::isnan(v)) {
            return "f64::NAN";
        }
        std::ostringstream ss;
        ss.precision(17);
        ss << v;
        std::string s = ss.str();
        if (s.find('.') == std::string::npos && s.find('e') == std::string::npos) {
            s += ".0";
        }
        return s + "_f64";
    }
    //=============================================================================
    // Sanitized block id to SCREAMING_SNAKE_CASE for Rust const names (clippy lint
    // `non_upper_case_globals`).
    std::string
    toUpperConst(const std::string& id)
    {
        std::string r = id;
        for (auto& c : r) {
            c = static_cast<char>(std::toupper(static_cast<unsigned char>(c)));
        }
        return r;
    }
    //=============================================================================
    // Wrap a Rust expression so it can safely appear inside a larger expr.
    std::string
    rWrap(const std::string& e)
    {
        if (e.size() > 2 && e.front() == '(' && e.back() == ')') {
            return e;
        }
        return "(" + e + ")";
    }
    //=============================================================================
    // Map a C input expression to Rust: numeric literals get an _f64 suffix.
    std::string
    toRustExpr(const std::string& cExpr)
    {
        if (!cExpr.empty() && (std::isdigit((unsigned char)cExpr[0]) || cExpr[0] == '-')) {
            try {
                size_t pos = 0;
                double v = std::stod(cExpr, &pos);
                if (pos == cExpr.size()) {
                    return rf(v);
                }
            } catch (...) {
            }
        }
        return cExpr;
    }
    //=============================================================================
    // Only the functions actually emitted by the block handlers are listed.
    std::string
    libmCall(const std::string& cfn, const std::string& arg)
    {
        static const std::unordered_map<std::string, std::string> map {
            { "fabs", "libm::fabs" },
            { "fmax", "libm::fmax" },
            { "fmin", "libm::fmin" },
            { "sin", "libm::sin" },
            { "cos", "libm::cos" },
            { "sqrt", "libm::sqrt" },
            { "exp", "libm::exp" },
            { "log", "libm::log" },
            { "pow", "libm::pow" },
            { "floor", "libm::floor" },
            { "ceil", "libm::ceil" },
            { "atan2", "libm::atan2" },
            { "tanh", "libm::tanh" },
        };
        auto it = map.find(cfn);
        std::string fn = (it != map.end()) ? it->second : ("libm::" + cfn);
        return fn + "(" + arg + ")";
    }
    //=============================================================================
    // Rust expression for an input wire. The variable naming matches the C generator,
    // so this delegates to the shared per-port helper.
    std::string
    rInputExpr(const CodegenInputsMap& inputs, const std::string& blockId, int index,
        const std::string& dflt)
    {
        return getInputExpr(inputs, blockId, index, dflt);
    }
    //=============================================================================
} // anonymous namespace
//=============================================================================
CodegenRustResult
generateRust(const json& sourceDiagram, const CodegenOptions& opts)
{
    // Codegen may run before any simulation: make sure the block registry is
    // populated so per-block codegen templates (built-in and plugin) resolve.
    Nelson::NFlow::ensureBuiltinsRegistered();
    DashboardCodegenPlan dashboardPlan = buildDashboardCodegenPlan(sourceDiagram, "rust");
    const json& diagram = dashboardPlan.diagram;

    double sampleTime = opts.sampleTime;
    double dtVal = (sampleTime > 0) ? sampleTime : 0.01;

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

    json blocksJson = diagram.contains(nflow::kBlocks) ? diagram[nflow::kBlocks] : json::array();
    json connsJson
        = diagram.contains(nflow::kConnections) ? diagram[nflow::kConnections] : json::array();
    json varsJson
        = diagram.contains(nflow::kVariables) ? diagram[nflow::kVariables] : json::object();
    VarMap variables = buildVarMap(varsJson);

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
    // outPortCount[id] = number of generated output variables per block
    // (out_<id>, out_<id>_p1, ...).
    std::unordered_map<std::string, int> outPortCount;
    for (const auto& b : blocks) {
        std::string id = jstr(b, nflow::kId);
        byId[id] = b;
        inputs[id] = {};
        outPortCount[id] = std::max(1, jint(b, nflow::kOutputs, 1));
    }
    for (const auto& conn : connections) {
        std::string to = jstr(conn, nflow::kTo);
        int toI = jint(conn, nflow::kToIndex, 0);
        std::string from = jstr(conn, nflow::kFrom);
        int fromI = jint(conn, nflow::kFromIndex, 0);
        auto& list = inputs[to];
        if ((int)list.size() <= toI) {
            list.resize(toI + 1);
        }
        list[toI] = { from, fromI };
        auto oit = outPortCount.find(from);
        if (oit != outPortCount.end()) {
            oit->second = std::max(oit->second, fromI + 1);
        }
    }

    // Exact-64 ports get typed Rust variables (i64/u64) and integer-space arithmetic.
    nflow::CodegenWidths codegenTypes = nflow::computeCodegenWidths(diagram);
    std::vector<CodegenDiagnostic> diagnostics;
    // single and the small integer types get a generator-side quantization
    // after each producing block (mirroring the simulator's double-lane
    // computation + quantizeToType); the integer form needs a helper fn in
    // the emitted crate.
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
    auto rustTypeOf = [](const std::string& tn) -> std::string {
        if (tn == "int64") {
            return "i64";
        }
        if (tn == "uint64") {
            return "u64";
        }
        return "f64";
    };

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
        labelSinkByName[getLabelName(b)] = jstr(b, nflow::kId);
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

    // Field types follow the signal feeding each label sink (exact-64 ports keep
    // their integer type).
    auto outputFieldType = [&](const std::string& name) -> std::string {
        auto sit = labelSinkByName.find(name);
        if (sit == labelSinkByName.end()) {
            return "f64";
        }
        return rustTypeOf(portTypeName(sit->second, 0));
    };

    // The vector expansion records label sinks fed by a named nonvirtual bus;
    // their per-element fields re-group into one #[repr(C)] struct-typed
    // ModelOutput field. A group whose element sinks did not all survive as
    // output labels falls back to the flattened per-element fields.
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

    // Bus-typed external label sources re-group into one struct-typed ModelInput
    // field, like the output grouping above.
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
    auto inputFieldType = [&](const std::string& name) -> std::string {
        auto iit = inputSourceByName.find(name);
        if (iit == inputSourceByName.end()) {
            return "f64";
        }
        return rustTypeOf(portTypeName(iit->second, 0));
    };

    // One entry per scalar column of the input interface: plain inputs map
    // 1:1, a struct input contributes one column per packed element
    // ("u.pos[0]", "u.sub.a"). Drives the CSV header mapping and filling.
    struct InColumn
    {
        std::string label; // header name (also the access path)
        std::string rtype;
    };
    std::vector<InColumn> inColumns;
    for (const auto& name : inputNames) {
        auto beIt = busInElemOf.find(name);
        if (beIt != busInElemOf.end()) {
            if (beIt->second.second == 0) {
                const auto& o = busPlan.inputs[(size_t)beIt->second.first];
                for (size_t k = 0; k < o.accesses.size(); ++k) {
                    inColumns.push_back({ o.name + o.accesses[k], rustTypeOf(o.leafTypes[k]) });
                }
            }
            continue;
        }
        inColumns.push_back({ name, inputFieldType(name) });
    }

    // One entry per scalar column of the generated interface: plain outputs
    // map 1:1, a struct output contributes one column per packed element
    // ("y.pos[0]", "y.sub.a"). Drives output_names and the CSV printing.
    struct OutColumn
    {
        std::string label; // header name (also the access path)
        std::string rtype;
    };
    std::vector<OutColumn> outColumns;
    for (const auto& name : outputNames) {
        auto beIt = busElemOf.find(name);
        if (beIt != busElemOf.end()) {
            if (beIt->second.second == 0) {
                const auto& o = busPlan.outputs[(size_t)beIt->second.first];
                for (size_t k = 0; k < o.accesses.size(); ++k) {
                    outColumns.push_back({ o.name + o.accesses[k], rustTypeOf(o.leafTypes[k]) });
                }
            }
            continue;
        }
        outColumns.push_back({ name, outputFieldType(name) });
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
    // them.
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
        if (!Nelson::NFlow::BlockRegistry::instance().getCodegen(btype, "rust", ctpl)
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
    std::vector<FilterInfo> lpfBlocks, hpfBlocks;
    std::vector<DelayInfo> delayBlocks;

    std::vector<std::string> stateFields; // e.g. "int_foo: f64,"
    std::vector<std::string> initLines; // e.g. "s.int_foo = 0.0_f64;"
    std::vector<std::string> constLines; // module-level consts / statics
    std::vector<std::string> helperLines; // helper fn bodies

    auto addField = [&](const std::string& name, const std::string& init = "0.0_f64",
                        const std::string& ftype = "f64") {
        stateFields.push_back("    pub " + name + ": " + ftype + ",");
        initLines.push_back("    s." + name + " = " + init + ";");
    };
    for (const auto& tunable : dashboardPlan.tunables) {
        addField(tunable.field, rf(tunable.initialValue));
    }

    // Block types that fall back to the default emission (reported to callers).
    std::set<std::string> unknownTypes;
    unknownTypes.insert(dashboardPlan.errors.begin(), dashboardPlan.errors.end());

    // Shared once-keys for per-block state emitters.
    std::set<std::string> codegenOnceKeys;

    // Reset-subsystem members (params.__nfgReset = gate id): their state
    // fields and initializers, collected during the state pass so the gate's
    // edge can re-seed them (emitted right after the condGate block).
    // resetRaw carries the RAW addInit lines of members whose state is
    // declared via declState (ddelay, ...): the runtime re-runs the whole
    // INIT phase on a reset edge, so those lines are replayed verbatim too.
    std::map<std::string, std::vector<std::pair<std::string, std::string>>> resetFields;
    std::map<std::string, std::vector<std::string>> resetRaw;
    std::string currentResetGate;

    auto makeFieldArgs = [&](const std::string& sid, const json* blk, const json* prm) {
        Nelson::NFlow::BlockCodegenStateArgs sa;
        sa.id = sid;
        sa.block = blk;
        sa.params = prm;
        sa.variables = &variables;
        sa.dt = dtVal;
        sa.fmt = [](double v) { return rf(v); };
        sa.addState = [&](const std::string& n, const std::string& init, const std::string& ftype) {
            addField(n, init.empty() ? "0.0_f64" : init, ftype.empty() ? "f64" : ftype);
            if (!currentResetGate.empty()) {
                resetFields[currentResetGate].push_back({ n, init.empty() ? "0.0_f64" : init });
            }
        };
        sa.declState = [&](const std::string& l) { stateFields.push_back(l); };
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
            if (Nelson::NFlow::BlockRegistry::instance().getCodegen(type, "rust", ctpl)
                && !ctpl.notImplemented && ctpl.emitState) {
                auto sa = makeFieldArgs(id, &block, &params);
                ctpl.emitState(sa);
            } else if (Nelson::NFlow::BlockRegistry::instance().getCodegen(type, "rust", ctpl)
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
                            return "s." + tunable->field;
                        }
                        if (const auto* tunable = dashboardPlan.variableFor(params, pname)) {
                            return "s." + tunable->field;
                        }
                        return rf(bd.paramDouble(pname, dflt));
                    }
                    return "{" + tok + "}";
                };
                for (const auto& sv : ctpl.state) {
                    const std::string init
                        = sv.init.empty() ? "0.0_f64" : expandBlockTemplate(sv.init, resolveInit);
                    addField(sv.name + "_" + id, init);
                    if (!currentResetGate.empty()) {
                        resetFields[currentResetGate].push_back({ sv.name + "_" + id, init });
                    }
                }
            }
        }
    }

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
            if (Nelson::NFlow::BlockRegistry::instance().getCodegen(btype, "rust", st)
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
            auto sa = makeFieldArgs(sanitizeId(sbd.id()), item.block, &sparams);
            item.tpl.emitShared(sa);
        }
    }

    std::vector<std::string> lines;

    lines.push_back("// Generated by NFlow " NFLOW_PRODUCT_VERSION " - Rust no_std target");
    lines.push_back("// Do not edit manually.");
    lines.push_back("#![no_std]");
    lines.push_back("");
    lines.push_back("// Math primitives via the `libm` crate (no_std compatible).");
    lines.push_back("// Add to Cargo.toml: libm = \"0.2\"");
    lines.push_back("extern crate libm;");
    lines.push_back("");

    for (const auto& l : constLines) {
        lines.push_back(l);
    }
    if (!constLines.empty()) {
        lines.push_back("");
    }

    // Emit as `pub const UPPER_NAME` so rustc doesn't warn about
    // non-upper-case globals or unused private constants.
    if (varsJson.is_object()) {
        bool any = false;
        for (auto it = varsJson.begin(); it != varsJson.end(); ++it) {
            std::string nm = sanitizeId(it.key());
            double val = 0;
            auto vi = variables.find(it.key());
            if (vi != variables.end()) {
                val = vi->second;
            }
            std::string cn = toUpperConst(nm);
            lines.push_back("pub const " + cn + ": f64 = " + rf(val) + ";");
            any = true;
        }
        if (any) {
            lines.push_back("");
        }
    }

    lines.push_back("pub const INPUT_COUNT:  usize = " + std::to_string(inColumns.size()) + ";");
    lines.push_back("pub const OUTPUT_COUNT: usize = " + std::to_string(outColumns.size()) + ";");
    lines.push_back("");

    for (const auto& l : helperLines) {
        lines.push_back(l);
    }
    if (anySmallInt) {
        // Small-integer quantization: NaN -> 0, round to nearest, then clamp
        // (saturate) or wrap modulo the type span, exactly like the
        // simulator's quantizeToType.
        lines.push_back("#[allow(dead_code)]");
        lines.push_back("fn nflow_quant_i(v: f64, lo: f64, hi: f64, sat: bool) -> f64 {");
        lines.push_back("    if v != v { return 0.0; }");
        lines.push_back("    let r = libm::round(v);");
        lines.push_back("    if sat { return if r < lo { lo } else if r > hi { hi } else { r }; }");
        lines.push_back("    let span = hi - lo + 1.0;");
        lines.push_back("    let mut m = libm::fmod(r - lo, span);");
        lines.push_back("    if m < 0.0 { m += span; }");
        lines.push_back("    lo + m");
        lines.push_back("}");
        lines.push_back("");
    }

    lines.push_back("#[derive(Clone)]");
    lines.push_back("pub struct ModelState {");
    for (const auto& f : stateFields) {
        lines.push_back(f);
    }
    if (stateFields.empty()) {
        lines.push_back("    pub _unused: f64,");
    }
    lines.push_back("}");
    lines.push_back("");
    if (!dashboardPlan.tunables.empty()) {
        lines.push_back("impl ModelState {");
        for (const auto& tunable : dashboardPlan.tunables) {
            lines.push_back("    pub fn set_" + tunable.field + "(&mut self, value: f64) {");
            lines.push_back("        self." + tunable.field + " = value;");
            lines.push_back("    }");
        }
        lines.push_back("}");
        lines.push_back("");
    }

    // Emitted before ModelInput/ModelOutput, both of which may embed them.
    if (hasBusOutputs || hasBusInputs) {
        lines.push_back("// Nonvirtual bus types");
        auto zeroOf = [](const std::string& rt) -> std::string {
            if (rt == "i64") {
                return "0_i64";
            }
            if (rt == "u64") {
                return "0_u64";
            }
            return "0.0_f64";
        };
        for (const auto& bt : busPlan.types) {
            lines.push_back("#[repr(C)]");
            lines.push_back("#[derive(Clone, Copy)]");
            lines.push_back("pub struct " + bt.name + " {");
            for (const auto& m : bt.members) {
                if (m.isBus) {
                    lines.push_back("    pub " + m.name + ": " + m.busName + ",");
                } else if (m.width > 1) {
                    std::string dimsNote;
                    if (m.dims.size() > 1) {
                        dimsNote = " // dims ";
                        for (size_t k = 0; k < m.dims.size(); ++k) {
                            if (k > 0) {
                                dimsNote += "x";
                            }
                            dimsNote += std::to_string(m.dims[k]);
                        }
                        dimsNote += ", column-major";
                    }
                    lines.push_back("    pub " + m.name + ": [" + rustTypeOf(m.typeName) + "; "
                        + std::to_string(m.width) + "]," + dimsNote);
                } else {
                    lines.push_back("    pub " + m.name + ": " + rustTypeOf(m.typeName) + ",");
                }
            }
            lines.push_back("}");
            // Explicit Default (array fields stay away from the derive limits).
            lines.push_back("impl Default for " + bt.name + " {");
            lines.push_back("    fn default() -> Self {");
            lines.push_back("        " + bt.name + " {");
            for (const auto& m : bt.members) {
                if (m.isBus) {
                    lines.push_back("            " + m.name + ": " + m.busName + "::default(),");
                } else if (m.width > 1) {
                    lines.push_back("            " + m.name + ": [" + zeroOf(rustTypeOf(m.typeName))
                        + "; " + std::to_string(m.width) + "],");
                } else {
                    lines.push_back(
                        "            " + m.name + ": " + zeroOf(rustTypeOf(m.typeName)) + ",");
                }
            }
            lines.push_back("        }");
            lines.push_back("    }");
            lines.push_back("}");
            lines.push_back("");
        }
    }

    // Field types follow the label source's declared port type; a nonvirtual
    // bus group collapses into one struct-typed field placed at its first
    // element's position.
    lines.push_back("#[derive(Clone, Default)]");
    lines.push_back("pub struct ModelInput {");
    for (const auto& nm : inputNames) {
        auto beIt = busInElemOf.find(nm);
        if (beIt != busInElemOf.end()) {
            if (beIt->second.second == 0) {
                const auto& o = busPlan.inputs[(size_t)beIt->second.first];
                lines.push_back("    pub " + o.name + ": " + o.busName + ",");
            }
            continue;
        }
        lines.push_back("    pub " + nm + ": " + inputFieldType(nm) + ",");
    }
    if (inputNames.empty()) {
        lines.push_back("    pub _unused: f64,");
    }
    lines.push_back("}");
    lines.push_back("");

    // Nonvirtual bus groups collapse into one struct-typed field placed at
    // their first element's position.
    lines.push_back("#[derive(Clone, Default)]");
    lines.push_back("pub struct ModelOutput {");
    for (const auto& nm : outputNames) {
        auto beIt = busElemOf.find(nm);
        if (beIt != busElemOf.end()) {
            if (beIt->second.second == 0) {
                const auto& o = busPlan.outputs[(size_t)beIt->second.first];
                lines.push_back("    pub " + o.name + ": " + o.busName + ",");
            }
            continue;
        }
        lines.push_back("    pub " + nm + ": " + outputFieldType(nm) + ",");
    }
    if (outputNames.empty()) {
        lines.push_back("    pub _unused: f64,");
    }
    lines.push_back("}");
    lines.push_back("");

    lines.push_back("impl Default for ModelState {");
    lines.push_back("    fn default() -> Self {");
    lines.push_back("        let mut s = unsafe { core::mem::zeroed::<ModelState>() };");
    lines.push_back("        nflow_init(&mut s);");
    lines.push_back("        s");
    lines.push_back("    }");
    lines.push_back("}");
    lines.push_back("");

    lines.push_back("pub fn nflow_init(s: &mut ModelState) {");
    for (const auto& l : initLines) {
        lines.push_back(l);
    }
    lines.push_back("}");
    lines.push_back("");

    // Multi-rate detection (shared by both emission paths).
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

    // Unified variable-step codegen gate, as in the C generator: for solver 'ode4'
    // with every stateful block joining the RK4 scheme (continuousWidth / emitRk4)
    // over an all-scalar/double diagram, emit a global rhs() + 4-stage step.
    // Otherwise keep the per-block Euler emission.
    std::string solverName;
    if (diagram.contains("solver") && diagram["solver"].is_string()) {
        solverName = diagram["solver"].get<std::string>();
    }
    // ode45 selects the embedded adaptive Dormand-Prince 5(4) stepper instead of
    // the fixed 4-stage RK4; both share the same __nflow_rhs and packed state.
    const bool isOde45 = (solverName == "ode45");
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
    bool anyTyped = false;
    if (codegenTypes.ok) {
        for (const auto& kv : codegenTypes.outTypes) {
            for (const auto& tn : kv.second) {
                if (isExact64Name(tn)) {
                    anyTyped = true;
                }
            }
        }
    }
    struct Rk4State
    {
        std::string id;
        std::string type;
        int offset;
        int width;
        const json* block;
        const json* params;
    };
    std::vector<Rk4State> rk4Layout;
    std::unordered_map<std::string, size_t> rk4IndexById;
    std::unordered_map<std::string, json> rk4ParamsById;
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
    } else if (gatedRk4 && anyTyped) {
        gatedRk4 = false;
        rk4DowngradeReason = "the model carries int64/uint64-typed signals";
    } else if (gatedRk4 && (hasBusInputs || hasBusOutputs)) {
        gatedRk4 = false;
        rk4DowngradeReason = "the model has bus-typed external ports";
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
                = Nelson::NFlow::BlockRegistry::instance().getCodegen(gtype, "rust", gctpl);
            // A block that declares the RK4 seam joins by its continuousWidth,
            // whatever pass it declares its state in (the Rust tf declares its
            // state field in emitShared, not emitState).
            if (haveTpl && gctpl.continuousWidth && gctpl.emitRk4) {
                json gp = jval(gb, nflow::kParams);
                if (gp.is_null()) {
                    gp = json::object();
                }
                rk4ParamsById[gid] = gp;
                Nelson::NFlow::BlockCodegenArgs wa;
                wa.id = sanitizeId(gbd.id());
                wa.block = &gb;
                wa.params = &rk4ParamsById[gid];
                wa.variables = &variables;
                wa.dt = dtVal;
                wa.fmt = [](double v) { return rf(v); };
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
                continue; // w == 0: algebraic despite a state field
            }
            // Synthetic conditional-lowering blocks are RK4-safe (see the C
            // generator): condGate re-runs every sub-stage, condHold updates its
            // boundary latch once per step. Neither forces the Euler fallback.
            if (gtype == "condGate" || gtype == "condHold") {
                continue;
            }
            // No RK4 seam: a stateful block forces the Euler fallback.
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
            if (!Nelson::NFlow::BlockRegistry::instance().getCodegen(ctype, "rust", xtpl)
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
                    const std::string rt = rustTypeOf(tn);
                    lines.push_back(
                        "    let mut " + codegenOutVar(rawId, p) + ": " + rt + " = 0_" + rt + ";");
                } else {
                    lines.push_back("    let mut " + codegenOutVar(rawId, p) + ": f64 = 0.0_f64;");
                }
            }
        }
    };

    // Pre-load the outputs of latched blocks read by earlier-emitted consumers
    // (feedback cycles broken at a non-feedthrough stateful block). Emits
    // nothing for acyclic diagrams.
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
            if (!Nelson::NFlow::BlockRegistry::instance().getCodegen(btype, "rust", ctpl)
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
            std::vector<std::string> tins;
            for (int p = 0; p < 3; ++p) {
                tins.push_back(rInputExpr(
                    inputs, rawId, p, ctpl.inputDefault.empty() ? "0.0_f64" : ctpl.inputDefault));
            }
            a.in = tins;
            a.block = &block;
            a.params = &preloadParamsById[rawId];
            a.variables = &variables;
            a.dt = dtVal;
            a.fmt = [](double v) { return rf(v); };
            a.wrap = [](const std::string& e) { return rWrap(e); };
            a.sanitize = [](const std::string& s) { return sanitizeId(s); };
            a.line = [&lines](const std::string& l) { lines.push_back("    " + l); };
            if (ctpl.emitOutput) {
                ctpl.emitOutput(a);
            } else if (ctpl.emitRk4) {
                a.rk4Op = Nelson::NFlow::Rk4Output;
                ctpl.emitRk4(a);
            }
        }
    };

    // Topological per-block emission. rhsMode = true emits the pure rhs() body:
    // a continuous block emits its output from the scattered state and writes its
    // derivative into __xdot instead of its fused Euler update.
    auto emitOneBlock = [&](bool rhsMode, const std::string& id) {
        auto bit = byId.find(id);
        if (bit == byId.end()) {
            return;
        }
        const json& block = bit->second;
        nflow::BlockDescriptor bd(block, variables);
        std::string bid = sanitizeId(bd.id());
        std::string type = bd.type();
        json params = jval(block, nflow::kParams);
        if (params.is_null()) {
            params = json::object();
        }

        std::string dflt = (type == "mult") ? "1.0_f64" : "0.0_f64";
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
            inExprs.push_back(rInputExpr(inputs, jstr(block, nflow::kId), p, dflt));
        }
        std::string in0 = inExprs[0];
        std::string in1 = inExprs[1];
        std::string in2 = inExprs[2];

        lines.push_back("    // " + type + "  " + jstr(block, nflow::kId));

        // Registered per-block codegen template (built-in or plugin): expand it
        // and skip the native emission chain for this block.
        {
            Nelson::NFlow::BlockCodegenTemplate ctpl;
            if (Nelson::NFlow::BlockRegistry::instance().getCodegen(type, "rust", ctpl)
                && !ctpl.notImplemented
                && (ctpl.emitStep || ctpl.emitStepTyped || !ctpl.step.empty())) {
                std::vector<std::string> tins = inExprs;
                if (!ctpl.inputDefault.empty()) {
                    for (int p = 0; p < static_cast<int>(tins.size()); ++p) {
                        tins[p] = rInputExpr(inputs, jstr(block, nflow::kId), p, ctpl.inputDefault);
                    }
                }
                // Resolved signal types; exact-64 routes to the typed emitter.
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
                    {
                        auto iit = inputs.find(jstr(block, nflow::kId));
                        if (iit != inputs.end()) {
                            for (const auto& ref : iit->second) {
                                a.inputIds.push_back(ref.id);
                            }
                        }
                    }
                    a.outType = outTn;
                    a.inTypes = inTns;
                    a.block = &block;
                    a.params = &params;
                    a.variables = &variables;
                    a.dt = dtVal;
                    a.fmt = [](double v) { return rf(v); };
                    a.wrap = [](const std::string& e) { return rWrap(e); };
                    a.sanitize = [](const std::string& s) { return sanitizeId(s); };
                    a.line = [&lines](const std::string& l) { lines.push_back("    " + l); };
                    return a;
                };
                // RK4 rhs() body: a continuous block emits its output from the
                // scattered state and its derivative into __xdot.
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
                // condHold under RK4: latch once per major step (the report
                // pass, __report) and track the live member value while the gate
                // is open, holding the latch while it is closed (the disabled
                // member's raw output is garbage, so the branch never reads it).
                // The fused emitStep is kept for the Euler path.
                // condLatch under RK4: latched once per major step, as in the
                // C generator (a stage-time latch would hide every later edge).
                if (rhsMode && type == "condLatch") {
                    lines.push_back("    if __report {");
                    if (ctpl.emitStep) {
                        Nelson::NFlow::BlockCodegenArgs a = buildArgs();
                        a.line = [&lines](const std::string& l) { lines.push_back("      " + l); };
                        ctpl.emitStep(a);
                    }
                    lines.push_back("    }");
                    return;
                }
                if (rhsMode && type == "condHold") {
                    const std::string gate
                        = sanitizeId(params.contains("gate") && params["gate"].is_string()
                                ? params["gate"].get<std::string>()
                                : std::string());
                    const std::string fld = "s.ch_" + bid;
                    const std::string inx = tins[0];
                    if (gate.empty()) {
                        lines.push_back("    out_" + bid + " = " + inx + ";");
                    } else {
                        lines.push_back("    if __report { if out_" + gate + " != 0.0 { " + fld
                            + " = " + inx + "; } }");
                        lines.push_back("    out_" + bid + " = if out_" + gate + " != 0.0 { " + inx
                            + " } else { " + fld + " };");
                    }
                    return;
                }
                // condGate (level mode) under RK4: latched once per major
                // step and read before it is refreshed, matching the runtime.
                if (rhsMode && type == "condGate"
                    && params.value("kind", std::string()) == "enable") {
                    const std::string lv = "s.nfgl_" + bid;
                    const std::string live = "if " + tins[0] + " > 0.0 { 1.0 } else { 0.0 }";
                    // Latched once per major step; out_<id> is the widened gate.
                    lines.push_back("    let __gp_" + bid + ": f64 = " + lv + ";");
                    lines.push_back("    if __report { " + lv + " = " + live + "; }");
                    lines.push_back("    out_" + bid + " = if " + lv + " != 0.0 || __gp_" + bid
                        + " != 0.0 { 1.0 } else { 0.0 };");
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
                        bool wrapped = false;
                        if (!numPart.empty() && numPart.back() == 'w') {
                            numPart.pop_back();
                            wrapped = true;
                        }
                        if (!numPart.empty()
                            && numPart.find_first_not_of("0123456789") == std::string::npos) {
                            int p = std::atoi(numPart.c_str());
                            std::string expr;
                            if (p >= 0 && p < static_cast<int>(tins.size())) {
                                expr = tins[p];
                            } else {
                                expr = ctpl.inputDefault.empty() ? "0.0_f64" : ctpl.inputDefault;
                            }
                            return wrapped ? rWrap(expr) : expr;
                        }
                    }
                    if (tok == "dt") {
                        return "dt";
                    }
                    if (tok == "t") {
                        return "t";
                    }
                    if (tok.rfind("state:", 0) == 0) {
                        return "s." + tok.substr(6) + "_" + bid;
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
                            return "s." + tunable->field;
                        }
                        if (const auto* tunable = dashboardPlan.variableFor(params, pname)) {
                            return "s." + tunable->field;
                        }
                        return rf(bd.paramDouble(pname, dflt));
                    }
                    return "{" + tok + "}";
                };
                std::string body = expandBlockTemplate(ctpl.step, resolve);
                std::stringstream bodyStream(body);
                std::string bodyLine;
                while (std::getline(bodyStream, bodyLine)) {
                    lines.push_back("    " + bodyLine);
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
                // sinkInput already holds the generated variable name.
                lines.push_back("    out_" + bid + " = "
                    + (mapping->sinkInput.empty() ? "0.0_f64" : mapping->sinkInput) + ";");
            } else {
                std::string nm = getLabelName(block);
                auto beIt = busInElemOf.find(nm);
                if (beIt != busInElemOf.end()) {
                    const auto& o = busPlan.inputs[(size_t)beIt->second.first];
                    lines.push_back("    out_" + bid + " = inp." + o.name
                        + o.accesses[(size_t)beIt->second.second] + ";");
                } else {
                    lines.push_back("    out_" + bid + " = inp." + nm + ";");
                }
            }
        } else if (type == nflow::kLabelSink) {
            std::string nm = getLabelName(block);
            lines.push_back("    out_" + bid + " = " + in0 + ";");
            auto beIt = busElemOf.find(nm);
            std::string tgt;
            if (beIt != busElemOf.end()) {
                const auto& o = busPlan.outputs[(size_t)beIt->second.first];
                tgt = "out." + o.name + o.accesses[(size_t)beIt->second.second];
            } else {
                tgt = "out." + nm;
            }
            // On the RK4 path the rhs runs once per sub-stage; only the reporting
            // stage (__report) writes the model output.
            if (rhsMode) {
                lines.push_back("    if __report { " + tgt + " = out_" + bid + "; }");
            } else {
                lines.push_back("    " + tgt + " = out_" + bid + ";");
            }

        } else if (type == "mux") {
            lines.push_back("    out_" + bid + " = " + in0 + "; // Virtual Mux (flattened out)");

        } else if (type == "demux") {
            lines.push_back("    out_" + bid + " = " + in0 + "; // Virtual Demux (flattened out)");

        } else if (type == "expression") {
            std::string raw = jstr(params, nflow::kExpr, "u");
            std::string san = sanitizeExpression(raw);
            std::string emitted;
            if (isSafeUserExpression(san, variables)) {
                emitted = Nelson::NFlow::emitMathExpressionCodegen(san, "rust", in0);
            }
            if (emitted.empty()) {
                // Same contract as the C generator: keep the marker, fail the
                // generation (a silent constant 0.0 diverges from the simulation).
                lines.push_back(
                    "    out_" + bid + " = 0.0_f64; // nflow: rejected non-math expression");
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
                lines.push_back("    out_" + bid + " = " + emitted + ";");
            }

        } else {
            lines.push_back("    out_" + bid + " = 0.0_f64; // unknown: " + type);
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
                lines.push_back("    " + v + " = (" + v + " as f32) as f64;");
                continue;
            }
            double qlo = 0.0;
            double qhi = 0.0;
            if (!Nelson::NFlow::codegenSmallIntRange(tn, qlo, qhi)) {
                continue;
            }
            lines.push_back("    " + v + " = nflow_quant_i(" + v + ", " + rf(qlo) + ", " + rf(qhi)
                + ", " + (saturate ? "true" : "false") + ");");
        }
    };

    // See the C generator: an enable publishes a WIDENED gate on out_<id> that
    // also covers the sample closing it, while the gate of the step stays in
    // the latch and is what the derivative mask reads.
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
        return levelGates.count(g) ? ("s.nfgl_" + g) : ("out_" + g);
    };
    auto emitBlockBody = [&](bool rhsMode) {
        for (const auto& id : order) {
            // Conditional-subsystem member (V1 enable): wrap the emission in
            // the gate guard so a closed gate skips outputs AND state updates.
            std::vector<std::string> gates;
            {
                auto bit = byId.find(id);
                if (bit != byId.end()) {
                    gates = condGatesOf(bit->second);
                }
            }
            for (const auto& g : gates) {
                lines.push_back("    if out_" + g + " != 0.0 {");
            }
            emitOneBlock(rhsMode, id);
            if (anySmallTyped) {
                emitSmallTypeQuantize(id);
            }
            for (size_t k = 0; k < gates.size(); ++k) {
                lines.push_back("    }");
            }
            // RK4 rhs masking: a gated continuous member whose gate is closed must freeze.
            // Force its packed derivative slice to zero when the chain is closed, like the
            // simulator zeroing a disabled subsystem's xdot. Inert on the Euler path and for
            // stateless members.
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
                        lines.push_back("    if !(" + cond + ") {");
                        for (int i = 0; i < rs.width; ++i) {
                            lines.push_back(
                                "        __xdot[" + std::to_string(rs.offset + i) + "] = 0.0;");
                        }
                        lines.push_back("    }");
                    }
                }
            }
            // Reset gate: on its edge, re-seed every member state to its
            // initial value BEFORE the members run (runtime parity).
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
                                "    if out_" + sanitizeId(ro.get<std::string>()) + " != 0.0 {");
                            if (anyField) {
                                for (const auto& fi : rit->second) {
                                    lines.push_back(
                                        "        s." + fi.first + " = " + fi.second + ";");
                                }
                            }
                            if (anyRaw) {
                                for (const auto& rl : rrit->second) {
                                    lines.push_back("        " + rl);
                                }
                            }
                            lines.push_back("    }");
                        }
                    }
                }
            }
        }
    };

    if (!gatedRk4) {
        lines.push_back("// Intermediate `out_*` variables mirror the C generator's local");
        lines.push_back("// wire values.  Sink/scope blocks consume them without producing");
        lines.push_back("// a visible output, so some assignments are intentionally silent.");
        lines.push_back("#[allow(unused_variables, unused_assignments, unused_mut)]");
        lines.push_back("pub fn nflow_step(s: &mut ModelState,");
        lines.push_back("                  inp: &ModelInput,");
        lines.push_back("                  out: &mut ModelOutput,");
        lines.push_back("                  t: f64) {");
        lines.push_back("    let dt: f64 = " + rf(dtVal) + ";");
        lines.push_back("    let _ = dt; // suppress unused warning when not needed");
        if (hasMultiRate) {
            lines.push_back("    let __nflow_step: i64 = (t / dt + 0.5) as i64;");
        }
        lines.push_back("");
        emitOutDecls();
        lines.push_back("");
        emitLatchedPreloads();
        emitBlockBody(false);
        lines.push_back("}");
    } else {
        const std::string N = std::to_string(rk4NX);
        // Emit one Rk4Op (gather / scatter) for a continuous block, mapping its
        // native state fields to the packed array `arr`.
        auto rk4EmitOp = [&](const Rk4State& rs, int op, const std::string& arr) {
            Nelson::NFlow::BlockCodegenTemplate ctpl;
            if (!Nelson::NFlow::BlockRegistry::instance().getCodegen(rs.type, "rust", ctpl)
                || !ctpl.emitRk4) {
                return;
            }
            Nelson::NFlow::BlockCodegenArgs a;
            a.id = rs.id;
            a.block = rs.block;
            a.params = rs.params;
            a.variables = &variables;
            a.dt = dtVal;
            a.fmt = [](double v) { return rf(v); };
            a.sanitize = [](const std::string& s) { return sanitizeId(s); };
            a.line = [&lines](const std::string& l) { lines.push_back("    " + l); };
            a.stateOffset = rs.offset;
            a.rk4Op = op;
            a.rk4Arr = arr;
            ctpl.emitRk4(a);
        };
        // Zero-crossing surfaces: packed offsets in diagram order.
        // The rhs gains a trailing Option<&mut [f64]> __g and writes every surface in
        // its tail.
        const CodegenCrossScan crossScan = buildCodegenCrossScan(blocks, "rust", variables);
        const auto& crossingList = crossScan.list;
        const auto& crossParams = crossScan.params;
        const int crossingTotal = crossScan.total;
        const bool hasCross = crossingTotal > 0;
        // Pure right-hand side: scatter __x into each block's state, evaluate the
        // graph (each continuous block emits its output plus its derivative into
        // __xdot). The model output is written only on the reporting stage
        // (__report), so the RK sub-stages leave it untouched.
        lines.push_back("#[allow(unused_variables, unused_assignments, unused_mut, "
                        "clippy::too_many_arguments, clippy::needless_range_loop)]");
        lines.push_back(
            "fn __nflow_rhs(s: &mut ModelState, inp: &ModelInput, out: &mut ModelOutput,");
        lines.push_back(std::string("               t: f64, __x: &[f64], __xdot: &mut [f64], "
                                    "__report: bool")
            + (hasCross ? ", __g: Option<&mut [f64]>" : "") + ") {");
        lines.push_back("    let dt: f64 = " + rf(dtVal) + ";");
        lines.push_back("    let _ = dt; let _ = inp; let _ = &out; let _ = __report;");
        for (const auto& rs : rk4Layout) {
            rk4EmitOp(rs, Nelson::NFlow::Rk4Scatter, "__x");
        }
        lines.push_back("");
        emitOutDecls();
        lines.push_back("");
        emitLatchedPreloads();
        emitBlockBody(true);
        if (hasCross) {
            // Surface writes at the current scattered state (sim ZERO_CROSSING).
            lines.push_back("    if let Some(__g) = __g {");
            for (size_t ci = 0; ci < crossingList.size(); ++ci) {
                const CodegenCrossScan::Entry& ce = crossingList[ci];
                if (!isOde45 && ce.type == "step") {
                    // Time-only surface, fixed-step path (see the C generator).
                    lines.push_back("        __g[" + std::to_string(ce.offset) + "] = -1.0;");
                    continue;
                }
                Nelson::NFlow::BlockCodegenTemplate xtpl;
                Nelson::NFlow::BlockRegistry::instance().getCodegen(ce.type, "rust", xtpl);
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
                a.fmt = [](double v) { return rf(v); };
                a.sanitize = [](const std::string& s) { return sanitizeId(s); };
                a.line = [&lines](const std::string& l) { lines.push_back("        " + l); };
                a.rk4Op = Nelson::NFlow::Rk4Crossing;
                a.rk4Arr = "__g";
                a.crossingOffset = ce.offset;
                xtpl.emitRk4(a);
            }
            lines.push_back("    }");
        }
        lines.push_back("}");
        lines.push_back("");
        nflow::SolverEmitSpec solverSpec;
        solverSpec.rust = true;
        solverSpec.lines = &lines;
        solverSpec.stateCount = N;
        solverSpec.dtLiteral = rf(dtVal);
        solverSpec.rtolLiteral = rf(ode45RelTol);
        solverSpec.atolLiteral = rf(ode45AbsTol);
        solverSpec.hmaxLiteral = rf(ode45MaxStep);
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
            // Embedded adaptive Dormand-Prince 5(4) (ode45): integrate each
            // communication step [t, t+dt] with adaptive sub-steps landing exactly
            // on t+dt, reporting the pre-step output at t. Same tableau, error norm
            // and step control as the simulator's DormandPrinceBackend; k1 is
            // recomputed each attempt (no FSAL) to match it.
            nflow::emitDp45Stepper(solverSpec);
        }
    }
    lines.push_back("");

    // Symmetric teardown hook completing the init / step / terminate API. The
    // generated ModelState owns no resources, so the body is empty.
    lines.push_back("pub fn nflow_terminate(s: &mut ModelState) {");
    lines.push_back("    let _ = s;");
    lines.push_back("}");
    lines.push_back("");

    std::string src;
    for (const auto& l : lines) {
        src += l + "\n";
    }

    // main.rs is a standard Rust binary (uses std) that links the lib, giving file I/O
    // and CLI args without touching the no_std core. The deployable artefact for a
    // microcontroller is the lib; the host runner is the bin.
    std::string mainSrc;
    if (opts.includeMain) {
        nflow::RustMainSpec ms;
        ms.durationLiteral = rf(mainDuration);
        ms.dtLiteral = rf(dtVal);
        ms.hasBusOutputs = hasBusOutputs;
        ms.hasStopBlock = hasStopBlock;
        for (const auto& col : inColumns) {
            ms.inColumns.push_back({ col.label, col.rtype });
        }
        for (const auto& col : outColumns) {
            ms.outColumns.push_back({ col.label, col.rtype });
        }
        ms.outputNames.assign(outputNames.begin(), outputNames.end());
        for (const auto& l : nflow::emitRustMainLines(ms)) {
            mainSrc += l + "\n";
        }
    }

    std::string cargo;
    cargo += "[package]\n";
    cargo += "name    = \"nflow_model\"\n";
    cargo += "version = \"0.1.0\"\n";
    cargo += "edition = \"2021\"\n";
    cargo += "\n";
    cargo += "[lib]\n";
    cargo += "name = \"nflow_model\"\n";
    cargo += "path = \"src/lib.rs\"\n";
    cargo += "\n";
    if (opts.includeMain) {
        cargo += "[[bin]]\n";
        cargo += "name = \"nflow_run\"\n";
        cargo += "path = \"src/main.rs\"\n";
        cargo += "\n";
    }
    cargo += "[dependencies]\n";
    cargo += "libm = \"0.2\"   # no_std-compatible math\n";
    cargo += "\n";
    cargo += "[profile.release]\n";
    cargo += "opt-level = \"z\"  # size-optimised for embedded targets\n";
    cargo += "lto       = true\n";
    cargo += "codegen-units = 1\n";

    CodegenRustResult res;
    res.rustSource = std::move(src);
    res.mainSource = std::move(mainSrc);
    res.cargoToml = std::move(cargo);
    res.unknownTypes.assign(unknownTypes.begin(), unknownTypes.end());
    res.diagnostics = std::move(diagnostics);
    return res;
}
//=============================================================================
} // namespace nflow
//=============================================================================
