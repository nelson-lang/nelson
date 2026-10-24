//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NFlowCodegenPipeline.hpp"
#include "BlockRegistry.hpp"
#include "NFlowCodegenFlatten.hpp"
#include "NFlowCodegenHelpers.hpp"
#include "SimEngine.hpp"
#include <cmath>
#include <cstdio>
#include <set>
//=============================================================================
namespace nflow {
//=============================================================================
namespace {
    //=============================================================================
    CodegenDiagnostic
    makeError(const std::string& gate)
    {
        CodegenDiagnostic d;
        d.severity = CodegenDiagnostic::Severity::Error;
        d.gate = gate;
        return d;
    }
    //=============================================================================
} // namespace
//=============================================================================
CodegenPipelineResult
runCodegenPipeline(const CodegenPipelineRequest& request, const CodegenPipelineHooks& hooks)
{
    CodegenPipelineResult result;
    CodegenOptions opts = request.options;
    std::string diagramStr = request.diagramJson;

    // The dimension/type propagation and the generators need every block's
    // dims/type rules registered.
    Nelson::NFlow::ensureBuiltinsRegistered();

    auto fail = [&result](CodegenDiagnostic d) -> CodegenPipelineResult& {
        result.diagnostics.push_back(std::move(d));
        result.ok = false;
        return result;
    };

    // Workspace baking: failures are deliberately non-fatal (an unresolvable
    // variable leaves the diagram unbaked and the block emits its marker).
    if (hooks.resolveWorkspaceInputs) {
        std::string wsErr;
        (void)hooks.resolveWorkspaceInputs(diagramStr, wsErr);
    }

    // Early modelica reject: detected on the ORIGINAL diagram, before the FMU
    // expansion rewrites the block away.
    if (diagramStr.find("modelica") != std::string::npos) {
        try {
            json originalDiagram = json::parse(diagramStr);
            std::string badType;
            if (!firstNonGeneratableBlockId(originalDiagram, &badType).empty()
                && badType == "modelica") {
                CodegenDiagnostic d = makeError("modelica");
                d.blockType = "modelica";
                return fail(std::move(d));
            }
        } catch (const nlohmann::json::exception&) {
            // The parse stage below reports malformed model JSON.
        }
    }

    if (hooks.expandModelica) {
        std::string expandErr;
        std::string expanded = hooks.expandModelica(diagramStr, expandErr);
        if (!expandErr.empty()) {
            CodegenDiagnostic d = makeError("expandModelica");
            d.message = expandErr;
            return fail(std::move(d));
        }
        diagramStr = std::move(expanded);
    } else if (diagramStr.find("\"modelica\"") != std::string::npos) {
        CodegenDiagnostic d = makeError("hookMissing");
        d.detail = "modelica";
        return fail(std::move(d));
    }

    if (hooks.expandAcausal) {
        std::string expandErr;
        std::string expanded = hooks.expandAcausal(diagramStr, expandErr);
        if (!expandErr.empty()) {
            CodegenDiagnostic d = makeError("expandAcausal");
            d.message = expandErr;
            return fail(std::move(d));
        }
        diagramStr = std::move(expanded);
    } else if (diagramStr.find("physicalWires") != std::string::npos) {
        CodegenDiagnostic d = makeError("hookMissing");
        d.detail = "acausal";
        return fail(std::move(d));
    }

    // Opportunistic: without the hook, or after a failed reduction, the raw diagram
    // stays. A non-reducible island is then either generatable on its own (embedded
    // Newton) or rejected by the non-generatable gate.
    if (hooks.reduceLinearIslands && diagramStr.find("physicalIsland") != std::string::npos) {
        std::string reduceErr;
        std::string reduced = hooks.reduceLinearIslands(diagramStr, reduceErr);
        if (reduceErr.empty() && !reduced.empty()) {
            diagramStr = std::move(reduced);
        }
    }

    json diagram;
    try {
        diagram = json::parse(diagramStr);
    } catch (const nlohmann::json::exception&) {
        CodegenDiagnostic d = makeError("parse");
        d.message = "invalid .nflow model JSON";
        return fail(std::move(d));
    }

    if (request.useModelSampleTime && diagram.contains("sampleTime")
        && diagram["sampleTime"].is_number()) {
        const double dt = diagram["sampleTime"].get<double>();
        if (std::isfinite(dt) && dt > 0.0) {
            opts.sampleTime = dt;
        }
    }

    if (diagram.contains("solver") && diagram["solver"].is_string()) {
        const std::string solverName = diagram["solver"].get<std::string>();
        if (solverName == "variableStiff" || solverName == "variableNonstiff"
            || solverName == "cvodesBdf" || solverName == "cvodesAdams" || solverName == "dae"
            || solverName == "idas") {
            CodegenDiagnostic d = makeError("solver");
            d.detail = solverName;
            return fail(std::move(d));
        }
    }

    // Conditional gate runs on the original diagram: flattening drops the control port.
    {
        std::string condType;
        std::string condId = firstConditionalBlockId(diagram, &condType);
        if (!condId.empty()) {
            CodegenDiagnostic d = makeError("conditional");
            d.blockId = condId;
            d.blockType = condType;
            return fail(std::move(d));
        }
    }

    std::string flattenError;
    diagram = DiagramFlattener().flatten(diagram, &flattenError);
    if (!flattenError.empty()) {
        CodegenDiagnostic d = makeError("flatten");
        d.message = flattenError;
        return fail(std::move(d));
    }

    // The simulation streams its CSV itself; generated code has no file I/O. Each
    // fileSink becomes an external labelSink, a model-output field that the generated
    // runner's CSV writes as its own column (tagged with the block id). Runs after
    // flatten so fileSinks nested in subsystems are covered.
    if (diagram.contains("blocks") && diagram["blocks"].is_array()) {
        for (auto& b : diagram["blocks"]) {
            if (b.is_object() && b.contains("type") && b["type"].is_string()
                && b["type"].get<std::string>() == "fileSink") {
                b["type"] = "labelSink";
                if (!b.contains("params") || !b["params"].is_object()) {
                    b["params"] = json::object();
                }
                const std::string bid = (b.contains("id") && b["id"].is_string())
                    ? b["id"].get<std::string>()
                    : std::string("fileSink");
                b["params"]["GotoTag"] = bid;
                b["params"]["isExternalPort"] = true;
            }
        }
    }

    {
        std::string badType;
        std::string badDetail;
        std::string badId = firstNonGeneratableBlockId(diagram, &badType, &badDetail);
        if (!badId.empty()) {
            CodegenDiagnostic d = makeError("nonGeneratable");
            d.blockId = badId;
            d.blockType = badType;
            d.detail = badDetail;
            return fail(std::move(d));
        }
    }

    // Same rules as the simulator (SimEngineMultirate): integer sub-rates only,
    // self-scheduled block types keep their own Ts parameter, and the blocks whose
    // emitters honor a slower SampleTime come from the registry
    // (BlockCodegenTemplate::multirateCapable), not a hardcoded list.
    {
        double baseDt = 0.01;
        if (diagram.contains("sampleTime") && diagram["sampleTime"].is_number()) {
            baseDt = diagram["sampleTime"].get<double>();
        }
        if (baseDt <= 0.0) {
            baseDt = 0.01;
        }
        if (diagram.contains("blocks") && diagram["blocks"].is_array()) {
            const std::string tplLang = (opts.target == "rust") ? "rust" : "c";
            for (const auto& b : diagram["blocks"]) {
                if (!b.contains("params") || !b["params"].is_object()) {
                    continue;
                }
                const auto& p = b["params"];
                if (!p.contains("SampleTime")) {
                    continue;
                }
                const std::string bt = b.value("type", std::string());
                const std::string bid = b.value("id", std::string());
                if (!p["SampleTime"].is_number()) {
                    if (p["SampleTime"].is_string()) {
                        // The simulator ignores a non-numeric SampleTime; warn
                        // so the user learns the block runs at the base rate.
                        CodegenDiagnostic w;
                        w.severity = CodegenDiagnostic::Severity::Warning;
                        w.gate = "sampleTimeIgnored";
                        w.blockId = bid;
                        w.message = "block '" + bid
                            + "': SampleTime is not numeric and is ignored (the block runs at "
                              "the base rate, as in the simulation); use a numeric SampleTime.";
                        result.diagnostics.push_back(std::move(w));
                    }
                    continue;
                }
                const double ts = p["SampleTime"].get<double>();
                if (ts <= baseDt * (1.0 + 1e-9)) {
                    continue;
                }
                // These blocks gate their own advance on their own sample time,
                // so they need no generic multi-rate schedule; see
                // computeUpdateRates() and blockSelfSchedulesRate().
                if (nflow::blockSelfSchedulesRate(bt)) {
                    continue;
                }
                const double ratio = ts / baseDt;
                const double rounded = std::floor(ratio + 0.5);
                if (std::abs(ratio - rounded) > 1e-6 * rounded) {
                    char tsBuf[32];
                    char dtBuf[32];
                    std::snprintf(tsBuf, sizeof(tsBuf), "%g", ts);
                    std::snprintf(dtBuf, sizeof(dtBuf), "%g", baseDt);
                    CodegenDiagnostic d = makeError("multirateNonInteger");
                    d.blockId = bid;
                    d.blockType = bt;
                    d.message = std::string("block '") + bid + "' SampleTime " + tsBuf
                        + " is not an integer multiple of the base step " + dtBuf
                        + " (multi-rate requires integer sub-rates).";
                    return fail(std::move(d));
                }
                Nelson::NFlow::BlockCodegenTemplate tpl;
                const bool capable
                    = Nelson::NFlow::BlockRegistry::instance().getCodegen(bt, tplLang, tpl)
                    && tpl.multirateCapable;
                if (!capable) {
                    CodegenDiagnostic d = makeError("multirate");
                    d.blockId = bid;
                    d.blockType = bt;
                    return fail(std::move(d));
                }
            }
        }
    }

    {
        json lowered = lowerComplexDiagram(diagram);
        if (lowered.is_object() && lowered.contains("error")) {
            CodegenDiagnostic d = makeError("complexLower");
            d.message = lowered["error"].get<std::string>();
            return fail(std::move(d));
        }
        diagram = std::move(lowered);
    }

    {
        VectorExpandResult xr = expandVectorDiagram(diagram);
        if (!xr.ok) {
            CodegenDiagnostic d = makeError("vectorExpand");
            d.message = xr.error;
            d.blockId = xr.errorBlockId;
            return fail(std::move(d));
        }
        diagram = std::move(xr.diagram);
    }

    std::vector<std::string> unknownTypes;
    if (opts.target == "rust") {
        result.rust = generateRust(diagram, opts);
        for (const auto& d : result.rust.diagnostics) {
            result.diagnostics.push_back(d);
        }
        unknownTypes = result.rust.unknownTypes;
    } else {
        result.c = generateC(diagram, opts);
        for (const auto& d : result.c.diagnostics) {
            result.diagnostics.push_back(d);
        }
        unknownTypes = result.c.unknownTypes;
    }
    if (!unknownTypes.empty()) {
        std::string list;
        for (const auto& t : unknownTypes) {
            if (!list.empty()) {
                list += ", ";
            }
            list += t;
        }
        CodegenDiagnostic d = makeError("unknownTypes");
        d.detail = list;
        result.diagnostics.push_back(std::move(d));
    }

    if (request.failOnWarning) {
        for (auto& d : result.diagnostics) {
            if (d.severity == CodegenDiagnostic::Severity::Warning) {
                d.severity = CodegenDiagnostic::Severity::Error;
            }
        }
    }
    result.ok = !result.hasError();
    return result;
}
//=============================================================================
std::string
composeCodegenMessageEnglish(const CodegenDiagnostic& d)
{
    if (d.gate == "modelica" || (d.gate == "nonGeneratable" && d.blockType == "modelica")) {
        return "Code generation is not supported for Modelica blocks; they are compiled to "
               "FMUs for simulation, and generated standalone code cannot embed the FMU "
               "runtime. Remove Modelica blocks before generating code.";
    }
    if (d.gate == "hookMissing") {
        return "Code generation: this model requires an interpreter pass (" + d.detail
            + " expansion) that is not available on this path. Generate it with "
              "nflow_codegenerate from the Nelson command line instead.";
    }
    if (d.gate == "parse") {
        return "Code generation: " + d.message;
    }
    if (d.gate == "solver") {
        return "Code generation is not supported for the variable-step solver '" + d.detail
            + "': it has no embedded lowering, so the generated fixed-step code would "
              "differ from the simulation. Select the fixed-step 'ode4' (RK4) / 'ode1' or "
              "the adaptive 'ode45' solver for code generation, or simulate this model.";
    }
    if (d.gate == "conditional") {
        return "Code generation is not yet supported for conditional execution (" + d.blockType
            + ", block '" + d.blockId
            + "'): the control-port gate and if/switchCase/merge action selection "
              "have no C/Rust lowering. Simulate these models with the solver instead.";
    }
    if (d.gate == "nonGeneratable") {
        if (d.blockType == "nelsonFunction") {
            return "Code generation is not supported for nelsonFunction blocks "
                   "(they run interpreted Nelson code). Replace them with the "
                   "expression block or an equivalent subsystem.";
        }
        if (d.blockType == "integrator") {
            return "Code generation is not supported for an integrator with an external "
                   "reset or an external initial condition ('"
                + d.detail
                + "'): a discrete event on a continuous state has no C / Rust lowering, and "
                  "emitting an integrator that never resets would not be this model. "
                  "Simulate it, or reset through a resettable subsystem.";
        }
        if (d.blockType == "constraint") {
            return "Code generation is not supported for constraint (differential-"
                   "algebraic) blocks; they are solved by the IDAS runtime. Simulate "
                   "these models with the 'dae' solver instead.";
        }
        if (d.blockType == "physicalIsland") {
            return "Code generation for this physical (acausal) island is not supported: "
                   "the embedded-Newton lowering (C and Rust) covers resistor/conductor/"
                   "capacitor/inductor/source/sensor/diode/radiation islands, but this one "
                   "uses a component kind outside that set. Simulate these models with the "
                   "'dae' solver instead.";
        }
        if (d.blockType == "planarMechanicalIsland") {
            return "Code generation for this planar mechanical island is not supported: "
                   "the joint-free lowering (C and Rust) covers bodies, loads, "
                   "spring-dampers and position/speed/frame sensors, but this island uses "
                   "the component kind '"
                + d.detail
                + "', which needs the runtime DAE solver. Simulate these models with the "
                  "'dae' solver instead.";
        }
        return "Code generation is not supported for FMU (FMI Co-Simulation) "
               "blocks; the generated program cannot embed the FMU runtime. "
               "Remove FMU blocks before generating code.";
    }
    if (d.gate == "multirate") {
        return "Code generation is not yet supported for a multi-rate SampleTime "
               "on this block type (block '"
            + d.blockId
            + "'). It is supported on discrete state-holding blocks (unit delay, "
              "difference, counters, edge and change detectors); for other blocks "
              "remove the per-block SampleTime or simulate with the solver "
              "instead.";
    }
    if (d.gate == "unknownTypes") {
        return "Code generation is not implemented for these block types: " + d.detail;
    }
    if (d.gate == "workspace" || d.gate == "expandModelica" || d.gate == "expandAcausal"
        || d.gate == "flatten" || d.gate == "complexLower" || d.gate == "vectorExpand") {
        std::string suffix;
        if ((d.gate == "vectorExpand" || d.gate == "complexLower") && !d.blockId.empty()) {
            suffix = " (block '" + d.blockId + "')";
        }
        return "Code generation: " + d.message + suffix;
    }
    // Generator-produced findings (algebraicLoop, typedPort, expressionRejected
    // as errors; solverDowngrade and friends as warnings): errors carry the
    // builtin's "Code generation: " prefix, warnings surface verbatim.
    if (d.severity == CodegenDiagnostic::Severity::Error) {
        return "Code generation: " + d.message;
    }
    return d.message;
}
//=============================================================================
bool
hasNativeCodegenPath(const std::string& type)
{
    static const std::set<std::string> kNative = {
        // flattened away before emission
        "subsystem",
        // native emitter branches in both generators
        "labelSource",
        "labelSink",
        // rewritten by the pipeline into an external labelSink
        "fileSink",
        "expression",
        "mux",
        "demux",
        "terminator",
        "comment",
        // erased by the vector expansion (element rewiring)
        "selector",
        "reshape",
        "concatenate",
        "busCreator",
        "busSelector",
        "busAssignment",
        "assignment",
        // lowered by the vector expansion (scalar blocks / a width constant)
        "matmul",
        "width",
        // erased by the complex lowering (re/im rewiring)
        "realImagToComplex",
        "complexToRealImag",
        "magnitudeAngleToComplex",
        "complexToMagnitudeAngle",
        "conjugate",
    };
    if (kNative.count(type) > 0) {
        return true;
    }
    // Dashboard blocks are stripped (observers) or become tunable state
    // fields with a generated setter.
    return type.rfind("dashboard", 0) == 0;
}
//=============================================================================
} // namespace nflow
//=============================================================================
