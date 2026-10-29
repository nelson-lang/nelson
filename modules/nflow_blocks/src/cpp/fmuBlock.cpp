//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "FmuComponent.hpp"
#include "AssetPath.hpp"
#include <algorithm>
#include <filesystem>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // FMU (FMI 3.0 Co-Simulation) block.
    //
    //   INIT   : resolve the FMU path (relative paths are taken against the
    //            diagram baseDir), build the component through the registered
    //            factory, initialize it, and store it in BlockState::external.
    //   OUTPUT : copy the FMU's current Float64 outputs to the block output ports.
    //   UPDATE : advance the FMU by one communication step (dt).
    //
    // The FMU keeps its start-value inputs (no external inputs are wired yet), so
    // the block reproduces the free response of the model, consistent with the
    // fmiCoSimulate() scripting entry point.
    //=============================================================================
    static std::string
    fmuPathParam(const Block& b)
    {
        for (const char* key : { "path", "fmu", "file" }) {
            if (asJson(b.params).contains(key) && asJson(b.params)[key].is_string()) {
                std::string p = asJson(b.params)[key].get<std::string>();
                if (!p.empty()) {
                    return p;
                }
            }
        }
        return std::string {};
    }
    //=============================================================================
    bool
    handleFmu(SimCtx& ctx, const Block& b, Phase phase)
    {
        BlockState& st = getState(ctx, b.nid);

        if (phase == Phase::INIT) {
            std::string path = fmuPathParam(b);
            if (path.empty()) {
                if (ctx.diag) {
                    SimDiagnostic d;
                    d.code = "fmu_no_path";
                    d.blockId = b.id;
                    d.message = "FMU block has no 'path' parameter.";
                    d.time = ctx.t;
                    ctx.diag->fail(d);
                }
                return false;
            }
            const std::filesystem::path fmuPath = resolveAssetPath(ctx, path);

            SimDiagnostic diag;
            std::unique_ptr<IFmuComponent> comp = makeFmuComponent(fmuPath.u8string(), diag);
            if (!comp) {
                diag.blockId = b.id;
                diag.time = ctx.t;
                if (ctx.diag) {
                    ctx.diag->fail(diag);
                }
                return false;
            }
            // Map FMU ports to the block's declared input/output variable names.
            // Empty lists keep the full enumeration in modelDescription order.
            {
                std::vector<std::string> inNames, outNames;
                if (asJson(b.params).contains("inputNames") && asJson(b.params)["inputNames"].is_array()) {
                    for (const auto& e : asJson(b.params)["inputNames"]) {
                        if (e.is_string()) {
                            inNames.push_back(e.get<std::string>());
                        }
                    }
                }
                if (asJson(b.params).contains("outputNames") && asJson(b.params)["outputNames"].is_array()) {
                    for (const auto& e : asJson(b.params)["outputNames"]) {
                        if (e.is_string()) {
                            outNames.push_back(e.get<std::string>());
                        }
                    }
                }
                if (!comp->configurePorts(inNames, outNames, diag)) {
                    diag.blockId = b.id;
                    diag.time = ctx.t;
                    if (ctx.diag) {
                        ctx.diag->fail(diag);
                    }
                    return false;
                }
            }
            // params.parameterValues maps FMU parameter name -> value.
            std::vector<std::string> paramNames;
            std::vector<double> paramValues;
            if (asJson(b.params).contains("parameterValues") && asJson(b.params)["parameterValues"].is_object()) {
                for (const auto& item : asJson(b.params)["parameterValues"].items()) {
                    if (item.value().is_number()) {
                        paramNames.push_back(item.key());
                        paramValues.push_back(item.value().get<double>());
                    }
                }
            }
            comp->setSolverTolerance(ctx.relTol);
            if (!comp->initialize(0.0, ctx.tEnd, paramNames, paramValues, diag)) {
                diag.blockId = b.id;
                diag.time = ctx.t;
                if (ctx.diag) {
                    ctx.diag->fail(diag);
                }
                return false;
            }
            st.external = std::move(comp);
            return false;
        }

        IFmuComponent* comp = dynamic_cast<IFmuComponent*>(st.external.get());
        if (comp == nullptr) {
            // Not loaded (INIT failed): emit zeros so the graph stays well-defined.
            for (int p = 0; p < b.outputs; ++p) {
                setOutput(ctx, b.nid, 0.0, p);
            }
            return false;
        }

        if (phase == Phase::OUTPUT) {
            // Flat element buffer spanning every output port (array ports occupy
            // outputPortWidth() slots); scatter each port's slice into its signal.
            const int nElem = comp->totalOutputElements();
            std::vector<double> out(nElem > 0 ? nElem : 1, 0.0);
            SimDiagnostic diag;
            if (!comp->getOutputs(out.data(), nElem, diag)) {
                diag.blockId = b.id;
                diag.time = ctx.t;
                if (ctx.diag) {
                    ctx.diag->fail(diag);
                }
                return false;
            }
            int at = 0;
            for (int p = 0; p < comp->numOutputs(); ++p) {
                const int cw = comp->outputPortWidth(p);
                if (p < b.outputs) {
                    const int w = std::min(cw, outputWidth(ctx, b.nid, p));
                    double* y = outputSlice(ctx, b.nid, p);
                    for (int k = 0; k < w; ++k) {
                        y[k] = (at + k < nElem) ? out[at + k] : 0.0;
                    }
                }
                at += cw;
            }
            return false;
        }

        if (phase == Phase::UPDATE) {
            SimDiagnostic diag;
            const int nElem = comp->totalInputElements();
            if (nElem > 0) {
                std::vector<double> inVals(static_cast<size_t>(nElem), 0.0);
                int at = 0;
                for (int p = 0; p < comp->numInputs(); ++p) {
                    const int cw = comp->inputPortWidth(p);
                    if (p < b.inputs) {
                        SigView u = getInputSig(ctx, b.nid, p);
                        const int w = std::min(cw, std::max(1, u.width));
                        for (int k = 0; k < w && at + k < nElem; ++k) {
                            inVals[at + k] = sigAt(u, k);
                        }
                    }
                    at += cw;
                }
                if (!comp->setInputs(inVals.data(), nElem, diag)) {
                    diag.blockId = b.id;
                    diag.time = ctx.t;
                    if (ctx.diag) {
                        ctx.diag->fail(diag);
                    }
                    return false;
                }
            }
            if (!comp->doStep(ctx.t, ctx.dt, diag)) {
                diag.blockId = b.id;
                diag.time = ctx.t;
                if (ctx.diag) {
                    ctx.diag->fail(diag);
                }
            } else if (diag.requestStop) {
                // The FMU asked to terminate: end the run cleanly after this step.
                ctx.stopRequested = true;
            }
            return false;
        }

        return false;
    }
    //=============================================================================
    void
    registerFmuBlock(BlockRegistry& reg)
    {
        BlockMetadata meta;
        meta.acceptsVectorInputs = true;
        // Output-port widths come from the block's "outputWidths" param (an array
        // of element counts, written by the FMU inspection step from each output
        // variable's FMI3 array size). Absent -> scalar ports (width 1).
        meta.resolveDims = [](const Block& b, const ValMap&, const std::vector<PortSig>&,
                               std::vector<PortSig>& outSigs, std::string&) {
            auto setW = [&](int p, int w) {
                if (p >= 0 && p < (int)outSigs.size()) {
                    outSigs[p].setVector(std::max(1, w));
                }
            };
            if (asJson(b.params).contains("outputWidths")) {
                const auto& ws = asJson(b.params)["outputWidths"];
                if (ws.is_array()) {
                    for (int p = 0; p < (int)ws.size(); ++p) {
                        if (ws[p].is_number()) {
                            setW(p, ws[p].get<int>());
                        }
                    }
                } else if (ws.is_number()) {
                    // A single-output-array FMU: one width value (host JSON
                    // encoders often collapse a 1-element array to a scalar).
                    setW(0, ws.get<int>());
                }
            }
            return true;
        };
        reg.registerHandler(nflow::kFmu, handleFmu, { Phase::INIT, Phase::OUTPUT, Phase::UPDATE },
            "builtin.fmu", meta);
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
