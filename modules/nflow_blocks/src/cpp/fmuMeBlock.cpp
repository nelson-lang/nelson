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
    // FMU (FMI 3.0 Model Exchange) block.
    //
    // Unlike the Co-Simulation block (which owns its solver and advances by
    // doStep in UPDATE), a Model Exchange FMU exposes only the model equations.
    // Its continuous states join the diagram's global state vector, so the shared
    // solver (CVODES / fixed-step RK) integrates them with the native continuous
    // blocks, and its event indicators feed the shared zero-crossing machinery.
    //
    //   INIT          : resolve the FMU path, build + initialize the ME component,
    //                   capture the initial continuous states into BlockState::vec
    //                   (which the engine gathers into the global x0), store the
    //                   component in BlockState::external.
    //   OUTPUT        : sync the FMU to (t, x) + inputs, read its outputs to ports.
    //   DERIVATIVE    : sync + read state derivatives into the block's xdot slice.
    //   ZERO_CROSSING : sync + read event indicators into the block's g slice.
    //   EVENT         : at a located crossing, run the FMU's discrete-state update
    //                   and write the (possibly reset) state back into the x slice.
    //=============================================================================
    static std::string
    fmuMePathParam(const Block& b)
    {
        for (const char* key : { "path", "fmu", "file" }) {
            if (b.params.contains(key) && b.params[key].is_string()) {
                std::string p = b.params[key].get<std::string>();
                if (!p.empty()) {
                    return p;
                }
            }
        }
        return std::string {};
    }
    //=============================================================================
    static void
    failFmuMe(SimCtx& ctx, const Block& b, SimDiagnostic& diag)
    {
        diag.blockId = b.id;
        diag.time = ctx.t;
        if (ctx.diag) {
            ctx.diag->fail(diag);
        }
    }
    //=============================================================================
    static bool
    pushInputs(SimCtx& ctx, const Block& b, IFmuMeComponent* comp, SimDiagnostic& diag)
    {
        const int nElem = comp->totalInputElements();
        if (nElem <= 0) {
            return true;
        }
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
        return comp->setInputs(inVals.data(), nElem, diag);
    }
    //=============================================================================
    // Copy the FMU's outputs to the block's output ports (array ports occupy
    // outputPortWidth() slots each).
    static bool
    pullOutputs(SimCtx& ctx, const Block& b, IFmuMeComponent* comp, SimDiagnostic& diag)
    {
        const int nElem = comp->totalOutputElements();
        std::vector<double> out(nElem > 0 ? nElem : 1, 0.0);
        if (!comp->getOutputs(out.data(), nElem, diag)) {
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
        return true;
    }
    //=============================================================================
    bool
    handleFmuMe(SimCtx& ctx, const Block& b, Phase phase)
    {
        BlockState& st = getState(ctx, b.nid);

        if (phase == Phase::INIT) {
            std::string path = fmuMePathParam(b);
            if (path.empty()) {
                if (ctx.diag) {
                    SimDiagnostic d;
                    d.code = "fmu_no_path";
                    d.blockId = b.id;
                    d.message = "FMU (Model Exchange) block has no 'path' parameter.";
                    d.time = ctx.t;
                    ctx.diag->fail(d);
                }
                return false;
            }
            const std::filesystem::path fmuPath = resolveAssetPath(ctx, path);

            SimDiagnostic diag;
            std::unique_ptr<IFmuMeComponent> comp = makeFmuMeComponent(fmuPath.u8string(), diag);
            if (!comp) {
                diag.blockId = b.id;
                diag.time = ctx.t;
                if (ctx.diag) {
                    ctx.diag->fail(diag);
                }
                return false;
            }
            std::vector<std::string> paramNames;
            std::vector<double> paramValues;
            if (b.params.contains("parameterValues") && b.params["parameterValues"].is_object()) {
                for (const auto& item : b.params["parameterValues"].items()) {
                    if (item.value().is_number()) {
                        paramNames.push_back(item.key());
                        paramValues.push_back(item.value().get<double>());
                    }
                }
            }
            comp->setSolverTolerance(ctx.relTol); // diagram tolerance -> FMU init
            if (!comp->initialize(0.0, ctx.tEnd, paramNames, paramValues, diag)) {
                diag.blockId = b.id;
                diag.time = ctx.t;
                if (ctx.diag) {
                    ctx.diag->fail(diag);
                }
                return false;
            }
            // Seed the block's state vector with the FMU's initial continuous states
            // so the engine's initial-state gather (the !st.vec.empty() branch) picks
            // them up into the global x0.
            const int nx = comp->numContinuousStates();
            if (nx > 0) {
                st.vec.assign(static_cast<size_t>(nx), 0.0);
                comp->getInitialStates(st.vec.data(), nx);
            }
            st.external = std::move(comp);
            return false;
        }

        IFmuMeComponent* comp = dynamic_cast<IFmuMeComponent*>(st.external.get());
        if (comp == nullptr) {
            // Not loaded (INIT failed): keep the graph well-defined.
            if (phase == Phase::OUTPUT) {
                for (int p = 0; p < b.outputs; ++p) {
                    setOutput(ctx, b.nid, 0.0, p);
                }
            }
            return false;
        }

        const int nx = comp->numContinuousStates();

        if (phase == Phase::OUTPUT) {
            // Sync the FMU to the solver's current time + continuous state (from the
            // global vector under a variable-step solver, else the latched state),
            // apply inputs, and read the outputs.
            double* xs = blockX(ctx, b.nid);
            SimDiagnostic diag;
            if (xs) {
                if (!comp->setStateTime(ctx.t, xs, nx, diag)) {
                    failFmuMe(ctx, b, diag);
                    return false;
                }
            } else if (!st.vec.empty()) {
                if (!comp->setStateTime(ctx.t, st.vec.data(), (int)st.vec.size(), diag)) {
                    failFmuMe(ctx, b, diag);
                    return false;
                }
            }
            if (!pushInputs(ctx, b, comp, diag)) {
                failFmuMe(ctx, b, diag);
                return false;
            }
            if (!pullOutputs(ctx, b, comp, diag)) {
                failFmuMe(ctx, b, diag);
                return false;
            }
            return false;
        }

        if (phase == Phase::DERIVATIVE) {
            double* xs = blockX(ctx, b.nid);
            double* xdot = blockXdot(ctx, b.nid);
            if (xs && xdot) {
                SimDiagnostic diag;
                if (!comp->setStateTime(ctx.t, xs, nx, diag) || !pushInputs(ctx, b, comp, diag)
                    || !comp->getDerivatives(xdot, nx, diag)) {
                    failFmuMe(ctx, b, diag);
                }
            }
            return false;
        }

        if (phase == Phase::ZERO_CROSSING) {
            double* xs = blockX(ctx, b.nid);
            double* g = blockG(ctx, b.nid);
            if (xs && g) {
                SimDiagnostic diag;
                if (!comp->setStateTime(ctx.t, xs, nx, diag) || !pushInputs(ctx, b, comp, diag)
                    || !comp->getEventIndicators(g, comp->numEventIndicators(), diag)) {
                    failFmuMe(ctx, b, diag);
                }
            }
            return false;
        }

        if (phase == Phase::EVENT) {
            double* xs = blockX(ctx, b.nid);
            if (xs && nx > 0) {
                SimDiagnostic diag;
                bool changed = false;
                if (!comp->setStateTime(ctx.t, xs, nx, diag) || !pushInputs(ctx, b, comp, diag)
                    || !comp->handleEvent(xs, xs, nx, changed, diag)) {
                    failFmuMe(ctx, b, diag);
                }
            }
            return false;
        }

        return false;
    }
    //=============================================================================
    void
    registerFmuMeBlock(BlockRegistry& reg)
    {
        BlockMetadata meta;
        meta.acceptsVectorInputs = true;
        meta.requiresDoubleInputs = false;
        // Continuous states and event indicators are written to the block params by
        // the FMU inspection step (numContinuousStates / numEventIndicators). They
        // are needed at state-layout time, which runs after INIT but reads only the
        // Block/ValMap.
        meta.continuousStateCount = [](const Block& b, const ValMap&) -> int {
            if (b.params.contains("numContinuousStates")
                && b.params["numContinuousStates"].is_number()) {
                return std::max(0, b.params["numContinuousStates"].get<int>());
            }
            return 0;
        };
        meta.zeroCrossingCount = [](const Block& b, const ValMap&) -> int {
            if (b.params.contains("numEventIndicators")
                && b.params["numEventIndicators"].is_number()) {
                return std::max(0, b.params["numEventIndicators"].get<int>());
            }
            return 0;
        };
        meta.resolveDims = [](const Block& b, const ValMap&, const std::vector<PortSig>&,
                               std::vector<PortSig>& outSigs, std::string&) {
            auto setW = [&](int p, int w) {
                if (p >= 0 && p < (int)outSigs.size()) {
                    outSigs[p].setVector(std::max(1, w));
                }
            };
            if (b.params.contains("outputWidths")) {
                const auto& ws = b.params["outputWidths"];
                if (ws.is_array()) {
                    for (int p = 0; p < (int)ws.size(); ++p) {
                        if (ws[p].is_number()) {
                            setW(p, ws[p].get<int>());
                        }
                    }
                } else if (ws.is_number()) {
                    setW(0, ws.get<int>());
                }
            }
            return true;
        };
        reg.registerHandler(nflow::kFmuMe, handleFmuMe,
            { Phase::INIT, Phase::OUTPUT, Phase::DERIVATIVE, Phase::ZERO_CROSSING, Phase::EVENT },
            "builtin.fmu", meta);
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
