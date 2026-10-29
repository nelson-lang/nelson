//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Numerical linearization. Uses the same setup helpers as runSimulation
// (buildModelTopology / buildPhaseSchedule / runInitPhase / buildStateLayout)
// and the same global RHS to central-difference the state-space Jacobians
// A = df/dx, B = df/du, C = dy/dx, D = dy/du about the post-INIT operating point.
//=============================================================================
#include <cmath>
#include <functional>
#include <limits>
#include <string>
#include <vector>
#include "nlohmann/json.hpp"
#include "SimEngine.hpp"
#include "SimEngineTopology.hpp"
#include "SimEngineSchedule.hpp"
#include "SimEngineInit.hpp"
#include "SimEngineLabels.hpp"
#include "SimEngineAlgebraic.hpp"
#include "SimEngineAssets.hpp"
#include "StateLayout.hpp"
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
using json = nlohmann::json;
//=============================================================================
static std::string
linErr(const std::string& reason)
{
    json j;
    j["status"] = "error";
    j["reason"] = reason;
    return j.dump();
}
//=============================================================================
std::string
linearizeModel(const std::string& diagramJson)
{
    json diagram;
    try {
        diagram = json::parse(diagramJson);
    } catch (const std::exception& ex) {
        return linErr(std::string("JSON parse error: ") + ex.what());
    }

    // linmod / trim use Label Sinks as the model outputs, but buildModelTopology
    // still requires a scope/display/toWorkspace sink. Inject an inert scope when
    // there is none: it produces no data, so it does not affect the linearization.
    // Done on the parsed diagram so callers can pass the model JSON without a lossy
    // jsondecode round-trip.
    if (diagram.contains("blocks") && diagram["blocks"].is_array()) {
        bool hasSink = false;
        for (const auto& b : diagram["blocks"]) {
            const std::string t = b.value("type", std::string());
            if (t == "scope" || t == "xyScope" || t == "xyzScope" || t == nflow::kDisplay
                || t == "toWorkspace") {
                hasSink = true;
                break;
            }
        }
        if (!hasSink) {
            json scope;
            scope["id"] = "__linmod_scope__";
            scope["type"] = "scope";
            scope["inputs"] = 1;
            scope["outputs"] = 0;
            scope["params"] = json::object();
            diagram["blocks"].push_back(scope);
        }
    }

    ensureBuiltinsRegistered();
    BlockRegistry& reg = BlockRegistry::instance();

    ModelTopology topo;
    if (auto early = buildModelTopology(diagram, topo)) {
        return *early;
    }
    std::vector<Block>& blocks = topo.blocks;
    const int totalSlots = topo.totalSlots;

    PhaseSchedule sched;
    if (auto early = buildPhaseSchedule(blocks, reg, topo.inputIds, topo.outputBase, sched)) {
        return *early;
    }

    std::vector<BlockState> blockState(blocks.size());
    std::vector<double> outputs(static_cast<size_t>(totalSlots), 0.0);
    std::vector<double> outputsImag(topo.sigLayout.hasComplex ? totalSlots : 0, 0.0);
    std::vector<long long> outputsI64(topo.sigLayout.hasExact64 ? totalSlots : 0, 0);

    SimCtx ctx { blocks, topo.inputIds, topo.outputBase, topo.labelSinks, topo.nameToId, blockState,
        outputs, topo.variables, topo.dt, 0.0 };
    ctx.tEnd = topo.samples * topo.dt;
    ctx.sigs = &topo.sigLayout.sigs;
    ctx.portBase = &topo.sigLayout.portBase;
    ctx.inputSigs = &topo.sigLayout.inputSigs;
    if (topo.sigLayout.hasComplex) {
        ctx.outputsImag = &outputsImag;
    }
    if (topo.sigLayout.hasExact64) {
        ctx.outputsI64 = &outputsI64;
    }
    ctx.busRegistry = &topo.busRegistry;
    SimDiagnostics diagnostics;
    ctx.diag = &diagnostics;
    std::string baseDirStr;
    if (diagram.contains("baseDir") && diagram["baseDir"].is_string()) {
        baseDirStr = diagram["baseDir"].get<std::string>();
    }
    ctx.baseDir = &baseDirStr;
    const std::map<std::string, std::string> exampleRoots = packagedExampleRoots();
    ctx.exampleRoots = &exampleRoots;
    if (diagram.contains("relTol") && diagram["relTol"].is_number()) {
        ctx.relTol = diagram["relTol"].get<double>();
    }

    if (auto early = runInitPhase(ctx, blocks, blockState, reg, topo.variables, sched.initIdx,
            diagnostics, topo.inputIds)) {
        return *early;
    }

    StateLayout layout = buildStateLayout(blocks, blockState, reg, topo.variables);
    const int nx = layout.totalContinuous;

    // Model outputs y = the labelSink blocks (their input-wire value).
    std::vector<BlockId> outputNids;
    for (const Block& b : blocks) {
        if (b.type == nflow::kLabelSink) {
            outputNids.push_back(b.nid);
        }
    }
    const int ny = static_cast<int>(outputNids.size());

    // Model inputs u = the external-port labelSource blocks (isExternalPort).
    // resolveLabelSourcesOnce leaves their output slot untouched, so a value
    // injected into it (slot = outputBase[nid]) propagates to the graph. A plain
    // label source without isExternalPort is internal routing (goto/from), not a
    // model input.
    std::vector<int> inputSlots;
    for (const Block& b : blocks) {
        if (b.type == nflow::kLabelSource && asJson(b.params).contains(nflow::kIsExternalPort)
            && asJson(b.params)[nflow::kIsExternalPort].get<bool>()) {
            inputSlots.push_back(topo.outputBase[b.nid]);
        }
    }
    const int nu = static_cast<int>(inputSlots.size());

    std::vector<int> derivIdx;
    for (int i = 0; i < static_cast<int>(blocks.size()); ++i) {
        if (reg.hasPhase(blocks[i].type, Phase::DERIVATIVE)) {
            derivIdx.push_back(i);
        }
    }

    auto resolveLabelSources = [&]() -> bool {
        return resolveLabelSourcesOnce(blocks, topo.labelSinks, topo.nameToId, topo.inputIds,
            topo.outputBase, topo.sigLayout, outputs);
    };
    std::vector<int> algFailBlocks;
    // Linearization is an instantaneous single-rate snapshot: an empty rate table
    // makes every block rate 1 (no multi-rate sample-and-hold), which is correct
    // for a Jacobian at a fixed operating point.
    const std::vector<int> linUpdateRate;
    auto solveAlg = [&]() -> bool {
        return solveAlgebraic(ctx, blocks, reg, sched.algOrdered, sched.algIdx, sched.algComponents,
            topo.outputBase, topo.sigLayout, outputs, sched.hasLabelBlocks, resolveLabelSources,
            algFailBlocks, linUpdateRate);
    };

    // Post-INIT continuous state x0 from the block states, recursing into subsystems.
    std::vector<double> x0(static_cast<size_t>(nx > 0 ? nx : 1), 0.0);
    std::function<void(const std::vector<Block>&, const std::vector<BlockState>&)> gatherStates
        = [&](const std::vector<Block>& bs, const std::vector<BlockState>& states) {
              for (int i = 0; i < static_cast<int>(bs.size()); ++i) {
                  const BlockState& st = states[static_cast<size_t>(bs[i].nid)];
                  if (st.xOffset >= 0) {
                      if (bs[i].type == "tf") {
                          for (int k = 0; k < st.xLength && k < (int)st.tfState.size(); ++k) {
                              x0[static_cast<size_t>(st.xOffset + k)] = st.tfState[k];
                          }
                      } else if (bs[i].type == "pid") {
                          x0[static_cast<size_t>(st.xOffset)] = st.pidIntegral;
                      } else if (!st.vec.empty()) {
                          for (int k = 0; k < st.xLength && k < (int)st.vec.size(); ++k) {
                              x0[static_cast<size_t>(st.xOffset + k)] = st.vec[k];
                          }
                      } else {
                          x0[static_cast<size_t>(st.xOffset)] = st.scalar;
                      }
                  }
                  if (st.subsystem) {
                      gatherStates(st.subsystem->blocks, st.subsystem->blockStateVec);
                  }
              }
          };
    gatherStates(blocks, blockState);

    // Optional operating-point overrides from the request: x0 (length nx) and
    // u0 (length nu). Absent -> post-INIT state and zero inputs. A length-1
    // vector may arrive as a bare JSON number, so accept scalar or array.
    auto readOpVec = [&](const char* key, int n, std::vector<double>& dst) {
        if (!diagram.contains(key)) {
            return;
        }
        const json& v = diagram[key];
        if (v.is_number() && n == 1) {
            dst[0] = v.get<double>();
        } else if (v.is_array() && static_cast<int>(v.size()) == n) {
            for (int k = 0; k < n; ++k) {
                if (v[static_cast<size_t>(k)].is_number()) {
                    dst[static_cast<size_t>(k)] = v[static_cast<size_t>(k)].get<double>();
                }
            }
        }
    };
    readOpVec("x0", nx, x0);
    std::vector<double> u0(static_cast<size_t>(nu), 0.0);
    readOpVec("u0", nu, u0);

    // Evaluate f(x,u) and y(x,u): scatter x, inject u into the (otherwise 0)
    // label-source slots, run OUTPUT, resolve, algebraic, DERIVATIVE, then read the
    // derivative vector and each output's input wire.
    std::vector<double> xdotBuf(static_cast<size_t>(nx > 0 ? nx : 1), 0.0);
    auto evalFY = [&](std::vector<double>& xstate, const std::vector<double>& u,
                      std::vector<double>& fOut, std::vector<double>& yOut) -> bool {
        ctx.x = xstate.data();
        ctx.xdot = xdotBuf.data();
        ctx.nx = nx;
        std::fill(outputs.begin(), outputs.end(), 0.0);
        for (int j = 0; j < nu; ++j) {
            outputs[static_cast<size_t>(inputSlots[static_cast<size_t>(j)])]
                = u[static_cast<size_t>(j)];
        }
        for (int i : sched.outputIdx) {
            auto fn = reg.getHandler(blocks[i].type);
            if (fn) {
                fn(ctx, blocks[i], Phase::OUTPUT);
            }
        }
        resolveLabelSources();
        if (!solveAlg()) {
            return false;
        }
        std::fill(xdotBuf.begin(), xdotBuf.end(), 0.0);
        for (int i : derivIdx) {
            auto fn = reg.getHandler(blocks[i].type);
            if (fn) {
                fn(ctx, blocks[i], Phase::DERIVATIVE);
            }
        }
        for (int k = 0; k < nx; ++k) {
            fOut[static_cast<size_t>(k)] = xdotBuf[static_cast<size_t>(k)];
        }
        for (int k = 0; k < ny; ++k) {
            yOut[static_cast<size_t>(k)]
                = getInput(ctx, outputNids[static_cast<size_t>(k)], 0, 0.0);
        }
        return true;
    };

    std::vector<double> A(static_cast<size_t>(nx) * static_cast<size_t>(nx), 0.0);
    std::vector<double> B(static_cast<size_t>(nx) * static_cast<size_t>(nu), 0.0);
    std::vector<double> C(static_cast<size_t>(ny) * static_cast<size_t>(nx), 0.0);
    std::vector<double> D(static_cast<size_t>(ny) * static_cast<size_t>(nu), 0.0);
    const double sqrtEps = std::sqrt(std::numeric_limits<double>::epsilon());
    std::vector<double> fp(nx, 0.0), fm(nx, 0.0), yp(ny, 0.0), ym(ny, 0.0);
    std::vector<double> xp, xm;
    // Residual f0 = f(x0, u0) and output y0 = y(x0, u0) at the operating point
    // (trim's Newton iteration and a caller's equilibrium check consume these).
    std::vector<double> f0(nx, 0.0), y0(ny, 0.0);
    {
        std::vector<double> xbase = x0;
        if (!evalFY(xbase, u0, f0, y0)) {
            return linErr("algebraic loop failed to converge during linearization");
        }
    }
    // A = df/dx, C = dy/dx (perturb the state, inputs at u0).
    for (int j = 0; j < nx; ++j) {
        const double h = sqrtEps * std::max(1.0, std::abs(x0[static_cast<size_t>(j)]));
        xp = x0;
        xm = x0;
        xp[static_cast<size_t>(j)] += h;
        xm[static_cast<size_t>(j)] -= h;
        if (!evalFY(xp, u0, fp, yp) || !evalFY(xm, u0, fm, ym)) {
            return linErr("algebraic loop failed to converge during linearization");
        }
        const double inv = 1.0 / (2.0 * h);
        for (int i = 0; i < nx; ++i) {
            A[static_cast<size_t>(i) * nx + j] = (fp[i] - fm[i]) * inv;
        }
        for (int k = 0; k < ny; ++k) {
            C[static_cast<size_t>(k) * nx + j] = (yp[k] - ym[k]) * inv;
        }
    }
    // B = df/du, D = dy/du (perturb each input, state at x0).
    std::vector<double> up, um;
    for (int j = 0; j < nu; ++j) {
        const double h = sqrtEps; // u0 = 0, so h = sqrt(eps)
        up = u0;
        um = u0;
        up[static_cast<size_t>(j)] += h;
        um[static_cast<size_t>(j)] -= h;
        std::vector<double> x0a = x0, x0b = x0;
        if (!evalFY(x0a, up, fp, yp) || !evalFY(x0b, um, fm, ym)) {
            return linErr("algebraic loop failed to converge during linearization");
        }
        const double inv = 1.0 / (2.0 * h);
        for (int i = 0; i < nx; ++i) {
            B[static_cast<size_t>(i) * nu + j] = (fp[i] - fm[i]) * inv;
        }
        for (int k = 0; k < ny; ++k) {
            D[static_cast<size_t>(k) * nu + j] = (yp[k] - ym[k]) * inv;
        }
    }

    // Per-state DAE id (1 = differential, 0 = algebraic). For a mixed block (an
    // electrical / thermal physicalIsland) it partitions the descriptor system, so a
    // linear island can be reduced to an explicit ODE state-space by eliminating its
    // algebraic states (nflow_reduce_descriptor). All 1 for a pure ODE (mechanical
    // island, tf).
    std::vector<double> daeId(static_cast<size_t>(nx), 1.0);
    fillDaeIds(blocks, blockState, reg, topo.variables, daeId);

    json result;
    result["status"] = "done";
    result["nx"] = nx;
    result["nu"] = nu;
    result["ny"] = ny;
    result["A"] = A; // row-major nx x nx
    result["B"] = B; // row-major nx x nu
    result["C"] = C; // row-major ny x nx
    result["D"] = D; // row-major ny x nu
    result["f0"] = f0; // xdot at the operating point (length nx)
    result["y0"] = y0; // outputs at the operating point (length ny)
    result["mask"] = daeId; // per-state 1 = differential, 0 = algebraic
    return result.dump();
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
