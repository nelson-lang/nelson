//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SimEngineSolverLoop.hpp"
#include "SolverBackend.hpp"
#include "StateLayout.hpp"
#include "FieldNames.hpp"
#include "SimEngineAlgebraic.hpp"
#include "SimEngineRuntime.hpp"
#include <algorithm>
#include <cmath>
#include <chrono>
#include <memory>
#include <thread>
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
std::optional<std::string>
runSolverLoop(SolverLoopCtx& c)
{
    SimCtx& ctx = c.ctx;
    std::vector<Block>& blocks = c.blocks;
    BlockRegistry& reg = c.reg;
    std::vector<BlockState>& blockState = c.blockState;
    const std::unordered_map<std::string, double>& variables = c.variables;
    const std::vector<std::vector<int>>& inputIds = c.inputIds;
    const std::vector<int>& outputIdx = c.outputIdx;
    const std::vector<int>& algOrdered = c.algOrdered;
    const std::vector<int>& afterStepIdx = c.afterStepIdx;
    std::vector<double>& outputs = c.outputs;
    std::vector<double>& time = c.time;
    const double dt = c.dt;
    const int samples = c.samples;
    const std::string& solverName = c.solverName;
    const bool solverIsFixedRk = c.solverIsFixedRk;
    const bool hasFunctionCall = c.hasFunctionCall;
    const nlohmann::json& diagram = c.diagram;
    bool& algLoopFailed = c.algLoopFailed;
    double& algLoopTime = c.algLoopTime;
    std::function<bool()>& isCancelled = c.isCancelled;
    std::function<bool()>& solveAlgebraicSystem = c.solveAlgebraicSystem;
    std::function<bool()>& resolveLabelSources = c.resolveLabelSources;
    std::function<std::string()>& cancelledResult = c.cancelledResult;
    // Function-call generators fire from ALGEBRAIC, which also runs inside the
    // side-effect-free rhs / zero-crossing evaluations; ctx.sideEffectFree makes
    // invokeSubsystem a no-op there, so callees fire only in recordAt.
    (void)hasFunctionCall;
    // Continuous states are integrated by the backend (CVODES or the native
    // fixed-step RK); purely discrete blocks (UPDATE without DERIVATIVE)
    // advance once per output sample.
    std::vector<int> derivIdx;
    std::vector<int> zcIdx;
    std::vector<int> eventIdx;
    std::vector<int> discreteUpdateIdx;
    for (int i = 0; i < (int)blocks.size(); ++i) {
        const bool hasDeriv = reg.hasPhase(blocks[i].type, Phase::DERIVATIVE);
        if (hasDeriv) {
            derivIdx.push_back(i);
        }
        if (reg.hasPhase(blocks[i].type, Phase::ZERO_CROSSING)) {
            zcIdx.push_back(i);
        }
        if (reg.hasPhase(blocks[i].type, Phase::EVENT)) {
            eventIdx.push_back(i);
        }
        // Continuous blocks stay out of the discrete UPDATE pass: their fused
        // Euler/RK4 UPDATE would double-integrate what the solver advances.
        // Blocks holding BOTH kinds of state stay in it and skip the
        // solver-owned part themselves: a subsystem (discrete inner blocks),
        // a pid (the previous input of its derivative term), an integrator
        // with an external reset (the reset writes the IC back into x).
        const bool resettableIntegrator = blocks[i].type == "integrator"
            && asJson(blocks[i].params).contains("ExternalReset")
            && asJson(blocks[i].params)["ExternalReset"].is_string()
            && asJson(blocks[i].params)["ExternalReset"].get<std::string>() != "none";
        const bool isSubsystem
            = (blocks[i].type == "subsystem" || blocks[i].type == "pid" || resettableIntegrator);
        if ((!hasDeriv || isSubsystem) && reg.hasPhase(blocks[i].type, Phase::UPDATE)) {
            discreteUpdateIdx.push_back(i);
        }
    }
    StateLayout layout = buildStateLayout(blocks, blockState, reg, variables);
    const int nx = layout.totalContinuous;
    const int ng = layout.totalZeroCrossings;
    std::vector<double> gvec(static_cast<size_t>(ng > 0 ? ng : 1), 0.0);
    if (nx == 0) {
        return buildOutputJson("error",
            "solver '" + solverName + "' requires continuous states; none found in diagram", time,
            blocks, blockState, inputIds, "");
    }
    // Algebraic states (physicalIsland, planarMechanicalIsland, `constraint`)
    // form a DAE the explicit backends cannot integrate: a constraint residual
    // taken as a derivative silently diverges. Only the native steppers accept
    // a physicalIsland, which they advance with an embedded implicit step
    // (SimCtx::islandImplicitStep); everything else is rejected towards 'dae'.
    const bool daeBackend = (solverName == "dae" || solverName == "idas");
    if (!daeBackend) {
        std::vector<double> idProbe(static_cast<size_t>(nx), 1.0);
        fillDaeIds(blocks, blockState, reg, variables, idProbe);
        const bool nativeBackend = solverIsFixedRk || solverName == "ode45";
        std::string offenderId;
        std::string offenderType;
        bool offenderIsIsland = false;
        std::function<void(const std::vector<Block>&, const std::vector<BlockState>&)> scanAlgebraic
            = [&](const std::vector<Block>& bs, const std::vector<BlockState>& states) {
                  for (const Block& blk : bs) {
                      const BlockState& bst = states[static_cast<size_t>(blk.nid)];
                      if (bst.xOffset >= 0 && bst.xLength > 0 && offenderId.empty()) {
                          bool hasAlg = false;
                          for (int k = 0; k < bst.xLength && bst.xOffset + k < nx; ++k) {
                              if (idProbe[static_cast<size_t>(bst.xOffset + k)] < 0.5) {
                                  hasAlg = true;
                                  break;
                              }
                          }
                          const bool island = (blk.type == "physicalIsland");
                          if (hasAlg && !(island && nativeBackend)) {
                              offenderId = blk.id;
                              offenderType = blk.type;
                              offenderIsIsland = island;
                          }
                      }
                      if (bst.subsystem) {
                          scanAlgebraic(bst.subsystem->blocks, bst.subsystem->blockStateVec);
                          for (const auto& rep : bst.subsystem->feReplicas) {
                              if (rep) {
                                  scanAlgebraic(rep->blocks, rep->blockStateVec);
                              }
                          }
                      }
                  }
              };
        scanAlgebraic(blocks, blockState);
        if (!offenderId.empty()) {
            std::string reason = "solver '" + solverName
                + "' cannot integrate the algebraic (DAE) states of block '" + offenderId + "' ("
                + offenderType + "); use solver 'dae'";
            if (offenderIsIsland) {
                reason += ", or a native solver ('ode1', 'ode4', 'ode45') which advances"
                          " a physical island with an embedded implicit step";
            }
            return buildOutputJson("error", reason, time, blocks, blockState, inputIds, "");
        }
    }
    std::vector<double> xvec(static_cast<size_t>(nx), 0.0);
    std::vector<double> xdotvec(static_cast<size_t>(nx), 0.0);
    // Initial continuous state; inner blocks carry global xOffsets, so a
    // subsystem-wrapped integrator seeds x0 like a top-level one.
    std::function<void(const std::vector<Block>&, const std::vector<BlockState>&)> gatherStates
        = [&](const std::vector<Block>& bs, const std::vector<BlockState>& states) {
              for (int i = 0; i < (int)bs.size(); ++i) {
                  const BlockState& st = states[static_cast<size_t>(bs[i].nid)];
                  if (st.xOffset >= 0) {
                      if (bs[i].type == "tf") {
                          for (int k = 0; k < st.xLength && k < (int)st.tfState.size(); ++k) {
                              xvec[static_cast<size_t>(st.xOffset + k)] = st.tfState[k];
                          }
                      } else if (bs[i].type == "pid") {
                          xvec[static_cast<size_t>(st.xOffset)] = st.pidIntegral;
                      } else if (!st.vec.empty()) {
                          for (int k = 0; k < st.xLength && k < (int)st.vec.size(); ++k) {
                              xvec[static_cast<size_t>(st.xOffset + k)] = st.vec[k];
                          }
                      } else {
                          xvec[static_cast<size_t>(st.xOffset)] = st.scalar;
                      }
                  }
                  if (st.subsystem) {
                      gatherStates(st.subsystem->blocks, st.subsystem->blockStateVec);
                      // for-each replicas: independent initial conditions per channel
                      for (const auto& rep : st.subsystem->feReplicas) {
                          if (rep) {
                              gatherStates(rep->blocks, rep->blockStateVec);
                          }
                      }
                  }
              }
          };
    gatherStates(blocks, blockState);

    SolverOptions sopts;
    sopts.name = solverName;
    if (solverIsFixedRk) {
        sopts.fixedStep = dt;
    }
    if (diagram.contains("relTol") && diagram["relTol"].is_number()) {
        sopts.relTol = diagram["relTol"].get<double>();
    }
    if (diagram.contains("absTol") && diagram["absTol"].is_number()) {
        sopts.absTol = { diagram["absTol"].get<double>() };
    }
    if (diagram.contains("maxStep") && diagram["maxStep"].is_number()) {
        sopts.maxStep = diagram["maxStep"].get<double>();
    }

    SimDiagnostic bdiag;
    std::unique_ptr<ISolverBackend> backend = makeSolverBackend(sopts, bdiag);
    if (!backend) {
        // The CVODES backends are registered by the ode_solvers module when it
        // loads: name them in the hint instead of a bare "unavailable".
        std::string hint;
        if (solverName == "variableStiff" || solverName == "variableNonstiff"
            || solverName == "cvodesBdf" || solverName == "cvodesAdams") {
            hint = " The variable-step solvers are provided by the 'ode_solvers'"
                   " module (CVODES); load it, or use a fixed-step solver"
                   " ('ode1' or 'ode4').";
        }
        return buildOutputJson("error", "solver backend unavailable: " + bdiag.message + hint, time,
            blocks, blockState, inputIds, "");
    }

    OdeProblem problem;
    problem.nx = nx;
    problem.ng = ng;
    problem.x0 = xvec;
    // A triggered subsystem's control surface reports only its configured edge;
    // blocks without the metadata hook keep direction 0 (both).
    if (ng > 0) {
        problem.rootDirection.assign(static_cast<size_t>(ng), 0);
        fillRootDirections(blocks, blockState, reg, variables, problem.rootDirection);
    }
    // A control-edge surface reads a SIGNAL (its trigger port), so the roots
    // callback must refresh the algebraic chain feeding it; gated to keep the
    // cheaper roots evaluation otherwise.
    bool zcReadsSignals = false;
    {
        // nids are indexed level by level: the recursion must carry the inner state vector,
        // or an inner nid looked up in the outer states recurses into the same subsystem
        // forever.
        std::function<bool(const std::vector<Block>&, const std::vector<BlockState>&)>
            anyCtrlSurface
            = [&](const std::vector<Block>& bs, const std::vector<BlockState>& sts) -> bool {
            for (const Block& bb : bs) {
                if (bb.type == "subsystem" && asJson(bb.params).contains("controlPort")
                    && asJson(bb.params)["controlPort"].is_object()) {
                    const std::string kind
                        = asJson(bb.params)["controlPort"].value("kind", std::string());
                    if (kind == "trigger" || kind == "enableTrigger") {
                        return true;
                    }
                }
                const size_t idx = static_cast<size_t>(bb.nid);
                if (idx >= sts.size()) {
                    continue;
                }
                const BlockState& bst = sts[idx];
                if (bst.subsystem
                    && anyCtrlSurface(bst.subsystem->blocks, bst.subsystem->blockStateVec)) {
                    return true;
                }
            }
            return false;
        };
        zcReadsSignals = (ng > 0) && anyCtrlSurface(blocks, blockState);
    }
    if (ng > 0) {
        // Surfaces that depend on TIME alone (a step source) are not localised
        // on the fixed-step path: a fixed-step stepper cannot land between
        // samples, and stopping AT the instant would make the result depend on
        // whether it falls on the sample grid. Held constant, the surface never
        // changes sign. State events keep their localisation.
        std::vector<int> timeOnlyG;
        if (solverIsFixedRk) {
            std::function<void(const std::vector<Block>&, const std::vector<BlockState>&)> collect
                = [&](const std::vector<Block>& bs, const std::vector<BlockState>& st) {
                      for (size_t i = 0; i < bs.size() && i < st.size(); ++i) {
                          if (bs[i].type == "step" && st[i].gOffset >= 0) {
                              for (int k = 0; k < st[i].gLength; ++k) {
                                  timeOnlyG.push_back(st[i].gOffset + k);
                              }
                          }
                          if (st[i].subsystem) {
                              collect(st[i].subsystem->blocks, st[i].subsystem->blockStateVec);
                              for (const auto& rep : st[i].subsystem->feReplicas) {
                                  if (rep) {
                                      collect(rep->blocks, rep->blockStateVec);
                                  }
                              }
                          }
                      }
                  };
            collect(blocks, blockState);
        }
        problem.roots
            = [&, zcReadsSignals, timeOnlyG](double tt, const double* xin, double* gout) -> bool {
            ctx.t = tt;
            ctx.x = const_cast<double*>(xin);
            ctx.g = gout;
            ctx.sideEffectFree = true; // pure evaluation: no callee firing
            std::fill(outputs.begin(), outputs.end(), 0.0);
            for (int i : outputIdx) {
                auto fn = reg.getHandler(blocks[i].type);
                if (fn) {
                    fn(ctx, blocks[i], Phase::OUTPUT);
                }
            }
            resolveLabelSources();
            if (zcReadsSignals) {
                // Refresh the algebraic chain, then re-run OUTPUT so feedthrough
                // terms see the solved algebraic blocks (as in rhs / recordAt).
                solveAlgebraicSystem();
                for (int i : outputIdx) {
                    auto fn = reg.getHandler(blocks[i].type);
                    if (fn) {
                        fn(ctx, blocks[i], Phase::OUTPUT);
                    }
                }
                resolveLabelSources();
            }
            for (int i : zcIdx) {
                auto fn = reg.getHandler(blocks[i].type);
                if (fn) {
                    fn(ctx, blocks[i], Phase::ZERO_CROSSING);
                }
            }
            for (int gi : timeOnlyG) {
                gout[gi] = -1.0; // known instant: crossed, not localised
            }
            return true;
        };
    }
    problem.rhs = [&](double tt, const double* xin, double* xdotout) -> bool {
        ctx.t = tt;
        ctx.x = const_cast<double*>(xin);
        ctx.xdot = xdotout;
        ctx.nx = nx;
        ctx.sideEffectFree = true; // rhs is a pure evaluation: no callee firing
        std::fill(outputs.begin(), outputs.end(), 0.0);
        for (int i : outputIdx) {
            auto fn = reg.getHandler(blocks[i].type);
            if (fn) {
                fn(ctx, blocks[i], Phase::OUTPUT);
            }
        }
        resolveLabelSources();
        // Algebraic loops are solved to convergence inside the RHS: a single pass leaves a
        // loop nested in a continuous dynamic wrong.
        if (!solveAlgebraicSystem()) {
            algLoopFailed = true;
            algLoopTime = tt;
            return false; // abort the step; reported after the solve loop
        }
        // Re-run OUTPUT: a feedthrough term (pid kp*u, biproper tf D*u, stateSpace
        // D) reads routing blocks (gain/sum/mux) that register only ALGEBRAIC,
        // i.e. are computed after the first OUTPUT pass; without this the
        // DERIVATIVE below would see a stale zero input.
        for (int i : outputIdx) {
            auto fn = reg.getHandler(blocks[i].type);
            if (fn) {
                fn(ctx, blocks[i], Phase::OUTPUT);
            }
        }
        resolveLabelSources();
        for (int i : derivIdx) {
            auto fn = reg.getHandler(blocks[i].type);
            if (fn) {
                fn(ctx, blocks[i], Phase::DERIVATIVE);
            }
        }
        return true;
    };

    // IDAS residual: a differential state (id = 1) contributes x' - f, an
    // algebraic one (id = 0) contributes g(x), which the block wrote into its
    // xdot slot during DERIVATIVE.
    if ((solverName == "dae" || solverName == "idas") && nx > 0) {
        problem.id.assign(static_cast<size_t>(nx), 1.0);
        fillDaeIds(blocks, blockState, reg, variables, problem.id);
        problem.residual
            = [&](double tt, const double* xin, const double* xpin, double* resid) -> bool {
            std::vector<double> f(static_cast<size_t>(nx), 0.0);
            if (!problem.rhs(tt, xin, f.data())) {
                return false;
            }
            for (int i = 0; i < nx; ++i) {
                resid[i] = (problem.id[static_cast<size_t>(i)] > 0.5) ? (xpin[i] - f[i]) : f[i];
            }
            return true;
        };
        // x'0 = f(x0) for differential states, 0 for algebraic ones (IDACalcIC
        // refines them); physical islands may seed their own guess into xvec.
        problem.xp0.assign(static_cast<size_t>(nx), 0.0);
        ctx.initialStateSeed = true;
        problem.rhs(0.0, xvec.data(), problem.xp0.data());
        ctx.initialStateSeed = false;
        problem.x0 = xvec; // carry the seeded guess into the backend
        for (int i = 0; i < nx; ++i) {
            if (problem.id[static_cast<size_t>(i)] < 0.5) {
                problem.xp0[static_cast<size_t>(i)] = 0.0;
            }
        }
    }

    if (!backend->init(problem, sopts, bdiag)) {
        return buildOutputJson("error", "solver init failed: " + bdiag.message, time, blocks,
            blockState, inputIds, "");
    }
    // DAE: take the IDACalcIC-corrected IC so the t0 sample of an algebraic
    // variable is consistent, not its raw guess. No-op for explicit backends.
    backend->consistentInitialState(xvec.data());

    auto recordAt = [&](double tt) {
        ctx.t = tt;
        ctx.x = xvec.data();
        ctx.xdot = xdotvec.data();
        ctx.nx = nx;
        ctx.sideEffectFree = false; // a real pass: a functionCall generator may fire here
        std::fill(outputs.begin(), outputs.end(), 0.0);
        for (int i : outputIdx) {
            auto fn = reg.getHandler(blocks[i].type);
            if (fn) {
                fn(ctx, blocks[i], Phase::OUTPUT);
            }
        }
        resolveLabelSources();
        // Same solve as the rhs: a single ordered pass recorded an algebraic loop at its
        // first iterate while the trajectory itself was right.
        if (!solveAlgebraicSystem()) {
            algLoopFailed = true;
            algLoopTime = tt;
        }
        // Re-run OUTPUT so feedthrough terms reflect the algebraic blocks just
        // solved (as in rhs); generators fire from ALGEBRAIC, so this second
        // pass does not re-fire them.
        for (int i : outputIdx) {
            auto fn = reg.getHandler(blocks[i].type);
            if (fn) {
                fn(ctx, blocks[i], Phase::OUTPUT);
            }
        }
        resolveLabelSources();
        for (int i : afterStepIdx) {
            auto fn = reg.getHandler(blocks[i].type);
            if (fn) {
                fn(ctx, blocks[i], Phase::AFTER_STEP);
            }
        }
    };

    auto runDiscreteUpdate = [&](double tt) {
        ctx.t = tt;
        ctx.sideEffectFree = false;
        for (int i : discreteUpdateIdx) {
            // Multi-rate: a slower block updates only on its sample hit; OUTPUT
            // holds its value between hits.
            const int rate = (i < (int)c.updateRate.size()) ? c.updateRate[i] : 1;
            if (rate > 1 && (ctx.stepIndex % rate) != 0) {
                continue;
            }
            auto fn = reg.getHandler(blocks[i].type);
            if (fn) {
                fn(ctx, blocks[i], Phase::UPDATE);
            }
        }
    };

    ctx.stepIndex = 0;
    recordAt(0.0);
    runDiscreteUpdate(0.0);
    time.push_back(0.0);
    double tReached = 0.0;

    // DAE only: an implicit multistep corrector cannot solve the residual across
    // a hard source jump (step, square/pulse edges), so integration stops just
    // before each known switching instant and cold-restarts there (IDACalcIC).
    std::vector<double> discInstants;
    const bool useDiscStops = (solverName == "dae" || solverName == "idas");
    if (useDiscStops) {
        const double tEndAll = samples * dt;
        for (const Block& blk : blocks) {
            const BlockMetadata meta = reg.metadataOf(blk.type);
            if (meta.discontinuityInstants) {
                meta.discontinuityInstants(blk, variables, 0.0, tEndAll, discInstants);
            }
        }
        std::sort(discInstants.begin(), discInstants.end());
        discInstants.erase(std::unique(discInstants.begin(), discInstants.end(),
                               [](double a, double b) { return std::fabs(a - b) < 1e-12; }),
            discInstants.end());
    }
    const bool hasDisc = !discInstants.empty();
    size_t discPos = 0;
    // A discrete block feeding the continuous part jumps at step boundaries,
    // which a BDF corrector cannot integrate across with a stale history
    // (IDASolve flag -3): reinit at each output step, but only when some
    // continuous-state block actually has a wired signal input.
    bool hasCoupledContinuous = false;
    for (size_t i = 0; i < blocks.size(); ++i) {
        if (blocks[i].inputs > 0 && reg.hasPhase(blocks[i].type, Phase::DERIVATIVE)) {
            hasCoupledContinuous = true;
            break;
        }
    }
    const bool reinitDiscreteDae
        = useDiscStops && !discreteUpdateIdx.empty() && hasCoupledContinuous;
    for (int step = 1; step <= samples; ++step) {
        if (c.isPaused) {
            auto pauseBegin = std::chrono::steady_clock::now();
            while (c.isPaused && c.isPaused()) {
                if (isCancelled && isCancelled()) {
                    return cancelledResult();
                }
                c.applySimEvents();
                std::this_thread::sleep_for(std::chrono::milliseconds(5));
            }
            c.pauseOffset += std::chrono::steady_clock::now() - pauseBegin;
        }
        c.applySimEvents();
        if (c.useRealTime) {
            const auto deadline = c.simWallStart
                + std::chrono::duration_cast<std::chrono::steady_clock::duration>(
                    std::chrono::duration<double>(step * dt))
                + c.pauseOffset;
            while (std::chrono::steady_clock::now() < deadline) {
                if (isCancelled && isCancelled()) {
                    return cancelledResult();
                }
                c.applySimEvents();
                // Keep the time indicator moving during a long inter-sample wait:
                // progress-only frames at the wall-clock simulated time.
                if (c.onProgress && c.progress) {
                    const double tEndAll = samples * dt;
                    double wallSimTime = std::chrono::duration<double>(
                        std::chrono::steady_clock::now() - c.simWallStart - c.pauseOffset)
                                             .count();
                    wallSimTime = std::min(wallSimTime, step * dt);
                    wallSimTime = std::max(wallSimTime, (step - 1) * dt);
                    const double prog = (tEndAll > 0.0) ? wallSimTime / tEndAll : 1.0;
                    publishSimProgress(*c.progress, c.onProgress, prog, wallSimTime, time, blocks,
                        blockState, inputIds, false);
                }
                std::this_thread::sleep_for(std::chrono::milliseconds(5));
            }
        }
        const double tout = step * dt;
        ctx.stepIndex = step; // major-step index (stable across this step)
        // A multistep method's history is invalid across a jump: cold-restart
        // at every located discontinuity.
        int guard = 0;
        while (tReached < tout - 1e-12 && guard++ < 100000) {
            // Stop a hair before the next scheduled discontinuity so the
            // corrector's last residual still sees the pre-jump source value.
            double tGoal = tout;
            bool stopAtDisc = false;
            double discTime = 0.0;
            double discBack = 0.0;
            if (hasDisc) {
                while (discPos < discInstants.size() && discInstants[discPos] <= tReached + 1e-12) {
                    ++discPos;
                }
                if (discPos < discInstants.size() && discInstants[discPos] <= tout + 1e-12) {
                    discTime = discInstants[discPos];
                    discBack = 1.0e-9 * std::max(1.0, std::fabs(discTime));
                    tGoal = discTime - discBack;
                    if (tGoal <= tReached) {
                        tGoal = discTime; // instants too close together
                    }
                    stopAtDisc = true;
                }
                backend->setStopTime(tGoal);
            }
            AdvanceResult ar = backend->advanceTo(tGoal, xvec.data());
            if (ar.status == AdvanceStatus::Failed) {
                if (algLoopFailed) {
                    break; // an unresolved algebraic loop: reported below
                }
                return buildOutputJson("error",
                    "solver step failed at t=" + doubleToStr(ar.t) + ": " + ar.diagnostic.message,
                    time, blocks, blockState, inputIds, "");
            }
            tReached = ar.t;
            if (ar.status != AdvanceStatus::RootFound && stopAtDisc && tReached >= tGoal - 1e-12) {
                // Restart just PAST the discontinuity: a sign-based square wave
                // still holds its old value exactly at the edge.
                const double tRestart = discTime + discBack;
                SimDiagnostic rdiag;
                if (!backend->reinit(tRestart, xvec.data(), rdiag)) {
                    return buildOutputJson("error",
                        "solver reinit failed at t=" + doubleToStr(tRestart) + ": " + rdiag.message,
                        time, blocks, blockState, inputIds, "");
                }
                backend->consistentInitialState(xvec.data());
                tReached = tRestart;
                continue;
            }
            if (ar.status == AdvanceStatus::RootFound) {
                // Event-reacting blocks (FMI Model Exchange) may reset the
                // continuous state: OUTPUT+ALGEBRAIC at the crossing so their
                // inputs are current, then EVENT writes back into xvec.
                if (!eventIdx.empty()) {
                    ctx.t = ar.t;
                    ctx.x = xvec.data();
                    ctx.xdot = xdotvec.data();
                    ctx.nx = nx;
                    ctx.sideEffectFree = false; // committed pass, state may change
                    std::fill(outputs.begin(), outputs.end(), 0.0);
                    for (int i : outputIdx) {
                        auto fn = reg.getHandler(blocks[i].type);
                        if (fn) {
                            fn(ctx, blocks[i], Phase::OUTPUT);
                        }
                    }
                    resolveLabelSources();
                    if (!solveAlgebraicSystem()) {
                        algLoopFailed = true;
                        algLoopTime = ar.t;
                    }
                    for (int i : eventIdx) {
                        auto fn = reg.getHandler(blocks[i].type);
                        if (fn) {
                            fn(ctx, blocks[i], Phase::EVENT);
                        }
                    }
                }
                // Reinit a hair PAST the root: an element whose equation flips at
                // the surface (ideal switch) is still in its old mode exactly at
                // the root, and a reinit there would re-solve the old topology.
                const double rootBack = 1.0e-9 * std::max(1.0, std::fabs(ar.t));
                const double tRoot = ar.t + rootBack;
                SimDiagnostic rdiag;
                if (!backend->reinit(tRoot, xvec.data(), rdiag)) {
                    return buildOutputJson("error",
                        "solver reinit failed at t=" + doubleToStr(tRoot) + ": " + rdiag.message,
                        time, blocks, blockState, inputIds, "");
                }
                backend->consistentInitialState(xvec.data());
                tReached = tRoot;
            } else {
                break; // reached tout
            }
        }
        if (algLoopFailed) {
            break; // stop the output loop; the diagnostic is built below
        }
        recordAt(tout);
        runDiscreteUpdate(tout);
        if (reinitDiscreteDae && step < samples) {
            SimDiagnostic rdiag;
            if (backend->reinit(tout, xvec.data(), rdiag)) {
                backend->consistentInitialState(xvec.data());
            }
        }
        time.push_back(tout);
        if (c.onProgress && c.progress) {
            const double progress = samples > 0 ? static_cast<double>(step) / samples : 1.0;
            publishSimProgress(*c.progress, c.onProgress, progress, tout, time, blocks, blockState,
                inputIds, false);
        }
        if (ctx.stopRequested) {
            break; // an FMU requested a clean early stop; keep results so far
        }
        if (isCancelled && isCancelled()) {
            return cancelledResult();
        }
    }
    // Leave the global-state pointers cleared for the result phase.
    ctx.x = nullptr;
    ctx.xdot = nullptr;
    ctx.g = nullptr;
    ctx.nx = 0;

    return std::nullopt;
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
