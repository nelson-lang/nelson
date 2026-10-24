//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SimEngineFixedStep.hpp"
#include "SimEngineCoupling.hpp"
#include "SimEngineRuntime.hpp"
#include <algorithm>
#include <chrono>
#include <thread>
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
std::optional<std::string>
runFixedStepLoop(FixedStepLoopCtx& c)
{
    using Clock = std::chrono::steady_clock;
    for (int step = 0; step <= c.samples; ++step) {
        double t = step * c.dt;
        c.ctx.t = t;
        c.ctx.stepIndex = step; // major-step index (stable across this step)
        std::fill(c.outputs.begin(), c.outputs.end(), 0.0);
        c.time.push_back(t);

        // Warm-start algebraic slots from the previous step's converged
        // values (full port-0 slice when the block latched a vector).
        for (int i : c.algOrdered) {
            BlockId nid = c.blocks[i].nid;
            const BlockState& st = c.blockState[nid];
            if (!st.outLatch.empty()) {
                const int w = c.sigLayout.sigs[c.sigLayout.portBase[nid]].width();
                const int n = std::min<int>(w, (int)st.outLatch.size());
                for (int k = 0; k < n; ++k) {
                    c.outputs[c.outputBase[nid] + k] = st.outLatch[k];
                }
            } else {
                c.outputs[c.outputBase[nid]] = st.output;
            }
        }

        for (int i : c.outputIdx) {
            auto fn = c.reg.getHandler(c.blocks[i].type);
            if (fn) {
                fn(c.ctx, c.blocks[i], Phase::OUTPUT);
            }
        }

        if (c.needsAlgebraicSolve) {
            if (!c.solveAlgebraicSystem()) {
                c.algLoopFailed = true;
                c.algLoopTime = t;
                break;
            }
        }

        for (int i : c.afterStepIdx) {
            auto fn = c.reg.getHandler(c.blocks[i].type);
            if (fn) {
                fn(c.ctx, c.blocks[i], Phase::AFTER_STEP);
            }
        }

        // Progress publication (wall-clock cadence): an incremental frame with only
        // the samples recorded since the previous publish. A run finishing within
        // one period emits nothing.
        if (c.onProgress && c.progress) {
            const double prog = (c.samples > 0) ? (double)step / c.samples : 1.0;
            publishSimProgress(*c.progress, c.onProgress, prog,
                c.time.empty() ? 0.0 : c.time.back(), c.time, c.blocks, c.blockState, c.inputIds,
                false);
        }

        if (c.isCancelled && c.isCancelled()) {
            return c.cancelledResult();
        }

        if (c.useRealTime) {
            auto deadline = c.simWallStart
                + std::chrono::duration_cast<Clock::duration>(
                    std::chrono::duration<double>((step + 1) * c.dt))
                + c.pauseOffset;

            while (true) {
                if (c.isCancelled && c.isCancelled()) {
                    return c.cancelledResult();
                }
                // Handle pause: accumulate pause time so deadline stays accurate
                if (c.isPaused && c.isPaused()) {
                    auto pauseBegin = Clock::now();
                    while (c.isPaused && c.isPaused()) {
                        if (c.isCancelled && c.isCancelled()) {
                            return c.cancelledResult();
                        }
                        c.applySimEvents();
                        std::this_thread::sleep_for(std::chrono::milliseconds(5));
                    }
                    auto pausedFor = Clock::now() - pauseBegin;
                    c.pauseOffset += pausedFor;
                    deadline += pausedFor;
                }
                c.applySimEvents();
                // Keep the time indicator moving during a long inter-sample wait:
                // progress-only frames at the wall-clock simulated time, clamped
                // to the step interval.
                if (c.onProgress && c.progress) {
                    const double tEnd = c.samples * c.dt;
                    double wallSimTime = std::chrono::duration<double>(
                        Clock::now() - c.simWallStart - c.pauseOffset)
                                             .count();
                    wallSimTime = std::min(wallSimTime, (step + 1) * c.dt);
                    wallSimTime = std::max(wallSimTime, t);
                    const double prog = (tEnd > 0.0) ? wallSimTime / tEnd : 1.0;
                    publishSimProgress(*c.progress, c.onProgress, prog, wallSimTime, c.time,
                        c.blocks, c.blockState, c.inputIds, false);
                }
                auto now = Clock::now();
                if (now >= deadline) {
                    break;
                }
                // Sleep in short increments to stay responsive
                auto remaining = deadline - now;
                auto sleepFor = std::min(remaining,
                    std::chrono::duration_cast<Clock::duration>(std::chrono::milliseconds(5)));
                std::this_thread::sleep_for(sleepFor);
            }
        } else {
            if (c.isPaused) {
                while (c.isPaused && c.isPaused()) {
                    if (c.isCancelled && c.isCancelled()) {
                        return c.cancelledResult();
                    }
                    c.applySimEvents();
                    std::this_thread::sleep_for(std::chrono::milliseconds(50));
                }
            }
            c.applySimEvents();
        }

        // Skip on the final sample: outputs at t = tEnd are already recorded, and
        // stateful external blocks (FMUs) must not advance past the stop time.
        if (step < c.samples) {
            // Blocks with discrete-event handling (an FMI Model Exchange component)
            // process their events at the sample point, before the state update.
            for (int i : c.eventIdx) {
                auto fn = c.reg.getHandler(c.blocks[i].type);
                if (fn) {
                    fn(c.ctx, c.blocks[i], Phase::EVENT);
                }
            }
            // Opt-in iterative co-simulation coupling (SimEngineCoupling.cpp). When off,
            // or without a rollback-capable FMU, it returns false and the update phase
            // is a single Jacobi pass.
            bool didIterate = c.iterativeCoupling
                && runIterativeCoupledUpdate(
                    c.ctx, c.updateIdx, c.outputIdx, c.reg, c.needsAlgebraicSolve,
                    [&c]() { c.solveAlgebraicSystem(); }, c.couplingMaxIter, c.couplingTol);
            if (!didIterate) {
                for (int i : c.updateIdx) {
                    // Multi-rate: a slower block updates only on its sample hit; OUTPUT
                    // re-emits the unchanged state between hits (zero-order hold).
                    // rate 1 = every sample.
                    const int rate = (i < (int)c.updateRate.size()) ? c.updateRate[i] : 1;
                    if (rate > 1 && (step % rate) != 0) {
                        continue;
                    }
                    auto fn = c.reg.getHandler(c.blocks[i].type);
                    if (fn) {
                        fn(c.ctx, c.blocks[i], Phase::UPDATE);
                    }
                }
            }
        }
        if (c.ctx.stopRequested) {
            break; // an FMU requested a clean early stop; keep results so far
        }
    }
    return std::nullopt;
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
