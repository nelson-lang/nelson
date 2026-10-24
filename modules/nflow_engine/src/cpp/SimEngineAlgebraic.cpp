//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SimEngineAlgebraic.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <utility>
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
// Newton fallback for a cyclic algebraic component that Gauss-Seidel could not
// settle (e.g. a non-contractive / ill-conditioned loop). Solves the fixed-point
// residual F(x) = sweep(x) - x = 0 with a finite-difference Jacobian; leaves the
// component outputs at the solution and returns true on convergence.
static bool
newtonSolveAlgebraicComponent(SimCtx& ctx, std::vector<Block>& blocks, BlockRegistry& reg,
    const AlgebraicComponent& comp, const std::vector<int>& outputBase,
    const SignalLayout& sigLayout, std::vector<double>& outputs, double relTol, double absTol,
    const std::vector<int>& updateRate);
// Dense linear solve A*x = b in place (partial-pivot Gaussian elimination),
// A row-major n*n, solution written back into b. Returns false when A is
// singular (the loop has no unique solution).
static bool
solveDenseLinearPartialPivot(std::vector<double>& A, std::vector<double>& b, int n);
//=============================================================================
bool
solveAlgebraic(SimCtx& ctx, std::vector<Block>& blocks, BlockRegistry& reg,
    const std::vector<int>& algOrdered, const std::vector<int>& algIdx,
    const std::vector<AlgebraicComponent>& algComponents, const std::vector<int>& outputBase,
    const SignalLayout& sigLayout, std::vector<double>& outputs, bool hasLabelBlocks,
    const std::function<bool()>& resolveLabelSources, std::vector<int>& algFailBlocks,
    const std::vector<int>& updateRate)
{
    auto blockRate = [&](int i) -> int { return (i < (int)updateRate.size()) ? updateRate[i] : 1; };
    // A component is converged when every output moved by less than relTol*|x| + absTol
    // over a full Gauss-Seidel sweep.
    const double algRelTol = 1e-9;
    const double algAbsTol = 1e-12;
    const int algMaxIter = 500;
    std::vector<double> algScratch;
    // Evaluate one algebraic block and return the largest normalized change
    // of its port-0 output over the evaluation (0 when unchanged).
    auto evalAlgMeasure = [&](int i) -> double {
        const BlockId nid = blocks[i].nid;
        const int base = outputBase[nid];
        const int w = sigLayout.sigs[sigLayout.portBase[nid]].width();
        algScratch.assign(outputs.begin() + base, outputs.begin() + base + w);
        evalAlgebraicBlockMultirate(ctx, blocks, reg, i, blockRate(i));
        double maxRel = 0.0;
        for (int k = 0; k < w; ++k) {
            const double nv = outputs[base + k];
            if (!std::isfinite(nv)) {
                // A loop output diverged to inf/NaN: definitively not settled.
                // Return +inf (never <= 1) so the sweep can't falsely report
                // convergence via the NaN-max quirk (std::max(x, NaN) == x) and
                // the component is handed to the Newton fallback instead.
                return std::numeric_limits<double>::infinity();
            }
            const double delta = std::abs(nv - algScratch[k]);
            const double denom = algRelTol * std::abs(nv) + algAbsTol;
            const double rel = delta / denom;
            if (rel > maxRel) {
                maxRel = rel;
            }
        }
        return maxRel;
    };
    // Solve the algebraic subsystem in place. Returns true on convergence; on
    // failure fills algFailBlocks with the blocks of the offending component
    // (only that loop, not the whole algebraic set).
    algFailBlocks.clear();
    if (hasLabelBlocks) {
        // goto/from wires are virtual and outside the SCC graph: iterate
        // the whole algebraic set to a numeric fixed point.
        for (int iter = 0; iter < algMaxIter; ++iter) {
            double maxRel = 0.0;
            resolveLabelSources();
            for (int i : algOrdered) {
                maxRel = std::max(maxRel, evalAlgMeasure(i));
            }
            if (resolveLabelSources()) {
                maxRel = std::max(maxRel, 2.0); // a label moved: not settled
            }
            if (maxRel <= 1.0) {
                return true;
            }
        }
        algFailBlocks = algIdx;
        return false;
    }
    for (const auto& comp : algComponents) {
        if (!comp.cyclic) {
            for (int i : comp.blocks) {
                evalAlgebraicBlockMultirate(ctx, blocks, reg, i, blockRate(i));
            }
            continue;
        }
        bool converged = false;
        for (int iter = 0; iter < algMaxIter && !converged; ++iter) {
            double maxRel = 0.0;
            for (int i : comp.blocks) {
                maxRel = std::max(maxRel, evalAlgMeasure(i));
            }
            converged = (maxRel <= 1.0);
            if (!std::isfinite(maxRel)) {
                // Diverged to a non-finite output: stop sweeping (further
                // iterations only reproduce inf/NaN) and hand it to Newton.
                // A slow-but-finite contractive loop keeps iterating.
                break;
            }
        }
        if (!converged) {
            // Gauss-Seidel stalled: fall back to Newton on the fixed-point
            // residual. A well-posed linear loop (even a non-contractive one
            // Gauss-Seidel diverges on) is solved exactly; ill-conditioned
            // nonlinear loops get quadratic convergence the sweep cannot.
            if (newtonSolveAlgebraicComponent(ctx, blocks, reg, comp, outputBase, sigLayout,
                    outputs, algRelTol, algAbsTol, updateRate)) {
                continue;
            }
            algFailBlocks = comp.blocks;
            return false;
        }
    }
    return true;
}
//=============================================================================
void
evalAlgebraicBlockMultirate(
    SimCtx& ctx, std::vector<Block>& blocks, BlockRegistry& reg, int blockIndex, int rate)
{
    auto fn = reg.getHandler(blocks[blockIndex].type);
    if (rate <= 1) {
        if (fn) {
            fn(ctx, blocks[blockIndex], Phase::ALGEBRAIC);
        }
        return;
    }
    // A feedthrough block that re-derives its output every base step (difference, the
    // detect* family) is exempt from the sample-and-hold: its output is recomputed
    // from the current input on every step and only its UPDATE state advance is
    // rate-gated (in the fixed-step / solver loops), as in the generated code. The
    // metadata query is reached only on the rare sub-rate (rate > 1) path.
    if (reg.metadataOf(blocks[blockIndex].type).algebraicOutputEveryStep) {
        if (fn) {
            fn(ctx, blocks[blockIndex], Phase::ALGEBRAIC);
        }
        return;
    }
    const BlockId nid = blocks[blockIndex].nid;
    const int base = ctx.outputBase[nid];
    // Port-0 slice width (all-scalar layout => 1); the port-0 output occupies
    // [base, base + w) in the outputs buffer, matching evalAlgMeasure.
    int w = 1;
    if (ctx.sigs != nullptr && ctx.portBase != nullptr) {
        w = (*ctx.sigs)[(*ctx.portBase)[nid]].width();
    }
    BlockState& st = ctx.blockState[nid];
    // step 0 is always a hit (0 % rate == 0), so heldOutput is latched before any
    // between-hits step; the size guard re-latches defensively if the width ever
    // changed under the block.
    const bool hit = (ctx.stepIndex % rate) == 0;
    if (hit || !st.heldValid || static_cast<int>(st.heldOutput.size()) != w) {
        if (fn) {
            fn(ctx, blocks[blockIndex], Phase::ALGEBRAIC);
        }
        st.heldOutput.assign(ctx.outputs.begin() + base, ctx.outputs.begin() + base + w);
        st.heldValid = true;
    } else {
        // Between hits: replay the latched output; the block is a constant source
        // this step (its inputs are ignored), so it converges trivially in the SCC
        // sweep and freezes its edge of any algebraic loop.
        for (int k = 0; k < w; ++k) {
            ctx.outputs[base + k] = st.heldOutput[k];
        }
    }
}
//=============================================================================
static bool
solveDenseLinearPartialPivot(std::vector<double>& A, std::vector<double>& b, int n)
{
    for (int col = 0; col < n; ++col) {
        // Partial pivot: largest magnitude in this column at or below the diagonal.
        int piv = col;
        double best = std::abs(A[(size_t)col * n + col]);
        for (int r = col + 1; r < n; ++r) {
            const double v = std::abs(A[(size_t)r * n + col]);
            if (v > best) {
                best = v;
                piv = r;
            }
        }
        if (best < 1e-300) {
            return false; // singular: loop has no unique solution
        }
        if (piv != col) {
            for (int c = 0; c < n; ++c) {
                std::swap(A[(size_t)col * n + c], A[(size_t)piv * n + c]);
            }
            std::swap(b[col], b[piv]);
        }
        const double d = A[(size_t)col * n + col];
        for (int r = col + 1; r < n; ++r) {
            const double f = A[(size_t)r * n + col] / d;
            if (f != 0.0) {
                for (int c = col; c < n; ++c) {
                    A[(size_t)r * n + c] -= f * A[(size_t)col * n + c];
                }
                b[r] -= f * b[col];
            }
        }
    }
    for (int r = n - 1; r >= 0; --r) {
        double s = b[r];
        for (int c = r + 1; c < n; ++c) {
            s -= A[(size_t)r * n + c] * b[c];
        }
        b[r] = s / A[(size_t)r * n + r];
    }
    return true;
}
//=============================================================================
static bool
newtonSolveAlgebraicComponent(SimCtx& ctx, std::vector<Block>& blocks, BlockRegistry& reg,
    const AlgebraicComponent& comp, const std::vector<int>& outputBase,
    const SignalLayout& sigLayout, std::vector<double>& outputs, double relTol, double absTol,
    const std::vector<int>& updateRate)
{
    // Unknowns = every port-0 output scalar of the blocks in this loop (mirrors
    // the port-0 measure Gauss-Seidel uses). Bail on very large loops: Newton is
    // O(n^2) sweeps x O(n^3) solve and only a niche fallback.
    constexpr int kNewtonMaxUnknowns = 64;
    constexpr int kNewtonMaxIter = 50;
    std::vector<int> uidx;
    for (int i : comp.blocks) {
        const BlockId nid = blocks[i].nid;
        const int base = outputBase[nid];
        const int w = sigLayout.sigs[sigLayout.portBase[nid]].width();
        for (int k = 0; k < w; ++k) {
            uidx.push_back(base + k);
        }
    }
    const int n = static_cast<int>(uidx.size());
    if (n == 0 || n > kNewtonMaxUnknowns) {
        return false;
    }
    auto sweep = [&]() {
        for (int i : comp.blocks) {
            const int rate = (i < (int)updateRate.size()) ? updateRate[i] : 1;
            evalAlgebraicBlockMultirate(ctx, blocks, reg, i, rate);
        }
    };
    // Residual F(x) = sweep(x) - x. Writes x into the loop outputs, runs one
    // Gauss-Seidel sweep, reads back G(x), and returns the same normalized
    // convergence measure the fixed-point loop uses (converged when <= 1).
    std::vector<double> gx(n);
    auto residual = [&](const std::vector<double>& x, std::vector<double>& F) -> double {
        for (int j = 0; j < n; ++j) {
            outputs[uidx[j]] = x[j];
        }
        sweep();
        F.resize(n);
        double meas = 0.0;
        for (int j = 0; j < n; ++j) {
            const double g = outputs[uidx[j]];
            gx[j] = g;
            F[j] = g - x[j];
            const double denom = relTol * std::abs(g) + absTol;
            const double rel = std::abs(F[j]) / denom;
            if (rel > meas) {
                meas = rel;
            }
        }
        return meas;
    };
    // Warm-start from the current loop outputs, unless Gauss-Seidel already
    // diverged them to overflow territory (a non-contractive loop grows without
    // bound). Restarting such a component from zero keeps the sweep inside a
    // block's handler from overflowing (e.g. K*s with s ~ 1e308) and lets Newton
    // reach the well-posed solution; a linear loop converges from any finite seed.
    std::vector<double> x(n);
    bool warmOk = true;
    for (int j = 0; j < n; ++j) {
        const double v = outputs[uidx[j]];
        if (!std::isfinite(v) || std::abs(v) > 1e30) {
            warmOk = false;
            break;
        }
    }
    for (int j = 0; j < n; ++j) {
        x[j] = warmOk ? outputs[uidx[j]] : 0.0;
    }
    std::vector<double> F, Fp, xp(n), J, rhs(n);
    for (int it = 0; it < kNewtonMaxIter; ++it) {
        const double meas = residual(x, F);
        if (meas <= 1.0) {
            return true; // outputs already hold G(x), the fixed point
        }
        // Forward finite-difference Jacobian J[r*n + c] = dF_r / dx_c.
        J.assign(static_cast<size_t>(n) * n, 0.0);
        xp = x;
        for (int c = 0; c < n; ++c) {
            const double h = 1e-7 * (std::abs(x[c]) + 1.0);
            xp[c] = x[c] + h;
            residual(xp, Fp);
            for (int r = 0; r < n; ++r) {
                J[static_cast<size_t>(r) * n + c] = (Fp[r] - F[r]) / h;
            }
            xp[c] = x[c];
        }
        for (int r = 0; r < n; ++r) {
            rhs[r] = -F[r];
        }
        if (!solveDenseLinearPartialPivot(J, rhs, n)) {
            return false; // singular Jacobian: ill-posed loop
        }
        for (int j = 0; j < n; ++j) {
            x[j] += rhs[j];
        }
    }
    // Out of iterations: leave the outputs consistent with the best x and report
    // whether that last state actually satisfies the convergence criterion.
    std::vector<double> Ffin;
    return residual(x, Ffin) <= 1.0;
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
