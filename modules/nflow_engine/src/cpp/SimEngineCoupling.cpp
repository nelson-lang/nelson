//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SimEngineCoupling.hpp"
#include "FmuComponent.hpp"
#include "FieldNames.hpp"
#include <algorithm>
#include <cmath>
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
// Number of output signal elements (sum of port widths) of block `nid`.
static int
outputElementCount(const SimCtx& ctx, int i)
{
    const int nid = ctx.blocks[i].nid;
    if (ctx.sigs != nullptr && ctx.portBase != nullptr) {
        int n = 0;
        for (int p = 0; p < ctx.blocks[i].outputs; ++p) {
            n += (*ctx.sigs)[(*ctx.portBase)[nid] + p].width();
        }
        return n;
    }
    return ctx.blocks[i].outputs; // all-scalar layout: one slot per port
}
//=============================================================================
bool
runIterativeCoupledUpdate(SimCtx& ctx, const std::vector<int>& updateIdx,
    const std::vector<int>& outputIdx, const BlockRegistry& reg, bool needsAlgebraicSolve,
    const std::function<void()>& solveAlgebraic, int couplingMaxIter, double couplingTol)
{
    const std::vector<Block>& blocks = ctx.blocks;
    auto runPhase = [&](int i, Phase ph) {
        auto fn = reg.getHandler(blocks[i].type);
        if (fn) {
            fn(ctx, blocks[i], ph);
        }
    };
    auto fmuComp = [&](int i) -> IFmuComponent* {
        return dynamic_cast<IFmuComponent*>(ctx.blockState[blocks[i].nid].external.get());
    };

    std::vector<int> fmuIdx, nativeUpd;
    for (int i : updateIdx) {
        IFmuComponent* c = (blocks[i].type == nflow::kFmu) ? fmuComp(i) : nullptr;
        if (c != nullptr && c->canSaveState()) {
            fmuIdx.push_back(i);
        } else {
            nativeUpd.push_back(i);
        }
    }
    if (fmuIdx.empty()) {
        return false; // nothing to iterate: caller runs the default single pass
    }

    std::vector<double>& outputs = ctx.outputs;
    auto gatherFmuOutputs = [&](std::vector<double>& y) {
        y.clear();
        for (int i : fmuIdx) {
            const int base = ctx.outputBase[blocks[i].nid];
            const int n = outputElementCount(ctx, i);
            for (int k = 0; k < n; ++k) {
                y.push_back(outputs[base + k]);
            }
        }
    };
    auto scatterFmuOutputs = [&](const std::vector<double>& y) {
        size_t o = 0;
        for (int i : fmuIdx) {
            const int base = ctx.outputBase[blocks[i].nid];
            const int n = outputElementCount(ctx, i);
            for (int k = 0; k < n; ++k) {
                outputs[base + k] = y[o++];
            }
        }
    };

    SimDiagnostic sdiag;
    for (int i : fmuIdx) {
        fmuComp(i)->saveState(sdiag); // snapshot each FMU state at t
    }
    std::vector<double> yPrev;
    gatherFmuOutputs(yPrev); // y^(0): FMU outputs already computed at t
    std::vector<double> yNew;
    for (int iter = 0; iter < couplingMaxIter; ++iter) {
        scatterFmuOutputs(yPrev);
        for (int i : outputIdx) {
            if (blocks[i].type != nflow::kFmu) {
                runPhase(i, Phase::OUTPUT);
            }
        }
        if (needsAlgebraicSolve && solveAlgebraic) {
            solveAlgebraic();
        }
        for (int i : fmuIdx) {
            fmuComp(i)->restoreState(sdiag);
        }
        for (int i : fmuIdx) {
            runPhase(i, Phase::UPDATE);
        }
        for (int i : fmuIdx) {
            runPhase(i, Phase::OUTPUT); // copy the resulting t+h outputs
        }
        gatherFmuOutputs(yNew);
        double md = 0.0;
        for (size_t k = 0; k < yNew.size(); ++k) {
            md = (std::max)(md, std::abs(yNew[k] - yPrev[k]));
        }
        yPrev.swap(yNew);
        if (md <= couplingTol) {
            break;
        }
    }
    // Advance native stateful blocks once (v1 assumes they sit outside the loop).
    for (int i : nativeUpd) {
        runPhase(i, Phase::UPDATE);
    }
    return true;
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
