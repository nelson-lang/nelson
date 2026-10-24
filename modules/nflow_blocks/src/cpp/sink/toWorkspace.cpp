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
#include "NFlowBlockDescriptor.hpp"
#include <cmath>
#include <limits>
#include <algorithm>
#include "sink_blocks.hpp"
//=============================================================================
// toWorkspace: accumulates its input signal at major steps; the collected
// samples are written into the Nelson workspace AFTER the simulation by the
// builtin layer (see NFlowWorkspaceIO / NelsonWorkspaceBridge), according to
// the SaveFormat parameter.
//
// Decimation keeps one sample out of k (starting with the first), applied
// BEFORE the MaxDataPoints window, which keeps the LAST N decimated samples.
//=============================================================================
bool
Nelson::NFlow::handleToWorkspace(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    if (phase == Phase::INIT) {
        // Reused BlockState storage: scopeSeries = one series per signal
        // element, xSeries = sample times, scalar2 = step counter,
        // delaySamples = MaxDataPoints.
        SigView v = getInputSig(ctx, b.nid, 0);
        st.scopeSeries.assign(std::max(1, v.width), std::vector<double> {});
        // Typed lanes: latch the source port's nature once.
        st.scopeSeriesImag.clear();
        st.scopeSeriesI64.clear();
        st.scopeExactType = 0;
        if (v.isComplex) {
            st.scopeSeriesImag.assign(std::max(1, v.width), std::vector<double> {});
        }
        if (sigTypeIsExact64(v.type)) {
            st.scopeSeriesI64.assign(std::max(1, v.width), std::vector<long long> {});
            st.scopeExactType = (int)v.type;
        }
        st.xSeries.clear();
        st.scalar2 = 0.0;
        nflow::BlockDescriptor bd(b, ctx.variables);
        st.delaySamples
            = bd.paramDouble(nflow::kMaxDataPoints, std::numeric_limits<double>::infinity());
        if (!(st.delaySamples >= 1.0)) { // NaN, 0 or negative -> unlimited
            st.delaySamples = std::numeric_limits<double>::infinity();
        }
        return false;
    }
    if (phase != Phase::AFTER_STEP) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    int decimation = static_cast<int>(bd.paramDouble(nflow::kDecimation, 1.0));
    if (decimation < 1) {
        decimation = 1;
    }
    const int stepIndex = static_cast<int>(st.scalar2);
    st.scalar2 += 1.0;
    if (stepIndex % decimation != 0) {
        return false;
    }
    SigView v = getInputSig(ctx, b.nid, 0);
    const bool conn = hasInput(ctx, b.nid, 0);
    const int w = (int)st.scopeSeries.size();
    const bool bounded = std::isfinite(st.delaySamples);
    const size_t maxPoints = bounded ? (size_t)st.delaySamples : 0;
    if (bounded && maxPoints == 0) {
        return false;
    }
    if (bounded && st.xSeries.size() >= maxPoints) {
        st.xSeries.erase(st.xSeries.begin());
        for (auto& series : st.scopeSeries) {
            if (!series.empty()) {
                series.erase(series.begin());
            }
        }
        for (auto& series : st.scopeSeriesImag) {
            if (!series.empty()) {
                series.erase(series.begin());
            }
        }
        for (auto& series : st.scopeSeriesI64) {
            if (!series.empty()) {
                series.erase(series.begin());
            }
        }
    }
    st.xSeries.push_back(ctx.t);
    for (int i = 0; i < w; ++i) {
        const int src = std::min(i, std::max(0, v.width - 1));
        const double val = conn ? sigAt(v, src) : std::numeric_limits<double>::quiet_NaN();
        st.scopeSeries[i].push_back(val);
        if (i < (int)st.scopeSeriesImag.size()) {
            const double iv = (conn && v.imag && v.width > 0)
                ? ((v.width == 1) ? v.imag[0] : v.imag[src])
                : 0.0;
            st.scopeSeriesImag[i].push_back(iv);
        }
        if (i < (int)st.scopeSeriesI64.size()) {
            const long long ev
                = (conn && v.idata) ? ((v.width == 1) ? v.idata[0] : v.idata[src]) : 0;
            st.scopeSeriesI64[i].push_back(ev);
        }
    }
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCToWorkspace()
{
    // Workspace logging has no meaning in generated code: intentional no-op.
    BlockCodegenTemplate t;
    t.step = "out_{id} = {in0}; /* toWorkspace: no-op in generated code */";
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustToWorkspace()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = {in0}; // toWorkspace: no-op in generated code";
    return t;
}
//=============================================================================
