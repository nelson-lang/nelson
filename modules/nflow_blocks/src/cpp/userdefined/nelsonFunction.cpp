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
#include "NelsonWorkspaceBridge.hpp"
#include <cmath>
#include <algorithm>
#include "userdefined_blocks.hpp"
//=============================================================================
// nelsonFunction: evaluates a Nelson function at every simulation step,
// through the NelsonWorkspaceBridge (the interpreter is never called from
// this module directly).
//
// Fcn is a function name ("sin"), an anonymous function source ("@(u) 2*u")
// or an expression using u ("atan2(u(1), u(2))"). One input port u
// (scalar/vector double), one output port, direct feedthrough.
//
// The output width is fixed by the signal-layout pass: OutputDimensions -1
// inherits the INPUT width (element-wise assumption); set an explicit value
// when the function changes the width. INIT probes the function once with
// the initial input and fails with a clear diagnostic when the produced
// width disagrees with the layout.
//=============================================================================
namespace {
//=============================================================================
std::vector<double>
gatherInput(Nelson::NFlow::SimCtx& ctx, const Nelson::NFlow::Block& b)
{
    using namespace Nelson::NFlow;
    SigView v = getInputSig(ctx, b.nid, 0);
    const int w = std::max(1, v.width);
    std::vector<double> u((size_t)w, 0.0);
    if (hasInput(ctx, b.nid, 0)) {
        for (int i = 0; i < w; ++i) {
            u[i] = sigAt(v, i);
        }
    }
    return u;
}
//=============================================================================
} // namespace
//=============================================================================
bool
Nelson::NFlow::handleNelsonFunction(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::INIT && phase != Phase::ALGEBRAIC) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    const std::string fcn = bd.paramStr(nflow::kFcn, "sin");
    const PortSig* ps = portSigOf(ctx, b.nid, 0);
    const int outW = ps ? ps->width() : 1;

    if (phase == Phase::INIT) {
        if (!hasNelsonWorkspaceBridge()) {
            if (ctx.diag) {
                SimDiagnostic d;
                d.code = "nelsonfunction_no_bridge";
                d.blockId = b.id;
                d.message = "nelsonFunction block requires the Nelson interpreter "
                            "(workspace bridge not installed).";
                d.time = ctx.t;
                ctx.diag->fail(d);
            }
            return false;
        }
        // Probe once with the initial input to validate the function and the
        // output width chosen by the signal-layout pass.
        std::vector<double> u = gatherInput(ctx, b);
        NelsonCallResult probe = callNelsonFunction(fcn, u);
        if (!probe.ok) {
            if (ctx.diag) {
                SimDiagnostic d;
                d.code = "nelsonfunction_probe_failed";
                d.blockId = b.id;
                d.message = "nelsonFunction block '" + fcn + "': " + probe.errorMessage;
                d.time = ctx.t;
                ctx.diag->fail(d);
            }
            return false;
        }
        if ((int)probe.y.size() != outW) {
            if (ctx.diag) {
                SimDiagnostic d;
                d.code = "nelsonfunction_width_mismatch";
                d.blockId = b.id;
                d.message = "nelsonFunction block '" + fcn + "': the width of the result was "
                    + std::to_string(probe.y.size()) + " when a width of " + std::to_string(outW)
                    + " was expected (set OutputDimensions accordingly).";
                d.time = ctx.t;
                ctx.diag->fail(d);
            }
        }
        return false;
    }

    // ALGEBRAIC: direct feedthrough evaluation. A complex input passes both
    // lanes; a complex result fills the imaginary lane (the output port is
    // complex whenever the input is, by OR-propagation).
    std::vector<double> u = gatherInput(ctx, b);
    std::vector<double> uImag;
    {
        SigView v = getInputSig(ctx, b.nid, 0);
        if (v.isComplex && v.imag && v.width > 0) {
            uImag.resize(u.size(), 0.0);
            for (size_t i = 0; i < u.size(); ++i) {
                uImag[i] = (v.width == 1) ? v.imag[0] : v.imag[(int)i];
            }
        }
    }
    NelsonCallResult res = callNelsonFunction(fcn, u, uImag.empty() ? nullptr : &uImag);
    if (!res.ok || (int)res.y.size() != outW) {
        if (ctx.diag) {
            SimDiagnostic d;
            d.code = res.ok ? "nelsonfunction_width_mismatch" : "nelsonfunction_call_failed";
            d.blockId = b.id;
            d.message = res.ok ? ("nelsonFunction block '" + fcn + "': the width of the result was "
                                     + std::to_string(res.y.size()) + " when a width of "
                                     + std::to_string(outW) + " was expected.")
                               : ("nelsonFunction block '" + fcn + "': " + res.errorMessage);
            d.time = ctx.t;
            ctx.diag->fail(d);
        }
        return false;
    }
    if (ps && ps->isComplex) {
        return emitElementwiseComplex(ctx, b.nid, [&](int i) {
            const double im
                = (res.isComplex && (size_t)i < res.yImag.size()) ? res.yImag[(size_t)i] : 0.0;
            return std::complex<double> { res.y[(size_t)i], im };
        });
    }
    if (res.isComplex && ctx.diag) {
        SimDiagnostic d;
        d.code = "nelsonfunction_complex_result";
        d.blockId = b.id;
        d.message = "nelsonFunction block '" + fcn
            + "' returned a complex result on a real output port (feed the block a complex "
              "input so the port is declared complex).";
        d.time = ctx.t;
        ctx.diag->fail(d);
        return false;
    }
    return emitElementwise(ctx, b.nid, [&](int i) { return res.y[(size_t)i]; });
}
//=============================================================================
