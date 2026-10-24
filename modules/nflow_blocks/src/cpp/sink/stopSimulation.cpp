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
#include "NFlowCodegenLang.hpp"
#include <cmath>
#include "sink_blocks.hpp"
//=============================================================================
// stopSimulation: a sink that ends the run at the end of the step in which any
// element of its input is nonzero (via SimCtx::stopRequested, which both
// engine loops honor). Generated code mirrors it with a SHARED
// nflow_stop_requested state flag the block raises at its topological
// position (equivalent to AFTER_STEP: sinks emit after their producers and
// the flag is only consumed between steps); the generated main() breaks its
// loop after writing the row of the stopping step, and library consumers read
// the field between calls.
//=============================================================================
bool
Nelson::NFlow::handleStopSimulation(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::AFTER_STEP) {
        return false;
    }
    if (!hasInput(ctx, b.nid, 0)) {
        return false;
    }
    SigView u = getInputSig(ctx, b.nid, 0);
    for (int i = 0; i < u.width; ++i) {
        if (sigAt(u, i) != 0.0) {
            ctx.stopRequested = true;
            break;
        }
    }
    return false;
}
//=============================================================================
// The stop flag is one SHARED state field however many stopSimulation blocks
// (or expanded vector elements) the model carries: `once` guards the
// declaration, every copy OR-raises it.
static Nelson::NFlow::BlockCodegenTemplate
makeStopSimulationCodegen(Nelson::NFlow::CodegenLang L)
{
    using namespace Nelson::NFlow;
    BlockCodegenTemplate t;
    t.emitState = [L](const BlockCodegenStateArgs& a) {
        if (!a.once("nflow_stop_requested")) {
            return;
        }
        if (L.rust) {
            a.declState("    pub nflow_stop_requested: f64,");
            a.addInit("    s.nflow_stop_requested = 0.0;");
        } else {
            a.declState("double nflow_stop_requested;");
            a.addInit("  s->nflow_stop_requested = 0.0;");
        }
    };
    t.emitStep = [L](const BlockCodegenArgs& a) {
        a.line("out_" + a.id + " = " + a.in[0] + ";");
        if (L.rust) {
            a.line("if " + a.in[0] + " != 0.0 { " + L.sref("nflow_stop_requested") + " = 1.0; }");
        } else {
            a.line("if (" + a.in[0] + " != 0.0) " + L.sref("nflow_stop_requested") + " = 1.0;");
        }
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCStopSimulation()
{
    return makeStopSimulationCodegen({ false });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustStopSimulation()
{
    return makeStopSimulationCodegen({ true });
}
//=============================================================================
