//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// coulombViscousFriction: a static friction characteristic combining a viscous
// term proportional to the input (Gain) and a Coulomb term of fixed magnitude
// opposing the direction of motion (Offset). y = Gain*u + Offset*sign(u), so
// y(0) = 0 (sign(0) = 0). Scalar or vector (element-wise); C / Rust codegen.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include <cmath>
#include "nonlinear_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleCoulombViscousFriction(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    if (!hasInput(ctx, b.nid, 0)) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    const double gain = bd.paramDouble("Gain", 1.0);
    const double offset = bd.paramDouble("Offset", 1.0);
    SigView uv = getInputSig(ctx, b.nid, 0);
    return emitElementwise(ctx, b.nid, [&](int i) {
        const double u = sigAt(uv, i);
        const double s = (u > 0.0) ? 1.0 : ((u < 0.0) ? -1.0 : 0.0);
        return gain * u + offset * s;
    });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCCoulombViscousFriction()
{
    BlockCodegenTemplate t;
    t.step = "{ double u = {in0};\n"
             "  double s = (u > 0.0) ? 1.0 : ((u < 0.0) ? -1.0 : 0.0);\n"
             "  out_{id} = {param:Gain:1.0} * u + {param:Offset:1.0} * s; }";
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustCoulombViscousFriction()
{
    BlockCodegenTemplate t;
    t.step
        = "out_{id} = {param:Gain:1.0} * {in0} + {param:Offset:1.0} * "
          "(if {in0} > 0.0_f64 { 1.0_f64 } else if {in0} < 0.0_f64 { -1.0_f64 } else { 0.0_f64 });";
    return t;
}
//=============================================================================
