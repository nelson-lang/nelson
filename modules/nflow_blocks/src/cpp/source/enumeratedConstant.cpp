//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// enumeratedConstant: outputs a fixed enumeration value. EnumClass names the
// enumeration (documentation only) and Value is the underlying numeric value of
// the selected member. Behaves like a constant carrying an enumerated meaning;
// scalar output. C / Rust code generation.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "source_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleEnumeratedConstant(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::OUTPUT) {
        return false;
    }
    nflow::BlockDescriptor bd(b, ctx.variables);
    setOutput(ctx, b.nid, bd.paramDouble("Value", 0.0));
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCEnumeratedConstant()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = {param:Value:0.0};";
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustEnumeratedConstant()
{
    BlockCodegenTemplate t;
    t.step = "out_{id} = {param:Value:0.0};";
    return t;
}
//=============================================================================
