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
#include <algorithm>
#include "sink_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleTerminator(SimCtx& /*ctx*/, const Block& /*b*/, Phase /*phase*/)
{
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCTerminator()
{
    // Code generation stays in the C generator (emission is entangled with
    // generator-wide analysis).
    return BlockCodegenTemplate::unimplemented();
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustTerminator()
{
    // Code generation stays in the Rust generator (emission is entangled with
    // generator-wide analysis).
    return BlockCodegenTemplate::unimplemented();
}
//=============================================================================
