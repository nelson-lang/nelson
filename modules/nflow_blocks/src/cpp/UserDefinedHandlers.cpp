//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "userdefined/userdefined_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    void
    registerUserDefinedHandlers(BlockRegistry& reg)
    {
        BlockMetadata meta;
        meta.directFeedthrough = true;
        reg.registerHandler("nelsonFunction", handleNelsonFunction,
            { Phase::INIT, Phase::ALGEBRAIC }, "builtin.userdefined", meta);
        // No codegen registration: nelsonFunction is interpreter-only and the
        // generators reject it up front (same policy as FMU blocks).

        reg.registerHandler("expression", handleExpression, { Phase::INIT, Phase::ALGEBRAIC },
            "builtin.userdefined", meta);
        // expression codegen stays native in the generators (AST-based
        // reconstruction shared with the sanitizer), not template-driven.
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
