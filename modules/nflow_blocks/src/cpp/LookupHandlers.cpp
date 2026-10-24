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
#include "lookup/lookup_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    void
    registerLookupHandlers(BlockRegistry& reg)
    {
        BlockMetadata ft;
        ft.directFeedthrough = true;
        ft.acceptsVectorInputs = true;
        ft.requiresDoubleInputs = true;

        reg.registerHandler("lookup1D", handleLookup1D, { Phase::ALGEBRAIC }, "builtin.lookup", ft);
        reg.setDimsMetadata("lookup1D", true, resolveLookup1DDims);
        reg.registerCodegen("lookup1D", "c", getCodeGenCLookup1D());
        reg.registerCodegen("lookup1D", "rust", getCodeGenRustLookup1D());

        reg.registerHandler("lookup2D", handleLookup2D, { Phase::ALGEBRAIC }, "builtin.lookup", ft);
        reg.setDimsMetadata("lookup2D", true, resolveLookup2DDims);
        reg.registerCodegen("lookup2D", "c", getCodeGenCLookup2D());
        reg.registerCodegen("lookup2D", "rust", getCodeGenRustLookup2D());

        reg.registerHandler("lookupND", handleLookupND, { Phase::ALGEBRAIC }, "builtin.lookup", ft);
        reg.setDimsMetadata("lookupND", true, resolveLookupNDDims);
        reg.registerCodegen("lookupND", "c", getCodeGenCLookupND());
        reg.registerCodegen("lookupND", "rust", getCodeGenRustLookupND());

        reg.registerHandler(
            "directLookup", handleDirectLookup, { Phase::ALGEBRAIC }, "builtin.lookup", ft);
        reg.setDimsMetadata("directLookup", true, resolveDirectLookupDims);
        reg.registerCodegen("directLookup", "c", getCodeGenCDirectLookup());
        reg.registerCodegen("directLookup", "rust", getCodeGenRustDirectLookup());

        // Prelookup family + dynamic lookup (native only, no code generation).
        reg.registerHandler(
            "prelookup", handlePrelookup, { Phase::ALGEBRAIC }, "builtin.lookup", ft);
        reg.setDimsMetadata("prelookup", true, resolvePrelookupDims);
        reg.registerHandler("interpolationPrelookup", handleInterpolationPrelookup,
            { Phase::ALGEBRAIC }, "builtin.lookup", ft);
        reg.setDimsMetadata("interpolationPrelookup", true, resolveInterpolationPrelookupDims);
        reg.registerHandler(
            "lookupDynamic", handleLookupDynamic, { Phase::ALGEBRAIC }, "builtin.lookup", ft);
        reg.setDimsMetadata("lookupDynamic", true, resolveLookupDynamicDims);

        // luScalar is code-generation-only (produced by the vector-expansion
        // pass); it has no interpreter handler, only C / Rust templates.
        reg.registerCodegen("luScalar", "c", getCodeGenCLuScalar());
        reg.registerCodegen("luScalar", "rust", getCodeGenRustLuScalar());
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
