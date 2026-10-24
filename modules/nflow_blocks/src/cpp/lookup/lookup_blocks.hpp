//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // lookup1D: 1-D interpolated lookup table (see lookup1D.cpp).
    bool
    handleLookup1D(SimCtx& ctx, const Block& b, Phase phase);
    bool
    resolveLookup1DDims(const Block& b, const ValMap& vars, const std::vector<PortSig>& inSigs,
        std::vector<PortSig>& outSigs, std::string& err);
    BlockCodegenTemplate
    getCodeGenCLookup1D();
    BlockCodegenTemplate
    getCodeGenRustLookup1D();
    //=============================================================================
    // lookup2D: 2-D interpolated lookup table (see lookup2D.cpp).
    bool
    handleLookup2D(SimCtx& ctx, const Block& b, Phase phase);
    bool
    resolveLookup2DDims(const Block& b, const ValMap& vars, const std::vector<PortSig>& inSigs,
        std::vector<PortSig>& outSigs, std::string& err);
    BlockCodegenTemplate
    getCodeGenCLookup2D();
    BlockCodegenTemplate
    getCodeGenRustLookup2D();
    //=============================================================================
    // lookupND: n-D interpolated lookup table (see lookupND.cpp).
    bool
    handleLookupND(SimCtx& ctx, const Block& b, Phase phase);
    bool
    resolveLookupNDDims(const Block& b, const ValMap& vars, const std::vector<PortSig>& inSigs,
        std::vector<PortSig>& outSigs, std::string& err);
    BlockCodegenTemplate
    getCodeGenCLookupND();
    BlockCodegenTemplate
    getCodeGenRustLookupND();
    //=============================================================================
    // directLookup: Direct Lookup Table (n-D), Element mode (see directLookup.cpp).
    bool
    handleDirectLookup(SimCtx& ctx, const Block& b, Phase phase);
    bool
    resolveDirectLookupDims(const Block& b, const ValMap& vars, const std::vector<PortSig>& inSigs,
        std::vector<PortSig>& outSigs, std::string& err);
    BlockCodegenTemplate
    getCodeGenCDirectLookup();
    BlockCodegenTemplate
    getCodeGenRustDirectLookup();
    //=============================================================================
    // prelookup: interval index + fraction for a shared breakpoint search
    // (see prelookup.cpp). Native only.
    bool
    handlePrelookup(SimCtx& ctx, const Block& b, Phase phase);
    bool
    resolvePrelookupDims(const Block& b, const ValMap& vars, const std::vector<PortSig>& inSigs,
        std::vector<PortSig>& outSigs, std::string& err);
    //=============================================================================
    // interpolationPrelookup: interpolate a static Table from a prelookup
    // [k, f] pair (see interpolationPrelookup.cpp). Native only.
    bool
    handleInterpolationPrelookup(SimCtx& ctx, const Block& b, Phase phase);
    bool
    resolveInterpolationPrelookupDims(const Block& b, const ValMap& vars,
        const std::vector<PortSig>& inSigs, std::vector<PortSig>& outSigs, std::string& err);
    //=============================================================================
    // lookupDynamic: 1-D interpolated lookup with breakpoints/table from input
    // ports (see lookupDynamic.cpp). Native only.
    bool
    handleLookupDynamic(SimCtx& ctx, const Block& b, Phase phase);
    bool
    resolveLookupDynamicDims(const Block& b, const ValMap& vars, const std::vector<PortSig>& inSigs,
        std::vector<PortSig>& outSigs, std::string& err);
    //=============================================================================
    // luScalar: code-generation-only scalar helper the vector-expansion pass
    // lowers prelookup / interpolationPrelookup / lookupDynamic into (see
    // luScalar.cpp). No interpreter handler.
    BlockCodegenTemplate
    getCodeGenCLuScalar();
    BlockCodegenTemplate
    getCodeGenRustLuScalar();
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
