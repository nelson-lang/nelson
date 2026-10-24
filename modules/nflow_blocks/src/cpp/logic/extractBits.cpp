//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// extractBits: extracts a contiguous range of NumBitsToExtract bits starting at
// bit StartBit (0-based, LSB) from the (integer-valued) input, right-aligned in
// the output (Extract Bits, range form): out = (x >> StartBit) & ((1 <<
// NumBitsToExtract) - 1). Input values are reinterpreted as NumBits-wide
// unsigned integers (default 32). Pure algebraic feedthrough, element-wise over
// the input width. C / Rust code generation.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "NFlowCodegenHelpers.hpp"
#include <cmath>
#include <cstdint>
#include <string>
#include "logic_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    static uint64_t
    ebFullMask(const nflow::BlockDescriptor& bd)
    {
        double nb = std::floor(bd.paramDouble("NumBits", 32.0));
        if (nb < 1.0) {
            nb = 1.0;
        }
        if (nb >= 64.0) {
            return ~0ULL;
        }
        return (1ULL << static_cast<unsigned>(nb)) - 1ULL;
    }
    //=============================================================================
    static void
    ebParams(const nflow::BlockDescriptor& bd, unsigned& start, uint64_t& extractMask)
    {
        long long s = std::llround(bd.paramDouble("StartBit", 0.0));
        if (s < 0) {
            s = 0;
        }
        if (s > 63) {
            s = 63;
        }
        long long n = std::llround(bd.paramDouble("NumBitsToExtract", 8.0));
        if (n < 1) {
            n = 1;
        }
        if (n > 64) {
            n = 64;
        }
        start = static_cast<unsigned>(s);
        extractMask = (n >= 64) ? ~0ULL : ((1ULL << static_cast<unsigned>(n)) - 1ULL);
    }
    //=============================================================================
    // Where the extracted bits land. 'keepWeight' (the default) leaves them at
    // their weight, so bits 2..5 of 214 read 5 * 2^2 = 20, the value a
    // fixed-point type would carry. 'rightAligned' returns them as a number in
    // their own right (5).
    static bool
    ebKeepsWeight(const nflow::BlockDescriptor& bd)
    {
        return bd.paramStr("OutputScaling", "keepWeight") != "rightAligned";
    }
    //=============================================================================
    bool
    handleExtractBits(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        if (!hasInput(ctx, b.nid, 0)) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const uint64_t fm = ebFullMask(bd);
        unsigned start = 0;
        uint64_t em = 0;
        ebParams(bd, start, em);
        const bool keepWeight = ebKeepsWeight(bd);
        SigView u = getInputSig(ctx, b.nid, 0);
        return emitElementwise(ctx, b.nid, [&](int i) -> double {
            const uint64_t x = static_cast<uint64_t>(std::llround(sigAt(u, i))) & fm;
            const uint64_t bits = (x >> start) & em;
            return static_cast<double>(keepWeight ? (bits << start) : bits);
        });
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCExtractBits()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const uint64_t fm = ebFullMask(bd);
            unsigned start = 0;
            uint64_t em = 0;
            ebParams(bd, start, em);
            const std::string xu = "(((unsigned long long)llround(" + a.in[0] + ")) & "
                + std::to_string(fm) + "ULL)";
            const std::string bits
                = "((" + xu + " >> " + std::to_string(start) + ") & " + std::to_string(em) + "ULL)";
            a.line("out_" + a.id + " = (double)("
                + (ebKeepsWeight(bd) ? (bits + " << " + std::to_string(start)) : bits) + ");");
        };
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustExtractBits()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const uint64_t fm = ebFullMask(bd);
            unsigned start = 0;
            uint64_t em = 0;
            ebParams(bd, start, em);
            const std::string xu = "(((libm::round(" + a.in[0] + ") as i64) as u64) & "
                + std::to_string(fm) + "u64)";
            const std::string bits = "((" + xu + " >> " + std::to_string(start) + "u32) & "
                + std::to_string(em) + "u64)";
            a.line("out_" + a.id + " = ("
                + (ebKeepsWeight(bd) ? ("(" + bits + " << " + std::to_string(start) + "u32)")
                                     : bits)
                + ") as f64;");
        };
        return t;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
