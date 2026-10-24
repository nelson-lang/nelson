//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// bitClear: clears the bit at position BitIndex in the (integer-valued) input
// to 0 (Bit Clear). BitIndex is 0-based; values are reinterpreted as NumBits-
// wide unsigned integers (default 32). Pure algebraic feedthrough, element-wise
// over the input width. C / Rust code generation.
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
    bitClearFullMask(const nflow::BlockDescriptor& bd)
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
    static uint64_t
    bitClearBitOf(const nflow::BlockDescriptor& bd)
    {
        long long idx = std::llround(bd.paramDouble("BitIndex", 0.0));
        if (idx < 0 || idx > 63) {
            return 0ULL;
        }
        return 1ULL << static_cast<unsigned>(idx);
    }
    //=============================================================================
    bool
    handleBitClear(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        if (!hasInput(ctx, b.nid, 0)) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const uint64_t fm = bitClearFullMask(bd);
        const uint64_t bit = bitClearBitOf(bd) & fm;
        SigView u = getInputSig(ctx, b.nid, 0);
        return emitElementwise(ctx, b.nid, [&](int i) -> double {
            const uint64_t x = static_cast<uint64_t>(std::llround(sigAt(u, i))) & fm;
            return static_cast<double>((x & (~bit)) & fm);
        });
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCBitClear()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const uint64_t fm = bitClearFullMask(bd);
            const uint64_t bit = bitClearBitOf(bd) & fm;
            const std::string xu = "(((unsigned long long)llround(" + a.in[0] + ")) & "
                + std::to_string(fm) + "ULL)";
            a.line("out_" + a.id + " = (double)((" + xu + " & (~" + std::to_string(bit) + "ULL)) & "
                + std::to_string(fm) + "ULL);");
        };
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustBitClear()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const uint64_t fm = bitClearFullMask(bd);
            const uint64_t bit = bitClearBitOf(bd) & fm;
            const std::string xu = "(((libm::round(" + a.in[0] + ") as i64) as u64) & "
                + std::to_string(fm) + "u64)";
            a.line("out_" + a.id + " = ((" + xu + " & (!" + std::to_string(bit) + "u64)) & "
                + std::to_string(fm) + "u64) as f64;");
        };
        return t;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
