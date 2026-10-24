//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// shiftArithmetic: arithmetic bit shift of the (integer-valued) input (Shift
// Arithmetic). ShiftDirection is "Left" (multiply by 2^ShiftNumber) or "Right"
// (sign-preserving divide by 2^ShiftNumber, i.e. arithmetic right shift).
// Values are treated as signed 64-bit integers. Pure algebraic feedthrough,
// element-wise over the input width. C / Rust code generation.
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
    // Signed shift amount, clamped to a well-defined range: positive shifts left,
    // negative shifts right.
    static int
    shiftAmount(const nflow::BlockDescriptor& bd)
    {
        long long raw = std::llround(bd.paramDouble("ShiftNumber", 1.0));
        const std::string dir = bd.paramStr("ShiftDirection", "Left");
        if (dir == "Right") {
            raw = -raw;
        }
        if (raw > 62) {
            raw = 62;
        }
        if (raw < -62) {
            raw = -62;
        }
        return static_cast<int>(raw);
    }
    //=============================================================================
    bool
    handleShiftArithmetic(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        if (!hasInput(ctx, b.nid, 0)) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const int n = shiftAmount(bd);
        SigView u = getInputSig(ctx, b.nid, 0);
        return emitElementwise(ctx, b.nid, [&](int i) -> double {
            const int64_t x = static_cast<int64_t>(std::llround(sigAt(u, i)));
            int64_t r;
            if (n >= 0) {
                r = static_cast<int64_t>(static_cast<uint64_t>(x) << static_cast<unsigned>(n));
            } else {
                r = x >> static_cast<unsigned>(-n);
            }
            return static_cast<double>(r);
        });
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCShiftArithmetic()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const int n = shiftAmount(bd);
            const std::string x = "((long long)llround(" + a.in[0] + "))";
            if (n >= 0) {
                a.line("out_" + a.id + " = (double)((long long)(((unsigned long long)" + x + ") << "
                    + std::to_string(n) + "));");
            } else {
                a.line("out_" + a.id + " = (double)(" + x + " >> " + std::to_string(-n) + ");");
            }
        };
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustShiftArithmetic()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const int n = shiftAmount(bd);
            const std::string x = "(" + a.in[0] + ".round() as i64)";
            if (n >= 0) {
                a.line("out_" + a.id + " = (((" + x + " as u64) << " + std::to_string(n)
                    + "u32) as i64) as f64;");
            } else {
                a.line("out_" + a.id + " = (" + x + " >> " + std::to_string(-n) + "u32) as f64;");
            }
        };
        return t;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
