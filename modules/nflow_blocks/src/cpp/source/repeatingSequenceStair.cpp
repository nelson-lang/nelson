//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// repeatingSequenceStair: a periodic staircase source (Repeating Sequence
// Stair). Steps through the OutValues vector one entry per sample, holding each
// value for a step, and repeats from the start once the end is reached. No
// input; scalar output. If OutValues is empty the output is 0.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include <cmath>
#include <vector>
#include <string>
#include "source_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    bool
    handleRepeatingSequenceStair(SimCtx& ctx, const Block& b, Phase phase)
    {
        auto& st = getState(ctx, b.nid);
        if (phase == Phase::INIT) {
            st.scalar = 0.0; // current index into OutValues
            st.output = 0.0;
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const std::vector<double> vals = bd.paramList("OutValues");
        const int n = static_cast<int>(vals.size());
        if (phase == Phase::OUTPUT) {
            double out = 0.0;
            if (n > 0) {
                int idx = static_cast<int>(st.scalar + 0.5);
                if (idx < 0) {
                    idx = 0;
                }
                if (idx >= n) {
                    idx = idx % n;
                }
                out = vals[idx];
            }
            setOutput(ctx, b.nid, out);
            return false;
        }
        if (phase == Phase::UPDATE) {
            if (n > 0) {
                st.scalar = std::fmod(st.scalar + 1.0, static_cast<double>(n));
            }
            return false;
        }
        return false;
    }
    //=============================================================================
    static std::string
    rssTableLiteralC(const std::vector<double>& vals)
    {
        std::string s = "{ ";
        for (size_t i = 0; i < vals.size(); ++i) {
            s += nflow::formatNumber(vals[i]);
            if (i + 1 < vals.size()) {
                s += ", ";
            }
        }
        s += " }";
        return s;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCRepeatingSequenceStair()
    {
        BlockCodegenTemplate t;
        t.emitState
            = [](const BlockCodegenStateArgs& a) { a.addState("rss_idx_" + a.id, "0.0", ""); };
        t.emitStep = [](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const std::vector<double> vals = bd.paramList("OutValues");
            const int n = static_cast<int>(vals.size());
            if (n <= 0) {
                a.line("out_" + a.id + " = 0.0;");
                return;
            }
            const std::string ns = std::to_string(n);
            a.line("static const double rss_tbl_" + a.id + "[" + ns
                + "] = " + rssTableLiteralC(vals) + ";");
            a.line("out_" + a.id + " = rss_tbl_" + a.id + "[((int)(s->rss_idx_" + a.id
                + " + 0.5)) % " + ns + "];");
            a.line("s->rss_idx_" + a.id + " = fmod(s->rss_idx_" + a.id + " + 1.0, " + ns + ".0);");
        };
        return t;
    }
    //=============================================================================
    static std::string
    rssTableLiteralRust(const BlockCodegenArgs& a, const std::vector<double>& vals)
    {
        std::string s = "[";
        for (size_t i = 0; i < vals.size(); ++i) {
            s += a.fmt(vals[i]);
            if (i + 1 < vals.size()) {
                s += ", ";
            }
        }
        s += "]";
        return s;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustRepeatingSequenceStair()
    {
        BlockCodegenTemplate t;
        t.emitState
            = [](const BlockCodegenStateArgs& a) { a.addState("rss_idx_" + a.id, "0.0_f64", ""); };
        t.emitStep = [](const BlockCodegenArgs& a) {
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const std::vector<double> vals = bd.paramList("OutValues");
            const int n = static_cast<int>(vals.size());
            if (n <= 0) {
                a.line("out_" + a.id + " = 0.0_f64;");
                return;
            }
            const std::string ns = std::to_string(n);
            a.line("let rss_tbl_" + a.id + ": [f64; " + ns + "] = " + rssTableLiteralRust(a, vals)
                + ";");
            a.line("out_" + a.id + " = rss_tbl_" + a.id + "[(s.rss_idx_" + a.id + " as usize) % "
                + ns + "];");
            a.line(
                "s.rss_idx_" + a.id + " = (s.rss_idx_" + a.id + " + 1.0_f64) % " + ns + ".0_f64;");
        };
        return t;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
