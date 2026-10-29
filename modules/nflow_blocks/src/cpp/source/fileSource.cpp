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
#include <vector>
#include <string>
#include "source_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleFileSource(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    if (phase == Phase::INIT) {
        if (asJson(b.params).contains(nflow::kTimes)
            && asJson(b.params)[nflow::kTimes].is_array()) {
            for (const auto& v : asJson(b.params)[nflow::kTimes]) {
                st.fsrcTimes.push_back(v.is_number() ? v.get<double>() : 0.0);
            }
        }
        if (asJson(b.params).contains(nflow::kValues)
            && asJson(b.params)[nflow::kValues].is_array()) {
            for (const auto& v : asJson(b.params)[nflow::kValues]) {
                st.fsrcValues.push_back(v.is_number() ? v.get<double>() : 0.0);
            }
        }
        st.fsrcIdx = 0;
        return false;
    }
    if (phase != Phase::OUTPUT) {
        return false;
    }
    if (st.fsrcTimes.empty()) {
        setOutput(ctx, b.nid, 0.0);
        return false;
    }
    // Stateless, rewind-safe lookup: largest index with fsrcTimes[idx] <= t.
    // OUTPUT is a pure function of ctx.t, so it can re-run at any stage time
    // under a variable-step solver.
    int idx = static_cast<int>(std::upper_bound(st.fsrcTimes.begin(), st.fsrcTimes.end(), ctx.t)
                  - st.fsrcTimes.begin())
        - 1;
    if (idx < 0) {
        idx = 0;
    }
    setOutput(ctx, b.nid, idx < (int)st.fsrcValues.size() ? st.fsrcValues[idx] : 0.0);
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCFileSource()
{
    BlockCodegenTemplate t;
    // The (times, values) table is embedded in the block params, so the source
    // lowers to a static table plus the same rewind-safe piecewise-constant
    // lookup the simulator uses (largest index with times[i] <= t, clamped to 0).
    t.emitStep = [](const BlockCodegenArgs& a) {
        std::vector<double> times, values;
        if (asJson(a.params) && asJson(a.params)->contains(nflow::kTimes)
            && (*asJson(a.params))[nflow::kTimes].is_array()) {
            for (const auto& v : (*asJson(a.params))[nflow::kTimes]) {
                times.push_back(v.is_number() ? v.get<double>() : 0.0);
            }
        }
        if (asJson(a.params) && asJson(a.params)->contains(nflow::kValues)
            && (*asJson(a.params))[nflow::kValues].is_array()) {
            for (const auto& v : (*asJson(a.params))[nflow::kValues]) {
                values.push_back(v.is_number() ? v.get<double>() : 0.0);
            }
        }
        if (times.empty() || values.empty()) {
            a.line("out_" + a.id + " = 0.0;");
            return;
        }
        std::string ts = "static const double __fsrc_t_" + a.id + "[] = {";
        std::string vs = "static const double __fsrc_v_" + a.id + "[] = {";
        for (size_t i = 0; i < times.size(); ++i) {
            ts += (i ? ", " : "") + a.fmt(times[i]);
        }
        for (size_t i = 0; i < values.size(); ++i) {
            vs += (i ? ", " : "") + a.fmt(values[i]);
        }
        ts += "};";
        vs += "};";
        a.line(ts);
        a.line(vs);
        const std::string n = std::to_string(times.size());
        const std::string vn = std::to_string(values.size());
        a.line("{ int __fi = " + n + " - 1; while (__fi > 0 && __fsrc_t_" + a.id
            + "[__fi] > t) "
              "__fi--;");
        a.line("  out_" + a.id + " = __fsrc_v_" + a.id + "[__fi < " + vn + " ? __fi : " + vn
            + " - 1]; }");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustFileSource()
{
    BlockCodegenTemplate t;
    t.emitStep = [](const BlockCodegenArgs& a) {
        std::vector<double> times, values;
        if (asJson(a.params) && asJson(a.params)->contains(nflow::kTimes)
            && (*asJson(a.params))[nflow::kTimes].is_array()) {
            for (const auto& v : (*asJson(a.params))[nflow::kTimes]) {
                times.push_back(v.is_number() ? v.get<double>() : 0.0);
            }
        }
        if (asJson(a.params) && asJson(a.params)->contains(nflow::kValues)
            && (*asJson(a.params))[nflow::kValues].is_array()) {
            for (const auto& v : (*asJson(a.params))[nflow::kValues]) {
                values.push_back(v.is_number() ? v.get<double>() : 0.0);
            }
        }
        if (times.empty() || values.empty()) {
            a.line("out_" + a.id + " = 0.0_f64;");
            return;
        }
        std::string ts = "let __fsrc_t_" + a.id + " = [";
        std::string vs = "let __fsrc_v_" + a.id + " = [";
        for (size_t i = 0; i < times.size(); ++i) {
            ts += (i ? ", " : "") + a.fmt(times[i]);
        }
        for (size_t i = 0; i < values.size(); ++i) {
            vs += (i ? ", " : "") + a.fmt(values[i]);
        }
        ts += "];";
        vs += "];";
        a.line(ts);
        a.line(vs);
        a.line("let mut __fi: usize = " + std::to_string(times.size() - 1) + ";");
        a.line("while __fi > 0 && __fsrc_t_" + a.id + "[__fi] > t { __fi -= 1; }");
        a.line("let __fj: usize = if __fi < " + std::to_string(values.size()) + " { __fi } else { "
            + std::to_string(values.size() - 1) + " };");
        a.line("out_" + a.id + " = __fsrc_v_" + a.id + "[__fj];");
    };
    return t;
}
//=============================================================================
