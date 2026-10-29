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
#include "source_blocks.hpp"
//=============================================================================
// fromWorkspace: emits a signal read from a Nelson workspace variable.
//
// The variable is resolved on the interpreter thread before the simulation
// starts (nflow_simulateBuiltin / the GUI launch path): its samples are
// injected into the block params as 'times' (N entries), 'values' (N*width
// entries, per-sample layout) and 'width', like fileSource. The handler never
// touches the workspace, so OUTPUT stays a pure function of ctx.t (variable-step
// safe).
//
// Semantics:
// - time must be non-decreasing (duplicates describe discontinuities);
// - Interpolate 'on': linear; at a duplicated time the NEW value wins;
//   before the first point: linear extrapolation from the first two points;
// - Interpolate 'off': hold the value of the latest sample with time <= t;
//   zero before the first point;
// - after the final point, per OutputAfterFinalValue: 'Extrapolation'
//   (linear, requires Interpolate on - validated at INIT), 'Setting to zero',
//   'Holding final value'.
//=============================================================================
namespace {
//=============================================================================
enum AfterFinalMode
{
    AFTER_EXTRAPOLATION = 0,
    AFTER_ZERO = 1,
    AFTER_HOLD = 2
};
//=============================================================================
} // namespace
//=============================================================================
bool
Nelson::NFlow::handleFromWorkspace(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    if (phase == Phase::INIT) {
        // Reused BlockState storage: fsrcTimes = sample times, fsrcValues =
        // per-sample values (k * width + i), fsrcIdx = width,
        // scalar = interpolate flag, scalar2 = after-final mode.
        st.fsrcTimes.clear();
        st.fsrcValues.clear();
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
        st.fsrcValuesImag.clear();
        if (asJson(b.params).contains("valuesImag") && asJson(b.params)["valuesImag"].is_array()) {
            for (const auto& v : asJson(b.params)["valuesImag"]) {
                st.fsrcValuesImag.push_back(v.is_number() ? v.get<double>() : 0.0);
            }
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        st.fsrcIdx = std::max(1, (int)bd.paramDouble(nflow::kWidth, 1.0));
        const bool interpolate = bd.paramStr(nflow::kInterpolate, "on") != "off";
        st.scalar = interpolate ? 1.0 : 0.0;
        const std::string afterFinal = bd.paramStr(nflow::kOutputAfterFinalValue, "Extrapolation");
        if (afterFinal == "Setting to zero") {
            st.scalar2 = AFTER_ZERO;
        } else if (afterFinal == "Holding final value") {
            st.scalar2 = AFTER_HOLD;
        } else {
            st.scalar2 = AFTER_EXTRAPOLATION;
        }
        if (st.fsrcTimes.empty()) {
            if (ctx.diag) {
                SimDiagnostic d;
                d.code = "fromworkspace_no_data";
                d.blockId = b.id;
                d.message = "fromWorkspace block: variable '"
                    + nflow::BlockDescriptor(b, ctx.variables)
                          .paramStr(nflow::kVariableName, "simin")
                    + "' was not resolved before the simulation started.";
                d.time = ctx.t;
                ctx.diag->fail(d);
            }
            return false;
        }
        if (!interpolate && st.scalar2 == AFTER_EXTRAPOLATION) {
            if (ctx.diag) {
                SimDiagnostic d;
                d.code = "fromworkspace_extrap_without_interp";
                d.blockId = b.id;
                d.message = "fromWorkspace block: unable to extrapolate output values after "
                            "the final data value because interpolation is not enabled.";
                d.time = ctx.t;
                ctx.diag->fail(d);
            }
        }
        return false;
    }
    if (phase != Phase::OUTPUT) {
        return false;
    }
    const int n = (int)st.fsrcTimes.size();
    const int w = std::max(1, st.fsrcIdx);
    const PortSig* ps = portSigOf(ctx, b.nid, 0);
    const int outW = ps ? ps->width() : 1;
    double scalarOut = 0.0;
    double* y = (outW > 1) ? outputSlice(ctx, b.nid, 0) : &scalarOut;
    if (n == 0) {
        for (int i = 0; i < outW; ++i) {
            y[i] = 0.0;
        }
        if (outW <= 1) {
            setOutput(ctx, b.nid, scalarOut);
        }
        return false;
    }
    const bool interpolate = st.scalar != 0.0;
    const double t = ctx.t;
    const double t0 = st.fsrcTimes.front();
    const double tN = st.fsrcTimes.back();
    // Interpolation / hold / after-final rules for one storage lane (the
    // imaginary lane of a complex source follows the same rules).
    auto computeLane = [&](const std::vector<double>& vals, double* dst) {
        auto sample = [&](int k, int i) -> double {
            const size_t pos = (size_t)k * w + i;
            return pos < vals.size() ? vals[pos] : 0.0;
        };
        auto linearFrom = [&](int k0, int k1, int i) -> double {
            const double ta = st.fsrcTimes[k0];
            const double tb = st.fsrcTimes[k1];
            const double va = sample(k0, i);
            const double vb = sample(k1, i);
            if (tb == ta) {
                return vb;
            }
            return va + (vb - va) * (t - ta) / (tb - ta);
        };
        for (int i = 0; i < std::min(outW, w); ++i) {
            double v = 0.0;
            if (t < t0) {
                v = (interpolate && n >= 2) ? linearFrom(0, 1, i)
                                            : (interpolate ? sample(0, i) : 0.0);
            } else if (t > tN) {
                switch ((int)st.scalar2) {
                case AFTER_ZERO:
                    v = 0.0;
                    break;
                case AFTER_HOLD:
                    v = sample(n - 1, i);
                    break;
                default: // AFTER_EXTRAPOLATION (validated: interpolate is on)
                    v = (n >= 2) ? linearFrom(n - 2, n - 1, i) : sample(n - 1, i);
                    break;
                }
            } else {
                // last index with time <= t; at a duplicated time this picks
                // the right-most sample, so the NEW value wins at the
                // discontinuity.
                int idx = (int)(std::upper_bound(st.fsrcTimes.begin(), st.fsrcTimes.end(), t)
                              - st.fsrcTimes.begin())
                    - 1;
                if (idx < 0) {
                    idx = 0;
                }
                if (!interpolate || st.fsrcTimes[idx] == t || idx + 1 >= n) {
                    v = sample(idx, i);
                } else {
                    v = linearFrom(idx, idx + 1, i);
                }
            }
            dst[i] = v;
        }
        for (int i = w; i < outW; ++i) {
            dst[i] = 0.0;
        }
    };
    computeLane(st.fsrcValues, y);
    if (ps && ps->isComplex) {
        double scalarImag = 0.0;
        double* yim = (outW > 1) ? outputSliceImag(ctx, b.nid, 0) : &scalarImag;
        if (yim) {
            computeLane(st.fsrcValuesImag, yim);
            if (outW <= 1) {
                double* slot = outputSliceImag(ctx, b.nid, 0);
                if (slot) {
                    slot[0] = scalarImag;
                }
            }
        }
    }
    if (outW <= 1) {
        setOutput(ctx, b.nid, scalarOut);
    }
    return false;
}
//=============================================================================
namespace {
//=============================================================================
// Shared codegen helpers: the workspace samples are baked into the generated
// code as constant tables plus one shared lookup helper per language.
//=============================================================================
struct BakedData
{
    std::vector<double> times;
    std::vector<double> values;
    int interpolate = 1;
    int afterMode = AFTER_EXTRAPOLATION;
    bool scalar = false;
};
//=============================================================================
BakedData
bakedDataOf(const nlohmann::json& params)
{
    BakedData d;
    if (params.contains(nflow::kTimes) && params[nflow::kTimes].is_array()) {
        for (const auto& v : params[nflow::kTimes]) {
            d.times.push_back(v.is_number() ? v.get<double>() : 0.0);
        }
    }
    if (params.contains(nflow::kValues) && params[nflow::kValues].is_array()) {
        for (const auto& v : params[nflow::kValues]) {
            d.values.push_back(v.is_number() ? v.get<double>() : 0.0);
        }
    }
    int width = 1;
    if (params.contains(nflow::kWidth) && params[nflow::kWidth].is_number()) {
        width = std::max(1, params[nflow::kWidth].get<int>());
    }
    std::string interp = "on";
    if (params.contains(nflow::kInterpolate) && params[nflow::kInterpolate].is_string()) {
        interp = params[nflow::kInterpolate].get<std::string>();
    }
    d.interpolate = (interp != "off") ? 1 : 0;
    std::string after = "Extrapolation";
    if (params.contains(nflow::kOutputAfterFinalValue)
        && params[nflow::kOutputAfterFinalValue].is_string()) {
        after = params[nflow::kOutputAfterFinalValue].get<std::string>();
    }
    d.afterMode = (after == "Setting to zero") ? AFTER_ZERO
        : (after == "Holding final value")     ? AFTER_HOLD
                                               : AFTER_EXTRAPOLATION;
    d.scalar = (width == 1) && !d.times.empty() && d.values.size() == d.times.size();
    return d;
}
//=============================================================================
std::string
numberList(const std::vector<double>& values, const std::function<std::string(double)>& fmt)
{
    std::string out;
    for (size_t i = 0; i < values.size(); ++i) {
        if (i > 0) {
            out += ", ";
        }
        out += fmt(values[i]);
    }
    return out;
}
//=============================================================================
} // namespace
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCFromWorkspace()
{
    BlockCodegenTemplate t;
    t.emitShared = [](const BlockCodegenStateArgs& a) {
        if (a.once("fromWorkspace_helper_c")) {
            a.addHelper("static double nflow_from_workspace(const double* ts, const double* "
                        "vs, int n, double t, int interp, int after_mode)");
            a.addHelper("{");
            a.addHelper("  int idx = 0, lo = 0, hi = n - 1;");
            a.addHelper("  if (n <= 0) { return 0.0; }");
            a.addHelper("  if (t < ts[0]) {");
            a.addHelper("    if (!interp) { return 0.0; }");
            a.addHelper("    if (n < 2 || ts[1] == ts[0]) { return vs[0]; }");
            a.addHelper("    return vs[0] + (vs[1] - vs[0]) * (t - ts[0]) / (ts[1] - ts[0]);");
            a.addHelper("  }");
            a.addHelper("  if (t > ts[n - 1]) {");
            a.addHelper("    if (after_mode == 1) { return 0.0; }");
            a.addHelper("    if (after_mode == 2 || n < 2 || ts[n - 1] == ts[n - 2]) { return "
                        "vs[n - 1]; }");
            a.addHelper("    return vs[n - 2] + (vs[n - 1] - vs[n - 2]) * (t - ts[n - 2]) / "
                        "(ts[n - 1] - ts[n - 2]);");
            a.addHelper("  }");
            a.addHelper("  while (lo <= hi) {");
            a.addHelper("    int mid = (lo + hi) / 2;");
            a.addHelper(
                "    if (ts[mid] <= t) { idx = mid; lo = mid + 1; } else { hi = mid - 1; }");
            a.addHelper("  }");
            a.addHelper("  if (!interp || ts[idx] == t || idx + 1 >= n || ts[idx + 1] == "
                        "ts[idx]) { return vs[idx]; }");
            a.addHelper("  return vs[idx] + (vs[idx + 1] - vs[idx]) * (t - ts[idx]) / (ts[idx "
                        "+ 1] - ts[idx]);");
            a.addHelper("}");
        }
        BakedData d = bakedDataOf(*asJson(a.params));
        if (!d.scalar) {
            return;
        }
        const std::string n = std::to_string(d.times.size());
        a.addConst("static const double fw_t_" + a.id + "[" + n + "] = { "
            + numberList(d.times, a.fmt) + " };");
        a.addConst("static const double fw_v_" + a.id + "[" + n + "] = { "
            + numberList(d.values, a.fmt) + " };");
    };
    t.emitStep = [](const BlockCodegenArgs& a) {
        BakedData d = bakedDataOf(*asJson(a.params));
        if (!d.scalar) {
            a.line("out_" + a.id
                + " = 0.0; /* fromWorkspace: no baked data (vector or "
                  "unresolved variable) */");
            return;
        }
        a.line("out_" + a.id + " = nflow_from_workspace(fw_t_" + a.id + ", fw_v_" + a.id + ", "
            + std::to_string(d.times.size()) + ", t, " + std::to_string(d.interpolate) + ", "
            + std::to_string(d.afterMode) + ");");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustFromWorkspace()
{
    BlockCodegenTemplate t;
    t.emitShared = [](const BlockCodegenStateArgs& a) {
        if (a.once("fromWorkspace_helper_rust")) {
            a.addHelper("fn nflow_from_workspace(ts: &[f64], vs: &[f64], t: f64, interp: "
                        "bool, after_mode: i32) -> f64 {");
            a.addHelper("    let n = ts.len();");
            a.addHelper("    if n == 0 { return 0.0; }");
            a.addHelper("    if t < ts[0] {");
            a.addHelper("        if !interp { return 0.0; }");
            a.addHelper("        if n < 2 || ts[1] == ts[0] { return vs[0]; }");
            a.addHelper("        return vs[0] + (vs[1] - vs[0]) * (t - ts[0]) / (ts[1] - ts[0]);");
            a.addHelper("    }");
            a.addHelper("    if t > ts[n - 1] {");
            a.addHelper("        if after_mode == 1 { return 0.0; }");
            a.addHelper("        if after_mode == 2 || n < 2 || ts[n - 1] == ts[n - 2] { "
                        "return vs[n - 1]; }");
            a.addHelper("        return vs[n - 2] + (vs[n - 1] - vs[n - 2]) * (t - ts[n - 2]) "
                        "/ (ts[n - 1] - ts[n - 2]);");
            a.addHelper("    }");
            a.addHelper("    let mut idx = 0usize;");
            a.addHelper("    let (mut lo, mut hi) = (0isize, (n - 1) as isize);");
            a.addHelper("    while lo <= hi {");
            a.addHelper("        let mid = ((lo + hi) / 2) as usize;");
            a.addHelper("        if ts[mid] <= t { idx = mid; lo = mid as isize + 1; } else { "
                        "hi = mid as isize - 1; }");
            a.addHelper("    }");
            a.addHelper("    if !interp || ts[idx] == t || idx + 1 >= n || ts[idx + 1] == "
                        "ts[idx] { return vs[idx]; }");
            a.addHelper("    vs[idx] + (vs[idx + 1] - vs[idx]) * (t - ts[idx]) / (ts[idx + 1] "
                        "- ts[idx])");
            a.addHelper("}");
        }
        BakedData d = bakedDataOf(*asJson(a.params));
        if (!d.scalar) {
            return;
        }
        const std::string n = std::to_string(d.times.size());
        a.addConst(
            "const FW_T_" + a.id + ": [f64; " + n + "] = [ " + numberList(d.times, a.fmt) + " ];");
        a.addConst(
            "const FW_V_" + a.id + ": [f64; " + n + "] = [ " + numberList(d.values, a.fmt) + " ];");
    };
    t.emitStep = [](const BlockCodegenArgs& a) {
        BakedData d = bakedDataOf(*asJson(a.params));
        if (!d.scalar) {
            a.line("out_" + a.id
                + " = 0.0_f64; // fromWorkspace: no baked data (vector or unresolved "
                  "variable)");
            return;
        }
        a.line("out_" + a.id + " = nflow_from_workspace(&FW_T_" + a.id + ", &FW_V_" + a.id + ", t, "
            + (d.interpolate ? "true" : "false") + ", " + std::to_string(d.afterMode) + ");");
    };
    return t;
}
//=============================================================================
