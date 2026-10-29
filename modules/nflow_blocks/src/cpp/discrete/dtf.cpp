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
#include "NFlowCodegenHelpers.hpp"
#include <cmath>
#include <algorithm>
#include "discrete_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleDtf(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    if (phase == Phase::INIT) {
        auto toPolyD = [](const json& j) -> std::vector<double> {
            std::vector<double> v;
            if (j.is_array()) {
                for (const auto& el : j) {
                    v.push_back(el.is_number() ? el.get<double>() : 0.0);
                }
            } else if (j.is_number()) {
                v.push_back(j.get<double>());
            }
            return v;
        };
        // Same reading as the emitted code: buildDiscreteTf normalises by the
        // leading denominator coefficient and pads a lower-degree numerator on
        // the left, so both lists are polynomials in z.
        const nflow::DiscreteTf model
            = nflow::buildDiscreteTf(toPolyD(asJson(b.params).value(nflow::kNum, json::array())),
                toPolyD(asJson(b.params).value(nflow::kDen, json::array())));
        std::vector<double> numArr = model.num;
        std::vector<double> denArr = model.den;
        // Per-element input/output histories: xHist is flat [width * nx],
        // yHist flat [width * ny]; the coefficients are shared.
        const int w = std::max(1, outputWidth(ctx, b.nid, 0));
        st.tfNum = numArr;
        st.tfDen = denArr;
        st.xHist.assign(numArr.size() * (size_t)w, 0.0);
        st.yHist.assign(std::max((int)denArr.size() - 1, 0) * (size_t)w, 0.0);
        st.outLatch.assign(w, 0.0);
        st.dNextTime = 0.0;
        st.output = 0.0;
        return false;
    }
    if (phase == Phase::ALGEBRAIC) {
        // The output answers the CURRENT input: a biproper system feeds through
        // and only a lower-degree numerator delays.
        const int w = std::max(1, outputWidth(ctx, b.nid, 0));
        const bool onHit = (ctx.t + 1e-6 >= st.dNextTime);
        if (!onHit) {
            if (w <= 1) {
                setOutput(ctx, b.nid, st.outLatch.empty() ? st.output : st.outLatch[0]);
            } else {
                double* yv = outputSlice(ctx, b.nid, 0);
                for (int e = 0; e < w; ++e) {
                    yv[e] = (e < (int)st.outLatch.size()) ? st.outLatch[e] : 0.0;
                }
            }
            return false;
        }
        const int nx = (int)st.tfNum.size();
        const int ny = std::max((int)st.tfDen.size() - 1, 0);
        SigView u = getInputSig(ctx, b.nid, 0);
        std::vector<double> xs(nx), ys(ny);
        if (w <= 1) {
            const double* xh = st.xHist.data();
            const double* yh = st.yHist.data();
            if (nx > 0) {
                xs[0] = sigAt(u, 0);
            }
            for (int k = 1; k < nx; ++k) {
                xs[k] = xh[k - 1];
            }
            for (int k = 0; k < ny; ++k) {
                ys[k] = yh[k];
            }
            setOutput(ctx, b.nid, evalDiscreteTf(st.tfNum, st.tfDen, xs, ys));
        } else {
            double* yv = outputSlice(ctx, b.nid, 0);
            for (int e = 0; e < w; ++e) {
                const double* xh = st.xHist.data() + (size_t)e * nx;
                const double* yh = st.yHist.data() + (size_t)e * ny;
                if (nx > 0) {
                    xs[0] = sigAt(u, e);
                }
                for (int k = 1; k < nx; ++k) {
                    xs[k] = xh[k - 1];
                }
                for (int k = 0; k < ny; ++k) {
                    ys[k] = yh[k];
                }
                yv[e] = evalDiscreteTf(st.tfNum, st.tfDen, xs, ys);
            }
        }
        return false;
    }
    if (phase == Phase::UPDATE) {
        nflow::BlockDescriptor bd(b, ctx.variables);
        double ts = std::max(0.001, bd.paramDouble(nflow::kTs, ctx.dt));
        if (ctx.t + 1e-6 >= st.dNextTime) {
            const int w = std::max(1, outputWidth(ctx, b.nid, 0));
            const int nx = (int)st.tfNum.size();
            const int ny = std::max((int)st.tfDen.size() - 1, 0);
            SigView u = getInputSig(ctx, b.nid, 0);
            if ((int)st.outLatch.size() != w) {
                st.outLatch.assign(w, 0.0);
            }
            std::vector<double> xs(nx), ys(ny);
            for (int e = 0; e < w; ++e) {
                double* xh = st.xHist.data() + (size_t)e * nx;
                double* yh = st.yHist.data() + (size_t)e * ny;
                for (int k = nx - 1; k > 0; --k) {
                    xh[k] = xh[k - 1];
                }
                if (nx > 0) {
                    xh[0] = sigAt(u, e);
                }
                for (int k = 0; k < nx; ++k) {
                    xs[k] = xh[k];
                }
                for (int k = 0; k < ny; ++k) {
                    ys[k] = yh[k];
                }
                double y = evalDiscreteTf(st.tfNum, st.tfDen, xs, ys);
                for (int k = ny - 1; k > 0; --k) {
                    yh[k] = yh[k - 1];
                }
                if (ny > 0) {
                    yh[0] = y;
                }
                st.outLatch[e] = y;
            }
            st.dNextTime = ctx.t + ts;
            st.output = st.outLatch.empty() ? 0.0 : st.outLatch[0];
        }
        return false;
    }
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCDtf()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        auto num = bd.paramList(nflow::kNum);
        auto den = bd.paramList(nflow::kDen);
        auto model = nflow::buildDiscreteTf(num, den);
        int numLen = static_cast<int>(model.num.size());
        int denLen = static_cast<int>(model.den.size());
        a.addConst("static const int dtf_num_" + a.id + "_n = " + std::to_string(numLen) + ";");
        a.addConst("static const int dtf_den_" + a.id + "_n = " + std::to_string(denLen) + ";");
        std::string numStr = "";
        for (int i = 0; i < numLen; ++i) {
            if (i > 0) {
                numStr += ", ";
            }
            numStr += a.fmt(model.num[i]);
        }
        std::string denStr = "";
        for (int i = 0; i < denLen; ++i) {
            if (i > 0) {
                denStr += ", ";
            }
            denStr += a.fmt(model.den[i]);
        }
        a.addConst("static const double dtf_num_" + a.id + "[" + std::to_string(numLen) + "] = {"
            + numStr + "};");
        a.addConst("static const double dtf_den_" + a.id + "[" + std::to_string(denLen) + "] = {"
            + denStr + "};");
        a.declState("double dtf_x_" + a.id + "[" + std::to_string(numLen) + "];");
        a.declState("double dtf_y_" + a.id + "[" + std::to_string(std::max(0, denLen - 1)) + "];");
        a.addState("dtf_next_" + a.id, "", "");
        a.addState("dtf_last_" + a.id, "", "");
        a.addInit("  for (int i = 0; i < " + std::to_string(numLen) + "; i++) s->dtf_x_" + a.id
            + "[i] = 0.0;");
        a.addInit("  for (int i = 0; i < " + std::to_string(std::max(0, denLen - 1))
            + "; i++) s->dtf_y_" + a.id + "[i] = 0.0;");
    };
    t.emitOutput = [](const BlockCodegenArgs& a) {
        a.line("out_" + a.id + " = (dtf_den_" + a.id + "_n > 1) ? s->dtf_y_" + a.id + "[0] : 0.0;");
    };
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double ts = std::fmax(0.001, bd.paramDouble(nflow::kTs, 0.0));
        if (ts == 0) {
            ts = a.dt;
        }
        // The output is the value computed ON the hit (held between hits), not
        // the one from the previous hit: a biproper system feeds through.
        a.line("if (t + 1e-6 >= s->dtf_next_" + a.id + ") {");
        a.line("  for (int i = dtf_num_" + a.id + "_n - 1; i > 0; i--) s->dtf_x_" + a.id
            + "[i] = s->dtf_x_" + a.id + "[i - 1];");
        a.line("  s->dtf_x_" + a.id + "[0] = " + a.in[0] + ";");
        a.line("  double y = 0.0;");
        a.line("  for (int i = 0; i < dtf_num_" + a.id + "_n; i++) y += dtf_num_" + a.id
            + "[i] * s->dtf_x_" + a.id + "[i];");
        a.line("  for (int i = 1; i < dtf_den_" + a.id + "_n; i++) y -= dtf_den_" + a.id
            + "[i] * s->dtf_y_" + a.id + "[i - 1];");
        a.line("  for (int i = dtf_den_" + a.id + "_n - 2; i > 0; i--) s->dtf_y_" + a.id
            + "[i] = s->dtf_y_" + a.id + "[i - 1];");
        a.line("  if (dtf_den_" + a.id + "_n > 1) s->dtf_y_" + a.id + "[0] = y;");
        a.line("  s->dtf_last_" + a.id + " = y;");
        a.line("  s->dtf_next_" + a.id + " = t + " + a.fmt(ts) + ";");
        a.line("}");
        a.line("out_" + a.id + " = s->dtf_last_" + a.id + ";");
    };
    return t;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustDtf()
{
    BlockCodegenTemplate t;
    t.emitState = [](const BlockCodegenStateArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        auto num = bd.paramList(nflow::kNum);
        auto den = bd.paramList(nflow::kDen);
        auto model = nflow::buildDiscreteTf(num, den);
        int numLen = static_cast<int>(model.num.size());
        int denLen = static_cast<int>(model.den.size());
        std::string cu = a.id;
        for (auto& c : cu) {
            c = static_cast<char>(std::toupper(static_cast<unsigned char>(c)));
        }
        a.addConst("const DTF_NUM_" + cu + "_N: usize = " + std::to_string(numLen) + ";");
        a.addConst("const DTF_DEN_" + cu + "_N: usize = " + std::to_string(denLen) + ";");
        std::string numStr;
        for (int i = 0; i < numLen; ++i) {
            if (i > 0) {
                numStr += ", ";
            }
            numStr += a.fmt(model.num[i]);
        }
        std::string denStr;
        for (int i = 0; i < denLen; ++i) {
            if (i > 0) {
                denStr += ", ";
            }
            denStr += a.fmt(model.den[i]);
        }
        a.addConst(
            "const DTF_NUM_" + cu + ": [f64; " + std::to_string(numLen) + "] = [" + numStr + "];");
        a.addConst(
            "const DTF_DEN_" + cu + ": [f64; " + std::to_string(denLen) + "] = [" + denStr + "];");
        a.declState("    pub dtf_x_" + a.id + ": [f64; " + std::to_string(numLen) + "],");
        a.declState(
            "    pub dtf_y_" + a.id + ": [f64; " + std::to_string(std::max(1, denLen - 1)) + "],");
        a.declState("    pub dtf_next_" + a.id + ": f64,");
        a.declState("    pub dtf_last_" + a.id + ": f64,");
        a.addInit(
            "    s.dtf_x_" + a.id + " = [" + a.fmt(0.0) + "; " + std::to_string(numLen) + "]; ");
        a.addInit("    s.dtf_y_" + a.id + " = [" + a.fmt(0.0) + "; "
            + std::to_string(std::max(1, denLen - 1)) + "]; ");
        a.addInit("    s.dtf_next_" + a.id + " = 0.0_f64;");
        a.addInit("    s.dtf_last_" + a.id + " = 0.0_f64;");
    };
    t.emitOutput
        = [](const BlockCodegenArgs& a) { a.line("out_" + a.id + " = s.dtf_last_" + a.id + ";"); };
    t.emitStep = [](const BlockCodegenArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        double ts = std::fmax(0.001, bd.paramDouble(nflow::kTs, 0.0));
        if (ts == 0) {
            ts = a.dt;
        }
        std::string cu = a.id;
        for (auto& c : cu) {
            c = static_cast<char>(std::toupper(static_cast<unsigned char>(c)));
        }
        // The value computed ON the hit is what leaves, held between hits: a
        // biproper system feeds through.
        a.line("if (t + 1e-6 >= s.dtf_next_" + a.id + ") {");
        a.line("    for i in (1..DTF_NUM_" + cu + "_N).rev() { s.dtf_x_" + a.id + "[i] = s.dtf_x_"
            + a.id + "[i - 1]; }");
        a.line("    s.dtf_x_" + a.id + "[0] = " + a.in[0] + ";");
        a.line("    let mut y = 0.0_f64;");
        a.line("    for i in 0..DTF_NUM_" + cu + "_N { y += DTF_NUM_" + cu + "[i] * s.dtf_x_" + a.id
            + "[i]; }");
        a.line("    for i in 1..DTF_DEN_" + cu + "_N { y -= DTF_DEN_" + cu + "[i] * s.dtf_y_" + a.id
            + "[i - 1]; }");
        a.line("    for i in (1..(DTF_DEN_" + cu + "_N - 1)).rev() { s.dtf_y_" + a.id
            + "[i] = s.dtf_y_" + a.id + "[i - 1]; }");
        a.line("    if DTF_DEN_" + cu + "_N > 1 { s.dtf_y_" + a.id + "[0] = y; }");
        a.line("    s.dtf_last_" + a.id + " = y;");
        a.line("    s.dtf_next_" + a.id + " = t + " + a.fmt(ts) + ";");
        a.line("}");
        a.line("out_" + a.id + " = s.dtf_last_" + a.id + ";");
    };
    return t;
}
//=============================================================================
