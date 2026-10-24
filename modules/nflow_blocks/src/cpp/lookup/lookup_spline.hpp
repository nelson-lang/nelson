//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Shared 1-D spline math (cubic not-a-knot + Akima) and a single-axis evaluator
// used by the multi-dimensional lookup blocks to build separable tensor-product
// splines. lookup1D keeps its own copies (frozen / validated); this header is
// the reusable form for lookup2D / lookupND.
//=============================================================================
#pragma once
//=============================================================================
#include <vector>
#include <string>
#include <cmath>
#include <algorithm>
#include "BlockRegistry.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace lookupspline {
        //=============================================================================
        inline std::vector<double>
        cubicSplineM(const std::vector<double>& x, const std::vector<double>& y)
        {
            const int n = (int)x.size();
            std::vector<double> M(std::max(0, n), 0.0);
            if (n < 3) {
                return M;
            }
            std::vector<double> h(n - 1);
            for (int i = 0; i < n - 1; ++i) {
                h[i] = x[i + 1] - x[i];
            }
            std::vector<std::vector<double>> A(n, std::vector<double>(n, 0.0));
            std::vector<double> rhs(n, 0.0);
            for (int i = 1; i <= n - 2; ++i) {
                A[i][i - 1] = h[i - 1];
                A[i][i] = 2.0 * (h[i - 1] + h[i]);
                A[i][i + 1] = h[i];
                rhs[i] = 6.0 * ((y[i + 1] - y[i]) / h[i] - (y[i] - y[i - 1]) / h[i - 1]);
            }
            if (n >= 4) {
                A[0][0] = h[1];
                A[0][1] = -(h[0] + h[1]);
                A[0][2] = h[0];
                A[n - 1][n - 3] = h[n - 2];
                A[n - 1][n - 2] = -(h[n - 3] + h[n - 2]);
                A[n - 1][n - 1] = h[n - 3];
            } else {
                A[0][0] = 1.0;
                A[n - 1][n - 1] = 1.0;
            }
            for (int col = 0; col < n; ++col) {
                int piv = col;
                for (int r = col + 1; r < n; ++r) {
                    if (std::fabs(A[r][col]) > std::fabs(A[piv][col])) {
                        piv = r;
                    }
                }
                std::swap(A[col], A[piv]);
                std::swap(rhs[col], rhs[piv]);
                const double d = A[col][col];
                if (std::fabs(d) < 1e-300) {
                    continue;
                }
                for (int r = 0; r < n; ++r) {
                    if (r == col) {
                        continue;
                    }
                    const double f = A[r][col] / d;
                    if (f != 0.0) {
                        for (int c = col; c < n; ++c) {
                            A[r][c] -= f * A[col][c];
                        }
                        rhs[r] -= f * rhs[col];
                    }
                }
            }
            for (int i = 0; i < n; ++i) {
                const double d = A[i][i];
                M[i] = (std::fabs(d) > 1e-300) ? rhs[i] / d : 0.0;
            }
            return M;
        }
        //=============================================================================
        inline double
        cubicSplineEval(const std::vector<double>& x, const std::vector<double>& y,
            const std::vector<double>& M, int i, double u)
        {
            const double h = x[i + 1] - x[i];
            if (h == 0.0) {
                return y[i];
            }
            const double A = (x[i + 1] - u) / h;
            const double B = (u - x[i]) / h;
            return A * y[i] + B * y[i + 1]
                + ((A * A * A - A) * M[i] + (B * B * B - B) * M[i + 1]) * (h * h) / 6.0;
        }
        //=============================================================================
        inline std::vector<double>
        akimaDerivs(const std::vector<double>& x, const std::vector<double>& y)
        {
            const int n = (int)x.size();
            std::vector<double> t(std::max(0, n), 0.0);
            if (n < 3) {
                return t;
            }
            const int ns = n - 1;
            std::vector<double> ext(n + 3, 0.0);
            for (int i = 0; i < ns; ++i) {
                ext[i + 2] = (y[i + 1] - y[i]) / (x[i + 1] - x[i]);
            }
            ext[1] = 2.0 * ext[2] - ext[3];
            ext[0] = 2.0 * ext[1] - ext[2];
            ext[n + 1] = 2.0 * ext[n] - ext[n - 1];
            ext[n + 2] = 2.0 * ext[n + 1] - ext[n];
            for (int i = 0; i < n; ++i) {
                const double w1 = std::fabs(ext[i + 3] - ext[i + 2]);
                const double w2 = std::fabs(ext[i + 1] - ext[i]);
                t[i] = (w1 + w2 == 0.0) ? 0.5 * (ext[i + 1] + ext[i + 2])
                                        : (w1 * ext[i + 1] + w2 * ext[i + 2]) / (w1 + w2);
            }
            return t;
        }
        //=============================================================================
        inline double
        akimaEval(const std::vector<double>& x, const std::vector<double>& y,
            const std::vector<double>& t, int i, double u)
        {
            const double h = x[i + 1] - x[i];
            if (h == 0.0) {
                return y[i];
            }
            const double p = (y[i + 1] - y[i]) / h;
            const double dx = u - x[i];
            const double a2 = (3.0 * p - 2.0 * t[i] - t[i + 1]) / h;
            const double a3 = (t[i] + t[i + 1] - 2.0 * p) / (h * h);
            return y[i] + t[i] * dx + a2 * dx * dx + a3 * dx * dx * dx;
        }
        //=============================================================================
        inline bool
        isSpline(const std::string& m)
        {
            return m == "Cubic spline" || m == "Akima spline";
        }
        //=============================================================================
        // Evaluate a single-axis spline (interp = "Cubic spline" | "Akima spline")
        // through (x, y) at u, with the given ExtrapMethod. n < 3 falls back to a
        // linear evaluation. Extrapolation uses the end cubic when extrap is a spline
        // method, else Linear or Clip.
        inline double
        spline1DValue(const std::vector<double>& x, const std::vector<double>& y, double u,
            const std::string& interp, const std::string& extrap)
        {
            const int n = (int)x.size();
            if (n == 0) {
                return 0.0;
            }
            if (n == 1) {
                return y[0];
            }
            const bool akima = (interp == "Akima spline");
            std::vector<double> coef;
            if (n >= 3) {
                coef = akima ? akimaDerivs(x, y) : cubicSplineM(x, y);
            }
            auto eval = [&](int i, double uu) {
                return akima ? akimaEval(x, y, coef, i, uu) : cubicSplineEval(x, y, coef, i, uu);
            };
            const bool sx = isSpline(extrap) && n >= 3;
            if (u <= x[0]) {
                if (u == x[0]) {
                    return y[0];
                }
                if (sx) {
                    return eval(0, u);
                }
                if (extrap == "Linear") {
                    return y[0] + (u - x[0]) * ((y[1] - y[0]) / (x[1] - x[0]));
                }
                return y[0];
            }
            if (u >= x[n - 1]) {
                if (u == x[n - 1]) {
                    return y[n - 1];
                }
                if (sx) {
                    return eval(n - 2, u);
                }
                if (extrap == "Linear") {
                    return y[n - 1]
                        + (u - x[n - 1]) * ((y[n - 1] - y[n - 2]) / (x[n - 1] - x[n - 2]));
                }
                return y[n - 1];
            }
            int i = n - 2;
            while (i > 0 && u < x[i]) {
                --i;
            }
            if (n < 3) {
                const double f = (u - x[i]) / (x[i + 1] - x[i]);
                return y[i] + f * (y[i + 1] - y[i]);
            }
            return eval(i, u);
        }
        //=============================================================================
        // Shared generated helpers mirroring the evaluators above: nflow_spl_eval
        // (one segment, cubic or Akima), nflow_spl_val (full single-axis value
        // with the extrapolation modes: 0 = clip, 1 = linear, 2 = spline), and
        // nflow_spl_solve (runtime coefficients: dense not-a-knot Gauss-Jordan
        // with partial pivoting, or Akima derivatives). Emitted once per target
        // (guard with a.once) by every spline-capable lookup block.
        //=============================================================================
        inline void
        emitSplineCodegenHelpers(const Nelson::NFlow::BlockCodegenStateArgs& a, bool rust)
        {
            auto H = [&](const char* l) { a.addHelper(l); };
            if (rust) {
                H("fn nflow_spl_eval(x: &[f64], y: &[f64], c: &[f64], i: usize, u: f64, akima: "
                  "bool) -> f64 {");
                H("    let h = x[i + 1] - x[i];");
                H("    if h == 0.0 { return y[i]; }");
                H("    if akima {");
                H("        let p = (y[i + 1] - y[i]) / h;");
                H("        let dx = u - x[i];");
                H("        let a2 = (3.0 * p - 2.0 * c[i] - c[i + 1]) / h;");
                H("        let a3 = (c[i] + c[i + 1] - 2.0 * p) / (h * h);");
                H("        return y[i] + c[i] * dx + a2 * dx * dx + a3 * dx * dx * dx;");
                H("    }");
                H("    let aa = (x[i + 1] - u) / h;");
                H("    let bb = (u - x[i]) / h;");
                H("    aa * y[i] + bb * y[i + 1] + ((aa * aa * aa - aa) * c[i] + (bb * bb * bb - "
                  "bb) * c[i + 1]) * (h * h) / 6.0");
                H("}");
                H("fn nflow_spl_val(x: &[f64], y: &[f64], c: &[f64], u: f64, akima: bool, xmode: "
                  "i32) -> f64 {");
                H("    let n = x.len();");
                H("    if n == 0 { return 0.0; }");
                H("    if n == 1 { return y[0]; }");
                H("    let sx = xmode == 2 && n >= 3;");
                H("    if u <= x[0] {");
                H("        if u == x[0] { return y[0]; }");
                H("        if sx { return nflow_spl_eval(x, y, c, 0, u, akima); }");
                H("        if xmode == 1 { return y[0] + (u - x[0]) * ((y[1] - y[0]) / (x[1] - "
                  "x[0])); }");
                H("        return y[0];");
                H("    }");
                H("    if u >= x[n - 1] {");
                H("        if u == x[n - 1] { return y[n - 1]; }");
                H("        if sx { return nflow_spl_eval(x, y, c, n - 2, u, akima); }");
                H("        if xmode == 1 { return y[n - 1] + (u - x[n - 1]) * ((y[n - 1] - y[n - "
                  "2]) / (x[n - 1] - x[n - 2])); }");
                H("        return y[n - 1];");
                H("    }");
                H("    let mut i = n - 2;");
                H("    while i > 0 && u < x[i] { i -= 1; }");
                H("    if n < 3 {");
                H("        let f = (u - x[i]) / (x[i + 1] - x[i]);");
                H("        return y[i] + f * (y[i + 1] - y[i]);");
                H("    }");
                H("    nflow_spl_eval(x, y, c, i, u, akima)");
                H("}");
                H("fn nflow_spl_solve(x: &[f64], y: &[f64], cout: &mut [f64], wk: &mut [f64], "
                  "akima: bool) {");
                H("    let n = x.len();");
                H("    for i in 0..n { cout[i] = 0.0; }");
                H("    if n < 3 { return; }");
                H("    if akima {");
                H("        for i in 0..n - 1 { wk[i + 2] = (y[i + 1] - y[i]) / (x[i + 1] - x[i]); "
                  "}");
                H("        wk[1] = 2.0 * wk[2] - wk[3];");
                H("        wk[0] = 2.0 * wk[1] - wk[2];");
                H("        wk[n + 1] = 2.0 * wk[n] - wk[n - 1];");
                H("        wk[n + 2] = 2.0 * wk[n + 1] - wk[n];");
                H("        for i in 0..n {");
                H("            let w1 = libm::fabs(wk[i + 3] - wk[i + 2]);");
                H("            let w2 = libm::fabs(wk[i + 1] - wk[i]);");
                H("            cout[i] = if w1 + w2 == 0.0 { 0.5 * (wk[i + 1] + wk[i + 2]) } else "
                  "{ (w1 * wk[i + 1] + w2 * wk[i + 2]) / (w1 + w2) };");
                H("        }");
                H("        return;");
                H("    }");
                H("    for i in 0..n * n + n { wk[i] = 0.0; }");
                H("    for i in 1..=n - 2 {");
                H("        let h0 = x[i] - x[i - 1];");
                H("        let h1 = x[i + 1] - x[i];");
                H("        wk[i * n + (i - 1)] = h0;");
                H("        wk[i * n + i] = 2.0 * (h0 + h1);");
                H("        wk[i * n + (i + 1)] = h1;");
                H("        wk[n * n + i] = 6.0 * ((y[i + 1] - y[i]) / h1 - (y[i] - y[i - 1]) / "
                  "h0);");
                H("    }");
                H("    if n >= 4 {");
                H("        let h0 = x[1] - x[0];");
                H("        let h1 = x[2] - x[1];");
                H("        let hm3 = x[n - 2] - x[n - 3];");
                H("        let hm2 = x[n - 1] - x[n - 2];");
                H("        wk[0] = h1; wk[1] = -(h0 + h1); wk[2] = h0;");
                H("        wk[(n - 1) * n + (n - 3)] = hm2;");
                H("        wk[(n - 1) * n + (n - 2)] = -(hm3 + hm2);");
                H("        wk[(n - 1) * n + (n - 1)] = hm3;");
                H("    } else {");
                H("        wk[0] = 1.0;");
                H("        wk[(n - 1) * n + (n - 1)] = 1.0;");
                H("    }");
                H("    for col in 0..n {");
                H("        let mut piv = col;");
                H("        for r in col + 1..n {");
                H("            if libm::fabs(wk[r * n + col]) > libm::fabs(wk[piv * n + col]) { "
                  "piv = r; }");
                H("        }");
                H("        if piv != col {");
                H("            for cc in 0..n { wk.swap(col * n + cc, piv * n + cc); }");
                H("            wk.swap(n * n + col, n * n + piv);");
                H("        }");
                H("        let d = wk[col * n + col];");
                H("        if libm::fabs(d) < 1e-300 { continue; }");
                H("        for r in 0..n {");
                H("            if r == col { continue; }");
                H("            let f = wk[r * n + col] / d;");
                H("            if f != 0.0 {");
                H("                for cc in col..n { wk[r * n + cc] -= f * wk[col * n + cc]; }");
                H("                wk[n * n + r] -= f * wk[n * n + col];");
                H("            }");
                H("        }");
                H("    }");
                H("    for i in 0..n {");
                H("        let d2 = wk[i * n + i];");
                H("        cout[i] = if libm::fabs(d2) > 1e-300 { wk[n * n + i] / d2 } else { 0.0 "
                  "};");
                H("    }");
                H("}");
                return;
            }
            H("static double nflow_spl_eval(const double* x, const double* y, const double* c, int "
              "i, double u, int akima) {");
            H("  double h = x[i + 1] - x[i];");
            H("  if (h == 0.0) return y[i];");
            H("  if (akima) {");
            H("    double p = (y[i + 1] - y[i]) / h;");
            H("    double dx = u - x[i];");
            H("    double a2 = (3.0 * p - 2.0 * c[i] - c[i + 1]) / h;");
            H("    double a3 = (c[i] + c[i + 1] - 2.0 * p) / (h * h);");
            H("    return y[i] + c[i] * dx + a2 * dx * dx + a3 * dx * dx * dx;");
            H("  }");
            H("  {");
            H("    double A = (x[i + 1] - u) / h;");
            H("    double B = (u - x[i]) / h;");
            H("    return A * y[i] + B * y[i + 1] + ((A * A * A - A) * c[i] + (B * B * B - B) * "
              "c[i + 1]) * (h * h) / 6.0;");
            H("  }");
            H("}");
            H("static double nflow_spl_val(const double* x, const double* y, const double* c, int "
              "n, double u, int akima, int xmode) {");
            H("  if (n == 0) return 0.0;");
            H("  if (n == 1) return y[0];");
            H("  {");
            H("    int sx = (xmode == 2) && (n >= 3);");
            H("    if (u <= x[0]) {");
            H("      if (u == x[0]) return y[0];");
            H("      if (sx) return nflow_spl_eval(x, y, c, 0, u, akima);");
            H("      if (xmode == 1) return y[0] + (u - x[0]) * ((y[1] - y[0]) / (x[1] - x[0]));");
            H("      return y[0];");
            H("    }");
            H("    if (u >= x[n - 1]) {");
            H("      if (u == x[n - 1]) return y[n - 1];");
            H("      if (sx) return nflow_spl_eval(x, y, c, n - 2, u, akima);");
            H("      if (xmode == 1) return y[n - 1] + (u - x[n - 1]) * ((y[n - 1] - y[n - 2]) / "
              "(x[n - 1] - x[n - 2]));");
            H("      return y[n - 1];");
            H("    }");
            H("    {");
            H("      int i = n - 2;");
            H("      while (i > 0 && u < x[i]) i--;");
            H("      if (n < 3) {");
            H("        double f = (u - x[i]) / (x[i + 1] - x[i]);");
            H("        return y[i] + f * (y[i + 1] - y[i]);");
            H("      }");
            H("      return nflow_spl_eval(x, y, c, i, u, akima);");
            H("    }");
            H("  }");
            H("}");
            H("static void nflow_spl_solve(const double* x, const double* y, int n, double* c, "
              "double* wk, int akima) {");
            H("  int i, r, col, cc;");
            H("  for (i = 0; i < n; ++i) c[i] = 0.0;");
            H("  if (n < 3) return;");
            H("  if (akima) {");
            H("    double* ext = wk;");
            H("    for (i = 0; i < n - 1; ++i) ext[i + 2] = (y[i + 1] - y[i]) / (x[i + 1] - "
              "x[i]);");
            H("    ext[1] = 2.0 * ext[2] - ext[3];");
            H("    ext[0] = 2.0 * ext[1] - ext[2];");
            H("    ext[n + 1] = 2.0 * ext[n] - ext[n - 1];");
            H("    ext[n + 2] = 2.0 * ext[n + 1] - ext[n];");
            H("    for (i = 0; i < n; ++i) {");
            H("      double w1 = fabs(ext[i + 3] - ext[i + 2]);");
            H("      double w2 = fabs(ext[i + 1] - ext[i]);");
            H("      c[i] = (w1 + w2 == 0.0) ? 0.5 * (ext[i + 1] + ext[i + 2]) : (w1 * ext[i + 1] "
              "+ w2 * ext[i + 2]) / (w1 + w2);");
            H("    }");
            H("    return;");
            H("  }");
            H("  {");
            H("    double* A = wk;");
            H("    double* rhs = wk + n * n;");
            H("    for (i = 0; i < n * n; ++i) A[i] = 0.0;");
            H("    for (i = 0; i < n; ++i) rhs[i] = 0.0;");
            H("    for (i = 1; i <= n - 2; ++i) {");
            H("      double h0 = x[i] - x[i - 1];");
            H("      double h1 = x[i + 1] - x[i];");
            H("      A[i * n + (i - 1)] = h0;");
            H("      A[i * n + i] = 2.0 * (h0 + h1);");
            H("      A[i * n + (i + 1)] = h1;");
            H("      rhs[i] = 6.0 * ((y[i + 1] - y[i]) / h1 - (y[i] - y[i - 1]) / h0);");
            H("    }");
            H("    if (n >= 4) {");
            H("      double h0 = x[1] - x[0], h1 = x[2] - x[1];");
            H("      double hm3 = x[n - 2] - x[n - 3], hm2 = x[n - 1] - x[n - 2];");
            H("      A[0] = h1; A[1] = -(h0 + h1); A[2] = h0;");
            H("      A[(n - 1) * n + (n - 3)] = hm2;");
            H("      A[(n - 1) * n + (n - 2)] = -(hm3 + hm2);");
            H("      A[(n - 1) * n + (n - 1)] = hm3;");
            H("    } else {");
            H("      A[0] = 1.0;");
            H("      A[(n - 1) * n + (n - 1)] = 1.0;");
            H("    }");
            H("    for (col = 0; col < n; ++col) {");
            H("      int piv = col;");
            H("      double d;");
            H("      for (r = col + 1; r < n; ++r) {");
            H("        if (fabs(A[r * n + col]) > fabs(A[piv * n + col])) piv = r;");
            H("      }");
            H("      if (piv != col) {");
            H("        double tswp;");
            H("        for (cc = 0; cc < n; ++cc) { tswp = A[col * n + cc]; A[col * n + cc] = "
              "A[piv * n + cc]; A[piv * n + cc] = tswp; }");
            H("        tswp = rhs[col]; rhs[col] = rhs[piv]; rhs[piv] = tswp;");
            H("      }");
            H("      d = A[col * n + col];");
            H("      if (fabs(d) < 1e-300) continue;");
            H("      for (r = 0; r < n; ++r) {");
            H("        double f;");
            H("        if (r == col) continue;");
            H("        f = A[r * n + col] / d;");
            H("        if (f != 0.0) {");
            H("          for (cc = col; cc < n; ++cc) A[r * n + cc] -= f * A[col * n + cc];");
            H("          rhs[r] -= f * rhs[col];");
            H("        }");
            H("      }");
            H("    }");
            H("    for (i = 0; i < n; ++i) {");
            H("      double d2 = A[i * n + i];");
            H("      c[i] = (fabs(d2) > 1e-300) ? rhs[i] / d2 : 0.0;");
            H("    }");
            H("  }");
            H("}");
        }
        //=============================================================================
        // Per-block glue for a separable tensor-product spline lookup of any
        // dimension count: axis-0 coefficients are baked (one solve per innermost
        // table column, computed here with the very evaluators the runtime uses);
        // every outer axis is reduced at run time (per-column dense solve). The
        // reduction is in-place over one buffer: pass k writes element c while
        // reading the window [c*nk, (c+1)*nk), and c <= c*nk always.
        //=============================================================================
        inline void
        emitSplineLookupCodegen(const Nelson::NFlow::BlockCodegenArgs& a, bool rust,
            const std::vector<std::vector<double>>& bps, const std::vector<double>& tbl,
            const std::string& interp, const std::string& extrap)
        {
            const int N = (int)bps.size();
            const bool akima = (interp == "Akima spline");
            const int xmode = (extrap == "Linear") ? 1 : (isSpline(extrap) ? 2 : 0);
            const std::string id = a.id;
            const std::string akimaLit = rust ? (akima ? "true" : "false") : (akima ? "1" : "0");
            const std::string xmodeLit = std::to_string(xmode);
            auto arrLine = [&](const std::string& name, const std::vector<double>& v) {
                std::string s
                    = rust ? "let " + name + " = [" : "static const double " + name + "[] = {";
                for (size_t i = 0; i < v.size(); ++i) {
                    s += (i ? ", " : "") + a.fmt(v[i]);
                }
                s += rust ? "];" : "};";
                a.line(s);
            };
            // Baked axis-0 coefficients, one block of n0 per innermost column.
            const int n0 = (int)bps[0].size();
            const int nCols = (int)tbl.size() / std::max(1, n0);
            std::vector<double> ic;
            ic.reserve(tbl.size());
            {
                std::vector<double> col((size_t)n0, 0.0);
                for (int c = 0; c < nCols; ++c) {
                    for (int i = 0; i < n0; ++i) {
                        col[(size_t)i] = tbl[(size_t)c * n0 + i];
                    }
                    if (n0 >= 3) {
                        const std::vector<double> cf
                            = akima ? akimaDerivs(bps[0], col) : cubicSplineM(bps[0], col);
                        ic.insert(ic.end(), cf.begin(), cf.end());
                    } else {
                        ic.insert(ic.end(), (size_t)n0, 0.0);
                    }
                }
            }
            const std::string tv = "__sptb_" + id;
            const std::string icv = "__spic_" + id;
            arrLine(tv, tbl);
            arrLine(icv, ic);
            for (int k = 0; k < N; ++k) {
                arrLine("__spbp_" + id + "_" + std::to_string(k), bps[k]);
                a.line((rust ? "let __spu_" : "double __spu_") + id + "_" + std::to_string(k)
                    + " = " + a.in[(size_t)k] + ";");
            }
            int wkMax = 4;
            int ocMax = 2;
            for (int k = 1; k < N; ++k) {
                const int nk = (int)bps[k].size();
                wkMax = std::max(wkMax, nk * (nk + 1));
                ocMax = std::max(ocMax, nk);
            }
            const std::string S1 = std::to_string(nCols);
            const std::string n0s = std::to_string(n0);
            const std::string bp0 = "__spbp_" + id + "_0";
            if (rust) {
                a.line("let mut __spv_" + id + " = [0.0_f64; " + S1 + "];");
                a.line("let mut __spo_" + id + " = [0.0_f64; " + std::to_string(ocMax) + "];");
                a.line("let mut __spw_" + id + " = [0.0_f64; " + std::to_string(wkMax) + "];");
                a.line("for __c in 0.." + S1 + " {");
                a.line("    let __t = nflow_spl_val(&" + bp0 + ", &" + tv + "[__c * " + n0s
                    + "..__c * " + n0s + " + " + n0s + "], &" + icv + "[__c * " + n0s + "..__c * "
                    + n0s + " + " + n0s + "], __spu_" + id + "_0, " + akimaLit + ", " + xmodeLit
                    + ");");
                a.line("    __spv_" + id + "[__c] = __t;");
                a.line("}");
            } else {
                a.line("double __spv_" + id + "[" + S1 + "];");
                a.line("double __spo_" + id + "[" + std::to_string(ocMax) + "];");
                a.line("double __spw_" + id + "[" + std::to_string(wkMax) + "];");
                a.line("for (int __c = 0; __c < " + S1 + "; __c++) {");
                a.line("  __spv_" + id + "[__c] = nflow_spl_val(" + bp0 + ", &" + tv + "[__c * "
                    + n0s + "], &" + icv + "[__c * " + n0s + "], " + n0s + ", __spu_" + id + "_0, "
                    + akimaLit + ", " + xmodeLit + ");");
                a.line("}");
            }
            int S = nCols;
            for (int k = 1; k < N; ++k) {
                const int nk = (int)bps[k].size();
                S /= nk;
                const std::string Ss = std::to_string(S);
                const std::string nks = std::to_string(nk);
                const std::string bpk = "__spbp_" + id + "_" + std::to_string(k);
                const std::string uk = "__spu_" + id + "_" + std::to_string(k);
                if (rust) {
                    a.line("for __c in 0.." + Ss + " {");
                    a.line("    nflow_spl_solve(&" + bpk + ", &__spv_" + id + "[__c * " + nks
                        + "..__c * " + nks + " + " + nks + "], &mut __spo_" + id + ", &mut __spw_"
                        + id + ", " + akimaLit + ");");
                    a.line("    let __t = nflow_spl_val(&" + bpk + ", &__spv_" + id + "[__c * "
                        + nks + "..__c * " + nks + " + " + nks + "], &__spo_" + id + "[0.." + nks
                        + "], " + uk + ", " + akimaLit + ", " + xmodeLit + ");");
                    a.line("    __spv_" + id + "[__c] = __t;");
                    a.line("}");
                } else {
                    a.line("for (int __c = 0; __c < " + Ss + "; __c++) {");
                    a.line("  nflow_spl_solve(" + bpk + ", &__spv_" + id + "[__c * " + nks + "], "
                        + nks + ", __spo_" + id + ", __spw_" + id + ", " + akimaLit + ");");
                    a.line("  __spv_" + id + "[__c] = nflow_spl_val(" + bpk + ", &__spv_" + id
                        + "[__c * " + nks + "], __spo_" + id + ", " + nks + ", " + uk + ", "
                        + akimaLit + ", " + xmodeLit + ");");
                    a.line("}");
                }
            }
            a.line("out_" + id + " = __spv_" + id + "[0];");
        }
        //=============================================================================
    } // namespace lookupspline
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
