//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cmath>
#include <vector>
#include "Interpolation1DKernels.hpp"
//=============================================================================
// The arithmetic follows the generic implementation operation by operation so
// that both give bitwise identical results.
//=============================================================================
namespace Nelson {
//=============================================================================
static double
integerPower(double t, double exponent);
//=============================================================================
bool
isUniformGrid(const double* x, indexType n)
{
    double h0 = x[1] - x[0];
    double tolerance = 1e-10 * std::max(std::abs(h0), 1.);
    for (indexType k = 1; k < n - 1; ++k) {
        if (!(std::abs((x[k + 1] - x[k]) - h0) <= tolerance)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
void
cubicConvolution1D(const double* x, const double* y, indexType n, const double* q, indexType nq,
    bool useConstant, double constant, double* out)
{
    double h = x[1] - x[0];
    double lo = x[0];
    double hi = x[n - 1];
    for (indexType k = 0; k < nq; ++k) {
        double qi = q[k];
        if (qi < lo || qi > hi) {
            out[k] = useConstant ? constant : std::nan("");
            continue;
        }
        double position = std::floor((qi - lo) / h) + 1;
        auto idx = static_cast<indexType>(std::min(std::max(position, 1.), double(n - 1)));
        double t = (qi - x[idx - 1]) / h;
        double yi = y[idx - 1];
        double yp1 = y[idx];
        double ym1 = (idx >= 2) ? y[idx - 2] : 3 * y[0] - 3 * y[1] + y[2];
        double yp2 = (idx + 2 <= n) ? y[idx + 1] : 3 * y[n - 1] - 3 * y[n - 2] + y[n - 3];
        out[k] = yi
            + 0.5 * t
                * ((yp1 - ym1)
                    + t
                        * ((2 * ym1 - 5 * yi + 4 * yp1 - yp2)
                            + t * (-ym1 + 3 * yi - 3 * yp1 + yp2)));
    }
}
//=============================================================================
void
makima1D(const double* x, const double* y, indexType n, const double* q, indexType nq,
    bool useConstant, double constant, double* out)
{
    std::vector<double> d = makimaSlopes(x, y, n);
    for (indexType k = 0; k < nq; ++k) {
        double xk = q[k];
        if (std::isnan(xk)) {
            out[k] = std::nan("");
            continue;
        }
        if ((xk < x[0] || xk > x[n - 1]) && useConstant) {
            out[k] = constant;
            continue;
        }
        out[k] = makimaHermite(x, y, d.data(), n, xk);
    }
}
//=============================================================================
double
makimaHermite(const double* x, const double* y, const double* d, indexType n, double xk)
{
    indexType left = 0;
    if (xk <= x[0]) {
        left = 0;
    } else if (xk >= x[n - 1]) {
        left = n - 2;
    } else {
        left = static_cast<indexType>(std::upper_bound(x, x + n, xk) - x) - 1;
        left = std::min(left, n - 2);
    }
    double h = x[left + 1] - x[left];
    double t = (xk - x[left]) / h;
    double t3 = integerPower(t, 3);
    double t2 = integerPower(t, 2);
    double h00 = (2 * t3) - (3 * t2) + 1;
    double h10 = t3 - (2 * t2) + t;
    double h01 = (-2 * t3) + (3 * t2);
    double h11 = t3 - t2;
    return h00 * y[left] + h10 * h * d[left] + h01 * y[left + 1] + h11 * h * d[left + 1];
}
//=============================================================================
static double
integerPower(double t, double exponent)
{
    // same evaluation as the '^' operator for a real base and integer exponent
    if (std::isnan(t)) {
        return std::nan("");
    }
    if (std::isinf(t) && t < 0) {
        return std::pow(-0., -exponent);
    }
    if (t < 0) {
        double magnitude = std::pow(-t, exponent);
        return (std::fmod(exponent, 2.) == 0.) ? magnitude : -magnitude;
    }
    return std::pow(t, exponent);
}
//=============================================================================
std::vector<double>
makimaSlopes(const double* x, const double* y, indexType n)
{
    // modified Akima: the '+ |sum| / 2' terms avoid the 0/0 form of the
    // original scheme on runs of equal non-zero slopes
    std::vector<double> delta(n - 1);
    for (indexType k = 0; k < n - 1; ++k) {
        delta[k] = (y[k + 1] - y[k]) / (x[k + 1] - x[k]);
    }
    std::vector<double> d(n);
    if (n == 2) {
        d[0] = delta[0];
        d[1] = delta[0];
        return d;
    }
    // slopes extended by two linear extrapolations on each side
    std::vector<double> m(n + 3);
    for (indexType k = 0; k < n - 1; ++k) {
        m[k + 2] = delta[k];
    }
    m[1] = 2 * m[2] - m[3];
    m[0] = 2 * m[1] - m[2];
    m[n + 1] = 2 * m[n] - m[n - 1];
    m[n + 2] = 2 * m[n + 1] - m[n];
    for (indexType i = 0; i < n; ++i) {
        double w1 = std::abs(m[i + 3] - m[i + 2]) + std::abs(m[i + 3] + m[i + 2]) / 2;
        double w2 = std::abs(m[i + 1] - m[i]) + std::abs(m[i + 1] + m[i]) / 2;
        if (w1 + w2 == 0) {
            d[i] = 0;
        } else {
            d[i] = (w1 * m[i + 1] + w2 * m[i + 2]) / (w1 + w2);
        }
    }
    return d;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
