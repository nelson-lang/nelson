//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <complex>
#include <limits>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "BesselFunctions.hpp"
#include "AmosBessel.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
using cplx = std::complex<double>;
static const double besselNaN = std::numeric_limits<double>::quiet_NaN();
static const double besselInf = std::numeric_limits<double>::infinity();
//=============================================================================
// same rules as OMP_PARALLEL_FOR_LOOP, an element costs about a microsecond
static bool
useParallelLoop(indexType count)
{
#if WITH_OPENMP
    return NelsonConfiguration::getInstance()->isOpenMPParallelizationEnabled()
        && (uint64_t)count * 64
        > NelsonConfiguration::getInstance()->getOpenMPParallelizationThreshold();
#else
    return false;
#endif
}
//=============================================================================
static inline bool
isIntegerValue(double v)
{
    return std::isfinite(v) && v == std::floor(v);
}
//=============================================================================
static inline bool
isOddInteger(double v)
{
    return isIntegerValue(v) && std::fmod(std::fabs(v), 2.0) == 1.0;
}
//=============================================================================
// sin(pi v) and cos(pi v) with exact values at integers and half integers
static inline void
sinCosPi(double v, double& s, double& c)
{
    double r = std::fmod(v, 2.0);
    if (r < 0) {
        r += 2.0;
    }
    if (r == 0.0) {
        s = 0;
        c = 1;
    } else if (r == 0.5) {
        s = 1;
        c = 0;
    } else if (r == 1.0) {
        s = 0;
        c = -1;
    } else if (r == 1.5) {
        s = -1;
        c = 0;
    } else {
        const double pi = 3.14159265358979323846;
        s = std::sin(pi * r);
        c = std::cos(pi * r);
    }
}
//=============================================================================
// raw Amos call for a nonnegative order, returns the Amos error code in ierr
static cplx
amosBessel(BesselFunctionKind kind, double fnu, const cplx& z, int kode, int& ierr)
{
    double cr[1] = { 0 };
    double ci[1] = { 0 };
    double wr[1] = { 0 };
    double wi[1] = { 0 };
    int nz = 0;
    ierr = 0;
    switch (kind) {
    case BesselFunctionKind::J:
        Amos::zbesj(z.real(), z.imag(), fnu, kode, 1, cr, ci, nz, ierr);
        break;
    case BesselFunctionKind::Y:
        Amos::zbesy(z.real(), z.imag(), fnu, kode, 1, cr, ci, nz, wr, wi, ierr);
        break;
    case BesselFunctionKind::I:
        Amos::zbesi(z.real(), z.imag(), fnu, kode, 1, cr, ci, nz, ierr);
        break;
    case BesselFunctionKind::K:
        Amos::zbesk(z.real(), z.imag(), fnu, kode, 1, cr, ci, nz, ierr);
        break;
    case BesselFunctionKind::H1:
        Amos::zbesh(z.real(), z.imag(), fnu, kode, 1, 1, cr, ci, nz, ierr);
        break;
    case BesselFunctionKind::H2:
        Amos::zbesh(z.real(), z.imag(), fnu, kode, 2, 1, cr, ci, nz, ierr);
        break;
    }
    return cplx(cr[0], ci[0]);
}
//=============================================================================
// K of real order nu >= 0 at real x > 0 (times exp(x) when scaled), faster than the complex path
// Temme series for x <= 2, Steed continued fraction above; false near the range limits
static bool
besselKRealPositive(double nu, double x, bool scaled, double& value)
{
    const double pi = 3.14159265358979323846;
    const double eps = 1e-16;
    // Taylor coefficients of 1/Gamma(1 + mu)
    static const double rg[22]
        = { 1.0, 0.57721566490153286061, -0.65587807152025388108, -0.042002635034095235529,
              0.1665386113822914895, -0.042197734555544336748, -0.0096219715278769735621,
              0.0072189432466630995424, -0.0011651675918590651121, -0.00021524167411495097282,
              0.00012805028238811618615, -0.000020134854780788238656, -1.2504934821426706573e-6,
              1.1330272319816958824e-6, -2.0563384169776071035e-7, 6.1160951044814158179e-9,
              5.0020076444692229301e-9, -1.1812745704870201446e-9, 1.0434267116911005105e-10,
              7.782263439905071254e-12, -3.6968056186422057082e-12, 5.100370287454475979e-13 };
    const int nl = (int)(nu + 0.5);
    const double mu = nu - nl;
    const double m2 = mu * mu;
    const double xi = 1.0 / x;
    double kmu = 0;
    double k1 = 0;
    // exponent carried apart from K: -x when the pair comes scaled by exp(x)
    double kExp = 0;
    if (x <= 2.0) {
        double even = 0;
        double odd = 0;
        for (int k = 10; k >= 0; --k) {
            even = even * m2 + rg[2 * k];
            odd = odd * m2 + rg[2 * k + 1];
        }
        const double g1 = -odd;
        const double g2 = even;
        const double gp = even + mu * odd;
        const double gm = even - mu * odd;
        const double x2 = 0.5 * x;
        const double pimu = pi * mu;
        const double fct = (std::fabs(pimu) < eps) ? 1.0 : pimu / std::sin(pimu);
        const double d = -std::log(x2);
        const double e = mu * d;
        const double fct2 = (std::fabs(e) < eps) ? 1.0 : std::sinh(e) / e;
        double ff = fct * (g1 * std::cosh(e) + g2 * fct2 * d);
        double sum = ff;
        const double ee = std::exp(e);
        double p = 0.5 * ee / gp;
        double q = 0.5 / (ee * gm);
        double c = 1.0;
        const double dd = x2 * x2;
        double sum1 = p;
        for (int i = 1; i < 500; ++i) {
            const double fi = (double)i;
            ff = (fi * ff + p + q) / (fi * fi - m2);
            c *= dd / fi;
            p /= (fi - mu);
            q /= (fi + mu);
            const double del = c * ff;
            sum += del;
            sum1 += c * (p - fi * ff);
            if (std::fabs(del) < std::fabs(sum) * eps) {
                break;
            }
        }
        kmu = sum;
        k1 = sum1 * 2.0 * xi;
    } else {
        double b = 2.0 * (1.0 + x);
        double d = 1.0 / b;
        double h = d;
        double delh = d;
        double q1 = 0.0;
        double q2 = 1.0;
        const double a1 = 0.25 - m2;
        double q = a1;
        double c = a1;
        double a = -a1;
        double s = 1.0 + q * delh;
        for (int i = 1; i < 100000; ++i) {
            a -= 2.0 * i;
            c = -a * c / (i + 1.0);
            const double qn = (q1 - b * q2) / a;
            q1 = q2;
            q2 = qn;
            q += c * qn;
            b += 2.0;
            d = 1.0 / (b + a * d);
            delh = (b * d - 1.0) * delh;
            h += delh;
            const double dels = q * delh;
            s += dels;
            if (std::fabs(dels / s) < eps) {
                break;
            }
        }
        kmu = std::sqrt(pi / (2.0 * x)) / s;
        k1 = kmu * (mu + x + 0.5 - a1 * h) * xi;
        kExp = -x;
    }
    // forward recurrence, rescaled to stay in range
    for (int i = 1; i <= nl; ++i) {
        const double t = (mu + i) * 2.0 * xi * k1 + kmu;
        kmu = k1;
        k1 = t;
        if (std::fabs(k1) > 1e290) {
            kmu *= 1e-290;
            k1 *= 1e-290;
            kExp += 667.7496769682733;
        }
    }
    const double e = scaled ? kExp + x : kExp;
    // near the overflow or underflow limits the Amos routines decide (their own thresholds)
    const double logValue = e + std::log(kmu);
    if (!(std::fabs(logValue) < 664.87)) {
        return false;
    }
    value = (std::fabs(e) < 700) ? kmu * std::exp(e) : std::exp(logValue);
    return true;
}
//=============================================================================
// keeps the most severe error: zero argument (1), then overflow (2), then others
static inline void
mergeAmosError(int& acc, int e)
{
    if (e == 0 || e == 3) {
        return;
    }
    if (acc == 0 || e == 1 || (e == 2 && acc != 1)) {
        acc = e;
    }
}
//=============================================================================
// nonnegative order, with exact formulas on the negative real axis
static cplx
besselNonNegativeOrder(BesselFunctionKind kind, double v, const cplx& z, int kode, int& err)
{
    int e = 0;
    if (kind == BesselFunctionKind::K && z.imag() == 0.0 && z.real() > 0
        && std::isfinite(z.real())) {
        double value = 0;
        if (besselKRealPositive(v, z.real(), kode == 2, value)) {
            return cplx(value, 0.0);
        }
    }
    const bool isHankel = (kind == BesselFunctionKind::H1 || kind == BesselFunctionKind::H2);
    if (isHankel && z.imag() == 0.0 && z.real() > 0 && z.real() < v) {
        // below the order |J| << |Y|: H = J +/- iY keeps the small real part accurate
        double jx = amosBessel(BesselFunctionKind::J, v, z, 1, e).real();
        mergeAmosError(err, e);
        double yx = amosBessel(BesselFunctionKind::Y, v, z, 1, e).real();
        mergeAmosError(err, e);
        const double sign = (kind == BesselFunctionKind::H1) ? 1.0 : -1.0;
        cplx r(jx, sign * yx);
        if (kode == 2) {
            // scaled H1 carries exp(-i z), scaled H2 carries exp(i z)
            r *= cplx(std::cos(z.real()), -sign * std::sin(z.real()));
        }
        return r;
    }
    if (!(z.imag() == 0.0 && z.real() < 0)) {
        cplx r = amosBessel(kind, v, z, kode, e);
        mergeAmosError(err, e);
        return r;
    }
    const cplx x(-z.real(), 0.0);
    double s = 0;
    double c = 0;
    sinCosPi(v, s, c);
    const cplx ePlus(c, s);
    const cplx eMinus(c, -s);
    const double pi = 3.14159265358979323846;
    cplx r;
    switch (kind) {
    case BesselFunctionKind::J:
    case BesselFunctionKind::I: {
        // J(v, -x) = exp(i pi v) J(v, x), same rule for I
        r = ePlus * amosBessel(kind, v, x, kode, e);
        mergeAmosError(err, e);
    } break;
    case BesselFunctionKind::Y: {
        // Y(v, -x) = exp(-i pi v) Y(v, x) + 2 i cos(pi v) J(v, x)
        r = eMinus * amosBessel(BesselFunctionKind::Y, v, x, kode, e);
        mergeAmosError(err, e);
        if (c != 0) {
            r += cplx(0, 2 * c) * amosBessel(BesselFunctionKind::J, v, x, kode, e);
            mergeAmosError(err, e);
        }
    } break;
    case BesselFunctionKind::K: {
        // K(v, -x) = exp(-i pi v) K(v, x) - i pi I(v, x)
        cplx kx = amosBessel(BesselFunctionKind::K, v, x, kode, e);
        mergeAmosError(err, e);
        cplx ix = amosBessel(BesselFunctionKind::I, v, x, kode, e);
        mergeAmosError(err, e);
        if (kode == 2) {
            // scaled K(v, x) carries exp(x), the result needs exp(-x)
            kx *= std::exp(-2.0 * x.real());
        }
        r = eMinus * kx - cplx(0, pi) * ix;
    } break;
    case BesselFunctionKind::H1: {
        // H1(v, -x) = -exp(-i pi v) H2(v, x)
        r = -eMinus * besselNonNegativeOrder(BesselFunctionKind::H2, v, x, kode, err);
    } break;
    case BesselFunctionKind::H2: {
        // H2(v, -x) = 2 J(v, -x) - H1(v, -x) = (2 exp(i pi v) + exp(-i pi v)) J - i exp(-i pi v) Y
        cplx jx = amosBessel(BesselFunctionKind::J, v, x, 1, e);
        mergeAmosError(err, e);
        cplx yx = amosBessel(BesselFunctionKind::Y, v, x, 1, e);
        mergeAmosError(err, e);
        r = (2.0 * ePlus + eMinus) * jx - cplx(0, 1) * eMinus * yx;
        if (kode == 2) {
            // scaled H2 carries exp(i z) = exp(-i x)
            r *= cplx(std::cos(x.real()), -std::sin(x.real()));
        }
    } break;
    }
    return r;
}
//=============================================================================
// values at z = +Inf or -Inf, as given by the reference implementation
static cplx
besselAtRealInfinity(BesselFunctionKind kind, bool positive, bool scaled, bool forceReal)
{
    cplx nanValue = forceReal ? cplx(besselNaN, 0) : cplx(besselNaN, besselNaN);
    switch (kind) {
    case BesselFunctionKind::I:
        return scaled ? nanValue : cplx(besselInf, 0);
    case BesselFunctionKind::K:
        return (scaled || positive) ? cplx(0, 0) : cplx(besselInf, 0);
    default:
        return nanValue;
    }
}
//=============================================================================
static cplx
besselElement(BesselFunctionKind kind, double nu, const cplx& z, bool scaled)
{
    const bool isHankel = (kind == BesselFunctionKind::H1 || kind == BesselFunctionKind::H2);
    const bool zIsReal = (z.imag() == 0.0);
    const bool nuIsInteger = isIntegerValue(nu);
    if (std::isnan(nu) || std::isnan(z.real()) || std::isnan(z.imag())) {
        return cplx(besselNaN, 0);
    }
    // real results: real z >= 0, or J and I of integer order on the negative axis
    bool forceReal = false;
    if (!isHankel && zIsReal) {
        forceReal = !(z.real() < 0)
            || (nuIsInteger && (kind == BesselFunctionKind::J || kind == BesselFunctionKind::I));
    }
    if (std::isinf(z.real()) || std::isinf(z.imag())) {
        if (zIsReal) {
            return besselAtRealInfinity(kind, z.real() > 0, scaled, forceReal);
        }
        return cplx(besselNaN, besselNaN);
    }
    if (kind == BesselFunctionKind::Y && z == cplx(0, 0)) {
        return cplx(-besselInf, 0);
    }
    const int kode = scaled ? 2 : 1;
    const double v = std::fabs(nu);
    cplx zz = z;
    double sign = 1.0;
    if (forceReal && z.real() < 0) {
        // J(n, -x) = (-1)^n J(n, x) and I(n, -x) = (-1)^n I(n, x)
        zz = cplx(-z.real(), 0.0);
        if (isOddInteger(v)) {
            sign = -sign;
        }
    }
    int err = 0;
    cplx r;
    if (nu >= 0 || kind == BesselFunctionKind::K) {
        r = besselNonNegativeOrder(kind, v, zz, kode, err);
    } else if (nuIsInteger) {
        r = besselNonNegativeOrder(kind, v, zz, kode, err);
        if (isOddInteger(v) && kind != BesselFunctionKind::I) {
            sign = -sign;
        }
    } else {
        // reflection formulas for a negative non integer order
        double s = 0;
        double c = 0;
        sinCosPi(v, s, c);
        switch (kind) {
        case BesselFunctionKind::J:
        case BesselFunctionKind::Y: {
            double cj = (kind == BesselFunctionKind::J) ? c : s;
            double cy = (kind == BesselFunctionKind::J) ? -s : c;
            cplx jv(0, 0);
            cplx yv(0, 0);
            if (cj != 0) {
                jv = besselNonNegativeOrder(BesselFunctionKind::J, v, zz, kode, err);
            }
            if (cy != 0) {
                yv = besselNonNegativeOrder(BesselFunctionKind::Y, v, zz, kode, err);
            }
            r = cj * jv + cy * yv;
        } break;
        case BesselFunctionKind::I: {
            r = besselNonNegativeOrder(BesselFunctionKind::I, v, zz, kode, err);
            if (s != 0) {
                cplx kv = besselNonNegativeOrder(BesselFunctionKind::K, v, zz, kode, err);
                if (scaled) {
                    // K is scaled by exp(z) and I by exp(-|real(z)|)
                    kv *= std::exp(-zz - std::fabs(zz.real()));
                }
                const double twoOverPi = 0.636619772367581343075535053490;
                r += (twoOverPi * s) * kv;
            }
        } break;
        default: {
            cplx h = besselNonNegativeOrder(kind, v, zz, kode, err);
            r = cplx(c, kind == BesselFunctionKind::H1 ? s : -s) * h;
        } break;
        }
    }
    if (err == 1) {
        // zero argument for K and H
        r = isHankel ? cplx(besselNaN, 0) : cplx(besselInf, 0);
    } else if (err == 2) {
        r = cplx(besselInf, 0);
    } else if (err != 0) {
        r = cplx(besselNaN, 0);
    } else {
        r *= sign;
    }
    if (forceReal) {
        r = cplx(r.real(), 0.0);
    }
    return r;
}
//=============================================================================
static cplx
airyElement(int k, const cplx& z, bool scaled)
{
    if (std::isnan(z.real()) || std::isnan(z.imag())) {
        return cplx(besselNaN, 0);
    }
    const bool zIsReal = (z.imag() == 0.0);
    // scaled Ai and Ai' are complex on the negative real axis
    const bool forceReal = zIsReal && (!scaled || k >= 2 || !(z.real() < 0));
    if (std::isinf(z.real()) || std::isinf(z.imag())) {
        if (zIsReal && !scaled && z.real() > 0) {
            return (k < 2) ? cplx(0, 0) : cplx(besselInf, 0);
        }
        return forceReal ? cplx(besselNaN, 0) : cplx(besselNaN, besselNaN);
    }
    double ar = 0;
    double ai = 0;
    int nz = 0;
    int ierr = 0;
    const int kode = scaled ? 2 : 1;
    if (k < 2) {
        Amos::zairy(z.real(), z.imag(), k, kode, ar, ai, nz, ierr);
    } else {
        Amos::zbiry(z.real(), z.imag(), k - 2, kode, ar, ai, ierr);
    }
    cplx r;
    switch (ierr) {
    case 0:
    case 3:
        r = cplx(ar, ai);
        break;
    case 2:
        r = cplx(besselInf, 0);
        break;
    default:
        r = cplx(besselNaN, 0);
        break;
    }
    if (forceReal) {
        r = cplx(r.real(), 0.0);
    }
    return r;
}
//=============================================================================
static bool
isFloatArgument(const ArrayOf& arg)
{
    switch (arg.dataClass()) {
    case NLS_DOUBLE:
    case NLS_SINGLE:
    case NLS_DCOMPLEX:
    case NLS_SCOMPLEX:
        return true;
    default:
        return false;
    }
}
//=============================================================================
void
checkBesselFloatArgument(const ArrayOf& arg)
{
    if (!isFloatArgument(arg)) {
        Error(L"Nelson:special_functions:singleOrDoubleExpected",
            L"Input arguments must be single or double.");
    }
}
//=============================================================================
void
checkBesselArguments(const ArrayOf& nu, const ArrayOf& z)
{
    checkBesselFloatArgument(nu);
    checkBesselFloatArgument(z);
    if (nu.isComplex()) {
        Error(L"Nelson:special_functions:besselNuMustBeReal", L"NU must be real.");
    }
    if (nu.isSparse() || z.isSparse()) {
        Error(L"Nelson:special_functions:denseExpected",
            L"dense type for all input arguments expected.");
    }
    if (!nu.isScalar() && !z.isScalar() && !nu.dimensions().equals(z.dimensions())) {
        Error(L"Nelson:special_functions:besselSizeMismatch",
            L"NU and Z must have the same size, or one of them must be a scalar.");
    }
}
//=============================================================================
bool
getBesselScaleArgument(const ArrayOf& scale)
{
    checkBesselFloatArgument(scale);
    if (scale.isScalar() && !scale.isSparse()) {
        ArrayOf s(scale);
        s.promoteType(NLS_DCOMPLEX);
        const double* p = (const double*)s.dataPointer();
        if (p[1] == 0 && (p[0] == 0 || p[0] == 1)) {
            return p[0] == 1;
        }
    }
    Error(L"Nelson:special_functions:invalidScale", L"SCALE must be equal to 0 or 1.");
    return false;
}
//=============================================================================
int
getBesselOrderSelector(const ArrayOf& k, int low, int high, const std::wstring& errorId,
    const std::wstring& errorMessage)
{
    checkBesselFloatArgument(k);
    if (k.isScalar() && !k.isSparse() && !k.isComplex()) {
        ArrayOf kk(k);
        kk.promoteType(NLS_DOUBLE);
        double value = ((const double*)kk.dataPointer())[0];
        if (isIntegerValue(value) && value >= low && value <= high) {
            return (int)value;
        }
    }
    Error(errorId, errorMessage);
    return low;
}
//=============================================================================
// complex double view of Z (real input keeps zero imaginary parts)
static ArrayOf
asComplexDouble(const ArrayOf& z)
{
    ArrayOf zz(z);
    zz.promoteType(NLS_DCOMPLEX);
    return zz;
}
//=============================================================================
// stores the result, real when every imaginary part is zero
static ArrayOf
finalizeResult(double* values, const Dimensions& dims, bool asSingle)
{
    ArrayOf res = ArrayOf(NLS_DCOMPLEX, dims, values);
    if (res.allReal()) {
        res.promoteType(asSingle ? NLS_SINGLE : NLS_DOUBLE);
    } else if (asSingle) {
        res.promoteType(NLS_SCOMPLEX);
    }
    return res;
}
//=============================================================================
ArrayOf
BesselFunction(BesselFunctionKind kind, const ArrayOf& nu, const ArrayOf& z, bool scaled)
{
    checkBesselArguments(nu, z);
    const bool asSingle = (nu.dataClass() == NLS_SINGLE || z.dataClass() == NLS_SINGLE
        || z.dataClass() == NLS_SCOMPLEX);
    Dimensions dims = nu.isScalar() ? z.dimensions() : nu.dimensions();
    const indexType count = dims.elementCount();
    ArrayOf nuD(nu);
    nuD.promoteType(NLS_DOUBLE);
    ArrayOf zD = asComplexDouble(z);
    const double* ptrNu = (const double*)nuD.dataPointer();
    const auto* ptrZ = reinterpret_cast<const cplx*>(zD.dataPointer());
    const bool scalarNu = nu.isScalar();
    const bool scalarZ = z.isScalar();
    double* values = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, count, stringVector(), false);
    auto* out = reinterpret_cast<cplx*>(values);
    // the cost depends on NU and Z, dynamic chunks keep the threads busy
    const ompIndexType n = (ompIndexType)count;
    const bool parallel = useParallelLoop(count);
#if WITH_OPENMP
#pragma omp parallel for schedule(dynamic, 256) if (parallel)
#endif
    for (ompIndexType k = 0; k < n; ++k) {
        out[k] = besselElement(
            kind, scalarNu ? ptrNu[0] : ptrNu[k], scalarZ ? ptrZ[0] : ptrZ[k], scaled);
    }
    return finalizeResult(values, dims, asSingle);
}
//=============================================================================
ArrayOf
AiryFunction(int k, const ArrayOf& z, bool scaled)
{
    checkBesselFloatArgument(z);
    if (z.isSparse()) {
        Error(L"Nelson:special_functions:denseExpected",
            L"dense type for all input arguments expected.");
    }
    const bool asSingle = (z.dataClass() == NLS_SINGLE || z.dataClass() == NLS_SCOMPLEX);
    Dimensions dims = z.dimensions();
    const indexType count = dims.elementCount();
    ArrayOf zD = asComplexDouble(z);
    const auto* ptrZ = reinterpret_cast<const cplx*>(zD.dataPointer());
    double* values = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, count, stringVector(), false);
    auto* out = reinterpret_cast<cplx*>(values);
    const ompIndexType n = (ompIndexType)count;
    const bool parallel = useParallelLoop(count);
#if WITH_OPENMP
#pragma omp parallel for schedule(dynamic, 256) if (parallel)
#endif
    for (ompIndexType i = 0; i < n; ++i) {
        out[i] = airyElement(k, ptrZ[i], scaled);
    }
    return finalizeResult(values, dims, asSingle);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
