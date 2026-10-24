//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
template <typename T>
static inline T
powi(T a, T b)
{
    unsigned long u;
    T p = 1;
    T x = a;
    T n = b;

    if (n != 0) {
        if (n < 0) {
            if (std::is_signed<T>()) {
                n = -n;
            }
            x = 1 / x;
        }
        for (u = (unsigned long)n;;) {
            if (u & 01) {
                p = scalar_scalar_integer_times(p, x);
            }
            if (u >>= 1) {
                x = scalar_scalar_integer_times(x, x);
            } else {
                break;
            }
        }
    }
    return (p);
}
//=============================================================================
double
power_dd(double a, double b);
//=============================================================================
// Exponentiation by squaring for a non-negative integer exponent, avoiding libm pow()
// in the hot a.^2 / a.^3 idioms. It multiplies the signed base directly, so sign, zero,
// infinity and NaN behave like a real integer power for any base ((-2)^3 == -8,
// (-Inf)^2 == +Inf, (-0)^3 keeps -0).
static inline double
fastIntPow(double a, unsigned long u)
{
    double result = 1.0;
    double x = a;
    while (u) {
        if (u & 1UL) {
            result *= x;
        }
        u >>= 1;
        if (u) {
            x *= x;
        }
    }
    return result;
}
//=============================================================================
// Integer exponent with 2 <= |b| <= 64, handled by fastIntPow.
static inline bool
isSmallIntegerExponent(double b)
{
    return std::isfinite(b) && std::trunc(b) == b && std::abs(b) >= 2.0 && std::abs(b) <= 64.0;
}
//=============================================================================
static inline bool
isOddInteger(double value)
{
    return std::isfinite(value) && std::trunc(value) == value && std::fmod(value, 2.0) != 0.0;
}
//=============================================================================
void
power_zi(double* p, const double* a, int b)
{
    if (a[1] == 0) {
        p[0] = power_dd(a[0], static_cast<double>(b));
        p[1] = 0;
        return;
    }
    // Complex base, integer exponent: repeated multiplication. std::pow(ca, int) goes
    // through exp/log and leaves parasitic components (i^4 = 1 - 2e-16i); squaring is
    // exact (i^4 == 1, complex(0,1e-4)^2 == -1e-8).
    std::complex<double> base(a[0], a[1]);
    bool negative = b < 0;
    unsigned long u = negative ? static_cast<unsigned long>(-static_cast<long>(b))
                               : static_cast<unsigned long>(b);
    std::complex<double> result(1.0, 0.0);
    while (u) {
        if (u & 1UL) {
            result *= base;
        }
        u >>= 1;
        if (u) {
            base *= base;
        }
    }
    if (negative) {
        result = std::complex<double>(1.0, 0.0) / result;
    }
    p[0] = result.real();
    p[1] = result.imag();
}
//=============================================================================
void
power_zz(double* c, const double* a, const double* b)
{
    if (b[0] == 0 && b[1] == 0) {
        c[0] = 1;
        c[1] = 0;
        return;
    }
    auto mag = complex_abs<double>(a[0], a[1]);
    if (mag == 0) {
        c[0] = 0;
        c[1] = 0;
        return;
    }
    if (a[1] == 0 && b[1] == 0 && (a[0] >= 0 || std::isinf(b[0]) || std::trunc(b[0]) == b[0])) {
        // real result: computed in real arithmetic ((-2)^3 == -8 exactly, (-1)^Inf == 1)
        c[0] = power_dd(a[0], b[0]);
        c[1] = 0;
        return;
    }
    // Complex base, integer exponent: exact repeated multiplication, so
    // complex(0,1e-4)^2 is exactly -1e-8.
    if (a[1] != 0 && b[1] == 0 && std::isfinite(b[0]) && std::trunc(b[0]) == b[0]
        && std::abs(b[0]) <= 2147483647.0) {
        power_zi(c, a, static_cast<int>(b[0]));
        return;
    }
    std::complex<double> ca(a[0], a[1]);
    std::complex<double> cb(b[0], b[1]);
    std::complex<double> cc;
    if (ca.imag() == 0) {
        cc = pow(ca.real(), cb);
    } else {
        cc = std::exp(cb * log(ca));
    }
    // clean-up relative to the magnitude of the result (do not zero out
    // legitimate small values such as complex(0, 1e-4)^2 == -1e-8)
    double tol = std::numeric_limits<double>::epsilon() * std::abs(cc);
    c[0] = (fabs(cc.real()) < tol) ? 0 : cc.real();
    if (ca.imag() == 0 && !std::isfinite(cc.imag())) {
        c[1] = 0;
    } else {
        c[1] = (fabs(cc.imag()) < tol) ? 0 : cc.imag();
    }
}
//=============================================================================
double
power_di(double a, int b)
{
    return std::pow(a, b);
}
//=============================================================================
double
power_dd(double a, double b)
{
    // NaN propagates even for a zero exponent (NaN^0 is NaN, not 1)
    if (std::isnan(a) || std::isnan(b)) {
        return std::nan("");
    }
    if (b == 0) {
        return 1;
    }
    if (b == 1) {
        return a;
    }
    if (a == 0) {
        // an odd integer exponent keeps the sign of a negative zero
        bool negativeResult = std::signbit(a) && isOddInteger(b);
        if (b < 0) {
            return negativeResult ? -std::numeric_limits<double>::infinity()
                                  : std::numeric_limits<double>::infinity();
        }
        return negativeResult ? -0.0 : 0.0;
    }
    if (a == -1 && std::isinf(b)) {
        return 1;
    }
    if (std::isinf(b) && b > 0) {
        if (abs(a) > 1) {
            return std::numeric_limits<double>::infinity();
        }
    }
    if (std::isinf(b) && b < 0) {
        if (abs(a) > 1) {
            return 0;
        }
    }
    if (std::isinf(b) && b > 0) {
        if (abs(a) < 1) {
            return 0;
        }
    }
    if (std::isinf(b) && b < 0) {
        if (abs(a) < 1) {
            return std::numeric_limits<double>::infinity();
        }
    }
    if (std::isinf(a) && a > 0) {
        if (b > 0) {
            return std::numeric_limits<double>::infinity();
        }
        if (b < 0) {
            return 0;
        }
    }
    if (std::isinf(a) && a < 0) {
        // an odd integer exponent keeps the sign of the base
        bool negativeResult = isOddInteger(b);
        if (b > 0) {
            return negativeResult ? -std::numeric_limits<double>::infinity()
                                  : std::numeric_limits<double>::infinity();
        }
        return negativeResult ? -0.0 : 0.0;
    }
    if (a < 0) {
        if (std::trunc(b) == b) {
            // negative base, integer exponent: exact real computation. The signed-base
            // fast path keeps the sign ((-2)^3 == -8) and matches (-x)^2 == x^2 bit-for-bit.
            if (isSmallIntegerExponent(b)) {
                long n = static_cast<long>(b);
                double result = fastIntPow(a, static_cast<unsigned long>(n < 0 ? -n : n));
                return (n < 0) ? (1.0 / result) : result;
            }
            double mag = pow(-a, b);
            return (std::fmod(b, 2.0) == 0.0) ? mag : -mag;
        }
        // negative base, non-integer exponent: complex result (handled elsewhere)
        return std::nan("");
    }
    // a is strictly positive and finite here; for a small integer exponent squaring
    // replaces the libm pow() call.
    if (isSmallIntegerExponent(b)) {
        long n = static_cast<long>(b);
        double result = fastIntPow(a, static_cast<unsigned long>(n < 0 ? -n : n));
        return (n < 0) ? (1.0 / result) : result;
    }
    return pow(a, b);
}
//=============================================================================
void
cicpower(int n, single* c, const single* a, int stride1, int* b, int stride2)
{
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            power_zi(z3, z1, b[i]);
            c[2 * i] = static_cast<single>(z3[0]);
            c[2 * i + 1] = static_cast<single>(z3[1]);
        }
    } else if (stride1 == 1 && stride2 == 0) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            power_zi(z3, z1, b[0]);
            c[2 * i] = static_cast<single>(z3[0]);
            c[2 * i + 1] = static_cast<single>(z3[1]);
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z3[2];
            z1[0] = a[2 * 0];
            z1[1] = a[2 * 0 + 1];
            power_zi(z3, z1, b[i]);
            c[2 * i] = static_cast<single>(z3[0]);
            c[2 * i + 1] = static_cast<single>(z3[1]);
        }
    }
}
//=============================================================================
void
cfcpower(int n, single* c, const single* a, int stride1, const single* b, int stride2)
{
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            z2[0] = b[i];
            z2[1] = 0;
            power_zz(z3, z1, z2);
            c[2 * i] = static_cast<single>(z3[0]);
            c[2 * i + 1] = static_cast<single>(z3[1]);
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[2 * 0];
            z1[1] = a[2 * 0 + 1];
            z2[0] = b[i];
            z2[1] = 0;
            power_zz(z3, z1, z2);
            c[2 * i] = static_cast<single>(z3[0]);
            c[2 * i + 1] = static_cast<single>(z3[1]);
        }
    } else if (stride1 == 1 && stride2 == 0) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            z2[0] = b[0];
            z2[1] = 0;
            power_zz(z3, z1, z2);
            c[2 * i] = static_cast<single>(z3[0]);
            c[2 * i + 1] = static_cast<single>(z3[1]);
        }
    }
}
//=============================================================================
void
zdzpower(int n, double* c, const double* a, int stride1, const double* b, int stride2)
{
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            z2[0] = b[i];
            z2[1] = 0;
            power_zz(z3, z1, z2);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[2 * 0];
            z1[1] = a[2 * 0 + 1];
            z2[0] = b[i];
            z2[1] = 0;
            power_zz(z3, z1, z2);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
        }
    } else if (stride1 == 1 && stride2 == 0) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            z2[0] = b[0];
            z2[1] = 0;
            power_zz(z3, z1, z2);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
        }
    }
}
//=============================================================================
void
cccpower(int n, single* c, const single* a, int stride1, const single* b, int stride2)
{
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            z2[0] = b[2 * i];
            z2[1] = b[2 * i + 1];
            power_zz(z3, z1, z2);
            c[2 * i] = static_cast<single>(z3[0]);
            c[2 * i + 1] = static_cast<single>(z3[1]);
        }
    } else if (stride1 == 1 && stride2 == 0) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            z2[0] = b[2 * 0];
            z2[1] = b[2 * 0 + 1];
            power_zz(z3, z1, z2);
            c[2 * i] = static_cast<single>(z3[0]);
            c[2 * i + 1] = static_cast<single>(z3[1]);
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[2 * 0];
            z1[1] = a[2 * 0 + 1];
            z2[0] = b[2 * i];
            z2[1] = b[2 * i + 1];
            power_zz(z3, z1, z2);
            c[2 * i] = static_cast<single>(z3[0]);
            c[2 * i + 1] = static_cast<single>(z3[1]);
        }
    }
}
//=============================================================================
void
zzzpower(int n, double* c, const double* a, int stride1, const double* b, int stride2)
{
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];

            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            z2[0] = b[2 * i];
            z2[1] = b[2 * i + 1];
            power_zz(z3, z1, z2);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[0];
            z1[1] = a[1];
            z2[0] = b[2 * i];
            z2[1] = b[2 * i + 1];
            power_zz(z3, z1, z2);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
        }
    } else if (stride1 == 1 && stride2 == 0) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            z2[0] = b[2 * 0];
            z2[1] = b[2 * 0 + 1];
            power_zz(z3, z1, z2);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
        }
    }
}
//=============================================================================
void
zizpower(int n, double* c, const double* a, int stride1, int* b, int stride2)
{
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            power_zi(z3, z1, b[i]);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z3[2];
            z1[0] = a[2 * 0];
            z1[1] = a[2 * 0 + 1];
            power_zi(z3, z1, b[i]);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
        }
    } else if (stride1 == 1 && stride2 == 0) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            power_zi(z3, z1, b[0]);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
        }
    }
}
//=============================================================================
void
didpower(int n, double* c, double* a, int stride1, int* b, int stride2)
{
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = power_di(a[i], b[i]);
        }
    } else if (stride1 == 1 && stride2 == 0) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = power_di(a[i], b[0]);
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = power_di(a[0], b[i]);
        }
    }
}
//=============================================================================
void
dddpower(int n, double* c, double* a, int stride1, double* b, int stride2)
{
    if (stride1 == 1 && stride2 == 0) {
        // Scalar exponent (a.^2 / a.^3): analyze it once instead of running power_dd's
        // branch cascade per element. Signed-base multiplication matches power_dd for
        // small integer exponents. The 2/3/4 loops are straight-line so the compiler
        // vectorizes them; other exponents use squaring.
        if (isSmallIntegerExponent(b[0])) {
            long e = static_cast<long>(b[0]);
            if (e == 2) {
                OMP_PARALLEL_FOR_LOOP(n)
                for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
                    c[i] = a[i] * a[i];
                }
            } else if (e == 3) {
                OMP_PARALLEL_FOR_LOOP(n)
                for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
                    c[i] = a[i] * a[i] * a[i];
                }
            } else if (e == 4) {
                OMP_PARALLEL_FOR_LOOP(n)
                for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
                    double s = a[i] * a[i];
                    c[i] = s * s;
                }
            } else {
                bool negative = e < 0;
                unsigned long u = static_cast<unsigned long>(negative ? -e : e);
                OMP_PARALLEL_FOR_LOOP(n)
                for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
                    double result = fastIntPow(a[i], u);
                    c[i] = negative ? (1.0 / result) : result;
                }
            }
            return;
        }
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = power_dd(a[i], b[0]);
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = power_dd(a[0], b[i]);
        }
    } else if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = power_dd(a[i], b[i]);
        }
    }
}
//=============================================================================
void
fifpower(int n, single* c, single* a, int stride1, int* b, int stride2)
{
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = static_cast<single>(power_di(a[i], b[i]));
        }
    } else if (stride1 == 1 && stride2 == 0) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = static_cast<single>(power_di(a[i], b[0]));
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = static_cast<single>(power_di(a[0], b[i]));
        }
    }
}
//=============================================================================
void
fffpower(int n, single* c, single* a, int stride1, single* b, int stride2)
{
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = static_cast<single>(power_dd(a[i], b[i]));
        }
    } else if (stride1 == 1 && stride2 == 0) {
        // Scalar exponent: same hoisting as dddpower, computed in double and rounded
        // once to single.
        if (isSmallIntegerExponent(b[0])) {
            long e = static_cast<long>(b[0]);
            if (e == 2) {
                OMP_PARALLEL_FOR_LOOP(n)
                for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
                    double x = static_cast<double>(a[i]);
                    c[i] = static_cast<single>(x * x);
                }
            } else if (e == 3) {
                OMP_PARALLEL_FOR_LOOP(n)
                for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
                    double x = static_cast<double>(a[i]);
                    c[i] = static_cast<single>(x * x * x);
                }
            } else if (e == 4) {
                OMP_PARALLEL_FOR_LOOP(n)
                for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
                    double x = static_cast<double>(a[i]);
                    double s = x * x;
                    c[i] = static_cast<single>(s * s);
                }
            } else {
                bool negative = e < 0;
                unsigned long u = static_cast<unsigned long>(negative ? -e : e);
                OMP_PARALLEL_FOR_LOOP(n)
                for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
                    double result = fastIntPow(static_cast<double>(a[i]), u);
                    c[i] = static_cast<single>(negative ? (1.0 / result) : result);
                }
            }
            return;
        }
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = static_cast<single>(power_dd(a[i], b[0]));
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = static_cast<single>(power_dd(a[0], b[i]));
        }
    }
}
//=============================================================================
using vvfun = void (*)(indexType, void*, const void*, const int, const void*, const int);
//=============================================================================
template <typename T>
static void
powerInteger(
    const ArrayOf& A, const ArrayOf& B, indexType n, indexType stride1, indexType stride2, void* Cp)
{
    T* a = (T*)A.dataPointer();
    T* b = (T*)B.dataPointer();
    T* c = static_cast<T*>(Cp);
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < (ompIndexType)n; i++) {
            c[i] = powi<T>(a[i], b[i]);
        }
    } else if (stride1 == 1 && stride2 == 0) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < (ompIndexType)n; i++) {
            c[i] = powi<T>(a[i], b[0]);
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < (ompIndexType)n; i++) {
            c[i] = powi<T>(a[0], b[i]);
        }
    }
}
//=============================================================================
