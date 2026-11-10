//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <map>
#include "GpuElementwiseMath.hpp"
#include "GpuKernels.hpp"
#include "GpuDispatch.hpp"
#include "GpuContext.hpp"
#include "GpuTransfer.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// shared WGSL helpers (single precision)
static const char* MATH_HELPERS_WGSL = R"WGSL(
const NLS_PI : f32 = 3.14159265358979;
const NLS_D2R : f32 = 0.0174532925199433;
const NLS_R2D : f32 = 57.2957795130823;
fn nlsIsNan(x : f32) -> bool { return (bitcast<u32>(x) & 0x7fffffffu) > 0x7f800000u; }
fn nlsIsInf(x : f32) -> bool { return (bitcast<u32>(x) & 0x7fffffffu) == 0x7f800000u; }
// runtime values: the shader compiler rejects NaN and Inf constants
fn nlsNaN() -> f32 { var bits : u32 = 0x7fc00000u; return bitcast<f32>(bits); }
fn nlsInf() -> f32 { var bits : u32 = 0x7f800000u; return bitcast<f32>(bits); }
fn nlsSign(x : f32) -> f32 { return select(sign(x), x, nlsIsNan(x)); }
fn nlsIsInt(x : f32) -> bool { return !nlsIsInf(x) && !nlsIsNan(x) && floor(x) == x; }
fn nlsIsNegative(x : f32) -> bool { return (bitcast<u32>(x) & 0x80000000u) != 0u; }
fn nlsCopySign(m : f32, s : f32) -> f32 { return select(abs(m), -abs(m), nlsIsNegative(s)); }
// sin and cos after reduction by pi/4 in three parts (accurate for |x| up to a few thousand)
fn nlsSinCos(x : f32) -> vec2<f32> {
    if (nlsIsInf(x) || nlsIsNan(x)) { return vec2<f32>(nlsNaN(), nlsNaN()); }
    let ax = abs(x);
    var j = u32(1.27323954473516 * ax);
    if ((j & 1u) != 0u) { j = j + 1u; }
    let y = f32(j);
    j = j & 7u;
    let z = ((ax - y * 0.78515625) - y * 2.4187564849853515625e-4) - y * 3.77489497744594108e-8;
    let zz = z * z;
    let s = ((-1.9515295891e-4 * zz + 8.3321608736e-3) * zz - 1.6666654611e-1) * zz * z + z;
    let c = ((2.443315711809948e-5 * zz - 1.388731625493765e-3) * zz + 4.166664568298827e-2) * zz * zz
        - 0.5 * zz + 1.0;
    var sinSign : f32 = 1.0;
    var cosSign : f32 = 1.0;
    var k = j;
    if (k > 3u) { k = k - 4u; sinSign = -1.0; cosSign = -1.0; }
    if (k > 1u) { cosSign = -cosSign; }
    var sv = s;
    var cv = c;
    if (k == 1u || k == 2u) { sv = c; cv = s; }
    var sinValue = sinSign * sv;
    if (nlsIsNegative(x)) { sinValue = -sinValue; }
    return vec2<f32>(sinValue, cosSign * cv);
}
fn nlsSin(x : f32) -> f32 { return nlsSinCos(x).x; }
fn nlsCos(x : f32) -> f32 { return nlsSinCos(x).y; }
fn nlsTan(x : f32) -> f32 { let sc = nlsSinCos(x); return sc.x / sc.y; }
fn nlsAtan(x : f32) -> f32 {
    if (x == 0.0) { return x; }
    let ax = abs(x);
    var y0 : f32 = 0.0;
    var z = ax;
    if (ax > 2.414213562373095) { y0 = 1.5707963267948966; z = -1.0 / ax; }
    else if (ax > 0.4142135623730950) { y0 = 0.7853981633974483; z = (ax - 1.0) / (ax + 1.0); }
    let zz = z * z;
    let r = (((8.05374449538e-2 * zz - 1.38776856032e-1) * zz + 1.99777106478e-1) * zz
        - 3.33329491539e-1) * zz * z + z + y0;
    return nlsCopySign(r, x);
}
fn nlsAtan2(y : f32, x : f32) -> f32 {
    if (nlsIsNan(x) || nlsIsNan(y)) { return nlsNaN(); }
    if (nlsIsInf(x) && nlsIsInf(y)) {
        let q = select(0.7853981633974483, 2.356194490192345, x < 0.0);
        return nlsCopySign(q, y);
    }
    if (y == 0.0) {
        if (nlsIsNegative(x)) { return nlsCopySign(NLS_PI, y); }
        return y;
    }
    if (x == 0.0) { return nlsCopySign(1.5707963267948966, y); }
    var a : f32;
    if (abs(x) >= abs(y)) {
        a = nlsAtan(y / x);
        if (x < 0.0) { a = a + nlsCopySign(NLS_PI, y); }
    } else {
        a = nlsCopySign(1.5707963267948966, y) - nlsAtan(x / y);
    }
    return a;
}
fn nlsAsinCore(ax : f32) -> f32 {
    var z : f32;
    var v : f32;
    let big = ax > 0.5;
    if (big) { z = 0.5 * (1.0 - ax); v = sqrt(z); } else { z = ax * ax; v = ax; }
    var p = ((((4.2163199048e-2 * z + 2.4181311049e-2) * z + 4.5470025998e-2) * z
        + 7.4953002686e-2) * z + 1.6666752422e-1) * z * v + v;
    if (big) { p = 1.5707963267948966 - 2.0 * p; }
    return p;
}
fn nlsAsin(x : f32) -> f32 {
    if (nlsIsNan(x) || abs(x) > 1.0) { return nlsNaN(); }
    return nlsCopySign(nlsAsinCore(abs(x)), x);
}
fn nlsAcos(x : f32) -> f32 {
    if (nlsIsNan(x) || abs(x) > 1.0) { return nlsNaN(); }
    if (x < -0.5) { return NLS_PI - 2.0 * nlsAsinCore(sqrt(0.5 * (1.0 + x))); }
    if (x > 0.5) { return 2.0 * nlsAsinCore(sqrt(0.5 * (1.0 - x))); }
    return 1.5707963267948966 - nlsAsin(x);
}
fn nlsSinh(x : f32) -> f32 {
    if (x == 0.0) { return x; }
    let ax = abs(x);
    if (ax < 1.0) {
        let z = x * x;
        return ((2.03721912945e-4 * z + 8.33028376239e-3) * z + 1.66667160211e-1) * z * x + x;
    }
    if (ax > 80.0) { return nlsCopySign(exp(ax - 0.6931471805599453), x); }
    let e = exp(ax);
    return nlsCopySign(0.5 * (e - 1.0 / e), x);
}
fn nlsCosh(x : f32) -> f32 {
    let ax = abs(x);
    if (ax > 80.0) { return exp(ax - 0.6931471805599453); }
    let e = exp(ax);
    return 0.5 * (e + 1.0 / e);
}
fn nlsTanh(x : f32) -> f32 {
    if (nlsIsNan(x)) { return x; }
    if (x == 0.0) { return x; }
    let ax = abs(x);
    if (ax >= 9.0) { return nlsCopySign(1.0, x); }
    if (ax < 0.625) {
        let z = x * x;
        return ((((-5.70498872745e-3 * z + 2.06390887954e-2) * z - 5.37397155531e-2) * z
            + 1.33314422036e-1) * z - 3.33332819422e-1) * z * x + x;
    }
    return nlsCopySign(1.0 - 2.0 / (exp(2.0 * ax) + 1.0), x);
}
fn nlsAsinh(x : f32) -> f32 {
    if (nlsIsNan(x) || nlsIsInf(x)) { return x; }
    if (x == 0.0) { return x; }
    let ax = abs(x);
    if (ax > 1500.0) { return nlsCopySign(log(ax) + 0.6931471805599453, x); }
    let z = x * x;
    if (ax < 0.5) {
        return (((2.0122003309e-2 * z - 4.2699340972e-2) * z + 7.4847586088e-2) * z
            - 1.6666288134e-1) * z * x + x;
    }
    return nlsCopySign(log(ax + sqrt(z + 1.0)), x);
}
fn nlsAcosh(x : f32) -> f32 {
    if (nlsIsNan(x) || x < 1.0) { return nlsNaN(); }
    if (x > 1500.0) { return log(x) + 0.6931471805599453; }
    let z = x - 1.0;
    if (z < 0.5) {
        return ((((1.7596881071e-3 * z - 7.5272886713e-3) * z + 2.6454905019e-2) * z
            - 1.1784741703e-1) * z + 1.4142135263) * sqrt(z);
    }
    return log(x + sqrt(z * (x + 1.0)));
}
fn nlsAtanh(x : f32) -> f32 {
    if (nlsIsNan(x) || abs(x) > 1.0) { return nlsNaN(); }
    if (x == 0.0) { return x; }
    if (abs(x) == 1.0) { return x * nlsInf(); }
    if (abs(x) < 0.5) {
        let z = x * x;
        return ((((1.81740078349e-1 * z + 8.24370301058e-2) * z + 1.46691431730e-1) * z
            + 1.99782164500e-1) * z + 3.33337300303e-1) * z * x + x;
    }
    return 0.5 * log((1.0 + x) / (1.0 - x));
}
fn nlsHypot(a : f32, b : f32) -> f32 {
    if (nlsIsInf(a) || nlsIsInf(b)) { return nlsInf(); }
    let m = max(abs(a), abs(b));
    if (m == 0.0 || nlsIsNan(m)) { return m + abs(a) + abs(b); }
    let p = a / m;
    let q = b / m;
    return m * sqrt(p * p + q * q);
}
fn nlsCsqrt(re : f32, im : f32) -> vec2<f32> {
    if (re == 0.0 && im == 0.0) { return vec2<f32>(0.0, im); }
    if (nlsIsInf(im)) { return vec2<f32>(nlsInf(), im); }
    let t = sqrt(0.5 * (nlsHypot(re, im) + abs(re)));
    if (re >= 0.0) { return vec2<f32>(t, im / (2.0 * t)); }
    return vec2<f32>(abs(im) / (2.0 * t), nlsCopySign(t, im));
}
// division with a scaled denominator (no overflow for large parts)
fn nlsCdiv(ar : f32, ai : f32, br : f32, bi : f32) -> vec2<f32> {
    if (bi == 0.0) { return vec2<f32>(ar / br, ai / br); }
    if (br == 0.0) { return vec2<f32>(ai / bi, -ar / bi); }
    if (abs(br) >= abs(bi)) {
        let r = bi / br;
        let d = br + bi * r;
        return vec2<f32>((ar + ai * r) / d, (ai - ar * r) / d);
    }
    let r = br / bi;
    let d = br * r + bi;
    return vec2<f32>((ar * r + ai) / d, (ai * r - ar) / d);
}
fn nlsCexp(re : f32, im : f32) -> vec2<f32> {
    let e = exp(re);
    if (im == 0.0) { return vec2<f32>(e, im); }
    let sc = nlsSinCos(im);
    return vec2<f32>(e * sc.y, e * sc.x);
}
fn nlsClog(re : f32, im : f32) -> vec2<f32> {
    return vec2<f32>(log(nlsHypot(re, im)), nlsAtan2(im, re));
}
fn nlsCtan(re : f32, im : f32) -> vec2<f32> {
    if (abs(im) > 20.0) {
        return vec2<f32>(4.0 * nlsSin(2.0 * re) * exp(-2.0 * abs(im)), nlsCopySign(1.0, im));
    }
    let d = nlsCos(2.0 * re) + nlsCosh(2.0 * im);
    return vec2<f32>(nlsSin(2.0 * re) / d, nlsSinh(2.0 * im) / d);
}
// round half away from zero
fn nlsRound(x : f32) -> f32 {
    let t = trunc(x);
    if (abs(x - t) >= 0.5) { return t + sign(x); }
    return t;
}
fn nlsSind(x : f32) -> f32 {
    if (nlsIsInf(x) || nlsIsNan(x)) { return nlsNaN(); }
    let r = x % 360.0;
    if (r % 180.0 == 0.0) { return x * 0.0; }
    if (abs(r) == 90.0) { return sign(r); }
    if (abs(r) == 270.0) { return -sign(r); }
    return nlsSin(r * NLS_D2R);
}
fn nlsCosd(x : f32) -> f32 {
    if (nlsIsInf(x) || nlsIsNan(x)) { return nlsNaN(); }
    let r = abs(x % 360.0);
    if (r == 90.0 || r == 270.0) { return 0.0; }
    if (r == 0.0) { return 1.0; }
    if (r == 180.0) { return -1.0; }
    return nlsCos(r * NLS_D2R);
}
fn nlsSinpi(x : f32) -> f32 {
    if (nlsIsInf(x) || nlsIsNan(x)) { return nlsNaN(); }
    let r = x - 2.0 * floor(x / 2.0);
    if (r == 0.0 || r == 1.0) { return 0.0; }
    if (r == 0.5) { return 1.0; }
    if (r == 1.5) { return -1.0; }
    return nlsSin(NLS_PI * r);
}
fn nlsCospi(x : f32) -> f32 {
    if (nlsIsInf(x) || nlsIsNan(x)) { return nlsNaN(); }
    let r = x - 2.0 * floor(x / 2.0);
    if (r == 0.5 || r == 1.5) { return 0.0; }
    if (r == 0.0) { return 1.0; }
    if (r == 1.0) { return -1.0; }
    return nlsCos(NLS_PI * r);
}
fn nlsLog1p(x : f32) -> f32 {
    let u = 1.0 + x;
    if (u == 1.0) { return x; }
    if (nlsIsInf(u)) { return u; }
    return log(u) * x / (u - 1.0);
}
fn nlsExpm1(x : f32) -> f32 {
    let u = exp(x);
    if (u == 1.0) { return x; }
    if (u - 1.0 == -1.0) { return -1.0; }
    if (nlsIsInf(u)) { return u; }
    return (u - 1.0) * x / log(u);
}
fn nlsSnapQuotient(q : f32, n : f32) -> f32 {
    let r = round(q);
    if (abs(q - r) <= 2.4e-7 * max(1.0, abs(q))) { return r; }
    return n;
}
fn nlsMod(x : f32, y : f32) -> f32 {
    if (y == 0.0) { return x; }
    if (nlsIsNan(x) || nlsIsNan(y) || nlsIsInf(x)) { return nlsNaN(); }
    if (nlsIsInf(y)) {
        if (x == 0.0 || sign(x) == sign(y)) { return x; }
        return y;
    }
    let q = x / y;
    let m = x - nlsSnapQuotient(q, floor(q)) * y;
    if (m != 0.0 && sign(m) != sign(y)) { return m + y; }
    return m;
}
fn nlsRem(x : f32, y : f32) -> f32 {
    if (nlsIsNan(x) || nlsIsNan(y) || nlsIsInf(x) || y == 0.0) { return nlsNaN(); }
    if (nlsIsInf(y)) { return x; }
    let q = x / y;
    return x - nlsSnapQuotient(q, trunc(q)) * y;
}
fn nlsErfcPositive(z : f32) -> f32 {
    let t = 1.0 / (1.0 + 0.5 * z);
    let p = -1.26551223 + t * (1.00002368 + t * (0.37409196 + t * (0.09678418 + t * (-0.18628806
        + t * (0.27886807 + t * (-1.13520398 + t * (1.48851587 + t * (-0.82215223 + t * 0.17087277))))))));
    return t * exp(-z * z + p);
}
fn nlsErfcxPositive(z : f32) -> f32 {
    let t = 1.0 / (1.0 + 0.5 * z);
    let p = -1.26551223 + t * (1.00002368 + t * (0.37409196 + t * (0.09678418 + t * (-0.18628806
        + t * (0.27886807 + t * (-1.13520398 + t * (1.48851587 + t * (-0.82215223 + t * 0.17087277))))))));
    return t * exp(p);
}
fn nlsErf(x : f32) -> f32 {
    if (nlsIsNan(x)) { return x; }
    if (abs(x) < 0.5) {
        let x2 = x * x;
        let s = 1.0 - x2 * (1.0 / 3.0 - x2 * (0.1 - x2 * (1.0 / 42.0 - x2 * (1.0 / 216.0
            - x2 * (1.0 / 1320.0)))));
        return 1.1283791670955126 * x * s;
    }
    return sign(x) * (1.0 - nlsErfcPositive(abs(x)));
}
fn nlsErfc(x : f32) -> f32 {
    if (nlsIsNan(x)) { return x; }
    if (abs(x) < 0.5) { return 1.0 - nlsErf(x); }
    if (x >= 0.0) { return nlsErfcPositive(x); }
    return 2.0 - nlsErfcPositive(-x);
}
fn nlsErfcx(x : f32) -> f32 {
    if (nlsIsNan(x)) { return x; }
    if (x >= 0.0) {
        if (nlsIsInf(x)) { return 0.0; }
        return nlsErfcxPositive(x);
    }
    return 2.0 * exp(x * x) - nlsErfcxPositive(-x);
}
fn nlsErfinvCore(w0 : f32, x : f32) -> f32 {
    var w = w0;
    var p : f32;
    if (w < 5.0) {
        w = w - 2.5;
        p = 2.81022636e-08;
        p = 3.43273939e-07 + p * w;
        p = -3.5233877e-06 + p * w;
        p = -4.39150654e-06 + p * w;
        p = 0.00021858087 + p * w;
        p = -0.00125372503 + p * w;
        p = -0.00417768164 + p * w;
        p = 0.246640727 + p * w;
        p = 1.50140941 + p * w;
    } else {
        w = sqrt(w) - 3.0;
        p = -0.000200214257;
        p = 0.000100950558 + p * w;
        p = 0.00134934322 + p * w;
        p = -0.00367342844 + p * w;
        p = 0.00573950773 + p * w;
        p = -0.0076224613 + p * w;
        p = 0.00943887047 + p * w;
        p = 1.00167406 + p * w;
        p = 2.83297682 + p * w;
    }
    return p * x;
}
fn nlsErfinv(x : f32) -> f32 {
    if (nlsIsNan(x) || abs(x) > 1.0) { return nlsNaN(); }
    if (abs(x) == 1.0) { return x * nlsInf(); }
    return nlsErfinvCore(-log((1.0 - x) * (1.0 + x)), x);
}
fn nlsErfcinv(y : f32) -> f32 {
    if (nlsIsNan(y) || y < 0.0 || y > 2.0) { return nlsNaN(); }
    if (y == 0.0) { return nlsInf(); }
    if (y == 2.0) { return -nlsInf(); }
    return nlsErfinvCore(-log(y * (2.0 - y)), 1.0 - y);
}
fn nlsLogGammaPositive(x : f32) -> f32 {
    // Lanczos series (g = 5)
    let t = x + 5.5;
    let s = 1.000000000190015 + 76.18009172947146 / (x + 1.0) - 86.50532032941677 / (x + 2.0)
        + 24.01409824083091 / (x + 3.0) - 1.231739572450155 / (x + 4.0)
        + 0.001208650973866179 / (x + 5.0) - 0.000005395239384953 / (x + 6.0);
    return (x + 0.5) * log(t) - t + log(2.5066282746310005 * s / x);
}
fn nlsGamma(x : f32) -> f32 {
    if (nlsIsNan(x)) { return x; }
    if (x == 0.0 || nlsIsInf(x)) { return nlsInf(); }
    if (x < 0.0 && floor(x) == x) { return nlsInf(); }
    if (x > 35.04) { return nlsInf(); }
    if (x >= 0.5) {
        if (x <= 13.0 && floor(x) == x) {
            var f : f32 = 1.0;
            for (var k : f32 = 2.0; k < x; k = k + 1.0) { f = f * k; }
            return f;
        }
        return exp(nlsLogGammaPositive(x));
    }
    // reflection: gamma(x) gamma(1 - x) = pi / sin(pi x)
    return NLS_PI / (nlsSinpi(x) * exp(nlsLogGammaPositive(1.0 - x)));
}
fn nlsGammaln(x : f32) -> f32 {
    if (nlsIsNan(x)) { return x; }
    if (x == 0.0 || nlsIsInf(x)) { return nlsInf(); }
    return nlsLogGammaPositive(x);
}
fn nlsFactorial(n : f32) -> f32 {
    if (n > 34.0) { return nlsInf(); }
    var f : f32 = 1.0;
    for (var k : f32 = 2.0; k <= n; k = k + 1.0) { f = f * k; }
    return f;
}
fn nlsNextpow2(x : f32) -> f32 {
    if (nlsIsNan(x) || nlsIsInf(x)) { return abs(x); }
    if (x == 0.0) { return 0.0; }
    let parts = frexp(abs(x));
    if (parts.fract == 0.5) { return f32(parts.exp - 1); }
    return f32(parts.exp);
}
// r^n for a whole number n by repeated squaring (exact where pow is not)
fn nlsIpow(r : f32, n : f32) -> f32 {
    var e = u32(abs(n));
    var b = r;
    var p : f32 = 1.0;
    loop {
        if ((e & 1u) != 0u) { p = p * b; }
        e = e >> 1u;
        if (e == 0u) { break; }
        b = b * b;
    }
    return select(p, 1.0 / p, n < 0.0);
}
fn nlsNthroot(x : f32, n : f32) -> f32 {
    if (x == 0.0 || nlsIsInf(x) || nlsIsNan(x)) { return select(pow(abs(x), 1.0 / n), x, n > 0.0); }
    let ax = abs(x);
    var r = pow(ax, 1.0 / n);
    // one Newton step on r^n = |x|
    if (r != 0.0 && !nlsIsInf(r)) {
        r = r - (nlsIpow(r, n) - ax) / (n * nlsIpow(r, n - 1.0));
    }
    return sign(x) * r;
}
// real power: whole exponents by repeated squaring, the rest through pow on |x|
fn nlsPower(x : f32, y : f32) -> f32 {
    if (x == 1.0 || y == 0.0) { return 1.0; }
    if (nlsIsInt(y) && abs(y) <= 1048576.0) { return nlsIpow(x, y); }
    if (nlsIsInf(y) && abs(x) == 1.0) { return 1.0; }
    if (x < 0.0) { return pow(-x, y); }
    return pow(x, y);
}
fn nlsAsinC(x : f32) -> vec2<f32> {
    if (nlsIsNan(x)) { return vec2<f32>(x, 0.0); }
    if (x > 1.0) { return vec2<f32>(NLS_PI / 2.0, -log(x + sqrt(x * x - 1.0))); }
    if (x < -1.0) { return vec2<f32>(-NLS_PI / 2.0, log(-x + sqrt(x * x - 1.0))); }
    return vec2<f32>(nlsAsin(x), 0.0);
}
fn nlsAcosC(x : f32) -> vec2<f32> {
    if (nlsIsNan(x)) { return vec2<f32>(x, 0.0); }
    if (x > 1.0) { return vec2<f32>(0.0, log(x + sqrt(x * x - 1.0))); }
    if (x < -1.0) { return vec2<f32>(NLS_PI, -log(-x + sqrt(x * x - 1.0))); }
    return vec2<f32>(nlsAcos(x), 0.0);
}
fn nlsAcoshC(x : f32) -> vec2<f32> {
    if (nlsIsNan(x)) { return vec2<f32>(x, 0.0); }
    if (x >= 1.0) { return vec2<f32>(nlsAcosh(x), 0.0); }
    if (x >= -1.0) { return vec2<f32>(0.0, nlsAcos(x)); }
    return vec2<f32>(log(-x + sqrt(x * x - 1.0)), NLS_PI);
}
fn nlsAtanhC(x : f32) -> vec2<f32> {
    if (nlsIsNan(x)) { return vec2<f32>(x, 0.0); }
    if (abs(x) <= 1.0) { return vec2<f32>(nlsAtanh(x), 0.0); }
    // the imaginary part takes the sign of x
    var re : f32 = 0.0;
    if (!nlsIsInf(x)) { re = 0.5 * log(abs((1.0 + x) / (1.0 - x))); }
    return vec2<f32>(re, nlsCopySign(NLS_PI / 2.0, x));
}
fn nlsLogC(x : f32) -> vec2<f32> {
    if (nlsIsNan(x)) { return vec2<f32>(x, 0.0); }
    if (x < 0.0) { return vec2<f32>(log(-x), NLS_PI); }
    return vec2<f32>(log(x), 0.0);
}
// continued fraction of the incomplete beta (modified Lentz)
fn nlsBetaCf(x : f32, a : f32, b : f32) -> f32 {
    let qab = a + b;
    let qap = a + 1.0;
    let qam = a - 1.0;
    var c : f32 = 1.0;
    var d = 1.0 - qab * x / qap;
    if (abs(d) < 1e-30) { d = 1e-30; }
    d = 1.0 / d;
    var h = d;
    for (var m = 1; m < 1000; m = m + 1) {
        let fm = f32(m);
        let m2 = 2.0 * fm;
        var aa = fm * (b - fm) * x / ((qam + m2) * (a + m2));
        d = 1.0 + aa * d;
        if (abs(d) < 1e-30) { d = 1e-30; }
        c = 1.0 + aa / c;
        if (abs(c) < 1e-30) { c = 1e-30; }
        d = 1.0 / d;
        h = h * d * c;
        aa = -(a + fm) * (qab + fm) * x / ((a + m2) * (qap + m2));
        d = 1.0 + aa * d;
        if (abs(d) < 1e-30) { d = 1e-30; }
        c = 1.0 + aa / c;
        if (abs(c) < 1e-30) { c = 1e-30; }
        d = 1.0 / d;
        let step = d * c;
        h = h * step;
        if (abs(step - 1.0) < 3e-8) { break; }
    }
    return h;
}
// regularized incomplete beta (lower, upper) for 0 <= x <= 1 and a, b > 0
fn nlsBetaincPQ(x : f32, a : f32, b : f32) -> vec2<f32> {
    if (nlsIsNan(x) || nlsIsNan(a) || nlsIsNan(b)) { return vec2<f32>(nlsNaN(), nlsNaN()); }
    if (x <= 0.0) { return vec2<f32>(0.0, 1.0); }
    if (x >= 1.0) { return vec2<f32>(1.0, 0.0); }
    let front = exp(nlsGammaln(a + b) - nlsGammaln(a) - nlsGammaln(b) + a * log(x) + b * nlsLog1p(-x));
    // each tail from its own fraction when it is the small one, so 1 - t never cancels
    if (x < (a + 1.0) / (a + b + 2.0)) {
        let lower = front * nlsBetaCf(x, a, b) / a;
        if (lower <= 0.5) { return vec2<f32>(lower, 1.0 - lower); }
        let upper = front * nlsBetaCf(1.0 - x, b, a) / b;
        return vec2<f32>(1.0 - upper, upper);
    }
    let upper = front * nlsBetaCf(1.0 - x, b, a) / b;
    if (upper <= 0.5) { return vec2<f32>(1.0 - upper, upper); }
    let lower = front * nlsBetaCf(x, a, b) / a;
    return vec2<f32>(lower, 1.0 - lower);
}
// log(gamma(1 + a)) for 0 <= a < 0.2 by its Taylor series (zeta values), accurate near a = 0
fn nlsLgamma1pSmall(a : f32) -> f32 {
    let c = array<f32, 9>(-0.5772156649, 0.8224670334, -0.4006856344, 0.2705808084,
        -0.2073855510, 0.1695571770, -0.1440498968, 0.1255096685, -0.1113342659);
    var power = a;
    var sum : f32 = 0.0;
    for (var k = 0; k < 9; k = k + 1) {
        sum = sum + c[k] * power;
        power = power * a;
    }
    return sum;
}
// regularized incomplete gamma (lower P, upper Q) for x >= 0 and a >= 0: series below a + 1,
// continued fraction above, each tail computed on its own so the small one keeps its digits
fn nlsGammaincPQ(x : f32, a : f32) -> vec2<f32> {
    if (nlsIsNan(x) || nlsIsNan(a)) { return vec2<f32>(nlsNaN(), nlsNaN()); }
    if (a == 0.0 || nlsIsInf(x)) { return vec2<f32>(1.0, 0.0); }
    if (x == 0.0 || nlsIsInf(a)) { return vec2<f32>(0.0, 1.0); }
    let lead = exp(a * log(x) - x - nlsGammaln(a));
    if (x < a + 1.0) {
        var ap = a;
        var term = 1.0 / a;
        var sum = term;
        for (var n = 0; n < 2000; n = n + 1) {
            ap = ap + 1.0;
            term = term * x / ap;
            sum = sum + term;
            if (abs(term) < abs(sum) * 3e-8) { break; }
        }
        let lower = min(sum * lead, 1.0);
        if (a < 1.0) {
            // small a: the upper tail directly, as 1 - x^a / gamma(a + 1) plus a short series
            let lx = a * log(x) - select(nlsGammaln(a + 1.0), nlsLgamma1pSmall(a), a < 0.2);
            var t : f32 = 0.0;
            var power : f32 = 1.0;
            for (var n = 1; n < 200; n = n + 1) {
                power = -power * x / f32(n);
                let next = -power / (a + f32(n));
                t = t + next;
                if (abs(next) < abs(t) * 3e-8) { break; }
            }
            let upper = -nlsExpm1(lx) + exp(lx) * a * t;
            return vec2<f32>(lower, upper);
        }
        return vec2<f32>(lower, 1.0 - lower);
    }
    var b = x + 1.0 - a;
    var c : f32 = 1e30;
    var d = 1.0 / b;
    var h = d;
    for (var i = 1; i < 2000; i = i + 1) {
        let an = -f32(i) * (f32(i) - a);
        b = b + 2.0;
        d = an * d + b;
        if (abs(d) < 1e-30) { d = 1e-30; }
        c = b + an / c;
        if (abs(c) < 1e-30) { c = 1e-30; }
        d = 1.0 / d;
        let step = d * c;
        h = h * step;
        if (abs(step - 1.0) < 3e-8) { break; }
    }
    let upper = min(lead * h, 1.0);
    return vec2<f32>(1.0 - upper, upper);
}
)WGSL";
//=============================================================================
const std::string&
GpuMathHelpersWgsl()
{
    static const std::string helpers(MATH_HELPERS_WGSL);
    return helpers;
}
//=============================================================================
// how a real function behaves; expressions use `x` (and `y` for two inputs)
struct MathEntry
{
    int arity;
    // real result for a real input
    std::string realExpr;
    // true for an element whose result is complex (the whole result becomes complex)
    std::string complexWhen;
    // vec2<f32> (re, im) result for a real input, used when complexWhen holds somewhere
    std::string complexExpr;
    // true for an element the host rejects with an error: the host then runs the call
    std::string hostWhen;
};
//=============================================================================
static const std::map<std::string, MathEntry>&
mathTable()
{
    static const std::map<std::string, MathEntry> table = {
        { "sqrt",
            { 1, "sqrt(x)", "x < 0.0",
                "select(vec2<f32>(sqrt(x), 0.0), vec2<f32>(0.0, sqrt(-x)), x < 0.0)", "" } },
        { "realsqrt", { 1, "sqrt(x)", "", "", "x < 0.0" } },
        { "log", { 1, "log(x)", "x < 0.0", "nlsLogC(x)", "" } },
        { "reallog", { 1, "log(x)", "", "", "x < 0.0" } },
        { "log2", { 1, "log2(x)", "x < 0.0", "nlsLogC(x) / log(2.0)", "" } },
        { "log10", { 1, "log(x) / log(10.0)", "x < 0.0", "nlsLogC(x) / log(10.0)", "" } },
        { "log1p",
            { 1, "nlsLog1p(x)", "x < -1.0",
                "select(vec2<f32>(nlsLog1p(x), 0.0), vec2<f32>(log(-(1.0 + x)), NLS_PI), x < -1.0)",
                "" } },
        { "expm1", { 1, "nlsExpm1(x)", "", "", "" } },
        { "asin", { 1, "nlsAsin(x)", "abs(x) > 1.0", "nlsAsinC(x)", "" } },
        { "acos", { 1, "nlsAcos(x)", "abs(x) > 1.0", "nlsAcosC(x)", "" } },
        { "acosh", { 1, "nlsAcosh(x)", "x < 1.0", "nlsAcoshC(x)", "" } },
        { "atanh", { 1, "nlsAtanh(x)", "abs(x) > 1.0", "nlsAtanhC(x)", "" } },
        { "asec", { 1, "nlsAcos(1.0 / x)", "abs(x) < 1.0", "nlsAcosC(1.0 / x)", "" } },
        { "acsc", { 1, "nlsAsin(1.0 / x)", "abs(x) < 1.0", "nlsAsinC(1.0 / x)", "" } },
        { "asech", { 1, "nlsAcosh(1.0 / x)", "x < 0.0 || x > 1.0", "nlsAcoshC(1.0 / x)", "" } },
        { "acoth", { 1, "nlsAtanh(1.0 / x)", "abs(x) < 1.0", "nlsAtanhC(1.0 / x)", "" } },
        { "acsch", { 1, "nlsAsinh(1.0 / x)", "", "", "" } },
        { "asind", { 1, "nlsAsin(x) * NLS_R2D", "abs(x) > 1.0", "nlsAsinC(x) * NLS_R2D", "" } },
        { "acosd", { 1, "nlsAcos(x) * NLS_R2D", "abs(x) > 1.0", "nlsAcosC(x) * NLS_R2D", "" } },
        { "asecd",
            { 1, "nlsAcos(1.0 / x) * NLS_R2D", "abs(x) < 1.0", "nlsAcosC(1.0 / x) * NLS_R2D",
                "" } },
        { "acscd",
            { 1, "nlsAsin(1.0 / x) * NLS_R2D", "abs(x) < 1.0", "nlsAsinC(1.0 / x) * NLS_R2D",
                "" } },
        { "atand", { 1, "nlsAtan(x) * NLS_R2D", "", "", "" } },
        { "acotd", { 1, "nlsAtan(1.0 / x) * NLS_R2D", "", "", "" } },
        { "sind", { 1, "nlsSind(x)", "", "", "" } },
        { "cosd", { 1, "nlsCosd(x)", "", "", "" } },
        { "tand", { 1, "nlsSind(x) / nlsCosd(x)", "", "", "" } },
        { "secd", { 1, "1.0 / nlsCosd(x)", "", "", "" } },
        { "cscd", { 1, "1.0 / nlsSind(x)", "", "", "" } },
        { "cotd", { 1, "nlsCosd(x) / nlsSind(x)", "", "", "" } },
        { "sinpi", { 1, "nlsSinpi(x)", "", "", "" } },
        { "cospi", { 1, "nlsCospi(x)", "", "", "" } },
        { "erf", { 1, "nlsErf(x)", "", "", "" } },
        { "erfc", { 1, "nlsErfc(x)", "", "", "" } },
        { "erfcx", { 1, "nlsErfcx(x)", "", "", "" } },
        { "erfinv", { 1, "nlsErfinv(x)", "", "", "" } },
        { "erfcinv", { 1, "nlsErfcinv(x)", "", "", "" } },
        { "gamma", { 1, "nlsGamma(x)", "", "", "" } },
        { "gammaln", { 1, "nlsGammaln(x)", "", "", "x < 0.0" } },
        { "factorial",
            { 1, "nlsFactorial(x)", "", "", "x < 0.0 || (!nlsIsInt(x) && !nlsIsInf(x))" } },
        { "nextpow2", { 1, "nlsNextpow2(x)", "", "", "" } },
        { "atan2d", { 2, "nlsAtan2(x, y) * NLS_R2D", "", "", "" } },
        { "beta",
            { 2, "exp(nlsGammaln(x) + nlsGammaln(y) - nlsGammaln(x + y))", "", "",
                "x < 0.0 || y < 0.0" } },
        { "betaln",
            { 2, "nlsGammaln(x) + nlsGammaln(y) - nlsGammaln(x + y)", "", "",
                "x < 0.0 || y < 0.0" } },
        { "nthroot",
            { 2, "nlsNthroot(x, y)", "", "",
                "!nlsIsInt(y) || y == 0.0 || (x < 0.0 && abs(y % 2.0) != 1.0)" } },
        { "realpow", { 2, "nlsPower(x, y)", "", "", "x < 0.0 && !nlsIsInt(y) && !nlsIsInf(y)" } },
        { "power:wholeexponent", { 2, "nlsPower(x, y)", "", "", "" } },
        { "power",
            { 2, "nlsPower(x, y)", "x < 0.0 && !nlsIsInt(y) && !nlsIsInf(y)",
                "select(vec2<f32>(nlsPower(x, y), 0.0), pow(-x, y) * vec2<f32>(nlsCospi(y), "
                "nlsSinpi(y)), x < 0.0 && !nlsIsInt(y) && !nlsIsInf(y))",
                "" } },
        { "mod", { 2, "nlsMod(x, y)", "", "", "" } },
        { "sin", { 1, "nlsSin(x)", "", "", "" } },
        { "cos", { 1, "nlsCos(x)", "", "", "" } },
        { "tan", { 1, "nlsTan(x)", "", "", "" } },
        { "sec", { 1, "1.0 / nlsCos(x)", "", "", "" } },
        { "csc", { 1, "1.0 / nlsSin(x)", "", "", "" } },
        { "cot", { 1, "1.0 / nlsTan(x)", "", "", "" } },
        { "atan", { 1, "nlsAtan(x)", "", "", "" } },
        { "acot", { 1, "nlsAtan(1.0 / x)", "", "", "" } },
        { "sinh", { 1, "nlsSinh(x)", "", "", "" } },
        { "cosh", { 1, "nlsCosh(x)", "", "", "" } },
        { "tanh", { 1, "nlsTanh(x)", "", "", "" } },
        { "sech", { 1, "1.0 / nlsCosh(x)", "", "", "" } },
        { "csch", { 1, "1.0 / nlsSinh(x)", "", "", "" } },
        { "coth", { 1, "1.0 / nlsTanh(x)", "", "", "" } },
        { "asinh", { 1, "nlsAsinh(x)", "", "", "" } },
        { "atan2", { 2, "nlsAtan2(x, y)", "", "", "" } },
        { "hypot", { 2, "nlsHypot(x, y)", "", "", "" } },
        { "gammainc", { 2, "nlsGammaincPQ(x, y).x", "", "", "x < 0.0 || y < 0.0 || y > 100.0" } },
        { "gammainc:upper",
            { 2, "nlsGammaincPQ(x, y).y", "", "", "x < 0.0 || y < 0.0 || y > 100.0" } },
        { "rem", { 2, "nlsRem(x, y)", "", "", "" } },
    };
    return table;
}
//=============================================================================
// WGSL reading x (and y) for output element i
static std::string
kernelHead(int arity, bool expand, const std::string& outputs)
{
    std::string s = std::string(MATH_HELPERS_WGSL);
    if (arity == 1) {
        s += "struct Params { n : u32 };\n"
             "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
             "@group(0) @binding(1) var<storage, read_write> "
            + outputs
            + ";\n"
              "@group(0) @binding(2) var<uniform> params : Params;\n"
              "@compute @workgroup_size(256)\n"
              "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
              "    let i = gid.x;\n"
              "    if (i >= params.n) { return; }\n"
              "    let x = a[i];\n";
        return s;
    }
    s += GpuBinaryParamsWgsl(expand)
        + "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
          "@group(0) @binding(1) var<storage, read> b : array<f32>;\n"
          "@group(0) @binding(2) var<storage, read_write> "
        + outputs
        + ";\n"
          "@group(0) @binding(3) var<uniform> params : Params;\n"
          "@compute @workgroup_size(256)\n"
          "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
          "    let i = gid.x;\n"
          "    if (i >= params.n) { return; }\n"
          "    let ix = operandIndex(i);\n"
          "    let x = a[ix.x];\n"
          "    let y = b[ix.y];\n";
    return s;
}
//=============================================================================
// one kernel over the operands; `body` writes the output for element i
static void
runKernel(const std::string& key, const std::string& outputs, const std::string& body,
    const std::vector<GpuArrayObject*>& inputs, const GpuBinaryGeometry& geometry,
    WGPUBuffer output)
{
    bool binary = (inputs.size() == 2);
    std::string source = kernelHead((int)inputs.size(), geometry.expand, outputs) + body + "}\n";
    std::vector<WGPUBuffer> buffers;
    for (GpuArrayObject* input : inputs) {
        buffers.push_back(input->buffer());
    }
    buffers.push_back(output);
    if (binary) {
        GpuRunBinaryKernel(key, source, geometry, buffers);
        return;
    }
    uint32_t n = geometry.params[0];
    if (n == 0) {
        return;
    }
    WGPUComputePipeline pipeline
        = GpuContext::getInstance()->pipelineFor(key, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    uint32_t params[1] = { n };
    GpuDispatchKernel(pipeline, buffers, params, 1, (n + 255u) / 256u, 1);
}
//=============================================================================
// true when `predicate` holds for some element
static bool
anyElement(const std::string& key, const std::string& predicate,
    const std::vector<GpuArrayObject*>& inputs, const GpuBinaryGeometry& geometry)
{
    if (geometry.params[0] == 0) {
        return false;
    }
    GpuArrayObject* flag = AllocateGpuArray(Dimensions(1, 1), "uint32", false);
    ArrayOf owner = ArrayOf::handle(flag);
    runKernel(key, "flag : array<atomic<u32>>",
        "    if (" + predicate + ") { atomicOr(&flag[0], 1u); }\n", inputs, geometry,
        flag->buffer());
    ArrayOf value = GatherGpuArray(flag);
    return ((const uint32_t*)value.dataPointer())[0] != 0;
}
//=============================================================================
bool
GpuElementwiseMath(const std::string& name, const ArrayOfVector& args, ArrayOf& result)
{
    // a real host operand is copied to the device (usually a scalar)
    std::vector<ArrayOf> owners;
    std::vector<GpuArrayObject*> operands;
    for (const ArrayOf& arg : args) {
        GpuArrayObject* gpuObject = AsGpuArrayObject(arg);
        if (gpuObject == nullptr) {
            NelsonType type = arg.dataClass();
            if (arg.isSparse() || arg.isComplex()
                || (type != NLS_DOUBLE && type != NLS_SINGLE && type != NLS_LOGICAL)) {
                return false;
            }
            owners.push_back(CreateGpuArray(arg));
            gpuObject = AsGpuArrayObject(owners.back());
        }
        operands.push_back(gpuObject);
    }
    return GpuElementwiseMath(name, operands, result);
}
//=============================================================================
bool
GpuElementwiseMath(
    const std::string& name, const std::vector<GpuArrayObject*>& operands, ArrayOf& result)
{
    auto found = mathTable().find(name);
    if (found == mathTable().end() || (int)operands.size() != found->second.arity) {
        return false;
    }
    const MathEntry& entry = found->second;
    // operands: real single or logical arrays
    std::vector<GpuArrayObject*> inputs;
    for (GpuArrayObject* gpuObject : operands) {
        std::string type = gpuObject->underlyingType();
        if (gpuObject->isComplex() || (type != "single" && type != "logical")) {
            if (operands.size() == 1 && gpuObject->isComplex()) {
                static const std::map<std::string, ArrayOf (*)(GpuArrayObject*)> complexKernels
                    = { { "sqrt", GpuComplexSqrt }, { "log", GpuComplexLog },
                          { "sin", GpuComplexSin }, { "cos", GpuComplexCos },
                          { "tan", GpuComplexTan } };
                auto kernel = complexKernels.find(name);
                if (kernel != complexKernels.end()) {
                    result = kernel->second(gpuObject);
                    return true;
                }
            }
            return false;
        }
        inputs.push_back(gpuObject);
    }
    GpuBinaryGeometry geometry;
    if (inputs.size() == 2) {
        geometry = GpuComputeBinaryGeometry(inputs[0]->dimensions(), inputs[1]->dimensions());
    } else {
        geometry.outputDims = inputs[0]->dimensions();
        geometry.params = { (uint32_t)geometry.outputDims.elementCount() };
    }
    std::string key = "gpuArray:math:" + name;
    if (!entry.hostWhen.empty() && anyElement(key + ":host", entry.hostWhen, inputs, geometry)) {
        return false;
    }
    bool complexResult = !entry.complexWhen.empty()
        && anyElement(key + ":domain", entry.complexWhen, inputs, geometry);
    GpuArrayObject* output = AllocateGpuArray(geometry.outputDims, "single", complexResult);
    result = ArrayOf::handle(output);
    if (complexResult) {
        runKernel(key + ":complex", "c : array<f32>",
            "    let z = " + entry.complexExpr
                + ";\n    c[2u * i] = z.x;\n    c[2u * i + 1u] = z.y;\n",
            inputs, geometry, output->buffer());
    } else {
        runKernel(key, "c : array<f32>", "    c[i] = " + entry.realExpr + ";\n", inputs, geometry,
            output->buffer());
    }
    return true;
}
//=============================================================================
// x, a, b of one size (or scalars); a flag asks for the host outside 0 <= x <= 1, 0 < a, b <= 100
static const char* BETAINC_WGSL = R"WGSL(
struct Params { n : u32, nx : u32, na : u32, nb : u32, upper : u32, pad0 : u32, pad1 : u32, pad2 : u32 };
@group(0) @binding(0) var<storage, read> xs : array<f32>;
@group(0) @binding(1) var<storage, read> av : array<f32>;
@group(0) @binding(2) var<storage, read> bv : array<f32>;
@group(0) @binding(3) var<storage, read_write> out : array<f32>;
@group(0) @binding(4) var<storage, read_write> flag : array<atomic<u32>>;
@group(0) @binding(5) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let i = gid.x;
    if (i >= params.n) { return; }
    let x = xs[select(i, 0u, params.nx == 1u)];
    let a = av[select(i, 0u, params.na == 1u)];
    let b = bv[select(i, 0u, params.nb == 1u)];
    if (nlsIsNan(x) || nlsIsNan(a) || nlsIsNan(b) || x < 0.0 || x > 1.0 || a <= 0.0 || b <= 0.0 || a > 100.0 || b > 100.0) {
        atomicOr(&flag[0], 1u);
    }
    let r = nlsBetaincPQ(x, a, b);
    out[i] = select(r.x, r.y, params.upper == 1u);
}
)WGSL";
//=============================================================================
bool
GpuBetainc(GpuArrayObject* x, GpuArrayObject* a, GpuArrayObject* b, bool upper, ArrayOf& result)
{
    std::vector<GpuArrayObject*> operands = { x, a, b };
    Dimensions dims(1, 1);
    for (GpuArrayObject* g : operands) {
        if (g->isComplex() || g->isRawIntStorage() || !GpuIntegerResultType(g, g).empty()
            || g->isLogical()) {
            return false;
        }
        if (g->elementCount() != 1) {
            if (dims.elementCount() != 1 && !dims.equals(g->dimensions())) {
                return false;
            }
            dims = g->dimensions();
        }
    }
    GpuArrayObject* output = AllocateGpuArray(dims, "single", false);
    ArrayOf owner = ArrayOf::handle(output);
    uint32_t n = (uint32_t)dims.elementCount();
    if (n == 0) {
        result = owner;
        return true;
    }
    GpuArrayObject* flag = AllocateGpuArray(Dimensions(1, 1), "uint32", false);
    ArrayOf flagOwner = ArrayOf::handle(flag);
    std::string source = GpuMathHelpersWgsl() + BETAINC_WGSL;
    WGPUComputePipeline pipeline
        = GpuContext::getInstance()->pipelineFor("gpuArray:betainc", source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    uint32_t params[8] = { n, (uint32_t)x->elementCount(), (uint32_t)a->elementCount(),
        (uint32_t)b->elementCount(), upper ? 1u : 0u, 0u, 0u, 0u };
    GpuDispatchKernel(pipeline,
        { x->buffer(), a->buffer(), b->buffer(), output->buffer(), flag->buffer() }, params, 8,
        (n + 255u) / 256u, 1);
    ArrayOf flagValue = GatherGpuArray(flag);
    if (((const uint32_t*)flagValue.dataPointer())[0] != 0) {
        return false;
    }
    result = owner;
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
