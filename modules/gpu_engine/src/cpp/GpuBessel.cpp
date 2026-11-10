//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GpuBessel.hpp"
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
// Bessel and Airy functions of real arguments in single precision.
// Temme series for x < 2, Steed continued fractions above, Hankel expansion for large x.
static const char* BESSEL_WGSL = R"WGSL(
const NLS_BESSEL_EPS : f32 = 6e-8;
const NLS_BESSEL_TINY : f32 = 1e-30;
const NLS_BESSEL_BIG : f32 = 1e30;
// 1/Gamma(1 + mu) and 1/Gamma(1 - mu) terms for the Temme series: (g1, g2, 1/Gamma(1+mu), 1/Gamma(1-mu))
fn nlsBesselGammaTerms(mu : f32) -> vec4<f32> {
    var c = array<f32, 17>(1.0, 0.57721566490153286, -0.65587807152025388, -0.042002635034095236,
        0.16653861138229149, -0.042197734555544337, -0.0096219715278769736, 0.0072189432466630995,
        -0.0011651675918590651, -0.00021524167411495097, 0.00012805028238811619,
        -2.0134854780788239e-5, -1.2504934821426707e-6, 1.1330272319816959e-6,
        -2.0563384169776071e-7, 6.1160951044814158e-9, 5.0020076444692229e-9);
    let m2 = mu * mu;
    var even : f32 = 0.0;
    var odd : f32 = 0.0;
    for (var k : i32 = 8; k >= 0; k = k - 1) {
        even = even * m2 + c[2 * k];
        if (k < 8) { odd = odd * m2 + c[2 * k + 1]; }
    }
    return vec4<f32>(-odd, even, even + mu * odd, even - mu * odd);
}
// Hankel expansion: (P, Q) for order nu at large x
fn nlsBesselPQ(nu : f32, x : f32) -> vec2<f32> {
    let mu4 = 4.0 * nu * nu;
    let y8 = 8.0 * x;
    var p : f32 = 1.0;
    var q : f32 = 0.0;
    var term : f32 = 1.0;
    for (var k : i32 = 1; k < 200; k = k + 1) {
        let odd = f32(2 * k - 1);
        term = term * (mu4 - odd * odd) / (f32(k) * y8);
        let signed = select(term, -term, (k & 2) != 0);
        if ((k & 1) == 1) { q = q + signed; } else { p = p + signed; }
        if (abs(term) < NLS_BESSEL_EPS) { break; }
    }
    return vec2<f32>(p, q);
}
// (J, Y) of order nu >= 0 at 0 < x
fn nlsBesselJY(nu : f32, x : f32) -> vec2<f32> {
    let xi = 1.0 / x;
    let xi2 = 2.0 * xi;
    if (x >= max(30.0, 0.125 * nu * nu)) {
        let pq = nlsBesselPQ(nu, x);
        let phase = 0.5 * nu + 0.25;
        let cp = nlsCospi(phase);
        let sp = nlsSinpi(phase);
        let sc = nlsSinCos(x);
        let cchi = sc.y * cp + sc.x * sp;
        let schi = sc.x * cp - sc.y * sp;
        let a = sqrt(2.0 / (NLS_PI * x));
        return vec2<f32>(a * (pq.x * cchi - pq.y * schi), a * (pq.x * schi + pq.y * cchi));
    }
    var nl : i32 = 0;
    if (x < 2.0) { nl = i32(nu + 0.5); } else { nl = max(0, i32(nu - x + 1.5)); }
    let mu = nu - f32(nl);
    let m2 = mu * mu;
    // continued fraction for J'/J at nu, the sign of J follows the denominators
    var h = max(nu * xi, NLS_BESSEL_TINY);
    var d : f32 = 0.0;
    var c = h;
    var sgn : f32 = 1.0;
    for (var i : i32 = 0; i < 20000; i = i + 1) {
        let b = xi2 * (nu + f32(i + 1));
        d = b - d;
        if (abs(d) < NLS_BESSEL_TINY) { d = NLS_BESSEL_TINY; }
        c = b - 1.0 / c;
        if (abs(c) < NLS_BESSEL_TINY) { c = NLS_BESSEL_TINY; }
        d = 1.0 / d;
        let del = c * d;
        h = h * del;
        if (d < 0.0) { sgn = -sgn; }
        if (abs(del - 1.0) < NLS_BESSEL_EPS) { break; }
    }
    // downward recurrence from nu to mu, rescaled to stay in range
    var jl = sgn * NLS_BESSEL_TINY;
    var jpl = h * jl;
    var jl1 = jl;
    for (var l : i32 = nl; l >= 1; l = l - 1) {
        let fact = (mu + f32(l)) * xi;
        let t = fact * jl + jpl;
        jpl = (fact - xi) * t - jl;
        jl = t;
        if (abs(jl) > NLS_BESSEL_BIG) {
            jl = jl * 1e-30;
            jpl = jpl * 1e-30;
            jl1 = jl1 * 1e-30;
        }
    }
    if (jl == 0.0) { jl = NLS_BESSEL_EPS; }
    let f = jpl / jl;
    let w = xi2 / NLS_PI;
    var jmu : f32;
    var ymu : f32;
    var y1 : f32;
    if (x < 2.0) {
        let x2 = 0.5 * x;
        let pimu = NLS_PI * mu;
        let fct = select(pimu / nlsSin(pimu), 1.0, abs(pimu) < NLS_BESSEL_EPS);
        var dd = -log(x2);
        var e = mu * dd;
        let fct2 = select(nlsSinh(e) / e, 1.0, abs(e) < NLS_BESSEL_EPS);
        let g = nlsBesselGammaTerms(mu);
        var ff = 2.0 / NLS_PI * fct * (g.x * nlsCosh(e) + g.y * fct2 * dd);
        e = exp(e);
        var p = e / (g.z * NLS_PI);
        var q = 1.0 / (e * NLS_PI * g.w);
        let pimu2 = 0.5 * pimu;
        let fct3 = select(nlsSin(pimu2) / pimu2, 1.0, abs(pimu2) < NLS_BESSEL_EPS);
        let r = NLS_PI * pimu2 * fct3 * fct3;
        var cc : f32 = 1.0;
        dd = -x2 * x2;
        var sum = ff + r * q;
        var sum1 = p;
        for (var i : i32 = 1; i < 1000; i = i + 1) {
            let fi = f32(i);
            ff = (fi * ff + p + q) / (fi * fi - m2);
            cc = cc * dd / fi;
            p = p / (fi - mu);
            q = q / (fi + mu);
            let del = cc * (ff + r * q);
            sum = sum + del;
            sum1 = sum1 + cc * p - fi * del;
            if (abs(del) < (1.0 + abs(sum)) * NLS_BESSEL_EPS) { break; }
        }
        ymu = -sum;
        y1 = -sum1 * xi2;
        let ymup = mu * xi * ymu - y1;
        jmu = w / (ymup - f * ymu);
    } else {
        // Steed continued fraction for p + iq
        var a = 0.25 - m2;
        var p = -0.5 * xi;
        var q : f32 = 1.0;
        let br = 2.0 * x;
        var bi : f32 = 2.0;
        var fct = a * xi / (p * p + q * q);
        var cr = br + q * fct;
        var ci = bi + p * fct;
        var den = br * br + bi * bi;
        var dr = br / den;
        var di = -bi / den;
        var dlr = cr * dr - ci * di;
        var dli = cr * di + ci * dr;
        var t = p * dlr - q * dli;
        q = p * dli + q * dlr;
        p = t;
        for (var i : i32 = 1; i < 20000; i = i + 1) {
            a = a + f32(2 * i);
            bi = bi + 2.0;
            dr = a * dr + br;
            di = a * di + bi;
            if (abs(dr) + abs(di) < NLS_BESSEL_TINY) { dr = NLS_BESSEL_TINY; }
            fct = a / (cr * cr + ci * ci);
            cr = br + cr * fct;
            ci = bi - ci * fct;
            if (abs(cr) + abs(ci) < NLS_BESSEL_TINY) { cr = NLS_BESSEL_TINY; }
            den = dr * dr + di * di;
            dr = dr / den;
            di = -di / den;
            dlr = cr * dr - ci * di;
            dli = cr * di + ci * dr;
            t = p * dlr - q * dli;
            q = p * dli + q * dlr;
            p = t;
            if (abs(dlr - 1.0) + abs(dli) < NLS_BESSEL_EPS) { break; }
        }
        let gammaRatio = (p - f) / q;
        jmu = nlsCopySign(sqrt(w / ((p - f) * gammaRatio + q)), jl);
        ymu = jmu * gammaRatio;
        let ymup = ymu * (p + q / gammaRatio);
        y1 = mu * xi * ymu - ymup;
    }
    let jv = jl1 * (jmu / jl);
    // upward recurrence for Y
    for (var i : i32 = 1; i <= nl; i = i + 1) {
        let t = (mu + f32(i)) * xi2 * y1 - ymu;
        ymu = y1;
        y1 = t;
    }
    return vec2<f32>(jv, ymu);
}
// I and K of order nu >= 0 at 0 < x as (mI, eI, mK, eK): I = mI exp(eI), K = mK exp(eK)
fn nlsBesselIK(nu : f32, x : f32) -> vec4<f32> {
    let xi = 1.0 / x;
    let xi2 = 2.0 * xi;
    // fractional order in (-1/4, 3/4] keeps the Wronskian step free of cancellation
    let nl = i32(nu + 0.25);
    let mu = nu - f32(nl);
    let m2 = mu * mu;
    var kmu : f32;
    var k1 : f32;
    // exponent carried apart from K: -x when K comes scaled by exp(x)
    var kExp : f32 = 0.0;
    if (x < 2.0) {
        let x2 = 0.5 * x;
        let pimu = NLS_PI * mu;
        let fct = select(pimu / nlsSin(pimu), 1.0, abs(pimu) < NLS_BESSEL_EPS);
        var dd = -log(x2);
        var e = mu * dd;
        let fct2 = select(nlsSinh(e) / e, 1.0, abs(e) < NLS_BESSEL_EPS);
        let g = nlsBesselGammaTerms(mu);
        var ff = fct * (g.x * nlsCosh(e) + g.y * fct2 * dd);
        var sum = ff;
        e = exp(e);
        var p = 0.5 * e / g.z;
        var q = 0.5 / (e * g.w);
        var cc : f32 = 1.0;
        dd = x2 * x2;
        var sum1 = p;
        for (var i : i32 = 1; i < 1000; i = i + 1) {
            let fi = f32(i);
            ff = (fi * ff + p + q) / (fi * fi - m2);
            cc = cc * dd / fi;
            p = p / (fi - mu);
            q = q / (fi + mu);
            let del = cc * ff;
            sum = sum + del;
            sum1 = sum1 + cc * (p - fi * ff);
            if (abs(del) < abs(sum) * NLS_BESSEL_EPS) { break; }
        }
        kmu = sum;
        k1 = sum1 * xi2;
    } else {
        // Steed continued fraction, K scaled by exp(x)
        var b = 2.0 * (1.0 + x);
        var d = 1.0 / b;
        var h = d;
        var delh = d;
        var q1 : f32 = 0.0;
        var q2 : f32 = 1.0;
        let a1 = 0.25 - m2;
        var q = a1;
        var cc = a1;
        var a = -a1;
        var s = 1.0 + q * delh;
        for (var i : i32 = 1; i < 20000; i = i + 1) {
            a = a - f32(2 * i);
            cc = -a * cc / f32(i + 1);
            let qn = (q1 - b * q2) / a;
            q1 = q2;
            q2 = qn;
            q = q + cc * qn;
            b = b + 2.0;
            d = 1.0 / (b + a * d);
            delh = (b * d - 1.0) * delh;
            h = h + delh;
            let dels = q * delh;
            s = s + dels;
            if (abs(dels / s) < NLS_BESSEL_EPS) { break; }
        }
        h = a1 * h;
        kmu = sqrt(NLS_PI / (2.0 * x)) / s;
        k1 = kmu * (mu + x + 0.5 - h) * xi;
        kExp = -x;
    }
    let kmup = mu * xi * kmu - k1;
    var iMant : f32;
    var iExp : f32;
    if (x >= max(30.0, 0.25 * nu * nu)) {
        // large x expansion of exp(-x) I
        let mu4 = 4.0 * nu * nu;
        let y8 = 8.0 * x;
        var term : f32 = 1.0;
        var sum : f32 = 1.0;
        for (var k : i32 = 1; k < 200; k = k + 1) {
            let odd = f32(2 * k - 1);
            term = -term * (mu4 - odd * odd) / (f32(k) * y8);
            sum = sum + term;
            if (abs(term) < NLS_BESSEL_EPS) { break; }
        }
        iMant = sum / sqrt(2.0 * NLS_PI * x);
        iExp = x;
    } else {
        // continued fraction for I'/I, downward recurrence to mu, then the Wronskian
        var h = max(nu * xi, NLS_BESSEL_TINY);
        var d : f32 = 0.0;
        var c = h;
        for (var i : i32 = 0; i < 20000; i = i + 1) {
            let b = xi2 * (nu + f32(i + 1));
            d = 1.0 / (b + d);
            c = b + 1.0 / c;
            let del = c * d;
            h = h * del;
            if (abs(del - 1.0) < NLS_BESSEL_EPS) { break; }
        }
        var il = NLS_BESSEL_TINY;
        var ipl = h * il;
        // log of the factor removed while rescaling the recurrence
        var removed : f32 = 0.0;
        for (var l : i32 = nl; l >= 1; l = l - 1) {
            let fact = (mu + f32(l)) * xi;
            let t = fact * il + ipl;
            ipl = (fact - xi) * t + il;
            il = t;
            if (abs(il) > NLS_BESSEL_BIG) {
                il = il * 1e-30;
                ipl = ipl * 1e-30;
                removed = removed + 69.0775527898214;
            }
        }
        let f = ipl / il;
        // I(mu) from the Wronskian has the scaling of K, then I(nu) = I(mu) * il(start) / il(end), il(start) = exp(-69.08)
        iMant = (xi / (f * kmu - kmup)) / il;
        iExp = -kExp - removed - 69.0775527898214;
    }
    // upward recurrence for K, rescaled to stay in range
    for (var i : i32 = 1; i <= nl; i = i + 1) {
        let t = (mu + f32(i)) * xi2 * k1 + kmu;
        kmu = k1;
        k1 = t;
        if (abs(k1) > NLS_BESSEL_BIG) {
            kmu = kmu * 1e-30;
            k1 = k1 * 1e-30;
            kExp = kExp + 69.0775527898214;
        }
    }
    return vec4<f32>(iMant, iExp, kmu, kExp);
}
// v * exp(e) without an early overflow or underflow
fn nlsBesselTimesExp(v : f32, e : f32) -> f32 {
    if (v == 0.0 || nlsIsInf(v) || nlsIsNan(v)) { return v; }
    if (abs(e) < 60.0) {
        let direct = v * exp(e);
        if (abs(direct) > 1e-30 && abs(direct) < 1e30) { return direct; }
    }
    return nlsCopySign(exp(e + log(abs(v))), v);
}
// a + b skipping a zero coefficient (no 0 * Inf)
fn nlsBesselCombine(ca : f32, a : f32, cb : f32, b : f32) -> f32 {
    return select(ca * a, 0.0, ca == 0.0) + select(cb * b, 0.0, cb == 0.0);
}
// kind 0..5 = J, Y, I, K, H1, H2: (re, im, 1 when the host must compute)
fn nlsBesselValue(kind : u32, nu : f32, x : f32, scaled : bool) -> vec3<f32> {
    let host = vec3<f32>(0.0, 0.0, 1.0);
    if (nlsIsNan(nu) || nlsIsNan(x)) { return vec3<f32>(nlsNaN(), 0.0, 0.0); }
    let isHankel = kind >= 4u;
    let v = abs(nu);
    if (v > 100.0) { return host; }
    let intNu = nlsIsInt(nu);
    let odd = intNu && (v - 2.0 * floor(0.5 * v)) == 1.0;
    var ax = x;
    var sgn : f32 = 1.0;
    if (x < 0.0) {
        // only J and I of integer order stay real on the negative axis
        if (!(intNu && (kind == 0u || kind == 2u)) || nlsIsInf(x)) { return host; }
        ax = -x;
        if (odd) { sgn = -1.0; }
    }
    if (nlsIsInf(ax)) {
        if (isHankel) { return host; }
        if (kind == 2u) { return vec3<f32>(select(nlsInf(), nlsNaN(), scaled), 0.0, 0.0); }
        if (kind == 3u) { return vec3<f32>(0.0, 0.0, 0.0); }
        return vec3<f32>(nlsNaN(), 0.0, 0.0);
    }
    if (ax > 2000.0 && kind != 2u && kind != 3u) { return host; }
    if (ax == 0.0) {
        if (isHankel) { return host; }
        if (kind == 1u) { return vec3<f32>(-nlsInf(), 0.0, 0.0); }
        if (kind == 3u) { return vec3<f32>(nlsInf(), 0.0, 0.0); }
        if (nu == 0.0) { return vec3<f32>(1.0, 0.0, 0.0); }
        if (nu > 0.0 || intNu) { return vec3<f32>(0.0, 0.0, 0.0); }
        return vec3<f32>(nlsInf(), 0.0, 0.0);
    }
    let useReflection = nu < 0.0 && !intNu;
    var s : f32 = 0.0;
    var c : f32 = 1.0;
    if (useReflection) {
        s = nlsSinpi(v);
        c = nlsCospi(v);
    }
    if (kind == 2u || kind == 3u) {
        let ik = nlsBesselIK(v, ax);
        if (kind == 3u) {
            return vec3<f32>(nlsBesselTimesExp(ik.z, ik.w + select(0.0, ax, scaled)), 0.0, 0.0);
        }
        // scaled I carries exp(-|x|)
        let shift = select(0.0, -ax, scaled);
        var iv = nlsBesselTimesExp(ik.x, ik.y + shift);
        if (useReflection && s != 0.0) {
            iv = iv + 0.636619772367581 * s * nlsBesselTimesExp(ik.z, ik.w + shift);
        }
        return vec3<f32>(sgn * iv, 0.0, 0.0);
    }
    let jy = nlsBesselJY(v, ax);
    var jv = jy.x;
    var yv = jy.y;
    if (useReflection) {
        jv = nlsBesselCombine(c, jy.x, -s, jy.y);
        yv = nlsBesselCombine(s, jy.x, c, jy.y);
    } else if (nu < 0.0 && odd) {
        jv = -jv;
        yv = -yv;
    }
    if (kind == 0u) { return vec3<f32>(sgn * jv, 0.0, 0.0); }
    if (kind == 1u) { return vec3<f32>(yv, 0.0, 0.0); }
    var h = vec2<f32>(jv, select(yv, -yv, kind == 5u));
    if (scaled) {
        // exp(-i x) for H1, exp(i x) for H2
        let sc = nlsSinCos(ax);
        let e = vec2<f32>(sc.y, select(-sc.x, sc.x, kind == 5u));
        h = vec2<f32>(h.x * e.x - h.y * e.y, h.x * e.y + h.y * e.x);
    }
    return vec3<f32>(h.x, h.y, 0.0);
}
// Airy function k = 0..3 (Ai, Ai', Bi, Bi') of a real x: (value, 1 when the host must compute)
fn nlsAiryValue(k : u32, x : f32, scaled : bool) -> vec2<f32> {
    if (nlsIsNan(x)) { return vec2<f32>(nlsNaN(), 0.0); }
    if (nlsIsInf(x) || (x < 0.0 && (x < -150.0 || (scaled && k < 2u)))) { return vec2<f32>(0.0, 1.0); }
    let invSqrt3 = 0.577350269189626;
    if (x == 0.0) {
        let ai0 = 0.355028053887817;
        let aip0 = -0.258819403792807;
        var values = array<f32, 4>(ai0, aip0, ai0 / invSqrt3, -aip0 / invSqrt3);
        return vec2<f32>(values[k], 0.0);
    }
    let ax = abs(x);
    let r = sqrt(ax);
    let zeta = 2.0 / 3.0 * ax * r;
    let order = select(1.0 / 3.0, 2.0 / 3.0, k == 1u || k == 3u);
    if (x > 0.0) {
        let ik = nlsBesselIK(order, zeta);
        let factor = select(r, x, k == 1u || k == 3u);
        if (k < 2u) {
            // Ai = sqrt(x / 3) K(1/3, zeta) / pi, scaled by exp(zeta)
            let a = select(1.0, -1.0, k == 1u) * factor * invSqrt3 / NLS_PI;
            return vec2<f32>(a * nlsBesselTimesExp(ik.z, ik.w + select(0.0, zeta, scaled)), 0.0);
        }
        // Bi = sqrt(x) (2 I(1/3, zeta) / sqrt(3) + K(1/3, zeta) / pi), scaled by exp(-zeta)
        let shift = select(0.0, -zeta, scaled);
        let b = 2.0 * invSqrt3 * nlsBesselTimesExp(ik.x, ik.y + shift)
            + nlsBesselTimesExp(ik.z, ik.w + shift) / NLS_PI;
        return vec2<f32>(factor * b, 0.0);
    }
    let jy = nlsBesselJY(order, zeta);
    var values = array<f32, 4>(0.5 * r * (jy.x - invSqrt3 * jy.y), 0.5 * ax * (invSqrt3 * jy.y + jy.x),
        -0.5 * r * (jy.y + invSqrt3 * jy.x), 0.5 * ax * (invSqrt3 * jy.x - jy.y));
    return vec2<f32>(values[k], 0.0);
}
struct Params { n : u32, nnu : u32, nz : u32, kind : u32, scaled : u32, complexOut : u32, pad0 : u32, pad1 : u32 };
@group(0) @binding(0) var<storage, read> nuv : array<f32>;
@group(0) @binding(1) var<storage, read> zv : array<f32>;
@group(0) @binding(2) var<storage, read_write> out : array<f32>;
@group(0) @binding(3) var<storage, read_write> flag : array<atomic<u32>>;
@group(0) @binding(4) var<uniform> params : Params;
@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let i = gid.x;
    if (i >= params.n) { return; }
    let nu = nuv[select(i, 0u, params.nnu == 1u)];
    let z = zv[select(i, 0u, params.nz == 1u)];
    let scaled = params.scaled == 1u;
    if (params.kind >= 6u) {
        let a = nlsAiryValue(params.kind - 6u, z, scaled);
        if (a.y > 0.5) { atomicOr(&flag[0], 1u); }
        out[i] = a.x;
        return;
    }
    let r = nlsBesselValue(params.kind, nu, z, scaled);
    if (r.z > 0.5) { atomicOr(&flag[0], 1u); }
    if (params.complexOut == 1u) {
        out[2u * i] = r.x;
        out[2u * i + 1u] = r.y;
    } else {
        out[i] = r.x;
    }
}
)WGSL";
//=============================================================================
static bool
isRealFloatOperand(GpuArrayObject* g)
{
    return !g->isComplex() && !g->isRawIntStorage() && !g->isLogical()
        && GpuIntegerResultType(g, g).empty();
}
//=============================================================================
static bool
runBesselKernel(uint32_t kind, GpuArrayObject* nu, GpuArrayObject* z, bool scaled, ArrayOf& result)
{
    if (!isRealFloatOperand(nu) || !isRealFloatOperand(z)) {
        return false;
    }
    // same size or one scalar, as on the host
    Dimensions dims = z->dimensions();
    if (nu->elementCount() != 1) {
        if (z->elementCount() != 1 && !nu->dimensions().equals(z->dimensions())) {
            return false;
        }
        dims = nu->dimensions();
    }
    const bool complexOut = (kind == 4u || kind == 5u);
    GpuArrayObject* output = AllocateGpuArray(dims, "single", complexOut);
    ArrayOf owner = ArrayOf::handle(output);
    uint32_t n = (uint32_t)dims.elementCount();
    if (n == 0) {
        result = owner;
        return true;
    }
    GpuArrayObject* flag = AllocateGpuArray(Dimensions(1, 1), "uint32", false);
    ArrayOf flagOwner = ArrayOf::handle(flag);
    std::string source = GpuMathHelpersWgsl() + BESSEL_WGSL;
    WGPUComputePipeline pipeline
        = GpuContext::getInstance()->pipelineFor("gpuArray:bessel", source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    uint32_t params[8] = { n, (uint32_t)nu->elementCount(), (uint32_t)z->elementCount(), kind,
        scaled ? 1u : 0u, complexOut ? 1u : 0u, 0u, 0u };
    GpuDispatchKernel(pipeline, { nu->buffer(), z->buffer(), output->buffer(), flag->buffer() },
        params, 8, (n + 63u) / 64u, 1);
    ArrayOf flagValue = GatherGpuArray(flag);
    if (((const uint32_t*)flagValue.dataPointer())[0] != 0) {
        return false;
    }
    result = owner;
    return true;
}
//=============================================================================
bool
GpuBessel(int kind, GpuArrayObject* nu, GpuArrayObject* z, bool scaled, ArrayOf& result)
{
    if (kind < 0 || kind > 5) {
        return false;
    }
    return runBesselKernel((uint32_t)kind, nu, z, scaled, result);
}
//=============================================================================
bool
GpuAiry(int k, GpuArrayObject* z, bool scaled, ArrayOf& result)
{
    if (k < 0 || k > 3) {
        return false;
    }
    return runBesselKernel(6u + (uint32_t)k, z, z, scaled, result);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
