//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// C++ port of the public-domain Amos algorithms (ACM TOMS 644, netlib/amos).
// Range checks on |z| and on the order were removed: results are always computed.
//=============================================================================
#include <cmath>
#include <cfloat>
#include <climits>
#include <cstdlib>
#include "AmosBessel.hpp"
//=============================================================================
namespace Nelson::Amos {
//=============================================================================
static inline double
fmax_(double a, double b)
{
    return a > b ? a : b;
}
//=============================================================================
static inline double
fmin_(double a, double b)
{
    return a < b ? a : b;
}
//=============================================================================
static inline int
imax_(int a, int b)
{
    return a > b ? a : b;
}
//=============================================================================
static inline int
imin_(int a, int b)
{
    return a < b ? a : b;
}
//=============================================================================
static inline int
imod_(int a, int b)
{
    return a % b;
}
//=============================================================================
// truncation through single precision; out of range values give INT_MIN on every CPU
static inline int
fortranInt(double x)
{
    float f = (float)x;
    if (!(f > -2147483648.0f && f < 2147483648.0f)) {
        return INT_MIN;
    }
    return (int)f;
}
//=============================================================================
// IEEE double machine constants used by the algorithms
static inline double
d1mach(int i)
{
    switch (i) {
    case 1:
        return DBL_MIN;
    case 2:
        return DBL_MAX;
    case 3:
        return DBL_EPSILON / 2;
    case 4:
        return DBL_EPSILON;
    default:
        return 0.30102999566398119521;
    }
}
//=============================================================================
static inline int
i1mach(int i)
{
    switch (i) {
    case 9:
        return INT_MAX;
    case 14:
        return DBL_MANT_DIG;
    case 15:
        return DBL_MIN_EXP;
    default:
        return DBL_MAX_EXP;
    }
}
//=============================================================================
static double
zabs(double zr, double zi);
static void
zdiv(double ar, double ai, double br, double bi, double& cr, double& ci);
static void
zexp(double ar, double ai, double& br, double& bi);
static void
zlog(double ar, double ai, double& br, double& bi, int& ierr);
static void
zmlt(double ar, double ai, double br, double bi, double& cr, double& ci);
static void
zsqrt(double ar, double ai, double& br, double& bi);
static double
dgamln(double z, int& ierr);
static void
zshch(double zr, double zi, double& cshr, double& cshi, double& cchr, double& cchi);
static void
zuchk(double yr, double yi, int& nz, double ascle, double tol);
static void
zs1s2(double zrr, double zri, double& s1r, double& s1i, double& s2r, double& s2i, int& nz,
    double ascle, double alim, int& iuf);
static void
zkscl(double zrr, double zri, double fnu, int n, double* yr, double* yi, int& nz, double rzr,
    double rzi, double ascle, double tol, double elim);
static void
zrati(double zr, double zi, double fnu, int n, double* cyr, double* cyi, double tol);
static void
zseri(double zr, double zi, double fnu, int kode, int n, double* yr, double* yi, int& nz,
    double tol, double elim, double alim);
static void
zasyi(double zr, double zi, double fnu, int kode, int n, double* yr, double* yi, int& nz, double rl,
    double tol, double elim, double alim);
static void
zmlri(
    double zr, double zi, double fnu, int kode, int n, double* yr, double* yi, int& nz, double tol);
static void
zunik(double zrr, double zri, double fnu, int ikflg, int ipmtr, double tol, int& init, double& phir,
    double& phii, double& zeta1r, double& zeta1i, double& zeta2r, double& zeta2i, double& sumr,
    double& sumi, double* cwrkr, double* cwrki);
static void
zunhj(double zr, double zi, double fnu, int ipmtr, double tol, double& phir, double& phii,
    double& argr, double& argi, double& zeta1r, double& zeta1i, double& zeta2r, double& zeta2i,
    double& asumr, double& asumi, double& bsumr, double& bsumi);
static void
zuoik(double zr, double zi, double fnu, int kode, int ikflg, int n, double* yr, double* yi,
    int& nuf, double tol, double elim, double alim);
static void
zwrsk(double zrr, double zri, double fnu, int kode, int n, double* yr, double* yi, int& nz,
    double* cwr, double* cwi, double tol, double elim, double alim);
static void
zbknu(double zr, double zi, double fnu, int kode, int n, double* yr, double* yi, int& nz,
    double tol, double elim, double alim);
static void
zuni1(double zr, double zi, double fnu, int kode, int n, double* yr, double* yi, int& nz,
    int& nlast, double fnul, double tol, double elim, double alim);
static void
zuni2(double zr, double zi, double fnu, int kode, int n, double* yr, double* yi, int& nz,
    int& nlast, double fnul, double tol, double elim, double alim);
static void
zbuni(double zr, double zi, double fnu, int kode, int n, double* yr, double* yi, int& nz, int nui,
    int& nlast, double fnul, double tol, double elim, double alim);
static void
zunk1(double zr, double zi, double fnu, int kode, int mr, int n, double* yr, double* yi, int& nz,
    double tol, double elim, double alim);
static void
zunk2(double zr, double zi, double fnu, int kode, int mr, int n, double* yr, double* yi, int& nz,
    double tol, double elim, double alim);
static void
zbunk(double zr, double zi, double fnu, int kode, int mr, int n, double* yr, double* yi, int& nz,
    double tol, double elim, double alim);
static void
zacai(double zr, double zi, double fnu, int kode, int mr, int n, double* yr, double* yi, int& nz,
    double rl, double tol, double elim, double alim);
static void
zacon(double zr, double zi, double fnu, int kode, int mr, int n, double* yr, double* yi, int& nz,
    double rl, double fnul, double tol, double elim, double alim);
static void
zbinu(double zr, double zi, double fnu, int kode, int n, double* cyr, double* cyi, int& nz,
    double rl, double fnul, double tol, double elim, double alim);
//=============================================================================
static double
zabs(double zr, double zi)
{
    double result_ = 0.0;
    double q = 0;
    double s = 0;
    double u = 0;
    double v = 0;
    u = std::fabs(zr);
    v = std::fabs(zi);
    s = u + v;
    s = s * 1.0e+0;
    if (s == 0.0e+0) {
        goto L20;
    }
    if (u > v) {
        goto L10;
    }
    q = u / v;
    result_ = v * std::sqrt(1.e+0 + q * q);
    return result_;
L10:
    q = v / u;
    result_ = u * std::sqrt(1.e+0 + q * q);
    return result_;
L20:
    result_ = 0.0e+0;
    return result_;
}
//=============================================================================
static void
zdiv(double ar, double ai, double br, double bi, double& cr, double& ci)
{
    double bm = 0;
    double ca = 0;
    double cb = 0;
    double cc = 0;
    double cd = 0;
    bm = 1.0e0 / zabs(br, bi);
    cc = br * bm;
    cd = bi * bm;
    ca = (ar * cc + ai * cd) * bm;
    cb = (ai * cc - ar * cd) * bm;
    cr = ca;
    ci = cb;
    return;
}
//=============================================================================
static void
zexp(double ar, double ai, double& br, double& bi)
{
    double ca = 0;
    double cb = 0;
    double zm = 0;
    zm = std::exp(ar);
    ca = zm * std::cos(ai);
    cb = zm * std::sin(ai);
    br = ca;
    bi = cb;
    return;
}
//=============================================================================
static void
zlog(double ar, double ai, double& br, double& bi, int& ierr)
{
    const double dhpi = 1.570796326794896619231321696e+0;
    const double dpi = 3.141592653589793238462643383e+0;
    double dtheta = 0;
    double zm = 0;
    ierr = 0;
    if (ar == 0.0e+0) {
        goto L10;
    }
    if (ai == 0.0e+0) {
        goto L20;
    }
    dtheta = std::atan(ai / ar);
    if (dtheta <= 0.0e+0) {
        goto L40;
    }
    if (ar < 0.0e+0) {
        dtheta = dtheta - dpi;
    }
    goto L50;
L10:
    if (ai == 0.0e+0) {
        goto L60;
    }
    bi = dhpi;
    br = std::log(std::fabs(ai));
    if (ai < 0.0e+0) {
        bi = -bi;
    }
    return;
L20:
    if (ar > 0.0e+0) {
        goto L30;
    }
    br = std::log(std::fabs(ar));
    bi = dpi;
    return;
L30:
    br = std::log(ar);
    bi = 0.0e+0;
    return;
L40:
    if (ar < 0.0e+0) {
        dtheta = dtheta + dpi;
    }
L50:
    zm = zabs(ar, ai);
    br = std::log(zm);
    bi = dtheta;
    return;
L60:;
    ierr = 1;
    return;
}
//=============================================================================
static void
zmlt(double ar, double ai, double br, double bi, double& cr, double& ci)
{
    double ca = 0;
    double cb = 0;
    ca = ar * br - ai * bi;
    cb = ar * bi + ai * br;
    cr = ca;
    ci = cb;
    return;
}
//=============================================================================
static void
zsqrt(double ar, double ai, double& br, double& bi)
{
    const double dpi = 3.141592653589793238462643383e+0;
    const double drt = 7.071067811865475244008443621e-1;
    double dtheta = 0;
    double zm = 0;
    zm = zabs(ar, ai);
    zm = std::sqrt(zm);
    if (ar == 0.0e+0) {
        goto L10;
    }
    if (ai == 0.0e+0) {
        goto L20;
    }
    dtheta = std::atan(ai / ar);
    if (dtheta <= 0.0e+0) {
        goto L40;
    }
    if (ar < 0.0e+0) {
        dtheta = dtheta - dpi;
    }
    goto L50;
L10:
    if (ai > 0.0e+0) {
        goto L60;
    }
    if (ai < 0.0e+0) {
        goto L70;
    }
    br = 0.0e+0;
    bi = 0.0e+0;
    return;
L20:
    if (ar > 0.0e+0) {
        goto L30;
    }
    br = 0.0e+0;
    bi = std::sqrt(std::fabs(ar));
    return;
L30:
    br = std::sqrt(ar);
    bi = 0.0e+0;
    return;
L40:
    if (ar < 0.0e+0) {
        dtheta = dtheta + dpi;
    }
L50:
    dtheta = dtheta * 0.5e+0;
    br = zm * std::cos(dtheta);
    bi = zm * std::sin(dtheta);
    return;
L60:
    br = zm * drt;
    bi = zm * drt;
    return;
L70:
    br = zm * drt;
    bi = -zm * drt;
    return;
}
//=============================================================================
static double
dgamln(double z, int& ierr)
{
    const double con = 1.83787706640934548e+00;
    static const double cf[22] = { 8.33333333333333333e-02, -2.77777777777777778e-03,
        7.93650793650793651e-04, -5.95238095238095238e-04, 8.41750841750841751e-04,
        -1.91752691752691753e-03, 6.41025641025641026e-03, -2.95506535947712418e-02,
        1.79644372368830573e-01, -1.39243221690590112e+00, 1.34028640441683920e+01,
        -1.56848284626002017e+02, 2.19310333333333333e+03, -3.61087712537249894e+04,
        6.91472268851313067e+05, -1.52382215394074162e+07, 3.82900751391414141e+08,
        -1.08822660357843911e+10, 3.47320283765002252e+11, -1.23696021422692745e+13,
        4.88788064793079335e+14, -2.13203339609193739e+16 };
    static const double gln[100] = { 0.00000000000000000e+00, 0.00000000000000000e+00,
        6.93147180559945309e-01, 1.79175946922805500e+00, 3.17805383034794562e+00,
        4.78749174278204599e+00, 6.57925121201010100e+00, 8.52516136106541430e+00,
        1.06046029027452502e+01, 1.28018274800814696e+01, 1.51044125730755153e+01,
        1.75023078458738858e+01, 1.99872144956618861e+01, 2.25521638531234229e+01,
        2.51912211827386815e+01, 2.78992713838408916e+01, 3.06718601060806728e+01,
        3.35050734501368889e+01, 3.63954452080330536e+01, 3.93398841871994940e+01,
        4.23356164607534850e+01, 4.53801388984769080e+01, 4.84711813518352239e+01,
        5.16066755677643736e+01, 5.47847293981123192e+01, 5.80036052229805199e+01,
        6.12617017610020020e+01, 6.45575386270063311e+01, 6.78897431371815350e+01,
        7.12570389671680090e+01, 7.46582363488301644e+01, 7.80922235533153106e+01,
        8.15579594561150372e+01, 8.50544670175815174e+01, 8.85808275421976788e+01,
        9.21361756036870925e+01, 9.57196945421432025e+01, 9.93306124547874269e+01,
        1.02968198614513813e+02, 1.06631760260643459e+02, 1.10320639714757395e+02,
        1.14034211781461703e+02, 1.17771881399745072e+02, 1.21533081515438634e+02,
        1.25317271149356895e+02, 1.29123933639127215e+02, 1.32952575035616310e+02,
        1.36802722637326368e+02, 1.40673923648234259e+02, 1.44565743946344886e+02,
        1.48477766951773032e+02, 1.52409592584497358e+02, 1.56360836303078785e+02,
        1.60331128216630907e+02, 1.64320112263195181e+02, 1.68327445448427652e+02,
        1.72352797139162802e+02, 1.76395848406997352e+02, 1.80456291417543771e+02,
        1.84533828861449491e+02, 1.88628173423671591e+02, 1.92739047287844902e+02,
        1.96866181672889994e+02, 2.01009316399281527e+02, 2.05168199482641199e+02,
        2.09342586752536836e+02, 2.13532241494563261e+02, 2.17736934113954227e+02,
        2.21956441819130334e+02, 2.26190548323727593e+02, 2.30439043565776952e+02,
        2.34701723442818268e+02, 2.38978389561834323e+02, 2.43268849002982714e+02,
        2.47572914096186884e+02, 2.51890402209723194e+02, 2.56221135550009525e+02,
        2.60564940971863209e+02, 2.64921649798552801e+02, 2.69291097651019823e+02,
        2.73673124285693704e+02, 2.78067573440366143e+02, 2.82474292687630396e+02,
        2.86893133295426994e+02, 2.91323950094270308e+02, 2.95766601350760624e+02,
        3.00220948647014132e+02, 3.04686856765668715e+02, 3.09164193580146922e+02,
        3.13652829949879062e+02, 3.18152639620209327e+02, 3.22663499126726177e+02,
        3.27185287703775217e+02, 3.31717887196928473e+02, 3.36261181979198477e+02,
        3.40815058870799018e+02, 3.45379407062266854e+02, 3.49954118040770237e+02,
        3.54539085519440809e+02, 3.59134205369575399e+02 };
    double result_ = 0.0;
    double fln = 0;
    double fz = 0;
    int i = 0;
    int i1m = 0;
    int k = 0;
    int mz = 0;
    int nz = 0;
    double rln = 0;
    double s = 0;
    double t1 = 0;
    double tlg = 0;
    double trm = 0;
    double tst = 0;
    double wdtol = 0;
    double zdmy = 0;
    double zinc = 0;
    double zm = 0;
    double zmin = 0;
    double zp = 0;
    double zsq = 0;
    int lim0_ = 0;
    int lim1_ = 0;
    ierr = 0;
    if (z <= 0.0e0) {
        goto L70;
    }
    if (z > 101.0e0) {
        goto L10;
    }
    nz = fortranInt(z);
    fz = z - (double)(nz);
    if (fz > 0.0e0) {
        goto L10;
    }
    if (nz > 100) {
        goto L10;
    }
    result_ = gln[(nz)-1];
    return result_;
L10:;
    wdtol = d1mach(4);
    wdtol = fmax_(wdtol, 0.5e-18);
    i1m = i1mach(14);
    rln = d1mach(5) * (double)(i1m);
    fln = fmin_(rln, 20.0e0);
    fln = fmax_(fln, 3.0e0);
    fln = fln - 3.0e0;
    zm = 1.8000e0 + 0.3875e0 * fln;
    mz = fortranInt(zm) + 1;
    zmin = (double)(mz);
    zdmy = z;
    zinc = 0.0e0;
    if (z >= zmin) {
        goto L20;
    }
    zinc = zmin - (double)(nz);
    zdmy = z + zinc;
L20:;
    zp = 1.0e0 / zdmy;
    t1 = cf[(1) - 1] * zp;
    s = t1;
    if (zp < wdtol) {
        goto L40;
    }
    zsq = zp * zp;
    tst = t1 * wdtol;
    lim0_ = 22;
    for (k = 2; k <= lim0_; k += 1) {
        zp = zp * zsq;
        trm = cf[(k)-1] * zp;
        if (std::fabs(trm) < tst) {
            goto L40;
        }
        s = s + trm;
        ;
    }
L40:;
    if (zinc != 0.0e0) {
        goto L50;
    }
    tlg = std::log(z);
    result_ = z * (tlg - 1.0e0) + 0.5e0 * (con - tlg) + s;
    return result_;
L50:;
    zp = 1.0e0;
    nz = fortranInt(zinc);
    lim1_ = nz;
    for (i = 1; i <= lim1_; i += 1) {
        zp = zp * (z + (double)(i - 1));
        ;
    }
    tlg = std::log(zdmy);
    result_ = zdmy * (tlg - 1.0e0) - std::log(zp) + 0.5e0 * (con - tlg) + s;
    return result_;
L70:;
    ierr = 1;
    return result_;
}
//=============================================================================
static void
zshch(double zr, double zi, double& cshr, double& cshi, double& cchr, double& cchi)
{
    double ch = 0;
    double cn = 0;
    double sh = 0;
    double sn = 0;
    sh = std::sinh(zr);
    ch = std::cosh(zr);
    sn = std::sin(zi);
    cn = std::cos(zi);
    cshr = sh * cn;
    cshi = ch * sn;
    cchr = ch * cn;
    cchi = sh * sn;
    return;
}
//=============================================================================
static void
zuchk(double yr, double yi, int& nz, double ascle, double tol)
{
    double ss = 0;
    double st = 0;
    double wi = 0;
    double wr = 0;
    nz = 0;
    wr = std::fabs(yr);
    wi = std::fabs(yi);
    st = fmin_(wr, wi);
    if (st > ascle) {
        return;
    }
    ss = fmax_(wr, wi);
    st = st / tol;
    if (ss < st) {
        nz = 1;
    }
    return;
}
//=============================================================================
static void
zs1s2(double zrr, double zri, double& s1r, double& s1i, double& s2r, double& s2i, int& nz,
    double ascle, double alim, int& iuf)
{
    const double zeroi = 0.0e0;
    const double zeror = 0.0e0;
    double aa = 0;
    double aln = 0;
    double as1 = 0;
    double as2 = 0;
    double c1i = 0;
    double c1r = 0;
    int idum = 0;
    double s1di = 0;
    double s1dr = 0;
    nz = 0;
    as1 = zabs(s1r, s1i);
    as2 = zabs(s2r, s2i);
    if (s1r == 0.0e0 && s1i == 0.0e0) {
        goto L10;
    }
    if (as1 == 0.0e0) {
        goto L10;
    }
    aln = -zrr - zrr + std::log(as1);
    s1dr = s1r;
    s1di = s1i;
    s1r = zeror;
    s1i = zeroi;
    as1 = zeror;
    if (aln < (-alim)) {
        goto L10;
    }
    zlog(s1dr, s1di, c1r, c1i, idum);
    c1r = c1r - zrr - zrr;
    c1i = c1i - zri - zri;
    zexp(c1r, c1i, s1r, s1i);
    as1 = zabs(s1r, s1i);
    iuf = iuf + 1;
L10:;
    aa = fmax_(as1, as2);
    if (aa > ascle) {
        return;
    }
    s1r = zeror;
    s1i = zeroi;
    s2r = zeror;
    s2i = zeroi;
    nz = 1;
    iuf = 0;
    return;
}
//=============================================================================
static void
zkscl(double zrr, double zri, double fnu, int n, double* yr, double* yi, int& nz, double rzr,
    double rzi, double ascle, double tol, double elim)
{
    const double zeroi = 0.0e0;
    const double zeror = 0.0e0;
    double acs = 0;
    double alas = 0;
    double as = 0;
    double celmr = 0;
    double cki = 0;
    double ckr = 0;
    double csi = 0;
    double csr = 0;
    double cyi[2] = {};
    double cyr[2] = {};
    double elm = 0;
    double fn = 0;
    double helim = 0;
    int i = 0;
    int ic = 0;
    int idum = 0;
    int kk = 0;
    int nn = 0;
    int nw = 0;
    double s1i = 0;
    double s1r = 0;
    double s2i = 0;
    double s2r = 0;
    double str = 0;
    double zdi = 0;
    double zdr = 0;
    int lim0_ = 0;
    int lim1_ = 0;
    int lim2_ = 0;
    nz = 0;
    ic = 0;
    nn = imin_(2, n);
    lim0_ = nn;
    for (i = 1; i <= lim0_; i += 1) {
        s1r = yr[(i)-1];
        s1i = yi[(i)-1];
        cyr[(i)-1] = s1r;
        cyi[(i)-1] = s1i;
        as = zabs(s1r, s1i);
        acs = -zrr + std::log(as);
        nz = nz + 1;
        yr[(i)-1] = zeror;
        yi[(i)-1] = zeroi;
        if (acs < (-elim)) {
            goto L10;
        }
        zlog(s1r, s1i, csr, csi, idum);
        csr = csr - zrr;
        csi = csi - zri;
        str = std::exp(csr) / tol;
        csr = str * std::cos(csi);
        csi = str * std::sin(csi);
        zuchk(csr, csi, nw, ascle, tol);
        if (nw != 0) {
            goto L10;
        }
        yr[(i)-1] = csr;
        yi[(i)-1] = csi;
        ic = i;
        nz = nz - 1;
    L10:;
    }
    if (n == 1) {
        return;
    }
    if (ic > 1) {
        goto L20;
    }
    yr[(1) - 1] = zeror;
    yi[(1) - 1] = zeroi;
    nz = 2;
L20:;
    if (n == 2) {
        return;
    }
    if (nz == 0) {
        return;
    }
    fn = fnu + 1.0e0;
    ckr = fn * rzr;
    cki = fn * rzi;
    s1r = cyr[(1) - 1];
    s1i = cyi[(1) - 1];
    s2r = cyr[(2) - 1];
    s2i = cyi[(2) - 1];
    helim = 0.5e0 * elim;
    elm = std::exp(-elim);
    celmr = elm;
    zdr = zrr;
    zdi = zri;
    lim1_ = n;
    for (i = 3; i <= lim1_; i += 1) {
        kk = i;
        csr = s2r;
        csi = s2i;
        s2r = ckr * csr - cki * csi + s1r;
        s2i = cki * csr + ckr * csi + s1i;
        s1r = csr;
        s1i = csi;
        ckr = ckr + rzr;
        cki = cki + rzi;
        as = zabs(s2r, s2i);
        alas = std::log(as);
        acs = -zdr + alas;
        nz = nz + 1;
        yr[(i)-1] = zeror;
        yi[(i)-1] = zeroi;
        if (acs < (-elim)) {
            goto L25;
        }
        zlog(s2r, s2i, csr, csi, idum);
        csr = csr - zdr;
        csi = csi - zdi;
        str = std::exp(csr) / tol;
        csr = str * std::cos(csi);
        csi = str * std::sin(csi);
        zuchk(csr, csi, nw, ascle, tol);
        if (nw != 0) {
            goto L25;
        }
        yr[(i)-1] = csr;
        yi[(i)-1] = csi;
        nz = nz - 1;
        if (ic == kk - 1) {
            goto L40;
        }
        ic = kk;
        goto L30;
    L25:;
        if (alas < helim) {
            goto L30;
        }
        zdr = zdr - elim;
        s1r = s1r * celmr;
        s1i = s1i * celmr;
        s2r = s2r * celmr;
        s2i = s2i * celmr;
    L30:;
    }
    nz = n;
    if (ic == n) {
        nz = n - 1;
    }
    goto L45;
L40:;
    nz = kk - 2;
L45:;
    lim2_ = nz;
    for (i = 1; i <= lim2_; i += 1) {
        yr[(i)-1] = zeror;
        yi[(i)-1] = zeroi;
        ;
    }
    return;
}
//=============================================================================
static void
zrati(double zr, double zi, double fnu, int n, double* cyr, double* cyi, double tol)
{
    const double onei = 0.0e0;
    const double oner = 1.0e0;
    const double czeroi = 0.0e0;
    const double czeror = 0.0e0;
    const double rt2 = 1.41421356237309505e0;
    double ak = 0;
    double amagz = 0;
    double ap1 = 0;
    double ap2 = 0;
    double arg = 0;
    double az = 0;
    double cdfnui = 0;
    double cdfnur = 0;
    double dfnu = 0;
    double fdnu = 0;
    double flam = 0;
    double fnup = 0;
    int i = 0;
    int id = 0;
    int idnu = 0;
    int inu = 0;
    int itime = 0;
    int k = 0;
    int kk = 0;
    int magz = 0;
    double p1i = 0;
    double p1r = 0;
    double p2i = 0;
    double p2r = 0;
    double pti = 0;
    double ptr = 0;
    double rak = 0;
    double rap1 = 0;
    double rho = 0;
    double rzi = 0;
    double rzr = 0;
    double t1i = 0;
    double t1r = 0;
    double test = 0;
    double test1 = 0;
    double tti = 0;
    double ttr = 0;
    int lim0_ = 0;
    int lim1_ = 0;
    az = zabs(zr, zi);
    inu = fortranInt(fnu);
    idnu = inu + n - 1;
    magz = fortranInt(az);
    amagz = (double)(magz + 1);
    fdnu = (double)(idnu);
    fnup = fmax_(amagz, fdnu);
    id = idnu - magz - 1;
    itime = 1;
    k = 1;
    ptr = 1.0e0 / az;
    rzr = ptr * (zr + zr) * ptr;
    rzi = -ptr * (zi + zi) * ptr;
    t1r = rzr * fnup;
    t1i = rzi * fnup;
    p2r = -t1r;
    p2i = -t1i;
    p1r = oner;
    p1i = onei;
    t1r = t1r + rzr;
    t1i = t1i + rzi;
    if (id > 0) {
        id = 0;
    }
    ap2 = zabs(p2r, p2i);
    ap1 = zabs(p1r, p1i);
    arg = (ap2 + ap2) / (ap1 * tol);
    test1 = std::sqrt(arg);
    test = test1;
    rap1 = 1.0e0 / ap1;
    p1r = p1r * rap1;
    p1i = p1i * rap1;
    p2r = p2r * rap1;
    p2i = p2i * rap1;
    ap2 = ap2 * rap1;
L10:;
    k = k + 1;
    ap1 = ap2;
    ptr = p2r;
    pti = p2i;
    p2r = p1r - (t1r * ptr - t1i * pti);
    p2i = p1i - (t1r * pti + t1i * ptr);
    p1r = ptr;
    p1i = pti;
    t1r = t1r + rzr;
    t1i = t1i + rzi;
    ap2 = zabs(p2r, p2i);
    if (ap1 <= test) {
        goto L10;
    }
    if (itime == 2) {
        goto L20;
    }
    ak = zabs(t1r, t1i) * 0.5e0;
    flam = ak + std::sqrt(ak * ak - 1.0e0);
    rho = fmin_(ap2 / ap1, flam);
    test = test1 * std::sqrt(rho / (rho * rho - 1.0e0));
    itime = 2;
    goto L10;
L20:;
    kk = k + 1 - id;
    ak = (double)(kk);
    t1r = ak;
    t1i = czeroi;
    dfnu = fnu + (double)(n - 1);
    p1r = 1.0e0 / ap2;
    p1i = czeroi;
    p2r = czeror;
    p2i = czeroi;
    lim0_ = kk;
    for (i = 1; i <= lim0_; i += 1) {
        ptr = p1r;
        pti = p1i;
        rap1 = dfnu + t1r;
        ttr = rzr * rap1;
        tti = rzi * rap1;
        p1r = (ptr * ttr - pti * tti) + p2r;
        p1i = (ptr * tti + pti * ttr) + p2i;
        p2r = ptr;
        p2i = pti;
        t1r = t1r - oner;
        ;
    }
    if (p1r != czeror || p1i != czeroi) {
        goto L40;
    }
    p1r = tol;
    p1i = tol;
L40:;
    zdiv(p2r, p2i, p1r, p1i, cyr[(n)-1], cyi[(n)-1]);
    if (n == 1) {
        return;
    }
    k = n - 1;
    ak = (double)(k);
    t1r = ak;
    t1i = czeroi;
    cdfnur = fnu * rzr;
    cdfnui = fnu * rzi;
    lim1_ = n;
    for (i = 2; i <= lim1_; i += 1) {
        ptr = cdfnur + (t1r * rzr - t1i * rzi) + cyr[(k + 1) - 1];
        pti = cdfnui + (t1r * rzi + t1i * rzr) + cyi[(k + 1) - 1];
        ak = zabs(ptr, pti);
        if (ak != czeror) {
            goto L50;
        }
        ptr = tol;
        pti = tol;
        ak = tol * rt2;
    L50:;
        rak = oner / ak;
        cyr[(k)-1] = rak * ptr * rak;
        cyi[(k)-1] = -rak * pti * rak;
        t1r = t1r - oner;
        k = k - 1;
        ;
    }
    return;
}
//=============================================================================
static void
zseri(double zr, double zi, double fnu, int kode, int n, double* yr, double* yi, int& nz,
    double tol, double elim, double alim)
{
    const double onei = 0.0e0;
    const double oner = 1.0e0;
    const double zeroi = 0.0e0;
    const double zeror = 0.0e0;
    double aa = 0;
    double acz = 0;
    double ak = 0;
    double ak1i = 0;
    double ak1r = 0;
    double arm = 0;
    double ascle = 0;
    double atol = 0;
    double az = 0;
    double cki = 0;
    double ckr = 0;
    double coefi = 0;
    double coefr = 0;
    double crscr = 0;
    double czi = 0;
    double czr = 0;
    double dfnu = 0;
    double fnup = 0;
    double hzi = 0;
    double hzr = 0;
    int i = 0;
    int ib = 0;
    int idum = 0;
    int iflag = 0;
    int il = 0;
    int k = 0;
    int l = 0;
    int m = 0;
    int nn = 0;
    int nw = 0;
    double raz = 0;
    double rs = 0;
    double rtr1 = 0;
    double rzi = 0;
    double rzr = 0;
    double s = 0;
    double s1i = 0;
    double s1r = 0;
    double s2i = 0;
    double s2r = 0;
    double ss = 0;
    double sti = 0;
    double str = 0;
    double wi[2] = {};
    double wr[2] = {};
    int lim0_ = 0;
    int lim1_ = 0;
    int lim2_ = 0;
    int lim3_ = 0;
    nz = 0;
    az = zabs(zr, zi);
    if (az == 0.0e0) {
        goto L160;
    }
    arm = 1.0e+3 * d1mach(1);
    rtr1 = std::sqrt(arm);
    crscr = 1.0e0;
    iflag = 0;
    if (az < arm) {
        goto L150;
    }
    hzr = 0.5e0 * zr;
    hzi = 0.5e0 * zi;
    czr = zeror;
    czi = zeroi;
    if (az <= rtr1) {
        goto L10;
    }
    zmlt(hzr, hzi, hzr, hzi, czr, czi);
L10:;
    acz = zabs(czr, czi);
    nn = n;
    zlog(hzr, hzi, ckr, cki, idum);
L20:;
    dfnu = fnu + (double)(nn - 1);
    fnup = dfnu + 1.0e0;
    ak1r = ckr * dfnu;
    ak1i = cki * dfnu;
    ak = dgamln(fnup, idum);
    ak1r = ak1r - ak;
    if (kode == 2) {
        ak1r = ak1r - zr;
    }
    if (ak1r > (-elim)) {
        goto L40;
    }
L30:;
    nz = nz + 1;
    yr[(nn)-1] = zeror;
    yi[(nn)-1] = zeroi;
    if (acz > dfnu) {
        goto L190;
    }
    nn = nn - 1;
    if (nn == 0) {
        return;
    }
    goto L20;
L40:;
    if (ak1r > (-alim)) {
        goto L50;
    }
    iflag = 1;
    ss = 1.0e0 / tol;
    crscr = tol;
    ascle = arm * ss;
L50:;
    aa = std::exp(ak1r);
    if (iflag == 1) {
        aa = aa * ss;
    }
    coefr = aa * std::cos(ak1i);
    coefi = aa * std::sin(ak1i);
    atol = tol * acz / fnup;
    il = imin_(2, nn);
    lim0_ = il;
    for (i = 1; i <= lim0_; i += 1) {
        dfnu = fnu + (double)(nn - i);
        fnup = dfnu + 1.0e0;
        s1r = oner;
        s1i = onei;
        if (acz < tol * fnup) {
            goto L70;
        }
        ak1r = oner;
        ak1i = onei;
        ak = fnup + 2.0e0;
        s = fnup;
        aa = 2.0e0;
    L60:;
        rs = 1.0e0 / s;
        str = ak1r * czr - ak1i * czi;
        sti = ak1r * czi + ak1i * czr;
        ak1r = str * rs;
        ak1i = sti * rs;
        s1r = s1r + ak1r;
        s1i = s1i + ak1i;
        s = s + ak;
        ak = ak + 2.0e0;
        aa = aa * acz * rs;
        if (aa > atol) {
            goto L60;
        }
    L70:;
        s2r = s1r * coefr - s1i * coefi;
        s2i = s1r * coefi + s1i * coefr;
        wr[(i)-1] = s2r;
        wi[(i)-1] = s2i;
        if (iflag == 0) {
            goto L80;
        }
        zuchk(s2r, s2i, nw, ascle, tol);
        if (nw != 0) {
            goto L30;
        }
    L80:;
        m = nn - i + 1;
        yr[(m)-1] = s2r * crscr;
        yi[(m)-1] = s2i * crscr;
        if (i == il) {
            goto L90;
        }
        zdiv(coefr, coefi, hzr, hzi, str, sti);
        coefr = str * dfnu;
        coefi = sti * dfnu;
    L90:;
    }
    if (nn <= 2) {
        return;
    }
    k = nn - 2;
    ak = (double)(k);
    raz = 1.0e0 / az;
    str = zr * raz;
    sti = -zi * raz;
    rzr = (str + str) * raz;
    rzi = (sti + sti) * raz;
    if (iflag == 1) {
        goto L120;
    }
    ib = 3;
L100:;
    lim1_ = nn;
    for (i = ib; i <= lim1_; i += 1) {
        yr[(k)-1] = (ak + fnu) * (rzr * yr[(k + 1) - 1] - rzi * yi[(k + 1) - 1]) + yr[(k + 2) - 1];
        yi[(k)-1] = (ak + fnu) * (rzr * yi[(k + 1) - 1] + rzi * yr[(k + 1) - 1]) + yi[(k + 2) - 1];
        ak = ak - 1.0e0;
        k = k - 1;
        ;
    }
    return;
L120:;
    s1r = wr[(1) - 1];
    s1i = wi[(1) - 1];
    s2r = wr[(2) - 1];
    s2i = wi[(2) - 1];
    lim2_ = nn;
    for (l = 3; l <= lim2_; l += 1) {
        ckr = s2r;
        cki = s2i;
        s2r = s1r + (ak + fnu) * (rzr * ckr - rzi * cki);
        s2i = s1i + (ak + fnu) * (rzr * cki + rzi * ckr);
        s1r = ckr;
        s1i = cki;
        ckr = s2r * crscr;
        cki = s2i * crscr;
        yr[(k)-1] = ckr;
        yi[(k)-1] = cki;
        ak = ak - 1.0e0;
        k = k - 1;
        if (zabs(ckr, cki) > ascle) {
            goto L140;
        };
    }
    return;
L140:;
    ib = l + 1;
    if (ib > nn) {
        return;
    }
    goto L100;
L150:;
    nz = n;
    if (fnu == 0.0e0) {
        nz = nz - 1;
    }
L160:;
    yr[(1) - 1] = zeror;
    yi[(1) - 1] = zeroi;
    if (fnu != 0.0e0) {
        goto L170;
    }
    yr[(1) - 1] = oner;
    yi[(1) - 1] = onei;
L170:;
    if (n == 1) {
        return;
    }
    lim3_ = n;
    for (i = 2; i <= lim3_; i += 1) {
        yr[(i)-1] = zeror;
        yi[(i)-1] = zeroi;
        ;
    }
    return;
L190:;
    nz = -nz;
    return;
}
//=============================================================================
static void
zasyi(double zr, double zi, double fnu, int kode, int n, double* yr, double* yi, int& nz, double rl,
    double tol, double elim, double alim)
{
    const double onei = 0.0e0;
    const double oner = 1.0e0;
    const double pi = 3.14159265358979324e0;
    const double rtpi = 0.159154943091895336e0;
    const double zeroi = 0.0e0;
    const double zeror = 0.0e0;
    double aa = 0;
    double aez = 0;
    double ak = 0;
    double ak1i = 0;
    double ak1r = 0;
    double arg = 0;
    double arm = 0;
    double atol = 0;
    double az = 0;
    double bb = 0;
    double bk = 0;
    double cki = 0;
    double ckr = 0;
    double cs1i = 0;
    double cs1r = 0;
    double cs2i = 0;
    double cs2r = 0;
    double czi = 0;
    double czr = 0;
    double dfnu = 0;
    double dki = 0;
    double dkr = 0;
    double dnu2 = 0;
    double ezi = 0;
    double ezr = 0;
    double fdn = 0;
    int i = 0;
    int ib = 0;
    int il = 0;
    int inu = 0;
    int j = 0;
    int jl = 0;
    int k = 0;
    int koded = 0;
    int m = 0;
    int nn = 0;
    double p1i = 0;
    double p1r = 0;
    double raz = 0;
    double rtr1 = 0;
    double rzi = 0;
    double rzr = 0;
    double s = 0;
    double s2i = 0;
    double s2r = 0;
    double sgn = 0;
    double sqk = 0;
    double sti = 0;
    double str = 0;
    double tzi = 0;
    double tzr = 0;
    int lim0_ = 0;
    int lim1_ = 0;
    int lim2_ = 0;
    int lim3_ = 0;
    nz = 0;
    az = zabs(zr, zi);
    arm = 1.0e+3 * d1mach(1);
    rtr1 = std::sqrt(arm);
    il = imin_(2, n);
    dfnu = fnu + (double)(n - il);
    raz = 1.0e0 / az;
    str = zr * raz;
    sti = -zi * raz;
    ak1r = rtpi * str * raz;
    ak1i = rtpi * sti * raz;
    zsqrt(ak1r, ak1i, ak1r, ak1i);
    czr = zr;
    czi = zi;
    if (kode != 2) {
        goto L10;
    }
    czr = zeror;
    czi = zi;
L10:;
    if (std::fabs(czr) > elim) {
        goto L100;
    }
    dnu2 = dfnu + dfnu;
    koded = 1;
    if ((std::fabs(czr) > alim) && (n > 2)) {
        goto L20;
    }
    koded = 0;
    zexp(czr, czi, str, sti);
    zmlt(ak1r, ak1i, str, sti, ak1r, ak1i);
L20:;
    fdn = 0.0e0;
    if (dnu2 > rtr1) {
        fdn = dnu2 * dnu2;
    }
    ezr = zr * 8.0e0;
    ezi = zi * 8.0e0;
    aez = 8.0e0 * az;
    s = tol / aez;
    jl = fortranInt(rl + rl) + 2;
    p1r = zeror;
    p1i = zeroi;
    if (zi == 0.0e0) {
        goto L30;
    }
    inu = fortranInt(fnu);
    arg = (fnu - (double)(inu)) * pi;
    inu = inu + n - il;
    ak = -std::sin(arg);
    bk = std::cos(arg);
    if (zi < 0.0e0) {
        bk = -bk;
    }
    p1r = ak;
    p1i = bk;
    if (imod_(inu, 2) == 0) {
        goto L30;
    }
    p1r = -p1r;
    p1i = -p1i;
L30:;
    lim0_ = il;
    for (k = 1; k <= lim0_; k += 1) {
        sqk = fdn - 1.0e0;
        atol = s * std::fabs(sqk);
        sgn = 1.0e0;
        cs1r = oner;
        cs1i = onei;
        cs2r = oner;
        cs2i = onei;
        ckr = oner;
        cki = onei;
        ak = 0.0e0;
        aa = 1.0e0;
        bb = aez;
        dkr = ezr;
        dki = ezi;
        lim1_ = jl;
        for (j = 1; j <= lim1_; j += 1) {
            zdiv(ckr, cki, dkr, dki, str, sti);
            ckr = str * sqk;
            cki = sti * sqk;
            cs2r = cs2r + ckr;
            cs2i = cs2i + cki;
            sgn = -sgn;
            cs1r = cs1r + ckr * sgn;
            cs1i = cs1i + cki * sgn;
            dkr = dkr + ezr;
            dki = dki + ezi;
            aa = aa * std::fabs(sqk) / bb;
            bb = bb + aez;
            ak = ak + 8.0e0;
            sqk = sqk - ak;
            if (aa <= atol) {
                goto L50;
            };
        }
        goto L110;
    L50:;
        s2r = cs1r;
        s2i = cs1i;
        if (zr + zr >= elim) {
            goto L60;
        }
        tzr = zr + zr;
        tzi = zi + zi;
        zexp(-tzr, -tzi, str, sti);
        zmlt(str, sti, p1r, p1i, str, sti);
        zmlt(str, sti, cs2r, cs2i, str, sti);
        s2r = s2r + str;
        s2i = s2i + sti;
    L60:;
        fdn = fdn + 8.0e0 * dfnu + 4.0e0;
        p1r = -p1r;
        p1i = -p1i;
        m = n - il + k;
        yr[(m)-1] = s2r * ak1r - s2i * ak1i;
        yi[(m)-1] = s2r * ak1i + s2i * ak1r;
        ;
    }
    if (n <= 2) {
        return;
    }
    nn = n;
    k = nn - 2;
    ak = (double)(k);
    str = zr * raz;
    sti = -zi * raz;
    rzr = (str + str) * raz;
    rzi = (sti + sti) * raz;
    ib = 3;
    lim2_ = nn;
    for (i = ib; i <= lim2_; i += 1) {
        yr[(k)-1] = (ak + fnu) * (rzr * yr[(k + 1) - 1] - rzi * yi[(k + 1) - 1]) + yr[(k + 2) - 1];
        yi[(k)-1] = (ak + fnu) * (rzr * yi[(k + 1) - 1] + rzi * yr[(k + 1) - 1]) + yi[(k + 2) - 1];
        ak = ak - 1.0e0;
        k = k - 1;
        ;
    }
    if (koded == 0) {
        return;
    }
    zexp(czr, czi, ckr, cki);
    lim3_ = nn;
    for (i = 1; i <= lim3_; i += 1) {
        str = yr[(i)-1] * ckr - yi[(i)-1] * cki;
        yi[(i)-1] = yr[(i)-1] * cki + yi[(i)-1] * ckr;
        yr[(i)-1] = str;
        ;
    }
    return;
L100:;
    nz = -1;
    return;
L110:;
    nz = -2;
    return;
}
//=============================================================================
static void
zmlri(
    double zr, double zi, double fnu, int kode, int n, double* yr, double* yi, int& nz, double tol)
{
    const double onei = 0.0e0;
    const double oner = 1.0e0;
    const double zeroi = 0.0e0;
    const double zeror = 0.0e0;
    double ack = 0;
    double ak = 0;
    double ap = 0;
    double at = 0;
    double az = 0;
    double bk = 0;
    double cki = 0;
    double ckr = 0;
    double cnormi = 0;
    double cnormr = 0;
    double fkap = 0;
    double fkk = 0;
    double flam = 0;
    double fnf = 0;
    int i = 0;
    int iaz = 0;
    int idum = 0;
    int ifnu = 0;
    int inu = 0;
    int itime = 0;
    int k = 0;
    int kk = 0;
    int km = 0;
    int m = 0;
    double p1i = 0;
    double p1r = 0;
    double p2i = 0;
    double p2r = 0;
    double pti = 0;
    double ptr = 0;
    double raz = 0;
    double rho = 0;
    double rho2 = 0;
    double rzi = 0;
    double rzr = 0;
    double scle = 0;
    double sti = 0;
    double str = 0;
    double sumi = 0;
    double sumr = 0;
    double tfnf = 0;
    double tst = 0;
    int lim0_ = 0;
    int lim1_ = 0;
    int lim2_ = 0;
    int lim3_ = 0;
    int lim4_ = 0;
    int lim5_ = 0;
    scle = d1mach(1) / tol;
    nz = 0;
    az = zabs(zr, zi);
    iaz = fortranInt(az);
    ifnu = fortranInt(fnu);
    inu = ifnu + n - 1;
    at = (double)(iaz) + 1.0e0;
    raz = 1.0e0 / az;
    str = zr * raz;
    sti = -zi * raz;
    ckr = str * at * raz;
    cki = sti * at * raz;
    rzr = (str + str) * raz;
    rzi = (sti + sti) * raz;
    p1r = zeror;
    p1i = zeroi;
    p2r = oner;
    p2i = onei;
    ack = (at + 1.0e0) * raz;
    rho = ack + std::sqrt(ack * ack - 1.0e0);
    rho2 = rho * rho;
    tst = (rho2 + rho2) / ((rho2 - 1.0e0) * (rho - 1.0e0));
    tst = tst / tol;
    ak = at;
    lim0_ = 80;
    for (i = 1; i <= lim0_; i += 1) {
        ptr = p2r;
        pti = p2i;
        p2r = p1r - (ckr * ptr - cki * pti);
        p2i = p1i - (cki * ptr + ckr * pti);
        p1r = ptr;
        p1i = pti;
        ckr = ckr + rzr;
        cki = cki + rzi;
        ap = zabs(p2r, p2i);
        if (ap > tst * ak * ak) {
            goto L20;
        }
        ak = ak + 1.0e0;
        ;
    }
    goto L110;
L20:;
    i = i + 1;
    k = 0;
    if (inu < iaz) {
        goto L40;
    }
    p1r = zeror;
    p1i = zeroi;
    p2r = oner;
    p2i = onei;
    at = (double)(inu) + 1.0e0;
    str = zr * raz;
    sti = -zi * raz;
    ckr = str * at * raz;
    cki = sti * at * raz;
    ack = at * raz;
    tst = std::sqrt(ack / tol);
    itime = 1;
    lim1_ = 80;
    for (k = 1; k <= lim1_; k += 1) {
        ptr = p2r;
        pti = p2i;
        p2r = p1r - (ckr * ptr - cki * pti);
        p2i = p1i - (ckr * pti + cki * ptr);
        p1r = ptr;
        p1i = pti;
        ckr = ckr + rzr;
        cki = cki + rzi;
        ap = zabs(p2r, p2i);
        if (ap < tst) {
            goto L30;
        }
        if (itime == 2) {
            goto L40;
        }
        ack = zabs(ckr, cki);
        flam = ack + std::sqrt(ack * ack - 1.0e0);
        fkap = ap / zabs(p1r, p1i);
        rho = fmin_(flam, fkap);
        tst = tst * std::sqrt(rho / (rho * rho - 1.0e0));
        itime = 2;
    L30:;
    }
    goto L110;
L40:;
    k = k + 1;
    kk = imax_(i + iaz, k + inu);
    fkk = (double)(kk);
    p1r = zeror;
    p1i = zeroi;
    p2r = scle;
    p2i = zeroi;
    fnf = fnu - (double)(ifnu);
    tfnf = fnf + fnf;
    bk = dgamln(fkk + tfnf + 1.0e0, idum) - dgamln(fkk + 1.0e0, idum) - dgamln(tfnf + 1.0e0, idum);
    bk = std::exp(bk);
    sumr = zeror;
    sumi = zeroi;
    km = kk - inu;
    lim2_ = km;
    for (i = 1; i <= lim2_; i += 1) {
        ptr = p2r;
        pti = p2i;
        p2r = p1r + (fkk + fnf) * (rzr * ptr - rzi * pti);
        p2i = p1i + (fkk + fnf) * (rzi * ptr + rzr * pti);
        p1r = ptr;
        p1i = pti;
        ak = 1.0e0 - tfnf / (fkk + tfnf);
        ack = bk * ak;
        sumr = sumr + (ack + bk) * p1r;
        sumi = sumi + (ack + bk) * p1i;
        bk = ack;
        fkk = fkk - 1.0e0;
        ;
    }
    yr[(n)-1] = p2r;
    yi[(n)-1] = p2i;
    if (n == 1) {
        goto L70;
    }
    lim3_ = n;
    for (i = 2; i <= lim3_; i += 1) {
        ptr = p2r;
        pti = p2i;
        p2r = p1r + (fkk + fnf) * (rzr * ptr - rzi * pti);
        p2i = p1i + (fkk + fnf) * (rzi * ptr + rzr * pti);
        p1r = ptr;
        p1i = pti;
        ak = 1.0e0 - tfnf / (fkk + tfnf);
        ack = bk * ak;
        sumr = sumr + (ack + bk) * p1r;
        sumi = sumi + (ack + bk) * p1i;
        bk = ack;
        fkk = fkk - 1.0e0;
        m = n - i + 1;
        yr[(m)-1] = p2r;
        yi[(m)-1] = p2i;
        ;
    }
L70:;
    if (ifnu <= 0) {
        goto L90;
    }
    lim4_ = ifnu;
    for (i = 1; i <= lim4_; i += 1) {
        ptr = p2r;
        pti = p2i;
        p2r = p1r + (fkk + fnf) * (rzr * ptr - rzi * pti);
        p2i = p1i + (fkk + fnf) * (rzr * pti + rzi * ptr);
        p1r = ptr;
        p1i = pti;
        ak = 1.0e0 - tfnf / (fkk + tfnf);
        ack = bk * ak;
        sumr = sumr + (ack + bk) * p1r;
        sumi = sumi + (ack + bk) * p1i;
        bk = ack;
        fkk = fkk - 1.0e0;
        ;
    }
L90:;
    ptr = zr;
    pti = zi;
    if (kode == 2) {
        ptr = zeror;
    }
    zlog(rzr, rzi, str, sti, idum);
    p1r = -fnf * str + ptr;
    p1i = -fnf * sti + pti;
    ap = dgamln(1.0e0 + fnf, idum);
    ptr = p1r - ap;
    pti = p1i;
    p2r = p2r + sumr;
    p2i = p2i + sumi;
    ap = zabs(p2r, p2i);
    p1r = 1.0e0 / ap;
    zexp(ptr, pti, str, sti);
    ckr = str * p1r;
    cki = sti * p1r;
    ptr = p2r * p1r;
    pti = -p2i * p1r;
    zmlt(ckr, cki, ptr, pti, cnormr, cnormi);
    lim5_ = n;
    for (i = 1; i <= lim5_; i += 1) {
        str = yr[(i)-1] * cnormr - yi[(i)-1] * cnormi;
        yi[(i)-1] = yr[(i)-1] * cnormi + yi[(i)-1] * cnormr;
        yr[(i)-1] = str;
        ;
    }
    return;
L110:;
    nz = -2;
    return;
}
//=============================================================================
static void
zunik(double zrr, double zri, double fnu, int ikflg, int ipmtr, double tol, int& init, double& phir,
    double& phii, double& zeta1r, double& zeta1i, double& zeta2r, double& zeta2i, double& sumr,
    double& sumi, double* cwrkr, double* cwrki)
{
    const double onei = 0.0e0;
    const double oner = 1.0e0;
    const double zeroi = 0.0e0;
    const double zeror = 0.0e0;
    static const double c[120]
        = { 1.00000000000000000e+00, -2.08333333333333333e-01, 1.25000000000000000e-01,
              3.34201388888888889e-01, -4.01041666666666667e-01, 7.03125000000000000e-02,
              -1.02581259645061728e+00, 1.84646267361111111e+00, -8.91210937500000000e-01,
              7.32421875000000000e-02, 4.66958442342624743e+00, -1.12070026162229938e+01,
              8.78912353515625000e+00, -2.36408691406250000e+00, 1.12152099609375000e-01,
              -2.82120725582002449e+01, 8.46362176746007346e+01, -9.18182415432400174e+01,
              4.25349987453884549e+01, -7.36879435947963170e+00, 2.27108001708984375e-01,
              2.12570130039217123e+02, -7.65252468141181642e+02, 1.05999045252799988e+03,
              -6.99579627376132541e+02, 2.18190511744211590e+02, -2.64914304869515555e+01,
              5.72501420974731445e-01, -1.91945766231840700e+03, 8.06172218173730938e+03,
              -1.35865500064341374e+04, 1.16553933368645332e+04, -5.30564697861340311e+03,
              1.20090291321635246e+03, -1.08090919788394656e+02, 1.72772750258445740e+00,
              2.02042913309661486e+04, -9.69805983886375135e+04, 1.92547001232531532e+05,
              -2.03400177280415534e+05, 1.22200464983017460e+05, -4.11926549688975513e+04,
              7.10951430248936372e+03, -4.93915304773088012e+02, 6.07404200127348304e+00,
              -2.42919187900551333e+05, 1.31176361466297720e+06, -2.99801591853810675e+06,
              3.76327129765640400e+06, -2.81356322658653411e+06, 1.26836527332162478e+06,
              -3.31645172484563578e+05, 4.52187689813627263e+04, -2.49983048181120962e+03,
              2.43805296995560639e+01, 3.28446985307203782e+06, -1.97068191184322269e+07,
              5.09526024926646422e+07, -7.41051482115326577e+07, 6.63445122747290267e+07,
              -3.75671766607633513e+07, 1.32887671664218183e+07, -2.78561812808645469e+06,
              3.08186404612662398e+05, -1.38860897537170405e+04, 1.10017140269246738e+02,
              -4.93292536645099620e+07, 3.25573074185765749e+08, -9.39462359681578403e+08,
              1.55359689957058006e+09, -1.62108055210833708e+09, 1.10684281682301447e+09,
              -4.95889784275030309e+08, 1.42062907797533095e+08, -2.44740627257387285e+07,
              2.24376817792244943e+06, -8.40054336030240853e+04, 5.51335896122020586e+02,
              8.14789096118312115e+08, -5.86648149205184723e+09, 1.86882075092958249e+10,
              -3.46320433881587779e+10, 4.12801855797539740e+10, -3.30265997498007231e+10,
              1.79542137311556001e+10, -6.56329379261928433e+09, 1.55927986487925751e+09,
              -2.25105661889415278e+08, 1.73951075539781645e+07, -5.49842327572288687e+05,
              3.03809051092238427e+03, -1.46792612476956167e+10, 1.14498237732025810e+11,
              -3.99096175224466498e+11, 8.19218669548577329e+11, -1.09837515608122331e+12,
              1.00815810686538209e+12, -6.45364869245376503e+11, 2.87900649906150589e+11,
              -8.78670721780232657e+10, 1.76347306068349694e+10, -2.16716498322379509e+09,
              1.43157876718888981e+08, -3.87183344257261262e+06, 1.82577554742931747e+04,
              2.86464035717679043e+11, -2.40629790002850396e+12, 9.10934118523989896e+12,
              -2.05168994109344374e+13, 3.05651255199353206e+13, -3.16670885847851584e+13,
              2.33483640445818409e+13, -1.23204913055982872e+13, 4.61272578084913197e+12,
              -1.19655288019618160e+12, 2.05914503232410016e+11, -2.18229277575292237e+10,
              1.24700929351271032e+09, -2.91883881222208134e+07, 1.18838426256783253e+05 };
    static const double con[2] = { 3.98942280401432678e-01, 1.25331413731550025e+00 };
    double ac = 0;
    double crfni = 0;
    double crfnr = 0;
    int i = 0;
    int idum = 0;
    int j = 0;
    int k = 0;
    int l = 0;
    double rfn = 0;
    double si = 0;
    double sr = 0;
    double sri = 0;
    double srr = 0;
    double sti = 0;
    double str = 0;
    double t2i = 0;
    double t2r = 0;
    double test = 0;
    double ti = 0;
    double tr = 0;
    double zni = 0;
    double znr = 0;
    int lim0_ = 0;
    int lim1_ = 0;
    int lim2_ = 0;
    int lim3_ = 0;
    if (init != 0) {
        goto L40;
    }
    rfn = 1.0e0 / fnu;
    test = d1mach(1) * 1.0e+3;
    ac = fnu * test;
    if (std::fabs(zrr) > ac || std::fabs(zri) > ac) {
        goto L15;
    }
    zeta1r = 2.0e0 * std::fabs(std::log(test)) + fnu;
    zeta1i = 0.0e0;
    zeta2r = fnu;
    zeta2i = 0.0e0;
    phir = 1.0e0;
    phii = 0.0e0;
    return;
L15:;
    tr = zrr * rfn;
    ti = zri * rfn;
    sr = oner + (tr * tr - ti * ti);
    si = onei + (tr * ti + ti * tr);
    zsqrt(sr, si, srr, sri);
    str = oner + srr;
    sti = onei + sri;
    zdiv(str, sti, tr, ti, znr, zni);
    zlog(znr, zni, str, sti, idum);
    zeta1r = fnu * str;
    zeta1i = fnu * sti;
    zeta2r = fnu * srr;
    zeta2i = fnu * sri;
    zdiv(oner, onei, srr, sri, tr, ti);
    srr = tr * rfn;
    sri = ti * rfn;
    zsqrt(srr, sri, cwrkr[(16) - 1], cwrki[(16) - 1]);
    phir = cwrkr[(16) - 1] * con[(ikflg)-1];
    phii = cwrki[(16) - 1] * con[(ikflg)-1];
    if (ipmtr != 0) {
        return;
    }
    zdiv(oner, onei, sr, si, t2r, t2i);
    cwrkr[(1) - 1] = oner;
    cwrki[(1) - 1] = onei;
    crfnr = oner;
    crfni = onei;
    ac = 1.0e0;
    l = 1;
    lim0_ = 15;
    for (k = 2; k <= lim0_; k += 1) {
        sr = zeror;
        si = zeroi;
        lim1_ = k;
        for (j = 1; j <= lim1_; j += 1) {
            l = l + 1;
            str = sr * t2r - si * t2i + c[(l)-1];
            si = sr * t2i + si * t2r;
            sr = str;
            ;
        }
        str = crfnr * srr - crfni * sri;
        crfni = crfnr * sri + crfni * srr;
        crfnr = str;
        cwrkr[(k)-1] = crfnr * sr - crfni * si;
        cwrki[(k)-1] = crfnr * si + crfni * sr;
        ac = ac * rfn;
        test = std::fabs(cwrkr[(k)-1]) + std::fabs(cwrki[(k)-1]);
        if (ac < tol && test < tol) {
            goto L30;
        };
    }
    k = 15;
L30:;
    init = k;
L40:;
    if (ikflg == 2) {
        goto L60;
    }
    sr = zeror;
    si = zeroi;
    lim2_ = init;
    for (i = 1; i <= lim2_; i += 1) {
        sr = sr + cwrkr[(i)-1];
        si = si + cwrki[(i)-1];
        ;
    }
    sumr = sr;
    sumi = si;
    phir = cwrkr[(16) - 1] * con[(1) - 1];
    phii = cwrki[(16) - 1] * con[(1) - 1];
    return;
L60:;
    sr = zeror;
    si = zeroi;
    tr = oner;
    lim3_ = init;
    for (i = 1; i <= lim3_; i += 1) {
        sr = sr + tr * cwrkr[(i)-1];
        si = si + tr * cwrki[(i)-1];
        tr = -tr;
        ;
    }
    sumr = sr;
    sumi = si;
    phir = cwrkr[(16) - 1] * con[(2) - 1];
    phii = cwrki[(16) - 1] * con[(2) - 1];
    return;
}
//=============================================================================
static void
zunhj(double zr, double zi, double fnu, int ipmtr, double tol, double& phir, double& phii,
    double& argr, double& argi, double& zeta1r, double& zeta1i, double& zeta2r, double& zeta2i,
    double& asumr, double& asumi, double& bsumr, double& bsumi)
{
    const double onei = 0.0e0;
    const double oner = 1.0e0;
    const double ex1 = 3.33333333333333333e-01;
    const double ex2 = 6.66666666666666667e-01;
    const double gpi = 3.14159265358979324e+00;
    const double hpi = 1.57079632679489662e+00;
    const double thpi = 4.71238898038468986e+00;
    const double zeroi = 0.0e0;
    const double zeror = 0.0e0;
    static const double alfa[180]
        = { -4.44444444444444444e-03, -9.22077922077922078e-04, -8.84892884892884893e-05,
              1.65927687832449737e-04, 2.46691372741792910e-04, 2.65995589346254780e-04,
              2.61824297061500945e-04, 2.48730437344655609e-04, 2.32721040083232098e-04,
              2.16362485712365082e-04, 2.00738858762752355e-04, 1.86267636637545172e-04,
              1.73060775917876493e-04, 1.61091705929015752e-04, 1.50274774160908134e-04,
              1.40503497391269794e-04, 1.31668816545922806e-04, 1.23667445598253261e-04,
              1.16405271474737902e-04, 1.09798298372713369e-04, 1.03772410422992823e-04,
              9.82626078369363448e-05, 9.32120517249503256e-05, 8.85710852478711718e-05,
              8.42963105715700223e-05, 8.03497548407791151e-05, 7.66981345359207388e-05,
              7.33122157481777809e-05, 7.01662625163141333e-05, 6.72375633790160292e-05,
              6.93735541354588974e-04, 2.32241745182921654e-04, -1.41986273556691197e-05,
              -1.16444931672048640e-04, -1.50803558053048762e-04, -1.55121924918096223e-04,
              -1.46809756646465549e-04, -1.33815503867491367e-04, -1.19744975684254051e-04,
              -1.06184319207974020e-04, -9.37699549891194492e-05, -8.26923045588193274e-05,
              -7.29374348155221211e-05, -6.44042357721016283e-05, -5.69611566009369048e-05,
              -5.04731044303561628e-05, -4.48134868008882786e-05, -3.98688727717598864e-05,
              -3.55400532972042498e-05, -3.17414256609022480e-05, -2.83996793904174811e-05,
              -2.54522720634870566e-05, -2.28459297164724555e-05, -2.05352753106480604e-05,
              -1.84816217627666085e-05, -1.66519330021393806e-05, -1.50179412980119482e-05,
              -1.35554031379040526e-05, -1.22434746473858131e-05, -1.10641884811308169e-05,
              -3.54211971457743841e-04, -1.56161263945159416e-04, 3.04465503594936410e-05,
              1.30198655773242693e-04, 1.67471106699712269e-04, 1.70222587683592569e-04,
              1.56501427608594704e-04, 1.36339170977445120e-04, 1.14886692029825128e-04,
              9.45869093034688111e-05, 7.64498419250898258e-05, 6.07570334965197354e-05,
              4.74394299290508799e-05, 3.62757512005344297e-05, 2.69939714979224901e-05,
              1.93210938247939253e-05, 1.30056674793963203e-05, 7.82620866744496661e-06,
              3.59257485819351583e-06, 1.44040049814251817e-07, -2.65396769697939116e-06,
              -4.91346867098485910e-06, -6.72739296091248287e-06, -8.17269379678657923e-06,
              -9.31304715093561232e-06, -1.02011418798016441e-05, -1.08805962510592880e-05,
              -1.13875481509603555e-05, -1.17519675674556414e-05, -1.19987364870944141e-05,
              3.78194199201772914e-04, 2.02471952761816167e-04, -6.37938506318862408e-05,
              -2.38598230603005903e-04, -3.10916256027361568e-04, -3.13680115247576316e-04,
              -2.78950273791323387e-04, -2.28564082619141374e-04, -1.75245280340846749e-04,
              -1.25544063060690348e-04, -8.22982872820208365e-05, -4.62860730588116458e-05,
              -1.72334302366962267e-05, 5.60690482304602267e-06, 2.31395443148286800e-05,
              3.62642745856793957e-05, 4.58006124490188752e-05, 5.24595294959114050e-05,
              5.68396208545815266e-05, 5.94349820393104052e-05, 6.06478527578421742e-05,
              6.08023907788436497e-05, 6.01577894539460388e-05, 5.89199657344698500e-05,
              5.72515823777593053e-05, 5.52804375585852577e-05, 5.31063773802880170e-05,
              5.08069302012325706e-05, 4.84418647620094842e-05, 4.60568581607475370e-05,
              -6.91141397288294174e-04, -4.29976633058871912e-04, 1.83067735980039018e-04,
              6.60088147542014144e-04, 8.75964969951185931e-04, 8.77335235958235514e-04,
              7.49369585378990637e-04, 5.63832329756980918e-04, 3.68059319971443156e-04,
              1.88464535514455599e-04, 3.70663057664904149e-05, -8.28520220232137023e-05,
              -1.72751952869172998e-04, -2.36314873605872983e-04, -2.77966150694906658e-04,
              -3.02079514155456919e-04, -3.12594712643820127e-04, -3.12872558758067163e-04,
              -3.05678038466324377e-04, -2.93226470614557331e-04, -2.77255655582934777e-04,
              -2.59103928467031709e-04, -2.39784014396480342e-04, -2.20048260045422848e-04,
              -2.00443911094971498e-04, -1.81358692210970687e-04, -1.63057674478657464e-04,
              -1.45712672175205844e-04, -1.29425421983924587e-04, -1.14245691942445952e-04,
              1.92821964248775885e-03, 1.35592576302022234e-03, -7.17858090421302995e-04,
              -2.58084802575270346e-03, -3.49271130826168475e-03, -3.46986299340960628e-03,
              -2.82285233351310182e-03, -1.88103076404891354e-03, -8.89531718383947600e-04,
              3.87912102631035228e-06, 7.28688540119691412e-04, 1.26566373053457758e-03,
              1.62518158372674427e-03, 1.83203153216373172e-03, 1.91588388990527909e-03,
              1.90588846755546138e-03, 1.82798982421825727e-03, 1.70389506421121530e-03,
              1.55097127171097686e-03, 1.38261421852276159e-03, 1.20881424230064774e-03,
              1.03676532638344962e-03, 8.71437918068619115e-04, 7.16080155297701002e-04,
              5.72637002558129372e-04, 4.42089819465802277e-04, 3.24724948503090564e-04,
              2.20342042730246599e-04, 1.28412898401353882e-04, 4.82005924552095464e-05 };
    static const double ar[14] = { 1.00000000000000000e+00, 1.04166666666666667e-01,
        8.35503472222222222e-02, 1.28226574556327160e-01, 2.91849026464140464e-01,
        8.81627267443757652e-01, 3.32140828186276754e+00, 1.49957629868625547e+01,
        7.89230130115865181e+01, 4.74451538868264323e+02, 3.20749009089066193e+03,
        2.40865496408740049e+04, 1.98923119169509794e+05, 1.79190200777534383e+06 };
    static const double beta[210]
        = { 1.79988721413553309e-02, 5.59964911064388073e-03, 2.88501402231132779e-03,
              1.80096606761053941e-03, 1.24753110589199202e-03, 9.22878876572938311e-04,
              7.14430421727287357e-04, 5.71787281789704872e-04, 4.69431007606481533e-04,
              3.93232835462916638e-04, 3.34818889318297664e-04, 2.88952148495751517e-04,
              2.52211615549573284e-04, 2.22280580798883327e-04, 1.97541838033062524e-04,
              1.76836855019718004e-04, 1.59316899661821081e-04, 1.44347930197333986e-04,
              1.31448068119965379e-04, 1.20245444949302884e-04, 1.10449144504599392e-04,
              1.01828770740567258e-04, 9.41998224204237509e-05, 8.74130545753834437e-05,
              8.13466262162801467e-05, 7.59002269646219339e-05, 7.09906300634153481e-05,
              6.65482874842468183e-05, 6.25146958969275078e-05, 5.88403394426251749e-05,
              -1.49282953213429172e-03, -8.78204709546389328e-04, -5.02916549572034614e-04,
              -2.94822138512746025e-04, -1.75463996970782828e-04, -1.04008550460816434e-04,
              -5.96141953046457895e-05, -3.12038929076098340e-05, -1.26089735980230047e-05,
              -2.42892608575730389e-07, 8.05996165414273571e-06, 1.36507009262147391e-05,
              1.73964125472926261e-05, 1.98672978842133780e-05, 2.14463263790822639e-05,
              2.23954659232456514e-05, 2.28967783814712629e-05, 2.30785389811177817e-05,
              2.30321976080909144e-05, 2.28236073720348722e-05, 2.25005881105292418e-05,
              2.20981015361991429e-05, 2.16418427448103905e-05, 2.11507649256220843e-05,
              2.06388749782170737e-05, 2.01165241997081666e-05, 1.95913450141179244e-05,
              1.90689367910436740e-05, 1.85533719641636667e-05, 1.80475722259674218e-05,
              5.52213076721292790e-04, 4.47932581552384646e-04, 2.79520653992020589e-04,
              1.52468156198446602e-04, 6.93271105657043598e-05, 1.76258683069991397e-05,
              -1.35744996343269136e-05, -3.17972413350427135e-05, -4.18861861696693365e-05,
              -4.69004889379141029e-05, -4.87665447413787352e-05, -4.87010031186735069e-05,
              -4.74755620890086638e-05, -4.55813058138628452e-05, -4.33309644511266036e-05,
              -4.09230193157750364e-05, -3.84822638603221274e-05, -3.60857167535410501e-05,
              -3.37793306123367417e-05, -3.15888560772109621e-05, -2.95269561750807315e-05,
              -2.75978914828335759e-05, -2.58006174666883713e-05, -2.41308356761280200e-05,
              -2.25823509518346033e-05, -2.11479656768912971e-05, -1.98200638885294927e-05,
              -1.85909870801065077e-05, -1.74532699844210224e-05, -1.63997823854497997e-05,
              -4.74617796559959808e-04, -4.77864567147321487e-04, -3.20390228067037603e-04,
              -1.61105016119962282e-04, -4.25778101285435204e-05, 3.44571294294967503e-05,
              7.97092684075674924e-05, 1.03138236708272200e-04, 1.12466775262204158e-04,
              1.13103642108481389e-04, 1.08651634848774268e-04, 1.01437951597661973e-04,
              9.29298396593363896e-05, 8.40293133016089978e-05, 7.52727991349134062e-05,
              6.69632521975730872e-05, 5.92564547323194704e-05, 5.22169308826975567e-05,
              4.58539485165360646e-05, 4.01445513891486808e-05, 3.50481730031328081e-05,
              3.05157995034346659e-05, 2.64956119950516039e-05, 2.29363633690998152e-05,
              1.97893056664021636e-05, 1.70091984636412623e-05, 1.45547428261524004e-05,
              1.23886640995878413e-05, 1.04775876076583236e-05, 8.79179954978479373e-06,
              7.36465810572578444e-04, 8.72790805146193976e-04, 6.22614862573135066e-04,
              2.85998154194304147e-04, 3.84737672879366102e-06, -1.87906003636971558e-04,
              -2.97603646594554535e-04, -3.45998126832656348e-04, -3.53382470916037712e-04,
              -3.35715635775048757e-04, -3.04321124789039809e-04, -2.66722723047612821e-04,
              -2.27654214122819527e-04, -1.89922611854562356e-04, -1.55058918599093870e-04,
              -1.23778240761873630e-04, -9.62926147717644187e-05, -7.25178327714425337e-05,
              -5.22070028895633801e-05, -3.50347750511900522e-05, -2.06489761035551757e-05,
              -8.70106096849767054e-06, 1.13698686675100290e-06, 9.16426474122778849e-06,
              1.56477785428872620e-05, 2.08223629482466847e-05, 2.48923381004595156e-05,
              2.80340509574146325e-05, 3.03987774629861915e-05, 3.21156731406700616e-05,
              -1.80182191963885708e-03, -2.43402962938042533e-03, -1.83422663549856802e-03,
              -7.62204596354009765e-04, 2.39079475256927218e-04, 9.49266117176881141e-04,
              1.34467449701540359e-03, 1.48457495259449178e-03, 1.44732339830617591e-03,
              1.30268261285657186e-03, 1.10351597375642682e-03, 8.86047440419791759e-04,
              6.73073208165665473e-04, 4.77603872856582378e-04, 3.05991926358789362e-04,
              1.60315694594721630e-04, 4.00749555270613286e-05, -5.66607461635251611e-05,
              -1.32506186772982638e-04, -1.90296187989614057e-04, -2.32811450376937408e-04,
              -2.62628811464668841e-04, -2.82050469867598672e-04, -2.93081563192861167e-04,
              -2.97435962176316616e-04, -2.96557334239348078e-04, -2.91647363312090861e-04,
              -2.83696203837734166e-04, -2.73512317095673346e-04, -2.61750155806768580e-04,
              6.38585891212050914e-03, 9.62374215806377941e-03, 7.61878061207001043e-03,
              2.83219055545628054e-03, -2.09841352012720090e-03, -5.73826764216626498e-03,
              -7.70804244495414620e-03, -8.21011692264844401e-03, -7.65824520346905413e-03,
              -6.47209729391045177e-03, -4.99132412004966473e-03, -3.45612289713133280e-03,
              -2.01785580014170775e-03, -7.59430686781961401e-04, 2.84173631523859138e-04,
              1.10891667586337403e-03, 1.72901493872728771e-03, 2.16812590802684701e-03,
              2.45357710494539735e-03, 2.61281821058334862e-03, 2.67141039656276912e-03,
              2.65203073395980430e-03, 2.57411652877287315e-03, 2.45389126236094427e-03,
              2.30460058071795494e-03, 2.13684837686712662e-03, 1.95896528478870911e-03,
              1.77737008679454412e-03, 1.59690280765839059e-03, 1.42111975664438546e-03 };
    static const double br[14] = { 1.00000000000000000e+00, -1.45833333333333333e-01,
        -9.87413194444444444e-02, -1.43312053915895062e-01, -3.17227202678413548e-01,
        -9.42429147957120249e-01, -3.51120304082635426e+00, -1.57272636203680451e+01,
        -8.22814390971859444e+01, -4.92355370523670524e+02, -3.31621856854797251e+03,
        -2.48276742452085896e+04, -2.04526587315129788e+05, -1.83844491706820990e+06 };
    static const double c[105]
        = { 1.00000000000000000e+00, -2.08333333333333333e-01, 1.25000000000000000e-01,
              3.34201388888888889e-01, -4.01041666666666667e-01, 7.03125000000000000e-02,
              -1.02581259645061728e+00, 1.84646267361111111e+00, -8.91210937500000000e-01,
              7.32421875000000000e-02, 4.66958442342624743e+00, -1.12070026162229938e+01,
              8.78912353515625000e+00, -2.36408691406250000e+00, 1.12152099609375000e-01,
              -2.82120725582002449e+01, 8.46362176746007346e+01, -9.18182415432400174e+01,
              4.25349987453884549e+01, -7.36879435947963170e+00, 2.27108001708984375e-01,
              2.12570130039217123e+02, -7.65252468141181642e+02, 1.05999045252799988e+03,
              -6.99579627376132541e+02, 2.18190511744211590e+02, -2.64914304869515555e+01,
              5.72501420974731445e-01, -1.91945766231840700e+03, 8.06172218173730938e+03,
              -1.35865500064341374e+04, 1.16553933368645332e+04, -5.30564697861340311e+03,
              1.20090291321635246e+03, -1.08090919788394656e+02, 1.72772750258445740e+00,
              2.02042913309661486e+04, -9.69805983886375135e+04, 1.92547001232531532e+05,
              -2.03400177280415534e+05, 1.22200464983017460e+05, -4.11926549688975513e+04,
              7.10951430248936372e+03, -4.93915304773088012e+02, 6.07404200127348304e+00,
              -2.42919187900551333e+05, 1.31176361466297720e+06, -2.99801591853810675e+06,
              3.76327129765640400e+06, -2.81356322658653411e+06, 1.26836527332162478e+06,
              -3.31645172484563578e+05, 4.52187689813627263e+04, -2.49983048181120962e+03,
              2.43805296995560639e+01, 3.28446985307203782e+06, -1.97068191184322269e+07,
              5.09526024926646422e+07, -7.41051482115326577e+07, 6.63445122747290267e+07,
              -3.75671766607633513e+07, 1.32887671664218183e+07, -2.78561812808645469e+06,
              3.08186404612662398e+05, -1.38860897537170405e+04, 1.10017140269246738e+02,
              -4.93292536645099620e+07, 3.25573074185765749e+08, -9.39462359681578403e+08,
              1.55359689957058006e+09, -1.62108055210833708e+09, 1.10684281682301447e+09,
              -4.95889784275030309e+08, 1.42062907797533095e+08, -2.44740627257387285e+07,
              2.24376817792244943e+06, -8.40054336030240853e+04, 5.51335896122020586e+02,
              8.14789096118312115e+08, -5.86648149205184723e+09, 1.86882075092958249e+10,
              -3.46320433881587779e+10, 4.12801855797539740e+10, -3.30265997498007231e+10,
              1.79542137311556001e+10, -6.56329379261928433e+09, 1.55927986487925751e+09,
              -2.25105661889415278e+08, 1.73951075539781645e+07, -5.49842327572288687e+05,
              3.03809051092238427e+03, -1.46792612476956167e+10, 1.14498237732025810e+11,
              -3.99096175224466498e+11, 8.19218669548577329e+11, -1.09837515608122331e+12,
              1.00815810686538209e+12, -6.45364869245376503e+11, 2.87900649906150589e+11,
              -8.78670721780232657e+10, 1.76347306068349694e+10, -2.16716498322379509e+09,
              1.43157876718888981e+08, -3.87183344257261262e+06, 1.82577554742931747e+04 };
    static const double gama[30]
        = { 6.29960524947436582e-01, 2.51984209978974633e-01, 1.54790300415655846e-01,
              1.10713062416159013e-01, 8.57309395527394825e-02, 6.97161316958684292e-02,
              5.86085671893713576e-02, 5.04698873536310685e-02, 4.42600580689154809e-02,
              3.93720661543509966e-02, 3.54283195924455368e-02, 3.21818857502098231e-02,
              2.94646240791157679e-02, 2.71581677112934479e-02, 2.51768272973861779e-02,
              2.34570755306078891e-02, 2.19508390134907203e-02, 2.06210828235646240e-02,
              1.94388240897880846e-02, 1.83810633800683158e-02, 1.74293213231963172e-02,
              1.65685837786612353e-02, 1.57865285987918445e-02, 1.50729501494095594e-02,
              1.44193250839954639e-02, 1.38184805735341786e-02, 1.32643378994276568e-02,
              1.27517121970498651e-02, 1.22761545318762767e-02, 1.18338262398482403e-02 };
    double ac = 0;
    double ang = 0;
    double ap[30] = {};
    double atol = 0;
    double aw2 = 0;
    double azth = 0;
    double btol = 0;
    double cri[14] = {};
    double crr[14] = {};
    double dri[14] = {};
    double drr[14] = {};
    double fn13 = 0;
    double fn23 = 0;
    int ias = 0;
    int ibs = 0;
    int idum = 0;
    int is = 0;
    int j = 0;
    int jr = 0;
    int ju = 0;
    int k = 0;
    int kmax = 0;
    int kp1 = 0;
    int ks = 0;
    int l = 0;
    int l1 = 0;
    int l2 = 0;
    int lr = 0;
    int lrp1 = 0;
    int m = 0;
    double pi[30] = {};
    double pp = 0;
    double pr[30] = {};
    double przthi = 0;
    double przthr = 0;
    double ptfni = 0;
    double ptfnr = 0;
    double raw = 0;
    double raw2 = 0;
    double razth = 0;
    double rfn13 = 0;
    double rfnu = 0;
    double rfnu2 = 0;
    double rtzti = 0;
    double rtztr = 0;
    double rzthi = 0;
    double rzthr = 0;
    double sti = 0;
    double str = 0;
    double sumai = 0;
    double sumar = 0;
    double sumbi = 0;
    double sumbr = 0;
    double t2i = 0;
    double t2r = 0;
    double test = 0;
    double tfni = 0;
    double tfnr = 0;
    double tzai = 0;
    double tzar = 0;
    double upi[14] = {};
    double upr[14] = {};
    double w2i = 0;
    double w2r = 0;
    double wi = 0;
    double wr = 0;
    double zai = 0;
    double zar = 0;
    double zbi = 0;
    double zbr = 0;
    double zci = 0;
    double zcr = 0;
    double zetai = 0;
    double zetar = 0;
    double zthi = 0;
    double zthr = 0;
    int lim0_ = 0;
    int lim1_ = 0;
    int lim2_ = 0;
    int lim3_ = 0;
    int lim4_ = 0;
    int lim5_ = 0;
    int lim6_ = 0;
    int lim7_ = 0;
    int lim8_ = 0;
    int lim9_ = 0;
    rfnu = 1.0e0 / fnu;
    test = d1mach(1) * 1.0e+3;
    ac = fnu * test;
    if (std::fabs(zr) > ac || std::fabs(zi) > ac) {
        goto L15;
    }
    zeta1r = 2.0e0 * std::fabs(std::log(test)) + fnu;
    zeta1i = 0.0e0;
    zeta2r = fnu;
    zeta2i = 0.0e0;
    phir = 1.0e0;
    phii = 0.0e0;
    argr = 1.0e0;
    argi = 0.0e0;
    return;
L15:;
    zbr = zr * rfnu;
    zbi = zi * rfnu;
    rfnu2 = rfnu * rfnu;
    fn13 = std::pow(fnu, ex1);
    fn23 = fn13 * fn13;
    rfn13 = 1.0e0 / fn13;
    w2r = oner - zbr * zbr + zbi * zbi;
    w2i = onei - zbr * zbi - zbr * zbi;
    aw2 = zabs(w2r, w2i);
    if (aw2 > 0.25e0) {
        goto L130;
    }
    k = 1;
    pr[(1) - 1] = oner;
    pi[(1) - 1] = onei;
    sumar = gama[(1) - 1];
    sumai = zeroi;
    ap[(1) - 1] = 1.0e0;
    if (aw2 < tol) {
        goto L20;
    }
    lim0_ = 30;
    for (k = 2; k <= lim0_; k += 1) {
        pr[(k)-1] = pr[(k - 1) - 1] * w2r - pi[(k - 1) - 1] * w2i;
        pi[(k)-1] = pr[(k - 1) - 1] * w2i + pi[(k - 1) - 1] * w2r;
        sumar = sumar + pr[(k)-1] * gama[(k)-1];
        sumai = sumai + pi[(k)-1] * gama[(k)-1];
        ap[(k)-1] = ap[(k - 1) - 1] * aw2;
        if (ap[(k)-1] < tol) {
            goto L20;
        };
    }
    k = 30;
L20:;
    kmax = k;
    zetar = w2r * sumar - w2i * sumai;
    zetai = w2r * sumai + w2i * sumar;
    argr = zetar * fn23;
    argi = zetai * fn23;
    zsqrt(sumar, sumai, zar, zai);
    zsqrt(w2r, w2i, str, sti);
    zeta2r = str * fnu;
    zeta2i = sti * fnu;
    str = oner + ex2 * (zetar * zar - zetai * zai);
    sti = onei + ex2 * (zetar * zai + zetai * zar);
    zeta1r = str * zeta2r - sti * zeta2i;
    zeta1i = str * zeta2i + sti * zeta2r;
    zar = zar + zar;
    zai = zai + zai;
    zsqrt(zar, zai, str, sti);
    phir = str * rfn13;
    phii = sti * rfn13;
    if (ipmtr == 1) {
        goto L120;
    }
    sumbr = zeror;
    sumbi = zeroi;
    lim1_ = kmax;
    for (k = 1; k <= lim1_; k += 1) {
        sumbr = sumbr + pr[(k)-1] * beta[(k)-1];
        sumbi = sumbi + pi[(k)-1] * beta[(k)-1];
        ;
    }
    asumr = zeror;
    asumi = zeroi;
    bsumr = sumbr;
    bsumi = sumbi;
    l1 = 0;
    l2 = 30;
    btol = tol * (std::fabs(bsumr) + std::fabs(bsumi));
    atol = tol;
    pp = 1.0e0;
    ias = 0;
    ibs = 0;
    if (rfnu2 < tol) {
        goto L110;
    }
    lim2_ = 7;
    for (is = 2; is <= lim2_; is += 1) {
        atol = atol / rfnu2;
        pp = pp * rfnu2;
        if (ias == 1) {
            goto L60;
        }
        sumar = zeror;
        sumai = zeroi;
        lim3_ = kmax;
        for (k = 1; k <= lim3_; k += 1) {
            m = l1 + k;
            sumar = sumar + pr[(k)-1] * alfa[(m)-1];
            sumai = sumai + pi[(k)-1] * alfa[(m)-1];
            if (ap[(k)-1] < atol) {
                goto L50;
            };
        }
    L50:;
        asumr = asumr + sumar * pp;
        asumi = asumi + sumai * pp;
        if (pp < tol) {
            ias = 1;
        }
    L60:;
        if (ibs == 1) {
            goto L90;
        }
        sumbr = zeror;
        sumbi = zeroi;
        lim4_ = kmax;
        for (k = 1; k <= lim4_; k += 1) {
            m = l2 + k;
            sumbr = sumbr + pr[(k)-1] * beta[(m)-1];
            sumbi = sumbi + pi[(k)-1] * beta[(m)-1];
            if (ap[(k)-1] < atol) {
                goto L80;
            };
        }
    L80:;
        bsumr = bsumr + sumbr * pp;
        bsumi = bsumi + sumbi * pp;
        if (pp < btol) {
            ibs = 1;
        }
    L90:;
        if (ias == 1 && ibs == 1) {
            goto L110;
        }
        l1 = l1 + 30;
        l2 = l2 + 30;
        ;
    }
L110:;
    asumr = asumr + oner;
    pp = rfnu * rfn13;
    bsumr = bsumr * pp;
    bsumi = bsumi * pp;
L120:;
    return;
L130:;
    zsqrt(w2r, w2i, wr, wi);
    if (wr < 0.0e0) {
        wr = 0.0e0;
    }
    if (wi < 0.0e0) {
        wi = 0.0e0;
    }
    str = oner + wr;
    sti = wi;
    zdiv(str, sti, zbr, zbi, zar, zai);
    zlog(zar, zai, zcr, zci, idum);
    if (zci < 0.0e0) {
        zci = 0.0e0;
    }
    if (zci > hpi) {
        zci = hpi;
    }
    if (zcr < 0.0e0) {
        zcr = 0.0e0;
    }
    zthr = (zcr - wr) * 1.5e0;
    zthi = (zci - wi) * 1.5e0;
    zeta1r = zcr * fnu;
    zeta1i = zci * fnu;
    zeta2r = wr * fnu;
    zeta2i = wi * fnu;
    azth = zabs(zthr, zthi);
    ang = thpi;
    if (zthr >= 0.0e0 && zthi < 0.0e0) {
        goto L140;
    }
    ang = hpi;
    if (zthr == 0.0e0) {
        goto L140;
    }
    ang = std::atan(zthi / zthr);
    if (zthr < 0.0e0) {
        ang = ang + gpi;
    }
L140:;
    pp = std::pow(azth, ex2);
    ang = ang * ex2;
    zetar = pp * std::cos(ang);
    zetai = pp * std::sin(ang);
    if (zetai < 0.0e0) {
        zetai = 0.0e0;
    }
    argr = zetar * fn23;
    argi = zetai * fn23;
    zdiv(zthr, zthi, zetar, zetai, rtztr, rtzti);
    zdiv(rtztr, rtzti, wr, wi, zar, zai);
    tzar = zar + zar;
    tzai = zai + zai;
    zsqrt(tzar, tzai, str, sti);
    phir = str * rfn13;
    phii = sti * rfn13;
    if (ipmtr == 1) {
        goto L120;
    }
    raw = 1.0e0 / std::sqrt(aw2);
    str = wr * raw;
    sti = -wi * raw;
    tfnr = str * rfnu * raw;
    tfni = sti * rfnu * raw;
    razth = 1.0e0 / azth;
    str = zthr * razth;
    sti = -zthi * razth;
    rzthr = str * razth * rfnu;
    rzthi = sti * razth * rfnu;
    zcr = rzthr * ar[(2) - 1];
    zci = rzthi * ar[(2) - 1];
    raw2 = 1.0e0 / aw2;
    str = w2r * raw2;
    sti = -w2i * raw2;
    t2r = str * raw2;
    t2i = sti * raw2;
    str = t2r * c[(2) - 1] + c[(3) - 1];
    sti = t2i * c[(2) - 1];
    upr[(2) - 1] = str * tfnr - sti * tfni;
    upi[(2) - 1] = str * tfni + sti * tfnr;
    bsumr = upr[(2) - 1] + zcr;
    bsumi = upi[(2) - 1] + zci;
    asumr = zeror;
    asumi = zeroi;
    if (rfnu < tol) {
        goto L220;
    }
    przthr = rzthr;
    przthi = rzthi;
    ptfnr = tfnr;
    ptfni = tfni;
    upr[(1) - 1] = oner;
    upi[(1) - 1] = onei;
    pp = 1.0e0;
    btol = tol * (std::fabs(bsumr) + std::fabs(bsumi));
    ks = 0;
    kp1 = 2;
    l = 3;
    ias = 0;
    ibs = 0;
    lim5_ = 12;
    for (lr = 2; lr <= lim5_; lr += 2) {
        lrp1 = lr + 1;
        lim6_ = lrp1;
        for (k = lr; k <= lim6_; k += 1) {
            ks = ks + 1;
            kp1 = kp1 + 1;
            l = l + 1;
            zar = c[(l)-1];
            zai = zeroi;
            lim7_ = kp1;
            for (j = 2; j <= lim7_; j += 1) {
                l = l + 1;
                str = zar * t2r - t2i * zai + c[(l)-1];
                zai = zar * t2i + zai * t2r;
                zar = str;
                ;
            }
            str = ptfnr * tfnr - ptfni * tfni;
            ptfni = ptfnr * tfni + ptfni * tfnr;
            ptfnr = str;
            upr[(kp1)-1] = ptfnr * zar - ptfni * zai;
            upi[(kp1)-1] = ptfni * zar + ptfnr * zai;
            crr[(ks)-1] = przthr * br[(ks + 1) - 1];
            cri[(ks)-1] = przthi * br[(ks + 1) - 1];
            str = przthr * rzthr - przthi * rzthi;
            przthi = przthr * rzthi + przthi * rzthr;
            przthr = str;
            drr[(ks)-1] = przthr * ar[(ks + 2) - 1];
            dri[(ks)-1] = przthi * ar[(ks + 2) - 1];
            ;
        }
        pp = pp * rfnu2;
        if (ias == 1) {
            goto L180;
        }
        sumar = upr[(lrp1)-1];
        sumai = upi[(lrp1)-1];
        ju = lrp1;
        lim8_ = lr;
        for (jr = 1; jr <= lim8_; jr += 1) {
            ju = ju - 1;
            sumar = sumar + crr[(jr)-1] * upr[(ju)-1] - cri[(jr)-1] * upi[(ju)-1];
            sumai = sumai + crr[(jr)-1] * upi[(ju)-1] + cri[(jr)-1] * upr[(ju)-1];
            ;
        }
        asumr = asumr + sumar;
        asumi = asumi + sumai;
        test = std::fabs(sumar) + std::fabs(sumai);
        if (pp < tol && test < tol) {
            ias = 1;
        }
    L180:;
        if (ibs == 1) {
            goto L200;
        }
        sumbr = upr[(lr + 2) - 1] + upr[(lrp1)-1] * zcr - upi[(lrp1)-1] * zci;
        sumbi = upi[(lr + 2) - 1] + upr[(lrp1)-1] * zci + upi[(lrp1)-1] * zcr;
        ju = lrp1;
        lim9_ = lr;
        for (jr = 1; jr <= lim9_; jr += 1) {
            ju = ju - 1;
            sumbr = sumbr + drr[(jr)-1] * upr[(ju)-1] - dri[(jr)-1] * upi[(ju)-1];
            sumbi = sumbi + drr[(jr)-1] * upi[(ju)-1] + dri[(jr)-1] * upr[(ju)-1];
            ;
        }
        bsumr = bsumr + sumbr;
        bsumi = bsumi + sumbi;
        test = std::fabs(sumbr) + std::fabs(sumbi);
        if (pp < btol && test < btol) {
            ibs = 1;
        }
    L200:;
        if (ias == 1 && ibs == 1) {
            goto L220;
        };
    }
L220:;
    asumr = asumr + oner;
    str = -bsumr * rfn13;
    sti = -bsumi * rfn13;
    zdiv(str, sti, rtztr, rtzti, bsumr, bsumi);
    goto L120;
}
//=============================================================================
static void
zuoik(double zr, double zi, double fnu, int kode, int ikflg, int n, double* yr, double* yi,
    int& nuf, double tol, double elim, double alim)
{
    const double aic = 1.265512123484645396e+00;
    const double zeroi = 0.0e0;
    const double zeror = 0.0e0;
    double aarg = 0;
    double aphi = 0;
    double argi = 0;
    double argr = 0;
    double ascle = 0;
    double asumi = 0;
    double asumr = 0;
    double ax = 0;
    double ay = 0;
    double bsumi = 0;
    double bsumr = 0;
    double cwrki[16] = {};
    double cwrkr[16] = {};
    double czi = 0;
    double czr = 0;
    double fnn = 0;
    double gnn = 0;
    double gnu = 0;
    int i = 0;
    int idum = 0;
    int iform = 0;
    int init = 0;
    int nn = 0;
    int nw = 0;
    double phii = 0;
    double phir = 0;
    double rcz = 0;
    double sti = 0;
    double str = 0;
    double sumi = 0;
    double sumr = 0;
    double zbi = 0;
    double zbr = 0;
    double zeta1i = 0;
    double zeta1r = 0;
    double zeta2i = 0;
    double zeta2r = 0;
    double zni = 0;
    double znr = 0;
    double zri = 0;
    double zrr = 0;
    int lim0_ = 0;
    nuf = 0;
    nn = n;
    zrr = zr;
    zri = zi;
    if (zr >= 0.0e0) {
        goto L10;
    }
    zrr = -zr;
    zri = -zi;
L10:;
    zbr = zrr;
    zbi = zri;
    ax = std::fabs(zr) * 1.7321e0;
    ay = std::fabs(zi);
    iform = 1;
    if (ay > ax) {
        iform = 2;
    }
    gnu = fmax_(fnu, 1.0e0);
    if (ikflg == 1) {
        goto L20;
    }
    fnn = (double)(nn);
    gnn = fnu + fnn - 1.0e0;
    gnu = fmax_(gnn, fnn);
L20:;
    if (iform == 2) {
        goto L30;
    }
    init = 0;
    zunik(zrr, zri, gnu, ikflg, 1, tol, init, phir, phii, zeta1r, zeta1i, zeta2r, zeta2i, sumr,
        sumi, cwrkr, cwrki);
    czr = -zeta1r + zeta2r;
    czi = -zeta1i + zeta2i;
    goto L50;
L30:;
    znr = zri;
    zni = -zrr;
    if (zi > 0.0e0) {
        goto L40;
    }
    znr = -znr;
L40:;
    zunhj(znr, zni, gnu, 1, tol, phir, phii, argr, argi, zeta1r, zeta1i, zeta2r, zeta2i, asumr,
        asumi, bsumr, bsumi);
    czr = -zeta1r + zeta2r;
    czi = -zeta1i + zeta2i;
    aarg = zabs(argr, argi);
L50:;
    if (kode == 1) {
        goto L60;
    }
    czr = czr - zbr;
    czi = czi - zbi;
L60:;
    if (ikflg == 1) {
        goto L70;
    }
    czr = -czr;
    czi = -czi;
L70:;
    aphi = zabs(phir, phii);
    rcz = czr;
    if (rcz > elim) {
        goto L210;
    }
    if (rcz < alim) {
        goto L80;
    }
    rcz = rcz + std::log(aphi);
    if (iform == 2) {
        rcz = rcz - 0.25e0 * std::log(aarg) - aic;
    }
    if (rcz > elim) {
        goto L210;
    }
    goto L130;
L80:;
    if (rcz < (-elim)) {
        goto L90;
    }
    if (rcz > (-alim)) {
        goto L130;
    }
    rcz = rcz + std::log(aphi);
    if (iform == 2) {
        rcz = rcz - 0.25e0 * std::log(aarg) - aic;
    }
    if (rcz > (-elim)) {
        goto L110;
    }
L90:;
    lim0_ = nn;
    for (i = 1; i <= lim0_; i += 1) {
        yr[(i)-1] = zeror;
        yi[(i)-1] = zeroi;
        ;
    }
    nuf = nn;
    return;
L110:;
    ascle = 1.0e+3 * d1mach(1) / tol;
    zlog(phir, phii, str, sti, idum);
    czr = czr + str;
    czi = czi + sti;
    if (iform == 1) {
        goto L120;
    }
    zlog(argr, argi, str, sti, idum);
    czr = czr - 0.25e0 * str - aic;
    czi = czi - 0.25e0 * sti;
L120:;
    ax = std::exp(rcz) / tol;
    ay = czi;
    czr = ax * std::cos(ay);
    czi = ax * std::sin(ay);
    zuchk(czr, czi, nw, ascle, tol);
    if (nw != 0) {
        goto L90;
    }
L130:;
    if (ikflg == 2) {
        return;
    }
    if (n == 1) {
        return;
    }
L140:;
    gnu = fnu + (double)(nn - 1);
    if (iform == 2) {
        goto L150;
    }
    init = 0;
    zunik(zrr, zri, gnu, ikflg, 1, tol, init, phir, phii, zeta1r, zeta1i, zeta2r, zeta2i, sumr,
        sumi, cwrkr, cwrki);
    czr = -zeta1r + zeta2r;
    czi = -zeta1i + zeta2i;
    goto L160;
L150:;
    zunhj(znr, zni, gnu, 1, tol, phir, phii, argr, argi, zeta1r, zeta1i, zeta2r, zeta2i, asumr,
        asumi, bsumr, bsumi);
    czr = -zeta1r + zeta2r;
    czi = -zeta1i + zeta2i;
    aarg = zabs(argr, argi);
L160:;
    if (kode == 1) {
        goto L170;
    }
    czr = czr - zbr;
    czi = czi - zbi;
L170:;
    aphi = zabs(phir, phii);
    rcz = czr;
    if (rcz < (-elim)) {
        goto L180;
    }
    if (rcz > (-alim)) {
        return;
    }
    rcz = rcz + std::log(aphi);
    if (iform == 2) {
        rcz = rcz - 0.25e0 * std::log(aarg) - aic;
    }
    if (rcz > (-elim)) {
        goto L190;
    }
L180:;
    yr[(nn)-1] = zeror;
    yi[(nn)-1] = zeroi;
    nn = nn - 1;
    nuf = nuf + 1;
    if (nn == 0) {
        return;
    }
    goto L140;
L190:;
    ascle = 1.0e+3 * d1mach(1) / tol;
    zlog(phir, phii, str, sti, idum);
    czr = czr + str;
    czi = czi + sti;
    if (iform == 1) {
        goto L200;
    }
    zlog(argr, argi, str, sti, idum);
    czr = czr - 0.25e0 * str - aic;
    czi = czi - 0.25e0 * sti;
L200:;
    ax = std::exp(rcz) / tol;
    ay = czi;
    czr = ax * std::cos(ay);
    czi = ax * std::sin(ay);
    zuchk(czr, czi, nw, ascle, tol);
    if (nw != 0) {
        goto L180;
    }
    return;
L210:;
    nuf = -1;
    return;
}
//=============================================================================
static void
zwrsk(double zrr, double zri, double fnu, int kode, int n, double* yr, double* yi, int& nz,
    double* cwr, double* cwi, double tol, double elim, double alim)
{
    double act = 0;
    double acw = 0;
    double ascle = 0;
    double c1i = 0;
    double c1r = 0;
    double c2i = 0;
    double c2r = 0;
    double cinui = 0;
    double cinur = 0;
    double csclr = 0;
    double cti = 0;
    double ctr = 0;
    int i = 0;
    int nw = 0;
    double pti = 0;
    double ptr = 0;
    double ract = 0;
    double sti = 0;
    double str = 0;
    int lim0_ = 0;
    nz = 0;
    zbknu(zrr, zri, fnu, kode, 2, cwr, cwi, nw, tol, elim, alim);
    if (nw != 0) {
        goto L50;
    }
    zrati(zrr, zri, fnu, n, yr, yi, tol);
    cinur = 1.0e0;
    cinui = 0.0e0;
    if (kode == 1) {
        goto L10;
    }
    cinur = std::cos(zri);
    cinui = std::sin(zri);
L10:;
    acw = zabs(cwr[(2) - 1], cwi[(2) - 1]);
    ascle = 1.0e+3 * d1mach(1) / tol;
    csclr = 1.0e0;
    if (acw > ascle) {
        goto L20;
    }
    csclr = 1.0e0 / tol;
    goto L30;
L20:;
    ascle = 1.0e0 / ascle;
    if (acw < ascle) {
        goto L30;
    }
    csclr = tol;
L30:;
    c1r = cwr[(1) - 1] * csclr;
    c1i = cwi[(1) - 1] * csclr;
    c2r = cwr[(2) - 1] * csclr;
    c2i = cwi[(2) - 1] * csclr;
    str = yr[(1) - 1];
    sti = yi[(1) - 1];
    ptr = str * c1r - sti * c1i;
    pti = str * c1i + sti * c1r;
    ptr = ptr + c2r;
    pti = pti + c2i;
    ctr = zrr * ptr - zri * pti;
    cti = zrr * pti + zri * ptr;
    act = zabs(ctr, cti);
    ract = 1.0e0 / act;
    ctr = ctr * ract;
    cti = -cti * ract;
    ptr = cinur * ract;
    pti = cinui * ract;
    cinur = ptr * ctr - pti * cti;
    cinui = ptr * cti + pti * ctr;
    yr[(1) - 1] = cinur * csclr;
    yi[(1) - 1] = cinui * csclr;
    if (n == 1) {
        return;
    }
    lim0_ = n;
    for (i = 2; i <= lim0_; i += 1) {
        ptr = str * cinur - sti * cinui;
        cinui = str * cinui + sti * cinur;
        cinur = ptr;
        str = yr[(i)-1];
        sti = yi[(i)-1];
        yr[(i)-1] = cinur * csclr;
        yi[(i)-1] = cinui * csclr;
        ;
    }
    return;
L50:;
    nz = -1;
    if (nw == (-2)) {
        nz = -2;
    }
    return;
}
//=============================================================================
static void
zbknu(double zr, double zi, double fnu, int kode, int n, double* yr, double* yi, int& nz,
    double tol, double elim, double alim)
{
    const double onei = 0.0e0;
    const double oner = 1.0e0;
    const double ctwor = 2.0e0;
    const double czeroi = 0.0e0;
    const double czeror = 0.0e0;
    const double dpi = 3.14159265358979324e0;
    const double fpi = 1.89769999331517738e0;
    const double hpi = 1.57079632679489662e0;
    const int kmax = 30;
    const double r1 = 2.0e0;
    const double rthpi = 1.25331413731550025e0;
    const double spi = 1.90985931710274403e0;
    const double tth = 6.66666666666666666e-01;
    static const double cc[8] = { 5.77215664901532861e-01, -4.20026350340952355e-02,
        -4.21977345555443367e-02, 7.21894324666309954e-03, -2.15241674114950973e-04,
        -2.01348547807882387e-05, 1.13302723198169588e-06, 6.11609510448141582e-09 };
    double a1 = 0;
    double a2 = 0;
    double aa = 0;
    double ak = 0;
    double alas = 0;
    double as = 0;
    double ascle = 0;
    double bb = 0;
    double bk = 0;
    double bry[3] = {};
    double caz = 0;
    double cbi = 0;
    double cbr = 0;
    double cchi = 0;
    double cchr = 0;
    double celmr = 0;
    double cki = 0;
    double ckr = 0;
    double coefi = 0;
    double coefr = 0;
    double crscr = 0;
    double csclr = 0;
    double cshi = 0;
    double cshr = 0;
    double csi = 0;
    double csr = 0;
    double csrr[3] = {};
    double cssr[3] = {};
    double cyi[2] = {};
    double cyr[2] = {};
    double czi = 0;
    double czr = 0;
    double dnu = 0;
    double dnu2 = 0;
    double elm = 0;
    double etest = 0;
    double fc = 0;
    double fhs = 0;
    double fi = 0;
    double fk = 0;
    double fks = 0;
    double fmui = 0;
    double fmur = 0;
    double fr = 0;
    double g1 = 0;
    double g2 = 0;
    double helim = 0;
    int i = 0;
    int ic = 0;
    int idum = 0;
    int iflag = 0;
    int inu = 0;
    int inub = 0;
    int j = 0;
    int k = 0;
    int kflag = 0;
    int kk = 0;
    int koded = 0;
    int nw = 0;
    double p1i = 0;
    double p1r = 0;
    double p2i = 0;
    double p2m = 0;
    double p2r = 0;
    double pi = 0;
    double pr = 0;
    double pti = 0;
    double ptr = 0;
    double qi = 0;
    double qr = 0;
    double rak = 0;
    double rcaz = 0;
    double rzi = 0;
    double rzr = 0;
    double s = 0;
    double s1i = 0;
    double s1r = 0;
    double s2i = 0;
    double s2r = 0;
    double smui = 0;
    double smur = 0;
    double sti = 0;
    double str = 0;
    double t1 = 0;
    double t2 = 0;
    double tm = 0;
    double zdi = 0;
    double zdr = 0;
    int lim0_ = 0;
    int lim1_ = 0;
    int lim2_ = 0;
    int lim3_ = 0;
    int lim4_ = 0;
    int lim5_ = 0;
    caz = zabs(zr, zi);
    csclr = 1.0e0 / tol;
    crscr = tol;
    cssr[(1) - 1] = csclr;
    cssr[(2) - 1] = 1.0e0;
    cssr[(3) - 1] = crscr;
    csrr[(1) - 1] = crscr;
    csrr[(2) - 1] = 1.0e0;
    csrr[(3) - 1] = csclr;
    bry[(1) - 1] = 1.0e+3 * d1mach(1) / tol;
    bry[(2) - 1] = 1.0e0 / bry[(1) - 1];
    bry[(3) - 1] = d1mach(2);
    nz = 0;
    iflag = 0;
    koded = kode;
    rcaz = 1.0e0 / caz;
    str = zr * rcaz;
    sti = -zi * rcaz;
    rzr = (str + str) * rcaz;
    rzi = (sti + sti) * rcaz;
    inu = fortranInt(fnu + 0.5e0);
    dnu = fnu - (double)(inu);
    if (std::fabs(dnu) == 0.5e0) {
        goto L110;
    }
    dnu2 = 0.0e0;
    if (std::fabs(dnu) > tol) {
        dnu2 = dnu * dnu;
    }
    if (caz > r1) {
        goto L110;
    }
    fc = 1.0e0;
    zlog(rzr, rzi, smur, smui, idum);
    fmur = smur * dnu;
    fmui = smui * dnu;
    zshch(fmur, fmui, cshr, cshi, cchr, cchi);
    if (dnu == 0.0e0) {
        goto L10;
    }
    fc = dnu * dpi;
    fc = fc / std::sin(fc);
    smur = cshr / dnu;
    smui = cshi / dnu;
L10:;
    a2 = 1.0e0 + dnu;
    t2 = std::exp(-dgamln(a2, idum));
    t1 = 1.0e0 / (t2 * fc);
    if (std::fabs(dnu) > 0.1e0) {
        goto L40;
    }
    ak = 1.0e0;
    s = cc[(1) - 1];
    lim0_ = 8;
    for (k = 2; k <= lim0_; k += 1) {
        ak = ak * dnu2;
        tm = cc[(k)-1] * ak;
        s = s + tm;
        if (std::fabs(tm) < tol) {
            goto L30;
        };
    }
L30:
    g1 = -s;
    goto L50;
L40:;
    g1 = (t1 - t2) / (dnu + dnu);
L50:;
    g2 = (t1 + t2) * 0.5e0;
    fr = fc * (cchr * g1 + smur * g2);
    fi = fc * (cchi * g1 + smui * g2);
    zexp(fmur, fmui, str, sti);
    pr = 0.5e0 * str / t2;
    pi = 0.5e0 * sti / t2;
    zdiv(0.5e0, 0.0e0, str, sti, ptr, pti);
    qr = ptr / t1;
    qi = pti / t1;
    s1r = fr;
    s1i = fi;
    s2r = pr;
    s2i = pi;
    ak = 1.0e0;
    a1 = 1.0e0;
    ckr = oner;
    cki = onei;
    bk = 1.0e0 - dnu2;
    if (inu > 0 || n > 1) {
        goto L80;
    }
    if (caz < tol) {
        goto L70;
    }
    zmlt(zr, zi, zr, zi, czr, czi);
    czr = 0.25e0 * czr;
    czi = 0.25e0 * czi;
    t1 = 0.25e0 * caz * caz;
L60:;
    fr = (fr * ak + pr + qr) / bk;
    fi = (fi * ak + pi + qi) / bk;
    str = 1.0e0 / (ak - dnu);
    pr = pr * str;
    pi = pi * str;
    str = 1.0e0 / (ak + dnu);
    qr = qr * str;
    qi = qi * str;
    str = ckr * czr - cki * czi;
    rak = 1.0e0 / ak;
    cki = (ckr * czi + cki * czr) * rak;
    ckr = str * rak;
    s1r = ckr * fr - cki * fi + s1r;
    s1i = ckr * fi + cki * fr + s1i;
    a1 = a1 * t1 * rak;
    bk = bk + ak + ak + 1.0e0;
    ak = ak + 1.0e0;
    if (a1 > tol) {
        goto L60;
    }
L70:;
    yr[(1) - 1] = s1r;
    yi[(1) - 1] = s1i;
    if (koded == 1) {
        return;
    }
    zexp(zr, zi, str, sti);
    zmlt(s1r, s1i, str, sti, yr[(1) - 1], yi[(1) - 1]);
    return;
L80:;
    if (caz < tol) {
        goto L100;
    }
    zmlt(zr, zi, zr, zi, czr, czi);
    czr = 0.25e0 * czr;
    czi = 0.25e0 * czi;
    t1 = 0.25e0 * caz * caz;
L90:;
    fr = (fr * ak + pr + qr) / bk;
    fi = (fi * ak + pi + qi) / bk;
    str = 1.0e0 / (ak - dnu);
    pr = pr * str;
    pi = pi * str;
    str = 1.0e0 / (ak + dnu);
    qr = qr * str;
    qi = qi * str;
    str = ckr * czr - cki * czi;
    rak = 1.0e0 / ak;
    cki = (ckr * czi + cki * czr) * rak;
    ckr = str * rak;
    s1r = ckr * fr - cki * fi + s1r;
    s1i = ckr * fi + cki * fr + s1i;
    str = pr - fr * ak;
    sti = pi - fi * ak;
    s2r = ckr * str - cki * sti + s2r;
    s2i = ckr * sti + cki * str + s2i;
    a1 = a1 * t1 * rak;
    bk = bk + ak + ak + 1.0e0;
    ak = ak + 1.0e0;
    if (a1 > tol) {
        goto L90;
    }
L100:;
    kflag = 2;
    a1 = fnu + 1.0e0;
    ak = a1 * std::fabs(smur);
    if (ak > alim) {
        kflag = 3;
    }
    str = cssr[(kflag)-1];
    p2r = s2r * str;
    p2i = s2i * str;
    zmlt(p2r, p2i, rzr, rzi, s2r, s2i);
    s1r = s1r * str;
    s1i = s1i * str;
    if (koded == 1) {
        goto L210;
    }
    zexp(zr, zi, fr, fi);
    zmlt(s1r, s1i, fr, fi, s1r, s1i);
    zmlt(s2r, s2i, fr, fi, s2r, s2i);
    goto L210;
L110:;
    zsqrt(zr, zi, str, sti);
    zdiv(rthpi, czeroi, str, sti, coefr, coefi);
    kflag = 2;
    if (koded == 2) {
        goto L120;
    }
    if (zr > alim) {
        goto L290;
    }
    str = std::exp(-zr) * cssr[(kflag)-1];
    sti = -str * std::sin(zi);
    str = str * std::cos(zi);
    zmlt(coefr, coefi, str, sti, coefr, coefi);
L120:;
    if (std::fabs(dnu) == 0.5e0) {
        goto L300;
    }
    ak = std::cos(dpi * dnu);
    ak = std::fabs(ak);
    if (ak == czeror) {
        goto L300;
    }
    fhs = std::fabs(0.25e0 - dnu2);
    if (fhs == czeror) {
        goto L300;
    }
    t1 = (double)(i1mach(14) - 1);
    t1 = t1 * d1mach(5) * 3.321928094e0;
    t1 = fmax_(t1, 12.0e0);
    t1 = fmin_(t1, 60.0e0);
    t2 = tth * t1 - 6.0e0;
    if (zr != 0.0e0) {
        goto L130;
    }
    t1 = hpi;
    goto L140;
L130:;
    t1 = std::atan(zi / zr);
    t1 = std::fabs(t1);
L140:;
    if (t2 > caz) {
        goto L170;
    }
    etest = ak / (dpi * caz * tol);
    fk = oner;
    if (etest < oner) {
        goto L180;
    }
    fks = ctwor;
    ckr = caz + caz + ctwor;
    p1r = czeror;
    p2r = oner;
    lim1_ = kmax;
    for (i = 1; i <= lim1_; i += 1) {
        ak = fhs / fks;
        cbr = ckr / (fk + oner);
        ptr = p2r;
        p2r = cbr * p2r - p1r * ak;
        p1r = ptr;
        ckr = ckr + ctwor;
        fks = fks + fk + fk + ctwor;
        fhs = fhs + fk + fk;
        fk = fk + oner;
        str = std::fabs(p2r) * fk;
        if (etest < str) {
            goto L160;
        };
    }
    goto L310;
L160:;
    fk = fk + spi * t1 * std::sqrt(t2 / caz);
    fhs = std::fabs(0.25e0 - dnu2);
    goto L180;
L170:;
    a2 = std::sqrt(caz);
    ak = fpi * ak / (tol * std::sqrt(a2));
    aa = 3.0e0 * t1 / (1.0e0 + caz);
    bb = 14.7e0 * t1 / (28.0e0 + caz);
    ak = (std::log(ak) + caz * std::cos(aa) / (1.0e0 + 0.008e0 * caz)) / std::cos(bb);
    fk = 0.12125e0 * ak * ak / caz + 1.5e0;
L180:;
    k = fortranInt(fk);
    fk = (double)(k);
    fks = fk * fk;
    p1r = czeror;
    p1i = czeroi;
    p2r = tol;
    p2i = czeroi;
    csr = p2r;
    csi = p2i;
    lim2_ = k;
    for (i = 1; i <= lim2_; i += 1) {
        a1 = fks - fk;
        ak = (fks + fk) / (a1 + fhs);
        rak = 2.0e0 / (fk + oner);
        cbr = (fk + zr) * rak;
        cbi = zi * rak;
        ptr = p2r;
        pti = p2i;
        p2r = (ptr * cbr - pti * cbi - p1r) * ak;
        p2i = (pti * cbr + ptr * cbi - p1i) * ak;
        p1r = ptr;
        p1i = pti;
        csr = csr + p2r;
        csi = csi + p2i;
        fks = a1 - fk + oner;
        fk = fk - oner;
        ;
    }
    tm = zabs(csr, csi);
    ptr = 1.0e0 / tm;
    s1r = p2r * ptr;
    s1i = p2i * ptr;
    csr = csr * ptr;
    csi = -csi * ptr;
    zmlt(coefr, coefi, s1r, s1i, str, sti);
    zmlt(str, sti, csr, csi, s1r, s1i);
    if (inu > 0 || n > 1) {
        goto L200;
    }
    zdr = zr;
    zdi = zi;
    if (iflag == 1) {
        goto L270;
    }
    goto L240;
L200:;
    tm = zabs(p2r, p2i);
    ptr = 1.0e0 / tm;
    p1r = p1r * ptr;
    p1i = p1i * ptr;
    p2r = p2r * ptr;
    p2i = -p2i * ptr;
    zmlt(p1r, p1i, p2r, p2i, ptr, pti);
    str = dnu + 0.5e0 - ptr;
    sti = -pti;
    zdiv(str, sti, zr, zi, str, sti);
    str = str + 1.0e0;
    zmlt(str, sti, s1r, s1i, s2r, s2i);
L210:;
    str = dnu + 1.0e0;
    ckr = str * rzr;
    cki = str * rzi;
    if (n == 1) {
        inu = inu - 1;
    }
    if (inu > 0) {
        goto L220;
    }
    if (n > 1) {
        goto L215;
    }
    s1r = s2r;
    s1i = s2i;
L215:;
    zdr = zr;
    zdi = zi;
    if (iflag == 1) {
        goto L270;
    }
    goto L240;
L220:;
    inub = 1;
    if (iflag == 1) {
        goto L261;
    }
L225:;
    p1r = csrr[(kflag)-1];
    ascle = bry[(kflag)-1];
    lim3_ = inu;
    for (i = inub; i <= lim3_; i += 1) {
        str = s2r;
        sti = s2i;
        s2r = ckr * str - cki * sti + s1r;
        s2i = ckr * sti + cki * str + s1i;
        s1r = str;
        s1i = sti;
        ckr = ckr + rzr;
        cki = cki + rzi;
        if (kflag >= 3) {
            goto L230;
        }
        p2r = s2r * p1r;
        p2i = s2i * p1r;
        str = std::fabs(p2r);
        sti = std::fabs(p2i);
        p2m = fmax_(str, sti);
        if (p2m <= ascle) {
            goto L230;
        }
        kflag = kflag + 1;
        ascle = bry[(kflag)-1];
        s1r = s1r * p1r;
        s1i = s1i * p1r;
        s2r = p2r;
        s2i = p2i;
        str = cssr[(kflag)-1];
        s1r = s1r * str;
        s1i = s1i * str;
        s2r = s2r * str;
        s2i = s2i * str;
        p1r = csrr[(kflag)-1];
    L230:;
    }
    if (n != 1) {
        goto L240;
    }
    s1r = s2r;
    s1i = s2i;
L240:;
    str = csrr[(kflag)-1];
    yr[(1) - 1] = s1r * str;
    yi[(1) - 1] = s1i * str;
    if (n == 1) {
        return;
    }
    yr[(2) - 1] = s2r * str;
    yi[(2) - 1] = s2i * str;
    if (n == 2) {
        return;
    }
    kk = 2;
L250:;
    kk = kk + 1;
    if (kk > n) {
        return;
    }
    p1r = csrr[(kflag)-1];
    ascle = bry[(kflag)-1];
    lim4_ = n;
    for (i = kk; i <= lim4_; i += 1) {
        p2r = s2r;
        p2i = s2i;
        s2r = ckr * p2r - cki * p2i + s1r;
        s2i = cki * p2r + ckr * p2i + s1i;
        s1r = p2r;
        s1i = p2i;
        ckr = ckr + rzr;
        cki = cki + rzi;
        p2r = s2r * p1r;
        p2i = s2i * p1r;
        yr[(i)-1] = p2r;
        yi[(i)-1] = p2i;
        if (kflag >= 3) {
            goto L260;
        }
        str = std::fabs(p2r);
        sti = std::fabs(p2i);
        p2m = fmax_(str, sti);
        if (p2m <= ascle) {
            goto L260;
        }
        kflag = kflag + 1;
        ascle = bry[(kflag)-1];
        s1r = s1r * p1r;
        s1i = s1i * p1r;
        s2r = p2r;
        s2i = p2i;
        str = cssr[(kflag)-1];
        s1r = s1r * str;
        s1i = s1i * str;
        s2r = s2r * str;
        s2i = s2i * str;
        p1r = csrr[(kflag)-1];
    L260:;
    }
    return;
L261:;
    helim = 0.5e0 * elim;
    elm = std::exp(-elim);
    celmr = elm;
    ascle = bry[(1) - 1];
    zdr = zr;
    zdi = zi;
    ic = -1;
    j = 2;
    lim5_ = inu;
    for (i = 1; i <= lim5_; i += 1) {
        str = s2r;
        sti = s2i;
        s2r = str * ckr - sti * cki + s1r;
        s2i = sti * ckr + str * cki + s1i;
        s1r = str;
        s1i = sti;
        ckr = ckr + rzr;
        cki = cki + rzi;
        as = zabs(s2r, s2i);
        alas = std::log(as);
        p2r = -zdr + alas;
        if (p2r < (-elim)) {
            goto L263;
        }
        zlog(s2r, s2i, str, sti, idum);
        p2r = -zdr + str;
        p2i = -zdi + sti;
        p2m = std::exp(p2r) / tol;
        p1r = p2m * std::cos(p2i);
        p1i = p2m * std::sin(p2i);
        zuchk(p1r, p1i, nw, ascle, tol);
        if (nw != 0) {
            goto L263;
        }
        j = 3 - j;
        cyr[(j)-1] = p1r;
        cyi[(j)-1] = p1i;
        if (ic == (i - 1)) {
            goto L264;
        }
        ic = i;
        goto L262;
    L263:;
        if (alas < helim) {
            goto L262;
        }
        zdr = zdr - elim;
        s1r = s1r * celmr;
        s1i = s1i * celmr;
        s2r = s2r * celmr;
        s2i = s2i * celmr;
    L262:;
    }
    if (n != 1) {
        goto L270;
    }
    s1r = s2r;
    s1i = s2i;
    goto L270;
L264:;
    kflag = 1;
    inub = i + 1;
    s2r = cyr[(j)-1];
    s2i = cyi[(j)-1];
    j = 3 - j;
    s1r = cyr[(j)-1];
    s1i = cyi[(j)-1];
    if (inub <= inu) {
        goto L225;
    }
    if (n != 1) {
        goto L240;
    }
    s1r = s2r;
    s1i = s2i;
    goto L240;
L270:;
    yr[(1) - 1] = s1r;
    yi[(1) - 1] = s1i;
    if (n == 1) {
        goto L280;
    }
    yr[(2) - 1] = s2r;
    yi[(2) - 1] = s2i;
L280:;
    ascle = bry[(1) - 1];
    zkscl(zdr, zdi, fnu, n, yr, yi, nz, rzr, rzi, ascle, tol, elim);
    inu = n - nz;
    if (inu <= 0) {
        return;
    }
    kk = nz + 1;
    s1r = yr[(kk)-1];
    s1i = yi[(kk)-1];
    yr[(kk)-1] = s1r * csrr[(1) - 1];
    yi[(kk)-1] = s1i * csrr[(1) - 1];
    if (inu == 1) {
        return;
    }
    kk = nz + 2;
    s2r = yr[(kk)-1];
    s2i = yi[(kk)-1];
    yr[(kk)-1] = s2r * csrr[(1) - 1];
    yi[(kk)-1] = s2i * csrr[(1) - 1];
    if (inu == 2) {
        return;
    }
    t2 = fnu + (double)(kk - 1);
    ckr = t2 * rzr;
    cki = t2 * rzi;
    kflag = 1;
    goto L250;
L290:;
    koded = 2;
    iflag = 1;
    kflag = 2;
    goto L120;
L300:;
    s1r = coefr;
    s1i = coefi;
    s2r = coefr;
    s2i = coefi;
    goto L210;
L310:;
    nz = -2;
    return;
}
//=============================================================================
static void
zuni1(double zr, double zi, double fnu, int kode, int n, double* yr, double* yi, int& nz,
    int& nlast, double fnul, double tol, double elim, double alim)
{
    const double oner = 1.0e0;
    const double zeroi = 0.0e0;
    const double zeror = 0.0e0;
    double aphi = 0;
    double ascle = 0;
    double bry[3] = {};
    double c1r = 0;
    double c2i = 0;
    double c2m = 0;
    double c2r = 0;
    double crsc = 0;
    double cscl = 0;
    double csrr[3] = {};
    double cssr[3] = {};
    double cwrki[16] = {};
    double cwrkr[16] = {};
    double cyi[2] = {};
    double cyr[2] = {};
    double fn = 0;
    int i = 0;
    int iflag = 0;
    int init = 0;
    int k = 0;
    int m = 0;
    int nd = 0;
    int nn = 0;
    int nuf = 0;
    int nw = 0;
    double phii = 0;
    double phir = 0;
    double rast = 0;
    double rs1 = 0;
    double rzi = 0;
    double rzr = 0;
    double s1i = 0;
    double s1r = 0;
    double s2i = 0;
    double s2r = 0;
    double sti = 0;
    double str = 0;
    double sumi = 0;
    double sumr = 0;
    double zeta1i = 0;
    double zeta1r = 0;
    double zeta2i = 0;
    double zeta2r = 0;
    int lim0_ = 0;
    int lim1_ = 0;
    int lim2_ = 0;
    nz = 0;
    nd = n;
    nlast = 0;
    cscl = 1.0e0 / tol;
    crsc = tol;
    cssr[(1) - 1] = cscl;
    cssr[(2) - 1] = oner;
    cssr[(3) - 1] = crsc;
    csrr[(1) - 1] = crsc;
    csrr[(2) - 1] = oner;
    csrr[(3) - 1] = cscl;
    bry[(1) - 1] = 1.0e+3 * d1mach(1) / tol;
    fn = fmax_(fnu, 1.0e0);
    init = 0;
    zunik(zr, zi, fn, 1, 1, tol, init, phir, phii, zeta1r, zeta1i, zeta2r, zeta2i, sumr, sumi,
        cwrkr, cwrki);
    if (kode == 1) {
        goto L10;
    }
    str = zr + zeta2r;
    sti = zi + zeta2i;
    rast = fn / zabs(str, sti);
    str = str * rast * rast;
    sti = -sti * rast * rast;
    s1r = -zeta1r + str;
    s1i = -zeta1i + sti;
    goto L20;
L10:;
    s1r = -zeta1r + zeta2r;
    s1i = -zeta1i + zeta2i;
L20:;
    rs1 = s1r;
    if (std::fabs(rs1) > elim) {
        goto L130;
    }
L30:;
    nn = imin_(2, nd);
    lim0_ = nn;
    for (i = 1; i <= lim0_; i += 1) {
        fn = fnu + (double)(nd - i);
        init = 0;
        zunik(zr, zi, fn, 1, 0, tol, init, phir, phii, zeta1r, zeta1i, zeta2r, zeta2i, sumr, sumi,
            cwrkr, cwrki);
        if (kode == 1) {
            goto L40;
        }
        str = zr + zeta2r;
        sti = zi + zeta2i;
        rast = fn / zabs(str, sti);
        str = str * rast * rast;
        sti = -sti * rast * rast;
        s1r = -zeta1r + str;
        s1i = -zeta1i + sti + zi;
        goto L50;
    L40:;
        s1r = -zeta1r + zeta2r;
        s1i = -zeta1i + zeta2i;
    L50:;
        rs1 = s1r;
        if (std::fabs(rs1) > elim) {
            goto L110;
        }
        if (i == 1) {
            iflag = 2;
        }
        if (std::fabs(rs1) < alim) {
            goto L60;
        }
        aphi = zabs(phir, phii);
        rs1 = rs1 + std::log(aphi);
        if (std::fabs(rs1) > elim) {
            goto L110;
        }
        if (i == 1) {
            iflag = 1;
        }
        if (rs1 < 0.0e0) {
            goto L60;
        }
        if (i == 1) {
            iflag = 3;
        }
    L60:;
        s2r = phir * sumr - phii * sumi;
        s2i = phir * sumi + phii * sumr;
        str = std::exp(s1r) * cssr[(iflag)-1];
        s1r = str * std::cos(s1i);
        s1i = str * std::sin(s1i);
        str = s2r * s1r - s2i * s1i;
        s2i = s2r * s1i + s2i * s1r;
        s2r = str;
        if (iflag != 1) {
            goto L70;
        }
        zuchk(s2r, s2i, nw, bry[(1) - 1], tol);
        if (nw != 0) {
            goto L110;
        }
    L70:;
        cyr[(i)-1] = s2r;
        cyi[(i)-1] = s2i;
        m = nd - i + 1;
        yr[(m)-1] = s2r * csrr[(iflag)-1];
        yi[(m)-1] = s2i * csrr[(iflag)-1];
        ;
    }
    if (nd <= 2) {
        goto L100;
    }
    rast = 1.0e0 / zabs(zr, zi);
    str = zr * rast;
    sti = -zi * rast;
    rzr = (str + str) * rast;
    rzi = (sti + sti) * rast;
    bry[(2) - 1] = 1.0e0 / bry[(1) - 1];
    bry[(3) - 1] = d1mach(2);
    s1r = cyr[(1) - 1];
    s1i = cyi[(1) - 1];
    s2r = cyr[(2) - 1];
    s2i = cyi[(2) - 1];
    c1r = csrr[(iflag)-1];
    ascle = bry[(iflag)-1];
    k = nd - 2;
    fn = (double)(k);
    lim1_ = nd;
    for (i = 3; i <= lim1_; i += 1) {
        c2r = s2r;
        c2i = s2i;
        s2r = s1r + (fnu + fn) * (rzr * c2r - rzi * c2i);
        s2i = s1i + (fnu + fn) * (rzr * c2i + rzi * c2r);
        s1r = c2r;
        s1i = c2i;
        c2r = s2r * c1r;
        c2i = s2i * c1r;
        yr[(k)-1] = c2r;
        yi[(k)-1] = c2i;
        k = k - 1;
        fn = fn - 1.0e0;
        if (iflag >= 3) {
            goto L90;
        }
        str = std::fabs(c2r);
        sti = std::fabs(c2i);
        c2m = fmax_(str, sti);
        if (c2m <= ascle) {
            goto L90;
        }
        iflag = iflag + 1;
        ascle = bry[(iflag)-1];
        s1r = s1r * c1r;
        s1i = s1i * c1r;
        s2r = c2r;
        s2i = c2i;
        s1r = s1r * cssr[(iflag)-1];
        s1i = s1i * cssr[(iflag)-1];
        s2r = s2r * cssr[(iflag)-1];
        s2i = s2i * cssr[(iflag)-1];
        c1r = csrr[(iflag)-1];
    L90:;
    }
L100:;
    return;
L110:;
    if (rs1 > 0.0e0) {
        goto L120;
    }
    yr[(nd)-1] = zeror;
    yi[(nd)-1] = zeroi;
    nz = nz + 1;
    nd = nd - 1;
    if (nd == 0) {
        goto L100;
    }
    zuoik(zr, zi, fnu, kode, 1, nd, yr, yi, nuf, tol, elim, alim);
    if (nuf < 0) {
        goto L120;
    }
    nd = nd - nuf;
    nz = nz + nuf;
    if (nd == 0) {
        goto L100;
    }
    fn = fnu + (double)(nd - 1);
    if (fn >= fnul) {
        goto L30;
    }
    nlast = nd;
    return;
L120:;
    nz = -1;
    return;
L130:;
    if (rs1 > 0.0e0) {
        goto L120;
    }
    nz = n;
    lim2_ = n;
    for (i = 1; i <= lim2_; i += 1) {
        yr[(i)-1] = zeror;
        yi[(i)-1] = zeroi;
        ;
    }
    return;
}
//=============================================================================
static void
zuni2(double zr, double zi, double fnu, int kode, int n, double* yr, double* yi, int& nz,
    int& nlast, double fnul, double tol, double elim, double alim)
{
    const double aic = 1.265512123484645396e+00;
    const double oner = 1.0e0;
    const double hpi = 1.57079632679489662e+00;
    const double zeroi = 0.0e0;
    const double zeror = 0.0e0;
    static const double cipi[4] = { 0.0e0, 1.0e0, 0.0e0, -1.0e0 };
    static const double cipr[4] = { 1.0e0, 0.0e0, -1.0e0, 0.0e0 };
    double aarg = 0;
    double aii = 0;
    double air = 0;
    double ang = 0;
    double aphi = 0;
    double argi = 0;
    double argr = 0;
    double ascle = 0;
    double asumi = 0;
    double asumr = 0;
    double bry[3] = {};
    double bsumi = 0;
    double bsumr = 0;
    double c1r = 0;
    double c2i = 0;
    double c2m = 0;
    double c2r = 0;
    double car = 0;
    double cidi = 0;
    double crsc = 0;
    double cscl = 0;
    double csrr[3] = {};
    double cssr[3] = {};
    double cyi[2] = {};
    double cyr[2] = {};
    double daii = 0;
    double dair = 0;
    double fn = 0;
    int i = 0;
    int idum = 0;
    int iflag = 0;
    int in = 0;
    int inu = 0;
    int j = 0;
    int k = 0;
    int nai = 0;
    int nd = 0;
    int ndai = 0;
    int nn = 0;
    int nuf = 0;
    int nw = 0;
    double phii = 0;
    double phir = 0;
    double rast = 0;
    double raz = 0;
    double rs1 = 0;
    double rzi = 0;
    double rzr = 0;
    double s1i = 0;
    double s1r = 0;
    double s2i = 0;
    double s2r = 0;
    double sar = 0;
    double sti = 0;
    double str = 0;
    double zbi = 0;
    double zbr = 0;
    double zeta1i = 0;
    double zeta1r = 0;
    double zeta2i = 0;
    double zeta2r = 0;
    double zni = 0;
    double znr = 0;
    int lim0_ = 0;
    int lim1_ = 0;
    int lim2_ = 0;
    nz = 0;
    nd = n;
    nlast = 0;
    cscl = 1.0e0 / tol;
    crsc = tol;
    cssr[(1) - 1] = cscl;
    cssr[(2) - 1] = oner;
    cssr[(3) - 1] = crsc;
    csrr[(1) - 1] = crsc;
    csrr[(2) - 1] = oner;
    csrr[(3) - 1] = cscl;
    bry[(1) - 1] = 1.0e+3 * d1mach(1) / tol;
    znr = zi;
    zni = -zr;
    zbr = zr;
    zbi = zi;
    cidi = -oner;
    inu = fortranInt(fnu);
    ang = hpi * (fnu - (double)(inu));
    c2r = std::cos(ang);
    c2i = std::sin(ang);
    car = c2r;
    sar = c2i;
    in = inu + n - 1;
    in = imod_(in, 4) + 1;
    str = c2r * cipr[(in)-1] - c2i * cipi[(in)-1];
    c2i = c2r * cipi[(in)-1] + c2i * cipr[(in)-1];
    c2r = str;
    if (zi > 0.0e0) {
        goto L10;
    }
    znr = -znr;
    zbi = -zbi;
    cidi = -cidi;
    c2i = -c2i;
L10:;
    fn = fmax_(fnu, 1.0e0);
    zunhj(znr, zni, fn, 1, tol, phir, phii, argr, argi, zeta1r, zeta1i, zeta2r, zeta2i, asumr,
        asumi, bsumr, bsumi);
    if (kode == 1) {
        goto L20;
    }
    str = zbr + zeta2r;
    sti = zbi + zeta2i;
    rast = fn / zabs(str, sti);
    str = str * rast * rast;
    sti = -sti * rast * rast;
    s1r = -zeta1r + str;
    s1i = -zeta1i + sti;
    goto L30;
L20:;
    s1r = -zeta1r + zeta2r;
    s1i = -zeta1i + zeta2i;
L30:;
    rs1 = s1r;
    if (std::fabs(rs1) > elim) {
        goto L150;
    }
L40:;
    nn = imin_(2, nd);
    lim0_ = nn;
    for (i = 1; i <= lim0_; i += 1) {
        fn = fnu + (double)(nd - i);
        zunhj(znr, zni, fn, 0, tol, phir, phii, argr, argi, zeta1r, zeta1i, zeta2r, zeta2i, asumr,
            asumi, bsumr, bsumi);
        if (kode == 1) {
            goto L50;
        }
        str = zbr + zeta2r;
        sti = zbi + zeta2i;
        rast = fn / zabs(str, sti);
        str = str * rast * rast;
        sti = -sti * rast * rast;
        s1r = -zeta1r + str;
        s1i = -zeta1i + sti + std::fabs(zi);
        goto L60;
    L50:;
        s1r = -zeta1r + zeta2r;
        s1i = -zeta1i + zeta2i;
    L60:;
        rs1 = s1r;
        if (std::fabs(rs1) > elim) {
            goto L120;
        }
        if (i == 1) {
            iflag = 2;
        }
        if (std::fabs(rs1) < alim) {
            goto L70;
        }
        aphi = zabs(phir, phii);
        aarg = zabs(argr, argi);
        rs1 = rs1 + std::log(aphi) - 0.25e0 * std::log(aarg) - aic;
        if (std::fabs(rs1) > elim) {
            goto L120;
        }
        if (i == 1) {
            iflag = 1;
        }
        if (rs1 < 0.0e0) {
            goto L70;
        }
        if (i == 1) {
            iflag = 3;
        }
    L70:;
        zairy(argr, argi, 0, 2, air, aii, nai, idum);
        zairy(argr, argi, 1, 2, dair, daii, ndai, idum);
        str = dair * bsumr - daii * bsumi;
        sti = dair * bsumi + daii * bsumr;
        str = str + (air * asumr - aii * asumi);
        sti = sti + (air * asumi + aii * asumr);
        s2r = phir * str - phii * sti;
        s2i = phir * sti + phii * str;
        str = std::exp(s1r) * cssr[(iflag)-1];
        s1r = str * std::cos(s1i);
        s1i = str * std::sin(s1i);
        str = s2r * s1r - s2i * s1i;
        s2i = s2r * s1i + s2i * s1r;
        s2r = str;
        if (iflag != 1) {
            goto L80;
        }
        zuchk(s2r, s2i, nw, bry[(1) - 1], tol);
        if (nw != 0) {
            goto L120;
        }
    L80:;
        if (zi <= 0.0e0) {
            s2i = -s2i;
        }
        str = s2r * c2r - s2i * c2i;
        s2i = s2r * c2i + s2i * c2r;
        s2r = str;
        cyr[(i)-1] = s2r;
        cyi[(i)-1] = s2i;
        j = nd - i + 1;
        yr[(j)-1] = s2r * csrr[(iflag)-1];
        yi[(j)-1] = s2i * csrr[(iflag)-1];
        str = -c2i * cidi;
        c2i = c2r * cidi;
        c2r = str;
        ;
    }
    if (nd <= 2) {
        goto L110;
    }
    raz = 1.0e0 / zabs(zr, zi);
    str = zr * raz;
    sti = -zi * raz;
    rzr = (str + str) * raz;
    rzi = (sti + sti) * raz;
    bry[(2) - 1] = 1.0e0 / bry[(1) - 1];
    bry[(3) - 1] = d1mach(2);
    s1r = cyr[(1) - 1];
    s1i = cyi[(1) - 1];
    s2r = cyr[(2) - 1];
    s2i = cyi[(2) - 1];
    c1r = csrr[(iflag)-1];
    ascle = bry[(iflag)-1];
    k = nd - 2;
    fn = (double)(k);
    lim1_ = nd;
    for (i = 3; i <= lim1_; i += 1) {
        c2r = s2r;
        c2i = s2i;
        s2r = s1r + (fnu + fn) * (rzr * c2r - rzi * c2i);
        s2i = s1i + (fnu + fn) * (rzr * c2i + rzi * c2r);
        s1r = c2r;
        s1i = c2i;
        c2r = s2r * c1r;
        c2i = s2i * c1r;
        yr[(k)-1] = c2r;
        yi[(k)-1] = c2i;
        k = k - 1;
        fn = fn - 1.0e0;
        if (iflag >= 3) {
            goto L100;
        }
        str = std::fabs(c2r);
        sti = std::fabs(c2i);
        c2m = fmax_(str, sti);
        if (c2m <= ascle) {
            goto L100;
        }
        iflag = iflag + 1;
        ascle = bry[(iflag)-1];
        s1r = s1r * c1r;
        s1i = s1i * c1r;
        s2r = c2r;
        s2i = c2i;
        s1r = s1r * cssr[(iflag)-1];
        s1i = s1i * cssr[(iflag)-1];
        s2r = s2r * cssr[(iflag)-1];
        s2i = s2i * cssr[(iflag)-1];
        c1r = csrr[(iflag)-1];
    L100:;
    }
L110:;
    return;
L120:;
    if (rs1 > 0.0e0) {
        goto L140;
    }
    yr[(nd)-1] = zeror;
    yi[(nd)-1] = zeroi;
    nz = nz + 1;
    nd = nd - 1;
    if (nd == 0) {
        goto L110;
    }
    zuoik(zr, zi, fnu, kode, 1, nd, yr, yi, nuf, tol, elim, alim);
    if (nuf < 0) {
        goto L140;
    }
    nd = nd - nuf;
    nz = nz + nuf;
    if (nd == 0) {
        goto L110;
    }
    fn = fnu + (double)(nd - 1);
    if (fn < fnul) {
        goto L130;
    }
    in = inu + nd - 1;
    in = imod_(in, 4) + 1;
    c2r = car * cipr[(in)-1] - sar * cipi[(in)-1];
    c2i = car * cipi[(in)-1] + sar * cipr[(in)-1];
    if (zi <= 0.0e0) {
        c2i = -c2i;
    }
    goto L40;
L130:;
    nlast = nd;
    return;
L140:;
    nz = -1;
    return;
L150:;
    if (rs1 > 0.0e0) {
        goto L140;
    }
    nz = n;
    lim2_ = n;
    for (i = 1; i <= lim2_; i += 1) {
        yr[(i)-1] = zeror;
        yi[(i)-1] = zeroi;
        ;
    }
    return;
}
//=============================================================================
static void
zbuni(double zr, double zi, double fnu, int kode, int n, double* yr, double* yi, int& nz, int nui,
    int& nlast, double fnul, double tol, double elim, double alim)
{
    double ascle = 0;
    double ax = 0;
    double ay = 0;
    double bry[3] = {};
    double c1i = 0;
    double c1m = 0;
    double c1r = 0;
    double csclr = 0;
    double cscrr = 0;
    double cyi[2] = {};
    double cyr[2] = {};
    double dfnu = 0;
    double fnui = 0;
    double gnu = 0;
    int i = 0;
    int iflag = 0;
    int iform = 0;
    int k = 0;
    int nl = 0;
    int nw = 0;
    double raz = 0;
    double rzi = 0;
    double rzr = 0;
    double s1i = 0;
    double s1r = 0;
    double s2i = 0;
    double s2r = 0;
    double sti = 0;
    double str = 0;
    int lim0_ = 0;
    int lim1_ = 0;
    nz = 0;
    ax = std::fabs(zr) * 1.7321e0;
    ay = std::fabs(zi);
    iform = 1;
    if (ay > ax) {
        iform = 2;
    }
    if (nui == 0) {
        goto L60;
    }
    fnui = (double)(nui);
    dfnu = fnu + (double)(n - 1);
    gnu = dfnu + fnui;
    if (iform == 2) {
        goto L10;
    }
    zuni1(zr, zi, gnu, kode, 2, cyr, cyi, nw, nlast, fnul, tol, elim, alim);
    goto L20;
L10:;
    zuni2(zr, zi, gnu, kode, 2, cyr, cyi, nw, nlast, fnul, tol, elim, alim);
L20:;
    if (nw < 0) {
        goto L50;
    }
    if (nw != 0) {
        goto L90;
    }
    str = zabs(cyr[(1) - 1], cyi[(1) - 1]);
    bry[(1) - 1] = 1.0e+3 * d1mach(1) / tol;
    bry[(2) - 1] = 1.0e0 / bry[(1) - 1];
    bry[(3) - 1] = bry[(2) - 1];
    iflag = 2;
    ascle = bry[(2) - 1];
    csclr = 1.0e0;
    if (str > bry[(1) - 1]) {
        goto L21;
    }
    iflag = 1;
    ascle = bry[(1) - 1];
    csclr = 1.0e0 / tol;
    goto L25;
L21:;
    if (str < bry[(2) - 1]) {
        goto L25;
    }
    iflag = 3;
    ascle = bry[(3) - 1];
    csclr = tol;
L25:;
    cscrr = 1.0e0 / csclr;
    s1r = cyr[(2) - 1] * csclr;
    s1i = cyi[(2) - 1] * csclr;
    s2r = cyr[(1) - 1] * csclr;
    s2i = cyi[(1) - 1] * csclr;
    raz = 1.0e0 / zabs(zr, zi);
    str = zr * raz;
    sti = -zi * raz;
    rzr = (str + str) * raz;
    rzi = (sti + sti) * raz;
    lim0_ = nui;
    for (i = 1; i <= lim0_; i += 1) {
        str = s2r;
        sti = s2i;
        s2r = (dfnu + fnui) * (rzr * str - rzi * sti) + s1r;
        s2i = (dfnu + fnui) * (rzr * sti + rzi * str) + s1i;
        s1r = str;
        s1i = sti;
        fnui = fnui - 1.0e0;
        if (iflag >= 3) {
            goto L30;
        }
        str = s2r * cscrr;
        sti = s2i * cscrr;
        c1r = std::fabs(str);
        c1i = std::fabs(sti);
        c1m = fmax_(c1r, c1i);
        if (c1m <= ascle) {
            goto L30;
        }
        iflag = iflag + 1;
        ascle = bry[(iflag)-1];
        s1r = s1r * cscrr;
        s1i = s1i * cscrr;
        s2r = str;
        s2i = sti;
        csclr = csclr * tol;
        cscrr = 1.0e0 / csclr;
        s1r = s1r * csclr;
        s1i = s1i * csclr;
        s2r = s2r * csclr;
        s2i = s2i * csclr;
    L30:;
    }
    yr[(n)-1] = s2r * cscrr;
    yi[(n)-1] = s2i * cscrr;
    if (n == 1) {
        return;
    }
    nl = n - 1;
    fnui = (double)(nl);
    k = nl;
    lim1_ = nl;
    for (i = 1; i <= lim1_; i += 1) {
        str = s2r;
        sti = s2i;
        s2r = (fnu + fnui) * (rzr * str - rzi * sti) + s1r;
        s2i = (fnu + fnui) * (rzr * sti + rzi * str) + s1i;
        s1r = str;
        s1i = sti;
        str = s2r * cscrr;
        sti = s2i * cscrr;
        yr[(k)-1] = str;
        yi[(k)-1] = sti;
        fnui = fnui - 1.0e0;
        k = k - 1;
        if (iflag >= 3) {
            goto L40;
        }
        c1r = std::fabs(str);
        c1i = std::fabs(sti);
        c1m = fmax_(c1r, c1i);
        if (c1m <= ascle) {
            goto L40;
        }
        iflag = iflag + 1;
        ascle = bry[(iflag)-1];
        s1r = s1r * cscrr;
        s1i = s1i * cscrr;
        s2r = str;
        s2i = sti;
        csclr = csclr * tol;
        cscrr = 1.0e0 / csclr;
        s1r = s1r * csclr;
        s1i = s1i * csclr;
        s2r = s2r * csclr;
        s2i = s2i * csclr;
    L40:;
    }
    return;
L50:;
    nz = -1;
    if (nw == (-2)) {
        nz = -2;
    }
    return;
L60:;
    if (iform == 2) {
        goto L70;
    }
    zuni1(zr, zi, fnu, kode, n, yr, yi, nw, nlast, fnul, tol, elim, alim);
    goto L80;
L70:;
    zuni2(zr, zi, fnu, kode, n, yr, yi, nw, nlast, fnul, tol, elim, alim);
L80:;
    if (nw < 0) {
        goto L50;
    }
    nz = nw;
    return;
L90:;
    nlast = n;
    return;
}
//=============================================================================
static void
zunk1(double zr, double zi, double fnu, int kode, int mr, int n, double* yr, double* yi, int& nz,
    double tol, double elim, double alim)
{
    const double oner = 1.0e0;
    const double pi = 3.14159265358979324e0;
    const double zeroi = 0.0e0;
    const double zeror = 0.0e0;
    double ang = 0;
    double aphi = 0;
    double asc = 0;
    double ascle = 0;
    double bry[3] = {};
    double c1i = 0;
    double c1r = 0;
    double c2i = 0;
    double c2m = 0;
    double c2r = 0;
    double cki = 0;
    double ckr = 0;
    double crsc = 0;
    double cscl = 0;
    double csgni = 0;
    double cspni = 0;
    double cspnr = 0;
    double csr = 0;
    double csrr[3] = {};
    double cssr[3] = {};
    double cwrki[48] = {};
    double cwrkr[48] = {};
    double cyi[2] = {};
    double cyr[2] = {};
    double fmr = 0;
    double fn = 0;
    double fnf = 0;
    int i = 0;
    int ib = 0;
    int ic = 0;
    int iflag = 0;
    int ifn = 0;
    int il = 0;
    int init[2] = {};
    int initd = 0;
    int inu = 0;
    int ipard = 0;
    int iuf = 0;
    int j = 0;
    int k = 0;
    int kdflg = 0;
    int kflag = 0;
    int kk = 0;
    int m = 0;
    int nw = 0;
    double phidi = 0;
    double phidr = 0;
    double phii[2] = {};
    double phir[2] = {};
    double rast = 0;
    double razr = 0;
    double rs1 = 0;
    double rzi = 0;
    double rzr = 0;
    double s1i = 0;
    double s1r = 0;
    double s2i = 0;
    double s2r = 0;
    double sgn = 0;
    double sti = 0;
    double str = 0;
    double sumdi = 0;
    double sumdr = 0;
    double sumi[2] = {};
    double sumr[2] = {};
    double zet1di = 0;
    double zet1dr = 0;
    double zet2di = 0;
    double zet2dr = 0;
    double zeta1i[2] = {};
    double zeta1r[2] = {};
    double zeta2i[2] = {};
    double zeta2r[2] = {};
    double zri = 0;
    double zrr = 0;
    int lim0_ = 0;
    int lim1_ = 0;
    int lim2_ = 0;
    int lim3_ = 0;
    int lim4_ = 0;
    kdflg = 1;
    nz = 0;
    cscl = 1.0e0 / tol;
    crsc = tol;
    cssr[(1) - 1] = cscl;
    cssr[(2) - 1] = oner;
    cssr[(3) - 1] = crsc;
    csrr[(1) - 1] = crsc;
    csrr[(2) - 1] = oner;
    csrr[(3) - 1] = cscl;
    bry[(1) - 1] = 1.0e+3 * d1mach(1) / tol;
    bry[(2) - 1] = 1.0e0 / bry[(1) - 1];
    bry[(3) - 1] = d1mach(2);
    zrr = zr;
    zri = zi;
    if (zr >= 0.0e0) {
        goto L10;
    }
    zrr = -zr;
    zri = -zi;
L10:;
    j = 2;
    lim0_ = n;
    for (i = 1; i <= lim0_; i += 1) {
        j = 3 - j;
        fn = fnu + (double)(i - 1);
        init[(j)-1] = 0;
        zunik(zrr, zri, fn, 2, 0, tol, init[(j)-1], phir[(j)-1], phii[(j)-1], zeta1r[(j)-1],
            zeta1i[(j)-1], zeta2r[(j)-1], zeta2i[(j)-1], sumr[(j)-1], sumi[(j)-1],
            &cwrkr[((1) - 1) + (16) * ((j)-1)], &cwrki[((1) - 1) + (16) * ((j)-1)]);
        if (kode == 1) {
            goto L20;
        }
        str = zrr + zeta2r[(j)-1];
        sti = zri + zeta2i[(j)-1];
        rast = fn / zabs(str, sti);
        str = str * rast * rast;
        sti = -sti * rast * rast;
        s1r = zeta1r[(j)-1] - str;
        s1i = zeta1i[(j)-1] - sti;
        goto L30;
    L20:;
        s1r = zeta1r[(j)-1] - zeta2r[(j)-1];
        s1i = zeta1i[(j)-1] - zeta2i[(j)-1];
    L30:;
        rs1 = s1r;
        if (std::fabs(rs1) > elim) {
            goto L60;
        }
        if (kdflg == 1) {
            kflag = 2;
        }
        if (std::fabs(rs1) < alim) {
            goto L40;
        }
        aphi = zabs(phir[(j)-1], phii[(j)-1]);
        rs1 = rs1 + std::log(aphi);
        if (std::fabs(rs1) > elim) {
            goto L60;
        }
        if (kdflg == 1) {
            kflag = 1;
        }
        if (rs1 < 0.0e0) {
            goto L40;
        }
        if (kdflg == 1) {
            kflag = 3;
        }
    L40:;
        s2r = phir[(j)-1] * sumr[(j)-1] - phii[(j)-1] * sumi[(j)-1];
        s2i = phir[(j)-1] * sumi[(j)-1] + phii[(j)-1] * sumr[(j)-1];
        str = std::exp(s1r) * cssr[(kflag)-1];
        s1r = str * std::cos(s1i);
        s1i = str * std::sin(s1i);
        str = s2r * s1r - s2i * s1i;
        s2i = s1r * s2i + s2r * s1i;
        s2r = str;
        if (kflag != 1) {
            goto L50;
        }
        zuchk(s2r, s2i, nw, bry[(1) - 1], tol);
        if (nw != 0) {
            goto L60;
        }
    L50:;
        cyr[(kdflg)-1] = s2r;
        cyi[(kdflg)-1] = s2i;
        yr[(i)-1] = s2r * csrr[(kflag)-1];
        yi[(i)-1] = s2i * csrr[(kflag)-1];
        if (kdflg == 2) {
            goto L75;
        }
        kdflg = 2;
        goto L70;
    L60:;
        if (rs1 > 0.0e0) {
            goto L300;
        }
        if (zr < 0.0e0) {
            goto L300;
        }
        kdflg = 1;
        yr[(i)-1] = zeror;
        yi[(i)-1] = zeroi;
        nz = nz + 1;
        if (i == 1) {
            goto L70;
        }
        if ((yr[(i - 1) - 1] == zeror) && (yi[(i - 1) - 1] == zeroi)) {
            goto L70;
        }
        yr[(i - 1) - 1] = zeror;
        yi[(i - 1) - 1] = zeroi;
        nz = nz + 1;
    L70:;
    }
    i = n;
L75:;
    razr = 1.0e0 / zabs(zrr, zri);
    str = zrr * razr;
    sti = -zri * razr;
    rzr = (str + str) * razr;
    rzi = (sti + sti) * razr;
    ckr = fn * rzr;
    cki = fn * rzi;
    ib = i + 1;
    if (n < ib) {
        goto L160;
    }
    fn = fnu + (double)(n - 1);
    ipard = 1;
    if (mr != 0) {
        ipard = 0;
    }
    initd = 0;
    zunik(zrr, zri, fn, 2, ipard, tol, initd, phidr, phidi, zet1dr, zet1di, zet2dr, zet2di, sumdr,
        sumdi, &cwrkr[((1) - 1) + (16) * ((3) - 1)], &cwrki[((1) - 1) + (16) * ((3) - 1)]);
    if (kode == 1) {
        goto L80;
    }
    str = zrr + zet2dr;
    sti = zri + zet2di;
    rast = fn / zabs(str, sti);
    str = str * rast * rast;
    sti = -sti * rast * rast;
    s1r = zet1dr - str;
    s1i = zet1di - sti;
    goto L90;
L80:;
    s1r = zet1dr - zet2dr;
    s1i = zet1di - zet2di;
L90:;
    rs1 = s1r;
    if (std::fabs(rs1) > elim) {
        goto L95;
    }
    if (std::fabs(rs1) < alim) {
        goto L100;
    }
    aphi = zabs(phidr, phidi);
    rs1 = rs1 + std::log(aphi);
    if (std::fabs(rs1) < elim) {
        goto L100;
    }
L95:;
    if (std::fabs(rs1) > 0.0e0) {
        goto L300;
    }
    if (zr < 0.0e0) {
        goto L300;
    }
    nz = n;
    lim1_ = n;
    for (i = 1; i <= lim1_; i += 1) {
        yr[(i)-1] = zeror;
        yi[(i)-1] = zeroi;
        ;
    }
    return;
L100:;
    s1r = cyr[(1) - 1];
    s1i = cyi[(1) - 1];
    s2r = cyr[(2) - 1];
    s2i = cyi[(2) - 1];
    c1r = csrr[(kflag)-1];
    ascle = bry[(kflag)-1];
    lim2_ = n;
    for (i = ib; i <= lim2_; i += 1) {
        c2r = s2r;
        c2i = s2i;
        s2r = ckr * c2r - cki * c2i + s1r;
        s2i = ckr * c2i + cki * c2r + s1i;
        s1r = c2r;
        s1i = c2i;
        ckr = ckr + rzr;
        cki = cki + rzi;
        c2r = s2r * c1r;
        c2i = s2i * c1r;
        yr[(i)-1] = c2r;
        yi[(i)-1] = c2i;
        if (kflag >= 3) {
            goto L120;
        }
        str = std::fabs(c2r);
        sti = std::fabs(c2i);
        c2m = fmax_(str, sti);
        if (c2m <= ascle) {
            goto L120;
        }
        kflag = kflag + 1;
        ascle = bry[(kflag)-1];
        s1r = s1r * c1r;
        s1i = s1i * c1r;
        s2r = c2r;
        s2i = c2i;
        s1r = s1r * cssr[(kflag)-1];
        s1i = s1i * cssr[(kflag)-1];
        s2r = s2r * cssr[(kflag)-1];
        s2i = s2i * cssr[(kflag)-1];
        c1r = csrr[(kflag)-1];
    L120:;
    }
L160:;
    if (mr == 0) {
        return;
    }
    nz = 0;
    fmr = (double)(mr);
    sgn = -std::copysign(pi, fmr);
    csgni = sgn;
    inu = fortranInt(fnu);
    fnf = fnu - (double)(inu);
    ifn = inu + n - 1;
    ang = fnf * sgn;
    cspnr = std::cos(ang);
    cspni = std::sin(ang);
    if (imod_(ifn, 2) == 0) {
        goto L170;
    }
    cspnr = -cspnr;
    cspni = -cspni;
L170:;
    asc = bry[(1) - 1];
    iuf = 0;
    kk = n;
    kdflg = 1;
    ib = ib - 1;
    ic = ib - 1;
    lim3_ = n;
    for (k = 1; k <= lim3_; k += 1) {
        fn = fnu + (double)(kk - 1);
        m = 3;
        if (n > 2) {
            goto L175;
        }
    L172:;
        initd = init[(j)-1];
        phidr = phir[(j)-1];
        phidi = phii[(j)-1];
        zet1dr = zeta1r[(j)-1];
        zet1di = zeta1i[(j)-1];
        zet2dr = zeta2r[(j)-1];
        zet2di = zeta2i[(j)-1];
        sumdr = sumr[(j)-1];
        sumdi = sumi[(j)-1];
        m = j;
        j = 3 - j;
        goto L180;
    L175:;
        if ((kk == n) && (ib < n)) {
            goto L180;
        }
        if ((kk == ib) || (kk == ic)) {
            goto L172;
        }
        initd = 0;
    L180:;
        zunik(zrr, zri, fn, 1, 0, tol, initd, phidr, phidi, zet1dr, zet1di, zet2dr, zet2di, sumdr,
            sumdi, &cwrkr[((1) - 1) + (16) * ((m)-1)], &cwrki[((1) - 1) + (16) * ((m)-1)]);
        if (kode == 1) {
            goto L200;
        }
        str = zrr + zet2dr;
        sti = zri + zet2di;
        rast = fn / zabs(str, sti);
        str = str * rast * rast;
        sti = -sti * rast * rast;
        s1r = -zet1dr + str;
        s1i = -zet1di + sti;
        goto L210;
    L200:;
        s1r = -zet1dr + zet2dr;
        s1i = -zet1di + zet2di;
    L210:;
        rs1 = s1r;
        if (std::fabs(rs1) > elim) {
            goto L260;
        }
        if (kdflg == 1) {
            iflag = 2;
        }
        if (std::fabs(rs1) < alim) {
            goto L220;
        }
        aphi = zabs(phidr, phidi);
        rs1 = rs1 + std::log(aphi);
        if (std::fabs(rs1) > elim) {
            goto L260;
        }
        if (kdflg == 1) {
            iflag = 1;
        }
        if (rs1 < 0.0e0) {
            goto L220;
        }
        if (kdflg == 1) {
            iflag = 3;
        }
    L220:;
        str = phidr * sumdr - phidi * sumdi;
        sti = phidr * sumdi + phidi * sumdr;
        s2r = -csgni * sti;
        s2i = csgni * str;
        str = std::exp(s1r) * cssr[(iflag)-1];
        s1r = str * std::cos(s1i);
        s1i = str * std::sin(s1i);
        str = s2r * s1r - s2i * s1i;
        s2i = s2r * s1i + s2i * s1r;
        s2r = str;
        if (iflag != 1) {
            goto L230;
        }
        zuchk(s2r, s2i, nw, bry[(1) - 1], tol);
        if (nw == 0) {
            goto L230;
        }
        s2r = zeror;
        s2i = zeroi;
    L230:;
        cyr[(kdflg)-1] = s2r;
        cyi[(kdflg)-1] = s2i;
        c2r = s2r;
        c2i = s2i;
        s2r = s2r * csrr[(iflag)-1];
        s2i = s2i * csrr[(iflag)-1];
        s1r = yr[(kk)-1];
        s1i = yi[(kk)-1];
        if (kode == 1) {
            goto L250;
        }
        zs1s2(zrr, zri, s1r, s1i, s2r, s2i, nw, asc, alim, iuf);
        nz = nz + nw;
    L250:;
        yr[(kk)-1] = s1r * cspnr - s1i * cspni + s2r;
        yi[(kk)-1] = cspnr * s1i + cspni * s1r + s2i;
        kk = kk - 1;
        cspnr = -cspnr;
        cspni = -cspni;
        if (c2r != 0.0e0 || c2i != 0.0e0) {
            goto L255;
        }
        kdflg = 1;
        goto L270;
    L255:;
        if (kdflg == 2) {
            goto L275;
        }
        kdflg = 2;
        goto L270;
    L260:;
        if (rs1 > 0.0e0) {
            goto L300;
        }
        s2r = zeror;
        s2i = zeroi;
        goto L230;
    L270:;
    }
    k = n;
L275:;
    il = n - k;
    if (il == 0) {
        return;
    }
    s1r = cyr[(1) - 1];
    s1i = cyi[(1) - 1];
    s2r = cyr[(2) - 1];
    s2i = cyi[(2) - 1];
    csr = csrr[(iflag)-1];
    ascle = bry[(iflag)-1];
    fn = (double)(inu + il);
    lim4_ = il;
    for (i = 1; i <= lim4_; i += 1) {
        c2r = s2r;
        c2i = s2i;
        s2r = s1r + (fn + fnf) * (rzr * c2r - rzi * c2i);
        s2i = s1i + (fn + fnf) * (rzr * c2i + rzi * c2r);
        s1r = c2r;
        s1i = c2i;
        fn = fn - 1.0e0;
        c2r = s2r * csr;
        c2i = s2i * csr;
        ckr = c2r;
        cki = c2i;
        c1r = yr[(kk)-1];
        c1i = yi[(kk)-1];
        if (kode == 1) {
            goto L280;
        }
        zs1s2(zrr, zri, c1r, c1i, c2r, c2i, nw, asc, alim, iuf);
        nz = nz + nw;
    L280:;
        yr[(kk)-1] = c1r * cspnr - c1i * cspni + c2r;
        yi[(kk)-1] = c1r * cspni + c1i * cspnr + c2i;
        kk = kk - 1;
        cspnr = -cspnr;
        cspni = -cspni;
        if (iflag >= 3) {
            goto L290;
        }
        c2r = std::fabs(ckr);
        c2i = std::fabs(cki);
        c2m = fmax_(c2r, c2i);
        if (c2m <= ascle) {
            goto L290;
        }
        iflag = iflag + 1;
        ascle = bry[(iflag)-1];
        s1r = s1r * csr;
        s1i = s1i * csr;
        s2r = ckr;
        s2i = cki;
        s1r = s1r * cssr[(iflag)-1];
        s1i = s1i * cssr[(iflag)-1];
        s2r = s2r * cssr[(iflag)-1];
        s2i = s2i * cssr[(iflag)-1];
        csr = csrr[(iflag)-1];
    L290:;
    }
    return;
L300:;
    nz = -1;
    return;
}
//=============================================================================
static void
zunk2(double zr, double zi, double fnu, int kode, int mr, int n, double* yr, double* yi, int& nz,
    double tol, double elim, double alim)
{
    const double aic = 1.26551212348464539e+00;
    const double oner = 1.0e0;
    const double cr1i = 1.73205080756887729e0;
    const double cr1r = 1.0e0;
    const double cr2i = -8.66025403784438647e-01;
    const double cr2r = -0.5e0;
    const double hpi = 1.57079632679489662e+00;
    const double pi = 3.14159265358979324e+00;
    const double zeroi = 0.0e0;
    const double zeror = 0.0e0;
    static const double cipi[4] = { 0.0e0, -1.0e0, 0.0e0, 1.0e0 };
    static const double cipr[4] = { 1.0e0, 0.0e0, -1.0e0, 0.0e0 };
    double aarg = 0;
    double aii = 0;
    double air = 0;
    double ang = 0;
    double aphi = 0;
    double argdi = 0;
    double argdr = 0;
    double argi[2] = {};
    double argr[2] = {};
    double asc = 0;
    double ascle = 0;
    double asumdi = 0;
    double asumdr = 0;
    double asumi[2] = {};
    double asumr[2] = {};
    double bry[3] = {};
    double bsumdi = 0;
    double bsumdr = 0;
    double bsumi[2] = {};
    double bsumr[2] = {};
    double c1i = 0;
    double c1r = 0;
    double c2i = 0;
    double c2m = 0;
    double c2r = 0;
    double car = 0;
    double cki = 0;
    double ckr = 0;
    double crsc = 0;
    double cscl = 0;
    double csgni = 0;
    double csi = 0;
    double cspni = 0;
    double cspnr = 0;
    double csr = 0;
    double csrr[3] = {};
    double cssr[3] = {};
    double cyi[2] = {};
    double cyr[2] = {};
    double daii = 0;
    double dair = 0;
    double fmr = 0;
    double fn = 0;
    double fnf = 0;
    int i = 0;
    int ib = 0;
    int ic = 0;
    int idum = 0;
    int iflag = 0;
    int ifn = 0;
    int il = 0;
    int in = 0;
    int inu = 0;
    int ipard = 0;
    int iuf = 0;
    int j = 0;
    int k = 0;
    int kdflg = 0;
    int kflag = 0;
    int kk = 0;
    int nai = 0;
    int ndai = 0;
    int nw = 0;
    double phidi = 0;
    double phidr = 0;
    double phii[2] = {};
    double phir[2] = {};
    double pti = 0;
    double ptr = 0;
    double rast = 0;
    double razr = 0;
    double rs1 = 0;
    double rzi = 0;
    double rzr = 0;
    double s1i = 0;
    double s1r = 0;
    double s2i = 0;
    double s2r = 0;
    double sar = 0;
    double sgn = 0;
    double sti = 0;
    double str = 0;
    double yy = 0;
    double zbi = 0;
    double zbr = 0;
    double zet1di = 0;
    double zet1dr = 0;
    double zet2di = 0;
    double zet2dr = 0;
    double zeta1i[2] = {};
    double zeta1r[2] = {};
    double zeta2i[2] = {};
    double zeta2r[2] = {};
    double zni = 0;
    double znr = 0;
    double zri = 0;
    double zrr = 0;
    int lim0_ = 0;
    int lim1_ = 0;
    int lim2_ = 0;
    int lim3_ = 0;
    int lim4_ = 0;
    kdflg = 1;
    nz = 0;
    cscl = 1.0e0 / tol;
    crsc = tol;
    cssr[(1) - 1] = cscl;
    cssr[(2) - 1] = oner;
    cssr[(3) - 1] = crsc;
    csrr[(1) - 1] = crsc;
    csrr[(2) - 1] = oner;
    csrr[(3) - 1] = cscl;
    bry[(1) - 1] = 1.0e+3 * d1mach(1) / tol;
    bry[(2) - 1] = 1.0e0 / bry[(1) - 1];
    bry[(3) - 1] = d1mach(2);
    zrr = zr;
    zri = zi;
    if (zr >= 0.0e0) {
        goto L10;
    }
    zrr = -zr;
    zri = -zi;
L10:;
    yy = zri;
    znr = zri;
    zni = -zrr;
    zbr = zrr;
    zbi = zri;
    inu = fortranInt(fnu);
    fnf = fnu - (double)(inu);
    ang = -hpi * fnf;
    car = std::cos(ang);
    sar = std::sin(ang);
    c2r = hpi * sar;
    c2i = -hpi * car;
    kk = imod_(inu, 4) + 1;
    str = c2r * cipr[(kk)-1] - c2i * cipi[(kk)-1];
    sti = c2r * cipi[(kk)-1] + c2i * cipr[(kk)-1];
    csr = cr1r * str - cr1i * sti;
    csi = cr1r * sti + cr1i * str;
    if (yy > 0.0e0) {
        goto L20;
    }
    znr = -znr;
    zbi = -zbi;
L20:;
    j = 2;
    lim0_ = n;
    for (i = 1; i <= lim0_; i += 1) {
        j = 3 - j;
        fn = fnu + (double)(i - 1);
        zunhj(znr, zni, fn, 0, tol, phir[(j)-1], phii[(j)-1], argr[(j)-1], argi[(j)-1],
            zeta1r[(j)-1], zeta1i[(j)-1], zeta2r[(j)-1], zeta2i[(j)-1], asumr[(j)-1], asumi[(j)-1],
            bsumr[(j)-1], bsumi[(j)-1]);
        if (kode == 1) {
            goto L30;
        }
        str = zbr + zeta2r[(j)-1];
        sti = zbi + zeta2i[(j)-1];
        rast = fn / zabs(str, sti);
        str = str * rast * rast;
        sti = -sti * rast * rast;
        s1r = zeta1r[(j)-1] - str;
        s1i = zeta1i[(j)-1] - sti;
        goto L40;
    L30:;
        s1r = zeta1r[(j)-1] - zeta2r[(j)-1];
        s1i = zeta1i[(j)-1] - zeta2i[(j)-1];
    L40:;
        rs1 = s1r;
        if (std::fabs(rs1) > elim) {
            goto L70;
        }
        if (kdflg == 1) {
            kflag = 2;
        }
        if (std::fabs(rs1) < alim) {
            goto L50;
        }
        aphi = zabs(phir[(j)-1], phii[(j)-1]);
        aarg = zabs(argr[(j)-1], argi[(j)-1]);
        rs1 = rs1 + std::log(aphi) - 0.25e0 * std::log(aarg) - aic;
        if (std::fabs(rs1) > elim) {
            goto L70;
        }
        if (kdflg == 1) {
            kflag = 1;
        }
        if (rs1 < 0.0e0) {
            goto L50;
        }
        if (kdflg == 1) {
            kflag = 3;
        }
    L50:;
        c2r = argr[(j)-1] * cr2r - argi[(j)-1] * cr2i;
        c2i = argr[(j)-1] * cr2i + argi[(j)-1] * cr2r;
        zairy(c2r, c2i, 0, 2, air, aii, nai, idum);
        zairy(c2r, c2i, 1, 2, dair, daii, ndai, idum);
        str = dair * bsumr[(j)-1] - daii * bsumi[(j)-1];
        sti = dair * bsumi[(j)-1] + daii * bsumr[(j)-1];
        ptr = str * cr2r - sti * cr2i;
        pti = str * cr2i + sti * cr2r;
        str = ptr + (air * asumr[(j)-1] - aii * asumi[(j)-1]);
        sti = pti + (air * asumi[(j)-1] + aii * asumr[(j)-1]);
        ptr = str * phir[(j)-1] - sti * phii[(j)-1];
        pti = str * phii[(j)-1] + sti * phir[(j)-1];
        s2r = ptr * csr - pti * csi;
        s2i = ptr * csi + pti * csr;
        str = std::exp(s1r) * cssr[(kflag)-1];
        s1r = str * std::cos(s1i);
        s1i = str * std::sin(s1i);
        str = s2r * s1r - s2i * s1i;
        s2i = s1r * s2i + s2r * s1i;
        s2r = str;
        if (kflag != 1) {
            goto L60;
        }
        zuchk(s2r, s2i, nw, bry[(1) - 1], tol);
        if (nw != 0) {
            goto L70;
        }
    L60:;
        if (yy <= 0.0e0) {
            s2i = -s2i;
        }
        cyr[(kdflg)-1] = s2r;
        cyi[(kdflg)-1] = s2i;
        yr[(i)-1] = s2r * csrr[(kflag)-1];
        yi[(i)-1] = s2i * csrr[(kflag)-1];
        str = csi;
        csi = -csr;
        csr = str;
        if (kdflg == 2) {
            goto L85;
        }
        kdflg = 2;
        goto L80;
    L70:;
        if (rs1 > 0.0e0) {
            goto L320;
        }
        if (zr < 0.0e0) {
            goto L320;
        }
        kdflg = 1;
        yr[(i)-1] = zeror;
        yi[(i)-1] = zeroi;
        nz = nz + 1;
        str = csi;
        csi = -csr;
        csr = str;
        if (i == 1) {
            goto L80;
        }
        if ((yr[(i - 1) - 1] == zeror) && (yi[(i - 1) - 1] == zeroi)) {
            goto L80;
        }
        yr[(i - 1) - 1] = zeror;
        yi[(i - 1) - 1] = zeroi;
        nz = nz + 1;
    L80:;
    }
    i = n;
L85:;
    razr = 1.0e0 / zabs(zrr, zri);
    str = zrr * razr;
    sti = -zri * razr;
    rzr = (str + str) * razr;
    rzi = (sti + sti) * razr;
    ckr = fn * rzr;
    cki = fn * rzi;
    ib = i + 1;
    if (n < ib) {
        goto L180;
    }
    fn = fnu + (double)(n - 1);
    ipard = 1;
    if (mr != 0) {
        ipard = 0;
    }
    zunhj(znr, zni, fn, ipard, tol, phidr, phidi, argdr, argdi, zet1dr, zet1di, zet2dr, zet2di,
        asumdr, asumdi, bsumdr, bsumdi);
    if (kode == 1) {
        goto L90;
    }
    str = zbr + zet2dr;
    sti = zbi + zet2di;
    rast = fn / zabs(str, sti);
    str = str * rast * rast;
    sti = -sti * rast * rast;
    s1r = zet1dr - str;
    s1i = zet1di - sti;
    goto L100;
L90:;
    s1r = zet1dr - zet2dr;
    s1i = zet1di - zet2di;
L100:;
    rs1 = s1r;
    if (std::fabs(rs1) > elim) {
        goto L105;
    }
    if (std::fabs(rs1) < alim) {
        goto L120;
    }
    aphi = zabs(phidr, phidi);
    rs1 = rs1 + std::log(aphi);
    if (std::fabs(rs1) < elim) {
        goto L120;
    }
L105:;
    if (rs1 > 0.0e0) {
        goto L320;
    }
    if (zr < 0.0e0) {
        goto L320;
    }
    nz = n;
    lim1_ = n;
    for (i = 1; i <= lim1_; i += 1) {
        yr[(i)-1] = zeror;
        yi[(i)-1] = zeroi;
        ;
    }
    return;
L120:;
    s1r = cyr[(1) - 1];
    s1i = cyi[(1) - 1];
    s2r = cyr[(2) - 1];
    s2i = cyi[(2) - 1];
    c1r = csrr[(kflag)-1];
    ascle = bry[(kflag)-1];
    lim2_ = n;
    for (i = ib; i <= lim2_; i += 1) {
        c2r = s2r;
        c2i = s2i;
        s2r = ckr * c2r - cki * c2i + s1r;
        s2i = ckr * c2i + cki * c2r + s1i;
        s1r = c2r;
        s1i = c2i;
        ckr = ckr + rzr;
        cki = cki + rzi;
        c2r = s2r * c1r;
        c2i = s2i * c1r;
        yr[(i)-1] = c2r;
        yi[(i)-1] = c2i;
        if (kflag >= 3) {
            goto L130;
        }
        str = std::fabs(c2r);
        sti = std::fabs(c2i);
        c2m = fmax_(str, sti);
        if (c2m <= ascle) {
            goto L130;
        }
        kflag = kflag + 1;
        ascle = bry[(kflag)-1];
        s1r = s1r * c1r;
        s1i = s1i * c1r;
        s2r = c2r;
        s2i = c2i;
        s1r = s1r * cssr[(kflag)-1];
        s1i = s1i * cssr[(kflag)-1];
        s2r = s2r * cssr[(kflag)-1];
        s2i = s2i * cssr[(kflag)-1];
        c1r = csrr[(kflag)-1];
    L130:;
    }
L180:;
    if (mr == 0) {
        return;
    }
    nz = 0;
    fmr = (double)(mr);
    sgn = -std::copysign(pi, fmr);
    csgni = sgn;
    if (yy <= 0.0e0) {
        csgni = -csgni;
    }
    ifn = inu + n - 1;
    ang = fnf * sgn;
    cspnr = std::cos(ang);
    cspni = std::sin(ang);
    if (imod_(ifn, 2) == 0) {
        goto L190;
    }
    cspnr = -cspnr;
    cspni = -cspni;
L190:;
    csr = sar * csgni;
    csi = car * csgni;
    in = imod_(ifn, 4) + 1;
    c2r = cipr[(in)-1];
    c2i = cipi[(in)-1];
    str = csr * c2r + csi * c2i;
    csi = -csr * c2i + csi * c2r;
    csr = str;
    asc = bry[(1) - 1];
    iuf = 0;
    kk = n;
    kdflg = 1;
    ib = ib - 1;
    ic = ib - 1;
    lim3_ = n;
    for (k = 1; k <= lim3_; k += 1) {
        fn = fnu + (double)(kk - 1);
        if (n > 2) {
            goto L175;
        }
    L172:;
        phidr = phir[(j)-1];
        phidi = phii[(j)-1];
        argdr = argr[(j)-1];
        argdi = argi[(j)-1];
        zet1dr = zeta1r[(j)-1];
        zet1di = zeta1i[(j)-1];
        zet2dr = zeta2r[(j)-1];
        zet2di = zeta2i[(j)-1];
        asumdr = asumr[(j)-1];
        asumdi = asumi[(j)-1];
        bsumdr = bsumr[(j)-1];
        bsumdi = bsumi[(j)-1];
        j = 3 - j;
        goto L210;
    L175:;
        if ((kk == n) && (ib < n)) {
            goto L210;
        }
        if ((kk == ib) || (kk == ic)) {
            goto L172;
        }
        zunhj(znr, zni, fn, 0, tol, phidr, phidi, argdr, argdi, zet1dr, zet1di, zet2dr, zet2di,
            asumdr, asumdi, bsumdr, bsumdi);
    L210:;
        if (kode == 1) {
            goto L220;
        }
        str = zbr + zet2dr;
        sti = zbi + zet2di;
        rast = fn / zabs(str, sti);
        str = str * rast * rast;
        sti = -sti * rast * rast;
        s1r = -zet1dr + str;
        s1i = -zet1di + sti;
        goto L230;
    L220:;
        s1r = -zet1dr + zet2dr;
        s1i = -zet1di + zet2di;
    L230:;
        rs1 = s1r;
        if (std::fabs(rs1) > elim) {
            goto L280;
        }
        if (kdflg == 1) {
            iflag = 2;
        }
        if (std::fabs(rs1) < alim) {
            goto L240;
        }
        aphi = zabs(phidr, phidi);
        aarg = zabs(argdr, argdi);
        rs1 = rs1 + std::log(aphi) - 0.25e0 * std::log(aarg) - aic;
        if (std::fabs(rs1) > elim) {
            goto L280;
        }
        if (kdflg == 1) {
            iflag = 1;
        }
        if (rs1 < 0.0e0) {
            goto L240;
        }
        if (kdflg == 1) {
            iflag = 3;
        }
    L240:;
        zairy(argdr, argdi, 0, 2, air, aii, nai, idum);
        zairy(argdr, argdi, 1, 2, dair, daii, ndai, idum);
        str = dair * bsumdr - daii * bsumdi;
        sti = dair * bsumdi + daii * bsumdr;
        str = str + (air * asumdr - aii * asumdi);
        sti = sti + (air * asumdi + aii * asumdr);
        ptr = str * phidr - sti * phidi;
        pti = str * phidi + sti * phidr;
        s2r = ptr * csr - pti * csi;
        s2i = ptr * csi + pti * csr;
        str = std::exp(s1r) * cssr[(iflag)-1];
        s1r = str * std::cos(s1i);
        s1i = str * std::sin(s1i);
        str = s2r * s1r - s2i * s1i;
        s2i = s2r * s1i + s2i * s1r;
        s2r = str;
        if (iflag != 1) {
            goto L250;
        }
        zuchk(s2r, s2i, nw, bry[(1) - 1], tol);
        if (nw == 0) {
            goto L250;
        }
        s2r = zeror;
        s2i = zeroi;
    L250:;
        if (yy <= 0.0e0) {
            s2i = -s2i;
        }
        cyr[(kdflg)-1] = s2r;
        cyi[(kdflg)-1] = s2i;
        c2r = s2r;
        c2i = s2i;
        s2r = s2r * csrr[(iflag)-1];
        s2i = s2i * csrr[(iflag)-1];
        s1r = yr[(kk)-1];
        s1i = yi[(kk)-1];
        if (kode == 1) {
            goto L270;
        }
        zs1s2(zrr, zri, s1r, s1i, s2r, s2i, nw, asc, alim, iuf);
        nz = nz + nw;
    L270:;
        yr[(kk)-1] = s1r * cspnr - s1i * cspni + s2r;
        yi[(kk)-1] = s1r * cspni + s1i * cspnr + s2i;
        kk = kk - 1;
        cspnr = -cspnr;
        cspni = -cspni;
        str = csi;
        csi = -csr;
        csr = str;
        if (c2r != 0.0e0 || c2i != 0.0e0) {
            goto L255;
        }
        kdflg = 1;
        goto L290;
    L255:;
        if (kdflg == 2) {
            goto L295;
        }
        kdflg = 2;
        goto L290;
    L280:;
        if (rs1 > 0.0e0) {
            goto L320;
        }
        s2r = zeror;
        s2i = zeroi;
        goto L250;
    L290:;
    }
    k = n;
L295:;
    il = n - k;
    if (il == 0) {
        return;
    }
    s1r = cyr[(1) - 1];
    s1i = cyi[(1) - 1];
    s2r = cyr[(2) - 1];
    s2i = cyi[(2) - 1];
    csr = csrr[(iflag)-1];
    ascle = bry[(iflag)-1];
    fn = (double)(inu + il);
    lim4_ = il;
    for (i = 1; i <= lim4_; i += 1) {
        c2r = s2r;
        c2i = s2i;
        s2r = s1r + (fn + fnf) * (rzr * c2r - rzi * c2i);
        s2i = s1i + (fn + fnf) * (rzr * c2i + rzi * c2r);
        s1r = c2r;
        s1i = c2i;
        fn = fn - 1.0e0;
        c2r = s2r * csr;
        c2i = s2i * csr;
        ckr = c2r;
        cki = c2i;
        c1r = yr[(kk)-1];
        c1i = yi[(kk)-1];
        if (kode == 1) {
            goto L300;
        }
        zs1s2(zrr, zri, c1r, c1i, c2r, c2i, nw, asc, alim, iuf);
        nz = nz + nw;
    L300:;
        yr[(kk)-1] = c1r * cspnr - c1i * cspni + c2r;
        yi[(kk)-1] = c1r * cspni + c1i * cspnr + c2i;
        kk = kk - 1;
        cspnr = -cspnr;
        cspni = -cspni;
        if (iflag >= 3) {
            goto L310;
        }
        c2r = std::fabs(ckr);
        c2i = std::fabs(cki);
        c2m = fmax_(c2r, c2i);
        if (c2m <= ascle) {
            goto L310;
        }
        iflag = iflag + 1;
        ascle = bry[(iflag)-1];
        s1r = s1r * csr;
        s1i = s1i * csr;
        s2r = ckr;
        s2i = cki;
        s1r = s1r * cssr[(iflag)-1];
        s1i = s1i * cssr[(iflag)-1];
        s2r = s2r * cssr[(iflag)-1];
        s2i = s2i * cssr[(iflag)-1];
        csr = csrr[(iflag)-1];
    L310:;
    }
    return;
L320:;
    nz = -1;
    return;
}
//=============================================================================
static void
zbunk(double zr, double zi, double fnu, int kode, int mr, int n, double* yr, double* yi, int& nz,
    double tol, double elim, double alim)
{
    double ax = 0;
    double ay = 0;
    nz = 0;
    ax = std::fabs(zr) * 1.7321e0;
    ay = std::fabs(zi);
    if (ay > ax) {
        goto L10;
    }
    zunk1(zr, zi, fnu, kode, mr, n, yr, yi, nz, tol, elim, alim);
    goto L20;
L10:;
    zunk2(zr, zi, fnu, kode, mr, n, yr, yi, nz, tol, elim, alim);
L20:;
    return;
}
//=============================================================================
static void
zacai(double zr, double zi, double fnu, int kode, int mr, int n, double* yr, double* yi, int& nz,
    double rl, double tol, double elim, double alim)
{
    const double pi = 3.14159265358979324e0;
    double arg = 0;
    double ascle = 0;
    double az = 0;
    double c1i = 0;
    double c1r = 0;
    double c2i = 0;
    double c2r = 0;
    double csgni = 0;
    double csgnr = 0;
    double cspni = 0;
    double cspnr = 0;
    double cyi[2] = {};
    double cyr[2] = {};
    double dfnu = 0;
    double fmr = 0;
    int inu = 0;
    int iuf = 0;
    int nn = 0;
    int nw = 0;
    double sgn = 0;
    double yy = 0;
    double zni = 0;
    double znr = 0;
    nz = 0;
    znr = -zr;
    zni = -zi;
    az = zabs(zr, zi);
    nn = n;
    dfnu = fnu + (double)(n - 1);
    if (az <= 2.0e0) {
        goto L10;
    }
    if (az * az * 0.25e0 > dfnu + 1.0e0) {
        goto L20;
    }
L10:;
    zseri(znr, zni, fnu, kode, nn, yr, yi, nw, tol, elim, alim);
    goto L40;
L20:;
    if (az < rl) {
        goto L30;
    }
    zasyi(znr, zni, fnu, kode, nn, yr, yi, nw, rl, tol, elim, alim);
    if (nw < 0) {
        goto L80;
    }
    goto L40;
L30:;
    zmlri(znr, zni, fnu, kode, nn, yr, yi, nw, tol);
    if (nw < 0) {
        goto L80;
    }
L40:;
    zbknu(znr, zni, fnu, kode, 1, cyr, cyi, nw, tol, elim, alim);
    if (nw != 0) {
        goto L80;
    }
    fmr = (double)(mr);
    sgn = -std::copysign(pi, fmr);
    csgnr = 0.0e0;
    csgni = sgn;
    if (kode == 1) {
        goto L50;
    }
    yy = -zni;
    csgnr = -csgni * std::sin(yy);
    csgni = csgni * std::cos(yy);
L50:;
    inu = fortranInt(fnu);
    arg = (fnu - (double)(inu)) * sgn;
    cspnr = std::cos(arg);
    cspni = std::sin(arg);
    if (imod_(inu, 2) == 0) {
        goto L60;
    }
    cspnr = -cspnr;
    cspni = -cspni;
L60:;
    c1r = cyr[(1) - 1];
    c1i = cyi[(1) - 1];
    c2r = yr[(1) - 1];
    c2i = yi[(1) - 1];
    if (kode == 1) {
        goto L70;
    }
    iuf = 0;
    ascle = 1.0e+3 * d1mach(1) / tol;
    zs1s2(znr, zni, c1r, c1i, c2r, c2i, nw, ascle, alim, iuf);
    nz = nz + nw;
L70:;
    yr[(1) - 1] = cspnr * c1r - cspni * c1i + csgnr * c2r - csgni * c2i;
    yi[(1) - 1] = cspnr * c1i + cspni * c1r + csgnr * c2i + csgni * c2r;
    return;
L80:;
    nz = -1;
    if (nw == (-2)) {
        nz = -2;
    }
    return;
}
//=============================================================================
static void
zacon(double zr, double zi, double fnu, int kode, int mr, int n, double* yr, double* yi, int& nz,
    double rl, double fnul, double tol, double elim, double alim)
{
    const double oner = 1.0e0;
    const double pi = 3.14159265358979324e0;
    const double zeror = 0.0e0;
    double arg = 0;
    double as2 = 0;
    double ascle = 0;
    double azn = 0;
    double bry[3] = {};
    double bscle = 0;
    double c1i = 0;
    double c1m = 0;
    double c1r = 0;
    double c2i = 0;
    double c2r = 0;
    double cki = 0;
    double ckr = 0;
    double cpn = 0;
    double cscl = 0;
    double cscr = 0;
    double csgni = 0;
    double csgnr = 0;
    double cspni = 0;
    double cspnr = 0;
    double csr = 0;
    double csrr[3] = {};
    double cssr[3] = {};
    double cyi[2] = {};
    double cyr[2] = {};
    double fmr = 0;
    double fn = 0;
    int i = 0;
    int inu = 0;
    int iuf = 0;
    int kflag = 0;
    int nn = 0;
    int nw = 0;
    double pti = 0;
    double ptr = 0;
    double razn = 0;
    double rzi = 0;
    double rzr = 0;
    double s1i = 0;
    double s1r = 0;
    double s2i = 0;
    double s2r = 0;
    double sc1i = 0;
    double sc1r = 0;
    double sc2i = 0;
    double sc2r = 0;
    double sgn = 0;
    double spn = 0;
    double sti = 0;
    double str = 0;
    double yy = 0;
    double zni = 0;
    double znr = 0;
    int lim0_ = 0;
    nz = 0;
    znr = -zr;
    zni = -zi;
    nn = n;
    zbinu(znr, zni, fnu, kode, nn, yr, yi, nw, rl, fnul, tol, elim, alim);
    if (nw < 0) {
        goto L90;
    }
    nn = imin_(2, n);
    zbknu(znr, zni, fnu, kode, nn, cyr, cyi, nw, tol, elim, alim);
    if (nw != 0) {
        goto L90;
    }
    s1r = cyr[(1) - 1];
    s1i = cyi[(1) - 1];
    fmr = (double)(mr);
    sgn = -std::copysign(pi, fmr);
    csgnr = zeror;
    csgni = sgn;
    if (kode == 1) {
        goto L10;
    }
    yy = -zni;
    cpn = std::cos(yy);
    spn = std::sin(yy);
    zmlt(csgnr, csgni, cpn, spn, csgnr, csgni);
L10:;
    inu = fortranInt(fnu);
    arg = (fnu - (double)(inu)) * sgn;
    cpn = std::cos(arg);
    spn = std::sin(arg);
    cspnr = cpn;
    cspni = spn;
    if (imod_(inu, 2) == 0) {
        goto L20;
    }
    cspnr = -cspnr;
    cspni = -cspni;
L20:;
    iuf = 0;
    c1r = s1r;
    c1i = s1i;
    c2r = yr[(1) - 1];
    c2i = yi[(1) - 1];
    ascle = 1.0e+3 * d1mach(1) / tol;
    if (kode == 1) {
        goto L30;
    }
    zs1s2(znr, zni, c1r, c1i, c2r, c2i, nw, ascle, alim, iuf);
    nz = nz + nw;
    sc1r = c1r;
    sc1i = c1i;
L30:;
    zmlt(cspnr, cspni, c1r, c1i, str, sti);
    zmlt(csgnr, csgni, c2r, c2i, ptr, pti);
    yr[(1) - 1] = str + ptr;
    yi[(1) - 1] = sti + pti;
    if (n == 1) {
        return;
    }
    cspnr = -cspnr;
    cspni = -cspni;
    s2r = cyr[(2) - 1];
    s2i = cyi[(2) - 1];
    c1r = s2r;
    c1i = s2i;
    c2r = yr[(2) - 1];
    c2i = yi[(2) - 1];
    if (kode == 1) {
        goto L40;
    }
    zs1s2(znr, zni, c1r, c1i, c2r, c2i, nw, ascle, alim, iuf);
    nz = nz + nw;
    sc2r = c1r;
    sc2i = c1i;
L40:;
    zmlt(cspnr, cspni, c1r, c1i, str, sti);
    zmlt(csgnr, csgni, c2r, c2i, ptr, pti);
    yr[(2) - 1] = str + ptr;
    yi[(2) - 1] = sti + pti;
    if (n == 2) {
        return;
    }
    cspnr = -cspnr;
    cspni = -cspni;
    azn = zabs(znr, zni);
    razn = 1.0e0 / azn;
    str = znr * razn;
    sti = -zni * razn;
    rzr = (str + str) * razn;
    rzi = (sti + sti) * razn;
    fn = fnu + 1.0e0;
    ckr = fn * rzr;
    cki = fn * rzi;
    cscl = 1.0e0 / tol;
    cscr = tol;
    cssr[(1) - 1] = cscl;
    cssr[(2) - 1] = oner;
    cssr[(3) - 1] = cscr;
    csrr[(1) - 1] = cscr;
    csrr[(2) - 1] = oner;
    csrr[(3) - 1] = cscl;
    bry[(1) - 1] = ascle;
    bry[(2) - 1] = 1.0e0 / ascle;
    bry[(3) - 1] = d1mach(2);
    as2 = zabs(s2r, s2i);
    kflag = 2;
    if (as2 > bry[(1) - 1]) {
        goto L50;
    }
    kflag = 1;
    goto L60;
L50:;
    if (as2 < bry[(2) - 1]) {
        goto L60;
    }
    kflag = 3;
L60:;
    bscle = bry[(kflag)-1];
    s1r = s1r * cssr[(kflag)-1];
    s1i = s1i * cssr[(kflag)-1];
    s2r = s2r * cssr[(kflag)-1];
    s2i = s2i * cssr[(kflag)-1];
    csr = csrr[(kflag)-1];
    lim0_ = n;
    for (i = 3; i <= lim0_; i += 1) {
        str = s2r;
        sti = s2i;
        s2r = ckr * str - cki * sti + s1r;
        s2i = ckr * sti + cki * str + s1i;
        s1r = str;
        s1i = sti;
        c1r = s2r * csr;
        c1i = s2i * csr;
        str = c1r;
        sti = c1i;
        c2r = yr[(i)-1];
        c2i = yi[(i)-1];
        if (kode == 1) {
            goto L70;
        }
        if (iuf < 0) {
            goto L70;
        }
        zs1s2(znr, zni, c1r, c1i, c2r, c2i, nw, ascle, alim, iuf);
        nz = nz + nw;
        sc1r = sc2r;
        sc1i = sc2i;
        sc2r = c1r;
        sc2i = c1i;
        if (iuf != 3) {
            goto L70;
        }
        iuf = -4;
        s1r = sc1r * cssr[(kflag)-1];
        s1i = sc1i * cssr[(kflag)-1];
        s2r = sc2r * cssr[(kflag)-1];
        s2i = sc2i * cssr[(kflag)-1];
        str = sc2r;
        sti = sc2i;
    L70:;
        ptr = cspnr * c1r - cspni * c1i;
        pti = cspnr * c1i + cspni * c1r;
        yr[(i)-1] = ptr + csgnr * c2r - csgni * c2i;
        yi[(i)-1] = pti + csgnr * c2i + csgni * c2r;
        ckr = ckr + rzr;
        cki = cki + rzi;
        cspnr = -cspnr;
        cspni = -cspni;
        if (kflag >= 3) {
            goto L80;
        }
        ptr = std::fabs(c1r);
        pti = std::fabs(c1i);
        c1m = fmax_(ptr, pti);
        if (c1m <= bscle) {
            goto L80;
        }
        kflag = kflag + 1;
        bscle = bry[(kflag)-1];
        s1r = s1r * csr;
        s1i = s1i * csr;
        s2r = str;
        s2i = sti;
        s1r = s1r * cssr[(kflag)-1];
        s1i = s1i * cssr[(kflag)-1];
        s2r = s2r * cssr[(kflag)-1];
        s2i = s2i * cssr[(kflag)-1];
        csr = csrr[(kflag)-1];
    L80:;
    }
    return;
L90:;
    nz = -1;
    if (nw == (-2)) {
        nz = -2;
    }
    return;
}
//=============================================================================
static void
zbinu(double zr, double zi, double fnu, int kode, int n, double* cyr, double* cyi, int& nz,
    double rl, double fnul, double tol, double elim, double alim)
{
    const double zeroi = 0.0e0;
    const double zeror = 0.0e0;
    double az = 0;
    double cwi[2] = {};
    double cwr[2] = {};
    double dfnu = 0;
    int i = 0;
    int inw = 0;
    int nlast = 0;
    int nn = 0;
    int nui = 0;
    int nw = 0;
    int lim0_ = 0;
    nz = 0;
    az = zabs(zr, zi);
    nn = n;
    dfnu = fnu + (double)(n - 1);
    if (az <= 2.0e0) {
        goto L10;
    }
    if (az * az * 0.25e0 > dfnu + 1.0e0) {
        goto L20;
    }
L10:;
    zseri(zr, zi, fnu, kode, nn, cyr, cyi, nw, tol, elim, alim);
    inw = std::abs(nw);
    nz = nz + inw;
    nn = nn - inw;
    if (nn == 0) {
        return;
    }
    if (nw >= 0) {
        goto L120;
    }
    dfnu = fnu + (double)(nn - 1);
L20:;
    if (az < rl) {
        goto L40;
    }
    if (dfnu <= 1.0e0) {
        goto L30;
    }
    if (az + az < dfnu * dfnu) {
        goto L50;
    }
L30:;
    zasyi(zr, zi, fnu, kode, nn, cyr, cyi, nw, rl, tol, elim, alim);
    if (nw < 0) {
        goto L130;
    }
    goto L120;
L40:;
    if (dfnu <= 1.0e0) {
        goto L70;
    }
L50:;
    zuoik(zr, zi, fnu, kode, 1, nn, cyr, cyi, nw, tol, elim, alim);
    if (nw < 0) {
        goto L130;
    }
    nz = nz + nw;
    nn = nn - nw;
    if (nn == 0) {
        return;
    }
    dfnu = fnu + (double)(nn - 1);
    if (dfnu > fnul) {
        goto L110;
    }
    if (az > fnul) {
        goto L110;
    }
L60:;
    if (az > rl) {
        goto L80;
    }
L70:;
    zmlri(zr, zi, fnu, kode, nn, cyr, cyi, nw, tol);
    if (nw < 0) {
        goto L130;
    }
    goto L120;
L80:;
    zuoik(zr, zi, fnu, kode, 2, 2, cwr, cwi, nw, tol, elim, alim);
    if (nw >= 0) {
        goto L100;
    }
    nz = nn;
    lim0_ = nn;
    for (i = 1; i <= lim0_; i += 1) {
        cyr[(i)-1] = zeror;
        cyi[(i)-1] = zeroi;
        ;
    }
    return;
L100:;
    if (nw > 0) {
        goto L130;
    }
    zwrsk(zr, zi, fnu, kode, nn, cyr, cyi, nw, cwr, cwi, tol, elim, alim);
    if (nw < 0) {
        goto L130;
    }
    goto L120;
L110:;
    nui = fortranInt(fnul - dfnu) + 1;
    nui = imax_(nui, 0);
    zbuni(zr, zi, fnu, kode, nn, cyr, cyi, nw, nui, nlast, fnul, tol, elim, alim);
    if (nw < 0) {
        goto L130;
    }
    nz = nz + nw;
    if (nlast == 0) {
        goto L120;
    }
    nn = nlast;
    goto L60;
L120:;
    return;
L130:;
    nz = -1;
    if (nw == (-2)) {
        nz = -2;
    }
    return;
}
//=============================================================================
void
zbesh(double zr, double zi, double fnu, int kode, int m, int n, double* cyr, double* cyi, int& nz,
    int& ierr)
{
    const double hpi = 1.57079632679489662e0;
    double aa = 0;
    double alim = 0;
    double aln = 0;
    double arg = 0;
    double ascle = 0;
    double atol = 0;
    double az = 0;
    double bb = 0;
    double csgni = 0;
    double csgnr = 0;
    double dig = 0;
    double elim = 0;
    double fmm = 0;
    double fn = 0;
    double fnul = 0;
    int i = 0;
    int inu = 0;
    int inuh = 0;
    int ir = 0;
    int k = 0;
    int k1 = 0;
    int k2 = 0;
    int mm = 0;
    int mr = 0;
    int nn = 0;
    int nuf = 0;
    int nw = 0;
    double r1m5 = 0;
    double rhpi = 0;
    double rl = 0;
    double rtol = 0;
    double sgn = 0;
    double sti = 0;
    double str = 0;
    double tol = 0;
    double ufl = 0;
    double zni = 0;
    double znr = 0;
    double zti = 0;
    int lim0_ = 0;
    ierr = 0;
    nz = 0;
    if (zr == 0.0e0 && zi == 0.0e0) {
        ierr = 1;
    }
    if (fnu < 0.0e0) {
        ierr = 1;
    }
    if (m < 1 || m > 2) {
        ierr = 1;
    }
    if (kode < 1 || kode > 2) {
        ierr = 1;
    }
    if (n < 1) {
        ierr = 1;
    }
    if (ierr != 0) {
        return;
    }
    nn = n;
    tol = fmax_(d1mach(4), 1.0e-18);
    k1 = i1mach(15);
    k2 = i1mach(16);
    r1m5 = d1mach(5);
    k = imin_(std::abs(k1), std::abs(k2));
    elim = 2.303e0 * ((double)(k)*r1m5 - 3.0e0);
    k1 = i1mach(14) - 1;
    aa = r1m5 * (double)(k1);
    dig = fmin_(aa, 18.0e0);
    aa = aa * 2.303e0;
    alim = elim + fmax_(-aa, -41.45e0);
    fnul = 10.0e0 + 6.0e0 * (dig - 3.0e0);
    rl = 1.2e0 * dig + 3.0e0;
    fn = fnu + (double)(nn - 1);
    mm = 3 - m - m;
    fmm = (double)(mm);
    znr = fmm * zi;
    zni = -fmm * zr;
    az = zabs(zr, zi);
    aa = 0.5e0 / tol;
    bb = (double)(i1mach(9)) * 0.5e0;
    aa = fmin_(aa, bb);
    aa = std::sqrt(aa);
    if (az > aa) {
        ierr = 3;
    }
    if (fn > aa) {
        ierr = 3;
    }
    ufl = d1mach(1) * 1.0e+3;
    if (az < ufl) {
        goto L230;
    }
    if (fnu > fnul) {
        goto L90;
    }
    if (fn <= 1.0e0) {
        goto L70;
    }
    if (fn > 2.0e0) {
        goto L60;
    }
    if (az > tol) {
        goto L70;
    }
    arg = 0.5e0 * az;
    aln = -fn * std::log(arg);
    if (aln > elim) {
        goto L230;
    }
    goto L70;
L60:;
    zuoik(znr, zni, fnu, kode, 2, nn, cyr, cyi, nuf, tol, elim, alim);
    if (nuf < 0) {
        goto L230;
    }
    nz = nz + nuf;
    nn = nn - nuf;
    if (nn == 0) {
        goto L140;
    }
L70:;
    if ((znr < 0.0e0) || (znr == 0.0e0 && zni < 0.0e0 && m == 2)) {
        goto L80;
    }
    zbknu(znr, zni, fnu, kode, nn, cyr, cyi, nz, tol, elim, alim);
    goto L110;
L80:;
    mr = -mm;
    zacon(znr, zni, fnu, kode, mr, nn, cyr, cyi, nw, rl, fnul, tol, elim, alim);
    if (nw < 0) {
        goto L240;
    }
    nz = nw;
    goto L110;
L90:;
    mr = 0;
    if ((znr >= 0.0e0) && (znr != 0.0e0 || zni >= 0.0e0 || m != 2)) {
        goto L100;
    }
    mr = -mm;
    if (znr != 0.0e0 || zni >= 0.0e0) {
        goto L100;
    }
    znr = -znr;
    zni = -zni;
L100:;
    zbunk(znr, zni, fnu, kode, mr, nn, cyr, cyi, nw, tol, elim, alim);
    if (nw < 0) {
        goto L240;
    }
    nz = nz + nw;
L110:;
    sgn = std::copysign(hpi, -fmm);
    inu = fortranInt(fnu);
    inuh = inu / 2;
    ir = inu - 2 * inuh;
    arg = (fnu - (double)(inu - ir)) * sgn;
    rhpi = 1.0e0 / sgn;
    csgni = rhpi * std::cos(arg);
    csgnr = -rhpi * std::sin(arg);
    if (imod_(inuh, 2) == 0) {
        goto L120;
    }
    csgnr = -csgnr;
    csgni = -csgni;
L120:;
    zti = -fmm;
    rtol = 1.0e0 / tol;
    ascle = ufl * rtol;
    lim0_ = nn;
    for (i = 1; i <= lim0_; i += 1) {
        aa = cyr[(i)-1];
        bb = cyi[(i)-1];
        atol = 1.0e0;
        if (fmax_(std::fabs(aa), std::fabs(bb)) > ascle) {
            goto L135;
        }
        aa = aa * rtol;
        bb = bb * rtol;
        atol = tol;
    L135:;
        str = aa * csgnr - bb * csgni;
        sti = aa * csgni + bb * csgnr;
        cyr[(i)-1] = str * atol;
        cyi[(i)-1] = sti * atol;
        str = -csgni * zti;
        csgni = csgnr * zti;
        csgnr = str;
        ;
    }
    return;
L140:;
    if (znr < 0.0e0) {
        goto L230;
    }
    return;
L230:;
    nz = 0;
    ierr = 2;
    return;
L240:;
    if (nw == (-1)) {
        goto L230;
    }
    nz = 0;
    ierr = 5;
    return;
    nz = 0;
    ierr = 4;
    return;
}
//=============================================================================
void
zbesi(
    double zr, double zi, double fnu, int kode, int n, double* cyr, double* cyi, int& nz, int& ierr)
{
    const double onei = 0.0e0;
    const double oner = 1.0e0;
    const double pi = 3.14159265358979324e0;
    double aa = 0;
    double alim = 0;
    double arg = 0;
    double ascle = 0;
    double atol = 0;
    double az = 0;
    double bb = 0;
    double csgni = 0;
    double csgnr = 0;
    double dig = 0;
    double elim = 0;
    double fn = 0;
    double fnul = 0;
    int i = 0;
    int inu = 0;
    int k = 0;
    int k1 = 0;
    int k2 = 0;
    int nn = 0;
    double r1m5 = 0;
    double rl = 0;
    double rtol = 0;
    double sti = 0;
    double str = 0;
    double tol = 0;
    double zni = 0;
    double znr = 0;
    int lim0_ = 0;
    ierr = 0;
    nz = 0;
    if (fnu < 0.0e0) {
        ierr = 1;
    }
    if (kode < 1 || kode > 2) {
        ierr = 1;
    }
    if (n < 1) {
        ierr = 1;
    }
    if (ierr != 0) {
        return;
    }
    tol = fmax_(d1mach(4), 1.0e-18);
    k1 = i1mach(15);
    k2 = i1mach(16);
    r1m5 = d1mach(5);
    k = imin_(std::abs(k1), std::abs(k2));
    elim = 2.303e0 * ((double)(k)*r1m5 - 3.0e0);
    k1 = i1mach(14) - 1;
    aa = r1m5 * (double)(k1);
    dig = fmin_(aa, 18.0e0);
    aa = aa * 2.303e0;
    alim = elim + fmax_(-aa, -41.45e0);
    rl = 1.2e0 * dig + 3.0e0;
    fnul = 10.0e0 + 6.0e0 * (dig - 3.0e0);
    az = zabs(zr, zi);
    fn = fnu + (double)(n - 1);
    aa = 0.5e0 / tol;
    bb = (double)(i1mach(9)) * 0.5e0;
    aa = fmin_(aa, bb);
    aa = std::sqrt(aa);
    if (az > aa) {
        ierr = 3;
    }
    if (fn > aa) {
        ierr = 3;
    }
    znr = zr;
    zni = zi;
    csgnr = oner;
    csgni = onei;
    if (zr >= 0.0e0) {
        goto L40;
    }
    znr = -zr;
    zni = -zi;
    inu = fortranInt(fnu);
    arg = (fnu - (double)(inu)) * pi;
    if (zi < 0.0e0) {
        arg = -arg;
    }
    csgnr = std::cos(arg);
    csgni = std::sin(arg);
    if (imod_(inu, 2) == 0) {
        goto L40;
    }
    csgnr = -csgnr;
    csgni = -csgni;
L40:;
    zbinu(znr, zni, fnu, kode, n, cyr, cyi, nz, rl, fnul, tol, elim, alim);
    if (nz < 0) {
        goto L120;
    }
    if (zr >= 0.0e0) {
        return;
    }
    nn = n - nz;
    if (nn == 0) {
        return;
    }
    rtol = 1.0e0 / tol;
    ascle = d1mach(1) * rtol * 1.0e+3;
    lim0_ = nn;
    for (i = 1; i <= lim0_; i += 1) {
        aa = cyr[(i)-1];
        bb = cyi[(i)-1];
        atol = 1.0e0;
        if (fmax_(std::fabs(aa), std::fabs(bb)) > ascle) {
            goto L55;
        }
        aa = aa * rtol;
        bb = bb * rtol;
        atol = tol;
    L55:;
        str = aa * csgnr - bb * csgni;
        sti = aa * csgni + bb * csgnr;
        cyr[(i)-1] = str * atol;
        cyi[(i)-1] = sti * atol;
        csgnr = -csgnr;
        csgni = -csgni;
        ;
    }
    return;
L120:;
    if (nz == (-2)) {
        goto L130;
    }
    nz = 0;
    ierr = 2;
    return;
L130:;
    nz = 0;
    ierr = 5;
    return;
    nz = 0;
    ierr = 4;
    return;
}
//=============================================================================
void
zbesj(
    double zr, double zi, double fnu, int kode, int n, double* cyr, double* cyi, int& nz, int& ierr)
{
    const double hpi = 1.57079632679489662e0;
    double aa = 0;
    double alim = 0;
    double arg = 0;
    double ascle = 0;
    double atol = 0;
    double az = 0;
    double bb = 0;
    double cii = 0;
    double csgni = 0;
    double csgnr = 0;
    double dig = 0;
    double elim = 0;
    double fn = 0;
    double fnul = 0;
    int i = 0;
    int inu = 0;
    int inuh = 0;
    int ir = 0;
    int k = 0;
    int k1 = 0;
    int k2 = 0;
    int nl = 0;
    double r1m5 = 0;
    double rl = 0;
    double rtol = 0;
    double sti = 0;
    double str = 0;
    double tol = 0;
    double zni = 0;
    double znr = 0;
    int lim0_ = 0;
    ierr = 0;
    nz = 0;
    if (fnu < 0.0e0) {
        ierr = 1;
    }
    if (kode < 1 || kode > 2) {
        ierr = 1;
    }
    if (n < 1) {
        ierr = 1;
    }
    if (ierr != 0) {
        return;
    }
    tol = fmax_(d1mach(4), 1.0e-18);
    k1 = i1mach(15);
    k2 = i1mach(16);
    r1m5 = d1mach(5);
    k = imin_(std::abs(k1), std::abs(k2));
    elim = 2.303e0 * ((double)(k)*r1m5 - 3.0e0);
    k1 = i1mach(14) - 1;
    aa = r1m5 * (double)(k1);
    dig = fmin_(aa, 18.0e0);
    aa = aa * 2.303e0;
    alim = elim + fmax_(-aa, -41.45e0);
    rl = 1.2e0 * dig + 3.0e0;
    fnul = 10.0e0 + 6.0e0 * (dig - 3.0e0);
    az = zabs(zr, zi);
    fn = fnu + (double)(n - 1);
    aa = 0.5e0 / tol;
    bb = (double)(i1mach(9)) * 0.5e0;
    aa = fmin_(aa, bb);
    aa = std::sqrt(aa);
    if (az > aa) {
        ierr = 3;
    }
    if (fn > aa) {
        ierr = 3;
    }
    cii = 1.0e0;
    inu = fortranInt(fnu);
    inuh = inu / 2;
    ir = inu - 2 * inuh;
    arg = (fnu - (double)(inu - ir)) * hpi;
    csgnr = std::cos(arg);
    csgni = std::sin(arg);
    if (imod_(inuh, 2) == 0) {
        goto L40;
    }
    csgnr = -csgnr;
    csgni = -csgni;
L40:;
    znr = zi;
    zni = -zr;
    if (zi >= 0.0e0) {
        goto L50;
    }
    znr = -znr;
    zni = -zni;
    csgni = -csgni;
    cii = -cii;
L50:;
    zbinu(znr, zni, fnu, kode, n, cyr, cyi, nz, rl, fnul, tol, elim, alim);
    if (nz < 0) {
        goto L130;
    }
    nl = n - nz;
    if (nl == 0) {
        return;
    }
    rtol = 1.0e0 / tol;
    ascle = d1mach(1) * rtol * 1.0e+3;
    lim0_ = nl;
    for (i = 1; i <= lim0_; i += 1) {
        aa = cyr[(i)-1];
        bb = cyi[(i)-1];
        atol = 1.0e0;
        if (fmax_(std::fabs(aa), std::fabs(bb)) > ascle) {
            goto L55;
        }
        aa = aa * rtol;
        bb = bb * rtol;
        atol = tol;
    L55:;
        str = aa * csgnr - bb * csgni;
        sti = aa * csgni + bb * csgnr;
        cyr[(i)-1] = str * atol;
        cyi[(i)-1] = sti * atol;
        str = -csgni * cii;
        csgni = csgnr * cii;
        csgnr = str;
        ;
    }
    return;
L130:;
    if (nz == (-2)) {
        goto L140;
    }
    nz = 0;
    ierr = 2;
    return;
L140:;
    nz = 0;
    ierr = 5;
    return;
    nz = 0;
    ierr = 4;
    return;
}
//=============================================================================
void
zbesk(
    double zr, double zi, double fnu, int kode, int n, double* cyr, double* cyi, int& nz, int& ierr)
{
    double aa = 0;
    double alim = 0;
    double aln = 0;
    double arg = 0;
    double az = 0;
    double bb = 0;
    double dig = 0;
    double elim = 0;
    double fn = 0;
    double fnul = 0;
    int k = 0;
    int k1 = 0;
    int k2 = 0;
    int mr = 0;
    int nn = 0;
    int nuf = 0;
    int nw = 0;
    double r1m5 = 0;
    double rl = 0;
    double tol = 0;
    double ufl = 0;
    ierr = 0;
    nz = 0;
    if (zi == 0.0E0 && zr == 0.0E0) {
        ierr = 1;
    }
    if (fnu < 0.0e0) {
        ierr = 1;
    }
    if (kode < 1 || kode > 2) {
        ierr = 1;
    }
    if (n < 1) {
        ierr = 1;
    }
    if (ierr != 0) {
        return;
    }
    nn = n;
    tol = fmax_(d1mach(4), 1.0e-18);
    k1 = i1mach(15);
    k2 = i1mach(16);
    r1m5 = d1mach(5);
    k = imin_(std::abs(k1), std::abs(k2));
    elim = 2.303e0 * ((double)(k)*r1m5 - 3.0e0);
    k1 = i1mach(14) - 1;
    aa = r1m5 * (double)(k1);
    dig = fmin_(aa, 18.0e0);
    aa = aa * 2.303e0;
    alim = elim + fmax_(-aa, -41.45e0);
    fnul = 10.0e0 + 6.0e0 * (dig - 3.0e0);
    rl = 1.2e0 * dig + 3.0e0;
    az = zabs(zr, zi);
    fn = fnu + (double)(nn - 1);
    aa = 0.5e0 / tol;
    bb = (double)(i1mach(9)) * 0.5e0;
    aa = fmin_(aa, bb);
    aa = std::sqrt(aa);
    if (az > aa) {
        ierr = 3;
    }
    if (fn > aa) {
        ierr = 3;
    }
    ufl = d1mach(1) * 1.0e+3;
    if (az < ufl) {
        goto L180;
    }
    if (fnu > fnul) {
        goto L80;
    }
    if (fn <= 1.0e0) {
        goto L60;
    }
    if (fn > 2.0e0) {
        goto L50;
    }
    if (az > tol) {
        goto L60;
    }
    arg = 0.5e0 * az;
    aln = -fn * std::log(arg);
    if (aln > elim) {
        goto L180;
    }
    goto L60;
L50:;
    zuoik(zr, zi, fnu, kode, 2, nn, cyr, cyi, nuf, tol, elim, alim);
    if (nuf < 0) {
        goto L180;
    }
    nz = nz + nuf;
    nn = nn - nuf;
    if (nn == 0) {
        goto L100;
    }
L60:;
    if (zr < 0.0e0) {
        goto L70;
    }
    zbknu(zr, zi, fnu, kode, nn, cyr, cyi, nw, tol, elim, alim);
    if (nw < 0) {
        goto L200;
    }
    nz = nw;
    return;
L70:;
    if (nz != 0) {
        goto L180;
    }
    mr = 1;
    if (zi < 0.0e0) {
        mr = -1;
    }
    zacon(zr, zi, fnu, kode, mr, nn, cyr, cyi, nw, rl, fnul, tol, elim, alim);
    if (nw < 0) {
        goto L200;
    }
    nz = nw;
    return;
L80:;
    mr = 0;
    if (zr >= 0.0e0) {
        goto L90;
    }
    mr = 1;
    if (zi < 0.0e0) {
        mr = -1;
    }
L90:;
    zbunk(zr, zi, fnu, kode, mr, nn, cyr, cyi, nw, tol, elim, alim);
    if (nw < 0) {
        goto L200;
    }
    nz = nz + nw;
    return;
L100:;
    if (zr < 0.0e0) {
        goto L180;
    }
    return;
L180:;
    nz = 0;
    ierr = 2;
    return;
L200:;
    if (nw == (-1)) {
        goto L180;
    }
    nz = 0;
    ierr = 5;
    return;
    nz = 0;
    ierr = 4;
    return;
}
//=============================================================================
void
zbesy(double zr, double zi, double fnu, int kode, int n, double* cyr, double* cyi, int& nz,
    double* cwrkr, double* cwrki, int& ierr)
{
    double aa = 0;
    double ascle = 0;
    double atol = 0;
    double bb = 0;
    double c1i = 0;
    double c1r = 0;
    double c2i = 0;
    double c2r = 0;
    double elim = 0;
    double exi = 0;
    double exr = 0;
    double ey = 0;
    double hcii = 0;
    int i = 0;
    int k = 0;
    int k1 = 0;
    int k2 = 0;
    int nz1 = 0;
    int nz2 = 0;
    double r1m5 = 0;
    double rtol = 0;
    double sti = 0;
    double str = 0;
    double tay = 0;
    double tol = 0;
    int lim0_ = 0;
    int lim1_ = 0;
    ierr = 0;
    nz = 0;
    if (zr == 0.0e0 && zi == 0.0e0) {
        ierr = 1;
    }
    if (fnu < 0.0e0) {
        ierr = 1;
    }
    if (kode < 1 || kode > 2) {
        ierr = 1;
    }
    if (n < 1) {
        ierr = 1;
    }
    if (ierr != 0) {
        return;
    }
    hcii = 0.5e0;
    zbesh(zr, zi, fnu, kode, 1, n, cyr, cyi, nz1, ierr);
    if (ierr != 0 && ierr != 3) {
        goto L170;
    }
    zbesh(zr, zi, fnu, kode, 2, n, cwrkr, cwrki, nz2, ierr);
    if (ierr != 0 && ierr != 3) {
        goto L170;
    }
    nz = imin_(nz1, nz2);
    if (kode == 2) {
        goto L60;
    }
    lim0_ = n;
    for (i = 1; i <= lim0_; i += 1) {
        str = cwrkr[(i)-1] - cyr[(i)-1];
        sti = cwrki[(i)-1] - cyi[(i)-1];
        cyr[(i)-1] = -sti * hcii;
        cyi[(i)-1] = str * hcii;
        ;
    }
    return;
L60:;
    tol = fmax_(d1mach(4), 1.0e-18);
    k1 = i1mach(15);
    k2 = i1mach(16);
    k = imin_(std::abs(k1), std::abs(k2));
    r1m5 = d1mach(5);
    elim = 2.303e0 * ((double)(k)*r1m5 - 3.0e0);
    exr = std::cos(zr);
    exi = std::sin(zr);
    ey = 0.0e0;
    tay = std::fabs(zi + zi);
    if (tay < elim) {
        ey = std::exp(-tay);
    }
    if (zi < 0.0e0) {
        goto L90;
    }
    c1r = exr * ey;
    c1i = exi * ey;
    c2r = exr;
    c2i = -exi;
L70:;
    nz = 0;
    rtol = 1.0e0 / tol;
    ascle = d1mach(1) * rtol * 1.0e+3;
    lim1_ = n;
    for (i = 1; i <= lim1_; i += 1) {
        aa = cwrkr[(i)-1];
        bb = cwrki[(i)-1];
        atol = 1.0e0;
        if (fmax_(std::fabs(aa), std::fabs(bb)) > ascle) {
            goto L75;
        }
        aa = aa * rtol;
        bb = bb * rtol;
        atol = tol;
    L75:;
        str = (aa * c2r - bb * c2i) * atol;
        sti = (aa * c2i + bb * c2r) * atol;
        aa = cyr[(i)-1];
        bb = cyi[(i)-1];
        atol = 1.0e0;
        if (fmax_(std::fabs(aa), std::fabs(bb)) > ascle) {
            goto L85;
        }
        aa = aa * rtol;
        bb = bb * rtol;
        atol = tol;
    L85:;
        str = str - (aa * c1r - bb * c1i) * atol;
        sti = sti - (aa * c1i + bb * c1r) * atol;
        cyr[(i)-1] = -sti * hcii;
        cyi[(i)-1] = str * hcii;
        if (str == 0.0e0 && sti == 0.0e0 && ey == 0.0e0) {
            nz = nz + 1;
        };
    }
    return;
L90:;
    c1r = exr;
    c1i = exi;
    c2r = exr * ey;
    c2i = -exi * ey;
    goto L70;
L170:;
    nz = 0;
    return;
}
//=============================================================================
void
zairy(double zr, double zi, int id, int kode, double& air, double& aii, int& nz, int& ierr)
{
    const double c1 = 3.55028053887817240e-01;
    const double c2 = 2.58819403792806799e-01;
    const double coef = 1.83776298473930683e-01;
    const double onei = 0.0e0;
    const double oner = 1.0e0;
    const double tth = 6.66666666666666667e-01;
    const double zeroi = 0.0e0;
    const double zeror = 0.0e0;
    double aa = 0;
    double ad = 0;
    double ak = 0;
    double alaz = 0;
    double alim = 0;
    double atrm = 0;
    double az = 0;
    double az3 = 0;
    double bb = 0;
    double bk = 0;
    double cc = 0;
    double ck = 0;
    double csqi = 0;
    double csqr = 0;
    double cyi[1] = {};
    double cyr[1] = {};
    double d1 = 0;
    double d2 = 0;
    double dig = 0;
    double dk = 0;
    double elim = 0;
    double fid = 0;
    double fnu = 0;
    int iflag = 0;
    int k = 0;
    int k1 = 0;
    int k2 = 0;
    int mr = 0;
    int nn = 0;
    double ptr = 0;
    double r1m5 = 0;
    double rl = 0;
    double s1i = 0;
    double s1r = 0;
    double s2i = 0;
    double s2r = 0;
    double sfac = 0;
    double sti = 0;
    double str = 0;
    double tol = 0;
    double trm1i = 0;
    double trm1r = 0;
    double trm2i = 0;
    double trm2r = 0;
    double z3i = 0;
    double z3r = 0;
    double ztai = 0;
    double ztar = 0;
    int lim0_ = 0;
    ierr = 0;
    nz = 0;
    if (id < 0 || id > 1) {
        ierr = 1;
    }
    if (kode < 1 || kode > 2) {
        ierr = 1;
    }
    if (ierr != 0) {
        return;
    }
    az = zabs(zr, zi);
    tol = fmax_(d1mach(4), 1.0e-18);
    fid = (double)(id);
    if (az > 1.0e0) {
        goto L70;
    }
    s1r = oner;
    s1i = onei;
    s2r = oner;
    s2i = onei;
    if (az < tol) {
        goto L170;
    }
    aa = az * az;
    if (aa < tol / az) {
        goto L40;
    }
    trm1r = oner;
    trm1i = onei;
    trm2r = oner;
    trm2i = onei;
    atrm = 1.0e0;
    str = zr * zr - zi * zi;
    sti = zr * zi + zi * zr;
    z3r = str * zr - sti * zi;
    z3i = str * zi + sti * zr;
    az3 = az * aa;
    ak = 2.0e0 + fid;
    bk = 3.0e0 - fid - fid;
    ck = 4.0e0 - fid;
    dk = 3.0e0 + fid + fid;
    d1 = ak * dk;
    d2 = bk * ck;
    ad = fmin_(d1, d2);
    ak = 24.0e0 + 9.0e0 * fid;
    bk = 30.0e0 - 9.0e0 * fid;
    lim0_ = 25;
    for (k = 1; k <= lim0_; k += 1) {
        str = (trm1r * z3r - trm1i * z3i) / d1;
        trm1i = (trm1r * z3i + trm1i * z3r) / d1;
        trm1r = str;
        s1r = s1r + trm1r;
        s1i = s1i + trm1i;
        str = (trm2r * z3r - trm2i * z3i) / d2;
        trm2i = (trm2r * z3i + trm2i * z3r) / d2;
        trm2r = str;
        s2r = s2r + trm2r;
        s2i = s2i + trm2i;
        atrm = atrm * az3 / ad;
        d1 = d1 + ak;
        d2 = d2 + bk;
        ad = fmin_(d1, d2);
        if (atrm < tol * ad) {
            goto L40;
        }
        ak = ak + 18.0e0;
        bk = bk + 18.0e0;
        ;
    }
L40:;
    if (id == 1) {
        goto L50;
    }
    air = s1r * c1 - c2 * (zr * s2r - zi * s2i);
    aii = s1i * c1 - c2 * (zr * s2i + zi * s2r);
    if (kode == 1) {
        return;
    }
    zsqrt(zr, zi, str, sti);
    ztar = tth * (zr * str - zi * sti);
    ztai = tth * (zr * sti + zi * str);
    zexp(ztar, ztai, str, sti);
    ptr = air * str - aii * sti;
    aii = air * sti + aii * str;
    air = ptr;
    return;
L50:;
    air = -s2r * c2;
    aii = -s2i * c2;
    if (az <= tol) {
        goto L60;
    }
    str = zr * s1r - zi * s1i;
    sti = zr * s1i + zi * s1r;
    cc = c1 / (1.0e0 + fid);
    air = air + cc * (str * zr - sti * zi);
    aii = aii + cc * (str * zi + sti * zr);
L60:;
    if (kode == 1) {
        return;
    }
    zsqrt(zr, zi, str, sti);
    ztar = tth * (zr * str - zi * sti);
    ztai = tth * (zr * sti + zi * str);
    zexp(ztar, ztai, str, sti);
    ptr = str * air - sti * aii;
    aii = str * aii + sti * air;
    air = ptr;
    return;
L70:;
    fnu = (1.0e0 + fid) / 3.0e0;
    k1 = i1mach(15);
    k2 = i1mach(16);
    r1m5 = d1mach(5);
    k = imin_(std::abs(k1), std::abs(k2));
    elim = 2.303e0 * ((double)(k)*r1m5 - 3.0e0);
    k1 = i1mach(14) - 1;
    aa = r1m5 * (double)(k1);
    dig = fmin_(aa, 18.0e0);
    aa = aa * 2.303e0;
    alim = elim + fmax_(-aa, -41.45e0);
    rl = 1.2e0 * dig + 3.0e0;
    alaz = std::log(az);
    aa = 0.5e0 / tol;
    bb = (double)(i1mach(9)) * 0.5e0;
    aa = fmin_(aa, bb);
    aa = std::pow(aa, tth);
    aa = std::sqrt(aa);
    if (az > aa) {
        ierr = 3;
    }
    zsqrt(zr, zi, csqr, csqi);
    ztar = tth * (zr * csqr - zi * csqi);
    ztai = tth * (zr * csqi + zi * csqr);
    iflag = 0;
    sfac = 1.0e0;
    ak = ztai;
    if (zr >= 0.0e0) {
        goto L80;
    }
    bk = ztar;
    ck = -std::fabs(bk);
    ztar = ck;
    ztai = ak;
L80:;
    if (zi != 0.0e0) {
        goto L90;
    }
    if (zr > 0.0e0) {
        goto L90;
    }
    ztar = 0.0e0;
    ztai = ak;
L90:;
    aa = ztar;
    if (aa >= 0.0e0 && zr > 0.0e0) {
        goto L110;
    }
    if (kode == 2) {
        goto L100;
    }
    if (aa > (-alim)) {
        goto L100;
    }
    aa = -aa + 0.25e0 * alaz;
    iflag = 1;
    sfac = tol;
    if (aa > elim) {
        goto L270;
    }
L100:;
    mr = 1;
    if (zi < 0.0e0) {
        mr = -1;
    }
    zacai(ztar, ztai, fnu, kode, mr, 1, cyr, cyi, nn, rl, tol, elim, alim);
    if (nn < 0) {
        goto L280;
    }
    nz = nz + nn;
    goto L130;
L110:;
    if (kode == 2) {
        goto L120;
    }
    if (aa < alim) {
        goto L120;
    }
    aa = -aa - 0.25e0 * alaz;
    iflag = 2;
    sfac = 1.0e0 / tol;
    if (aa < (-elim)) {
        goto L210;
    }
L120:;
    zbknu(ztar, ztai, fnu, kode, 1, cyr, cyi, nz, tol, elim, alim);
L130:;
    s1r = cyr[(1) - 1] * coef;
    s1i = cyi[(1) - 1] * coef;
    if (iflag != 0) {
        goto L150;
    }
    if (id == 1) {
        goto L140;
    }
    air = csqr * s1r - csqi * s1i;
    aii = csqr * s1i + csqi * s1r;
    return;
L140:;
    air = -(zr * s1r - zi * s1i);
    aii = -(zr * s1i + zi * s1r);
    return;
L150:;
    s1r = s1r * sfac;
    s1i = s1i * sfac;
    if (id == 1) {
        goto L160;
    }
    str = s1r * csqr - s1i * csqi;
    s1i = s1r * csqi + s1i * csqr;
    s1r = str;
    air = s1r / sfac;
    aii = s1i / sfac;
    return;
L160:;
    str = -(s1r * zr - s1i * zi);
    s1i = -(s1r * zi + s1i * zr);
    s1r = str;
    air = s1r / sfac;
    aii = s1i / sfac;
    return;
L170:;
    aa = 1.0e+3 * d1mach(1);
    s1r = zeror;
    s1i = zeroi;
    if (id == 1) {
        goto L190;
    }
    if (az <= aa) {
        goto L180;
    }
    s1r = c2 * zr;
    s1i = c2 * zi;
L180:;
    air = c1 - s1r;
    aii = -s1i;
    return;
L190:;
    air = -c2;
    aii = 0.0e0;
    aa = std::sqrt(aa);
    if (az <= aa) {
        goto L200;
    }
    s1r = 0.5e0 * (zr * zr - zi * zi);
    s1i = zr * zi;
L200:;
    air = air + c1 * s1r;
    aii = aii + c1 * s1i;
    return;
L210:;
    nz = 1;
    air = zeror;
    aii = zeroi;
    return;
L270:;
    nz = 0;
    ierr = 2;
    return;
L280:;
    if (nn == (-1)) {
        goto L270;
    }
    nz = 0;
    ierr = 5;
    return;
    ierr = 4;
    nz = 0;
    return;
}
//=============================================================================
void
zbiry(double zr, double zi, int id, int kode, double& bir, double& bii, int& ierr)
{
    const double c1 = 6.14926627446000736e-01;
    const double c2 = 4.48288357353826359e-01;
    const double coef = 5.77350269189625765e-01;
    const double onei = 0.0e0;
    const double oner = 1.0e0;
    const double pi = 3.14159265358979324e+00;
    const double tth = 6.66666666666666667e-01;
    double aa = 0;
    double ad = 0;
    double ak = 0;
    double alim = 0;
    double atrm = 0;
    double az = 0;
    double az3 = 0;
    double bb = 0;
    double bk = 0;
    double cc = 0;
    double ck = 0;
    double csqi = 0;
    double csqr = 0;
    double cyi[2] = {};
    double cyr[2] = {};
    double d1 = 0;
    double d2 = 0;
    double dig = 0;
    double dk = 0;
    double eaa = 0;
    double elim = 0;
    double fid = 0;
    double fmr = 0;
    double fnu = 0;
    double fnul = 0;
    int k = 0;
    int k1 = 0;
    int k2 = 0;
    int nz = 0;
    double r1m5 = 0;
    double rl = 0;
    double s1i = 0;
    double s1r = 0;
    double s2i = 0;
    double s2r = 0;
    double sfac = 0;
    double sti = 0;
    double str = 0;
    double tol = 0;
    double trm1i = 0;
    double trm1r = 0;
    double trm2i = 0;
    double trm2r = 0;
    double z3i = 0;
    double z3r = 0;
    double ztai = 0;
    double ztar = 0;
    int lim0_ = 0;
    ierr = 0;
    nz = 0;
    if (id < 0 || id > 1) {
        ierr = 1;
    }
    if (kode < 1 || kode > 2) {
        ierr = 1;
    }
    if (ierr != 0) {
        return;
    }
    az = zabs(zr, zi);
    tol = fmax_(d1mach(4), 1.0e-18);
    fid = (double)(id);
    if (az > 1.0E0) {
        goto L70;
    }
    s1r = oner;
    s1i = onei;
    s2r = oner;
    s2i = onei;
    if (az < tol) {
        goto L130;
    }
    aa = az * az;
    if (aa < tol / az) {
        goto L40;
    }
    trm1r = oner;
    trm1i = onei;
    trm2r = oner;
    trm2i = onei;
    atrm = 1.0e0;
    str = zr * zr - zi * zi;
    sti = zr * zi + zi * zr;
    z3r = str * zr - sti * zi;
    z3i = str * zi + sti * zr;
    az3 = az * aa;
    ak = 2.0e0 + fid;
    bk = 3.0e0 - fid - fid;
    ck = 4.0e0 - fid;
    dk = 3.0e0 + fid + fid;
    d1 = ak * dk;
    d2 = bk * ck;
    ad = fmin_(d1, d2);
    ak = 24.0e0 + 9.0e0 * fid;
    bk = 30.0e0 - 9.0e0 * fid;
    lim0_ = 25;
    for (k = 1; k <= lim0_; k += 1) {
        str = (trm1r * z3r - trm1i * z3i) / d1;
        trm1i = (trm1r * z3i + trm1i * z3r) / d1;
        trm1r = str;
        s1r = s1r + trm1r;
        s1i = s1i + trm1i;
        str = (trm2r * z3r - trm2i * z3i) / d2;
        trm2i = (trm2r * z3i + trm2i * z3r) / d2;
        trm2r = str;
        s2r = s2r + trm2r;
        s2i = s2i + trm2i;
        atrm = atrm * az3 / ad;
        d1 = d1 + ak;
        d2 = d2 + bk;
        ad = fmin_(d1, d2);
        if (atrm < tol * ad) {
            goto L40;
        }
        ak = ak + 18.0e0;
        bk = bk + 18.0e0;
        ;
    }
L40:;
    if (id == 1) {
        goto L50;
    }
    bir = c1 * s1r + c2 * (zr * s2r - zi * s2i);
    bii = c1 * s1i + c2 * (zr * s2i + zi * s2r);
    if (kode == 1) {
        return;
    }
    zsqrt(zr, zi, str, sti);
    ztar = tth * (zr * str - zi * sti);
    ztai = tth * (zr * sti + zi * str);
    aa = ztar;
    aa = -std::fabs(aa);
    eaa = std::exp(aa);
    bir = bir * eaa;
    bii = bii * eaa;
    return;
L50:;
    bir = s2r * c2;
    bii = s2i * c2;
    if (az <= tol) {
        goto L60;
    }
    cc = c1 / (1.0e0 + fid);
    str = s1r * zr - s1i * zi;
    sti = s1r * zi + s1i * zr;
    bir = bir + cc * (str * zr - sti * zi);
    bii = bii + cc * (str * zi + sti * zr);
L60:;
    if (kode == 1) {
        return;
    }
    zsqrt(zr, zi, str, sti);
    ztar = tth * (zr * str - zi * sti);
    ztai = tth * (zr * sti + zi * str);
    aa = ztar;
    aa = -std::fabs(aa);
    eaa = std::exp(aa);
    bir = bir * eaa;
    bii = bii * eaa;
    return;
L70:;
    fnu = (1.0e0 + fid) / 3.0e0;
    k1 = i1mach(15);
    k2 = i1mach(16);
    r1m5 = d1mach(5);
    k = imin_(std::abs(k1), std::abs(k2));
    elim = 2.303e0 * ((double)(k)*r1m5 - 3.0e0);
    k1 = i1mach(14) - 1;
    aa = r1m5 * (double)(k1);
    dig = fmin_(aa, 18.0e0);
    aa = aa * 2.303e0;
    alim = elim + fmax_(-aa, -41.45e0);
    rl = 1.2e0 * dig + 3.0e0;
    fnul = 10.0e0 + 6.0e0 * (dig - 3.0e0);
    aa = 0.5e0 / tol;
    bb = (double)(i1mach(9)) * 0.5e0;
    aa = fmin_(aa, bb);
    aa = std::pow(aa, tth);
    aa = std::sqrt(aa);
    if (az > aa) {
        ierr = 3;
    }
    zsqrt(zr, zi, csqr, csqi);
    ztar = tth * (zr * csqr - zi * csqi);
    ztai = tth * (zr * csqi + zi * csqr);
    sfac = 1.0e0;
    ak = ztai;
    if (zr >= 0.0e0) {
        goto L80;
    }
    bk = ztar;
    ck = -std::fabs(bk);
    ztar = ck;
    ztai = ak;
L80:;
    if (zi != 0.0e0 || zr > 0.0e0) {
        goto L90;
    }
    ztar = 0.0e0;
    ztai = ak;
L90:;
    aa = ztar;
    if (kode == 2) {
        goto L100;
    }
    bb = std::fabs(aa);
    if (bb < alim) {
        goto L100;
    }
    bb = bb + 0.25e0 * std::log(az);
    sfac = tol;
    if (bb > elim) {
        goto L190;
    }
L100:;
    fmr = 0.0e0;
    if (aa >= 0.0e0 && zr > 0.0e0) {
        goto L110;
    }
    fmr = pi;
    if (zi < 0.0e0) {
        fmr = -pi;
    }
    ztar = -ztar;
    ztai = -ztai;
L110:;
    zbinu(ztar, ztai, fnu, kode, 1, cyr, cyi, nz, rl, fnul, tol, elim, alim);
    if (nz < 0) {
        goto L200;
    }
    aa = fmr * fnu;
    z3r = sfac;
    str = std::cos(aa);
    sti = std::sin(aa);
    s1r = (str * cyr[(1) - 1] - sti * cyi[(1) - 1]) * z3r;
    s1i = (str * cyi[(1) - 1] + sti * cyr[(1) - 1]) * z3r;
    fnu = (2.0e0 - fid) / 3.0e0;
    zbinu(ztar, ztai, fnu, kode, 2, cyr, cyi, nz, rl, fnul, tol, elim, alim);
    cyr[(1) - 1] = cyr[(1) - 1] * z3r;
    cyi[(1) - 1] = cyi[(1) - 1] * z3r;
    cyr[(2) - 1] = cyr[(2) - 1] * z3r;
    cyi[(2) - 1] = cyi[(2) - 1] * z3r;
    zdiv(cyr[(1) - 1], cyi[(1) - 1], ztar, ztai, str, sti);
    s2r = (fnu + fnu) * str + cyr[(2) - 1];
    s2i = (fnu + fnu) * sti + cyi[(2) - 1];
    aa = fmr * (fnu - 1.0e0);
    str = std::cos(aa);
    sti = std::sin(aa);
    s1r = coef * (s1r + s2r * str - s2i * sti);
    s1i = coef * (s1i + s2r * sti + s2i * str);
    if (id == 1) {
        goto L120;
    }
    str = csqr * s1r - csqi * s1i;
    s1i = csqr * s1i + csqi * s1r;
    s1r = str;
    bir = s1r / sfac;
    bii = s1i / sfac;
    return;
L120:;
    str = zr * s1r - zi * s1i;
    s1i = zr * s1i + zi * s1r;
    s1r = str;
    bir = s1r / sfac;
    bii = s1i / sfac;
    return;
L130:;
    aa = c1 * (1.0e0 - fid) + fid * c2;
    bir = aa;
    bii = 0.0e0;
    return;
L190:;
    ierr = 2;
    nz = 0;
    return;
L200:;
    if (nz == (-1)) {
        goto L190;
    }
    nz = 0;
    ierr = 5;
    return;
    ierr = 4;
    nz = 0;
    return;
}
//=============================================================================
} // namespace Nelson::Amos
//=============================================================================
