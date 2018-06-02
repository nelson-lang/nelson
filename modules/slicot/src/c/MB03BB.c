/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static doublecomplex c_b2 = { 1., 0. };
static doublecomplex c_b7 = { 1., -2. };
static doublecomplex c_b8 = { 2., 2. };
static integer c__3 = 3;
static integer c__1 = 1;
static integer c__2 = 2;

EXPORTSYMBOL /* Subroutine */ int mb03bb_(base, lgbas, ulp, k, amap, s, sinv, a, lda1, lda2, alphar,
    alphai, beta, scal, dwork, info) doublereal *base,
    *lgbas, *ulp;
integer *k, *amap, *s, *sinv;
doublereal* a;
integer *lda1, *lda2;
doublereal *alphar, *alphai, *beta;
integer* scal;
doublereal* dwork;
integer* info;
{
    /* System generated locals */
    integer a_dim1, a_dim2, a_offset, i__1, i__2;
    doublereal d__1, d__2;
    doublecomplex z__1, z__2;
    /* Builtin functions */
    void d_cnjg();
    double d_imag(), log(), pow_dd();
    /* Local variables */
    static doublecomplex temp;
    extern /* Subroutine */ int zrot_();
    static integer i__, j;
    static doublecomplex t[4] /* was [2][2] */, z__[9] /* was [3][3] */;
    static integer iiter;
    static doublereal tempi, tempr;
    extern doublereal dlapy2_();
    static integer ai;
    static doublereal cs;
    static integer sl;
    static doublecomplex sn;
    extern /* Subroutine */ int dladiv_(), zlartg_();
    static integer pdm;
    static doublereal lhs, cst;
    static integer pdw;
    static doublereal rhs;
    static doublecomplex snt;
    /*     SLICOT RELEASE 5.0. */
    /*     Copyright (c) 2002-2010 NICONET e.V. */
    /*     This program is free software: you can redistribute it and/or */
    /*     modify it under the terms of the GNU General Public License as */
    /*     published by the Free Software Foundation, either version 2 of */
    /*     the License, or (at your option) any later version. */
    /*     This program is distributed in the hope that it will be useful, */
    /*     but WITHOUT ANY WARRANTY; without even the implied warranty of */
    /*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the */
    /*     GNU General Public License for more details. */
    /*     You should have received a copy of the GNU General Public License */
    /*     along with this program.  If not, see */
    /*     <http://www.gnu.org/licenses/>. */
    /*     PURPOSE */
    /*     To compute the eigenvalues of a general 2-by-2 matrix product via */
    /*     a complex single shifted periodic QZ algorithm. */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     BASE    (input)  DOUBLE PRECISION */
    /*             Machine base. */
    /*     LGBAS   (input)  DOUBLE PRECISION */
    /*             Logarithm of BASE. */
    /*     ULP     (input)  DOUBLE PRECISION */
    /*             Machine precision. */
    /*     K       (input)  INTEGER */
    /*             The number of factors.  K >= 1. */
    /*     AMAP    (input) INTEGER array, dimension (K) */
    /*             The map for accessing the factors, i.e., if AMAP(I) = J, */
    /*             then the factor A_I is stored at the J-th position in A. */
    /*     S       (input)  INTEGER array, dimension (K) */
    /*             The signature array. Each entry of S must be 1 or -1. */
    /*     SINV    (input) INTEGER */
    /*             Signature multiplier. Entries of S are virtually */
    /*             multiplied by SINV. */
    /*     A       (input)  DOUBLE PRECISION array, dimension (LDA1,LDA2,K) */
    /*             On entry, the leading 2-by-2-by-K part of this array must */
    /*             contain a 2-by-2 product (implicitly represented by its K */
    /*             factors) in upper Hessenberg-triangular form. */
    /*     LDA1    INTEGER */
    /*             The first leading dimension of the array A.  LDA1 >= 2. */
    /*     LDA2    INTEGER */
    /*             The second leading dimension of the array A.  LDA2 >= 2. */
    /*     ALPHAR  (output)  DOUBLE PRECISION array, dimension (2) */
    /*             On exit, if INFO = 0, this array contains the scaled real */
    /*             part of the two eigenvalues. If BETA(I) <> 0, then the */
    /*             I-th eigenvalue (I = 1 : 2) is given by */
    /*                 (ALPHAR(I) + ALPHAI(I)*SQRT(-1) ) * (BASE)**SCAL(I). */
    /*     ALPHAI  (output)  DOUBLE PRECISION array, dimension (2) */
    /*             On exit, if INFO = 0, this array contains the scaled */
    /*             imaginary part of the two eigenvalues. ALPHAI(1) >= 0. */
    /*     BETA    (output)  DOUBLE PRECISION array, dimension (2) */
    /*             On exit, if INFO = 0, this array contains information */
    /*             about infinite eigenvalues. If BETA(I) = 0, then the */
    /*             I-th eigenvalue is infinite. Otherwise, BETA(I) = 1.0. */
    /*     SCAL    (output)  INTEGER array, dimension (2) */
    /*             On exit, if INFO = 0, this array contains the scaling */
    /*             exponents for the two eigenvalues. */
    /*     Workspace */
    /*     DWORK   DOUBLE PRECISION array, dimension (8*K) */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             = 1:  the periodic QZ algorithm did not converge. */
    /*     METHOD */
    /*     A complex single shifted periodic QZ iteration is applied. */
    /*     CONTRIBUTOR */
    /*     D. Kressner, Technical Univ. Berlin, Germany, June 2001. */
    /*     REVISIONS */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Romania, */
    /*     July 2009, SLICOT Library version of the routine PLACP2. */
    /*     V. Sima, June 2010, July 2010. */
    /*     KEYWORDS */
    /*     Eigenvalues, QZ algorithm, periodic QZ algorithm, orthogonal */
    /*     transformation. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. Local Arrays .. */
    /*     .. External Functions .. */
    /*     .. External Subroutines .. */
    /*     .. Intrinsic Functions .. */
    /*     .. Executable Statements .. */
    /*     Apply a complex single shifted periodic QZ iteration. */
    /*     This might not be efficient but it seems to be reliable. */
    /* Parameter adjustments */
    --amap;
    --s;
    a_dim1 = *lda1;
    a_dim2 = *lda2;
    a_offset = a_dim1 * (a_dim2 + 1) + 1;
    a -= a_offset;
    --alphar;
    --alphai;
    --beta;
    --scal;
    --dwork;
    /* Function Body */
    pdw = 0;
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        ai = amap[i__];
        dwork[pdw + 1] = a[(ai * a_dim2 + 1) * a_dim1 + 1];
        dwork[pdw + 2] = 0.;
        dwork[pdw + 3] = a[(ai * a_dim2 + 1) * a_dim1 + 2];
        dwork[pdw + 4] = 0.;
        dwork[pdw + 5] = a[(ai * a_dim2 + 2) * a_dim1 + 1];
        dwork[pdw + 6] = 0.;
        dwork[pdw + 7] = a[(ai * a_dim2 + 2) * a_dim1 + 2];
        dwork[pdw + 8] = 0.;
        pdw += 8;
        /* L10: */
    }
    pdm = pdw;
    for (iiter = 1; iiter <= 60; ++iiter) {
        /*        Test for deflation. */
        lhs = dlapy2_(&dwork[3], &dwork[4]);
        /* Computing MAX */
        d__1 = dlapy2_(&dwork[1], &dwork[2]), d__2 = dlapy2_(&dwork[7], &dwork[8]);
        rhs = max(d__1, d__2);
        if (rhs == 0.) {
            rhs = dlapy2_(&dwork[5], &dwork[6]);
        }
        if (lhs <= *ulp * rhs) {
            goto L50;
        }
        /*        Start Iteration. */
        if (iiter == 1) {
            /*           Compute a randomly chosen initial unitary shift. */
            zlartg_(&c_b7, &c_b8, &cs, &sn, &temp);
        } else if (iiter % 30 == 0) {
            /*           Ad hoc shift. */
            d__1 = (doublereal)i__;
            z__1.r = d__1, z__1.i = 1.;
            zlartg_(&z__1, &c_b7, &cs, &sn, &temp);
        } else {
            /*           Compute the shift by a product QR decomposition. */
            cs = 1.;
            sn.r = 0., sn.i = 0.;
            zlartg_(&c_b2, &c_b2, &cst, &snt, &temp);
            pdw = pdm;
            for (i__ = *k; i__ >= 2; --i__) {
                pdw += -8;
                i__1 = pdw + 1;
                i__2 = pdw + 2;
                z__1.r = dwork[i__1], z__1.i = dwork[i__2];
                temp.r = z__1.r, temp.i = z__1.i;
                z__[0].r = temp.r, z__[0].i = temp.i;
                z__[1].r = 0., z__[1].i = 0.;
                z__[2].r = 0., z__[2].i = 0.;
                z__[3].r = 0., z__[3].i = 0.;
                z__[4].r = temp.r, z__[4].i = temp.i;
                i__1 = pdw + 3;
                i__2 = pdw + 4;
                z__1.r = dwork[i__1], z__1.i = dwork[i__2];
                z__[5].r = z__1.r, z__[5].i = z__1.i;
                z__[6].r = 0., z__[6].i = 0.;
                i__1 = pdw + 5;
                i__2 = pdw + 6;
                z__1.r = dwork[i__1], z__1.i = dwork[i__2];
                z__[7].r = z__1.r, z__[7].i = z__1.i;
                i__1 = pdw + 7;
                i__2 = pdw + 8;
                z__1.r = dwork[i__1], z__1.i = dwork[i__2];
                z__[8].r = z__1.r, z__[8].i = z__1.i;
                if (s[amap[i__]] == *sinv) {
                    d_cnjg(&z__1, &snt);
                    zrot_(&c__3, z__, &c__1, &z__[6], &c__1, &cst, &z__1);
                    d_cnjg(&z__1, &sn);
                    zrot_(&c__3, z__, &c__1, &z__[3], &c__1, &cs, &z__1);
                    zlartg_(z__, &z__[2], &cst, &snt, &temp);
                    zlartg_(&temp, &z__[1], &cs, &sn, &temp);
                } else {
                    zrot_(&c__3, z__, &c__3, &z__[2], &c__3, &cst, &snt);
                    zrot_(&c__3, z__, &c__3, &z__[1], &c__3, &cs, &sn);
                    temp.r = z__[8].r, temp.i = z__[8].i;
                    zlartg_(&temp, &z__[2], &cst, &snt, &z__[8]);
                    z__1.r = -snt.r, z__1.i = -snt.i;
                    snt.r = z__1.r, snt.i = z__1.i;
                    d_cnjg(&z__1, &snt);
                    zrot_(&c__2, z__, &c__1, &z__[6], &c__1, &cst, &z__1);
                    temp.r = z__[4].r, temp.i = z__[4].i;
                    zlartg_(&temp, &z__[1], &cs, &sn, &z__[4]);
                    z__1.r = -sn.r, z__1.i = -sn.i;
                    sn.r = z__1.r, sn.i = z__1.i;
                }
                /* L20: */
            }
            pdw = 0;
            i__1 = pdw + 1;
            i__2 = pdw + 2;
            z__1.r = dwork[i__1], z__1.i = dwork[i__2];
            z__[0].r = z__1.r, z__[0].i = z__1.i;
            i__1 = pdw + 3;
            i__2 = pdw + 4;
            z__1.r = dwork[i__1], z__1.i = dwork[i__2];
            z__[1].r = z__1.r, z__[1].i = z__1.i;
            i__1 = pdw + 3;
            i__2 = pdw + 4;
            z__2.r = dwork[i__1], z__2.i = dwork[i__2];
            z__1.r = -z__2.r, z__1.i = -z__2.i;
            z__[3].r = z__1.r, z__[3].i = z__1.i;
            z__[4].r = 0., z__[4].i = 0.;
            i__1 = pdw + 7;
            i__2 = pdw + 8;
            z__2.r = dwork[i__1], z__2.i = dwork[i__2];
            z__1.r = -z__2.r, z__1.i = -z__2.i;
            z__[6].r = z__1.r, z__[6].i = z__1.i;
            z__[7].r = 0., z__[7].i = 0.;
            d_cnjg(&z__1, &snt);
            zrot_(&c__2, z__, &c__1, &z__[6], &c__1, &cst, &z__1);
            d_cnjg(&z__1, &sn);
            zrot_(&c__2, z__, &c__1, &z__[3], &c__1, &cs, &z__1);
            zlartg_(z__, &z__[1], &cs, &sn, &temp);
        }
        cst = cs;
        snt.r = sn.r, snt.i = sn.i;
        pdw = pdm;
        for (i__ = *k; i__ >= 2; --i__) {
            pdw += -8;
            i__1 = pdw + 1;
            i__2 = pdw + 2;
            z__1.r = dwork[i__1], z__1.i = dwork[i__2];
            t[0].r = z__1.r, t[0].i = z__1.i;
            i__1 = pdw + 3;
            i__2 = pdw + 4;
            z__1.r = dwork[i__1], z__1.i = dwork[i__2];
            t[1].r = z__1.r, t[1].i = z__1.i;
            i__1 = pdw + 5;
            i__2 = pdw + 6;
            z__1.r = dwork[i__1], z__1.i = dwork[i__2];
            t[2].r = z__1.r, t[2].i = z__1.i;
            i__1 = pdw + 7;
            i__2 = pdw + 8;
            z__1.r = dwork[i__1], z__1.i = dwork[i__2];
            t[3].r = z__1.r, t[3].i = z__1.i;
            if (s[amap[i__]] == *sinv) {
                d_cnjg(&z__1, &sn);
                zrot_(&c__2, t, &c__1, &t[2], &c__1, &cs, &z__1);
                temp.r = t[0].r, temp.i = t[0].i;
                zlartg_(&temp, &t[1], &cs, &sn, t);
                t[1].r = 0., t[1].i = 0.;
                zrot_(&c__1, &t[2], &c__2, &t[3], &c__2, &cs, &sn);
            } else {
                zrot_(&c__2, t, &c__2, &t[1], &c__2, &cs, &sn);
                temp.r = t[3].r, temp.i = t[3].i;
                zlartg_(&temp, &t[1], &cs, &sn, &t[3]);
                t[1].r = 0., t[1].i = 0.;
                z__1.r = -sn.r, z__1.i = -sn.i;
                sn.r = z__1.r, sn.i = z__1.i;
                d_cnjg(&z__1, &sn);
                zrot_(&c__1, t, &c__1, &t[2], &c__1, &cs, &z__1);
            }
            dwork[pdw + 1] = t[0].r;
            dwork[pdw + 2] = d_imag(t);
            dwork[pdw + 3] = t[1].r;
            dwork[pdw + 4] = d_imag(&t[1]);
            dwork[pdw + 5] = t[2].r;
            dwork[pdw + 6] = d_imag(&t[2]);
            dwork[pdw + 7] = t[3].r;
            dwork[pdw + 8] = d_imag(&t[3]);
            /* L30: */
        }
        pdw = 0;
        i__1 = pdw + 1;
        i__2 = pdw + 2;
        z__1.r = dwork[i__1], z__1.i = dwork[i__2];
        t[0].r = z__1.r, t[0].i = z__1.i;
        i__1 = pdw + 3;
        i__2 = pdw + 4;
        z__1.r = dwork[i__1], z__1.i = dwork[i__2];
        t[1].r = z__1.r, t[1].i = z__1.i;
        i__1 = pdw + 5;
        i__2 = pdw + 6;
        z__1.r = dwork[i__1], z__1.i = dwork[i__2];
        t[2].r = z__1.r, t[2].i = z__1.i;
        i__1 = pdw + 7;
        i__2 = pdw + 8;
        z__1.r = dwork[i__1], z__1.i = dwork[i__2];
        t[3].r = z__1.r, t[3].i = z__1.i;
        zrot_(&c__2, t, &c__2, &t[1], &c__2, &cst, &snt);
        d_cnjg(&z__1, &sn);
        zrot_(&c__2, t, &c__1, &t[2], &c__1, &cs, &z__1);
        dwork[pdw + 1] = t[0].r;
        dwork[pdw + 2] = d_imag(t);
        dwork[pdw + 3] = t[1].r;
        dwork[pdw + 4] = d_imag(&t[1]);
        dwork[pdw + 5] = t[2].r;
        dwork[pdw + 6] = d_imag(&t[2]);
        dwork[pdw + 7] = t[3].r;
        dwork[pdw + 8] = d_imag(&t[3]);
        /* L40: */
    }
    /*     Not converged. */
    *info = 1;
    goto L80;
    /*     Converged. */
L50:
    for (j = 1; j <= 2; ++j) {
        pdw = 0;
        if (j == 2) {
            pdw = 6;
        }
        tempi = 0.;
        tempr = 1.;
        beta[j] = 1.;
        scal[j] = 0;
        i__1 = *k;
        for (i__ = 1; i__ <= i__1; ++i__) {
            rhs = dlapy2_(&dwork[pdw + 1], &dwork[pdw + 2]);
            if (rhs != 0.) {
                sl = (integer)(log(rhs) / *lgbas);
                d__1 = (doublereal)sl;
                dwork[pdw + 1] /= pow_dd(base, &d__1);
                d__1 = (doublereal)sl;
                dwork[pdw + 2] /= pow_dd(base, &d__1);
            }
            if (s[amap[i__]] == 1) {
                lhs = tempi;
                tempi = tempr * dwork[pdw + 2] + tempi * dwork[pdw + 1];
                tempr = tempr * dwork[pdw + 1] - lhs * dwork[pdw + 2];
                scal[j] += sl;
            } else if (rhs == 0.) {
                beta[j] = 0.;
            } else {
                lhs = tempr;
                rhs = tempi;
                dladiv_(&lhs, &rhs, &dwork[pdw + 1], &dwork[pdw + 2], &tempr, &tempi);
                scal[j] -= sl;
            }
            if (i__ % 10 == 0 || i__ == *k) {
                rhs = dlapy2_(&tempr, &tempi);
                if (rhs == 0.) {
                    scal[j] = 0;
                } else {
                    sl = (integer)(log(rhs) / *lgbas);
                    d__1 = (doublereal)sl;
                    tempr /= pow_dd(base, &d__1);
                    d__1 = (doublereal)sl;
                    tempi /= pow_dd(base, &d__1);
                    scal[j] += sl;
                }
            }
            pdw += 8;
            /* L60: */
        }
        alphar[j] = tempr;
        alphai[j] = tempi;
        /* L70: */
    }
    if (tempi > 0.) {
        alphar[2] = alphar[1];
        alphai[2] = alphai[1];
        alphar[1] = tempr;
        alphai[1] = tempi;
        tempr = (doublereal)scal[2];
        scal[2] = scal[1];
        scal[1] = (integer)tempr;
    }
    *info = 0;
L80:
    return 0;
    /* *** Last line of MB03BB *** */
} /* mb03bb_ */
