/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

EXPORTSYMBOL /* Subroutine */ int mb03bc_(
    k, amap, s, sinv, a, lda1, lda2, macpar, cv, sv, dwork) integer *k,
    *amap, *s, *sinv;
doublereal* a;
integer *lda1, *lda2;
doublereal *macpar, *cv, *sv, *dwork;
{
    /* System generated locals */
    integer a_dim1, a_dim2, a_offset, i__1;
    doublereal d__1, d__2, d__3, d__4, d__5;
    /* Builtin functions */
    double sqrt();
    /* Local variables */
    static doublereal base, sfmn, rmin, temp, rmax, rmns, rmxs, twos;
    static integer i__;
    static doublereal ssmin, ssmax;
    extern /* Subroutine */ int dlasv2_();
    static doublereal a11, a12, a22, b11, b12, b22, cc;
    static integer ai;
    static doublereal cl, cr, s11, s12, s22, sc, t11, t12, t22, sl, mx, sr;
    static integer pw;
    extern /* Subroutine */ int dlartg_();
    static doublereal mx2;
    static integer scl;
    static doublereal eps;
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
    /*     To compute the product singular value decomposition of the K-1 */
    /*     triangular factors corresponding to a 2-by-2 product of K */
    /*     factors in upper Hessenberg-triangular form. */
    /*     For a general product of 2-by-2 triangular matrices */
    /*                        S(2)        S(3)            S(K) */
    /*            A = A(:,:,2)    A(:,:,3)    ... A(:,:,K), */
    /*     Givens rotators are computed so that */
    /*                                                          S(i) */
    /*       [  CV(i-1) SV(i-1) ] [ A(1,1,i)(in)  A(1,2,i)(in) ] */
    /*       [ -SV(i-1) CV(i-1) ] [     0         A(2,2,i)(in) ] */
    /*                                      S(i) */
    /*       [ A(1,1,i)(out) A(1,2,i)(out) ]    [  CV(i) SV(i) ] */
    /*     = [     0         A(2,2,i)(out) ]    [ -SV(i) CV(i) ] */
    /*     stays upper triangular and */
    /*       [  CV(1) SV(1) ]       [ CV(K) -SV(K) ] */
    /*       [ -SV(1) CV(1) ] * A * [ SV(K)  CV(K) ] */
    /*     is diagonal. */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
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
    /*     A       (input/output)  DOUBLE PRECISION array, dimension */
    /*                             (LDA1,LDA2,K) */
    /*             On entry, the leading 2-by-2-by-K part of this array must */
    /*             contain a 2-by-2 product (implicitly represented by its K */
    /*             factors) in upper Hessenberg-triangular form. */
    /*             On exit, the leading 2-by-2-by-K part of this array */
    /*             contains modified triangular factors such that their */
    /*             product is diagonal. */
    /*     LDA1    INTEGER */
    /*             The first leading dimension of the array A.  LDA1 >= 2. */
    /*     LDA2    INTEGER */
    /*             The second leading dimension of the array A.  LDA2 >= 2. */
    /*     MACPAR  (input)  DOUBLE PRECISION array, dimension (5) */
    /*             Machine parameters: */
    /*             MACPAR(1)  overflow threshold,         DLAMCH( 'O' ); */
    /*             MACPAR(2)  underflow threshold,        DLAMCH( 'U' ); */
    /*             MACPAR(3)  safe minimum,               DLAMCH( 'S' ); */
    /*             MACPAR(4)  relative machine precision, DLAMCH( 'E' ); */
    /*             MACPAR(5)  base of the machine,        DLAMCH( 'B' ). */
    /*     CV      (output)  DOUBLE PRECISION array, dimension (K) */
    /*             On exit, the first K elements of this array contain the */
    /*             cosines of the Givens rotators. */
    /*     SV      (output)  DOUBLE PRECISION array, dimension (K) */
    /*             On exit, the first K elements of this array contain the */
    /*             sines of the Givens rotators. */
    /*     Workspace */
    /*     DWORK   DOUBLE PRECISION array, dimension 3*(K-1) */
    /*     METHOD */
    /*     The product singular value decomposition of the K-1 */
    /*     triangular factors are computed as described in [1]. */
    /*     REFERENCES */
    /*     [1] Bojanczyk, A. and Van Dooren, P. */
    /*         On propagating orthogonal transformations in a product of 2x2 */
    /*         triangular matrices. */
    /*         In Reichel, Ruttan and Varga: 'Numerical Linear Algebra', */
    /*         pp. 1-9, 1993. */
    /*     CONTRIBUTOR */
    /*     D. Kressner, Technical Univ. Berlin, Germany, June 2001. */
    /*     REVISIONS */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Romania, */
    /*     July 2009, SLICOT Library version of the routine PLAPST. */
    /*     V. Sima, Nov. 2010. */
    /*     KEYWORDS */
    /*     Eigenvalues, orthogonal transformation, singular values, */
    /*     singular value decomposition. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. External Subroutines .. */
    /*     .. Intrinsic Functions .. */
    /*     .. Executable Statements .. */
    /* Parameter adjustments */
    --amap;
    --s;
    a_dim1 = *lda1;
    a_dim2 = *lda2;
    a_offset = a_dim1 * (a_dim2 + 1) + 1;
    a -= a_offset;
    --macpar;
    --cv;
    --sv;
    --dwork;
    /* Function Body */
    rmax = macpar[1];
    rmxs = sqrt(rmax);
    rmin = macpar[2];
    rmns = sqrt(rmin);
    sfmn = macpar[3];
    eps = macpar[4];
    base = macpar[5];
    twos = sqrt(2.);
    /*     Compute the product of the 2-by-2 triangular matrices. */
    pw = 1;
    t11 = 1.;
    t12 = 0.;
    t22 = 1.;
    i__1 = *k;
    for (i__ = 2; i__ <= i__1; ++i__) {
        ai = amap[i__];
        a11 = a[(ai * a_dim2 + 1) * a_dim1 + 1];
        a12 = a[(ai * a_dim2 + 2) * a_dim1 + 1];
        a22 = a[(ai * a_dim2 + 2) * a_dim1 + 2];
        if (s[ai] != *sinv) {
            temp = a11;
            a11 = a22;
            a22 = temp;
            a12 = -a12;
        }
        /*        A and T are scaled so that the elements of the resulting */
        /*        product do not overflow. */
        mx = abs(a11) / rmxs;
        mx2 = abs(t11) / rmxs;
    L10:
        if (mx * mx2 >= 1.) {
            if (mx >= 1.) {
                mx /= base;
                a11 /= base;
                a22 /= base;
                a12 /= base;
            }
            if (mx2 >= 1.) {
                mx2 /= base;
                t11 /= base;
                t22 /= base;
                t12 /= base;
            }
            goto L10;
        }
        mx = abs(a22) / rmxs;
        mx2 = abs(t22) / rmxs;
    L20:
        if (mx * mx2 >= 1.) {
            if (mx >= 1.) {
                mx /= base;
                a11 /= base;
                a22 /= base;
                a12 /= base;
            }
            if (mx2 >= 1.) {
                mx2 /= base;
                t11 /= base;
                t22 /= base;
                t12 /= base;
            }
            goto L20;
        }
        mx = abs(a12) / rmxs;
        mx2 = abs(t11) / rmxs;
    L30:
        if (mx * mx2 >= .5) {
            if (mx >= .5) {
                mx /= base;
                a11 /= base;
                a22 /= base;
                a12 /= base;
            }
            if (mx2 >= .5) {
                mx2 /= base;
                t11 /= base;
                t22 /= base;
                t12 /= base;
            }
            goto L30;
        }
        mx = abs(a22) / rmxs;
        mx2 = abs(t12) / rmxs;
    L40:
        if (mx * mx2 >= .5) {
            if (mx >= .5) {
                mx /= base;
                a11 /= base;
                a22 /= base;
                a12 /= base;
            }
            if (mx2 >= .5) {
                mx2 /= base;
                t11 /= base;
                t22 /= base;
                t12 /= base;
            }
            goto L40;
        }
        /*        Avoid underflow if possible. */
        /* Computing MAX */
        d__1 = abs(a11), d__2 = abs(a22), d__1 = max(d__1, d__2), d__2 = abs(a12);
        mx = max(d__1, d__2);
        /* Computing MAX */
        d__1 = abs(t11), d__2 = abs(t22), d__1 = max(d__1, d__2), d__2 = abs(t12);
        mx2 = max(d__1, d__2);
        if (mx != 0. && mx2 != 0.) {
        L50:
            if (mx <= 1. / rmns && mx2 <= rmns || mx <= rmns && mx2 <= 1. / rmns) {
                if (mx <= mx2) {
                    mx *= base;
                    a11 *= base;
                    a22 *= base;
                    a12 *= base;
                } else {
                    mx2 *= base;
                    t11 *= base;
                    t22 *= base;
                    t12 *= base;
                }
                goto L50;
            }
        }
        t12 = t11 * a12 + t12 * a22;
        t11 *= a11;
        t22 *= a22;
        if (i__ < *k) {
            dwork[pw] = t11;
            dwork[pw + 1] = t12;
            dwork[pw + 2] = t22;
            pw += 3;
        }
        /* L60: */
    }
    /*     Compute the SVD of this product avoiding unnecessary */
    /*     overflow/underflow in the singular values. */
    /* Computing MAX */
    d__4 = (d__1 = t11 / 2., abs(d__1)) + (d__2 = t12 / 2., abs(d__2)),
    d__5 = (d__3 = t22 / 2., abs(d__3));
    temp = max(d__4, d__5);
    if (temp > rmax / (twos * 2.)) {
        temp /= base;
        t11 /= base;
        t12 /= base;
        t22 /= base;
    }
L70:
    if (temp < rmax / (base * 2. * twos) && t11 != 0. && t22 != 0.) {
        scl = 0;
        if (abs(t22) <= twos * rmin) {
            scl = 1;
        } else if (eps * abs(t12) > abs(t22)) {
            if (sqrt((abs(t11))) * sqrt((abs(t22))) <= sqrt(twos) * rmns * sqrt((abs(t12)))) {
                scl = 1;
            }
        } else {
            if (abs(t11) <= twos * rmin * ((d__1 = t12 / t22, abs(d__1)) + 1.)) {
                scl = 1;
            }
        }
        if (scl == 1) {
            temp *= base;
            t11 *= base;
            t12 *= base;
            t22 *= base;
            goto L70;
        }
    }
    dlasv2_(&t11, &t12, &t22, &ssmin, &ssmax, &sr, &cr, &sl, &cl);
    /*     Now, the last transformation is propagated to the front as */
    /*     described in [1]. */
    s11 = t11;
    s22 = t22;
    s12 = t12;
    cv[*k] = cr;
    sv[*k] = sr;
    for (i__ = *k; i__ >= 2; --i__) {
        ai = amap[i__];
        if (s[ai] == *sinv) {
            a11 = a[(ai * a_dim2 + 1) * a_dim1 + 1];
            a12 = a[(ai * a_dim2 + 2) * a_dim1 + 1];
            a22 = a[(ai * a_dim2 + 2) * a_dim1 + 2];
        } else {
            a11 = a[(ai * a_dim2 + 2) * a_dim1 + 2];
            a12 = -a[(ai * a_dim2 + 2) * a_dim1 + 1];
            a22 = a[(ai * a_dim2 + 1) * a_dim1 + 1];
        }
        if (i__ > 2) {
            pw += -3;
            t11 = dwork[pw];
            t12 = dwork[pw + 1];
            t22 = dwork[pw + 2];
            if ((d__1 = sr * cl * s22, abs(d__1)) < (d__2 = sl * cr * s11, abs(d__2))) {
                b11 = t22;
                b22 = t11;
                b12 = -t12;
                cc = cl;
                sc = sl;
            } else {
                b11 = a11;
                b12 = a12;
                b22 = a22;
                cc = cr;
                sc = sr;
            }
            /* Computing MAX */
            d__1 = abs(b11), d__2 = abs(b12), d__1 = max(d__1, d__2), d__2 = abs(b22);
            mx = max(d__1, d__2);
            if (mx > rmax / 2.) {
                b11 /= 2.;
                b22 /= 2.;
                b12 /= 2.;
            }
            d__1 = b11 * cc + b12 * sc;
            d__2 = sc * b22;
            dlartg_(&d__1, &d__2, &cc, &sc, &temp);
        } else {
            cc = cl;
            sc = sl;
        }
        if (abs(sc) < sfmn * abs(a22)) {
            a[(ai * a_dim2 + 1) * a_dim1 + 1] = sc * sr * a22 + cc * (cr * a11 + sr * a12);
        } else {
            a[(ai * a_dim2 + 1) * a_dim1 + 1] = a22 / sc * sr;
        }
        if (abs(sr) < sfmn * abs(a11)) {
            a[(ai * a_dim2 + 2) * a_dim1 + 2] = sc * sr * a11 + cr * (cc * a22 - sc * a12);
        } else {
            a[(ai * a_dim2 + 2) * a_dim1 + 2] = a11 / sr * sc;
        }
        a[(ai * a_dim2 + 2) * a_dim1 + 1] = (a12 * cr - a11 * sr) * cc + a22 * cr * sc;
        if (s[ai] != *sinv) {
            temp = a[(ai * a_dim2 + 1) * a_dim1 + 1];
            a[(ai * a_dim2 + 1) * a_dim1 + 1] = a[(ai * a_dim2 + 2) * a_dim1 + 2];
            a[(ai * a_dim2 + 2) * a_dim1 + 2] = temp;
            a[(ai * a_dim2 + 2) * a_dim1 + 1] = -a[(ai * a_dim2 + 2) * a_dim1 + 1];
        }
        cr = cc;
        sr = sc;
        cv[i__ - 1] = cr;
        sv[i__ - 1] = sr;
        s11 = t11;
        s12 = t12;
        s22 = t22;
        /* L80: */
    }
    cv[1] = cl;
    sv[1] = sl;
    return 0;
    /* *** Last line of MB03BC *** */
} /* mb03bc_ */
