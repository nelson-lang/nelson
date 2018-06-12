/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

EXPORTSYMBOL /* Subroutine */ int mb03ad_(
    shft, k, n, amap, s, sinv, a, lda1, lda2, c1, s1, c2, s2, shft_len) char* shft;
integer *k, *n, *amap, *s, *sinv;
doublereal* a;
integer *lda1, *lda2;
doublereal *c1, *s1, *c2, *s2;
ftnlen shft_len;
{
    /* System generated locals */
    integer a_dim1, a_dim2, a_offset;
    /* Builtin functions */
    double sqrt();
    /* Local variables */
    static doublereal beta;
    static logical sgle;
    static doublereal temp;
    static integer i__;
    static doublereal gamma, alpha, delta;
    extern logical lsame_();
    static doublereal c3, s3;
    static integer ai;
    extern /* Subroutine */ int dlartg_();
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
    /*     To compute two Givens rotations (C1,S1) and (C2,S2) */
    /*     such that the orthogonal matrix */
    /*                [  C1  S1  0 ]   [ 1  0   0  ] */
    /*           Q =  [ -S1  C1  0 ] * [ 0  C2  S2 ] */
    /*                [  0   0   1 ]   [ 0 -S2  C2 ] */
    /*     makes the first column of the real Wilkinson single/double shift */
    /*     polynomial of the general product of matrices, stored in the */
    /*     array A, parallel to the first unit vector. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     SHFT    CHARACTER*1 */
    /*             Specifies the number of shifts employed by the shift */
    /*             polynomial, as follows: */
    /*             = 'D':  two real shifts; */
    /*             = 'S':  one real shift. */
    /*     Input/Output Parameters */
    /*     K       (input)  INTEGER */
    /*             The number of factors.  K >= 1. */
    /*     N       (input)  INTEGER */
    /*             The order of the factors in the array A.  N >= 3. */
    /*     AMAP    (input) INTEGER array, dimension (K) */
    /*             The map for accessing the factors, i.e., if AMAP(I) = J, */
    /*             then the factor A_I is stored at the J-th position in A. */
    /*     S       (input)  INTEGER array, dimension (K) */
    /*             The signature array. Each entry of S must be 1 or -1. */
    /*     SINV    (input) INTEGER */
    /*             Signature multiplier. Entries of S are virtually */
    /*             multiplied by SINV. */
    /*     A       (input)  DOUBLE PRECISION array, dimension (LDA1,LDA2,K) */
    /*             On entry, the leading N-by-N-by-K part of this array must */
    /*             contain a n-by-n product (implicitly represented by its K */
    /*             factors) in upper Hessenberg form. */
    /*     LDA1    INTEGER */
    /*             The first leading dimension of the array A.  LDA1 >= N. */
    /*     LDA2    INTEGER */
    /*             The second leading dimension of the array A.  LDA2 >= N. */
    /*     C1      (output)  DOUBLE PRECISION */
    /*     S1      (output)  DOUBLE PRECISION */
    /*             On exit, C1 and S1 contain the parameters for the first */
    /*             Givens rotation. */
    /*     C2      (output)  DOUBLE PRECISION */
    /*     S2      (output)  DOUBLE PRECISION */
    /*             On exit, if SHFT = 'D', C2 and S2 contain the parameters */
    /*             for the second Givens rotation. */
    /*     METHOD */
    /*     Two Givens rotations are properly computed and applied. */
    /*     CONTRIBUTOR */
    /*     D. Kressner, Technical Univ. Berlin, Germany, June 2001. */
    /*     REVISIONS */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Romania, */
    /*     July 2009, SLICOT Library version of the routine PLASHF. */
    /*     KEYWORDS */
    /*     Eigenvalues, QZ algorithm, periodic QZ algorithm, orthogonal */
    /*     transformation. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. External Functions .. */
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
    /* Function Body */
    sgle = lsame_(shft, "S", 1L, 1L);
    *c1 = 1.;
    *s1 = 0.;
    *c2 = 1 / sqrt(2.);
    *s2 = *c2;
    for (i__ = *k; i__ >= 2; --i__) {
        ai = amap[i__];
        if (s[ai] == *sinv) {
            alpha = *c2 * a[(ai * a_dim2 + 1) * a_dim1 + 1];
            gamma = *s2 * a[*n + (*n + ai * a_dim2) * a_dim1];
            beta = *s2 * a[*n - 1 + (*n + ai * a_dim2) * a_dim1];
            beta = *c1 * beta + *s1 * a[*n - 1 + (*n - 1 + ai * a_dim2) * a_dim1];
            dlartg_(&alpha, &gamma, c2, s2, &temp);
            temp = *c1 * temp;
            dlartg_(&temp, &beta, c1, s1, &alpha);
        } else {
            temp = a[(ai * a_dim2 + 1) * a_dim1 + 1];
            beta = *s2 * temp;
            temp = *c2 * temp;
            alpha = *s1 * temp;
            gamma = a[*n + (*n + ai * a_dim2) * a_dim1];
            delta = *c2 * gamma;
            gamma = *s2 * gamma;
            dlartg_(&delta, &beta, c2, s2, &c3);
            delta = *c1 * a[*n - 1 + (*n + ai * a_dim2) * a_dim1] - *s1 * gamma;
            alpha = *c2 * alpha - *s2 * delta;
            gamma = *c1 * a[*n - 1 + (*n - 1 + ai * a_dim2) * a_dim1];
            dlartg_(&gamma, &alpha, c1, s1, &temp);
        }
        /* L10: */
    }
    ai = amap[1];
    alpha = a[(ai * a_dim2 + 1) * a_dim1 + 1] * *c2 - a[*n + (*n + ai * a_dim2) * a_dim1] * *s2;
    beta = *c1 * (*c2 * a[(ai * a_dim2 + 1) * a_dim1 + 2]);
    gamma = *c1 * (*s2 * a[*n - 1 + (*n + ai * a_dim2) * a_dim1])
        + *s1 * a[*n - 1 + (*n - 1 + ai * a_dim2) * a_dim1];
    alpha = alpha * *c1 - a[*n + (*n - 1 + ai * a_dim2) * a_dim1] * *s1;
    dlartg_(&alpha, &beta, c1, s1, &temp);
    /*     This is sufficient for single real shifts. */
    if (!sgle) {
        dlartg_(&temp, &gamma, c2, s2, &alpha);
        /*        Rotation 1 is preserved. */
        alpha = *c2;
        gamma = a[*n - 1 + (*n - 1 + ai * a_dim2) * a_dim1] * *c1 * *c2
            + a[*n + (*n - 1 + ai * a_dim2) * a_dim1] * *s2;
        delta = a[*n - 1 + (*n - 1 + ai * a_dim2) * a_dim1] * *s1 * *c2;
        dlartg_(&gamma, &delta, &c3, &s3, &temp);
        dlartg_(&alpha, &temp, c2, s2, &alpha);
        /*        Rotation 3 is preserved throughout the following complete loop. */
        for (i__ = *k; i__ >= 2; --i__) {
            ai = amap[i__];
            if (s[ai] == *sinv) {
                alpha = (a[(ai * a_dim2 + 1) * a_dim1 + 1] * *c1
                            + a[(ai * a_dim2 + 2) * a_dim1 + 1] * *s1)
                    * *c2;
                beta = a[(ai * a_dim2 + 2) * a_dim1 + 2] * *s1 * *c2;
                gamma = a[*n - 1 + (*n - 1 + ai * a_dim2) * a_dim1] * *s2;
                dlartg_(&alpha, &beta, c1, s1, &temp);
                dlartg_(&temp, &gamma, c2, s2, &alpha);
            } else {
                alpha = *c1 * a[(ai * a_dim2 + 1) * a_dim1 + 1];
                gamma = *s1 * a[(ai * a_dim2 + 1) * a_dim1 + 1];
                beta = *c1 * a[(ai * a_dim2 + 2) * a_dim1 + 1]
                    + *s1 * a[(ai * a_dim2 + 2) * a_dim1 + 2];
                delta = -(*s1) * a[(ai * a_dim2 + 2) * a_dim1 + 1]
                    + *c1 * a[(ai * a_dim2 + 2) * a_dim1 + 2];
                dlartg_(&delta, &gamma, c1, s1, &temp);
                alpha = -alpha * *s2;
                beta = -beta * *s2;
                alpha = *c1 * alpha + *s1 * beta;
                beta = *c2 * a[*n - 1 + (*n - 1 + ai * a_dim2) * a_dim1];
                dlartg_(&beta, &alpha, c2, s2, &temp);
                *s2 = -(*s2);
            }
            /* L20: */
        }
        /*        Last step: Let the rotations collap into A. */
        ai = amap[1];
        alpha = *c1 * a[(ai * a_dim2 + 1) * a_dim1 + 1] + *s1 * a[(ai * a_dim2 + 2) * a_dim1 + 1];
        beta = *c1 * a[(ai * a_dim2 + 1) * a_dim1 + 2] + *s1 * a[(ai * a_dim2 + 2) * a_dim1 + 2];
        gamma = *s1 * a[(ai * a_dim2 + 2) * a_dim1 + 3];
        alpha = *c2 * alpha - *s2 * c3;
        beta = *c2 * beta - *s2 * s3;
        gamma = *c2 * gamma;
        dlartg_(&beta, &gamma, c2, s2, &temp);
        dlartg_(&alpha, &temp, c1, s1, &beta);
    }
    return 0;
    /* *** Last line of MB03AD *** */
} /* mb03ad_ */
