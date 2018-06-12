/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__1 = 1;

EXPORTSYMBOL /* Subroutine */ int mb03be_(k, amap, s, sinv, a, lda1, lda2) integer *k, *amap, *s,
    *sinv;
doublereal* a;
integer *lda1, *lda2;
{
    /* System generated locals */
    integer a_dim1, a_dim2, a_offset;
    /* Local variables */
    static doublereal temp;
    extern /* Subroutine */ int drot_(), mb03ad_();
    static integer i__, l, ai;
    static doublereal cs, ct, sn, st;
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
    /*     To apply 10 iterations of a real single shifted periodic QZ */
    /*     algorithm to the 2-by-2 product of matrices stored in the array A. */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     K       (input)  INTEGER */
    /*             The number of factors.  K >= 1. */
    /*     AMAP    (input)  INTEGER array, dimension (K) */
    /*             The map for accessing the factors, i.e., if AMAP(I) = J, */
    /*             then the factor A_I is stored at the J-th position in A. */
    /*     S       (input)  INTEGER array, dimension (K) */
    /*             The signature array. Each entry of S must be 1 or -1. */
    /*     SINV    (input)  INTEGER */
    /*             Signature multiplier. Entries of S are virtually */
    /*             multiplied by SINV. */
    /*     A       (input/output)  DOUBLE PRECISION array, dimension */
    /*                             (LDA1,LDA2,K) */
    /*             On entry, the leading 2-by-2-by-K part of this array must */
    /*             contain a 2-by-2 product (implicitly represented by its K */
    /*             factors) in upper Hessenberg form. */
    /*             On exit, the leading 2-by-2-by-K part of this array */
    /*             contains the product after 10 iterations of a real shifted */
    /*             periodic QZ algorithm. */
    /*     LDA1    INTEGER */
    /*             The first leading dimension of the array A.  LDA1 >= 2. */
    /*     LDA2    INTEGER */
    /*             The second leading dimension of the array A.  LDA2 >= 2. */
    /*     METHOD */
    /*     Ten iterations of a real single shifted periodic QZ algorithm are */
    /*     applied to the 2-by-2 matrix product A. */
    /*     CONTRIBUTOR */
    /*     D. Kressner, Technical Univ. Berlin, Germany, June 2001. */
    /*     REVISIONS */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Romania, */
    /*     July 2009, SLICOT Library version of the routine PLARL2. */
    /*     KEYWORDS */
    /*     Eigenvalues, QZ algorithm, periodic QZ algorithm, orthogonal */
    /*     transformation. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. External Subroutines .. */
    /*     .. Executable Statements .. */
    /* Parameter adjustments */
    --amap;
    --s;
    a_dim1 = *lda1;
    a_dim2 = *lda2;
    a_offset = a_dim1 * (a_dim2 + 1) + 1;
    a -= a_offset;
    /* Function Body */
    for (i__ = 1; i__ <= 10; ++i__) {
        mb03ad_("Single", k, &c__2, &amap[1], &s[1], sinv, &a[a_offset], lda1, lda2, &cs, &sn, &ct,
            &st, 6L);
        ai = amap[1];
        drot_(&c__2, &a[(ai * a_dim2 + 1) * a_dim1 + 1], lda1, &a[(ai * a_dim2 + 1) * a_dim1 + 2],
            lda1, &cs, &sn);
        for (l = *k; l >= 2; --l) {
            ai = amap[l];
            if (s[ai] == *sinv) {
                drot_(&c__2, &a[(ai * a_dim2 + 1) * a_dim1 + 1], &c__1,
                    &a[(ai * a_dim2 + 2) * a_dim1 + 1], &c__1, &cs, &sn);
                temp = a[(ai * a_dim2 + 1) * a_dim1 + 1];
                dlartg_(&temp, &a[(ai * a_dim2 + 1) * a_dim1 + 2], &cs, &sn,
                    &a[(ai * a_dim2 + 1) * a_dim1 + 1]);
                a[(ai * a_dim2 + 1) * a_dim1 + 2] = 0.;
                temp = cs * a[(ai * a_dim2 + 2) * a_dim1 + 1]
                    + sn * a[(ai * a_dim2 + 2) * a_dim1 + 2];
                a[(ai * a_dim2 + 2) * a_dim1 + 2] = cs * a[(ai * a_dim2 + 2) * a_dim1 + 2]
                    - sn * a[(ai * a_dim2 + 2) * a_dim1 + 1];
                a[(ai * a_dim2 + 2) * a_dim1 + 1] = temp;
            } else {
                drot_(&c__2, &a[(ai * a_dim2 + 1) * a_dim1 + 1], lda1,
                    &a[(ai * a_dim2 + 1) * a_dim1 + 2], lda1, &cs, &sn);
                temp = a[(ai * a_dim2 + 2) * a_dim1 + 2];
                dlartg_(&temp, &a[(ai * a_dim2 + 1) * a_dim1 + 2], &cs, &sn,
                    &a[(ai * a_dim2 + 2) * a_dim1 + 2]);
                a[(ai * a_dim2 + 1) * a_dim1 + 2] = 0.;
                sn = -sn;
                temp = cs * a[(ai * a_dim2 + 1) * a_dim1 + 1]
                    + sn * a[(ai * a_dim2 + 2) * a_dim1 + 1];
                a[(ai * a_dim2 + 2) * a_dim1 + 1] = cs * a[(ai * a_dim2 + 2) * a_dim1 + 1]
                    - sn * a[(ai * a_dim2 + 1) * a_dim1 + 1];
                a[(ai * a_dim2 + 1) * a_dim1 + 1] = temp;
            }
            /* L10: */
        }
        ai = amap[1];
        drot_(&c__2, &a[(ai * a_dim2 + 1) * a_dim1 + 1], &c__1, &a[(ai * a_dim2 + 2) * a_dim1 + 1],
            &c__1, &cs, &sn);
        /* L20: */
    }
    return 0;
    /* *** Last line of MB03BE *** */
} /* mb03be_ */
