/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

EXPORTSYMBOL /* Subroutine */ int ma02cd_(n, kl, ku, a, lda) integer *n, *kl, *ku;
doublereal* a;
integer* lda;
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    /* Local variables */
    static integer i__;
    extern /* Subroutine */ int dswap_();
    static integer i1, lda1;
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
    /*     To compute the pertranspose of a central band of a square matrix. */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the square matrix A.  N >= 0. */
    /*     KL      (input) INTEGER */
    /*             The number of subdiagonals of A to be pertransposed. */
    /*             0 <= KL <= N-1. */
    /*     KU      (input) INTEGER */
    /*             The number of superdiagonals of A to be pertransposed. */
    /*             0 <= KU <= N-1. */
    /*     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) */
    /*             On entry, the leading N-by-N part of this array must */
    /*             contain a square matrix whose central band formed from */
    /*             the KL subdiagonals, the main diagonal and the KU */
    /*             superdiagonals will be pertransposed. */
    /*             On exit, the leading N-by-N part of this array contains */
    /*             the matrix A with its central band (the KL subdiagonals, */
    /*             the main diagonal and the KU superdiagonals) pertransposed */
    /*             (that is the elements of each antidiagonal appear in */
    /*             reversed order). This is equivalent to forming P*B'*P, */
    /*             where B is the matrix formed from the central band of A */
    /*             and P is a permutation matrix with ones down the secondary */
    /*             diagonal. */
    /*     LDA     INTEGER */
    /*             The leading dimension of the array A.  LDA >= max(1,N). */
    /*     CONTRIBUTOR */
    /*     A. Varga, German Aerospace Center, */
    /*     DLR Oberpfaffenhofen, March 1998. */
    /*     Based on the RASP routine DMPTR. */
    /*     REVISIONS */
    /*     A. Varga, December 2001. */
    /*     V. Sima, March 2004. */
    /*    ****************************************************************** */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. External Subroutines .. */
    /*     .. Intrinsic Functions .. */
    /*     .. Executable Statements .. */
    /*     Quick return if possible. */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    /* Function Body */
    if (*n <= 1) {
        return 0;
    }
    lda1 = *lda + 1;
    /*     Pertranspose the KL subdiagonals. */
    /* Computing MIN */
    i__2 = *kl, i__3 = *n - 2;
    i__1 = min(i__2, i__3);
    for (i__ = 1; i__ <= i__1; ++i__) {
        i1 = (*n - i__) / 2;
        if (i1 > 0) {
            i__2 = -lda1;
            dswap_(&i1, &a[i__ + 1 + a_dim1], &lda1, &a[*n - i1 + 1 + (*n - i1 + 1 - i__) * a_dim1],
                &i__2);
        }
        /* L10: */
    }
    /*     Pertranspose the KU superdiagonals. */
    /* Computing MIN */
    i__2 = *ku, i__3 = *n - 2;
    i__1 = min(i__2, i__3);
    for (i__ = 1; i__ <= i__1; ++i__) {
        i1 = (*n - i__) / 2;
        if (i1 > 0) {
            i__2 = -lda1;
            dswap_(&i1, &a[(i__ + 1) * a_dim1 + 1], &lda1,
                &a[*n - i1 + 1 - i__ + (*n - i1 + 1) * a_dim1], &i__2);
        }
        /* L20: */
    }
    /*     Pertranspose the diagonal. */
    i1 = *n / 2;
    if (i1 > 0) {
        i__1 = -lda1;
        dswap_(&i1, &a[a_dim1 + 1], &lda1, &a[*n - i1 + 1 + (*n - i1 + 1) * a_dim1], &i__1);
    }
    return 0;
    /* *** Last line of MA02CD *** */
} /* ma02cd_ */
