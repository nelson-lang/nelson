/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

EXPORTSYMBOL /* Subroutine */ int mb01sd_(jobs, m, n, a, lda, r__, c__, jobs_len) char* jobs;
integer *m, *n;
doublereal* a;
integer* lda;
doublereal *r__, *c__;
ftnlen jobs_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;
    /* Local variables */
    static integer i__, j;
    extern logical lsame_();
    static doublereal cj;
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
    /*     To scale a general M-by-N matrix A using the row and column */
    /*     scaling factors in the vectors R and C. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     JOBS    CHARACTER*1 */
    /*             Specifies the scaling operation to be done, as follows: */
    /*             = 'R':  row scaling, i.e., A will be premultiplied */
    /*                     by diag(R); */
    /*             = 'C':  column scaling, i.e., A will be postmultiplied */
    /*                     by diag(C); */
    /*             = 'B':  both row and column scaling, i.e., A will be */
    /*                     replaced by diag(R) * A * diag(C). */
    /*     Input/Output Parameters */
    /*     M       (input) INTEGER */
    /*             The number of rows of the matrix A.  M >= 0. */
    /*     N       (input) INTEGER */
    /*             The number of columns of the matrix A.  N >= 0. */
    /*     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) */
    /*             On entry, the M-by-N matrix A. */
    /*             On exit, the scaled matrix.  See JOBS for the form of the */
    /*             scaled matrix. */
    /*     LDA     INTEGER */
    /*             The leading dimension of the array A.  LDA >= max(1,M). */
    /*     R       (input) DOUBLE PRECISION array, dimension (M) */
    /*             The row scale factors for A. */
    /*             R is not referenced if JOBS = 'C'. */
    /*     C       (input) DOUBLE PRECISION array, dimension (N) */
    /*             The column scale factors for A. */
    /*             C is not referenced if JOBS = 'R'. */
    /*     CONTRIBUTOR */
    /*     A. Varga, German Aerospace Center, */
    /*     DLR Oberpfaffenhofen, April 1998. */
    /*     Based on the RASP routine DMSCAL. */
    /*    ****************************************************************** */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. External Functions .. */
    /*     .. Executable Statements .. */
    /*     Quick return if possible. */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --r__;
    --c__;
    /* Function Body */
    if (*m == 0 || *n == 0) {
        return 0;
    }
    if (lsame_(jobs, "C", 1L, 1L)) {
        /*        Column scaling, no row scaling. */
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
            cj = c__[j];
            i__2 = *m;
            for (i__ = 1; i__ <= i__2; ++i__) {
                a[i__ + j * a_dim1] = cj * a[i__ + j * a_dim1];
                /* L10: */
            }
            /* L20: */
        }
    } else if (lsame_(jobs, "R", 1L, 1L)) {
        /*        Row scaling, no column scaling. */
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
            i__2 = *m;
            for (i__ = 1; i__ <= i__2; ++i__) {
                a[i__ + j * a_dim1] = r__[i__] * a[i__ + j * a_dim1];
                /* L30: */
            }
            /* L40: */
        }
    } else if (lsame_(jobs, "B", 1L, 1L)) {
        /*        Row and column scaling. */
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
            cj = c__[j];
            i__2 = *m;
            for (i__ = 1; i__ <= i__2; ++i__) {
                a[i__ + j * a_dim1] = cj * r__[i__] * a[i__ + j * a_dim1];
                /* L50: */
            }
            /* L60: */
        }
    }
    return 0;
    /* *** Last line of MB01SD *** */
} /* mb01sd_ */
