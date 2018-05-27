/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

EXPORTSYMBOL /* Subroutine */ int mb01kd_(
    uplo, trans, n, k, alpha, a, lda, b, ldb, beta, c__, ldc, info, uplo_len, trans_len) char *uplo,
    *trans;
integer *n, *k;
doublereal *alpha, *a;
integer* lda;
doublereal* b;
integer* ldb;
doublereal *beta, *c__;
integer *ldc, *info;
ftnlen uplo_len;
ftnlen trans_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, i__1, i__2, i__3;
    /* Local variables */
    static doublereal temp1, temp2;
    static integer i__, j, l;
    extern logical lsame_();
    static logical ltran;
    extern /* Subroutine */ int xerbla_();
    static logical lup;
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
    /*     To perform one of the skew-symmetric rank 2k operations */
    /*         C := alpha*A*B' - alpha*B*A' + beta*C, */
    /*     or */
    /*         C := alpha*A'*B - alpha*B'*A + beta*C, */
    /*     where alpha and beta are scalars, C is a real N-by-N skew- */
    /*     symmetric matrix and A, B are N-by-K matrices in the first case */
    /*     and K-by-N matrices in the second case. */
    /*     This is a modified version of the vanilla implemented BLAS */
    /*     routine DSYR2K written by Jack Dongarra, Iain Duff, */
    /*     Jeremy Du Croz and Sven Hammarling. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     UPLO    CHARACTER*1 */
    /*             Specifies whether the upper or lower triangular part of */
    /*             the array C is to be referenced, as follows: */
    /*             = 'U':  only the strictly upper triangular part of C is to */
    /*                     be referenced; */
    /*             = 'L':  only the striclty lower triangular part of C is to */
    /*                     be referenced. */
    /*     TRANS   CHARACTER*1 */
    /*             Specifies the operation to be performed, as follows: */
    /*             = 'N':         C := alpha*A*B' - alpha*B*A' + beta*C; */
    /*             = 'T' or 'C':  C := alpha*A'*B - alpha*B'*A + beta*C. */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the matrix C.  N >= 0. */
    /*     K       (input) INTEGER */
    /*             If TRANS = 'N' the number of columns of A and B; and if */
    /*             TRANS = 'T' or TRANS = 'C' the number of rows of A and B. */
    /*             K >= 0. */
    /*     ALPHA   (input) DOUBLE PRECISION */
    /*             The scalar alpha. If alpha is zero, or N <= 1, or K = 0, */
    /*             A and B are not referenced. */
    /*     A       (input)  DOUBLE PRECISION array, dimension (LDA,KA), */
    /*             where KA is K when TRANS = 'N', and is N otherwise. */
    /*             On entry with TRANS = 'N', the leading N-by-K part of */
    /*             of this array must contain the matrix A. */
    /*             On entry with TRANS = 'T' or TRANS = 'C', the leading */
    /*             K-by-N part of this array must contain the matrix A. */
    /*     LDA     INTEGER */
    /*             The leading dimension of the array A. */
    /*             LDA >= MAX(1,N),  if TRANS = 'N'; */
    /*             LDA >= MAX(1,K),  if TRANS = 'T' or TRANS = 'C'. */
    /*     B       (input)  DOUBLE PRECISION array, dimension (LDB,KB), */
    /*             where KB is K when TRANS = 'N', and is N otherwise. */
    /*             On entry with TRANS = 'N', the leading N-by-K part of */
    /*             of this array must contain the matrix B. */
    /*             On entry with TRANS = 'T' or TRANS = 'C', the leading */
    /*             K-by-N part of this array must contain the matrix B. */
    /*     LDB     INTEGER */
    /*             The leading dimension of the array B. */
    /*             LDB >= MAX(1,N),  if TRANS = 'N'; */
    /*             LDB >= MAX(1,K),  if TRANS = 'T' or TRANS = 'C'. */
    /*     BETA    (input) DOUBLE PRECISION */
    /*             The scalar beta. If beta is zero C need not be set before */
    /*             entry. */
    /*     C       (input/output)  DOUBLE PRECISION array, dimension (LDC,N) */
    /*             On entry with UPLO = 'U', the leading N-by-N part of this */
    /*             array must contain the strictly upper triangular part of */
    /*             the matrix C. The lower triangular part of this array is */
    /*             not referenced. */
    /*             On entry with UPLO = 'L', the leading N-by-N part of this */
    /*             array must contain the strictly lower triangular part of */
    /*             the matrix C. The upper triangular part of this array is */
    /*             not referenced. */
    /*             On exit with UPLO = 'U', the leading N-by-N part of this */
    /*             array contains the strictly upper triangular part of the */
    /*             updated matrix C. */
    /*             On exit with UPLO = 'L', the leading N-by-N part of this */
    /*             array contains the strictly lower triangular part of the */
    /*             updated matrix C. */
    /*     LDC     INTEGER */
    /*             The leading dimension of the array C.  LDC >= MAX(1,N) */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -i, the i-th argument had an illegal */
    /*                   value. */
    /*     NUMERICAL ASPECTS */
    /*     Though being almost identical with the vanilla implementation */
    /*     of the BLAS routine DSYR2K the performance of this routine could */
    /*     be significantly lower in the case of vendor supplied, highly */
    /*     optimized BLAS. */
    /*     CONTRIBUTORS */
    /*     D. Kressner (Technical Univ. Berlin, Germany) and */
    /*     P. Benner (Technical Univ. Chemnitz, Germany), December 2003. */
    /*     REVISIONS */
    /*     V. Sima, Jan. 2010 (SLICOT version of the HAPACK routine DSKR2K). */
    /*     KEYWORDS */
    /*     Elementary matrix operations, */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. External Functions .. */
    /*     .. External Subroutines .. */
    /*     .. Intrinsic Functions .. */
    /*     .. Executable Statements .. */
    /*     Decode the scalar input parameters. */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    c_dim1 = *ldc;
    c_offset = c_dim1 + 1;
    c__ -= c_offset;
    /* Function Body */
    *info = 0;
    lup = lsame_(uplo, "U", 1L, 1L);
    ltran = lsame_(trans, "T", 1L, 1L) || lsame_(trans, "C", 1L, 1L);
    /*     Check the scalar input parameters. */
    if (!(lup || lsame_(uplo, "L", 1L, 1L))) {
        *info = -1;
    } else if (!(ltran || lsame_(trans, "N", 1L, 1L))) {
        *info = -2;
    } else if (*n < 0) {
        *info = -3;
    } else if (*k < 0) {
        *info = -4;
    } else if (!ltran && *lda < *n || *lda < 1 || ltran && *lda < *k) {
        *info = -7;
    } else if (!ltran && *ldb < *n || *ldb < 1 || ltran && *ldb < *k) {
        *info = -9;
    } else if (*ldc < max(1, *n)) {
        *info = -12;
    }
    /*     Return if there were illegal values. */
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("MB01KD", &i__1, 6L);
        return 0;
    }
    /*     Quick return if possible. */
    if (*n <= 1 || (*alpha == 0. || *k == 0) && *beta == 1.) {
        return 0;
    }
    /*     Special case ALPHA = 0. */
    if (*alpha == 0.) {
        if (lup) {
            if (*beta == 0.) {
                i__1 = *n;
                for (j = 2; j <= i__1; ++j) {
                    i__2 = j - 1;
                    for (i__ = 1; i__ <= i__2; ++i__) {
                        c__[i__ + j * c_dim1] = 0.;
                        /* L10: */
                    }
                    /* L20: */
                }
            } else {
                i__1 = *n;
                for (j = 2; j <= i__1; ++j) {
                    i__2 = j - 1;
                    for (i__ = 1; i__ <= i__2; ++i__) {
                        c__[i__ + j * c_dim1] = *beta * c__[i__ + j * c_dim1];
                        /* L30: */
                    }
                    /* L40: */
                }
            }
        } else {
            if (*beta == 0.) {
                i__1 = *n - 1;
                for (j = 1; j <= i__1; ++j) {
                    i__2 = *n;
                    for (i__ = j + 1; i__ <= i__2; ++i__) {
                        c__[i__ + j * c_dim1] = 0.;
                        /* L50: */
                    }
                    /* L60: */
                }
            } else {
                i__1 = *n - 1;
                for (j = 1; j <= i__1; ++j) {
                    i__2 = *n;
                    for (i__ = j + 1; i__ <= i__2; ++i__) {
                        c__[i__ + j * c_dim1] = *beta * c__[i__ + j * c_dim1];
                        /* L70: */
                    }
                    /* L80: */
                }
            }
        }
        return 0;
    }
    /*     Normal case. */
    if (!ltran) {
        /*        Update C := alpha*A*B' - alpha*B*A' + beta*C. */
        if (lup) {
            i__1 = *n;
            for (j = 2; j <= i__1; ++j) {
                if (*beta == 0.) {
                    i__2 = j - 1;
                    for (i__ = 1; i__ <= i__2; ++i__) {
                        c__[i__ + j * c_dim1] = 0.;
                        /* L90: */
                    }
                } else if (*beta != 1.) {
                    i__2 = j - 1;
                    for (i__ = 1; i__ <= i__2; ++i__) {
                        c__[i__ + j * c_dim1] = *beta * c__[i__ + j * c_dim1];
                        /* L100: */
                    }
                }
                i__2 = *k;
                for (l = 1; l <= i__2; ++l) {
                    if (a[j + l * a_dim1] != 0. || b[j + l * b_dim1] != 0.) {
                        temp1 = *alpha * b[j + l * b_dim1];
                        temp2 = *alpha * a[j + l * a_dim1];
                        i__3 = j - 1;
                        for (i__ = 1; i__ <= i__3; ++i__) {
                            c__[i__ + j * c_dim1] = c__[i__ + j * c_dim1]
                                + a[i__ + l * a_dim1] * temp1 - b[i__ + l * b_dim1] * temp2;
                            /* L110: */
                        }
                    }
                    /* L120: */
                }
                /* L130: */
            }
        } else {
            i__1 = *n - 1;
            for (j = 1; j <= i__1; ++j) {
                if (*beta == 0.) {
                    i__2 = *n;
                    for (i__ = j + 1; i__ <= i__2; ++i__) {
                        c__[i__ + j * c_dim1] = 0.;
                        /* L140: */
                    }
                } else if (*beta != 1.) {
                    i__2 = *n;
                    for (i__ = j + 1; i__ <= i__2; ++i__) {
                        c__[i__ + j * c_dim1] = *beta * c__[i__ + j * c_dim1];
                        /* L150: */
                    }
                }
                i__2 = *k;
                for (l = 1; l <= i__2; ++l) {
                    if (a[j + l * a_dim1] != 0. || b[j + l * b_dim1] != 0.) {
                        temp1 = *alpha * b[j + l * b_dim1];
                        temp2 = *alpha * a[j + l * a_dim1];
                        i__3 = *n;
                        for (i__ = j + 1; i__ <= i__3; ++i__) {
                            c__[i__ + j * c_dim1] = c__[i__ + j * c_dim1]
                                + a[i__ + l * a_dim1] * temp1 - b[i__ + l * b_dim1] * temp2;
                            /* L160: */
                        }
                    }
                    /* L170: */
                }
                /* L180: */
            }
        }
    } else {
        /*        Update C := alpha*A'*B - alpha*B'*A + beta*C. */
        if (lup) {
            i__1 = *n;
            for (j = 2; j <= i__1; ++j) {
                i__2 = j - 1;
                for (i__ = 1; i__ <= i__2; ++i__) {
                    temp1 = 0.;
                    temp2 = 0.;
                    i__3 = *k;
                    for (l = 1; l <= i__3; ++l) {
                        temp1 += a[l + i__ * a_dim1] * b[l + j * b_dim1];
                        temp2 += b[l + i__ * b_dim1] * a[l + j * a_dim1];
                        /* L190: */
                    }
                    if (*beta == 0.) {
                        c__[i__ + j * c_dim1] = *alpha * temp1 - *alpha * temp2;
                    } else {
                        c__[i__ + j * c_dim1]
                            = *beta * c__[i__ + j * c_dim1] + *alpha * temp1 - *alpha * temp2;
                    }
                    /* L200: */
                }
                /* L210: */
            }
        } else {
            i__1 = *n - 1;
            for (j = 1; j <= i__1; ++j) {
                i__2 = *n;
                for (i__ = j + 1; i__ <= i__2; ++i__) {
                    temp1 = 0.;
                    temp2 = 0.;
                    i__3 = *k;
                    for (l = 1; l <= i__3; ++l) {
                        temp1 += a[l + i__ * a_dim1] * b[l + j * b_dim1];
                        temp2 += b[l + i__ * b_dim1] * a[l + j * a_dim1];
                        /* L220: */
                    }
                    if (*beta == 0.) {
                        c__[i__ + j * c_dim1] = *alpha * temp1 - *alpha * temp2;
                    } else {
                        c__[i__ + j * c_dim1]
                            = *beta * c__[i__ + j * c_dim1] + *alpha * temp1 - *alpha * temp2;
                    }
                    /* L230: */
                }
                /* L240: */
            }
        }
    }
    return 0;
    /* *** Last line of MB01KD *** */
} /* mb01kd_ */
