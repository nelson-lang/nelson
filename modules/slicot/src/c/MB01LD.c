/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static doublereal c_b8 = 0.;
static integer c__0 = 0;
static doublereal c_b12 = 1.;
static integer c__1 = 1;
static doublereal c_b27 = -1.;

EXPORTSYMBOL /* Subroutine */ int mb01ld_(uplo, trans, m, n, alpha, beta, r__, ldr, a, lda, x, ldx,
    dwork, ldwork, info, uplo_len, trans_len) char *uplo,
    *trans;
integer *m, *n;
doublereal *alpha, *beta, *r__;
integer* ldr;
doublereal* a;
integer* lda;
doublereal* x;
integer* ldx;
doublereal* dwork;
integer *ldwork, *info;
ftnlen uplo_len;
ftnlen trans_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, r_dim1, r_offset, x_dim1, x_offset, i__1, i__2;
    /* Local variables */
    static integer i__, j;
    extern /* Subroutine */ int mb01kd_(), dscal_();
    extern logical lsame_();
    extern /* Subroutine */ int dgemv_(), dcopy_(), dtrmm_();
    static logical upper;
    static integer m2;
    extern /* Subroutine */ int dlascl_(), dlacpy_(), dlaset_(), xerbla_();
    static logical ltrans, nottra;
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
    /*     To compute the matrix formula */
    /*        _ */
    /*        R = alpha*R + beta*op( A )*X*op( A )', */
    /*                                                 _ */
    /*     where alpha and beta are scalars, R, X, and R are skew-symmetric */
    /*     matrices, A is a general matrix, and op( A ) is one of */
    /*        op( A ) = A   or   op( A ) = A'. */
    /*     The result is overwritten on R. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     UPLO    CHARACTER*1 */
    /*             Specifies which triangles of the skew-symmetric matrices R */
    /*             and X are given, as follows: */
    /*             = 'U':  the strictly upper triangular part is given; */
    /*             = 'L':  the strictly lower triangular part is given. */
    /*     TRANS   CHARACTER*1 */
    /*             Specifies the form of op( A ) to be used in the matrix */
    /*             multiplication, as follows: */
    /*             = 'N':  op( A ) = A; */
    /*             = 'T':  op( A ) = A'; */
    /*             = 'C':  op( A ) = A'. */
    /*     Input/Output Parameters */
    /*     M       (input) INTEGER           _ */
    /*             The order of the matrices R and R and the number of rows */
    /*             of the matrix op( A ).  M >= 0. */
    /*     N       (input) INTEGER */
    /*             The order of the matrix X and the number of columns of the */
    /*             matrix op( A ).  N >= 0. */
    /*     ALPHA   (input) DOUBLE PRECISION */
    /*             The scalar alpha. When alpha is zero then R need not be */
    /*             set before entry, except when R is identified with X in */
    /*             the call. */
    /*     BETA    (input) DOUBLE PRECISION */
    /*             The scalar beta. When beta is zero or N <= 1, or M <= 1, */
    /*             then A and X are not referenced. */
    /*     R       (input/output) DOUBLE PRECISION array, dimension (LDR,M) */
    /*             On entry with UPLO = 'U', the leading M-by-M strictly */
    /*             upper triangular part of this array must contain the */
    /*             strictly upper triangular part of the skew-symmetric */
    /*             matrix R. The lower triangle is not referenced. */
    /*             On entry with UPLO = 'L', the leading M-by-M strictly */
    /*             lower triangular part of this array must contain the */
    /*             strictly lower triangular part of the skew-symmetric */
    /*             matrix R. The upper triangle is not referenced. */
    /*             On exit, the leading M-by-M strictly upper triangular part */
    /*             (if UPLO = 'U'), or strictly lower triangular part */
    /*             (if UPLO = 'L'), of this array contains the corresponding */
    /*                                                             _ */
    /*             strictly triangular part of the computed matrix R. */
    /*     LDR     INTEGER */
    /*             The leading dimension of the array R.  LDR >= MAX(1,M). */
    /*     A       (input) DOUBLE PRECISION array, dimension (LDA,k) */
    /*             where k is N when TRANS = 'N' and is M when TRANS = 'T' or */
    /*             TRANS = 'C'. */
    /*             On entry with TRANS = 'N', the leading M-by-N part of this */
    /*             array must contain the matrix A. */
    /*             On entry with TRANS = 'T' or TRANS = 'C', the leading */
    /*             N-by-M part of this array must contain the matrix A. */
    /*     LDA     INTEGER */
    /*             The leading dimension of the array A.  LDA >= MAX(1,k), */
    /*             where k is M when TRANS = 'N' and is N when TRANS = 'T' or */
    /*             TRANS = 'C'. */
    /*     X       (input or input/output) DOUBLE PRECISION array, dimension */
    /*             (LDX,K), where K = N, if UPLO = 'U' or  LDWORK >= M*(N-1), */
    /*                  or K = MAX(N,M), if UPLO = 'L' and LDWORK <  M*(N-1). */
    /*             On entry, if UPLO = 'U', the leading N-by-N strictly upper */
    /*             triangular part of this array must contain the strictly */
    /*             upper triangular part of the skew-symmetric matrix X and */
    /*             the lower triangular part of the array is not referenced. */
    /*             On entry, if UPLO = 'L', the leading N-by-N strictly lower */
    /*             triangular part of this array must contain the strictly */
    /*             lower triangular part of the skew-symmetric matrix X and */
    /*             the upper triangular part of the array is not referenced. */
    /*             If LDWORK < M*(N-1), this array is overwritten with the */
    /*             matrix op(A)*X, if UPLO = 'U', or X*op(A)', if UPLO = 'L'. */
    /*     LDX     INTEGER */
    /*             The leading dimension of the array X. */
    /*             LDX >= MAX(1,N),   if UPLO = 'L' or  LDWORK >= M*(N-1); */
    /*             LDX >= MAX(1,N,M), if UPLO = 'U' and LDWORK <  M*(N-1). */
    /*     Workspace */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             This array is not referenced when beta = 0, or M <= 1, or */
    /*             N <= 1. */
    /*     LDWORK  The length of the array DWORK. */
    /*             LDWORK >= N, if  beta <> 0, and M > 0, and N >  1; */
    /*             LDWORK >= 0, if  beta =  0, or  M = 0, or  N <= 1. */
    /*             For optimum performance, LDWORK >= M*(N-1), if  beta <> 0, */
    /*             M > 1, and N > 1. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -k, the k-th argument had an illegal */
    /*                   value. */
    /*     METHOD */
    /*     The matrix expression is efficiently evaluated taking the skew- */
    /*     symmetry into account. If LDWORK >= M*(N-1), a BLAS 3 like */
    /*     implementation is used. Specifically, let X = T - T', with T a */
    /*     strictly upper or strictly lower triangular matrix, defined by */
    /*        T = striu( X ),  if UPLO = 'U', */
    /*        T = stril( X ),  if UPLO = 'L', */
    /*     where striu and stril denote the strictly upper triangular part */
    /*     and strictly lower triangular part of X, respectively. Then, */
    /*        A*X*A' = ( A*T )*A' - A*( A*T )',  for TRANS = 'N', */
    /*        A'*X*A = A'*( T*A ) - ( T*A )'*A,  for TRANS = 'T', or 'C', */
    /*     which involve BLAS 3 operations DTRMM and the skew-symmetric */
    /*     correspondent of DSYR2K (with a Fortran implementation available */
    /*     in the SLICOT Library routine MB01KD). */
    /*     If LDWORK < M*(N-1), a BLAS 2 implementation is used. */
    /*     NUMERICAL ASPECTS */
    /*     The algorithm requires approximately */
    /*                   2         2 */
    /*        3/2 x M x N + 1/2 x M */
    /*     operations. */
    /*     CONTRIBUTORS */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Jan. 2010. */
    /*     Based on the SLICOT Library routine MB01RU and the HAPACK Library */
    /*     routine DSKUPD. */
    /*     REVISIONS */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Oct. 2010. */
    /*     KEYWORDS */
    /*     Elementary matrix operations, matrix algebra, matrix operations. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. External Functions .. */
    /*     .. External Subroutines .. */
    /*     .. Intrinsic Functions .. */
    /*     .. Executable Statements .. */
    /*     Test the input scalar arguments. */
    /* Parameter adjustments */
    r_dim1 = *ldr;
    r_offset = r_dim1 + 1;
    r__ -= r_offset;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    x_dim1 = *ldx;
    x_offset = x_dim1 + 1;
    x -= x_offset;
    --dwork;
    /* Function Body */
    *info = 0;
    upper = lsame_(uplo, "U", 1L, 1L);
    nottra = lsame_(trans, "N", 1L, 1L);
    ltrans = lsame_(trans, "T", 1L, 1L) || lsame_(trans, "C", 1L, 1L);
    if (!upper && !lsame_(uplo, "L", 1L, 1L)) {
        *info = -1;
    } else if (!nottra && !ltrans) {
        *info = -2;
    } else if (*m < 0) {
        *info = -3;
    } else if (*n < 0) {
        *info = -4;
    } else if (*ldr < max(1, *m)) {
        *info = -8;
    } else if (*lda < 1 || ltrans && *lda < *n || nottra && *lda < *m) {
        *info = -10;
    } else if (*ldx < max(1, *n) || *ldx < *m && upper && *ldwork < *m * (*n - 1)) {
        *info = -12;
    } else if (*ldwork < 0 || *beta != 0. && *m > 1 && *n > 1 && *ldwork < *n) {
        *info = -14;
    }
    if (*info != 0) {
        /*        Error return. */
        i__1 = -(*info);
        xerbla_("MB01LD", &i__1, 6L);
        return 0;
    }
    /*     Quick return if possible. */
    if (*m <= 0) {
        return 0;
    }
    m2 = min(2, *m);
    if (*beta == 0. || *n <= 1) {
        if (upper) {
            i__ = 1;
            j = m2;
        } else {
            i__ = m2;
            j = 1;
        }
        if (*alpha == 0.) {
            /*           Special case alpha = 0. */
            i__1 = *m - 1;
            i__2 = *m - 1;
            dlaset_(uplo, &i__1, &i__2, &c_b8, &c_b8, &r__[i__ + j * r_dim1], ldr, 1L);
        } else {
            /*           Special case beta = 0 or N <= 1. */
            if (*alpha != 1.) {
                i__1 = *m - 1;
                i__2 = *m - 1;
                dlascl_(uplo, &c__0, &c__0, &c_b12, alpha, &i__1, &i__2, &r__[i__ + j * r_dim1],
                    ldr, info, 1L);
            }
        }
        return 0;
    }
    /*     General case: beta <> 0. */
    if (*ldwork >= *m * (*n - 1)) {
        /*        Use a BLAS 3 like implementation. */
        /*        Compute W = A*T or W = T*A in DWORK, and apply the updating */
        /*        formula (see METHOD section). Note that column 1 (if */
        /*        UPLO = 'U') or column N (if UPLO = 'L') is zero in the first */
        /*        case, and it is not stored; similarly, row N (if UPLO = 'U') or */
        /*        row 1 (if UPLO = 'L') is zero in the second case, and it is not */
        /*        stored. */
        /*        Workspace: need M*(N-1). */
        if (upper) {
            i__ = 1;
            j = m2;
        } else {
            i__ = m2;
            j = 1;
        }
        if (nottra) {
            i__1 = *n - 1;
            dlacpy_("Full", m, &i__1, &a[i__ * a_dim1 + 1], lda, &dwork[1], m, 4L);
            i__1 = *n - 1;
            dtrmm_("Right", uplo, "NoTranspose", "Non-unit", m, &i__1, &c_b12, &x[i__ + j * x_dim1],
                ldx, &dwork[1], m, 5L, 1L, 11L, 8L);
            i__1 = *n - 1;
            mb01kd_(uplo, trans, m, &i__1, beta, &dwork[1], m, &a[j * a_dim1 + 1], lda, alpha,
                &r__[r_offset], ldr, info, 1L, 1L);
        } else {
            i__1 = *n - 1;
            i__2 = *n - 1;
            dlacpy_("Full", &i__1, m, &a[j + a_dim1], lda, &dwork[1], &i__2, 4L);
            i__1 = *n - 1;
            i__2 = *n - 1;
            dtrmm_("Left", uplo, "NoTranspose", "Non-unit", &i__1, m, &c_b12, &x[i__ + j * x_dim1],
                ldx, &dwork[1], &i__2, 4L, 1L, 11L, 8L);
            i__1 = *n - 1;
            i__2 = *n - 1;
            mb01kd_(uplo, trans, m, &i__1, beta, &a[i__ + a_dim1], lda, &dwork[1], &i__2, alpha,
                &r__[r_offset], ldr, info, 1L, 1L);
        }
    } else {
        /*        Use a BLAS 2 implementation. */
        if (nottra) {
            /*           Compute A*X*A'. */
            if (upper) {
                /*              Compute A*X in X (M-by-N). */
                i__1 = *n - 1;
                for (j = 1; j <= i__1; ++j) {
                    i__2 = j - 1;
                    dcopy_(&i__2, &x[j * x_dim1 + 1], &c__1, &dwork[1], &c__1);
                    dwork[j] = 0.;
                    i__2 = *n - j;
                    dcopy_(&i__2, &x[j + (j + 1) * x_dim1], ldx, &dwork[j + 1], &c__1);
                    i__2 = *n - j;
                    dscal_(&i__2, &c_b27, &dwork[j + 1], &c__1);
                    dgemv_(trans, m, n, &c_b12, &a[a_offset], lda, &dwork[1], &c__1, &c_b8,
                        &x[j * x_dim1 + 1], &c__1, 1L);
                    /* L10: */
                }
                i__1 = *n - 1;
                dcopy_(&i__1, &x[*n * x_dim1 + 1], &c__1, &dwork[1], &c__1);
                i__1 = *n - 1;
                dgemv_(trans, m, &i__1, &c_b12, &a[a_offset], lda, &dwork[1], &c__1, &c_b8,
                    &x[*n * x_dim1 + 1], &c__1, 1L);
                /*              Compute alpha*striu( R ) + beta*striu( X*A' ) in the */
                /*              strictly upper triangular part of R. */
                i__1 = *m - 1;
                for (i__ = 1; i__ <= i__1; ++i__) {
                    dcopy_(n, &x[i__ + x_dim1], ldx, &dwork[1], &c__1);
                    i__2 = *m - i__;
                    dgemv_(trans, &i__2, n, beta, &a[i__ + 1 + a_dim1], lda, &dwork[1], &c__1,
                        alpha, &r__[i__ + (i__ + 1) * r_dim1], ldr, 1L);
                    /* L20: */
                }
            } else {
                /*              Compute X*A' in X (N-by-M). */
                i__1 = *n - 1;
                for (i__ = 1; i__ <= i__1; ++i__) {
                    i__2 = i__ - 1;
                    dcopy_(&i__2, &x[i__ + x_dim1], ldx, &dwork[1], &c__1);
                    dwork[i__] = 0.;
                    i__2 = *n - i__;
                    dcopy_(&i__2, &x[i__ + 1 + i__ * x_dim1], &c__1, &dwork[i__ + 1], &c__1);
                    i__2 = *n - i__;
                    dscal_(&i__2, &c_b27, &dwork[i__ + 1], &c__1);
                    dgemv_(trans, m, n, &c_b12, &a[a_offset], lda, &dwork[1], &c__1, &c_b8,
                        &x[i__ + x_dim1], ldx, 1L);
                    /* L30: */
                }
                i__1 = *n - 1;
                dcopy_(&i__1, &x[*n + x_dim1], ldx, &dwork[1], &c__1);
                i__1 = *n - 1;
                dgemv_(trans, m, &i__1, &c_b12, &a[a_offset], lda, &dwork[1], &c__1, &c_b8,
                    &x[*n + x_dim1], ldx, 1L);
                /*              Compute alpha*stril( R ) + beta*stril( A*X ) in the */
                /*              strictly lower triangular part of R. */
                i__1 = *m - 1;
                for (j = 1; j <= i__1; ++j) {
                    dcopy_(n, &x[j * x_dim1 + 1], &c__1, &dwork[1], &c__1);
                    i__2 = *m - j;
                    dgemv_(trans, &i__2, n, beta, &a[j + 1 + a_dim1], lda, &dwork[1], &c__1, alpha,
                        &r__[j + 1 + j * r_dim1], &c__1, 1L);
                    /* L40: */
                }
            }
        } else {
            /*           Compute A'*X*A. */
            if (upper) {
                /*              Compute A'*X in X (M-by-N). */
                i__1 = *n - 1;
                for (j = 1; j <= i__1; ++j) {
                    i__2 = j - 1;
                    dcopy_(&i__2, &x[j * x_dim1 + 1], &c__1, &dwork[1], &c__1);
                    dwork[j] = 0.;
                    i__2 = *n - j;
                    dcopy_(&i__2, &x[j + (j + 1) * x_dim1], ldx, &dwork[j + 1], &c__1);
                    i__2 = *n - j;
                    dscal_(&i__2, &c_b27, &dwork[j + 1], &c__1);
                    dgemv_(trans, n, m, &c_b12, &a[a_offset], lda, &dwork[1], &c__1, &c_b8,
                        &x[j * x_dim1 + 1], &c__1, 1L);
                    /* L50: */
                }
                i__1 = *n - 1;
                dcopy_(&i__1, &x[*n * x_dim1 + 1], &c__1, &dwork[1], &c__1);
                i__1 = *n - 1;
                dgemv_(trans, &i__1, m, &c_b12, &a[a_offset], lda, &dwork[1], &c__1, &c_b8,
                    &x[*n * x_dim1 + 1], &c__1, 1L);
                /*              Compute alpha*striu( R ) + beta*striu( X*A ) in the */
                /*              strictly upper triangular part of R. */
                i__1 = *m - 1;
                for (i__ = 1; i__ <= i__1; ++i__) {
                    dcopy_(n, &x[i__ + x_dim1], ldx, &dwork[1], &c__1);
                    i__2 = *m - i__;
                    dgemv_(trans, n, &i__2, beta, &a[(i__ + 1) * a_dim1 + 1], lda, &dwork[1], &c__1,
                        alpha, &r__[i__ + (i__ + 1) * r_dim1], ldr, 1L);
                    /* L60: */
                }
            } else {
                /*              Compute X*A in X (N-by-M). */
                i__1 = *n - 1;
                for (i__ = 1; i__ <= i__1; ++i__) {
                    i__2 = i__ - 1;
                    dcopy_(&i__2, &x[i__ + x_dim1], ldx, &dwork[1], &c__1);
                    dwork[i__] = 0.;
                    i__2 = *n - i__;
                    dcopy_(&i__2, &x[i__ + 1 + i__ * x_dim1], &c__1, &dwork[i__ + 1], &c__1);
                    i__2 = *n - i__;
                    dscal_(&i__2, &c_b27, &dwork[i__ + 1], &c__1);
                    dgemv_(trans, n, m, &c_b12, &a[a_offset], lda, &dwork[1], &c__1, &c_b8,
                        &x[i__ + x_dim1], ldx, 1L);
                    /* L70: */
                }
                i__1 = *n - 1;
                dcopy_(&i__1, &x[*n + x_dim1], ldx, &dwork[1], &c__1);
                i__1 = *n - 1;
                dgemv_(trans, &i__1, m, &c_b12, &a[a_offset], lda, &dwork[1], &c__1, &c_b8,
                    &x[*n + x_dim1], ldx, 1L);
                /*              Compute alpha*stril( R ) + beta*stril( A'*X ) in the */
                /*              strictly lower triangular part of R. */
                i__1 = *m - 1;
                for (j = 1; j <= i__1; ++j) {
                    dcopy_(n, &x[j * x_dim1 + 1], &c__1, &dwork[1], &c__1);
                    i__2 = *m - j;
                    dgemv_(trans, n, &i__2, beta, &a[(j + 1) * a_dim1 + 1], lda, &dwork[1], &c__1,
                        alpha, &r__[j + 1 + j * r_dim1], &c__1, 1L);
                    /* L80: */
                }
            }
        }
    }
    return 0;
    /* *** Last line of MB01LD *** */
} /* mb01ld_ */
