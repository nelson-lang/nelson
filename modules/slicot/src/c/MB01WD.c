/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static doublereal c_b12 = 0.;
static integer c__0 = 0;
static doublereal c_b16 = 1.;
static integer c__1 = 1;

EXPORTSYMBOL /* Subroutine */ int mb01wd_(dico, uplo, trans, hess, n, alpha, beta, r__, ldr, a, lda,
    t, ldt, info, dico_len, uplo_len, trans_len, hess_len) char *dico,
    *uplo, *trans, *hess;
integer* n;
doublereal *alpha, *beta, *r__;
integer* ldr;
doublereal* a;
integer* lda;
doublereal* t;
integer *ldt, *info;
ftnlen dico_len;
ftnlen uplo_len;
ftnlen trans_len;
ftnlen hess_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, r_dim1, r_offset, t_dim1, t_offset, i__1, i__2;
    doublereal d__1;
    /* Local variables */
    static char side[1];
    static integer info2, i__, j;
    extern /* Subroutine */ int mb01yd_(), mb01zd_();
    extern logical lsame_();
    static logical reduc, discr;
    extern /* Subroutine */ int dtrmm_();
    static logical upper;
    extern /* Subroutine */ int dsyrk_(), dlascl_(), dlaset_(), xerbla_();
    static char negtra[1];
    static logical transp;
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
    /*     _ */
    /*     R = alpha*( op( A )'*op( T )'*op( T ) + op( T )'*op( T )*op( A ) ) */
    /*         + beta*R,                                                  (1) */
    /*     if DICO = 'C', or */
    /*     _ */
    /*     R = alpha*( op( A )'*op( T )'*op( T )*op( A ) -  op( T )'*op( T )) */
    /*         + beta*R,                                                  (2) */
    /*                                                             _ */
    /*     if DICO = 'D', where alpha and beta are scalars, R, and R are */
    /*     symmetric matrices, T is a triangular matrix, A is a general or */
    /*     Hessenberg matrix, and op( M ) is one of */
    /*        op( M ) = M   or   op( M ) = M'. */
    /*     The result is overwritten on R. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     DICO    CHARACTER*1 */
    /*             Specifies the formula to be evaluated, as follows: */
    /*             = 'C':  formula (1), "continuous-time" case; */
    /*             = 'D':  formula (2), "discrete-time" case. */
    /*     UPLO    CHARACTER*1 */
    /*             Specifies which triangles of the symmetric matrix R and */
    /*             triangular matrix T are given, as follows: */
    /*             = 'U':  the upper triangular parts of R and T are given; */
    /*             = 'L':  the lower triangular parts of R and T are given; */
    /*     TRANS   CHARACTER*1 */
    /*             Specifies the form of op( M ) to be used, as follows: */
    /*             = 'N':  op( M ) = M; */
    /*             = 'T':  op( M ) = M'; */
    /*             = 'C':  op( M ) = M'. */
    /*     HESS    CHARACTER*1 */
    /*             Specifies the form of the matrix A, as follows: */
    /*             = 'F':  matrix A is full; */
    /*             = 'H':  matrix A is Hessenberg (or Schur), either upper */
    /*                     (if UPLO = 'U'), or lower (if UPLO = 'L'). */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the matrices R, A, and T.  N >= 0. */
    /*     ALPHA   (input) DOUBLE PRECISION */
    /*             The scalar alpha. When alpha is zero then the arrays A */
    /*             and T are not referenced. */
    /*     BETA    (input) DOUBLE PRECISION */
    /*             The scalar beta. When beta is zero then the array R need */
    /*             not be set before entry. */
    /*     R       (input/output) DOUBLE PRECISION array, dimension (LDR,N) */
    /*             On entry with UPLO = 'U', the leading N-by-N upper */
    /*             triangular part of this array must contain the upper */
    /*             triangular part of the symmetric matrix R. */
    /*             On entry with UPLO = 'L', the leading N-by-N lower */
    /*             triangular part of this array must contain the lower */
    /*             triangular part of the symmetric matrix R. */
    /*             On exit, the leading N-by-N upper triangular part (if */
    /*             UPLO = 'U'), or lower triangular part (if UPLO = 'L'), of */
    /*             this array contains the corresponding triangular part of */
    /*                                 _ */
    /*             the computed matrix R. */
    /*     LDR     INTEGER */
    /*             The leading dimension of array R.  LDR >= MAX(1,N). */
    /*     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) */
    /*             On entry, the leading N-by-N part of this array must */
    /*             contain the matrix A. If HESS = 'H' the elements below the */
    /*             first subdiagonal, if UPLO = 'U', or above the first */
    /*             superdiagonal, if UPLO = 'L', need not be set to zero, */
    /*             and are not referenced if DICO = 'D'. */
    /*             On exit, the leading N-by-N part of this array contains */
    /*             the following matrix product */
    /*                alpha*T'*T*A, if TRANS = 'N', or */
    /*                alpha*A*T*T', otherwise, */
    /*             if DICO = 'C', or */
    /*                T*A, if TRANS = 'N', or */
    /*                A*T, otherwise, */
    /*             if DICO = 'D' (and in this case, these products have a */
    /*             Hessenberg form, if HESS = 'H'). */
    /*     LDA     INTEGER */
    /*             The leading dimension of array A.  LDA >= MAX(1,N). */
    /*     T       (input) DOUBLE PRECISION array, dimension (LDT,N) */
    /*             If UPLO = 'U', the leading N-by-N upper triangular part of */
    /*             this array must contain the upper triangular matrix T and */
    /*             the strictly lower triangular part need not be set to zero */
    /*             (and it is not referenced). */
    /*             If UPLO = 'L', the leading N-by-N lower triangular part of */
    /*             this array must contain the lower triangular matrix T and */
    /*             the strictly upper triangular part need not be set to zero */
    /*             (and it is not referenced). */
    /*     LDT     INTEGER */
    /*             The leading dimension of array T.  LDT >= MAX(1,N). */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -k, the k-th argument had an illegal */
    /*                   value. */
    /*     METHOD */
    /*     The matrix expression (1) or (2) is efficiently evaluated taking */
    /*     the structure into account. BLAS 3 operations (DTRMM, DSYRK and */
    /*     their specializations) are used throughout. */
    /*     NUMERICAL ASPECTS */
    /*     If A is a full matrix, the algorithm requires approximately */
    /*      3 */
    /*     N  operations, if DICO = 'C'; */
    /*            3 */
    /*     7/6 x N  operations, if DICO = 'D'. */
    /*     CONTRIBUTORS */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Nov. 2000. */
    /*     REVISIONS */
    /*     - */
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
    t_dim1 = *ldt;
    t_offset = t_dim1 + 1;
    t -= t_offset;
    /* Function Body */
    *info = 0;
    discr = lsame_(dico, "D", 1L, 1L);
    upper = lsame_(uplo, "U", 1L, 1L);
    transp = lsame_(trans, "T", 1L, 1L) || lsame_(trans, "C", 1L, 1L);
    reduc = lsame_(hess, "H", 1L, 1L);
    if (!(discr || lsame_(dico, "C", 1L, 1L))) {
        *info = -1;
    } else if (!(upper || lsame_(uplo, "L", 1L, 1L))) {
        *info = -2;
    } else if (!(transp || lsame_(trans, "N", 1L, 1L))) {
        *info = -3;
    } else if (!(reduc || lsame_(hess, "F", 1L, 1L))) {
        *info = -4;
    } else if (*n < 0) {
        *info = -5;
    } else if (*ldr < max(1, *n)) {
        *info = -9;
    } else if (*lda < max(1, *n)) {
        *info = -11;
    } else if (*ldt < max(1, *n)) {
        *info = -13;
    }
    if (*info != 0) {
        /*        Error return. */
        i__1 = -(*info);
        xerbla_("MB01WD", &i__1, 6L);
        return 0;
    }
    /*     Quick return if possible. */
    if (*n == 0) {
        return 0;
    }
    if (*alpha == 0.) {
        if (*beta == 0.) {
            /*           Special case when both alpha = 0 and beta = 0. */
            dlaset_(uplo, n, n, &c_b12, &c_b12, &r__[r_offset], ldr, 1L);
        } else {
            /*           Special case alpha = 0. */
            if (*beta != 1.) {
                dlascl_(uplo, &c__0, &c__0, &c_b16, beta, n, n, &r__[r_offset], ldr, &info2, 1L);
            }
        }
        return 0;
    }
    /*     General case: alpha <> 0. */
    /*     Compute (in A) T*A, if TRANS = 'N', or */
    /*                    A*T, otherwise. */
    if (transp) {
        *(unsigned char*)side = 'R';
        *(unsigned char*)negtra = 'N';
    } else {
        *(unsigned char*)side = 'L';
        *(unsigned char*)negtra = 'T';
    }
    if (reduc && *n > 2) {
        mb01zd_(side, uplo, "NoTranspose", "Non-unit", n, n, &c__1, &c_b16, &t[t_offset], ldt,
            &a[a_offset], lda, &info2, 1L, 1L, 11L, 8L);
    } else {
        dtrmm_(side, uplo, "NoTranspose", "Non-unit", n, n, &c_b16, &t[t_offset], ldt, &a[a_offset],
            lda, 1L, 1L, 11L, 8L);
    }
    if (!discr) {
        /*        Compute (in A) alpha*T'*T*A, if TRANS = 'N', or */
        /*                       alpha*A*T*T', otherwise. */
        if (reduc && *n > 2) {
            mb01zd_(side, uplo, "Transpose", "Non-unit", n, n, &c__1, alpha, &t[t_offset], ldt,
                &a[a_offset], lda, &info2, 1L, 1L, 9L, 8L);
        } else {
            dtrmm_(side, uplo, "Transpose", "Non-unit", n, n, alpha, &t[t_offset], ldt,
                &a[a_offset], lda, 1L, 1L, 9L, 8L);
        }
        /*        Compute the required triangle of the result, using symmetry. */
        if (upper) {
            if (*beta == 0.) {
                i__1 = *n;
                for (j = 1; j <= i__1; ++j) {
                    i__2 = j;
                    for (i__ = 1; i__ <= i__2; ++i__) {
                        r__[i__ + j * r_dim1] = a[i__ + j * a_dim1] + a[j + i__ * a_dim1];
                        /* L10: */
                    }
                    /* L20: */
                }
            } else {
                i__1 = *n;
                for (j = 1; j <= i__1; ++j) {
                    i__2 = j;
                    for (i__ = 1; i__ <= i__2; ++i__) {
                        r__[i__ + j * r_dim1] = a[i__ + j * a_dim1] + a[j + i__ * a_dim1]
                            + *beta * r__[i__ + j * r_dim1];
                        /* L30: */
                    }
                    /* L40: */
                }
            }
        } else {
            if (*beta == 0.) {
                i__1 = *n;
                for (j = 1; j <= i__1; ++j) {
                    i__2 = *n;
                    for (i__ = j; i__ <= i__2; ++i__) {
                        r__[i__ + j * r_dim1] = a[i__ + j * a_dim1] + a[j + i__ * a_dim1];
                        /* L50: */
                    }
                    /* L60: */
                }
            } else {
                i__1 = *n;
                for (j = 1; j <= i__1; ++j) {
                    i__2 = *n;
                    for (i__ = j; i__ <= i__2; ++i__) {
                        r__[i__ + j * r_dim1] = a[i__ + j * a_dim1] + a[j + i__ * a_dim1]
                            + *beta * r__[i__ + j * r_dim1];
                        /* L70: */
                    }
                    /* L80: */
                }
            }
        }
    } else {
        /*        Compute (in R) alpha*A'*T'*T*A + beta*R, if TRANS = 'N', or */
        /*                       alpha*A*T*T'*A' + beta*R, otherwise. */
        if (reduc && *n > 2) {
            mb01yd_(uplo, negtra, n, n, &c__1, alpha, beta, &a[a_offset], lda, &r__[r_offset], ldr,
                &info2, 1L, 1L);
        } else {
            dsyrk_(uplo, negtra, n, n, alpha, &a[a_offset], lda, beta, &r__[r_offset], ldr, 1L, 1L);
        }
        /*        Compute (in R) -alpha*T'*T + R, if TRANS = 'N', or */
        /*                       -alpha*T*T' + R, otherwise. */
        d__1 = -(*alpha);
        mb01yd_(uplo, negtra, n, n, &c__0, &d__1, &c_b16, &t[t_offset], ldt, &r__[r_offset], ldr,
            &info2, 1L, 1L);
    }
    return 0;
    /* *** Last line of MB01WD *** */
} /* mb01wd_ */
