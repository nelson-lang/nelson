/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

EXPORTSYMBOL /* Subroutine */ int mb02od_(side, uplo, trans, diag, norm, m, n, alpha, a, lda, b,
    ldb, rcond, tol, iwork, dwork, info, side_len, uplo_len, trans_len, diag_len,
    norm_len) char *side,
    *uplo, *trans, *diag, *norm;
integer *m, *n;
doublereal *alpha, *a;
integer* lda;
doublereal* b;
integer* ldb;
doublereal *rcond, *tol;
integer* iwork;
doublereal* dwork;
integer* info;
ftnlen side_len;
ftnlen uplo_len;
ftnlen trans_len;
ftnlen diag_len;
ftnlen norm_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1;
    /* Local variables */
    static logical lside;
    extern logical lsame_();
    static integer nrowa;
    extern /* Subroutine */ int dtrsm_();
    extern doublereal dlamch_();
    static doublereal toldef;
    extern /* Subroutine */ int xerbla_(), dtrcon_();
    static logical onenrm;
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
    /*     To solve (if well-conditioned) one of the matrix equations */
    /*        op( A )*X = alpha*B,   or   X*op( A ) = alpha*B, */
    /*     where alpha is a scalar, X and B are m-by-n matrices, A is a unit, */
    /*     or non-unit, upper or lower triangular matrix and op( A ) is one */
    /*     of */
    /*        op( A ) = A   or   op( A ) = A'. */
    /*     An estimate of the reciprocal of the condition number of the */
    /*     triangular matrix A, in either the 1-norm or the infinity-norm, is */
    /*     also computed as */
    /*        RCOND = 1 / ( norm(A) * norm(inv(A)) ). */
    /*     and the specified matrix equation is solved only if RCOND is */
    /*     larger than a given tolerance TOL.  In that case, the matrix X is */
    /*     overwritten on B. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     SIDE    CHARACTER*1 */
    /*             Specifies whether op( A ) appears on the left or right */
    /*             of X as follows: */
    /*             = 'L':  op( A )*X = alpha*B; */
    /*             = 'R':  X*op( A ) = alpha*B. */
    /*     UPLO    CHARACTER*1 */
    /*             Specifies whether the matrix A is an upper or lower */
    /*             triangular matrix as follows: */
    /*             = 'U':  A is an upper triangular matrix; */
    /*             = 'L':  A is a lower triangular matrix. */
    /*     TRANS   CHARACTER*1 */
    /*             Specifies the form of op( A ) to be used in the matrix */
    /*             multiplication as follows: */
    /*             = 'N':  op( A ) = A; */
    /*             = 'T':  op( A ) = A'; */
    /*             = 'C':  op( A ) = A'. */
    /*     DIAG    CHARACTER*1 */
    /*             Specifies whether or not A is unit triangular as follows: */
    /*             = 'U':  A is assumed to be unit triangular; */
    /*             = 'N':  A is not assumed to be unit triangular. */
    /*     NORM    CHARACTER*1 */
    /*             Specifies whether the 1-norm condition number or the */
    /*             infinity-norm condition number is required: */
    /*             = '1' or 'O':  1-norm; */
    /*             = 'I':         Infinity-norm. */
    /*     Input/Output Parameters */
    /*     M       (input) INTEGER */
    /*             The number of rows of B.  M >= 0. */
    /*     N       (input) INTEGER */
    /*             The number of columns of B.  N >= 0. */
    /*     ALPHA   (input) DOUBLE PRECISION */
    /*             The scalar  alpha. When alpha is zero then A is not */
    /*             referenced and B need not be set before entry. */
    /*     A       (input) DOUBLE PRECISION array, dimension (LDA,k), */
    /*             where k is M when SIDE = 'L' and is N when SIDE = 'R'. */
    /*             On entry with UPLO = 'U', the leading k-by-k upper */
    /*             triangular part of this array must contain the upper */
    /*             triangular matrix and the strictly lower triangular part */
    /*             of A is not referenced. */
    /*             On entry with UPLO = 'L', the leading k-by-k lower */
    /*             triangular part of this array must contain the lower */
    /*             triangular matrix and the strictly upper triangular part */
    /*             of A is not referenced. */
    /*             Note that when DIAG = 'U', the diagonal elements of A are */
    /*             not referenced either, but are assumed to be unity. */
    /*     LDA     INTEGER */
    /*             The leading dimension of array A. */
    /*             LDA >= max(1,M) when SIDE = 'L'; */
    /*             LDA >= max(1,N) when SIDE = 'R'. */
    /*     B       (input/output) DOUBLE PRECISION array, dimension (LDB,N) */
    /*             On entry, the leading M-by-N part of this array must */
    /*             contain the right-hand side matrix B. */
    /*             On exit, if INFO = 0, the leading M-by-N part of this */
    /*             array contains the solution matrix X. */
    /*             Otherwise, this array is not modified by the routine. */
    /*     LDB     INTEGER */
    /*             The leading dimension of array B.  LDB >= max(1,M). */
    /*     RCOND   (output) DOUBLE PRECISION */
    /*             The reciprocal of the condition number of the matrix A, */
    /*             computed as RCOND = 1/(norm(A) * norm(inv(A))). */
    /*     Tolerances */
    /*     TOL     DOUBLE PRECISION */
    /*             The tolerance to be used to test for near singularity of */
    /*             the matrix A. If the user sets TOL > 0, then the given */
    /*             value of TOL is used as a lower bound for the reciprocal */
    /*             condition number of that matrix; a matrix whose estimated */
    /*             condition number is less than 1/TOL is considered to be */
    /*             nonsingular. If the user sets TOL <= 0, then an implicitly */
    /*             computed, default tolerance, defined by TOLDEF = k*k*EPS, */
    /*             is used instead, where EPS is the machine precision (see */
    /*             LAPACK Library routine DLAMCH). */
    /*     Workspace */
    /*     IWORK   INTEGER array, dimension (k) */
    /*     DWORK   DOUBLE PRECISION array, dimension (3*k) */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -i, the i-th argument had an illegal */
    /*                   value; */
    /*             = 1:  the matrix A is numerically singular, i.e. the */
    /*                   condition number estimate of A (in the specified */
    /*                   norm) exceeds 1/TOL. */
    /*     METHOD */
    /*     An estimate of the reciprocal of the condition number of the */
    /*     triangular matrix A (in the specified norm) is computed, and if */
    /*     this estimate is larger then the given (or default) tolerance, */
    /*     the specified matrix equation is solved using Level 3 BLAS */
    /*     routine DTRSM. */
    /*     REFERENCES */
    /*     None. */
    /*     NUMERICAL ASPECTS */
    /*                             2 */
    /*     The algorithm requires k N/2 operations. */
    /*     CONTRIBUTORS */
    /*     V. Sima, Katholieke Univ. Leuven, Belgium, Feb. 1997. */
    /*     REVISIONS */
    /*     February 20, 1998. */
    /*     KEYWORDS */
    /*     Condition number, matrix algebra, matrix operations. */
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
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    --iwork;
    --dwork;
    /* Function Body */
    lside = lsame_(side, "L", 1L, 1L);
    if (lside) {
        nrowa = *m;
    } else {
        nrowa = *n;
    }
    onenrm = *(unsigned char*)norm == '1' || lsame_(norm, "O", 1L, 1L);
    /*     Test the input scalar arguments. */
    *info = 0;
    if (!lside && !lsame_(side, "R", 1L, 1L)) {
        *info = -1;
    } else if (!lsame_(uplo, "U", 1L, 1L) && !lsame_(uplo, "L", 1L, 1L)) {
        *info = -2;
    } else if (!lsame_(trans, "N", 1L, 1L) && !lsame_(trans, "T", 1L, 1L)
        && !lsame_(trans, "C", 1L, 1L)) {
        *info = -3;
    } else if (!lsame_(diag, "U", 1L, 1L) && !lsame_(diag, "N", 1L, 1L)) {
        *info = -4;
    } else if (!onenrm && !lsame_(norm, "I", 1L, 1L)) {
        *info = -5;
    } else if (*m < 0) {
        *info = -6;
    } else if (*n < 0) {
        *info = -7;
    } else if (*lda < max(1, nrowa)) {
        *info = -10;
    } else if (*ldb < max(1, *m)) {
        *info = -12;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("MB02OD", &i__1, 6L);
        return 0;
    }
    /*     Quick return if possible. */
    if (nrowa == 0) {
        *rcond = 1.;
        return 0;
    }
    toldef = *tol;
    if (toldef <= 0.) {
        toldef = (doublereal)(nrowa * nrowa) * dlamch_("Epsilon", 7L);
    }
    dtrcon_(
        norm, uplo, diag, &nrowa, &a[a_offset], lda, rcond, &dwork[1], &iwork[1], info, 1L, 1L, 1L);
    if (*rcond > toldef) {
        dtrsm_(side, uplo, trans, diag, m, n, alpha, &a[a_offset], lda, &b[b_offset], ldb, 1L, 1L,
            1L, 1L);
    } else {
        *info = 1;
    }
    /* *** Last line of MB02OD *** */
} /* mb02od_ */
