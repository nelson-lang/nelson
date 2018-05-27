/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static doublereal c_b9 = 1.;
static doublereal c_b11 = 0.;

EXPORTSYMBOL /* Subroutine */ int tb01wd_(
    n, m, p, a, lda, b, ldb, c__, ldc, u, ldu, wr, wi, dwork, ldwork, info) integer *n,
    *m, *p;
doublereal* a;
integer* lda;
doublereal* b;
integer* ldb;
doublereal* c__;
integer* ldc;
doublereal* u;
integer* ldu;
doublereal *wr, *wi, *dwork;
integer *ldwork, *info;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, u_dim1, u_offset, i__1;
    doublereal d__1, d__2;
    /* Local variables */
    static integer sdim, ldwp, i__;
    extern /* Subroutine */ int dgees_(), dgemm_(), dgemv_(), dcopy_();
    static logical bwork[1];
    extern /* Subroutine */ int dlacpy_(), xerbla_();
    extern logical select_();
    static doublereal wrkopt;
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
    /*     To reduce the system state matrix A to an upper real Schur form */
    /*     by using an orthogonal similarity transformation A <-- U'*A*U and */
    /*     to apply the transformation to the matrices B and C: B <-- U'*B */
    /*     and C <-- C*U. */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the original state-space representation, */
    /*             i.e. the order of the matrix A.  N >= 0. */
    /*     M       (input) INTEGER */
    /*             The number of system inputs, or of columns of B.  M >= 0. */
    /*     P       (input) INTEGER */
    /*             The number of system outputs, or of rows of C.  P >= 0. */
    /*     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) */
    /*             On entry, the leading N-by-N part of this array must */
    /*             contain the original state dynamics matrix A. */
    /*             On exit, the leading N-by-N part of this array contains */
    /*             the matrix U' * A * U in real Schur form. The elements */
    /*             below the first subdiagonal are set to zero. */
    /*             Note:  A matrix is in real Schur form if it is upper */
    /*                    quasi-triangular with 1-by-1 and 2-by-2 blocks. */
    /*                    2-by-2 blocks are standardized in the form */
    /*                             [  a  b  ] */
    /*                             [  c  a  ] */
    /*                    where b*c < 0. The eigenvalues of such a block */
    /*                    are a +- sqrt(bc). */
    /*     LDA     INTEGER */
    /*             The leading dimension of array A.  LDA >= MAX(1,N). */
    /*     B       (input/output) DOUBLE PRECISION array, dimension (LDB,M) */
    /*             On entry, the leading N-by-M part of this array must */
    /*             contain the input matrix B. */
    /*             On exit, the leading N-by-M part of this array contains */
    /*             the transformed input matrix U' * B. */
    /*     LDB     INTEGER */
    /*             The leading dimension of array B.  LDB >= MAX(1,N). */
    /*     C       (input/output) DOUBLE PRECISION array, dimension (LDC,N) */
    /*             On entry, the leading P-by-N part of this array must */
    /*             contain the output matrix C. */
    /*             On exit, the leading P-by-N part of this array contains */
    /*             the transformed output matrix C * U. */
    /*     LDC     INTEGER */
    /*             The leading dimension of array C.  LDC >= MAX(1,P). */
    /*     U       (output) DOUBLE PRECISION array, dimension (LDU,N) */
    /*             The leading N-by-N part of this array contains the */
    /*             orthogonal transformation matrix used to reduce A to the */
    /*             real Schur form. The columns of U are the Schur vectors of */
    /*             matrix A. */
    /*     LDU     INTEGER */
    /*             The leading dimension of array U.  LDU >= max(1,N). */
    /*     WR, WI  (output) DOUBLE PRECISION arrays, dimension (N) */
    /*             WR and WI contain the real and imaginary parts, */
    /*             respectively, of the computed eigenvalues of A. The */
    /*             eigenvalues will be in the same order that they appear on */
    /*             the diagonal of the output real Schur form of A. Complex */
    /*             conjugate pairs of eigenvalues will appear consecutively */
    /*             with the eigenvalue having the positive imaginary part */
    /*             first. */
    /*     Workspace */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             On exit, if INFO = 0, DWORK(1) returns the optimal value */
    /*             of LDWORK. */
    /*     LDWORK  INTEGER */
    /*             The dimension of working array DWORK.  LWORK >= 3*N. */
    /*             For optimum performance LDWORK should be larger. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -i, the i-th argument had an illegal */
    /*                   value; */
    /*             > 0:  if INFO = i, the QR algorithm failed to compute */
    /*                   all the eigenvalues; elements i+1:N of WR and WI */
    /*                   contain those eigenvalues which have converged; */
    /*                   U contains the matrix which reduces A to its */
    /*                   partially converged Schur form. */
    /*     METHOD */
    /*     Matrix A is reduced to a real Schur form using an orthogonal */
    /*     similarity transformation A <- U'*A*U. Then, the transformation */
    /*     is applied to the matrices B and C: B <-- U'*B and C <-- C*U. */
    /*     NUMERICAL ASPECTS */
    /*                                     3 */
    /*     The algorithm requires about 10N  floating point operations. */
    /*     CONTRIBUTOR */
    /*     A. Varga, German Aerospace Center, */
    /*     DLR Oberpfaffenhofen, March 1998. */
    /*     Based on the RASP routine SRSFDC. */
    /*     REVISIONS */
    /*     - */
    /*     KEYWORDS */
    /*     Orthogonal transformation, real Schur form, similarity */
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
    u_dim1 = *ldu;
    u_offset = u_dim1 + 1;
    u -= u_offset;
    --wr;
    --wi;
    --dwork;
    /* Function Body */
    *info = 0;
    /*     Check input parameters. */
    if (*n < 0) {
        *info = -1;
    } else if (*m < 0) {
        *info = -2;
    } else if (*p < 0) {
        *info = -3;
    } else if (*lda < max(1, *n)) {
        *info = -5;
    } else if (*ldb < max(1, *n)) {
        *info = -7;
    } else if (*ldc < max(1, *p)) {
        *info = -9;
    } else if (*ldu < max(1, *n)) {
        *info = -11;
    } else if (*ldwork < *n * 3) {
        *info = -15;
    }
    if (*info != 0) {
        /*        Error return. */
        i__1 = -(*info);
        xerbla_("TB01WD", &i__1, 6L);
        return 0;
    }
    /*     Quick return if possible. */
    if (*n == 0) {
        return 0;
    }
    /*     Reduce A to real Schur form using an orthogonal similarity */
    /*     transformation A <- U'*A*U, accumulate the transformation in U */
    /*     and compute the eigenvalues of A in (WR,WI). */
    /*     Workspace:  need   3*N; */
    /*                 prefer larger. */
    dgees_("Vectors", "Not ordered", select_, n, &a[a_offset], lda, &sdim, &wr[1], &wi[1],
        &u[u_offset], ldu, &dwork[1], ldwork, bwork, info, 7L, 11L);
    wrkopt = dwork[1];
    if (*info != 0) {
        return 0;
    }
    /*     Apply the transformation: B <-- U'*B. */
    if (*ldwork < *n * *m) {
        /*        Not enough working space for using DGEMM. */
        i__1 = *m;
        for (i__ = 1; i__ <= i__1; ++i__) {
            dcopy_(n, &b[i__ * b_dim1 + 1], &c__1, &dwork[1], &c__1);
            dgemv_("Transpose", n, n, &c_b9, &u[u_offset], ldu, &dwork[1], &c__1, &c_b11,
                &b[i__ * b_dim1 + 1], &c__1, 9L);
            /* L10: */
        }
    } else {
        dlacpy_("Full", n, m, &b[b_offset], ldb, &dwork[1], n, 4L);
        dgemm_("Transpose", "No transpose", n, m, n, &c_b9, &u[u_offset], ldu, &dwork[1], n, &c_b11,
            &b[b_offset], ldb, 9L, 12L);
        /* Computing MAX */
        d__1 = wrkopt, d__2 = (doublereal)(*n * *m);
        wrkopt = max(d__1, d__2);
    }
    /*     Apply the transformation: C <-- C*U. */
    if (*ldwork < *n * *p) {
        /*        Not enough working space for using DGEMM. */
        i__1 = *p;
        for (i__ = 1; i__ <= i__1; ++i__) {
            dcopy_(n, &c__[i__ + c_dim1], ldc, &dwork[1], &c__1);
            dgemv_("Transpose", n, n, &c_b9, &u[u_offset], ldu, &dwork[1], &c__1, &c_b11,
                &c__[i__ + c_dim1], ldc, 9L);
            /* L20: */
        }
    } else {
        ldwp = max(1, *p);
        dlacpy_("Full", p, n, &c__[c_offset], ldc, &dwork[1], &ldwp, 4L);
        dgemm_("No transpose", "No transpose", p, n, n, &c_b9, &dwork[1], &ldwp, &u[u_offset], ldu,
            &c_b11, &c__[c_offset], ldc, 12L, 12L);
        /* Computing MAX */
        d__1 = wrkopt, d__2 = (doublereal)(*n * *p);
        wrkopt = max(d__1, d__2);
    }
    dwork[1] = wrkopt;
    return 0;
    /* *** Last line of TB01WD *** */
} /* tb01wd_ */
