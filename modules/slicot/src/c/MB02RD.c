/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static doublereal c_b11 = 1.;

EXPORTSYMBOL /* Subroutine */ int mb02rd_(
    trans, n, nrhs, h__, ldh, ipiv, b, ldb, info, trans_len) char* trans;
integer *n, *nrhs;
doublereal* h__;
integer *ldh, *ipiv;
doublereal* b;
integer *ldb, *info;
ftnlen trans_len;
{
    /* System generated locals */
    integer b_dim1, b_offset, h_dim1, h_offset, i__1;
    doublereal d__1;
    /* Local variables */
    static integer j;
    extern logical lsame_();
    extern /* Subroutine */ int dswap_(), daxpy_(), dtrsm_();
    static integer jp;
    extern /* Subroutine */ int xerbla_();
    static logical notran;
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
    /*     To solve a system of linear equations */
    /*        H * X = B  or  H' * X = B */
    /*     with an upper Hessenberg N-by-N matrix H using the LU */
    /*     factorization computed by MB02SD. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     TRANS   CHARACTER*1 */
    /*             Specifies the form of the system of equations: */
    /*             = 'N':  H * X = B  (No transpose) */
    /*             = 'T':  H'* X = B  (Transpose) */
    /*             = 'C':  H'* X = B  (Conjugate transpose = Transpose) */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the matrix H.  N >= 0. */
    /*     NRHS    (input) INTEGER */
    /*             The number of right hand sides, i.e., the number of */
    /*             columns of the matrix B.  NRHS >= 0. */
    /*     H       (input) DOUBLE PRECISION array, dimension (LDH,N) */
    /*             The factors L and U from the factorization H = P*L*U */
    /*             as computed by MB02SD. */
    /*     LDH     INTEGER */
    /*             The leading dimension of the array H.  LDH >= max(1,N). */
    /*     IPIV    (input) INTEGER array, dimension (N) */
    /*             The pivot indices from MB02SD; for 1<=i<=N, row i of the */
    /*             matrix was interchanged with row IPIV(i). */
    /*     B       (input/output) DOUBLE PRECISION array, dimension */
    /*             (LDB,NRHS) */
    /*             On entry, the right hand side matrix B. */
    /*             On exit, the solution matrix X. */
    /*     LDB     INTEGER */
    /*             The leading dimension of the array B.  LDB >= max(1,N). */
    /*     Error Indicator */
    /*     INFO    (output) INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -i, the i-th argument had an illegal */
    /*                   value. */
    /*     METHOD */
    /*     The routine uses the factorization */
    /*        H = P * L * U */
    /*     where P is a permutation matrix, L is lower triangular with unit */
    /*     diagonal elements (and one nonzero subdiagonal), and U is upper */
    /*     triangular. */
    /*     REFERENCES */
    /*     - */
    /*     NUMERICAL ASPECTS */
    /*                                2 */
    /*     The algorithm requires 0( N x NRHS ) operations. */
    /*     CONTRIBUTOR */
    /*     V. Sima, Katholieke Univ. Leuven, Belgium, June 1998. */
    /*     REVISIONS */
    /*     - */
    /*     KEYWORDS */
    /*     Hessenberg form, matrix algebra. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. External Functions .. */
    /*     .. External Subroutines .. */
    /*     .. Intrinsic Functions .. */
    /*     .. Executable Statements .. */
    /*     Test the input parameters. */
    /* Parameter adjustments */
    h_dim1 = *ldh;
    h_offset = h_dim1 + 1;
    h__ -= h_offset;
    --ipiv;
    b_dim1 = *ldb;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    /* Function Body */
    *info = 0;
    notran = lsame_(trans, "N", 1L, 1L);
    if (!notran && !lsame_(trans, "T", 1L, 1L) && !lsame_(trans, "C", 1L, 1L)) {
        *info = -1;
    } else if (*n < 0) {
        *info = -2;
    } else if (*nrhs < 0) {
        *info = -3;
    } else if (*ldh < max(1, *n)) {
        *info = -5;
    } else if (*ldb < max(1, *n)) {
        *info = -8;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("MB02RD", &i__1, 6L);
        return 0;
    }
    /*     Quick return if possible. */
    if (*n == 0 || *nrhs == 0) {
        return 0;
    }
    if (notran) {
        /*        Solve H * X = B. */
        /*        Solve L * X = B, overwriting B with X. */
        /*        L is represented as a product of permutations and unit lower */
        /*        triangular matrices L = P(1) * L(1) * ... * P(n-1) * L(n-1), */
        /*        where each transformation L(i) is a rank-one modification of */
        /*        the identity matrix. */
        i__1 = *n - 1;
        for (j = 1; j <= i__1; ++j) {
            jp = ipiv[j];
            if (jp != j) {
                dswap_(nrhs, &b[jp + b_dim1], ldb, &b[j + b_dim1], ldb);
            }
            d__1 = -h__[j + 1 + j * h_dim1];
            daxpy_(nrhs, &d__1, &b[j + b_dim1], ldb, &b[j + 1 + b_dim1], ldb);
            /* L10: */
        }
        /*        Solve U * X = B, overwriting B with X. */
        dtrsm_("Left", "Upper", "No transpose", "Non-unit", n, nrhs, &c_b11, &h__[h_offset], ldh,
            &b[b_offset], ldb, 4L, 5L, 12L, 8L);
    } else {
        /*        Solve H' * X = B. */
        /*        Solve U' * X = B, overwriting B with X. */
        dtrsm_("Left", "Upper", "Transpose", "Non-unit", n, nrhs, &c_b11, &h__[h_offset], ldh,
            &b[b_offset], ldb, 4L, 5L, 9L, 8L);
        /*        Solve L' * X = B, overwriting B with X. */
        for (j = *n - 1; j >= 1; --j) {
            d__1 = -h__[j + 1 + j * h_dim1];
            daxpy_(nrhs, &d__1, &b[j + 1 + b_dim1], ldb, &b[j + b_dim1], ldb);
            jp = ipiv[j];
            if (jp != j) {
                dswap_(nrhs, &b[jp + b_dim1], ldb, &b[j + b_dim1], ldb);
            }
            /* L20: */
        }
    }
    return 0;
    /* *** Last line of MB02RD *** */
} /* mb02rd_ */
