/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static doublereal c_b5 = 1.;

EXPORTSYMBOL /* Subroutine */ int mb03fd_(
    n, prec, a, lda, b, ldb, q1, ldq1, q2, ldq2, dwork, ldwork, info) integer* n;
doublereal *prec, *a;
integer* lda;
doublereal* b;
integer* ldb;
doublereal* q1;
integer* ldq1;
doublereal* q2;
integer* ldq2;
doublereal* dwork;
integer *ldwork, *info;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, q1_dim1, q1_offset, q2_dim1, q2_offset, i__1;
    doublereal d__1, d__2, d__3;
    /* Builtin functions */
    double d_sign(), sqrt();
    /* Local variables */
    static integer idum;
    extern /* Subroutine */ int dgges_();
    static logical compg;
    extern logical sb02ow_();
    static logical bwork[4];
    static doublereal a11, a22, b12, b21, co, si;
    extern /* Subroutine */ int dlartg_();
    static doublereal tmp;
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
    /*     To compute orthogonal matrices Q1 and Q2 for a real 2-by-2 or */
    /*     4-by-4 regular pencil */
    /*                   ( A11  0  )     (  0  B12 ) */
    /*       aA - bB = a (         ) - b (         ),                     (1) */
    /*                   (  0  A22 )     ( B21  0  ) */
    /*     such that Q2' A Q1 is upper triangular, Q2' B Q1 is upper quasi- */
    /*     triangular, and the eigenvalues with negative real parts (if there */
    /*     are any) are allocated on the top. The submatrices A11, A22, and */
    /*     B12 are upper triangular. If B21 is 2-by-2, then all the other */
    /*     blocks are nonsingular and the product */
    /*        -1        -1 */
    /*     A11   B12 A22   B21 has a pair of complex conjugate eigenvalues. */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the input pencil, N = 2 or N = 4. */
    /*     PREC    (input) DOUBLE PRECISION */
    /*             The machine precision, (relative machine precision)*base. */
    /*             See the LAPACK Library routine DLAMCH. */
    /*     A       (input/output) DOUBLE PRECISION array, dimension (LDA, N) */
    /*             On entry, the leading N-by-N part of this array must */
    /*             contain the matrix A of the pencil aA - bB. */
    /*             If N = 2, the diagonal elements only are referenced. */
    /*             On exit, if N = 4, the leading N-by-N part of this array */
    /*             contains the transformed upper triangular matrix of the */
    /*             generalized real Schur form of the pencil aA - bB. */
    /*             If N = 2, this array is unchanged on exit. */
    /*     LDA     INTEGER */
    /*             The leading dimension of the array A.  LDA >= N. */
    /*     B       (input/output) DOUBLE PRECISION array, dimension (LDB, N) */
    /*             On entry, the leading N-by-N part of this array must */
    /*             contain the matrix B of the pencil aA - bB. */
    /*             If N = 2, the anti-diagonal elements only are referenced. */
    /*             On exit, if N = 4, the leading N-by-N part of this array */
    /*             contains the transformed real Schur matrix of the */
    /*             generalized real Schur form of the pencil aA - bB. */
    /*             If N = 2, this array is unchanged on exit. */
    /*     LDB     INTEGER */
    /*             The leading dimension of the array B.  LDB >= N. */
    /*     Q1      (output) DOUBLE PRECISION array, dimension (LDQ1, N) */
    /*             The leading N-by-N part of this array contains the first */
    /*             orthogonal transformation matrix. */
    /*     LDQ1    INTEGER */
    /*             The leading dimension of the array Q1.  LDQ1 >= N. */
    /*     Q2      (output) DOUBLE PRECISION array, dimension (LDQ2, N) */
    /*             The leading N-by-N part of this array contains the second */
    /*             orthogonal transformation matrix. */
    /*     LDQ2    INTEGER */
    /*             The leading dimension of the array Q2.  LDQ2 >= N. */
    /*     Workspace */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             If N = 2, then DWORK is not referenced. */
    /*     LDWORK  INTEGER */
    /*             The dimension of the array DWORK. */
    /*             If N = 4, then LDWORK >= 63. For good performance LDWORK */
    /*             should be generally larger. */
    /*             If N = 2, then LDWORK >= 0. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0: succesful exit; */
    /*             = 1: the QZ iteration failed in the LAPACK routine DGGES; */
    /*             = 2: another error occured during execution of DGGES. */
    /*     METHOD */
    /*     The algorithm uses orthogonal transformations as described on page */
    /*     29 in [2]. */
    /*     REFERENCES */
    /*     [1] Benner, P., Byers, R., Mehrmann, V. and Xu, H. */
    /*         Numerical computation of deflating subspaces of skew- */
    /*         Hamiltonian/Hamiltonian pencils. */
    /*         SIAM J. Matrix Anal. Appl., 24 (1), pp. 165-190, 2002. */
    /*     [2] Benner, P., Byers, R., Losse, P., Mehrmann, V. and Xu, H. */
    /*         Numerical Solution of Real Skew-Hamiltonian/Hamiltonian */
    /*         Eigenproblems. */
    /*         Tech. Rep., Technical University Chemnitz, Germany, */
    /*         Nov. 2007. */
    /*     NUMERICAL ASPECTS */
    /*     The algorithm is numerically backward stable. */
    /*     CONTRIBUTOR */
    /*     Matthias Voigt, Fakultaet fuer Mathematik, Technische Universitaet */
    /*     Chemnitz, October 16, 2008. */
    /*     REVISIONS */
    /*     V. Sima, Aug. 2009 (SLICOT version of the routine MB03FD). */
    /*     V. Sima, Oct. 2009, Nov. 2009, Oct. 2010, Nov. 2010. */
    /*     KEYWORDS */
    /*     Eigenvalue exchange, matrix pencil, upper (quasi-)triangular */
    /*     matrix. */
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
    /*     For efficiency, the input arguments are not tested. */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    q1_dim1 = *ldq1;
    q1_offset = q1_dim1 + 1;
    q1 -= q1_offset;
    q2_dim1 = *ldq2;
    q2_offset = q2_dim1 + 1;
    q2 -= q2_offset;
    --dwork;
    /* Function Body */
    *info = 0;
    /*     Computations. */
    if (*n == 4) {
        i__1 = *ldwork - *n * 3;
        dgges_("Vector Computation", "Vector Computation", "Sorted", sb02ow_, n, &b[b_offset], ldb,
            &a[a_offset], lda, &idum, &dwork[1], &dwork[*n + 1], &dwork[(*n << 1) + 1],
            &q2[q2_offset], ldq2, &q1[q1_offset], ldq1, &dwork[*n * 3 + 1], &i__1, bwork, info, 18L,
            18L, 6L);
        if (*info != 0) {
            if (*info >= 1 && *info <= 4) {
                *info = 1;
            } else if (*info != 6) {
                *info = 2;
            } else {
                *info = 0;
            }
        }
        return 0;
    } else {
        /*        The pencil has infinite eigenvalues. The code decides this when */
        /*        A is (numerically) singular. */
        a11 = (d__1 = a[a_dim1 + 1], abs(d__1));
        a22 = (d__1 = a[(a_dim1 << 1) + 2], abs(d__1));
        b21 = (d__1 = b[b_dim1 + 2], abs(d__1));
        b12 = (d__1 = b[(b_dim1 << 1) + 1], abs(d__1));
        compg = FALSE_;
        if (a11 <= *prec * a22) {
            q1[q1_dim1 + 1] = 1.;
            q1[q1_dim1 + 2] = 0.;
            q1[(q1_dim1 << 1) + 1] = 0.;
            q1[(q1_dim1 << 1) + 2] = 1.;
            q2[q2_dim1 + 1] = 0.;
            q2[q2_dim1 + 2] = 1.;
            q2[(q2_dim1 << 1) + 1] = 1.;
            q2[(q2_dim1 << 1) + 2] = 0.;
        } else if (a22 <= *prec * a11) {
            q1[q1_dim1 + 1] = 0.;
            q1[q1_dim1 + 2] = 1.;
            q1[(q1_dim1 << 1) + 1] = 1.;
            q1[(q1_dim1 << 1) + 2] = 0.;
            q2[q2_dim1 + 1] = 1.;
            q2[q2_dim1 + 2] = 0.;
            q2[(q2_dim1 << 1) + 1] = 0.;
            q2[(q2_dim1 << 1) + 2] = 1.;
        } else {
            compg = TRUE_;
        }
        if (compg) {
            /*           The pencil has a double zero eigenvalue. */
            if (b21 <= *prec * b12) {
                q1[q1_dim1 + 1] = 1.;
                q1[q1_dim1 + 2] = 0.;
                q1[(q1_dim1 << 1) + 1] = 0.;
                q1[(q1_dim1 << 1) + 2] = 1.;
                q2[q2_dim1 + 1] = 1.;
                q2[q2_dim1 + 2] = 0.;
                q2[(q2_dim1 << 1) + 1] = 0.;
                q2[(q2_dim1 << 1) + 2] = 1.;
            } else if (b12 <= *prec * b21) {
                q1[q1_dim1 + 1] = 0.;
                q1[q1_dim1 + 2] = 1.;
                q1[(q1_dim1 << 1) + 1] = 1.;
                q1[(q1_dim1 << 1) + 2] = 0.;
                q2[q2_dim1 + 1] = 0.;
                q2[q2_dim1 + 2] = 1.;
                q2[(q2_dim1 << 1) + 1] = 1.;
                q2[(q2_dim1 << 1) + 2] = 0.;
            } else {
                compg = TRUE_;
            }
        }
        if (compg) {
            /*           The pencil has real eigenvalues. */
            d__2 = a[a_dim1 + 1] * a[(a_dim1 << 1) + 2];
            d__1 = d_sign(&c_b5, &d__2) * sqrt(a22 * b12);
            d__3 = sqrt(a11 * b21);
            dlartg_(&d__1, &d__3, &co, &si, &tmp);
            q1[q1_dim1 + 1] = co;
            q1[q1_dim1 + 2] = -si;
            q1[(q1_dim1 << 1) + 1] = si;
            q1[(q1_dim1 << 1) + 2] = co;
            d__1 = sqrt(a11 * b12);
            d__2 = sqrt(a22 * b21);
            dlartg_(&d__1, &d__2, &co, &si, &tmp);
            q2[q2_dim1 + 1] = co;
            q2[q2_dim1 + 2] = -si;
            q2[(q2_dim1 << 1) + 1] = si;
            q2[(q2_dim1 << 1) + 2] = co;
        }
    }
    return 0;
    /* *** Last line of MB03FD *** */
} /* mb03fd_ */
