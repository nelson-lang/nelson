/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c__16 = 16;
static integer c__0 = 0;
static integer c__1 = 1;
static integer c__2 = 2;
static integer c__4 = 4;
static doublereal c_b11 = 1.;

EXPORTSYMBOL /* Subroutine */ int mb03ed_(n, prec, a, lda, b, ldb, d__, ldd, q1, ldq1, q2, ldq2, q3,
    ldq3, dwork, ldwork, info) integer* n;
doublereal *prec, *a;
integer* lda;
doublereal* b;
integer* ldb;
doublereal* d__;
integer* ldd;
doublereal* q1;
integer* ldq1;
doublereal* q2;
integer* ldq2;
doublereal* q3;
integer* ldq3;
doublereal* dwork;
integer *ldwork, *info;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, d_dim1, d_offset, q1_dim1, q1_offset, q2_dim1,
        q2_offset, q3_dim1, q3_offset, i__1;
    doublereal d__1, d__2, d__3;
    /* Builtin functions */
    double d_sign(), sqrt();
    /* Local variables */
    static integer idum, ievs, iwrk;
    extern /* Subroutine */ int dgges_();
    static logical compg;
    extern logical sb02ow_();
    extern /* Subroutine */ int dcopy_(), dtrmm_();
    static logical bwork[4];
    extern /* Subroutine */ int dgeqr2_(), dorg2r_();
    static doublereal a11, b11, a22, b22, d12, d21, co, si;
    extern /* Subroutine */ int dlacpy_(), dlartg_();
    static doublereal dum[1], tmp;
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
    /*     To compute orthogonal matrices Q1, Q2, Q3 for a real 2-by-2 or */
    /*     4-by-4 regular pencil */
    /*                    ( A11  0  ) ( B11  0  )     (  0  D12 ) */
    /*       aAB - bD = a (         ) (         ) - b (         ),        (1) */
    /*                    (  0  A22 ) (  0  B22 )     ( D21  0  ) */
    /*     such that Q3' A Q2 and Q2' B Q1 are upper triangular, Q3' D Q1 is */
    /*     upper quasi-triangular, and the eigenvalues with negative real */
    /*     parts (if there are any) are allocated on the top. The submatrices */
    /*     A11, A22, B11, B22 and D12 are upper triangular. If D21 is 2-by-2, */
    /*     then all other blocks are nonsingular and the product */
    /*        -1        -1    -1        -1 */
    /*     A22   D21 B11   A11   D12 B22   has a pair of complex conjugate */
    /*     eigenvalues. */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the input pencil, N = 2 or N = 4. */
    /*     PREC    (input) DOUBLE PRECISION */
    /*             The machine precision, (relative machine precision)*base. */
    /*             See the LAPACK Library routine DLAMCH. */
    /*     A       (input) DOUBLE PRECISION array, dimension (LDA, N) */
    /*             The leading N-by-N upper triangular part of this array */
    /*             must contain the upper triangular matrix A of the pencil */
    /*             aAB - bD. The strictly lower triangular part and the */
    /*             entries of the (1,2) block are not referenced. */
    /*     LDA     INTEGER */
    /*             The leading dimension of the array A.  LDA >= N. */
    /*     B       (input) DOUBLE PRECISION array, dimension (LDB, N) */
    /*             The leading N-by-N upper triangular part of this array */
    /*             must contain the upper triangular matrix B of the pencil */
    /*             aAB - bD. The strictly lower triangular part and the */
    /*             entries of the (1,2) block are not referenced. */
    /*     LDB     INTEGER */
    /*             The leading dimension of the array B.  LDB >= N. */
    /*     D       (input/output) DOUBLE PRECISION array, dimension (LDD, N) */
    /*             On entry, the leading N-by-N part of this array must */
    /*             contain the matrix D of the pencil aAB - bD. */
    /*             On exit, if N = 4, the leading N-by-N part of this array */
    /*             contains the transformed matrix D in real Schur form. */
    /*             If N = 2, this array is unchanged on exit. */
    /*     LDD     INTEGER */
    /*             The leading dimension of the array D.  LDD >= N. */
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
    /*     Q3      (output) DOUBLE PRECISION array, dimension (LDQ3, N) */
    /*             The leading N-by-N part of this array contains the third */
    /*             orthogonal transformation matrix. */
    /*     LDQ3    INTEGER */
    /*             The leading dimension of the array Q3.  LDQ3 >= N. */
    /*     Workspace */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             If N = 2, then DWORK is not referenced. */
    /*     LDWORK  INTEGER */
    /*             The dimension of the array DWORK. */
    /*             If N = 4, then LDWORK >= 79. For good performance LDWORK */
    /*             should be generally larger. */
    /*             If N = 2, then LDWORK >= 0. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0: succesful exit; */
    /*             = 1: the QZ iteration failed in the LAPACK routine DGGES; */
    /*             = 2: another error occured during execution of DGGES. */
    /*     METHOD */
    /*     The algorithm uses orthogonal transformations as described on page */
    /*     20 in [2]. */
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
    /*     Chemnitz, October 22, 2008. */
    /*     REVISIONS */
    /*     V. Sima, Aug. 2009 (SLICOT version of the routine DBTFSX). */
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
    d_dim1 = *ldd;
    d_offset = d_dim1 + 1;
    d__ -= d_offset;
    q1_dim1 = *ldq1;
    q1_offset = q1_dim1 + 1;
    q1 -= q1_offset;
    q2_dim1 = *ldq2;
    q2_offset = q2_dim1 + 1;
    q2 -= q2_offset;
    q3_dim1 = *ldq3;
    q3_offset = q3_dim1 + 1;
    q3 -= q3_offset;
    --dwork;
    /* Function Body */
    *info = 0;
    /*     Computations. */
    if (*n == 4) {
        dum[0] = 0.;
        dcopy_(&c__16, dum, &c__0, &dwork[1], &c__1);
        dwork[1] = b[b_dim1 + 1];
        dwork[5] = b[(b_dim1 << 1) + 1];
        dwork[6] = b[(b_dim1 << 1) + 2];
        dwork[11] = b[b_dim1 * 3 + 3];
        dwork[15] = b[(b_dim1 << 2) + 3];
        dwork[16] = b[(b_dim1 << 2) + 4];
        dtrmm_("Left", "Upper", "No Transpose", "NonUnit", &c__2, &c__4, &c_b11, &a[a_offset], lda,
            &dwork[1], n, 4L, 5L, 12L, 7L);
        dtrmm_("Left", "Upper", "No Transpose", "NonUnit", &c__2, &c__2, &c_b11, &a[a_dim1 * 3 + 3],
            lda, &dwork[11], n, 4L, 5L, 12L, 7L);
        ievs = *n * *n + 1;
        iwrk = ievs + *n * 3;
        i__1 = *ldwork - iwrk + 1;
        dgges_("Vector Computation", "Vector Computation", "Sorted", sb02ow_, n, &d__[d_offset],
            ldd, &dwork[1], n, &idum, &dwork[ievs], &dwork[ievs + *n], &dwork[ievs + (*n << 1)],
            &q3[q3_offset], ldq3, &q1[q1_offset], ldq1, &dwork[iwrk], &i__1, bwork, info, 18L, 18L,
            6L);
        if (*info != 0) {
            if (*info >= 1 && *info <= 4) {
                *info = 1;
                return 0;
            } else if (*info != 6) {
                *info = 2;
                return 0;
            } else {
                *info = 0;
            }
        }
        dlacpy_("Full", n, n, &q1[q1_offset], ldq1, &q2[q2_offset], ldq2, 4L);
        dtrmm_("Left", "Upper", "No Transpose", "NonUnit", &c__2, &c__4, &c_b11, &b[b_offset], ldb,
            &q2[q2_offset], ldq2, 4L, 5L, 12L, 7L);
        dtrmm_("Left", "Upper", "No Transpose", "NonUnit", &c__2, &c__4, &c_b11, &b[b_dim1 * 3 + 3],
            ldb, &q2[q2_dim1 + 3], ldq2, 4L, 5L, 12L, 7L);
        dgeqr2_(n, n, &q2[q2_offset], ldq2, &dwork[1], &dwork[*n + 1], info);
        dorg2r_(n, n, n, &q2[q2_offset], ldq2, &dwork[1], &dwork[*n + 1], info);
    } else {
        /*        The pencil has infinite eigenvalues. The code decides this when */
        /*        A or B is (numerically) singular. Although the numerical */
        /*        singularity of A*B with respect to PREC is detected, the */
        /*        eigenvalues will not be infinite, but large, when neither A */
        /*        nor B is (numerically) singular. This allows a more accurate */
        /*        computation of the transformed A and B (using Q1, Q2, and Q3), */
        /*        as well as of the eigenvalues. */
        a11 = (d__1 = a[a_dim1 + 1], abs(d__1));
        a22 = (d__1 = a[(a_dim1 << 1) + 2], abs(d__1));
        b11 = (d__1 = b[b_dim1 + 1], abs(d__1));
        b22 = (d__1 = b[(b_dim1 << 1) + 2], abs(d__1));
        d21 = (d__1 = d__[d_dim1 + 2], abs(d__1));
        d12 = (d__1 = d__[(d_dim1 << 1) + 1], abs(d__1));
        compg = FALSE_;
        if (a11 * b11 <= *prec * a22 * b22) {
            if (a11 <= *prec * a22) {
                q1[q1_dim1 + 1] = 1.;
                q1[q1_dim1 + 2] = 0.;
                q1[(q1_dim1 << 1) + 1] = 0.;
                q1[(q1_dim1 << 1) + 2] = 1.;
                q2[q2_dim1 + 1] = 1.;
                q2[q2_dim1 + 2] = 0.;
                q2[(q2_dim1 << 1) + 1] = 0.;
                q2[(q2_dim1 << 1) + 2] = 1.;
                q3[q3_dim1 + 1] = 0.;
                q3[q3_dim1 + 2] = -1.;
                q3[(q3_dim1 << 1) + 1] = -1.;
                q3[(q3_dim1 << 1) + 2] = 0.;
            } else if (b11 <= *prec * b22) {
                q1[q1_dim1 + 1] = -1.;
                q1[q1_dim1 + 2] = 0.;
                q1[(q1_dim1 << 1) + 1] = 0.;
                q1[(q1_dim1 << 1) + 2] = -1.;
                q2[q2_dim1 + 1] = 0.;
                q2[q2_dim1 + 2] = 1.;
                q2[(q2_dim1 << 1) + 1] = 1.;
                q2[(q2_dim1 << 1) + 2] = 0.;
                q3[q3_dim1 + 1] = 0.;
                q3[q3_dim1 + 2] = 1.;
                q3[(q3_dim1 << 1) + 1] = 1.;
                q3[(q3_dim1 << 1) + 2] = 0.;
            } else {
                compg = TRUE_;
            }
        } else if (a22 * b22 <= *prec * a11 * b11) {
            if (a22 <= *prec * a11) {
                q1[q1_dim1 + 1] = 0.;
                q1[q1_dim1 + 2] = 1.;
                q1[(q1_dim1 << 1) + 1] = 1.;
                q1[(q1_dim1 << 1) + 2] = 0.;
                q2[q2_dim1 + 1] = 0.;
                q2[q2_dim1 + 2] = 1.;
                q2[(q2_dim1 << 1) + 1] = 1.;
                q2[(q2_dim1 << 1) + 2] = 0.;
                q3[q3_dim1 + 1] = -1.;
                q3[q3_dim1 + 2] = 0.;
                q3[(q3_dim1 << 1) + 1] = 0.;
                q3[(q3_dim1 << 1) + 2] = -1.;
            } else if (b22 <= *prec * b11) {
                q1[q1_dim1 + 1] = 0.;
                q1[q1_dim1 + 2] = -1.;
                q1[(q1_dim1 << 1) + 1] = -1.;
                q1[(q1_dim1 << 1) + 2] = 0.;
                q2[q2_dim1 + 1] = 1.;
                q2[q2_dim1 + 2] = 0.;
                q2[(q2_dim1 << 1) + 1] = 0.;
                q2[(q2_dim1 << 1) + 2] = 1.;
                q3[q3_dim1 + 1] = 1.;
                q3[q3_dim1 + 2] = 0.;
                q3[(q3_dim1 << 1) + 1] = 0.;
                q3[(q3_dim1 << 1) + 2] = 1.;
            } else {
                compg = TRUE_;
            }
            /*        The pencil has a double zero eigenvalue. */
        } else if (d21 <= *prec * d12) {
            q1[q1_dim1 + 1] = 1.;
            q1[q1_dim1 + 2] = 0.;
            q1[(q1_dim1 << 1) + 1] = 0.;
            q1[(q1_dim1 << 1) + 2] = 1.;
            q2[q2_dim1 + 1] = 1.;
            q2[q2_dim1 + 2] = 0.;
            q2[(q2_dim1 << 1) + 1] = 0.;
            q2[(q2_dim1 << 1) + 2] = 1.;
            q3[q3_dim1 + 1] = 1.;
            q3[q3_dim1 + 2] = 0.;
            q3[(q3_dim1 << 1) + 1] = 0.;
            q3[(q3_dim1 << 1) + 2] = 1.;
        } else if (d12 <= *prec * d21) {
            q1[q1_dim1 + 1] = 0.;
            q1[q1_dim1 + 2] = 1.;
            q1[(q1_dim1 << 1) + 1] = 1.;
            q1[(q1_dim1 << 1) + 2] = 0.;
            q2[q2_dim1 + 1] = 0.;
            q2[q2_dim1 + 2] = 1.;
            q2[(q2_dim1 << 1) + 1] = 1.;
            q2[(q2_dim1 << 1) + 2] = 0.;
            q3[q3_dim1 + 1] = 0.;
            q3[q3_dim1 + 2] = 1.;
            q3[(q3_dim1 << 1) + 1] = 1.;
            q3[(q3_dim1 << 1) + 2] = 0.;
        } else {
            compg = TRUE_;
        }
        if (compg) {
            /*           The pencil has real eigenvalues. */
            d__2 = a[a_dim1 + 1] * b[b_dim1 + 1] * a[(a_dim1 << 1) + 2] * b[(b_dim1 << 1) + 2];
            d__1 = d_sign(&c_b11, &d__2) * sqrt(a22 * b22 * d12);
            d__3 = sqrt(a11 * b11 * d21);
            dlartg_(&d__1, &d__3, &co, &si, &tmp);
            q1[q1_dim1 + 1] = co;
            q1[q1_dim1 + 2] = -si;
            q1[(q1_dim1 << 1) + 1] = si;
            q1[(q1_dim1 << 1) + 2] = co;
            d__2 = a[a_dim1 + 1] * a[(a_dim1 << 1) + 2];
            d__1 = d_sign(&c_b11, &d__2) * sqrt(a22 * b11 * d12);
            d__3 = sqrt(a11 * b22 * d21);
            dlartg_(&d__1, &d__3, &co, &si, &tmp);
            q2[q2_dim1 + 1] = co;
            q2[q2_dim1 + 2] = -si;
            q2[(q2_dim1 << 1) + 1] = si;
            q2[(q2_dim1 << 1) + 2] = co;
            d__1 = sqrt(a11 * b11 * d12);
            d__2 = sqrt(a22 * b22 * d21);
            dlartg_(&d__1, &d__2, &co, &si, &tmp);
            q3[q3_dim1 + 1] = co;
            q3[q3_dim1 + 2] = -si;
            q3[(q3_dim1 << 1) + 1] = si;
            q3[(q3_dim1 << 1) + 2] = co;
        }
    }
    return 0;
    /* *** Last line of MB03ED *** */
} /* mb03ed_ */
