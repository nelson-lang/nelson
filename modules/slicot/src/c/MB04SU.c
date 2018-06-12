/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c__1 = 1;

EXPORTSYMBOL /* Subroutine */ int mb04su_(
    m, n, a, lda, b, ldb, cs, tau, dwork, ldwork, info) integer *m,
    *n;
doublereal* a;
integer* lda;
doublereal* b;
integer* ldb;
doublereal *cs, *tau, *dwork;
integer *ldwork, *info;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1, i__2, i__3;
    /* Local variables */
    static doublereal temp;
    extern /* Subroutine */ int drot_();
    static integer i__, k;
    static doublereal alpha;
    extern /* Subroutine */ int dlarf_(), dlarfg_();
    static doublereal nu;
    extern /* Subroutine */ int dlartg_(), xerbla_();
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
    /*     To compute a symplectic QR decomposition of a real 2M-by-N matrix */
    /*     [A; B], */
    /*               [ A ]             [ R11  R12 ] */
    /*               [   ] = Q * R = Q [          ], */
    /*               [ B ]             [ R21  R22 ] */
    /*     where Q is a symplectic orthogonal matrix, R11 is upper triangular */
    /*     and R21 is strictly upper triangular. */
    /*     If [A; B] is symplectic then, theoretically, R21 = 0 and */
    /*     R22 = inv(R11)^T. Unblocked version. */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     M       (input) INTEGER */
    /*             The number of rows of A and B. M >= 0. */
    /*     N       (input) INTEGER */
    /*             The number of columns of A and B. N >= 0. */
    /*     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) */
    /*             On entry, the leading M-by-N part of this array must */
    /*             contain the matrix A. */
    /*             On exit, the leading M-by-N part of this array contains */
    /*             the matrix [ R11  R12 ] and, in the zero parts of R, */
    /*             information about the elementary reflectors used to */
    /*             compute the symplectic QR decomposition. */
    /*     LDA     INTEGER */
    /*             The leading dimension of the array A.  LDA >= MAX(1,M). */
    /*     B       (input/output) DOUBLE PRECISION array, dimension (LDB,N) */
    /*             On entry, the leading M-by-N part of this array must */
    /*             contain the matrix B. */
    /*             On exit, the leading M-by-N part of this array contains */
    /*             the matrix [ R21  R22 ] and, in the zero parts of B, */
    /*             information about the elementary reflectors used to */
    /*             compute the symplectic QR decomposition. */
    /*     LDB     INTEGER */
    /*             The leading dimension of the array B.  LDB >= MAX(1,M). */
    /*     CS      (output) DOUBLE PRECISION array, dimension (2 * min(M,N)) */
    /*             On exit, the first 2*min(M,N) elements of this array */
    /*             contain the cosines and sines of the symplectic Givens */
    /*             rotations used to compute the symplectic QR decomposition. */
    /*     TAU     (output) DOUBLE PRECISION array, dimension (min(M,N)) */
    /*             On exit, the first min(M,N) elements of this array */
    /*             contain the scalar factors of some of the elementary */
    /*             reflectors. */
    /*     Workspace */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             On exit, if INFO = 0,  DWORK(1)  returns the optimal */
    /*             value of LDWORK. */
    /*             On exit, if  INFO = -10,  DWORK(1)  returns the minimum */
    /*             value of LDWORK. */
    /*     LDWORK  INTEGER */
    /*             The length of the array DWORK.  LDWORK >= MAX(1,N). */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -i, the i-th argument had an illegal */
    /*                   value. */
    /*     METHOD */
    /*     The matrix Q is represented as a product of symplectic reflectors */
    /*     and Givens rotators */
    /*     Q = diag( H(1),H(1) ) G(1) diag( F(1),F(1) ) */
    /*         diag( H(2),H(2) ) G(2) diag( F(2),F(2) ) */
    /*                           .... */
    /*         diag( H(k),H(k) ) G(k) diag( F(k),F(k) ), */
    /*     where k = min(m,n). */
    /*     Each H(i) has the form */
    /*           H(i) = I - tau * w * w' */
    /*     where tau is a real scalar, and w is a real vector with */
    /*     w(1:i-1) = 0 and w(i) = 1; w(i+1:m) is stored on exit in */
    /*     B(i+1:m,i), and tau in B(i,i). */
    /*     Each F(i) has the form */
    /*           F(i) = I - nu * v * v' */
    /*     where nu is a real scalar, and v is a real vector with */
    /*     v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in */
    /*     A(i+1:m,i), and nu in TAU(i). */
    /*     Each G(i) is a Givens rotator acting on rows i of A and B, */
    /*     where the cosine is stored in CS(2*i-1) and the sine in */
    /*     CS(2*i). */
    /*     REFERENCES */
    /*     [1] Bunse-Gerstner, A. */
    /*         Matrix factorizations for symplectic QR-like methods. */
    /*         Linear Algebra Appl., 83, pp. 49-77, 1986. */
    /*     [2] Byers, R. */
    /*         Hamiltonian and Symplectic Algorithms for the Algebraic */
    /*         Riccati Equation. */
    /*         Ph.D. Dissertation, Center for Applied Mathematics, */
    /*         Cornell University, Ithaca, NY, 1983. */
    /*     NUMERICAL ASPECTS */
    /*     The algorithm requires */
    /*        8*M*N*N - 8/3*N*N*N +  2*M*N + 6*N*N + 8/3*N,  if M >= N, */
    /*        8*M*M*N - 8/3*M*M*M + 14*M*N - 6*M*M + 8/3*N,  if M <= N, */
    /*     floating point operations and is numerically backward stable. */
    /*     CONTRIBUTORS */
    /*     D. Kressner, Technical Univ. Berlin, Germany, and */
    /*     P. Benner, Technical Univ. Chemnitz, Germany, December 2003. */
    /*     REVISIONS */
    /*     V. Sima, June 2008 (SLICOT version of the HAPACK routine DGESQR). */
    /*     KEYWORDS */
    /*     Elementary matrix operations, orthogonal symplectic matrix. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. External Subroutines .. */
    /*     .. Intrinsic Functions .. */
    /*     .. Executable Statements .. */
    /*     Check the scalar input parameters. */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    --cs;
    --tau;
    --dwork;
    /* Function Body */
    *info = 0;
    if (*m < 0) {
        *info = -1;
    } else if (*n < 0) {
        *info = -2;
    } else if (*lda < max(1, *m)) {
        *info = -4;
    } else if (*ldb < max(1, *m)) {
        *info = -6;
    } else if (*ldwork < max(1, *n)) {
        dwork[1] = (doublereal)max(1, *n);
        *info = -10;
    }
    /*     Return if there were illegal values. */
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("MB04SU", &i__1, 6L);
        return 0;
    }
    /*     Quick return if possible. */
    k = min(*m, *n);
    if (k == 0) {
        dwork[1] = 1.;
        return 0;
    }
    i__1 = k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        /*        Generate elementary reflector H(i) to annihilate B(i+1:m,i). */
        alpha = b[i__ + i__ * b_dim1];
        i__2 = *m - i__ + 1;
        /* Computing MIN */
        i__3 = i__ + 1;
        dlarfg_(&i__2, &alpha, &b[min(i__3, *m) + i__ * b_dim1], &c__1, &nu);
        /*        Apply H(i) to A(i:m,i:n) and B(i:m,i+1:n) from the left. */
        b[i__ + i__ * b_dim1] = 1.;
        i__2 = *m - i__ + 1;
        i__3 = *n - i__ + 1;
        dlarf_("Left", &i__2, &i__3, &b[i__ + i__ * b_dim1], &c__1, &nu, &a[i__ + i__ * a_dim1],
            lda, &dwork[1], 4L);
        if (i__ < *n) {
            i__2 = *m - i__ + 1;
            i__3 = *n - i__;
            dlarf_("Left", &i__2, &i__3, &b[i__ + i__ * b_dim1], &c__1, &nu,
                &b[i__ + (i__ + 1) * b_dim1], ldb, &dwork[1], 4L);
        }
        b[i__ + i__ * b_dim1] = nu;
        /*        Generate symplectic Givens rotator G(i) to annihilate */
        /*        B(i,i). */
        temp = a[i__ + i__ * a_dim1];
        dlartg_(&temp, &alpha, &cs[(i__ << 1) - 1], &cs[i__ * 2], &a[i__ + i__ * a_dim1]);
        if (i__ < *n) {
            /*           Apply G(i) to [ A(i,i+1:n); B(i,i+1:n) ] from the left. */
            i__2 = *n - i__;
            drot_(&i__2, &a[i__ + (i__ + 1) * a_dim1], lda, &b[i__ + (i__ + 1) * b_dim1], ldb,
                &cs[(i__ << 1) - 1], &cs[i__ * 2]);
        }
        /*        Generate elementary reflector F(i) to annihilate A(i+1:m,i). */
        i__2 = *m - i__ + 1;
        /* Computing MIN */
        i__3 = i__ + 1;
        dlarfg_(&i__2, &a[i__ + i__ * a_dim1], &a[min(i__3, *m) + i__ * a_dim1], &c__1, &tau[i__]);
        if (i__ < *n) {
            /*           Apply F(i) to A(i:m,i+1:n) and B(i:m,i+1:n) from the */
            /*           left. */
            temp = a[i__ + i__ * a_dim1];
            a[i__ + i__ * a_dim1] = 1.;
            i__2 = *m - i__ + 1;
            i__3 = *n - i__;
            dlarf_("Left", &i__2, &i__3, &a[i__ + i__ * a_dim1], &c__1, &tau[i__],
                &a[i__ + (i__ + 1) * a_dim1], lda, &dwork[1], 4L);
            i__2 = *m - i__ + 1;
            i__3 = *n - i__;
            dlarf_("Left", &i__2, &i__3, &a[i__ + i__ * a_dim1], &c__1, &tau[i__],
                &b[i__ + (i__ + 1) * b_dim1], ldb, &dwork[1], 4L);
            a[i__ + i__ * a_dim1] = temp;
        }
        /* L10: */
    }
    dwork[1] = (doublereal)max(1, *n);
    return 0;
    /* *** Last line of MB04SU *** */
} /* mb04su_ */
