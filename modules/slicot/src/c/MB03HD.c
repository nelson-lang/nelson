/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static logical c_false = FALSE_;
static integer c__2 = 2;
static integer c__6 = 6;
static integer c__4 = 4;
static logical c_true = TRUE_;
static integer c__0 = 0;
static doublereal c_b27 = -1.;
static doublereal c_b30 = 1.;
static integer c__1 = 1;

EXPORTSYMBOL /* Subroutine */ int mb03hd_(
    n, a, lda, b, ldb, macpar, q, ldq, dwork, info) integer* n;
doublereal* a;
integer* lda;
doublereal* b;
integer* ldb;
doublereal *macpar, *q;
integer* ldq;
doublereal* dwork;
integer* info;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, q_dim1, q_offset;
    doublereal d__1, d__2, d__3, d__4;
    /* Builtin functions */
    double sqrt();
    /* Local variables */
    static integer itau;
    static doublereal smin;
    extern /* Subroutine */ int drot_();
    static doublereal smln;
    static integer iwrk;
    static doublereal d__, s, t;
    extern /* Subroutine */ int dgemm_(), mb02uw_(), dgeqr2_(), dorg2r_();
    static doublereal co, si;
    extern /* Subroutine */ int dlascl_(), dlacpy_(), dlartg_();
    static doublereal par[3], nrm;
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
    /*     To determine an orthogonal matrix Q, for a real regular 2-by-2 or */
    /*     4-by-4 skew-Hamiltonian/Hamiltonian pencil */
    /*                     ( A11 A12  )     ( B11  B12  ) */
    /*         aA - bB = a (        T ) - b (         T ) */
    /*                     (  0  A11  )     (  0  -B11  ) */
    /*                                             T  T */
    /*     in structured Schur form, such that  J Q  J  (aA - bB) Q  is still */
    /*     in structured Schur form but the eigenvalues are exchanged. */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the pencil aA - bB.  N = 2 or N = 4. */
    /*     A       (input) DOUBLE PRECISION array, dimension (LDA, N) */
    /*             If N = 4, the leading N/2-by-N upper trapezoidal part of */
    /*             this array must contain the first block row of the skew- */
    /*             Hamiltonian matrix A of the pencil aA - bB in structured */
    /*             Schur form. Only the entries (1,1), (1,2), (1,4), and */
    /*             (2,2) are referenced. */
    /*             If N = 2, this array is not referenced. */
    /*     LDA     INTEGER */
    /*             The leading dimension of the array A.  LDA >= N/2. */
    /*     B       (input) DOUBLE PRECISION array, dimension (LDB, N) */
    /*             The leading N/2-by-N part of this array must contain the */
    /*             first block row of the Hamiltonian matrix B of the */
    /*             pencil aA - bB in structured Schur form. The entry (2,3) */
    /*             is not referenced. */
    /*     LDB     INTEGER */
    /*             The leading dimension of the array B.  LDB >= N/2. */
    /*     MACPAR  (input)  DOUBLE PRECISION array, dimension (2) */
    /*             Machine parameters: */
    /*             MACPAR(1)  (machine precision)*base, DLAMCH( 'P' ); */
    /*             MACPAR(2)  safe minimum,             DLAMCH( 'S' ). */
    /*             This argument is not used for N = 2. */
    /*     Q       (output) DOUBLE PRECISION array, dimension (LDQ, N) */
    /*             The leading N-by-N part of this array contains the */
    /*             orthogonal transformation matrix Q. */
    /*     LDQ     INTEGER */
    /*             The leading dimension of the array Q.  LDQ >= N. */
    /*     Workspace */
    /*     DWORK   DOUBLE PRECISION array, dimension (24) */
    /*             If N = 2, then DWORK is not referenced. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0: succesful exit; */
    /*             = 1: the leading N/2-by-N/2 block of the matrix B is */
    /*                  numerically singular. */
    /*     METHOD */
    /*     The algorithm uses orthogonal transformations as described on page */
    /*     31 in [2]. The structure is exploited. */
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
    /*     V. Sima, Sep. 2009 (SLICOT version of the routine DHAUEX). */
    /*     REVISIONS */
    /*     V. Sima, Nov. 2009, Nov. 2010. */
    /*     KEYWORDS */
    /*     Eigenvalue exchange, skew-Hamiltonian/Hamiltonian pencil, */
    /*     structured Schur form. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. Local Arrays .. */
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
    --macpar;
    q_dim1 = *ldq;
    q_offset = q_dim1 + 1;
    q -= q_offset;
    --dwork;
    /* Function Body */
    *info = 0;
    /*     Computations. */
    if (*n == 4) {
        /*        Set machine constants. */
        par[0] = macpar[1];
        par[1] = macpar[2];
        /*        Compute si*inv(B11)*[ A11 A12 B12 ], using blocks of A and B. */
        /*        X11 = si*inv(B11)*A11. */
        /*        Also, set SMIN to avoid overflows in matrix multiplications. */
        dwork[1] = a[a_dim1 + 1];
        dwork[2] = 0.;
        dwork[5] = a[(a_dim1 << 1) + 1];
        dwork[6] = a[(a_dim1 << 1) + 2];
        dwork[9] = 0.;
        dwork[10] = -a[(a_dim1 << 2) + 1];
        dwork[11] = -dwork[1];
        dwork[12] = -dwork[5];
        dwork[13] = -dwork[10];
        dwork[14] = 0.;
        dwork[15] = 0.;
        dwork[16] = -dwork[6];
        dwork[17] = b[b_dim1 * 3 + 1];
        dwork[18] = b[(b_dim1 << 2) + 1];
        dwork[21] = b[(b_dim1 << 2) + 1];
        dwork[22] = b[(b_dim1 << 2) + 2];
        smln = par[1] * 2. / par[0];
        /* Computing MAX */
        /* Computing MAX */
        d__3 = abs(dwork[17]), d__4 = abs(dwork[22]);
        d__1 = abs(dwork[1]), d__1 = max(d__1, smln), d__2 = abs(dwork[10]), d__1 = max(d__1, d__2),
        d__2 = abs(dwork[5]) + abs(dwork[6]), d__1 = max(d__1, d__2),
        d__2 = abs(dwork[18]) + max(d__3, d__4);
        smin = sqrt(smln) / max(d__1, d__2);
        par[2] = smin;
        mb02uw_(&c_false, &c__2, &c__6, par, &b[b_offset], ldb, &dwork[1], &c__4, &si, info);
        if (*info != 0) {
            return 0;
        }
        /*        Compute X22 = -d*inv(B11')*A11'. */
        mb02uw_(&c_true, &c__2, &c__2, par, &b[b_offset], ldb, &dwork[11], &c__4, &d__, info);
        /*        Take si = min( si, d ) as unique scaling factor. */
        if (si < d__) {
            dlascl_("G", &c__0, &c__0, &d__, &si, &c__2, &c__2, &dwork[11], &c__4, info, 1L);
        } else if (si > d__) {
            dlascl_("G", &c__0, &c__0, &si, &d__, &c__2, &c__6, &dwork[1], &c__4, info, 1L);
        }
        /*        Compute X12 = si*( inv(B11)*A12 - ( inv(B11)*B12 )*X22 ). */
        dgemm_("No Transpose", "No Transpose", &c__2, &c__2, &c__2, &c_b27, &dwork[17], &c__4,
            &dwork[11], &c__4, &c_b30, &dwork[9], &c__4, 12L, 12L);
        /*        Scale X11, X12, and X22, so that 1-norm of X11 is 1. */
        /* Computing MAX */
        d__1 = abs(dwork[1]) + abs(dwork[2]), d__2 = abs(dwork[5]) + abs(dwork[6]),
        d__1 = max(d__1, d__2);
        nrm = max(d__1, smln);
        if (nrm > 1.) {
            dlascl_("G", &c__0, &c__0, &nrm, &c_b30, &c__2, &c__4, &dwork[1], &c__4, info, 1L);
            dlascl_("G", &c__0, &c__0, &nrm, &c_b30, &c__2, &c__2, &dwork[11], &c__4, info, 1L);
        }
        /*        Compute s = trace(X11). */
        s = dwork[1] + dwork[6];
        /*        Compute Y2, the last two columns of Y = X*X - s*X + t*I4, */
        /*        where X = ( Xij ), i,j = 1,2, X21 = 0, t = det(X11). */
        t = dwork[1] * dwork[6] - dwork[2] * dwork[5];
        dlacpy_("Full", &c__4, &c__2, &dwork[9], &c__4, &q[q_offset], ldq, 4L);
        d__1 = -s;
        dgemm_("No Transpose", "No Transpose", &c__2, &c__2, &c__4, &c_b30, &dwork[1], &c__4,
            &dwork[9], &c__4, &d__1, &q[q_offset], ldq, 12L, 12L);
        d__1 = -s;
        dgemm_("No Transpose", "No Transpose", &c__2, &c__2, &c__2, &c_b30, &dwork[11], &c__4,
            &dwork[11], &c__4, &d__1, &q[q_dim1 + 3], ldq, 12L, 12L);
        q[q_dim1 + 3] += t;
        q[(q_dim1 << 1) + 4] += t;
        itau = 1;
        iwrk = 3;
        /*        Triangularize Y2 and compute the orthogonal transformation */
        /*        matrix. */
        dgeqr2_(&c__4, &c__2, &q[q_offset], ldq, &dwork[itau], &dwork[iwrk], info);
        dorg2r_(&c__4, &c__4, &c__2, &q[q_offset], ldq, &dwork[itau], &dwork[iwrk], info);
        /*        Use the last two columns of Q to build a 2-by-4 matrix W. */
        /*        Postmultiply A with the first column of Q, and premultiply */
        /*        by W. Then, annihilate the second element of the result. */
        dwork[21] = a[a_dim1 + 1] * q[q_dim1 + 1] + a[(a_dim1 << 1) + 1] * q[q_dim1 + 2]
            + a[(a_dim1 << 2) + 1] * q[q_dim1 + 4];
        dwork[22] = a[(a_dim1 << 1) + 2] * q[q_dim1 + 2] - a[(a_dim1 << 2) + 1] * q[q_dim1 + 3];
        dwork[23] = a[a_dim1 + 1] * q[q_dim1 + 3];
        dwork[24] = a[(a_dim1 << 1) + 1] * q[q_dim1 + 3] + a[(a_dim1 << 1) + 2] * q[q_dim1 + 4];
        dwork[9] = q[q_dim1 * 3 + 3] * dwork[21] + q[q_dim1 * 3 + 4] * dwork[22]
            - q[q_dim1 * 3 + 1] * dwork[23] - q[q_dim1 * 3 + 2] * dwork[24];
        dwork[10] = q[(q_dim1 << 2) + 3] * dwork[21] + q[(q_dim1 << 2) + 4] * dwork[22]
            - q[(q_dim1 << 2) + 1] * dwork[23] - q[(q_dim1 << 2) + 2] * dwork[24];
        dlartg_(&dwork[9], &dwork[10], &co, &si, &t);
        drot_(&c__4, &q[q_dim1 * 3 + 1], &c__1, &q[(q_dim1 << 2) + 1], &c__1, &co, &si);
    } else {
        d__1 = b[b_dim1 + 1] * 2.;
        dlartg_(&b[(b_dim1 << 1) + 1], &d__1, &co, &si, &t);
        q[q_dim1 + 1] = co;
        q[q_dim1 + 2] = -si;
        q[(q_dim1 << 1) + 1] = si;
        q[(q_dim1 << 1) + 2] = co;
    }
    return 0;
    /* *** Last line of MB03HD *** */
} /* mb03hd_ */
