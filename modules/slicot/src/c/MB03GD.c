/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static doublereal c_b6 = 1.;
static doublereal c_b7 = -1.;
static doublereal c_b14 = 0.;
static integer c__4 = 4;
static integer c__1 = 1;

EXPORTSYMBOL /* Subroutine */ int mb03gd_(
    n, b, ldb, d__, ldd, macpar, q, ldq, u, ldu, dwork, ldwork, info) integer* n;
doublereal* b;
integer* ldb;
doublereal* d__;
integer* ldd;
doublereal *macpar, *q;
integer* ldq;
doublereal* u;
integer* ldu;
doublereal* dwork;
integer *ldwork, *info;
{
    /* System generated locals */
    integer b_dim1, b_offset, d_dim1, d_offset, q_dim1, q_offset, u_dim1, u_offset, i__1, i__2;
    doublereal d__1, d__2;
    /* Local variables */
    static integer itau;
    static doublereal smin1, smin2, smax1, smax2;
    static integer iwrk1, iwrk2;
    static doublereal f, g;
    static integer i__;
    static doublereal r__, s, t;
    extern /* Subroutine */ int dgemm_(), mb01ru_(), mb04su_();
    static doublereal sfmin;
    extern /* Subroutine */ int dswap_(), mb04wu_();
    static doublereal f1, g1, r1, t1;
    extern /* Subroutine */ int dgeqr2_(), dgerq2_(), dlasv2_(), dorgr2_(), dorm2r_(), dsyr2k_();
    static doublereal co;
    static integer ir;
    static doublereal si;
    extern /* Subroutine */ int dlacpy_(), dlartg_();
    static doublereal cl1, cl2, co2, cr1, cr2, si2, sl1, sl2, sr1, sr2;
    static integer ics;
    static doublereal eps;
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
    /*     To compute an orthogonal matrix Q and an orthogonal symplectic */
    /*     matrix U for a real regular 2-by-2 or 4-by-4 skew-Hamiltonian/ */
    /*     Hamiltonian pencil a J B' J' B - b D with */
    /*           ( B11  B12 )      (  D11  D12  ) */
    /*       B = (          ), D = (            ), */
    /*           (  0   B22 )      (   0  -D11' ) */
    /*     such that J Q' J' D Q and U' B Q keep block triangular form, but */
    /*     the eigenvalues are reordered. */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the pencil a J B' J' B - b D. N = 2 or N = 4. */
    /*     B       (input) DOUBLE PRECISION array, dimension (LDB, N) */
    /*             The leading N-by-N part of this array must contain the */
    /*             non-trivial factor of the decomposition of the */
    /*             skew-Hamiltonian input matrix J B' J' B. The (2,1) block */
    /*             is not referenced. */
    /*     LDB     INTEGER */
    /*             The leading dimension of the array B.  LDB >= N. */
    /*     D       (input) DOUBLE PRECISION array, dimension (LDD, N) */
    /*             The leading N/2-by-N part of this array must contain the */
    /*             first block row of the second matrix of a J B' J' B - b D. */
    /*             The matrix D has to be Hamiltonian. The strict lower */
    /*             triangle of the (1,2) block is not referenced. */
    /*     LDD     INTEGER */
    /*             The leading dimension of the array D.  LDD >= N/2. */
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
    /*     U       (output) DOUBLE PRECISION array, dimension (LDU, N) */
    /*             The leading N-by-N part of this array contains the */
    /*             orthogonal symplectic transformation matrix U. */
    /*     LDU     INTEGER */
    /*             The leading dimension of the array U.  LDU >= N. */
    /*     Workspace */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             If N = 2 then DWORK is not referenced. */
    /*     LDWORK  INTEGER */
    /*             The length of the array DWORK. */
    /*             If N = 2 then LDWORK >= 0; if N = 4 then LDWORK >= 12. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0: succesful exit; */
    /*             = 1: B11 or B22 is a (numerically) singular matrix. */
    /*     METHOD */
    /*     The algorithm uses orthogonal transformations as described on page */
    /*     22 in [1], but with an improved implementation. */
    /*     REFERENCES */
    /*     [1] Benner, P., Byers, R., Losse, P., Mehrmann, V. and Xu, H. */
    /*         Numerical Solution of Real Skew-Hamiltonian/Hamiltonian */
    /*         Eigenproblems. */
    /*         Tech. Rep., Technical University Chemnitz, Germany, */
    /*         Nov. 2007. */
    /*     NUMERICAL ASPECTS */
    /*     The algorithm is numerically backward stable. */
    /*     CONTRIBUTOR */
    /*     Matthias Voigt, Fakultaet fuer Mathematik, Technische Universitaet */
    /*     Chemnitz, October 29, 2008. */
    /*     V. Sima, Aug. 2009 (SLICOT version of the routine DHAFEX). */
    /*     REVISIONS */
    /*     V. Sima, Nov. 2009, July 2010, Nov. 2010. */
    /*     KEYWORDS */
    /*     Eigenvalue exchange, skew-Hamiltonian/Hamiltonian pencil, */
    /*     structured Schur form. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. External Subroutines .. */
    /*     .. Intrinsic Functions .. */
    /*     .. Executable Statements .. */
    /*     For efficiency, the input arguments are not tested. */
    /* Parameter adjustments */
    b_dim1 = *ldb;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    d_dim1 = *ldd;
    d_offset = d_dim1 + 1;
    d__ -= d_offset;
    --macpar;
    q_dim1 = *ldq;
    q_offset = q_dim1 + 1;
    q -= q_offset;
    u_dim1 = *ldu;
    u_offset = u_dim1 + 1;
    u -= u_offset;
    --dwork;
    /* Function Body */
    *info = 0;
    /*     Computations. */
    if (*n == 4) {
        /*        Set machine constants. */
        eps = macpar[1];
        sfmin = macpar[2];
        /*        Compute the first two columns of H = inv( B' )*J'*D*inv( B )*J */
        /*        in U, using the singular value decompositions of B11 and B22. */
        dlartg_(&b[b_dim1 + 1], &b[b_dim1 + 2], &co, &si, &r__);
        f = co * b[(b_dim1 << 1) + 1] + si * b[(b_dim1 << 1) + 2];
        g = co * b[(b_dim1 << 1) + 2] - si * b[(b_dim1 << 1) + 1];
        dlasv2_(&r__, &f, &g, &smin1, &smax1, &sr1, &cr1, &sl1, &cl1);
        /* Computing MAX */
        d__1 = sfmin, d__2 = eps * abs(smax1);
        if (abs(smin1) < max(d__1, d__2)) {
            *info = 1;
            return 0;
        }
        dlartg_(&b[b_dim1 * 3 + 3], &b[b_dim1 * 3 + 4], &co2, &si2, &r__);
        f = co2 * b[(b_dim1 << 2) + 3] + si2 * b[(b_dim1 << 2) + 4];
        g = co2 * b[(b_dim1 << 2) + 4] - si2 * b[(b_dim1 << 2) + 3];
        dlasv2_(&r__, &f, &g, &smin2, &smax2, &sr2, &cr2, &sl2, &cl2);
        /* Computing MAX */
        d__1 = sfmin, d__2 = eps * abs(smax2);
        if (abs(smin2) < max(d__1, d__2)) {
            *info = 1;
            return 0;
        }
        /*        Compute inv( B11' )*D11' and copy it in U12. */
        r__ = (cr1 * d__[d_dim1 + 1] + sr1 * d__[(d_dim1 << 1) + 1]) / smax1;
        f = (cr1 * d__[d_dim1 + 2] + sr1 * d__[(d_dim1 << 1) + 2]) / smax1;
        t = (cr1 * d__[(d_dim1 << 1) + 1] - sr1 * d__[d_dim1 + 1]) / smin1;
        g = (cr1 * d__[(d_dim1 << 1) + 2] - sr1 * d__[d_dim1 + 2]) / smin1;
        r1 = cl1 * r__ - sl1 * t;
        f1 = cl1 * f - sl1 * g;
        t1 = cl1 * t + sl1 * r__;
        g1 = cl1 * g + sl1 * f;
        u[u_dim1 * 3 + 1] = co * r1 - si * t1;
        u[u_dim1 * 3 + 2] = co * t1 + si * r1;
        u[(u_dim1 << 2) + 1] = co * f1 - si * g1;
        u[(u_dim1 << 2) + 2] = co * g1 + si * f1;
        /*        Compute D11*inv( B11 )*B12 + B12'*inv( B11' )*D11' - D12 in Q. */
        q[q_dim1 + 1] = d__[d_dim1 * 3 + 1];
        q[(q_dim1 << 1) + 1] = d__[(d_dim1 << 2) + 1];
        q[(q_dim1 << 1) + 2] = d__[(d_dim1 << 2) + 2];
        dsyr2k_("Upper", "Transpose", &c__2, &c__2, &c_b6, &u[u_dim1 * 3 + 1], ldu,
            &b[b_dim1 * 3 + 1], ldb, &c_b7, &q[q_offset], ldq, 5L, 9L);
        /*        Compute inv( B22 ) in U22. */
        r__ = cr2 / smax2;
        t = sr2 / smax2;
        f = -sr2 / smin2;
        g = cr2 / smin2;
        r1 = cl2 * r__ - sl2 * f;
        t1 = cl2 * t - sl2 * g;
        f1 = cl2 * f + sl2 * r__;
        g1 = cl2 * g + sl2 * t;
        u[u_dim1 * 3 + 3] = co2 * r1 - si2 * f1;
        u[u_dim1 * 3 + 4] = co2 * t1 - si2 * g1;
        u[(u_dim1 << 2) + 3] = co2 * f1 + si2 * r1;
        u[(u_dim1 << 2) + 4] = co2 * g1 + si2 * t1;
        /*        Compute H11 = -inv( B11' )*D11'*inv( B22 ) in U11. */
        dgemm_("No Transpose", "No Transpose", &c__2, &c__2, &c__2, &c_b7, &u[u_dim1 * 3 + 1], ldu,
            &u[u_dim1 * 3 + 3], ldu, &c_b14, &u[u_offset], ldu, 12L, 12L);
        /*        Compute H21 = inv( B22' )*Q*inv( B22 ) in U21. */
        mb01ru_("Upper", "Transpose", &c__2, &c__2, &c_b14, &c_b6, &u[u_dim1 + 3], ldu,
            &u[u_dim1 * 3 + 3], ldu, &q[q_offset], ldq, &dwork[1], &c__4, info, 5L, 9L);
        u[u_dim1 + 4] = u[(u_dim1 << 1) + 3];
        s = -(u[u_dim1 + 1] + u[(u_dim1 << 1) + 2]);
        /*        Compute Y1, the first two columns of Y = H*H - s*H + t*I4, */
        /*        where H = ( Hij ), i,j = 1,2, H12 = 0, t = det(H22). */
        /*        H is real lower Hamiltonian block triangular with the */
        /*        desired eigenvalues in the leading positions. */
        t = u[u_dim1 + 1] * u[(u_dim1 << 1) + 2] - u[u_dim1 + 2] * u[(u_dim1 << 1) + 1];
        dlacpy_("Full", &c__4, &c__2, &u[u_offset], ldu, &q[q_offset], ldq, 4L);
        q[q_dim1 * 3 + 1] = u[u_dim1 + 1] - s;
        q[q_dim1 * 3 + 2] = u[u_dim1 + 2];
        q[q_dim1 * 3 + 3] = u[u_dim1 + 1];
        q[q_dim1 * 3 + 4] = u[u_dim1 + 2];
        q[(q_dim1 << 2) + 1] = u[(u_dim1 << 1) + 1];
        q[(q_dim1 << 2) + 2] = u[(u_dim1 << 1) + 2] - s;
        q[(q_dim1 << 2) + 3] = u[(u_dim1 << 1) + 1];
        q[(q_dim1 << 2) + 4] = u[(u_dim1 << 1) + 2];
        dgemm_("No Transpose", "No Transpose", &c__4, &c__2, &c__2, &c_b6, &q[q_offset], ldq,
            &q[q_dim1 * 3 + 1], ldq, &c_b14, &u[u_offset], ldu, 12L, 12L);
        dgemm_("Transpose", "No Transpose", &c__2, &c__2, &c__2, &c_b7, &q[q_dim1 * 3 + 3], ldq,
            &q[q_dim1 + 3], ldq, &c_b6, &u[u_dim1 + 3], ldu, 9L, 12L);
        u[u_dim1 + 1] += t;
        u[(u_dim1 << 1) + 2] += t;
        /*        Compute the relevant part of the orthogonal symplectic */
        /*        matrix U performing the symplectic QR factorization of Y1. */
        /*        Workspace: need   10. */
        ics = 1;
        itau = ics + 4;
        iwrk2 = itau + 2;
        i__1 = *ldwork - iwrk2 + 1;
        mb04su_(&c__2, &c__2, &u[u_dim1 + 1], ldu, &u[u_dim1 + 3], ldu, &dwork[ics], &dwork[itau],
            &dwork[iwrk2], &i__1, info);
        i__1 = *ldwork - iwrk2 + 1;
        mb04wu_("No Transpose", "No Transpose", &c__2, &c__2, &c__2, &u[u_dim1 + 1], ldu,
            &u[u_dim1 + 3], ldu, &dwork[ics], &dwork[itau], &dwork[iwrk2], &i__1, info, 12L, 12L);
        /*        Compute J*U in U. */
        u[u_dim1 * 3 + 1] = u[u_dim1 + 1];
        u[u_dim1 * 3 + 2] = u[u_dim1 + 2];
        u[(u_dim1 << 2) + 1] = u[(u_dim1 << 1) + 1];
        u[(u_dim1 << 2) + 2] = u[(u_dim1 << 1) + 2];
        u[u_dim1 + 1] = -u[u_dim1 + 3];
        u[u_dim1 + 2] = -u[u_dim1 + 4];
        u[(u_dim1 << 1) + 1] = -u[(u_dim1 << 1) + 3];
        u[(u_dim1 << 1) + 2] = -u[(u_dim1 << 1) + 4];
        u[u_dim1 + 3] = -u[u_dim1 * 3 + 1];
        u[u_dim1 + 4] = -u[u_dim1 * 3 + 2];
        u[(u_dim1 << 1) + 3] = -u[(u_dim1 << 2) + 1];
        u[(u_dim1 << 1) + 4] = -u[(u_dim1 << 2) + 2];
        u[u_dim1 * 3 + 3] = u[u_dim1 + 1];
        u[u_dim1 * 3 + 4] = u[u_dim1 + 2];
        u[(u_dim1 << 2) + 3] = u[(u_dim1 << 1) + 1];
        u[(u_dim1 << 2) + 4] = u[(u_dim1 << 1) + 2];
        /*        Compute U'*B using structure. */
        dgemm_("Transpose", "No Transpose", &c__4, &c__2, &c__2, &c_b6, &u[u_offset], ldu,
            &b[b_offset], ldb, &c_b14, &q[q_offset], ldq, 9L, 12L);
        dgemm_("Transpose", "No Transpose", &c__4, &c__2, &c__4, &c_b6, &u[u_offset], ldu,
            &b[b_dim1 * 3 + 1], ldb, &c_b14, &q[q_dim1 * 3 + 1], ldq, 9L, 12L);
        /*        Determine Q using different elimination orders in the RQ and */
        /*        QR factorizations of U'*B. */
        /*        Workspace: need   12. */
        itau = 1;
        iwrk1 = itau + *n;
        dgerq2_(n, n, &q[q_offset], ldq, &dwork[itau], &dwork[iwrk1], info);
        ir = iwrk1;
        iwrk2 = ir + 4;
        dwork[ir] = q[q_dim1 * 3 + 3];
        dwork[ir + 1] = q[(q_dim1 << 2) + 3];
        dwork[ir + 2] = 0.;
        dwork[ir + 3] = q[(q_dim1 << 2) + 4];
        dorgr2_(n, n, n, &q[q_offset], ldq, &dwork[itau], &dwork[iwrk2], info);
        i__1 = *n;
        for (i__ = 2; i__ <= i__1; ++i__) {
            i__2 = *n - i__ + 1;
            dswap_(&i__2, &q[i__ + (i__ - 1) * q_dim1], &c__1, &q[i__ - 1 + i__ * q_dim1], ldq);
            /* L20: */
        }
        dgeqr2_(&c__2, &c__2, &dwork[ir], &c__2, &dwork[itau], &dwork[iwrk2], info);
        dorm2r_("Right", "No Transpose", n, &c__2, &c__2, &dwork[ir], &c__2, &dwork[itau],
            &q[q_dim1 * 3 + 1], ldq, &dwork[iwrk2], info, 5L, 12L);
    } else {
        g = b[b_dim1 + 1] * 2. * b[(b_dim1 << 1) + 2] * d__[d_dim1 + 1];
        d__1 = b[b_dim1 + 1] * b[(b_dim1 << 1) + 2] * d__[(d_dim1 << 1) + 1];
        dlartg_(&d__1, &g, &co, &si, &r__);
        q[q_dim1 + 1] = co;
        q[q_dim1 + 2] = -si;
        q[(q_dim1 << 1) + 1] = si;
        q[(q_dim1 << 1) + 2] = co;
        d__1 = b[b_dim1 + 1] * q[q_dim1 + 1] + b[(b_dim1 << 1) + 1] * q[q_dim1 + 2];
        d__2 = b[(b_dim1 << 1) + 2] * q[q_dim1 + 2];
        dlartg_(&d__1, &d__2, &co, &si, &r__);
        u[u_dim1 + 1] = co;
        u[u_dim1 + 2] = si;
        u[(u_dim1 << 1) + 1] = -si;
        u[(u_dim1 << 1) + 2] = co;
    }
    return 0;
    /* *** Last line of MB03GD *** */
} /* mb03gd_ */
