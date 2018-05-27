/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

doublereal ab13ad_(dico, equil, n, m, p, alpha, a, lda, b, ldb, c__, ldc, ns, hsv, dwork, ldwork,
    info, dico_len, equil_len) char *dico,
    *equil;
integer *n, *m, *p;
doublereal *alpha, *a;
integer* lda;
doublereal* b;
integer* ldb;
doublereal* c__;
integer *ldc, *ns;
doublereal *hsv, *dwork;
integer *ldwork, *info;
ftnlen dico_len;
ftnlen equil_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, i__1, i__2, i__3;
    doublereal ret_val = 0.;
    /* Builtin functions */
    double sqrt();
    /* Local variables */
    static integer ierr;
    extern doublereal ab13ax_();
    extern EXPORTSYMBOL /* Subroutine */ int tb01id_(), tb01kd_();
    extern logical lsame_();
    static logical discr;
    extern doublereal dlamch_();
    static integer kt, kw;
    extern /* Subroutine */ int xerbla_();
    static doublereal maxred, alpwrk;
    static integer kw1, kw2;
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
    /*     To compute the Hankel-norm of the ALPHA-stable projection of the */
    /*     transfer-function matrix G of the state-space system (A,B,C). */
    /*     FUNCTION VALUE */
    /*     AB13AD  DOUBLE PRECISION */
    /*             The Hankel-norm of the ALPHA-stable projection of G */
    /*             (if INFO = 0). */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     DICO    CHARACTER*1 */
    /*             Specifies the type of the system as follows: */
    /*             = 'C':  continuous-time system; */
    /*             = 'D':  discrete-time system. */
    /*     EQUIL   CHARACTER*1 */
    /*             Specifies whether the user wishes to preliminarily */
    /*             equilibrate the triplet (A,B,C) as follows: */
    /*             = 'S':  perform equilibration (scaling); */
    /*             = 'N':  do not perform equilibration. */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the state-space representation, i.e. the */
    /*             order of the matrix A.  N >= 0. */
    /*     M       (input) INTEGER */
    /*             The number of system inputs.  M >= 0. */
    /*     P       (input) INTEGER */
    /*             The number of system outputs.  P >= 0. */
    /*     ALPHA   (input) DOUBLE PRECISION */
    /*             Specifies the ALPHA-stability boundary for the eigenvalues */
    /*             of the state dynamics matrix A. For a continuous-time */
    /*             system (DICO = 'C'), ALPHA <= 0 is the boundary value for */
    /*             the real parts of eigenvalues, while for a discrete-time */
    /*             system (DICO = 'D'), 0 <= ALPHA <= 1 represents the */
    /*             boundary value for the moduli of eigenvalues. */
    /*             The ALPHA-stability domain does not include the boundary */
    /*             (see the Note below). */
    /*     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) */
    /*             On entry, the leading N-by-N part of this array must */
    /*             contain the state dynamics matrix A. */
    /*             On exit, if INFO = 0, the leading N-by-N part of this */
    /*             array contains the state dynamics matrix A in a block */
    /*             diagonal real Schur form with its eigenvalues reordered */
    /*             and separated. The resulting A has two diagonal blocks. */
    /*             The leading NS-by-NS part of A has eigenvalues in the */
    /*             ALPHA-stability domain and the trailing (N-NS) x (N-NS) */
    /*             part has eigenvalues outside the ALPHA-stability domain. */
    /*             Note: The ALPHA-stability domain is defined either */
    /*                   as the open half complex plane left to ALPHA, */
    /*                   for a continous-time system (DICO = 'C'), or the */
    /*                   interior of the ALPHA-radius circle centered in the */
    /*                   origin, for a discrete-time system (DICO = 'D'). */
    /*     LDA     INTEGER */
    /*             The leading dimension of array A.  LDA >= MAX(1,N). */
    /*     B       (input/output) DOUBLE PRECISION array, dimension (LDB,M) */
    /*             On entry, the leading N-by-M part of this array must */
    /*             contain the original input/state matrix B. */
    /*             On exit, if INFO = 0, the leading N-by-M part of this */
    /*             array contains the input/state matrix B of the transformed */
    /*             system. */
    /*     LDB     INTEGER */
    /*             The leading dimension of array B.  LDB >= MAX(1,N). */
    /*     C       (input/output) DOUBLE PRECISION array, dimension (LDC,N) */
    /*             On entry, the leading P-by-N part of this array must */
    /*             contain the original state/output matrix C. */
    /*             On exit, if INFO = 0, the leading P-by-N part of this */
    /*             array contains the state/output matrix C of the */
    /*             transformed system. */
    /*     LDC     INTEGER */
    /*             The leading dimension of array C.  LDC >= MAX(1,P). */
    /*     NS      (output) INTEGER */
    /*             The dimension of the ALPHA-stable subsystem. */
    /*     HSV     (output) DOUBLE PRECISION array, dimension (N) */
    /*             If INFO = 0, the leading NS elements of HSV contain the */
    /*             Hankel singular values of the ALPHA-stable part of the */
    /*             original system ordered decreasingly. */
    /*             HSV(1) is the Hankel norm of the ALPHA-stable subsystem. */
    /*     Workspace */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             On exit, if INFO = 0, DWORK(1) returns the optimal value */
    /*             of LDWORK. */
    /*     LDWORK  INTEGER */
    /*             The length of the array DWORK. */
    /*             LDWORK >= MAX(1,N*(MAX(N,M,P)+5)+N*(N+1)/2). */
    /*             For optimum performance LDWORK should be larger. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -i, the i-th argument had an illegal */
    /*                   value; */
    /*             = 1:  the computation of the ordered real Schur form of A */
    /*                   failed; */
    /*             = 2:  the separation of the ALPHA-stable/unstable diagonal */
    /*                   blocks failed because of very close eigenvalues; */
    /*             = 3:  the computed ALPHA-stable part is just stable, */
    /*                   having stable eigenvalues very near to the imaginary */
    /*                   axis (if DICO = 'C') or to the unit circle */
    /*                   (if DICO = 'D'); */
    /*             = 4:  the computation of Hankel singular values failed. */
    /*     METHOD */
    /*     Let be the following linear system */
    /*          d[x(t)] = Ax(t) + Bu(t) */
    /*          y(t)    = Cx(t)                               (1) */
    /*     where d[x(t)] is dx(t)/dt for a continuous-time system and x(t+1) */
    /*     for a discrete-time system, and let G be the corresponding */
    /*     transfer-function matrix. The following procedure is used to */
    /*     compute the Hankel-norm of the ALPHA-stable projection of G: */
    /*     1) Decompose additively G as */
    /*          G = G1 + G2 */
    /*        such that G1 = (As,Bs,Cs) has only ALPHA-stable poles and */
    /*        G2 = (Au,Bu,Cu) has only ALPHA-unstable poles. */
    /*        For the computation of the additive decomposition, the */
    /*        algorithm presented in [1] is used. */
    /*     2) Compute the Hankel-norm of ALPHA-stable projection G1 as the */
    /*        the maximum Hankel singular value of the system (As,Bs,Cs). */
    /*        The computation of the Hankel singular values is performed */
    /*        by using the square-root method of [2]. */
    /*     REFERENCES */
    /*     [1] Safonov, M.G., Jonckheere, E.A., Verma, M. and Limebeer, D.J. */
    /*         Synthesis of positive real multivariable feedback systems, */
    /*         Int. J. Control, Vol. 45, pp. 817-842, 1987. */
    /*     [2] Tombs, M.S. and Postlethwaite, I. */
    /*         Truncated balanced realization of stable, non-minimal */
    /*         state-space systems. */
    /*         Int. J. Control, Vol. 46, pp. 1319-1330, 1987. */
    /*     NUMERICAL ASPECTS */
    /*     The implemented method relies on a square-root technique. */
    /*                                     3 */
    /*     The algorithms require about 17N  floating point operations. */
    /*     CONTRIBUTOR */
    /*     C. Oara and A. Varga, German Aerospace Center, */
    /*     DLR Oberpfaffenhofen, July 1998. */
    /*     Based on the RASP routine SHANRM. */
    /*     REVISIONS */
    /*     Nov. 1998, V. Sima, Research Institute for Informatics, Bucharest. */
    /*     Dec. 1998, V. Sima, Katholieke Univ. Leuven, Leuven. */
    /*     Oct. 2001, V. Sima, Research Institute for Informatics, Bucharest. */
    /*     KEYWORDS */
    /*     Additive spectral decomposition, model reduction, */
    /*     multivariable system, state-space model, system norms. */
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
    c_dim1 = *ldc;
    c_offset = c_dim1 + 1;
    c__ -= c_offset;
    --hsv;
    --dwork;
    /* Function Body */
    *info = 0;
    discr = lsame_(dico, "D", 1L, 1L);
    /*     Test the input scalar arguments. */
    if (!(lsame_(dico, "C", 1L, 1L) || discr)) {
        *info = -1;
    } else if (!(lsame_(equil, "S", 1L, 1L) || lsame_(equil, "N", 1L, 1L))) {
        *info = -2;
    } else if (*n < 0) {
        *info = -3;
    } else if (*m < 0) {
        *info = -4;
    } else if (*p < 0) {
        *info = -5;
    } else if (discr && (*alpha < 0. || *alpha > 1.) || !discr && *alpha > 0.) {
        *info = -6;
    } else if (*lda < max(1, *n)) {
        *info = -8;
    } else if (*ldb < max(1, *n)) {
        *info = -10;
    } else if (*ldc < max(1, *p)) {
        *info = -12;
    } else /* if(complicated condition) */
    {
        /* Computing MAX */
        /* Computing MAX */
        i__3 = max(*n, *m);
        i__1 = 1, i__2 = *n * (max(i__3, *p) + 5) + *n * (*n + 1) / 2;
        if (*ldwork < max(i__1, i__2)) {
            *info = -16;
        }
    }
    if (*info != 0) {
        /*        Error return. */
        i__1 = -(*info);
        xerbla_("AB13AD", &i__1, 6L);
        return ret_val;
    }
    /*     Quick return if possible. */
    /* Computing MIN */
    i__1 = min(*n, *m);
    if (min(i__1, *p) == 0) {
        *ns = 0;
        ret_val = 0.;
        dwork[1] = 1.;
        return ret_val;
    }
    if (lsame_(equil, "S", 1L, 1L)) {
        /*        Scale simultaneously the matrices A, B and C: */
        /*        A <- inv(D)*A*D,  B <- inv(D)*B and C <- C*D, where D is a */
        /*        diagonal matrix. */
        /*        Workspace: N. */
        maxred = 100.;
        tb01id_("All", n, m, p, &maxred, &a[a_offset], lda, &b[b_offset], ldb, &c__[c_offset], ldc,
            &dwork[1], info, 3L);
    }
    /*     Correct the value of ALPHA to ensure stability. */
    alpwrk = *alpha;
    if (discr) {
        if (*alpha == 1.) {
            alpwrk = 1. - sqrt(dlamch_("E", 1L));
        }
    } else {
        if (*alpha == 0.) {
            alpwrk = -sqrt(dlamch_("E", 1L));
        }
    }
    /*     Allocate working storage. */
    kt = 1;
    kw1 = *n * *n + 1;
    kw2 = kw1 + *n;
    kw = kw2 + *n;
    /*     Reduce A to a block diagonal real Schur form, with the */
    /*     ALPHA-stable part in the leading diagonal position, using a */
    /*     non-orthogonal similarity transformation A <- inv(T)*A*T and */
    /*     apply the transformation to B and C: B <- inv(T)*B and C <- C*T. */
    /*     Workspace needed:      N*(N+2); */
    /*     Additional workspace:  need   3*N; */
    /*                            prefer larger. */
    i__1 = *ldwork - kw + 1;
    tb01kd_(dico, "Stable", "General", n, m, p, &alpwrk, &a[a_offset], lda, &b[b_offset], ldb,
        &c__[c_offset], ldc, ns, &dwork[kt], n, &dwork[kw1], &dwork[kw2], &dwork[kw], &i__1, &ierr,
        1L, 6L, 7L);
    if (ierr != 0) {
        if (ierr != 3) {
            *info = 1;
        } else {
            *info = 2;
        }
        return ret_val;
    }
    wrkopt = dwork[kw] + (doublereal)(kw - 1);
    if (*ns == 0) {
        ret_val = 0.;
    } else {
        /*        Workspace:  need   N*(MAX(N,M,P)+5)+N*(N+1)/2; */
        /*                    prefer larger. */
        ret_val = ab13ax_(dico, ns, m, p, &a[a_offset], lda, &b[b_offset], ldb, &c__[c_offset], ldc,
            &hsv[1], &dwork[1], ldwork, &ierr, 1L);
        if (ierr != 0) {
            *info = ierr + 2;
            return ret_val;
        }
        dwork[1] = max(wrkopt, dwork[1]);
    }
    return ret_val;
    /* *** Last line of AB13AD *** */
} /* ab13ad_ */
