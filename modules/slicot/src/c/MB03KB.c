/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static logical c_false = FALSE_;
static integer c_n1 = -1;
static integer c__0 = 0;
static doublereal c_b18 = 1.;
static integer c__2 = 2;
static integer c__1 = 1;
static doublereal c_b92 = -1.;
static integer c__3 = 3;
static integer c__4 = 4;

EXPORTSYMBOL /* Subroutine */ int mb03kb_(compq, whichq, ws, k, nc, kschur, j1, n1, n2, n, ni, s, t,
    ldt, ixt, q, ldq, ixq, tol, iwork, dwork, ldwork, info, compq_len) char* compq;
integer* whichq;
logical* ws;
integer *k, *nc, *kschur, *j1, *n1, *n2, *n, *ni, *s;
doublereal* t;
integer *ldt, *ixt;
doublereal* q;
integer *ldq, *ixq;
doublereal* tol;
integer* iwork;
doublereal* dwork;
integer *ldwork, *info;
ftnlen compq_len;
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1, d__2;
    /* Local variables */
    static integer iv1p1, iv2p1;
    static doublereal dnrm;
    static integer vloc, ltau;
    static doublereal temp[16], taus[2];
    static integer indf1, indf2;
    static doublereal dtau1, dtau2;
    static integer indv1, indv2, itau1, itau2, vloc1, ltau1, ltau2, vloc2, a, b, c__, i__, l;
    extern /* Subroutine */ int mb03kc_(), mb03ke_();
    static integer v, w;
    static logical fill21, fill43;
    extern logical lsame_();
    static integer indxc, indvf, itauf;
    static logical specq;
    static integer tau1p1, tau2p1, indtt;
    extern /* Subroutine */ int daxpy_();
    static integer indxv;
    static logical wantq;
    static integer j2, j3, j4, v1, v2, itauf1, itauf2;
    extern doublereal dlapy2_();
    static integer indvp1, itaup1;
    static doublereal tempm1[16];
    static integer ia, i11, i12, i21, i22, ib, ic, ii, nd, iq, mn, is, it, we;
    extern doublereal dlange_();
    extern /* Subroutine */ int dlarfg_(), dlascl_();
    static doublereal scaloc;
    static integer tt;
    extern /* Subroutine */ int dlacpy_();
    static logical fillin;
    extern /* Subroutine */ int xerbla_(), dlarfx_();
    static integer indtau;
    extern doublereal dlantr_();
    static doublereal tauloc, thresh;
    static integer nd2;
    static logical wantql;
    static integer minwrk;
    static doublereal v_1__, v_2__;
    static integer ip1;
    static doublereal smlnum, strong, v_3__, w_2__;
    static integer it2;
    static doublereal w_3__, x_11__, x_12__, x_21__, x_22__, eps;
    static integer ipp, tau;
    static doublereal tmp;
    static integer ltt, tau1, tau2;
    static doublereal tmp1, tmp2;
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
    /*     To reorder the diagonal blocks of the formal matrix product */
    /*        T22_K^S(K) * T22_K-1^S(K-1) * ... * T22_1^S(1)              (1) */
    /*     of length K in the generalized periodic Schur form */
    /*              [  T11_k  T12_k  T13_k  ] */
    /*        T_k = [    0    T22_k  T23_k  ],    k = 1, ..., K,          (2) */
    /*              [    0      0    T33_k  ] */
    /*     where */
    /*     - the submatrices T11_k are NI(k+1)-by-NI(k), if S(k) = 1, or */
    /*       NI(k)-by-NI(k+1), if S(k) = -1, and contain dimension-induced */
    /*       infinite eigenvalues, */
    /*     - the submatrices T22_k are NC-by-NC and contain core eigenvalues, */
    /*       which are generically neither zero nor infinite, */
    /*     - the submatrices T33_k contain dimension-induced zero */
    /*       eigenvalues, */
    /*     such that pairs of adjacent diagonal blocks of sizes 1 and/or 2 in */
    /*     the product (1) are swapped. */
    /*     Optionally, the transformation matrices Q_1,...,Q_K from the */
    /*     reduction into generalized periodic Schur form are updated with */
    /*     respect to the performed reordering. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     COMPQ   CHARACTER*1 */
    /*             = 'N': do not compute any of the matrices Q_k; */
    /*             = 'U': each coefficient of Q must contain an orthogonal */
    /*                    matrix Q1_k on entry, and the products Q1_k*Q_k are */
    /*                    returned, where Q_k, k = 1, ..., K, performed the */
    /*                    reordering; */
    /*             = 'W': the computation of each Q_k is specified */
    /*                    individually in the array WHICHQ. */
    /*     WHICHQ  INTEGER array, dimension (K) */
    /*             If COMPQ = 'W', WHICHQ(k) specifies the computation of Q_k */
    /*             as follows: */
    /*             = 0:   do not compute Q_k; */
    /*             > 0:   the kth coefficient of Q must contain an orthogonal */
    /*                    matrix Q1_k on entry, and the product Q1_k*Q_k is */
    /*                    returned. */
    /*             This array is not referenced if COMPQ <> 'W'. */
    /*     WS      LOGICAL */
    /*             = .FALSE. : do not perform the strong stability tests; */
    /*             = .TRUE.  : perform the strong stability tests; often, */
    /*                         this is not needed, and omitting them can save */
    /*                         some computations. */
    /*     Input/Output Parameters */
    /*     K       (input) INTEGER */
    /*             The period of the periodic matrix sequences T and Q (the */
    /*             number of factors in the matrix product).  K >= 2. */
    /*             (For K = 1, a standard eigenvalue reordering problem is */
    /*             obtained.) */
    /*     NC      (input) INTEGER */
    /*             The number of core eigenvalues.  0 <= NC <= min(N). */
    /*     KSCHUR  (input) INTEGER */
    /*             The index for which the matrix T22_kschur is upper quasi- */
    /*             triangular. */
    /*     J1      (input) INTEGER */
    /*             The index of the first row and column of the first block */
    /*             to swap in T22_k. */
    /*             1 <= J1 <= NC-N1-N2+1. */
    /*     N1      (input) INTEGER */
    /*             The order of the first block to swap.   N1 = 0, 1 or 2. */
    /*     N2      (input) INTEGER */
    /*             The order of the second block to swap.  N2 = 0, 1 or 2. */
    /*     N       (input) INTEGER array, dimension (K) */
    /*             The leading K elements of this array must contain the */
    /*             dimensions of the factors of the formal matrix product T, */
    /*             such that the k-th coefficient T_k is an N(k+1)-by-N(k) */
    /*             matrix, if S(k) = 1, or an N(k)-by-N(k+1) matrix, */
    /*             if S(k) = -1, k = 1, ..., K, where N(K+1) = N(1). */
    /*     NI      (input) INTEGER array, dimension (K) */
    /*             The leading K elements of this array must contain the */
    /*             dimensions of the factors of the matrix sequence T11_k. */
    /*             N(k) >= NI(k) + NC >= 0. */
    /*     S       (input) INTEGER array, dimension (K) */
    /*             The leading K elements of this array must contain the */
    /*             signatures (exponents) of the factors in the K-periodic */
    /*             matrix sequence. Each entry in S must be either 1 or -1; */
    /*             the value S(k) = -1 corresponds to using the inverse of */
    /*             the factor T_k. */
    /*     T       (input/output) DOUBLE PRECISION array, dimension (*) */
    /*             On entry, this array must contain at position IXT(k) the */
    /*             matrix T_k, which is at least N(k+1)-by-N(k), if S(k) = 1, */
    /*             or at least N(k)-by-N(k+1), if S(k) = -1, in periodic */
    /*             Schur form. */
    /*             On exit, the matrices T_k are overwritten by the reordered */
    /*             periodic Schur form. */
    /*     LDT     INTEGER array, dimension (K) */
    /*             The leading dimensions of the matrices T_k in the one- */
    /*             dimensional array T. */
    /*             LDT(k) >= max(1,N(k+1)),  if S(k) =  1, */
    /*             LDT(k) >= max(1,N(k)),    if S(k) = -1. */
    /*     IXT     INTEGER array, dimension (K) */
    /*             Start indices of the matrices T_k in the one-dimensional */
    /*             array T. */
    /*     Q       (input/output) DOUBLE PRECISION array, dimension (*) */
    /*             On entry, this array must contain at position IXQ(k) a */
    /*             matrix Q_k of size at least N(k)-by-N(k), provided that */
    /*             COMPQ = 'U', or COMPQ = 'W' and WHICHQ(k) > 0. */
    /*             On exit, if COMPQ = 'U', or COMPQ = 'W' and WHICHQ(k) > 0, */
    /*             Q_k is post-multiplied with the orthogonal matrix that */
    /*             performed the reordering. */
    /*             This array is not referenced if COMPQ = 'N'. */
    /*     LDQ     INTEGER array, dimension (K) */
    /*             The leading dimensions of the matrices Q_k in the one- */
    /*             dimensional array Q.  LDQ(k) >= 1, and */
    /*             LDQ(k) >= max(1,N(k)), if COMPQ = 'U', or COMPQ = 'W' and */
    /*                                                       WHICHQ(k) > 0; */
    /*             This array is not referenced if COMPQ = 'N'. */
    /*     IXQ     INTEGER array, dimension (K) */
    /*             Start indices of the matrices Q_k in the one-dimensional */
    /*             array Q. */
    /*             This array is not referenced if COMPQ = 'N'. */
    /*     Tolerances */
    /*     TOL     DOUBLE PRECISION array, dimension (3) */
    /*             This array contains tolerance parameters. The weak and */
    /*             strong stability tests use a threshold computed by the */
    /*             formula  MAX( c*EPS*NRM, SMLNUM ),  where c is a constant, */
    /*             NRM is the Frobenius norm of the matrix formed by */
    /*             concatenating K pairs of adjacent diagonal blocks of sizes */
    /*             1 and/or 2 in the T22_k submatrices from (2), which are */
    /*             swapped, and EPS and SMLNUM are the machine precision and */
    /*             safe minimum divided by EPS, respectively (see LAPACK */
    /*             Library routine DLAMCH). The norm NRM is computed by this */
    /*             routine; the other values are stored in the array TOL. */
    /*             TOL(1), TOL(2), and TOL(3) contain c, EPS, and SMLNUM, */
    /*             respectively. TOL(1) should normally be at least 10. */
    /*     Workspace */
    /*     IWORK   INTEGER array, dimension (4*K) */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             On exit, if INFO = 0, DWORK(1) returns the optimal LDWORK. */
    /*     LDWORK  INTEGER */
    /*             The dimension of the array DWORK. */
    /*             LDWORK >= 10*K + MN,                 if N1 = 1, N2 = 1; */
    /*             LDWORK >= 25*K + MN,                 if N1 = 1, N2 = 2; */
    /*             LDWORK >= MAX(23*K + MN, 25*K - 12), if N1 = 2, N2 = 1; */
    /*             LDWORK >= MAX(42*K + MN, 80*K - 48), if N1 = 2, N2 = 2; */
    /*             where MN = MXN, if MXN > 10, and MN = 0, otherwise, with */
    /*             MXN = MAX(N(k),k=1,...,K). */
    /*             If LDWORK = -1  a workspace query is assumed; the */
    /*             routine only calculates the optimal size of the DWORK */
    /*             array, returns this value as the first entry of the DWORK */
    /*             array, and no error message is issued by XERBLA. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -22, then LDWORK is too small; appropriate */
    /*                   value for LDWORK is returned in DWORK(1); the other */
    /*                   arguments are not tested, for efficiency; */
    /*             = 1:  the swap was rejected from stability reasons; the */
    /*                   blocks are not swapped and T and Q are unchanged. */
    /*     METHOD */
    /*     The algorithm described in [1] is used. Both weak and strong */
    /*     stability tests are performed. */
    /*     REFERENCES */
    /*     [1] Granat, R., Kagstrom, B. and Kressner, D. */
    /*         Computing periodic deflating subspaces associated with a */
    /*         specified set of eigenvalues. */
    /*         BIT Numerical Mathematics, vol. 47, 763-791, 2007. */
    /*     NUMERICAL ASPECTS */
    /*     The implemented method is numerically backward stable. */
    /*                                  3 */
    /*     The algorithm requires 0(K NC ) floating point operations. */
    /*     CONTRIBUTOR */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Romania, */
    /*     Mar. 2010, an essentially new version of the PEP routine */
    /*     PEP_DLAEXC, by R. Granat, Umea University, Sweden, Apr. 2008. */
    /*     REVISIONS */
    /*     V. Sima, Apr. 2010, May 2010, July 2010. */
    /*     KEYWORDS */
    /*     Eigenvalues, QZ algorithm, periodic QZ algorithm, orthogonal */
    /*     transformation. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. */
    /*     .. Scalar Arguments .. */
    /*     .. */
    /*     .. Array Arguments .. */
    /*     .. */
    /*     .. Local Scalars .. */
    /*     .. */
    /*     .. Local Arrays .. */
    /*     .. */
    /*     .. External Functions .. */
    /*     .. */
    /*     .. External Subroutines .. */
    /*     .. */
    /*     .. Intrinsic Functions .. */
    /*     .. */
    /*     .. Executable Statements .. */
    /*     Decode the input parameters */
    /* Parameter adjustments */
    --dwork;
    --iwork;
    --tol;
    --ixq;
    --ldq;
    --q;
    --ixt;
    --ldt;
    --t;
    --s;
    --ni;
    --n;
    --whichq;
    /* Function Body */
    *info = 0;
    wantq = lsame_(compq, "U", 1L, 1L);
    specq = lsame_(compq, "W", 1L, 1L);
    /*     Set the machine-dependent parameters. */
    eps = tol[2];
    smlnum = tol[3];
    /*     For efficiency reasons, the parameters are not checked. */
    /*     Set integer pointers to correct subsequences in T22_k and check */
    /*     workspace. For simplicity, below these subsequences are denoted */
    /*     by T11, T22 and T12 and are not to be confused with the T11_k, */
    /*     T22_k and T12_k in (2). Also set integer pointers to be used in */
    /*     Sylvester solver. */
    j2 = *j1 + *n1;
    i11 = 0;
    i21 = i11 + *k;
    i12 = i21 + *k;
    i22 = i12 + *k;
    mn = 0;
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        /* Computing MAX */
        i__2 = mn, i__3 = n[i__];
        mn = max(i__2, i__3);
        ip1 = i__ % *k + 1;
        if (s[i__] == 1) {
            ii = ixt[i__] + ni[i__] * ldt[i__] + ni[ip1] - 1;
        } else {
            ii = ixt[i__] + ni[ip1] * ldt[i__] + ni[i__] - 1;
        }
        iwork[i11 + i__] = ii + (*j1 - 1) * ldt[i__] + *j1;
        iwork[i21 + i__] = iwork[i11 + i__] + *n1;
        iwork[i12 + i__] = iwork[i11 + i__] + *n1 * ldt[i__];
        iwork[i22 + i__] = iwork[i12 + i__] + *n1;
        /* L10: */
    }
    /*     Divide workspace into different arrays and submatrices. */
    a = 1;
    if (*n1 == 1 && *n2 == 1) {
        b = a + *k;
        c__ = b + *k;
        tau = c__ + *k;
        v = tau + *k;
        tt = v + (*k << 1);
        w = tt + (*k << 2);
        we = tau;
        mn += *k * 10;
    } else if (*n1 == 1 && *n2 == 2) {
        b = a + *k;
        c__ = b + (*k << 2);
        tau1 = c__ + (*k << 1);
        v1 = tau1 + *k;
        tau2 = v1 + (*k << 1);
        v2 = tau2 + *k;
        tt = v2 + (*k << 1);
        ltau = tt + *k * 9;
        vloc = ltau + *k;
        w = vloc + (*k << 1);
        we = tau1;
        mn += *k * 25;
    } else if (*n1 == 2 && *n2 == 1) {
        b = a + (*k << 2);
        c__ = b + *k;
        tau = c__ + (*k << 1);
        v = tau + *k;
        tt = v + *k * 3;
        ltau = tt + *k * 9;
        vloc = ltau + *k;
        w = vloc + (*k << 1);
        we = tau;
        mn += *k * 23;
    } else if (*n1 == 2 && *n2 == 2) {
        b = a + (*k << 2);
        c__ = b + (*k << 2);
        tau1 = c__ + (*k << 2);
        v1 = tau1 + *k;
        tau2 = v1 + *k * 3;
        v2 = tau2 + *k;
        tt = v2 + *k * 3;
        ltau1 = tt + (*k << 4);
        vloc1 = ltau1 + *k;
        ltau2 = vloc1 + (*k << 1);
        vloc2 = ltau2 + *k;
        w = vloc2 + (*k << 1);
        we = tau1;
        mn += *k * 42;
    }
    mb03ke_(&c_false, &c_false, &c_n1, k, n1, n2, &eps, &smlnum, &s[1], &t[1], &t[1], &t[1],
        &scaloc, &dwork[1], &c_n1, info);
    /* Computing MAX */
    i__1 = (integer)dwork[1] + we - 1;
    minwrk = max(i__1, mn);
    /*     Quick return if possible. */
    dwork[1] = (doublereal)minwrk;
    if (*ldwork == -1) {
        return 0;
    } else if (*ldwork < minwrk) {
        *info = -22;
        i__1 = -(*info);
        xerbla_("MB03KB", &i__1, 6L);
        return 0;
    } else if (*nc <= 1 || *n1 <= 0 || *n2 <= 0 || *n1 > *nc || j2 > *nc || j2 + *n2 - 1 > *nc) {
        return 0;
    }
    /*     Compute some local indices. */
    j2 = *j1 + 1;
    j3 = *j1 + 2;
    j4 = *j1 + 3;
    /*     Solve the periodic Sylvester-like equation associated with */
    /*     the swap. */
    /*     Copy T11, T22 and T12 to workspace. Apply scaling to all of T22_k */
    /*     for numerical stability. */
    ia = a;
    ib = b;
    ic = c__;
    nd = *n1 + *n2;
    /* Computing 2nd power */
    i__1 = nd;
    nd2 = i__1 * i__1;
    dnrm = 0.;
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        it = iwork[i11 + i__];
        is = iwork[i12 + i__];
        iq = iwork[i22 + i__];
        tmp = dlantr_(
            "Frobenius", "Upper", "NonUnit", &nd, &nd, &t[it], &ldt[i__], &dwork[1], 9L, 5L, 7L);
        if (i__ == *kschur) {
            if (*n1 == 2) {
                tmp = dlapy2_(&t[it + 1], &tmp);
            }
            if (*n2 == 2) {
                tmp = dlapy2_(&t[iq + 1], &tmp);
            }
        }
        dnrm = dlapy2_(&dnrm, &tmp);
        tmp = max(tmp, smlnum);
        if (*n1 == 1) {
            dwork[ia] = t[it] / tmp;
            dwork[ic] = t[is] / tmp;
            if (*n2 == 1) {
                dwork[ib] = t[iq] / tmp;
            } else {
                dlacpy_("All", n2, n2, &t[iq], &ldt[i__], &dwork[ib], n2, 3L);
                dlascl_("General", &c__0, &c__0, &tmp, &c_b18, n2, n2, &dwork[ib], n2, info, 7L);
                dwork[ic + 1] = t[is + ldt[i__]] / tmp;
            }
        } else {
            dlacpy_("All", n1, n1, &t[it], &ldt[i__], &dwork[ia], n1, 3L);
            dlascl_("General", &c__0, &c__0, &tmp, &c_b18, n1, n1, &dwork[ia], n1, info, 7L);
            if (*n2 == 1) {
                dwork[ib] = t[iq] / tmp;
                dwork[ic] = t[is] / tmp;
                dwork[ic + 1] = t[is + 1] / tmp;
            } else {
                dlacpy_("All", n2, n2, &t[iq], &ldt[i__], &dwork[ib], n2, 3L);
                dlascl_("General", &c__0, &c__0, &tmp, &c_b18, n2, n2, &dwork[ib], n2, info, 7L);
                dlacpy_("All", n1, n2, &t[is], &ldt[i__], &dwork[ic], n1, 3L);
                dlascl_("General", &c__0, &c__0, &tmp, &c_b18, n1, n2, &dwork[ic], n1, info, 7L);
            }
        }
        /* Computing 2nd power */
        i__2 = *n1;
        ia += i__2 * i__2;
        /* Computing 2nd power */
        i__2 = *n2;
        ib += i__2 * i__2;
        ic += *n1 * *n2;
        /* L20: */
    }
    /*     Compute a machine-dependent threshold of the test for accepting */
    /*     a swap. */
    /* Computing MAX */
    d__1 = tol[1] * eps * dnrm;
    thresh = max(d__1, smlnum);
    /*     Call the periodic Sylvester-like equation solver. */
    /*     Workspace: need   WE - 1 + (4*K-3)*(N1*N2)**2 + K*N1*N2. */
    i__1 = *ldwork - we + 1;
    mb03ke_(&c_false, &c_false, &c_n1, k, n1, n2, &eps, &smlnum, &s[1], &dwork[a], &dwork[b],
        &dwork[c__], &scaloc, &dwork[we], &i__1, info);
    /*     Swap the adjacent diagonal blocks. */
    l = *n1 + *n1 + *n2 - 2;
    switch ((int)l) {
    case 1:
        goto L30;
    case 2:
        goto L70;
    case 3:
        goto L140;
    case 4:
        goto L210;
    }
L30:
    /*     Direct swap with N1 = 1 and N2 = 1. */
    /*     Generate elementary reflectors H_i such that: */
    /*     H_i( X_11_i ) = ( * ). */
    /*        ( scale  )   ( 0 ) */
    indxc = c__;
    indxv = v;
    i__1 = tau + *k - 1;
    for (indtau = tau; indtau <= i__1; ++indtau) {
        x_11__ = dwork[indxc];
        dwork[indxv] = x_11__;
        dwork[indxv + 1] = scaloc;
        dlarfg_(&c__2, &dwork[indxv], &dwork[indxv + 1], &c__1, &dwork[indtau]);
        dwork[indxv] = 1.;
        /*        Next, do weak stability test. */
        tauloc = dwork[indtau];
        tmp = scaloc * (1. - tauloc) + tauloc * dwork[indxv + 1] * x_11__;
        if (abs(tmp) > thresh) {
            goto L300;
        }
        ++indxc;
        indxv += 2;
        /* L40: */
    }
    if (*ws) {
        /*        The swap passed weak stability test - move on and perform the */
        /*        swapping temporarily into TT (workspace) and perform strong */
        /*        stability test. */
        indtau = tau;
        indxv = v;
        indtt = tt;
        i__1 = *k;
        for (i__ = 1; i__ <= i__1; ++i__) {
            ip1 = i__ % *k;
            indvp1 = v + (ip1 << 1);
            itaup1 = tau + ip1;
            dlacpy_("All", &c__2, &c__2, &t[iwork[i11 + i__]], &ldt[i__], temp, &c__2, 3L);
            dlacpy_("All", &c__2, &c__2, temp, &c__2, &dwork[indtt], &c__2, 3L);
            if (s[i__] == 1) {
                dlarfx_("Left", &c__2, &c__2, &dwork[indvp1], &dwork[itaup1], &dwork[indtt], &c__2,
                    &dwork[w], 4L);
                dlarfx_("Right", &c__2, &c__2, &dwork[indxv], &dwork[indtau], &dwork[indtt], &c__2,
                    &dwork[w], 5L);
            } else {
                dlarfx_("Right", &c__2, &c__2, &dwork[indvp1], &dwork[itaup1], &dwork[indtt], &c__2,
                    &dwork[w], 5L);
                dlarfx_("Left", &c__2, &c__2, &dwork[indxv], &dwork[indtau], &dwork[indtt], &c__2,
                    &dwork[w], 4L);
            }
            dlacpy_("All", &c__2, &c__2, &dwork[indtt], &c__2, tempm1, &c__2, 3L);
            if (s[i__] == 1) {
                dlarfx_("Left", &c__2, &c__2, &dwork[indvp1], &dwork[itaup1], tempm1, &c__2,
                    &dwork[w], 4L);
                dlarfx_("Right", &c__2, &c__2, &dwork[indxv], &dwork[indtau], tempm1, &c__2,
                    &dwork[w], 5L);
            } else {
                dlarfx_("Right", &c__2, &c__2, &dwork[indvp1], &dwork[itaup1], tempm1, &c__2,
                    &dwork[w], 5L);
                dlarfx_("Left", &c__2, &c__2, &dwork[indxv], &dwork[indtau], tempm1, &c__2,
                    &dwork[w], 4L);
            }
            daxpy_(&nd2, &c_b92, temp, &c__1, tempm1, &c__1);
            strong = dlange_("Frobenius", &c__2, &c__2, tempm1, &c__2, &dwork[1], 9L);
            if (strong > thresh) {
                goto L300;
            }
            ++indtau;
            indxv += 2;
            indtt += 4;
            /* L50: */
        }
    }
    /*     The swap was accepted - now update T and Q with respect to the */
    /*     swapping. */
    indtau = tau;
    indxv = v;
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        ip1 = i__ % *k;
        /*        Apply Householder transformations from left or right depending */
        /*        on S and accumulate transformations in matrices Q_i, i = 1:K. */
        indvp1 = v + (ip1 << 1);
        itaup1 = tau + ip1;
        ++ip1;
        it = iwork[i11 + i__] - *j1 + 1;
        if (s[i__] == 1) {
            i__2 = n[i__] - *j1 + 1;
            dlarfx_("Left", &c__2, &i__2, &dwork[indvp1], &dwork[itaup1], &t[iwork[i11 + i__]],
                &ldt[i__], &dwork[w], 4L);
            i__2 = ni[ip1] + j2;
            dlarfx_("Right", &i__2, &c__2, &dwork[indxv], &dwork[indtau], &t[it - ni[ip1]],
                &ldt[i__], &dwork[w], 5L);
        } else {
            i__2 = n[ip1] - *j1 + 1;
            dlarfx_("Left", &c__2, &i__2, &dwork[indxv], &dwork[indtau], &t[iwork[i11 + i__]],
                &ldt[i__], &dwork[w], 4L);
            i__2 = ni[i__] + j2;
            dlarfx_("Right", &i__2, &c__2, &dwork[indvp1], &dwork[itaup1], &t[it - ni[i__]],
                &ldt[i__], &dwork[w], 5L);
        }
        /*        Set to zero the fill-in element T(J2,J1,I). */
        t[iwork[i21 + i__]] = 0.;
        wantql = wantq;
        if (specq) {
            wantql = whichq[i__] != 0;
        }
        if (wantql) {
            iq = ixq[i__] + (*j1 - 1) * ldq[i__];
            dlarfx_("Right", &n[i__], &c__2, &dwork[indxv], &dwork[indtau], &q[iq], &ldq[i__],
                &dwork[w], 5L);
        }
        ++indtau;
        indxv += 2;
        /* L60: */
    }
    /*     Exit direct swap N1 = 1 and N2 = 1. */
    goto L290;
    /*     Direct swap with N1 = 1 and N2 = 2. */
L70:
    /*     Generate elementary reflectors H(1)_i and H(2)_i such that */
    /*     H(2)_i H(1)_i  ( X_11_i X_12_i ) = ( * * ). */
    /*                    (  scale    0   )   ( 0 * ) */
    /*                    (   0     scale )   ( 0 0 ) */
    itau2 = tau2;
    indxc = c__;
    indv1 = v1;
    indv2 = v2;
    i__1 = tau1 + *k - 1;
    for (itau1 = tau1; itau1 <= i__1; ++itau1) {
        /*        Compute elementary reflector H(1)_i. */
        x_11__ = dwork[indxc];
        x_12__ = dwork[indxc + 1];
        dwork[indv1] = x_11__;
        dwork[indv1 + 1] = scaloc;
        dlarfg_(&c__2, &dwork[indv1], &dwork[indv1 + 1], &c__1, &dwork[itau1]);
        dwork[indv1] = 1.;
        /*        Compute elementary reflector H(2)_i. */
        dwork[indv2] = x_12__;
        dwork[indv2 + 1] = 0.;
        dlarfx_("Left", &c__2, &c__1, &dwork[indv1], &dwork[itau1], &dwork[indv2], &c__2, &dwork[w],
            4L);
        dwork[indv2] = dwork[indv2 + 1];
        dwork[indv2 + 1] = scaloc;
        dlarfg_(&c__2, &dwork[indv2], &dwork[indv2 + 1], &c__1, &dwork[itau2]);
        dwork[indv2] = 1.;
        /*        Next, do weak stability test. */
        taus[0] = dwork[itau1];
        taus[1] = dwork[itau2];
        v_1__ = dwork[indv1 + 1];
        tmp1 = scaloc * (1. - taus[0]) + taus[0] * v_1__ * x_11__;
        /* Computing 2nd power */
        d__1 = v_1__;
        tmp2
            = -(scaloc * taus[0] * v_1__ + x_11__ * (1. - taus[0] * (d__1 * d__1))) * (1. - taus[1])
            + taus[1] * dwork[indv2 + 1] * x_12__;
        if (dlapy2_(&tmp1, &tmp2) > thresh) {
            goto L300;
        }
        ++itau2;
        indxc += 2;
        indv1 += 2;
        indv2 += 2;
        /* L80: */
    }
    /*     The swap passed weak stability test - move on and perform the */
    /*     swapping temporarily into TT (workspace). */
    itau1 = tau1;
    itau2 = tau2;
    indv1 = v1;
    indv2 = v2;
    indtt = tt;
    ltt = 3;
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        ip1 = i__ % *k;
        iv1p1 = v1 + (ip1 << 1);
        iv2p1 = v2 + (ip1 << 1);
        tau1p1 = tau1 + ip1;
        tau2p1 = tau2 + ip1;
        dlacpy_("All", &c__3, &c__3, &t[iwork[i11 + i__]], &ldt[i__], &dwork[indtt], &c__3, 3L);
        if (s[i__] == 1) {
            dlarfx_("Left", &c__2, &c__3, &dwork[iv1p1], &dwork[tau1p1], &dwork[indtt], &c__3,
                &dwork[w], 4L);
            dlarfx_("Left", &c__2, &c__3, &dwork[iv2p1], &dwork[tau2p1], &dwork[indtt + 1], &c__3,
                &dwork[w], 4L);
            dlarfx_("Right", &c__3, &c__2, &dwork[indv1], &dwork[itau1], &dwork[indtt], &c__3,
                &dwork[w], 5L);
            dlarfx_("Right", &c__3, &c__2, &dwork[indv2], &dwork[itau2], &dwork[indtt + 3], &c__3,
                &dwork[w], 5L);
        } else {
            dlarfx_("Right", &c__3, &c__2, &dwork[iv1p1], &dwork[tau1p1], &dwork[indtt], &c__3,
                &dwork[w], 5L);
            dlarfx_("Right", &c__3, &c__2, &dwork[iv2p1], &dwork[tau2p1], &dwork[indtt + 3], &c__3,
                &dwork[w], 5L);
            dlarfx_("Left", &c__2, &c__3, &dwork[indv1], &dwork[itau1], &dwork[indtt], &c__3,
                &dwork[w], 4L);
            dlarfx_("Left", &c__2, &c__3, &dwork[indv2], &dwork[itau2], &dwork[indtt + 1], &c__3,
                &dwork[w], 4L);
        }
        ++itau1;
        ++itau2;
        indv1 += 2;
        indv2 += 2;
        indtt += 9;
        /* L90: */
    }
    /*     Check for fill-in elements in the new 2-by-2 block. */
    fillin = FALSE_;
    indtt = tt + 1;
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        if (i__ != *kschur && (d__1 = dwork[indtt], abs(d__1)) > thresh) {
            fillin = TRUE_;
        }
        indtt += 9;
        /* L100: */
    }
    /*     Found fill-in elements? */
    if (fillin) {
        /*        Restore periodic Schur form. */
        mb03kc_(k, kschur, &ltt, &c__1, &s[1], &dwork[tt], &ltt, &dwork[vloc], &dwork[ltau]);
    }
    if (*ws) {
        /*        Perform strong stability test. */
        itau1 = tau1;
        itau2 = tau2;
        itauf = ltau;
        indv1 = v1;
        indv2 = v2;
        indvf = vloc;
        indtt = tt;
        i__1 = *k;
        for (i__ = 1; i__ <= i__1; ++i__) {
            ip1 = i__ % *k;
            dlacpy_("All", &c__3, &c__3, &dwork[indtt], &c__3, tempm1, &c__3, 3L);
            /*           Apply possible transformations from fill-in removal. */
            if (fillin) {
                indvp1 = vloc + (ip1 << 1);
                itaup1 = ltau + ip1;
                /*              Apply on top-left 2-by-2 block. */
                if (s[i__] == 1) {
                    dlarfx_("Left", &c__2, &c__3, &dwork[indvp1], &dwork[itaup1], tempm1, &c__3,
                        &dwork[w], 4L);
                    dlarfx_("Right", &c__3, &c__2, &dwork[indvf], &dwork[itauf], tempm1, &c__3,
                        &dwork[w], 5L);
                } else {
                    dlarfx_("Right", &c__3, &c__2, &dwork[indvp1], &dwork[itaup1], tempm1, &c__3,
                        &dwork[w], 5L);
                    dlarfx_("Left", &c__2, &c__3, &dwork[indvf], &dwork[itauf], tempm1, &c__3,
                        &dwork[w], 4L);
                }
            }
            /*           Take the "large" transformations. */
            iv1p1 = v1 + (ip1 << 1);
            iv2p1 = v2 + (ip1 << 1);
            tau1p1 = tau1 + ip1;
            tau2p1 = tau2 + ip1;
            /*           Apply H(1)_i+1 * H(2)_i+1 from left or right depending on S. */
            /*           Apply H(2)_i * H(1)_i from right or left depending on S. */
            if (s[i__] == 1) {
                dlarfx_("Left", &c__2, &c__3, &dwork[iv2p1], &dwork[tau2p1], &tempm1[1], &c__3,
                    &dwork[w], 4L);
                dlarfx_("Left", &c__2, &c__3, &dwork[iv1p1], &dwork[tau1p1], tempm1, &c__3,
                    &dwork[w], 4L);
                dlarfx_("Right", &c__3, &c__2, &dwork[indv2], &dwork[itau2], &tempm1[3], &c__3,
                    &dwork[w], 5L);
                dlarfx_("Right", &c__3, &c__2, &dwork[indv1], &dwork[itau1], tempm1, &c__3,
                    &dwork[w], 5L);
            } else {
                dlarfx_("Right", &c__3, &c__2, &dwork[iv2p1], &dwork[tau2p1], &tempm1[3], &c__3,
                    &dwork[w], 5L);
                dlarfx_("Right", &c__3, &c__2, &dwork[iv1p1], &dwork[tau1p1], tempm1, &c__3,
                    &dwork[w], 5L);
                dlarfx_("Left", &c__2, &c__3, &dwork[indv2], &dwork[itau2], &tempm1[1], &c__3,
                    &dwork[w], 4L);
                dlarfx_("Left", &c__2, &c__3, &dwork[indv1], &dwork[itau1], tempm1, &c__3,
                    &dwork[w], 4L);
            }
            /*           Compute residual norm. */
            dlacpy_("All", &c__3, &c__3, &t[iwork[i11 + i__]], &ldt[i__], temp, &c__3, 3L);
            daxpy_(&nd2, &c_b92, temp, &c__1, tempm1, &c__1);
            strong = dlange_("Frobenius", &c__3, &c__3, tempm1, &c__3, &dwork[1], 9L);
            if (strong > thresh) {
                goto L300;
            }
            ++itau1;
            ++itau2;
            ++itauf;
            indv1 += 2;
            indv2 += 2;
            indvf += 2;
            indtt += 9;
            /* L110: */
        }
    }
    /*     The swap was accepted - now update T and Q with respect to the */
    /*     swapping. */
    itau1 = tau1;
    itau2 = tau2;
    itauf = ltau;
    indv1 = v1;
    indv2 = v2;
    indvf = vloc;
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        ip1 = i__ % *k;
        it = iwork[i11 + i__] - *j1 + 1;
        /*        Apply Householder transformations from left or right depending */
        /*        on S and accumulate transformations in matrices Q_i, i = 1:K. */
        iv1p1 = v1 + (ip1 << 1);
        iv2p1 = v2 + (ip1 << 1);
        tau1p1 = tau1 + ip1;
        tau2p1 = tau2 + ip1;
        ++ip1;
        if (s[i__] == 1) {
            it -= ni[ip1];
            it2 = it + ldt[i__];
            i__2 = n[i__] - *j1 + 1;
            dlarfx_("Left", &c__2, &i__2, &dwork[iv1p1], &dwork[tau1p1], &t[iwork[i11 + i__]],
                &ldt[i__], &dwork[w], 4L);
            i__2 = n[i__] - *j1 + 1;
            dlarfx_("Left", &c__2, &i__2, &dwork[iv2p1], &dwork[tau2p1], &t[iwork[i21 + i__]],
                &ldt[i__], &dwork[w], 4L);
            i__2 = ni[ip1] + j3;
            dlarfx_("Right", &i__2, &c__2, &dwork[indv1], &dwork[itau1], &t[it], &ldt[i__],
                &dwork[w], 5L);
            i__2 = ni[ip1] + j3;
            dlarfx_("Right", &i__2, &c__2, &dwork[indv2], &dwork[itau2], &t[it2], &ldt[i__],
                &dwork[w], 5L);
        } else {
            it -= ni[i__];
            it2 = it + ldt[i__];
            i__2 = n[ip1] - *j1 + 1;
            dlarfx_("Left", &c__2, &i__2, &dwork[indv1], &dwork[itau1], &t[iwork[i11 + i__]],
                &ldt[i__], &dwork[w], 4L);
            i__2 = n[ip1] - *j1 + 1;
            dlarfx_("Left", &c__2, &i__2, &dwork[indv2], &dwork[itau2], &t[iwork[i21 + i__]],
                &ldt[i__], &dwork[w], 4L);
            i__2 = ni[i__] + j3;
            dlarfx_("Right", &i__2, &c__2, &dwork[iv1p1], &dwork[tau1p1], &t[it], &ldt[i__],
                &dwork[w], 5L);
            i__2 = ni[i__] + j3;
            dlarfx_("Right", &i__2, &c__2, &dwork[iv2p1], &dwork[tau2p1], &t[it2], &ldt[i__],
                &dwork[w], 5L);
        }
        wantql = wantq;
        if (specq) {
            wantql = whichq[i__] != 0;
        }
        if (wantql) {
            iq = ixq[i__] + (*j1 - 1) * ldq[i__];
            dlarfx_("Right", &n[i__], &c__2, &dwork[indv1], &dwork[itau1], &q[iq], &ldq[i__],
                &dwork[w], 5L);
            iq += ldq[i__];
            dlarfx_("Right", &n[i__], &c__2, &dwork[indv2], &dwork[itau2], &q[iq], &ldq[i__],
                &dwork[w], 5L);
        }
        /*        Apply Householder transformations from fill-in removal and */
        /*        accumulate transformations in matrices Q_i, i=1,...,K. */
        if (fillin) {
            iv1p1 = vloc + (ip1 - 1 << 1);
            tau1p1 = ltau + (ip1 - 1);
            if (s[i__] == 1) {
                i__2 = n[i__] - *j1 + 1;
                dlarfx_("Left", &c__2, &i__2, &dwork[iv1p1], &dwork[tau1p1], &t[iwork[i11 + i__]],
                    &ldt[i__], &dwork[w], 4L);
                i__2 = ni[ip1] + j2;
                dlarfx_("Right", &i__2, &c__2, &dwork[indvf], &dwork[itauf], &t[it], &ldt[i__],
                    &dwork[w], 5L);
            } else {
                i__2 = ni[i__] + j2;
                dlarfx_("Right", &i__2, &c__2, &dwork[iv1p1], &dwork[tau1p1], &t[it], &ldt[i__],
                    &dwork[w], 5L);
                i__2 = n[ip1] - *j1 + 1;
                dlarfx_("Left", &c__2, &i__2, &dwork[indvf], &dwork[itauf], &t[iwork[i11 + i__]],
                    &ldt[i__], &dwork[w], 4L);
            }
            wantql = wantq;
            if (specq) {
                wantql = whichq[i__] != 0;
            }
            if (wantql) {
                iq = ixq[i__] + (*j1 - 1) * ldq[i__];
                dlarfx_("Right", &n[i__], &c__2, &dwork[indvf], &dwork[itauf], &q[iq], &ldq[i__],
                    &dwork[w], 5L);
            }
        }
        ++itau1;
        ++itau2;
        ++itauf;
        indv1 += 2;
        indv2 += 2;
        indvf += 2;
        /* L120: */
    }
    /*     Set to zero the fill-in elements. */
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        t[iwork[i21 + i__] + 1] = 0.;
        t[iwork[i22 + i__] + 1] = 0.;
        if (i__ != *kschur) {
            t[iwork[i21 + i__]] = 0.;
        }
        /* L130: */
    }
    /*     Exit direct swap N1 = 1 and N2 = 2. */
    goto L290;
    /*     Direct swap with N1 = 2 and N2 = 1. */
L140:
    /*     Generate elementary reflectors H_i such that: */
    /*     H_i( X_11_i ) = ( * ). */
    /*        ( X_21_i )   ( 0 ) */
    /*        (  scale )   ( 0 ) */
    indxc = c__;
    indxv = v;
    i__1 = tau + *k - 1;
    for (indtau = tau; indtau <= i__1; ++indtau) {
        x_11__ = dwork[indxc];
        x_21__ = dwork[indxc + 1];
        dwork[indxv] = x_11__;
        dwork[indxv + 1] = x_21__;
        dwork[indxv + 2] = scaloc;
        dlarfg_(&c__3, &dwork[indxv], &dwork[indxv + 1], &c__1, &dwork[indtau]);
        dwork[indxv] = 1.;
        /*        Next, do weak stability test: check that */
        /*        ||H_11_i - X_i * H_21_i||_F <= tol, i = 1, ..., K. */
        v_2__ = dwork[indxv + 2];
        tauloc = dwork[indtau];
        tmp1 = scaloc * (1. - tauloc) + tauloc * v_2__ * x_11__;
        tmp2 = tauloc * (v_2__ * x_21__ - scaloc * dwork[indxv + 1]);
        if (dlapy2_(&tmp1, &tmp2) > thresh) {
            goto L300;
        }
        indxc += 2;
        indxv += 3;
        /* L150: */
    }
    /*     The swap passed weak stability test - move on and perform the */
    /*     swapping temporarily into TT (workspace). */
    indtau = tau;
    indxv = v;
    indtt = tt;
    ltt = 3;
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        ip1 = i__ % *k;
        indvp1 = v + ip1 * 3;
        itaup1 = tau + ip1;
        dlacpy_("All", &c__3, &c__3, &t[iwork[i11 + i__]], &ldt[i__], &dwork[indtt], &c__3, 3L);
        if (s[i__] == 1) {
            dlarfx_("Left", &c__3, &c__3, &dwork[indvp1], &dwork[itaup1], &dwork[indtt], &c__3,
                &dwork[w], 4L);
            dlarfx_("Right", &c__3, &c__3, &dwork[indxv], &dwork[indtau], &dwork[indtt], &c__3,
                &dwork[w], 5L);
        } else {
            dlarfx_("Right", &c__3, &c__3, &dwork[indvp1], &dwork[itaup1], &dwork[indtt], &c__3,
                &dwork[w], 5L);
            dlarfx_("Left", &c__3, &c__3, &dwork[indxv], &dwork[indtau], &dwork[indtt], &c__3,
                &dwork[w], 4L);
        }
        ++indtau;
        indxv += 3;
        indtt += 9;
        /* L160: */
    }
    /*     Check for fill-in elements. */
    fillin = FALSE_;
    indtt = tt + 5;
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        if (i__ != *kschur && (d__1 = dwork[indtt], abs(d__1)) > thresh) {
            fillin = TRUE_;
        }
        indtt += 9;
        /* L170: */
    }
    /*     Found fill-in elements? */
    if (fillin) {
        /*        Restore periodic Schur form. */
        mb03kc_(k, kschur, &ltt, &c__2, &s[1], &dwork[tt], &ltt, &dwork[vloc], &dwork[ltau]);
    }
    if (*ws) {
        /*        Perform strong stability test. */
        indtau = tau;
        indxv = v;
        itauf = ltau;
        indvf = vloc;
        indtt = tt;
        i__1 = *k;
        for (i__ = 1; i__ <= i__1; ++i__) {
            ip1 = i__ % *k;
            dlacpy_("All", &c__3, &c__3, &dwork[indtt], &c__3, tempm1, &c__3, 3L);
            if (fillin) {
                indvp1 = vloc + (ip1 << 1);
                itaup1 = ltau + ip1;
                if (s[i__] == 1) {
                    dlarfx_("Left", &c__2, &c__2, &dwork[indvp1], &dwork[itaup1], &tempm1[4], &c__3,
                        &dwork[w], 4L);
                    dlarfx_("Right", &c__3, &c__2, &dwork[indvf], &dwork[itauf], &tempm1[3], &c__3,
                        &dwork[w], 5L);
                } else {
                    dlarfx_("Right", &c__3, &c__2, &dwork[indvp1], &dwork[itaup1], &tempm1[3],
                        &c__3, &dwork[w], 5L);
                    dlarfx_("Left", &c__2, &c__2, &dwork[indvf], &dwork[itauf], &tempm1[4], &c__3,
                        &dwork[w], 4L);
                }
            }
            indvp1 = v + ip1 * 3;
            itaup1 = tau + ip1;
            if (s[i__] == 1) {
                dlarfx_("Left", &c__3, &c__3, &dwork[indvp1], &dwork[itaup1], tempm1, &c__3,
                    &dwork[w], 4L);
                dlarfx_("Right", &c__3, &c__3, &dwork[indxv], &dwork[indtau], tempm1, &c__3,
                    &dwork[w], 5L);
            } else {
                dlarfx_("Right", &c__3, &c__3, &dwork[indvp1], &dwork[itaup1], tempm1, &c__3,
                    &dwork[w], 5L);
                dlarfx_("Left", &c__3, &c__3, &dwork[indxv], &dwork[indtau], tempm1, &c__3,
                    &dwork[w], 4L);
            }
            dlacpy_("All", &c__3, &c__3, &t[iwork[i11 + i__]], &ldt[i__], temp, &c__3, 3L);
            daxpy_(&nd2, &c_b92, temp, &c__1, tempm1, &c__1);
            strong = dlange_("Frobenius", &c__3, &c__3, tempm1, &c__3, &dwork[1], 9L);
            if (strong > thresh) {
                goto L300;
            }
            ++indtau;
            indxv += 3;
            ++itauf;
            indvf += 2;
            indtt += 9;
            /* L180: */
        }
    }
    /*     The swap was accepted - now update T and Q with respect to the */
    /*     swapping. */
    indtau = tau;
    indxv = v;
    itauf = ltau;
    indvf = vloc;
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        ip1 = i__ % *k;
        it = iwork[i11 + i__] - *j1 + 1;
        /*        Apply Householder transformations from left or right depending */
        /*        on S and accumulate transformations in matrices Q_i, i = 1:K. */
        indvp1 = v + ip1 * 3;
        itaup1 = tau + ip1;
        ++ip1;
        if (s[i__] == 1) {
            it -= ni[ip1];
            i__2 = n[i__] - *j1 + 1;
            dlarfx_("Left", &c__3, &i__2, &dwork[indvp1], &dwork[itaup1], &t[iwork[i11 + i__]],
                &ldt[i__], &dwork[w], 4L);
            i__2 = ni[ip1] + j3;
            dlarfx_("Right", &i__2, &c__3, &dwork[indxv], &dwork[indtau], &t[it], &ldt[i__],
                &dwork[w], 5L);
        } else {
            it -= ni[i__];
            i__2 = n[ip1] - *j1 + 1;
            dlarfx_("Left", &c__3, &i__2, &dwork[indxv], &dwork[indtau], &t[iwork[i11 + i__]],
                &ldt[i__], &dwork[w], 4L);
            i__2 = ni[i__] + j3;
            dlarfx_("Right", &i__2, &c__3, &dwork[indvp1], &dwork[itaup1], &t[it], &ldt[i__],
                &dwork[w], 5L);
        }
        wantql = wantq;
        if (specq) {
            wantql = whichq[i__] != 0;
        }
        if (wantql) {
            iq = ixq[i__] + (*j1 - 1) * ldq[i__];
            dlarfx_("Right", &n[i__], &c__3, &dwork[indxv], &dwork[indtau], &q[iq], &ldq[i__],
                &dwork[w], 5L);
        }
        /*        Apply Householder transformations from fill-in removal and */
        /*        accumulate transformations in matrices Q_i, i=1,...,K. */
        if (fillin) {
            indvp1 = vloc + (ip1 - 1 << 1);
            itaup1 = ltau + ip1 - 1;
            it2 = it + ldt[i__];
            if (s[i__] == 1) {
                i__2 = n[i__] - *j1;
                dlarfx_("Left", &c__2, &i__2, &dwork[indvp1], &dwork[itaup1], &t[it2 + *j1],
                    &ldt[i__], &dwork[w], 4L);
                i__2 = ni[ip1] + j3;
                dlarfx_("Right", &i__2, &c__2, &dwork[indvf], &dwork[itauf], &t[it2], &ldt[i__],
                    &dwork[w], 5L);
            } else {
                i__2 = n[ip1] - *j1;
                dlarfx_("Left", &c__2, &i__2, &dwork[indvf], &dwork[itauf], &t[it2 + *j1],
                    &ldt[i__], &dwork[w], 4L);
                i__2 = ni[i__] + j3;
                dlarfx_("Right", &i__2, &c__2, &dwork[indvp1], &dwork[itaup1], &t[it2], &ldt[i__],
                    &dwork[w], 5L);
            }
            wantql = wantq;
            if (specq) {
                wantql = whichq[i__] != 0;
            }
            if (wantql) {
                iq = ixq[i__] + *j1 * ldq[i__];
                dlarfx_("Right", &n[i__], &c__2, &dwork[indvf], &dwork[itauf], &q[iq], &ldq[i__],
                    &dwork[w], 5L);
            }
        }
        ++indtau;
        indxv += 3;
        ++itauf;
        indvf += 2;
        /* L190: */
    }
    /*     Set to zero the fill-in elements below the main diagonal. */
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        it = iwork[i11 + i__] + 1;
        t[it] = 0.;
        t[it + 1] = 0.;
        if (i__ != *kschur) {
            t[it + ldt[i__] + 1] = 0.;
        }
        /* L200: */
    }
    /*     Exit direct swap N1 = 2 and N2 = 1. */
    goto L290;
    /*     Direct swap with N1 = 2 and N2 = 2. */
L210:
    /*     Generate elementary reflectors H(1)_i and H(2)_i such that */
    /*     H(2)_i H(1)_i  ( X_11_i X_12_i ) = ( * * ). */
    /*                    ( X_21_i X_22_i )   ( 0 * ) */
    /*                    (  scale    0   )   ( 0 0 ) */
    /*                    (   0     scale )   ( 0 0 ) */
    indxc = c__;
    itau2 = tau2;
    indv1 = v1;
    indv2 = v2;
    i__1 = tau1 + *k - 1;
    for (itau1 = tau1; itau1 <= i__1; ++itau1) {
        x_11__ = dwork[indxc];
        x_21__ = dwork[indxc + 1];
        x_12__ = dwork[indxc + 2];
        x_22__ = dwork[indxc + 3];
        /*        Compute elementary reflector H(1)_i. */
        dwork[indv1] = x_11__;
        dwork[indv1 + 1] = x_21__;
        dwork[indv1 + 2] = scaloc;
        dlarfg_(&c__3, &dwork[indv1], &dwork[indv1 + 1], &c__1, &dwork[itau1]);
        dwork[indv1] = 1.;
        /*        Compute elementary reflector H(2)_i. */
        dwork[indv2] = x_12__;
        dwork[indv2 + 1] = x_22__;
        dwork[indv2 + 2] = 0.;
        dlarfx_("Left", &c__3, &c__1, &dwork[indv1], &dwork[itau1], &dwork[indv2], &c__3, &dwork[w],
            4L);
        dwork[indv2] = dwork[indv2 + 1];
        dwork[indv2 + 1] = dwork[indv2 + 2];
        dwork[indv2 + 2] = scaloc;
        dlarfg_(&c__3, &dwork[indv2], &dwork[indv2 + 1], &c__1, &dwork[itau2]);
        dwork[indv2] = 1.;
        /*        Next, do weak stability test: check that */
        /*        ||QQ_11_i - X_i * QQ_21_i||_F <= tol, i = 1, ...,K, */
        /*        where QQ_i = H(1)_i * H(2)_i. */
        v_2__ = dwork[indv1 + 1];
        v_3__ = dwork[indv1 + 2];
        w_2__ = dwork[indv2 + 1];
        w_3__ = dwork[indv2 + 2];
        dtau1 = dwork[itau1];
        dtau2 = dwork[itau2];
        temp[0] = scaloc * (1. - dtau1) + x_11__ * dtau1 * v_3__;
        /* Computing 2nd power */
        d__1 = v_3__;
        temp[2] = scaloc * (dtau2 * w_2__ * dtau1 * v_3__ - dtau1 * v_2__ * (1. - dtau2))
            - x_11__
                * (-dtau1 * v_2__ * v_3__ * (1. - dtau2)
                      - (1. - dtau1 * (d__1 * d__1)) * dtau2 * w_2__)
            + x_12__ * dtau2 * w_3__;
        temp[1] = -scaloc * dtau1 * v_2__ + x_21__ * dtau1 * v_3__;
        /* Computing 2nd power */
        d__1 = v_2__;
        /* Computing 2nd power */
        d__2 = v_3__;
        temp[3] = scaloc
                * ((1. - dtau1 * (d__1 * d__1)) * (1. - dtau2)
                      + dtau1 * v_2__ * v_3__ * dtau2 * w_2__)
            - x_21__
                * (-dtau1 * v_2__ * v_3__ * (1. - dtau2)
                      - (1. - dtau1 * (d__2 * d__2)) * dtau2 * w_2__)
            + x_22__ * dtau2 * w_3__;
        if (dlange_("Frobenius", &c__2, &c__2, temp, &c__2, &dwork[1], 9L) > thresh) {
            goto L300;
        }
        indxc += 4;
        ++itau2;
        indv1 += 3;
        indv2 += 3;
        /* L220: */
    }
    /*     The swap passed weak stability test - move on and perform the */
    /*     swapping temporarily into TT (workspace). */
    itau1 = tau1;
    itau2 = tau2;
    indv1 = v1;
    indv2 = v2;
    indtt = tt;
    ltt = 4;
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        ip1 = i__ % *k;
        iv1p1 = v1 + ip1 * 3;
        iv2p1 = v2 + ip1 * 3;
        tau1p1 = tau1 + ip1;
        tau2p1 = tau2 + ip1;
        dlacpy_("All", &c__4, &c__4, &t[iwork[i11 + i__]], &ldt[i__], &dwork[indtt], &c__4, 3L);
        if (s[i__] == 1) {
            dlarfx_("Left", &c__3, &c__4, &dwork[iv1p1], &dwork[tau1p1], &dwork[indtt], &c__4,
                &dwork[w], 4L);
            dlarfx_("Left", &c__3, &c__4, &dwork[iv2p1], &dwork[tau2p1], &dwork[indtt + 1], &c__4,
                &dwork[w], 4L);
            dlarfx_("Right", &c__4, &c__3, &dwork[indv1], &dwork[itau1], &dwork[indtt], &c__4,
                &dwork[w], 5L);
            dlarfx_("Right", &c__4, &c__3, &dwork[indv2], &dwork[itau2], &dwork[indtt + 4], &c__4,
                &dwork[w], 5L);
        } else {
            dlarfx_("Right", &c__4, &c__3, &dwork[iv1p1], &dwork[tau1p1], &dwork[indtt], &c__4,
                &dwork[w], 5L);
            dlarfx_("Right", &c__4, &c__3, &dwork[iv2p1], &dwork[tau2p1], &dwork[indtt + 4], &c__4,
                &dwork[w], 5L);
            dlarfx_("Left", &c__3, &c__4, &dwork[indv1], &dwork[itau1], &dwork[indtt], &c__4,
                &dwork[w], 4L);
            dlarfx_("Left", &c__3, &c__4, &dwork[indv2], &dwork[itau2], &dwork[indtt + 1], &c__4,
                &dwork[w], 4L);
        }
        ++itau1;
        ++itau2;
        indv1 += 3;
        indv2 += 3;
        indtt += 16;
        /* L230: */
    }
    /*     Check for fill-in elements. */
    fillin = FALSE_;
    fill21 = FALSE_;
    indtt = tt + 1;
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        if (i__ != *kschur && (d__1 = dwork[indtt], abs(d__1)) > thresh) {
            fillin = TRUE_;
            fill21 = TRUE_;
        }
        indtt += 16;
        /* L240: */
    }
    /*     Found fill-in elements? */
    if (fillin) {
        /*        Restore periodic Schur form. */
        mb03kc_(k, kschur, &ltt, &c__1, &s[1], &dwork[tt], &ltt, &dwork[vloc1], &dwork[ltau1]);
    }
    /*     Check for fill-in elements again. */
    fillin = FALSE_;
    fill43 = FALSE_;
    indtt = tt + 11;
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        if (i__ != *kschur && (d__1 = dwork[indtt], abs(d__1)) > eps) {
            fillin = TRUE_;
            fill43 = TRUE_;
        }
        indtt += 16;
        /* L250: */
    }
    /*     Found fill-in elements? */
    if (fillin) {
        /*        Restore periodic Schur form. */
        mb03kc_(k, kschur, &ltt, &c__3, &s[1], &dwork[tt], &ltt, &dwork[vloc2], &dwork[ltau2]);
    }
    if (*ws) {
        /*        Perform strong stability test. */
        itau1 = tau1;
        itau2 = tau2;
        indv1 = v1;
        indv2 = v2;
        indtt = tt;
        if (fillin) {
            itauf1 = ltau1;
            itauf2 = ltau2;
            indf1 = vloc1;
            indf2 = vloc2;
        }
        i__1 = *k;
        for (i__ = 1; i__ <= i__1; ++i__) {
            ip1 = i__ % *k;
            dlacpy_("All", &c__4, &c__4, &dwork[indtt], &c__4, tempm1, &c__4, 3L);
            /*           Apply possible transformations from fill-in removal. */
            if (fillin) {
                iv1p1 = vloc1 + (ip1 << 1);
                iv2p1 = vloc2 + (ip1 << 1);
                tau1p1 = ltau1 + ip1;
                tau2p1 = ltau2 + ip1;
                /*              Apply on top-left 2-by-2 block. */
                if (fill21) {
                    if (s[i__] == 1) {
                        dlarfx_("Left", &c__2, &c__4, &dwork[iv1p1], &dwork[tau1p1], tempm1, &c__4,
                            &dwork[w], 4L);
                        dlarfx_("Right", &c__2, &c__2, &dwork[indf1], &dwork[itauf1], tempm1, &c__4,
                            &dwork[w], 5L);
                    } else {
                        dlarfx_("Right", &c__2, &c__2, &dwork[iv1p1], &dwork[tau1p1], tempm1, &c__4,
                            &dwork[w], 5L);
                        dlarfx_("Left", &c__2, &c__4, &dwork[indf1], &dwork[itauf1], tempm1, &c__4,
                            &dwork[w], 4L);
                    }
                }
                /*              Apply on down-right 2-by-2 block. */
                if (fill43) {
                    if (s[i__] == 1) {
                        dlarfx_("Left", &c__2, &c__2, &dwork[iv2p1], &dwork[tau2p1], &tempm1[10],
                            &c__4, &dwork[w], 4L);
                        dlarfx_("Right", &c__4, &c__2, &dwork[indf2], &dwork[itauf2], &tempm1[8],
                            &c__4, &dwork[w], 5L);
                    } else {
                        dlarfx_("Right", &c__4, &c__2, &dwork[iv2p1], &dwork[tau2p1], &tempm1[8],
                            &c__4, &dwork[w], 5L);
                        dlarfx_("Left", &c__2, &c__2, &dwork[indf2], &dwork[itauf2], &tempm1[10],
                            &c__4, &dwork[w], 4L);
                    }
                }
            }
            /*           Take the "large" transformations. */
            iv1p1 = v1 + ip1 * 3;
            iv2p1 = v2 + ip1 * 3;
            tau1p1 = tau1 + ip1;
            tau2p1 = tau2 + ip1;
            /*           Apply H(2)_i+1, H(1)_i+1, H(2)_i, H(1)_i from left or right */
            /*           depending on S. */
            if (s[i__] == 1) {
                dlarfx_("Left", &c__3, &c__4, &dwork[iv2p1], &dwork[tau2p1], &tempm1[1], &c__4,
                    &dwork[w], 4L);
                dlarfx_("Left", &c__3, &c__4, &dwork[iv1p1], &dwork[tau1p1], tempm1, &c__4,
                    &dwork[w], 4L);
                dlarfx_("Right", &c__4, &c__3, &dwork[indv2], &dwork[itau2], &tempm1[4], &c__4,
                    &dwork[w], 5L);
                dlarfx_("Right", &c__4, &c__3, &dwork[indv1], &dwork[itau1], tempm1, &c__4,
                    &dwork[w], 5L);
            } else {
                dlarfx_("Right", &c__4, &c__3, &dwork[iv2p1], &dwork[tau2p1], &tempm1[4], &c__4,
                    &dwork[w], 5L);
                dlarfx_("Right", &c__4, &c__3, &dwork[iv1p1], &dwork[tau1p1], tempm1, &c__4,
                    &dwork[w], 5L);
                dlarfx_("Left", &c__3, &c__4, &dwork[indv2], &dwork[itau2], &tempm1[1], &c__4,
                    &dwork[w], 4L);
                dlarfx_("Left", &c__3, &c__4, &dwork[indv1], &dwork[itau1], tempm1, &c__4,
                    &dwork[w], 4L);
            }
            /*           Compute residual norm. */
            dlacpy_("All", &c__4, &c__4, &t[iwork[i11 + i__]], &ldt[i__], temp, &c__4, 3L);
            daxpy_(&nd2, &c_b92, temp, &c__1, tempm1, &c__1);
            strong = dlange_("Frobenius", &c__4, &c__4, tempm1, &c__4, &dwork[1], 9L);
            if (strong > thresh) {
                goto L300;
            }
            ++itau1;
            ++itau2;
            indv1 += 3;
            indv2 += 3;
            indtt += 16;
            if (fillin) {
                ++itauf1;
                ++itauf2;
                indf1 += 2;
                indf2 += 2;
            }
            /* L260: */
        }
    }
    /*     The swap was accepted - now update T and Q with respect to the */
    /*     swapping. */
    itau1 = tau1;
    itau2 = tau2;
    indv1 = v1;
    indv2 = v2;
    if (fillin) {
        itauf1 = ltau1;
        itauf2 = ltau2;
        indf1 = vloc1;
        indf2 = vloc2;
    }
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        ip1 = i__ % *k;
        ipp = ip1 + 1;
        it = iwork[i11 + i__] - *j1 + 1;
        /*        Apply Householder transformations from left or right depending */
        /*        on S and accumulate transformations in matrices Q_i, i = 1:K. */
        iv1p1 = v1 + ip1 * 3;
        iv2p1 = v2 + ip1 * 3;
        tau1p1 = tau1 + ip1;
        tau2p1 = tau2 + ip1;
        if (s[i__] == 1) {
            it -= ni[ipp];
            it2 = it + ldt[i__];
            i__2 = n[i__] - *j1 + 1;
            dlarfx_("Left", &c__3, &i__2, &dwork[iv1p1], &dwork[tau1p1], &t[iwork[i11 + i__]],
                &ldt[i__], &dwork[w], 4L);
            i__2 = n[i__] - *j1 + 1;
            dlarfx_("Left", &c__3, &i__2, &dwork[iv2p1], &dwork[tau2p1], &t[iwork[i11 + i__] + 1],
                &ldt[i__], &dwork[w], 4L);
            i__2 = ni[ipp] + j4;
            dlarfx_("Right", &i__2, &c__3, &dwork[indv1], &dwork[itau1], &t[it], &ldt[i__],
                &dwork[w], 5L);
            i__2 = ni[ipp] + j4;
            dlarfx_("Right", &i__2, &c__3, &dwork[indv2], &dwork[itau2], &t[it2], &ldt[i__],
                &dwork[w], 5L);
        } else {
            it -= ni[i__];
            it2 = it + ldt[i__];
            i__2 = ni[i__] + j4;
            dlarfx_("Right", &i__2, &c__3, &dwork[iv1p1], &dwork[tau1p1], &t[it], &ldt[i__],
                &dwork[w], 5L);
            i__2 = ni[i__] + j4;
            dlarfx_("Right", &i__2, &c__3, &dwork[iv2p1], &dwork[tau2p1], &t[it2], &ldt[i__],
                &dwork[w], 5L);
            i__2 = n[ipp] - *j1 + 1;
            dlarfx_("Left", &c__3, &i__2, &dwork[indv1], &dwork[itau1], &t[iwork[i11 + i__]],
                &ldt[i__], &dwork[w], 4L);
            i__2 = n[ipp] - *j1 + 1;
            dlarfx_("Left", &c__3, &i__2, &dwork[indv2], &dwork[itau2], &t[iwork[i11 + i__] + 1],
                &ldt[i__], &dwork[w], 4L);
        }
        wantql = wantq;
        if (specq) {
            wantql = whichq[i__] != 0;
        }
        if (wantql) {
            iq = ixq[i__] + (*j1 - 1) * ldq[i__];
            dlarfx_("Right", &n[i__], &c__3, &dwork[indv1], &dwork[itau1], &q[iq], &ldq[i__],
                &dwork[w], 5L);
            iq += ldq[i__];
            dlarfx_("Right", &n[i__], &c__3, &dwork[indv2], &dwork[itau2], &q[iq], &ldq[i__],
                &dwork[w], 5L);
        }
        /*        Apply Householder transformations from fill-in removal and */
        /*        accumulate transformations. */
        if (fillin) {
            iv1p1 = vloc1 + (ip1 << 1);
            iv2p1 = vloc2 + (ip1 << 1);
            tau1p1 = ltau1 + ip1;
            tau2p1 = ltau2 + ip1;
            if (fill21) {
                if (s[i__] == 1) {
                    i__2 = n[i__] - *j1 + 1;
                    dlarfx_("Left", &c__2, &i__2, &dwork[iv1p1], &dwork[tau1p1],
                        &t[iwork[i11 + i__]], &ldt[i__], &dwork[w], 4L);
                    i__2 = ni[ipp] + j2;
                    dlarfx_("Right", &i__2, &c__2, &dwork[indf1], &dwork[itauf1], &t[it], &ldt[i__],
                        &dwork[w], 5L);
                } else {
                    i__2 = ni[i__] + j2;
                    dlarfx_("Right", &i__2, &c__2, &dwork[iv1p1], &dwork[tau1p1], &t[it], &ldt[i__],
                        &dwork[w], 5L);
                    i__2 = n[ipp] - *j1 + 1;
                    dlarfx_("Left", &c__2, &i__2, &dwork[indf1], &dwork[itauf1],
                        &t[iwork[i11 + i__]], &ldt[i__], &dwork[w], 4L);
                }
            }
            if (fill43) {
                it = iwork[i22 + i__];
                it2 += ldt[i__];
                if (s[i__] == 1) {
                    i__2 = n[i__] - j2;
                    dlarfx_("Left", &c__2, &i__2, &dwork[iv2p1], &dwork[tau2p1], &t[it], &ldt[i__],
                        &dwork[w], 4L);
                    i__2 = ni[ipp] + j4;
                    dlarfx_("Right", &i__2, &c__2, &dwork[indf2], &dwork[itauf2], &t[it2],
                        &ldt[i__], &dwork[w], 5L);
                } else {
                    i__2 = ni[i__] + j4;
                    dlarfx_("Right", &i__2, &c__2, &dwork[iv2p1], &dwork[tau2p1], &t[it2],
                        &ldt[i__], &dwork[w], 5L);
                    i__2 = n[ipp] - j2;
                    dlarfx_("Left", &c__2, &i__2, &dwork[indf2], &dwork[itauf2], &t[it], &ldt[i__],
                        &dwork[w], 4L);
                }
            }
            wantql = wantq;
            if (specq) {
                wantql = whichq[i__] != 0;
            }
            if (wantql) {
                if (fill21) {
                    iq = ixq[i__] + (*j1 - 1) * ldq[i__];
                    dlarfx_("Right", &n[i__], &c__2, &dwork[indf1], &dwork[itauf1], &q[iq],
                        &ldq[i__], &dwork[w], 5L);
                }
                if (fill43) {
                    iq = ixq[i__] + j2 * ldq[i__];
                    dlarfx_("Right", &n[i__], &c__2, &dwork[indf2], &dwork[itauf2], &q[iq],
                        &ldq[i__], &dwork[w], 5L);
                }
            }
        }
        ++itau1;
        ++itau2;
        indv1 += 3;
        indv2 += 3;
        if (fillin) {
            ++itauf1;
            ++itauf2;
            indf1 += 2;
            indf2 += 2;
        }
        /* L270: */
    }
    /*     Set to zero the fill-in elements below the main diagonal. */
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        it = iwork[i21 + i__];
        t[it] = 0.;
        t[it + 1] = 0.;
        it += ldt[i__];
        t[it] = 0.;
        t[it + 1] = 0.;
        if (i__ != *kschur) {
            t[iwork[i11 + i__] + 1] = 0.;
            t[iwork[i22 + i__] + 1] = 0.;
        }
        /* L280: */
    }
    /*     Exit direct swap N1 = 2 and N2 = 2. */
    /*     Normal exit. */
L290:
    /*     Store optimal workspace values and return. */
    dwork[1] = (doublereal)minwrk;
    return 0;
    /*     Exit with INFO = 1 if swap was rejected. */
L300:
    *info = 1;
    return 0;
    /* *** Last line of MB03KB *** */
} /* mb03kb_ */
