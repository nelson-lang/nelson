/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c_n1 = -1;
static doublereal c_b23 = 0.;
static doublereal c_b24 = 1.;

EXPORTSYMBOL /* Subroutine */ int mb03kd_(compq, whichq, strong, k, nc, kschur, n, ni, s, select, t,
    ldt, ixt, q, ldq, ixq, m, tol, iwork, dwork, ldwork, info, compq_len, strong_len) char* compq;
integer* whichq;
char* strong;
integer *k, *nc, *kschur, *n, *ni, *s;
logical* select;
doublereal* t;
integer *ldt, *ixt;
doublereal* q;
integer *ldq, *ixq, *m;
doublereal* tol;
integer* iwork;
doublereal* dwork;
integer *ldwork, *info;
ftnlen compq_len;
ftnlen strong_len;
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    /* Local variables */
    static logical pair;
    static integer mink;
    static doublereal tola[3];
    static integer minn, maxn, sumd;
    static logical swap;
    static integer i__, l;
    extern /* Subroutine */ int mb03ka_();
    extern logical lsame_();
    static logical specq, initq, wantq;
    static integer ll;
    extern doublereal dlamch_();
    static integer it, ls;
    static logical ws;
    extern /* Subroutine */ int dlaset_(), xerbla_();
    static char compqc[1];
    static logical wantql;
    static integer minsum, ip1, mnwork, nkp1;
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
    /*        T22_K^S(K) * T22_K-1^S(K-1) * ... * T22_1^S(1),             (1) */
    /*     of length K, in the generalized periodic Schur form, */
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
    /*     such that the M selected eigenvalues pointed to by the logical */
    /*     vector SELECT end up in the leading part of the matrix sequence */
    /*     T22_k. */
    /*     Given that N(k) = N(k+1) for all k where S(k) = -1, the T11_k are */
    /*     void and the first M columns of the updated orthogonal */
    /*     transformation matrix sequence Q_1, ..., Q_K span a periodic */
    /*     deflating subspace corresponding to the same eigenvalues. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     COMPQ   CHARACTER*1 */
    /*             Specifies whether to compute the orthogonal transformation */
    /*             matrices Q_k, as follows: */
    /*             = 'N': do not compute any of the matrices Q_k; */
    /*             = 'I': each coefficient of Q is initialized internally to */
    /*                    the identity matrix, and the orthogonal matrices */
    /*                    Q_k are returned, where Q_k, k = 1, ..., K, */
    /*                    performed the reordering; */
    /*             = 'U': each coefficient of Q must contain an orthogonal */
    /*                    matrix Q1_k on entry, and the products Q1_k*Q_k are */
    /*                    returned; */
    /*             = 'W': the computation of each Q_k is specified */
    /*                    individually in the array WHICHQ. */
    /*     WHICHQ  INTEGER array, dimension (K) */
    /*             If COMPQ = 'W', WHICHQ(k) specifies the computation of Q_k */
    /*             as follows: */
    /*             = 0:   do not compute Q_k; */
    /*             = 1:   the kth coefficient of Q is initialized to the */
    /*                    identity matrix, and the orthogonal matrix Q_k is */
    /*                    returned; */
    /*             = 2:   the kth coefficient of Q must contain an orthogonal */
    /*                    matrix Q1_k on entry, and the product Q1_k*Q_k is */
    /*                    returned. */
    /*             This array is not referenced if COMPQ <> 'W'. */
    /*     STRONG  CHARACTER*1 */
    /*             Specifies whether to perform the strong stability tests, */
    /*             as follows: */
    /*             = 'N': do not perform the strong stability tests; */
    /*             = 'S': perform the strong stability tests; often, this is */
    /*                    not needed, and omitting them can save some */
    /*                    computations. */
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
    /*             triangular. All other T22 matrices are upper triangular. */
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
    /*     SELECT  (input) LOGICAL array, dimension (NC) */
    /*             SELECT specifies the eigenvalues in the selected cluster. */
    /*             To select a real eigenvalue w(j), SELECT(j) must be set to */
    /*             .TRUE.. To select a complex conjugate pair of eigenvalues */
    /*             w(j) and w(j+1), corresponding to a 2-by-2 diagonal block, */
    /*             either SELECT(j) or SELECT(j+1) or both must be set to */
    /*             .TRUE.; a complex conjugate pair of eigenvalues must be */
    /*             either both included in the cluster or both excluded. */
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
    /*             COMPQ = 'U', or COMPQ = 'W' and WHICHQ(k) = 2. */
    /*             On exit, if COMPQ = 'I' or COMPQ = 'W' and WHICHQ(k) = 1, */
    /*             Q_k contains the orthogonal matrix that performed the */
    /*             reordering. If COMPQ = 'U', or COMPQ = 'W' and */
    /*             WHICHQ(k) = 2, Q_k is post-multiplied with the orthogonal */
    /*             matrix that performed the reordering. */
    /*             This array is not referenced if COMPQ = 'N'. */
    /*     LDQ     INTEGER array, dimension (K) */
    /*             The leading dimensions of the matrices Q_k in the one- */
    /*             dimensional array Q. */
    /*             LDQ(k) >= max(1,N(k)), if COMPQ = 'I', or COMPQ = 'U', or */
    /*                                       COMPQ = 'W' and WHICHQ(k) > 0; */
    /*             This array is not referenced if COMPQ = 'N'. */
    /*     IXQ     INTEGER array, dimension (K) */
    /*             Start indices of the matrices Q_k in the one-dimensional */
    /*             array Q. */
    /*             This array is not referenced if COMPQ = 'N'. */
    /*     M       (output) INTEGER */
    /*             The number of selected core eigenvalues which were */
    /*             reordered to the top of T22_k. */
    /*     Tolerances */
    /*     TOL     DOUBLE PRECISION */
    /*             The tolerance parameter c. The weak and strong stability */
    /*             tests performed for checking the reordering use a */
    /*             threshold computed by the formula  MAX(c*EPS*NRM, SMLNUM), */
    /*             where NRM is the varying Frobenius norm of the matrices */
    /*             formed by concatenating K pairs of adjacent diagonal */
    /*             blocks of sizes 1 and/or 2 in the T22_k submatrices from */
    /*             (2), which are swapped, and EPS and SMLNUM are the machine */
    /*             precision and safe minimum divided by EPS, respectively */
    /*             (see LAPACK Library routine DLAMCH). The value c should */
    /*             normally be at least 10. */
    /*     Workspace */
    /*     IWORK   INTEGER array, dimension (4*K) */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             On exit, if INFO = 0, DWORK(1) returns the optimal LDWORK. */
    /*     LDWORK  INTEGER */
    /*             The dimension of the array DWORK. */
    /*             LDWORK >= 10*K + MN, if all blocks involved in reordering */
    /*                                  have order 1; */
    /*             LDWORK >= 25*K + MN, if there is at least a block of */
    /*                                  order 2, but no adjacent blocks of */
    /*                                  order 2 are involved in reordering; */
    /*             LDWORK >= MAX(42*K + MN, 80*K - 48), if there is at least */
    /*                                  a pair of adjacent blocks of order 2 */
    /*                                  involved in reordering; */
    /*             where MN = MXN, if MXN > 10, and MN = 0, otherwise, with */
    /*             MXN = MAX(N(k),k=1,...,K). */
    /*             If LDWORK = -1  a workspace query is assumed; the */
    /*             routine only calculates the optimal size of the DWORK */
    /*             array, returns this value as the first entry of the DWORK */
    /*             array, and no error message is issued by XERBLA. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -i, the i-th argument had an illegal */
    /*                   value; */
    /*             = 1:  the reordering of T failed because some eigenvalues */
    /*                   are too close to separate (the problem is very ill- */
    /*                   conditioned); T may have been partially reordered. */
    /*     METHOD */
    /*     An adaptation of the LAPACK Library routine DTGSEN is used. */
    /*     NUMERICAL ASPECTS */
    /*     The implemented method is numerically backward stable. */
    /*     CONTRIBUTOR */
    /*     R. Granat, Umea University, Sweden, Apr. 2008. */
    /*     REVISIONS */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Romania, */
    /*     Mar. 2010, SLICOT Library version of the PEP routine PEP_DTGSEN. */
    /*     V. Sima, July, 2010. */
    /*     KEYWORDS */
    /*     Orthogonal transformation, periodic QZ algorithm, periodic */
    /*     Sylvester-like equations, QZ algorithm. */
    /*     ****************************************************************** */
    /* .. Parameters .. */
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
    /*     .. Local Functions .. */
    /*     .. */
    /*     .. Executable Statements .. */
    /*     Decode and test the input parameters. */
    /* Parameter adjustments */
    --dwork;
    --iwork;
    --ixq;
    --ldq;
    --q;
    --ixt;
    --ldt;
    --t;
    --select;
    --s;
    --ni;
    --n;
    --whichq;
    /* Function Body */
    *info = 0;
    initq = lsame_(compq, "I", 1L, 1L);
    wantq = lsame_(compq, "U", 1L, 1L) || initq;
    specq = lsame_(compq, "W", 1L, 1L);
    ws = lsame_(strong, "S", 1L, 1L);
    /*     Test all input arguments. */
    if (*k < 2) {
        *info = -4;
        /*     Check options for generating orthogonal factors. */
    } else if (!(lsame_(compq, "N", 1L, 1L) || wantq || specq)) {
        *info = -1;
    } else if (!(lsame_(strong, "N", 1L, 1L) || ws)) {
        *info = -3;
    } else if (*tol <= 0.) {
        *info = -18;
    }
    if (*info == 0 && specq) {
        i__1 = *k;
        for (l = 1; l <= i__1; ++l) {
            if (whichq[l] < 0 || whichq[l] > 2) {
                *info = -2;
            }
            /* L10: */
        }
    }
    /*     Check whether any of the dimensions is negative. */
    /*     At the same time the sequence of consecutive sums of dimension */
    /*     differences is formed and its minimum is determined. */
    /*     Also, the maximum of all dimensions is computed. */
    if (*info == 0) {
        sumd = 0;
        mink = *k;
        minsum = 0;
        maxn = 0;
        minn = n[*k];
        i__1 = *k;
        for (l = 1; l <= i__1; ++l) {
            if (l < *k && n[l] < minn) {
                minn = n[l];
            }
            nkp1 = n[l % *k + 1];
            if (n[l] < 0) {
                *info = -7;
            }
            if (s[l] == -1) {
                sumd += nkp1 - n[l];
            }
            if (sumd < minsum) {
                minsum = sumd;
                mink = l;
            }
            /* Computing MAX */
            i__2 = maxn, i__3 = n[l];
            maxn = max(i__2, i__3);
            /*           Check the condition N(l) >= NI(l) + NC >= 0. */
            if (*info == 0 && (n[l] < ni[l] + *nc || ni[l] < 0)) {
                *info = -8;
            }
            /* L20: */
        }
    }
    /*     Check the condition 0 <= NC <= min(N). */
    if (*info == 0 && (*nc < 0 || *nc > minn)) {
        *info = -5;
    }
    /*     Check KSCHUR. */
    if (*info == 0 && (*kschur < 1 || *kschur > *k)) {
        *info = -6;
    }
    /*     Check that the complete sum is zero; otherwise T is singular. */
    if (*info == 0 && sumd != 0) {
        *info = -7;
    }
    /*     Check signatures. */
    if (*info == 0) {
        i__1 = *k;
        for (l = 1; l <= i__1; ++l) {
            if ((i__2 = s[l], abs(i__2)) != 1) {
                *info = -9;
            }
            /* L30: */
        }
    }
    /*     Check the leading dimensions of T_k. */
    if (*info == 0) {
        i__1 = *k;
        for (l = 1; l <= i__1; ++l) {
            nkp1 = n[l % *k + 1];
            if (s[l] == 1) {
                if (ldt[l] < max(1, nkp1)) {
                    *info = -12;
                }
            } else {
                /* Computing MAX */
                i__2 = 1, i__3 = n[l];
                if (ldt[l] < max(i__2, i__3)) {
                    *info = -12;
                }
            }
            /* L40: */
        }
    }
    /*     Check the leading dimensions of Q_k. */
    if (*info == 0 && (wantq || specq)) {
        i__1 = *k;
        for (l = 1; l <= i__1; ++l) {
            wantql = wantq;
            if (specq) {
                wantql = whichq[l] != 0;
            }
            if (wantql) {
                /* Computing MAX */
                i__2 = 1, i__3 = n[l];
                if (ldq[l] < max(i__2, i__3)) {
                    *info = -15;
                }
            }
            /* L50: */
        }
    }
    /*     Set M to the dimension of the specified periodic invariant */
    /*     subspace. */
    *m = 0;
    i__ = *kschur;
    pair = FALSE_;
    ip1 = i__ % *k + 1;
    i__1 = *nc;
    for (l = 1; l <= i__1; ++l) {
        if (pair) {
            pair = FALSE_;
        } else {
            if (l < *nc) {
                if (s[i__] == 1) {
                    it = ixt[i__] + (ni[i__] + l - 1) * ldt[i__] + ni[ip1] + l;
                } else {
                    it = ixt[i__] + (ni[ip1] + l - 1) * ldt[i__] + ni[i__] + l;
                }
                if (t[it] == 0.) {
                    if (select[l]) {
                        ++(*m);
                    }
                } else {
                    pair = TRUE_;
                    if (select[l] || select[l + 1]) {
                        *m += 2;
                    }
                }
            } else {
                if (select[*nc]) {
                    ++(*m);
                }
            }
        }
        /* L70: */
    }
    /*     Set COMPQ for MB03KA, if needed. */
    if (initq) {
        *(unsigned char*)compqc = 'U';
    } else {
        *(unsigned char*)compqc = *(unsigned char*)compq;
    }
    /*     Check workspace. */
    if (*info == 0) {
        mb03ka_(compqc, &whichq[1], &ws, k, nc, kschur, &c__1, &c__1, &n[1], &ni[1], &s[1], &t[1],
            &ldt[1], &ixt[1], &q[1], &ldq[1], &ixq[1], &dwork[1], &iwork[1], &dwork[1], &c_n1, info,
            1L);
        /* Computing MAX */
        i__1 = 1, i__2 = (integer)dwork[1];
        mnwork = max(i__1, i__2);
        if (*ldwork != -1 && *ldwork < mnwork) {
            *info = -21;
        }
    }
    /*     Quick return if possible. */
    if (*ldwork == -1) {
        dwork[1] = (doublereal)mnwork;
        return 0;
    } else if (*info < 0) {
        i__1 = -(*info);
        xerbla_("MB03KD", &i__1, 6L);
        return 0;
    }
    /*     Compute some machine-dependent parameters. */
    tola[0] = *tol;
    tola[1] = dlamch_("Precision", 9L);
    tola[2] = dlamch_("Safe minimum", 12L) / tola[1];
    /*     Initialization of orthogonal factors. */
    i__1 = *k;
    for (l = 1; l <= i__1; ++l) {
        if (specq) {
            initq = whichq[l] == 1;
        }
        if (initq) {
            dlaset_("All", &n[l], &n[l], &c_b23, &c_b24, &q[ixq[l]], &ldq[l], 3L);
        }
        /* L80: */
    }
    /*     Collect the selected blocks at the top-left corner of T22_k. */
    ls = 0;
    pair = FALSE_;
    i__ = *kschur;
    ip1 = i__ % *k + 1;
    i__1 = *nc;
    for (l = 1; l <= i__1; ++l) {
        if (pair) {
            pair = FALSE_;
        } else {
            swap = select[l];
            if (l < *nc) {
                if (s[i__] == 1) {
                    it = ixt[i__] + (ni[i__] + l - 1) * ldt[i__] + ni[ip1] + l;
                } else {
                    it = ixt[i__] + (ni[ip1] + l - 1) * ldt[i__] + ni[i__] + l;
                }
                if (t[it] != 0.) {
                    pair = TRUE_;
                    swap = swap || select[l + 1];
                }
            }
            if (swap) {
                ++ls;
                /*              Swap the L-th block to position LS in T22_k. */
                ll = l;
                if (l != ls) {
                    mb03ka_(compqc, &whichq[1], &ws, k, nc, kschur, &ll, &ls, &n[1], &ni[1], &s[1],
                        &t[1], &ldt[1], &ixt[1], &q[1], &ldq[1], &ixq[1], tola, &iwork[1],
                        &dwork[1], ldwork, info, 1L);
                    if (*info != 0) {
                        /*                    Blocks too close to swap; exit. */
                        goto L100;
                    }
                }
                if (pair) {
                    ++ls;
                }
            }
        }
        /* L90: */
    }
L100:
    /*     Store optimal workspace length and return. */
    dwork[1] = (doublereal)mnwork;
    return 0;
    /* *** Last line of MB03KD *** */
} /* mb03kd_ */
