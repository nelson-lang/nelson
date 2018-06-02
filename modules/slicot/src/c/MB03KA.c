/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c_n1 = -1;
static integer c__2 = 2;

EXPORTSYMBOL /* Subroutine */ int mb03ka_(compq, whichq, ws, k, nc, kschur, ifst, ilst, n, ni, s, t,
    ldt, ixt, q, ldq, ixq, tol, iwork, dwork, ldwork, info, compq_len) char* compq;
integer* whichq;
logical* ws;
integer *k, *nc, *kschur, *ifst, *ilst, *n, *ni, *s;
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
    integer i__1, i__2;
    /* Local variables */
    static integer here, i__;
    extern /* Subroutine */ int mb03kb_();
    static integer it;
    extern /* Subroutine */ int xerbla_();
    static integer nbnext, minwrk, ip1, nbf, nbl;
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
    /*     of length K, in the generalized periodic Schur form */
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
    /*     such that the block with starting row index IFST in (1) is moved */
    /*     to row index ILST. The indices refer to the T22_k submatrices. */
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
    /*             triangular. All other T22 matrices are upper triangular. */
    /*     IFST    (input/output) INTEGER */
    /*     ILST    (input/output) INTEGER */
    /*             Specify the reordering of the diagonal blocks, as follows: */
    /*             The block with starting row index IFST in (1) is moved to */
    /*             row index ILST by a sequence of direct swaps between adjacent */
    /*             blocks in the product. */
    /*             On exit, if IFST pointed on entry to the second row of a */
    /*             2-by-2 block in the product, it is changed to point to the */
    /*             first row; ILST always points to the first row of the block */
    /*             in its final position in the product (which may differ from */
    /*             its input value by +1 or -1). */
    /*             1 <= IFST <= NC, 1 <= ILST <= NC. */
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
    /*             dimensional array Q. */
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
    /*             NRM is the Frobenius norm of the current matrix formed by */
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
    /*             LDWORK >= 10*K + MN, if all blocks between IFST and ILST */
    /*                                  have order 1; */
    /*             LDWORK >= 25*K + MN, if there is at least a block of */
    /*                                  order 2, but no adjacent blocks of */
    /*                                  order 2 can appear between IFST and */
    /*                                  ILST during reordering; */
    /*             LDWORK >= MAX(42*K + MN, 80*K - 48), if at least a pair of */
    /*                                  adjacent blocks of order 2 can appear */
    /*                                  between IFST and ILST during */
    /*                                  reordering; */
    /*             where MN = MXN, if MXN > 10, and MN = 0, otherwise, with */
    /*             MXN = MAX(N(k),k=1,...,K). */
    /*             If LDWORK = -1  a workspace query is assumed; the */
    /*             routine only calculates the optimal size of the DWORK */
    /*             array, returns this value as the first entry of the DWORK */
    /*             array, and no error message is issued by XERBLA. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -21, the LDWORK argument was too small; */
    /*             = 1:  the reordering of T failed because some eigenvalues */
    /*                   are too close to separate (the problem is very ill- */
    /*                   conditioned); T may have been partially reordered. */
    /*                   The returned value of ILST is the index where this */
    /*                   was detected. */
    /*     METHOD */
    /*     An adaptation of the LAPACK Library routine DTGEXC is used. */
    /*     NUMERICAL ASPECTS */
    /*     The implemented method is numerically backward stable. */
    /*     CONTRIBUTOR */
    /*     R. Granat, Umea University, Sweden, Apr. 2008. */
    /*     REVISIONS */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Romania, */
    /*     Mar. 2010, SLICOT Library version of the PEP routine PEP_DTGEXC. */
    /*     V. Sima, July 2010. */
    /*     KEYWORDS */
    /*     Orthogonal transformation, periodic QZ algorithm, periodic */
    /*     Sylvester-like equations, QZ algorithm. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. */
    /*     .. Scalar Arguments .. */
    /*     .. */
    /*     .. Array Arguments .. */
    /*     .. */
    /*     .. Local Scalars .. */
    /*     .. */
    /*     .. External Subroutines .. */
    /*     .. */
    /*     .. Intrinsic Functions .. */
    /*     .. */
    /*     .. Executable Statements .. */
    /*     For efficiency reasons the parameters are not checked, except for */
    /*     workspace. */
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
    if (*nc == 2) {
        nbf = 1;
        nbl = 1;
    } else if (*nc == 3) {
        nbf = 1;
        nbl = 2;
    } else {
        nbf = 2;
        nbl = 2;
    }
    mb03kb_(compq, &whichq[1], ws, k, nc, kschur, &c__1, &nbf, &nbl, &n[1], &ni[1], &s[1], &t[1],
        &ldt[1], &ixt[1], &q[1], &ldq[1], &ixq[1], &tol[1], &iwork[1], &dwork[1], &c_n1, info, 1L);
    /* Computing MAX */
    i__1 = 1, i__2 = (integer)dwork[1];
    minwrk = max(i__1, i__2);
    if (*ldwork != -1 && *ldwork < minwrk) {
        *info = -21;
    }
    /*     Quick return if possible */
    if (*ldwork == -1) {
        dwork[1] = (doublereal)minwrk;
        return 0;
    } else if (*info < 0) {
        i__1 = -(*info);
        xerbla_("MB03KA", &i__1, 6L);
        return 0;
    }
    /*     Set I and IP1 to point to KSCHUR and KSCHUR+1 to simplify */
    /*     indices below. */
    i__ = *kschur;
    ip1 = i__ % *k + 1;
    /*     Determine the first row of the block in T22_kschur corresponding */
    /*     to the first block in the product and find out if it is 1-by-1 or */
    /*     2-by-2. */
    if (*ifst > 1) {
        if (s[i__] == 1) {
            it = ixt[i__] + (ni[i__] + *ifst - 2) * ldt[i__] + ni[ip1] + *ifst - 1;
        } else {
            it = ixt[i__] + (ni[ip1] + *ifst - 2) * ldt[i__] + ni[i__] + *ifst - 1;
        }
        if (t[it] != 0.) {
            --(*ifst);
        }
    }
    nbf = 1;
    if (*ifst < *nc) {
        if (s[i__] == 1) {
            it = ixt[i__] + (ni[i__] + *ifst - 1) * ldt[i__] + ni[ip1] + *ifst;
        } else {
            it = ixt[i__] + (ni[ip1] + *ifst - 1) * ldt[i__] + ni[i__] + *ifst;
        }
        if (t[it] != 0.) {
            nbf = 2;
        }
    }
    /*     Determine the first row of the block in T_kschur corresponding */
    /*     to the last block in the product and find out it is 1-by-1 or */
    /*     2-by-2. */
    if (*ilst > 1) {
        if (s[i__] == 1) {
            it = ixt[i__] + (ni[i__] + *ilst - 2) * ldt[i__] + ni[ip1] + *ilst - 1;
        } else {
            it = ixt[i__] + (ni[ip1] + *ilst - 2) * ldt[i__] + ni[i__] + *ilst - 1;
        }
        if (t[it] != 0.) {
            --(*ilst);
        }
    }
    nbl = 1;
    if (*ilst < *nc) {
        if (s[i__] == 1) {
            it = ixt[i__] + (ni[i__] + *ilst - 1) * ldt[i__] + ni[ip1] + *ilst;
        } else {
            it = ixt[i__] + (ni[ip1] + *ilst - 1) * ldt[i__] + ni[i__] + *ilst;
        }
        if (t[it] != 0.) {
            nbl = 2;
        }
    }
    /*     If the specified and last block in the product were the same, */
    /*     return. */
    if (*ifst == *ilst) {
        return 0;
    }
    /*     If the specified block lies above the last block on the diagonal */
    /*     of the product and the blocks have unequal sizes, update ILST. */
    if (*ifst < *ilst) {
        /*        Update ILST. */
        if (nbf == 2 && nbl == 1) {
            --(*ilst);
        }
        if (nbf == 1 && nbl == 2) {
            ++(*ilst);
        }
        here = *ifst;
    L10:
        /*        Swap a block with next one below. */
        if (nbf == 1 || nbf == 2) {
            /*           Current next block is either 1-by-1 or 2-by-2. */
            nbnext = 1;
            if (here + nbf + 1 <= *nc) {
                if (s[i__] == 1) {
                    it = ixt[i__] + (ni[i__] + here + nbf - 1) * ldt[i__] + ni[ip1] + here + nbf;
                } else {
                    it = ixt[i__] + (ni[ip1] + here + nbf - 1) * ldt[i__] + ni[i__] + here + nbf;
                }
                if (t[it] != 0.) {
                    nbnext = 2;
                }
            }
            mb03kb_(compq, &whichq[1], ws, k, nc, kschur, &here, &nbf, &nbnext, &n[1], &ni[1],
                &s[1], &t[1], &ldt[1], &ixt[1], &q[1], &ldq[1], &ixq[1], &tol[1], &iwork[1],
                &dwork[1], ldwork, info, 1L);
            if (*info != 0) {
                *ilst = here;
                return 0;
            }
            here += nbnext;
            /*           Test if a 2-by-2 block breaks into two 1-by-1 blocks. */
            if (nbf == 2) {
                if (s[i__] == 1) {
                    it = ixt[i__] + (ni[i__] + here - 1) * ldt[i__] + ni[ip1] + here;
                } else {
                    it = ixt[i__] + (ni[ip1] + here - 1) * ldt[i__] + ni[i__] + here;
                }
                if (t[it] == 0.) {
                    nbf = 3;
                }
            }
        } else {
            /*           Current next block consists of two 1-by-1 blocks each of */
            /*           which must be swapped individually. */
            nbnext = 1;
            if (here + 3 <= *nc) {
                if (s[i__] == 1) {
                    it = ixt[i__] + (ni[i__] + here + 1) * ldt[i__] + ni[ip1] + here + 2;
                } else {
                    it = ixt[i__] + (ni[ip1] + here + 1) * ldt[i__] + ni[i__] + here + 2;
                }
                if (t[it] != 0.) {
                    nbnext = 2;
                }
            }
            i__1 = here + 1;
            mb03kb_(compq, &whichq[1], ws, k, nc, kschur, &i__1, &c__1, &nbnext, &n[1], &ni[1],
                &s[1], &t[1], &ldt[1], &ixt[1], &q[1], &ldq[1], &ixq[1], &tol[1], &iwork[1],
                &dwork[1], ldwork, info, 1L);
            if (*info != 0) {
                *ilst = here;
                return 0;
            }
            if (nbnext == 1) {
                /*              Swap two 1-by-1 blocks. */
                mb03kb_(compq, &whichq[1], ws, k, nc, kschur, &here, &c__1, &nbnext, &n[1], &ni[1],
                    &s[1], &t[1], &ldt[1], &ixt[1], &q[1], &ldq[1], &ixq[1], &tol[1], &iwork[1],
                    &dwork[1], ldwork, info, 1L);
                if (*info != 0) {
                    *ilst = here;
                    return 0;
                }
                ++here;
            } else {
                /*              Recompute NBNEXT in case 2-by-2 split. */
                if (s[i__] == 1) {
                    it = ixt[i__] + (ni[i__] + here) * ldt[i__] + ni[ip1] + here + 1;
                } else {
                    it = ixt[i__] + (ni[ip1] + here) * ldt[i__] + ni[i__] + here + 1;
                }
                if (t[it] == 0.) {
                    nbnext = 1;
                }
                if (nbnext == 2) {
                    /*                 The 2-by-2 block did not split. */
                    mb03kb_(compq, &whichq[1], ws, k, nc, kschur, &here, &c__1, &nbnext, &n[1],
                        &ni[1], &s[1], &t[1], &ldt[1], &ixt[1], &q[1], &ldq[1], &ixq[1], &tol[1],
                        &iwork[1], &dwork[1], ldwork, info, 1L);
                    if (*info != 0) {
                        *ilst = here;
                        return 0;
                    }
                    here += 2;
                } else {
                    /*                 The 2-by-2 block did split. */
                    mb03kb_(compq, &whichq[1], ws, k, nc, kschur, &here, &c__1, &c__1, &n[1],
                        &ni[1], &s[1], &t[1], &ldt[1], &ixt[1], &q[1], &ldq[1], &ixq[1], &tol[1],
                        &iwork[1], &dwork[1], ldwork, info, 1L);
                    if (*info != 0) {
                        *ilst = here;
                        return 0;
                    }
                    i__1 = here + 1;
                    mb03kb_(compq, &whichq[1], ws, k, nc, kschur, &i__1, &c__1, &c__1, &n[1],
                        &ni[1], &s[1], &t[1], &ldt[1], &ixt[1], &q[1], &ldq[1], &ixq[1], &tol[1],
                        &iwork[1], &dwork[1], ldwork, info, 1L);
                    if (*info != 0) {
                        *ilst = here + 1;
                        return 0;
                    }
                    here += 2;
                }
            }
        }
        if (here < *ilst) {
            goto L10;
        }
    } else {
        here = *ifst;
    L20:
        /*        Swap a block with next one above. */
        if (nbf == 1 || nbf == 2) {
            /*           Current block is either 1-by-1 or 2-by-2. */
            nbnext = 1;
            if (here >= 3) {
                if (s[i__] == 1) {
                    it = ixt[i__] + (ni[i__] + here - 3) * ldt[i__] + ni[ip1] + here - 2;
                } else {
                    it = ixt[i__] + (ni[ip1] + here - 3) * ldt[i__] + ni[i__] + here - 2;
                }
                if (t[it] != 0.) {
                    nbnext = 2;
                }
            }
            i__1 = here - nbnext;
            mb03kb_(compq, &whichq[1], ws, k, nc, kschur, &i__1, &nbnext, &nbf, &n[1], &ni[1],
                &s[1], &t[1], &ldt[1], &ixt[1], &q[1], &ldq[1], &ixq[1], &tol[1], &iwork[1],
                &dwork[1], ldwork, info, 1L);
            if (*info != 0) {
                *ilst = here;
                return 0;
            }
            here -= nbnext;
            /*           Test if a 2-by-2 block breaks into two 1-by-1 blocks. */
            if (nbf == 2) {
                if (s[i__] == 1) {
                    it = ixt[i__] + (ni[i__] + here - 1) * ldt[i__] + ni[ip1] + here;
                } else {
                    it = ixt[i__] + (ni[ip1] + here - 1) * ldt[i__] + ni[i__] + here;
                }
                if (t[it] == 0.) {
                    nbf = 3;
                }
            }
        } else {
            /*           Current block consists of two 1-by-1 blocks each of which */
            /*           must be swapped individually. */
            nbnext = 1;
            if (here >= 3) {
                if (s[i__] == 1) {
                    it = ixt[i__] + (ni[i__] + here - 3) * ldt[i__] + ni[ip1] + here - 2;
                } else {
                    it = ixt[i__] + (ni[ip1] + here - 3) * ldt[i__] + ni[i__] + here - 2;
                }
                if (t[it] != 0.) {
                    nbnext = 2;
                }
            }
            i__1 = here - nbnext;
            mb03kb_(compq, &whichq[1], ws, k, nc, kschur, &i__1, &nbnext, &c__1, &n[1], &ni[1],
                &s[1], &t[1], &ldt[1], &ixt[1], &q[1], &ldq[1], &ixq[1], &tol[1], &iwork[1],
                &dwork[1], ldwork, info, 1L);
            if (*info != 0) {
                *ilst = here;
                return 0;
            }
            if (nbnext == 1) {
                /*              Swap two 1-by-1 blocks. */
                mb03kb_(compq, &whichq[1], ws, k, nc, kschur, &here, &nbnext, &c__1, &n[1], &ni[1],
                    &s[1], &t[1], &ldt[1], &ixt[1], &q[1], &ldq[1], &ixq[1], &tol[1], &iwork[1],
                    &dwork[1], ldwork, info, 1L);
                if (*info != 0) {
                    *ilst = here;
                    return 0;
                }
                --here;
            } else {
                /*              Recompute NBNEXT in case 2-by-2 split. */
                if (s[i__] == 1) {
                    it = ixt[i__] + (ni[i__] + here - 2) * ldt[i__] + ni[ip1] + here - 1;
                } else {
                    it = ixt[i__] + (ni[ip1] + here - 2) * ldt[i__] + ni[i__] + here - 1;
                }
                if (t[it] == 0.) {
                    nbnext = 1;
                }
                if (nbnext == 2) {
                    /*                 The 2-by-2 block did not split. */
                    i__1 = here - 1;
                    mb03kb_(compq, &whichq[1], ws, k, nc, kschur, &i__1, &c__2, &c__1, &n[1],
                        &ni[1], &s[1], &t[1], &ldt[1], &ixt[1], &q[1], &ldq[1], &ixq[1], &tol[1],
                        &iwork[1], &dwork[1], ldwork, info, 1L);
                    if (*info != 0) {
                        *ilst = here;
                        return 0;
                    }
                    here += -2;
                } else {
                    /*                 The 2-by-2 block did split. */
                    mb03kb_(compq, &whichq[1], ws, k, nc, kschur, &here, &c__1, &c__1, &n[1],
                        &ni[1], &s[1], &t[1], &ldt[1], &ixt[1], &q[1], &ldq[1], &ixq[1], &tol[1],
                        &iwork[1], &dwork[1], ldwork, info, 1L);
                    if (*info != 0) {
                        *ilst = here;
                        return 0;
                    }
                    i__1 = here - 1;
                    mb03kb_(compq, &whichq[1], ws, k, nc, kschur, &i__1, &c__1, &c__1, &n[1],
                        &ni[1], &s[1], &t[1], &ldt[1], &ixt[1], &q[1], &ldq[1], &ixq[1], &tol[1],
                        &iwork[1], &dwork[1], ldwork, info, 1L);
                    if (*info != 0) {
                        *ilst = here - 1;
                        return 0;
                    }
                    here += -2;
                }
            }
        }
        if (here > *ilst) {
            goto L20;
        }
    }
    *ilst = here;
    /*     Store optimal workspace values and return. */
    dwork[1] = (doublereal)minwrk;
    return 0;
    /* *** Last line of MB03KA *** */
} /* mb03ka_ */
