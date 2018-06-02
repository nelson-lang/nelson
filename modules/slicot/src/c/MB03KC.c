/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__1 = 1;

EXPORTSYMBOL /* Subroutine */ int mb03kc_(k, khess, n, r__, s, a, lda, v, tau) integer *k, *khess,
    *n, *r__, *s;
doublereal* a;
integer* lda;
doublereal *v, *tau;
{
    /* System generated locals */
    integer i__1, i__2;
    /* Local variables */
    static doublereal work[2];
    static integer i__, i1, i2, ic, ir, no;
    extern /* Subroutine */ int dlarfg_();
    static integer ix;
    extern /* Subroutine */ int dlarfx_();
    static integer ip1, inc;
    static doublereal tmp[1];
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
    /*     To reduce a 2-by-2 general, formal matrix product A of length K, */
    /*        A_K^s(K) * A_K-1^s(K-1) * ... * A_1^s(1), */
    /*     to the periodic Hessenberg-triangular form using a K-periodic */
    /*     sequence of elementary reflectors (Householder matrices). The */
    /*     matrices A_k, k = 1, ..., K, are stored in the N-by-N-by-K array A */
    /*     starting in the R-th row and column, and N can be 3 or 4. */
    /*     Each elementary reflector H_k is represented as */
    /*        H_k = I - tau_k * v_k * v_k',                               (1) */
    /*     where I is the 2-by-2 identity, tau_k is a real scalar, and v_k is */
    /*     a vector of length 2, k = 1,...,K, and it is constructed such that */
    /*     the following holds for k = 1,...,K: */
    /*            H_{k+1} * A_k * H_k = T_k, if s(k) = 1, */
    /*                                                                    (2) */
    /*            H_k * A_k * H_{k+1} = T_k, if s(k) = -1, */
    /*     with H_{K+1} = H_1 and all T_k upper triangular except for */
    /*     T_{khess} which is full. Clearly, */
    /*        T_K^s(K) *...* T_1^s(1) = H_1 * A_K^s(K) *...* A_1^s(1) * H_1. */
    /*     The reflectors are suitably applied to the whole, extended N-by-N */
    /*     matrices Ae_k, not only to the submatrices A_k, k = 1, ..., K. */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     K       (input) INTEGER */
    /*             The number of matrices in the sequence A_k.  K >= 2. */
    /*     KHESS   (input) INTEGER */
    /*             The index for which the returned matrix A_khess should be */
    /*             in the Hessenberg form on output.  1 <= KHESS <= K. */
    /*     N       (input) INTEGER */
    /*             The order of the extended matrices.  N = 3 or N = 4. */
    /*     R       (input) INTEGER */
    /*             The starting row and column index for the */
    /*             2-by-2 submatrices.  R = 1, or R = N-1. */
    /*     S       (input) INTEGER array, dimension (K) */
    /*             The leading K elements of this array must contain the */
    /*             signatures of the factors. Each entry in S must be either */
    /*             1 or -1; the value S(k) = -1 corresponds to using the */
    /*             inverse of the factor A_k. */
    /*     A       (input/output) DOUBLE PRECISION array, dimension (*) */
    /*             On entry, this array must contain at position IXA(k) = */
    /*             (k-1)*N*LDA+1 the N-by-N matrix Ae_k stored with leading */
    /*             dimension LDA. */
    /*             On exit, this array contains at position IXA(k) the */
    /*             N-by-N matrix Te_k stored with leading dimension LDA. */
    /*     LDA     INTEGER */
    /*             Leading dimension of the matrices Ae_k and Te_k in the */
    /*             one-dimensional array A.  LDA >= N. */
    /*     V       (output) DOUBLE PRECISION array, dimension (2*K) */
    /*             On exit, this array contains the K vectors v_k, */
    /*             k = 1,...,K, defining the elementary reflectors H_k as */
    /*             in (1). The k-th reflector is stored in V(2*k-1:2*k). */
    /*     TAU     (output) DOUBLE PRECISION array, dimension (K) */
    /*             On exit, this array contains the K values of tau_k, */
    /*             k = 1,...,K, defining the elementary reflectors H_k */
    /*             as in (1). */
    /*     METHOD */
    /*     A K-periodic sequence of elementary reflectors (Householder */
    /*     matrices) is used. The computations start for k = khess with the */
    /*     left reflector in (1), which is the identity matrix. */
    /*     NUMERICAL ASPECTS */
    /*     The implemented method is numerically backward stable. */
    /*     CONTRIBUTOR */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Romania, */
    /*     Mar. 2010, an essentially new version of the PEP routine */
    /*     PEP_DGEHR2, by R. Granat, Umea University, Sweden, Apr. 2008. */
    /*     REVISIONS */
    /*     V. Sima, Apr. 2010, May 2010. */
    /*     KEYWORDS */
    /*     Orthogonal transformation, periodic QZ algorithm, QZ algorithm. */
    /*     ****************************************************************** */
    /*  .. Parameters .. */
    /*     .. */
    /*     .. Scalar Arguments .. */
    /*     .. */
    /*     .. Array Arguments .. */
    /*  .. */
    /*  .. Local Scalars .. */
    /*  .. */
    /*  .. Local Arrays .. */
    /*  .. */
    /*  .. External Subroutines .. */
    /*  .. */
    /*     .. Intrinsic Functions .. */
    /*  .. */
    /*  .. Executable Statements .. */
    /*     For efficiency reasons, the parameters are not checked. */
    /*     Compute the periodic Hessenberg form of A with the Hessenberg */
    /*     matrix at position KHESS - start construction from I = KHESS, */
    /*     i.e., to the left of (and including) the Hessenberg matrix in the */
    /*     corresponding matrix product. */
    /*     Since the problem is 2-by-2, the orthogonal matrix working on */
    /*     A_{khess} from the left, if s(khess) = 1, or from the right, */
    /*     if s(khess) = -1, hence H_{khess+1}, will be the identity. */
    /* Parameter adjustments */
    --tau;
    --v;
    --a;
    --s;
    /* Function Body */
    ir = (*r__ - 1) * *lda;
    ic = ir + *r__ - 1;
    no = *n - *r__;
    inc = *n * *lda;
    i1 = *khess * inc + 1;
    ip1 = *khess % *k + 1;
    tau[ip1] = 0.;
    v[(ip1 << 1) - 1] = 0.;
    v[ip1 * 2] = 0.;
    i__1 = *k;
    for (i__ = *khess + 1; i__ <= i__1; ++i__) {
        ip1 = i__ % *k;
        ix = i1 + ic;
        i2 = ip1 * inc + 1;
        ++ip1;
        /*        Compute and apply the reflector H_{i+1} working on A_i^s(i) */
        /*        from the left. */
        if (s[i__] == 1) {
            work[0] = 1.;
            work[1] = a[ix + 1];
            dlarfg_(&c__2, &a[ix], &work[1], &c__1, &tau[ip1]);
            v[(ip1 << 1) - 1] = 1.;
            v[ip1 * 2] = work[1];
            dlarfx_("Left", &c__2, &no, work, &tau[ip1], &a[ix + *lda], lda, tmp, 4L);
        } else {
            work[0] = a[ix + 1];
            work[1] = 1.;
            dlarfg_(&c__2, &a[ix + *lda + 1], work, &c__1, &tau[ip1]);
            v[(ip1 << 1) - 1] = work[0];
            v[ip1 * 2] = 1.;
            dlarfx_("Right", r__, &c__2, work, &tau[ip1], &a[i1 + ir], lda, tmp, 5L);
        }
        a[ix + 1] = 0.;
        /*        Apply the reflector to A_{mod(i,K)+1}. */
        if (s[ip1] == 1) {
            i__2 = *r__ + 1;
            dlarfx_("Right", &i__2, &c__2, work, &tau[ip1], &a[i2 + ir], lda, tmp, 5L);
        } else {
            i__2 = no + 1;
            dlarfx_("Left", &c__2, &i__2, work, &tau[ip1], &a[i2 + ic], lda, tmp, 4L);
        }
        i1 += inc;
        /* L10: */
    }
    /*     Continue to the right of the Hessenberg matrix. */
    i1 = 1;
    i__1 = *khess - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
        ip1 = i__ % *k;
        ix = i1 + ic;
        i2 = ip1 * inc + 1;
        ++ip1;
        /*        Compute and apply the reflector H_{i+1} working on A_i^s(i) */
        /*        from the left. */
        if (s[i__] == 1) {
            work[0] = 1.;
            work[1] = a[ix + 1];
            dlarfg_(&c__2, &a[ix], &work[1], &c__1, &tau[ip1]);
            v[(ip1 << 1) - 1] = 1.;
            v[ip1 * 2] = work[1];
            dlarfx_("Left", &c__2, &no, work, &tau[ip1], &a[ix + *lda], lda, tmp, 4L);
        } else {
            work[0] = a[ix + 1];
            work[1] = 1.;
            dlarfg_(&c__2, &a[ix + *lda + 1], work, &c__1, &tau[ip1]);
            v[(ip1 << 1) - 1] = work[0];
            v[ip1 * 2] = 1.;
            dlarfx_("Right", r__, &c__2, work, &tau[ip1], &a[i1 + ir], lda, tmp, 5L);
        }
        a[ix + 1] = 0.;
        /*        Apply the reflector to A_{mod(i,K)+1}. */
        if (s[ip1] == 1) {
            i__2 = *r__ + 1;
            dlarfx_("Right", &i__2, &c__2, work, &tau[ip1], &a[i2 + ir], lda, tmp, 5L);
        } else {
            i__2 = no + 1;
            dlarfx_("Left", &c__2, &i__2, work, &tau[ip1], &a[i2 + ic], lda, tmp, 4L);
        }
        i1 += inc;
        /* L20: */
    }
    /*     The periodic Hessenberg-triangular form has been computed. */
    return 0;
    /* *** Last line of MB03KC *** */
} /* mb03kc_ */
