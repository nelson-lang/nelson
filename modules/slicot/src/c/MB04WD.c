/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;
static integer c__3 = 3;

EXPORTSYMBOL /* Subroutine */ int mb04wd_(tranq1, tranq2, m, n, k, q1, ldq1, q2, ldq2, cs, tau,
    dwork, ldwork, info, tranq1_len, tranq2_len) char *tranq1,
    *tranq2;
integer *m, *n, *k;
doublereal* q1;
integer* ldq1;
doublereal* q2;
integer* ldq2;
doublereal *cs, *tau, *dwork;
integer *ldwork, *info;
ftnlen tranq1_len;
ftnlen tranq2_len;
{
    /* System generated locals */
    address a__1[2];
    integer q1_dim1, q1_offset, q2_dim1, q2_offset, i__1[2], i__2, i__3, i__4;
    char ch__1[2];
    /* Builtin functions */
    /* Subroutine */ int s_cat();
    double sqrt();
    /* Local variables */
    static integer ierr, pdrs;
    static logical ltrq1, ltrq2;
    static integer i__;
    extern /* Subroutine */ int mb04qc_(), mb04qf_();
    extern integer ue01md_();
    extern logical lsame_();
    static integer nbmin;
    extern /* Subroutine */ int mb04wu_();
    static integer ib, nb, ki, kk, nx;
    extern /* Subroutine */ int xerbla_();
    static integer wrkopt, pdt, pdw;
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
    /*     To generate a matrix Q with orthogonal columns (spanning an */
    /*     isotropic subspace), which is defined as the first n columns */
    /*     of a product of symplectic reflectors and Givens rotators, */
    /*         Q = diag( H(1),H(1) ) G(1) diag( F(1),F(1) ) */
    /*             diag( H(2),H(2) ) G(2) diag( F(2),F(2) ) */
    /*                               .... */
    /*             diag( H(k),H(k) ) G(k) diag( F(k),F(k) ). */
    /*     The matrix Q is returned in terms of its first 2*M rows */
    /*                      [  op( Q1 )   op( Q2 ) ] */
    /*                  Q = [                      ]. */
    /*                      [ -op( Q2 )   op( Q1 ) ] */
    /*     Blocked version of the SLICOT Library routine MB04WU. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     TRANQ1  CHARACTER*1 */
    /*             Specifies the form of op( Q1 ) as follows: */
    /*             = 'N':  op( Q1 ) = Q1; */
    /*             = 'T':  op( Q1 ) = Q1'; */
    /*             = 'C':  op( Q1 ) = Q1'. */
    /*     TRANQ2  CHARACTER*1 */
    /*             Specifies the form of op( Q2 ) as follows: */
    /*             = 'N':  op( Q2 ) = Q2; */
    /*             = 'T':  op( Q2 ) = Q2'; */
    /*             = 'C':  op( Q2 ) = Q2'. */
    /*     Input/Output Parameters */
    /*     M       (input) INTEGER */
    /*             The number of rows of the matrices Q1 and Q2. M >= 0. */
    /*     N       (input) INTEGER */
    /*             The number of columns of the matrices Q1 and Q2. */
    /*             M >= N >= 0. */
    /*     K       (input) INTEGER */
    /*             The number of symplectic Givens rotators whose product */
    /*             partly defines the matrix Q. N >= K >= 0. */
    /*     Q1      (input/output) DOUBLE PRECISION array, dimension */
    /*                     (LDQ1,N) if TRANQ1 = 'N', */
    /*                     (LDQ1,M) if TRANQ1 = 'T' or TRANQ1 = 'C' */
    /*             On entry with TRANQ1 = 'N', the leading M-by-K part of */
    /*             this array must contain in its i-th column the vector */
    /*             which defines the elementary reflector F(i). */
    /*             On entry with TRANQ1 = 'T' or TRANQ1 = 'C', the leading */
    /*             K-by-M part of this array must contain in its i-th row */
    /*             the vector which defines the elementary reflector F(i). */
    /*             On exit with TRANQ1 = 'N', the leading M-by-N part of this */
    /*             array contains the matrix Q1. */
    /*             On exit with TRANQ1 = 'T' or TRANQ1 = 'C', the leading */
    /*             N-by-M part of this array contains the matrix Q1'. */
    /*     LDQ1    INTEGER */
    /*             The leading dimension of the array Q1. */
    /*             LDQ1 >= MAX(1,M),  if TRANQ1 = 'N'; */
    /*             LDQ1 >= MAX(1,N),  if TRANQ1 = 'T' or TRANQ1 = 'C'. */
    /*     Q2      (input/output) DOUBLE PRECISION array, dimension */
    /*                     (LDQ2,N) if TRANQ2 = 'N', */
    /*                     (LDQ2,M) if TRANQ2 = 'T' or TRANQ2 = 'C' */
    /*             On entry with TRANQ2 = 'N', the leading M-by-K part of */
    /*             this array must contain in its i-th column the vector */
    /*             which defines the elementary reflector H(i) and, on the */
    /*             diagonal, the scalar factor of H(i). */
    /*             On entry with TRANQ2 = 'T' or TRANQ2 = 'C', the leading */
    /*             K-by-M part of this array must contain in its i-th row the */
    /*             vector which defines the elementary reflector H(i) and, on */
    /*             the diagonal, the scalar factor of H(i). */
    /*             On exit with TRANQ2 = 'N', the leading M-by-N part of this */
    /*             array contains the matrix Q2. */
    /*             On exit with TRANQ2 = 'T' or TRANQ2 = 'C', the leading */
    /*             N-by-M part of this array contains the matrix Q2'. */
    /*     LDQ2    INTEGER */
    /*             The leading dimension of the array Q2. */
    /*             LDQ2 >= MAX(1,M),  if TRANQ2 = 'N'; */
    /*             LDQ2 >= MAX(1,N),  if TRANQ2 = 'T' or TRANQ2 = 'C'. */
    /*     CS      (input) DOUBLE PRECISION array, dimension (2*K) */
    /*             On entry, the first 2*K elements of this array must */
    /*             contain the cosines and sines of the symplectic Givens */
    /*             rotators G(i). */
    /*     TAU     (input) DOUBLE PRECISION array, dimension (K) */
    /*             On entry, the first K elements of this array must */
    /*             contain the scalar factors of the elementary reflectors */
    /*             F(i). */
    /*     Workspace */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             On exit, if INFO = 0,  DWORK(1)  returns the optimal */
    /*             value of LDWORK, MAX(M+N,8*N*NB + 15*NB*NB), where NB is */
    /*             the optimal block size determined by the function UE01MD. */
    /*             On exit, if  INFO = -13,  DWORK(1)  returns the minimum */
    /*             value of LDWORK. */
    /*     LDWORK  INTEGER */
    /*             The length of the array DWORK.  LDWORK >= MAX(1,M+N). */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -i, the i-th argument had an illegal */
    /*                   value. */
    /*     REFERENCES */
    /*     [1] Kressner, D. */
    /*         Block algorithms for orthogonal symplectic factorizations. */
    /*         BIT, 43 (4), pp. 775-790, 2003. */
    /*     CONTRIBUTORS */
    /*     D. Kressner, Technical Univ. Berlin, Germany, and */
    /*     P. Benner, Technical Univ. Chemnitz, Germany, December 2003. */
    /*     REVISIONS */
    /*     V. Sima, June 2008 (SLICOT version of the HAPACK routine DOSGSB). */
    /*     KEYWORDS */
    /*     Elementary matrix operations, orthogonal symplectic matrix. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. External Functions .. */
    /*     .. External Subroutines .. */
    /*     .. Intrinsic Functions .. */
    /*     .. Executable Statements .. */
    /*     Decode the scalar input parameters. */
    /* Parameter adjustments */
    q1_dim1 = *ldq1;
    q1_offset = q1_dim1 + 1;
    q1 -= q1_offset;
    q2_dim1 = *ldq2;
    q2_offset = q2_dim1 + 1;
    q2 -= q2_offset;
    --cs;
    --tau;
    --dwork;
    /* Function Body */
    *info = 0;
    ltrq1 = lsame_(tranq1, "T", 1L, 1L) || lsame_(tranq1, "C", 1L, 1L);
    ltrq2 = lsame_(tranq2, "T", 1L, 1L) || lsame_(tranq2, "C", 1L, 1L);
    /* Writing concatenation */
    i__1[0] = 1, a__1[0] = tranq1;
    i__1[1] = 1, a__1[1] = tranq2;
    s_cat(ch__1, a__1, i__1, &c__2, 2L);
    nb = ue01md_(&c__1, "MB04WD", ch__1, m, n, k, 6L, 2L);
    /*     Check the scalar input parameters. */
    if (!(ltrq1 || lsame_(tranq1, "N", 1L, 1L))) {
        *info = -1;
    } else if (!(ltrq2 || lsame_(tranq2, "N", 1L, 1L))) {
        *info = -2;
    } else if (*m < 0) {
        *info = -3;
    } else if (*n < 0 || *n > *m) {
        *info = -4;
    } else if (*k < 0 || *k > *n) {
        *info = -5;
    } else if (ltrq1 && *ldq1 < max(1, *n) || !ltrq1 && *ldq1 < max(1, *m)) {
        *info = -7;
    } else if (ltrq2 && *ldq2 < max(1, *n) || !ltrq2 && *ldq2 < max(1, *m)) {
        *info = -9;
    } else /* if(complicated condition) */
    {
        /* Computing MAX */
        i__2 = 1, i__3 = *m + *n;
        if (*ldwork < max(i__2, i__3)) {
            /* Computing MAX */
            i__2 = 1, i__3 = *m + *n;
            dwork[1] = (doublereal)max(i__2, i__3);
            *info = -13;
        }
    }
    /*     Return if there were illegal values. */
    if (*info != 0) {
        i__2 = -(*info);
        xerbla_("MB04WD", &i__2, 6L);
        return 0;
    }
    /*     Quick return if possible. */
    if (*n == 0) {
        dwork[1] = 1.;
        return 0;
    }
    nbmin = 2;
    nx = 0;
    wrkopt = *m + *n;
    if (nb > 1 && nb < *k) {
        /*        Determine when to cross over from blocked to unblocked code. */
        /* Computing MAX */
        /* Writing concatenation */
        i__1[0] = 1, a__1[0] = tranq1;
        i__1[1] = 1, a__1[1] = tranq2;
        s_cat(ch__1, a__1, i__1, &c__2, 2L);
        i__2 = 0, i__3 = ue01md_(&c__3, "MB04WD", ch__1, m, n, k, 6L, 2L);
        nx = max(i__2, i__3);
        if (nx < *k) {
            /*           Determine if workspace is large enough for blocked code. */
            /* Computing MAX */
            i__2 = wrkopt, i__3 = (*n << 3) * nb + nb * 15 * nb;
            wrkopt = max(i__2, i__3);
            if (*ldwork < wrkopt) {
                /*              Not enough workspace to use optimal NB:  reduce NB and */
                /*              determine the minimum value of NB. */
                nb = (integer)(
                    (sqrt((doublereal)((*n << 4) * *n + *ldwork * 15)) - (doublereal)(*n << 2))
                    / 15.);
                /* Computing MAX */
                /* Writing concatenation */
                i__1[0] = 1, a__1[0] = tranq1;
                i__1[1] = 1, a__1[1] = tranq2;
                s_cat(ch__1, a__1, i__1, &c__2, 2L);
                i__2 = 2, i__3 = ue01md_(&c__2, "MB04WD", ch__1, m, n, k, 6L, 2L);
                nbmin = max(i__2, i__3);
            }
        }
    }
    if (nb >= nbmin && nb < *k && nx < *k) {
        /*        Use blocked code after the last block. */
        /*        The first kk columns are handled by the block method. */
        ki = (*k - nx - 1) / nb * nb;
        /* Computing MIN */
        i__2 = *k, i__3 = ki + nb;
        kk = min(i__2, i__3);
    } else {
        kk = 0;
    }
    /*     Use unblocked code for the last or only block. */
    if (kk < *n) {
        i__2 = *m - kk;
        i__3 = *n - kk;
        i__4 = *k - kk;
        mb04wu_(tranq1, tranq2, &i__2, &i__3, &i__4, &q1[kk + 1 + (kk + 1) * q1_dim1], ldq1,
            &q2[kk + 1 + (kk + 1) * q2_dim1], ldq2, &cs[(kk << 1) + 1], &tau[kk + 1], &dwork[1],
            ldwork, &ierr, 1L, 1L);
    }
    /*     Blocked code. */
    if (kk > 0) {
        pdrs = 1;
        pdt = pdrs + nb * 6 * nb;
        pdw = pdt + nb * 9 * nb;
        if (ltrq1 && ltrq2) {
            i__2 = -nb;
            for (i__ = ki + 1; i__2 < 0 ? i__ >= 1 : i__ <= 1; i__ += i__2) {
                /* Computing MIN */
                i__3 = nb, i__4 = *k - i__ + 1;
                ib = min(i__3, i__4);
                if (i__ + ib <= *n) {
                    /*                 Form the triangular factors of the symplectic block */
                    /*                 reflector SH. */
                    i__3 = *m - i__ + 1;
                    mb04qf_("Forward", "Rowwise", "Rowwise", &i__3, &ib, &q1[i__ + i__ * q1_dim1],
                        ldq1, &q2[i__ + i__ * q2_dim1], ldq2, &cs[(i__ << 1) - 1], &tau[i__],
                        &dwork[pdrs], &nb, &dwork[pdt], &nb, &dwork[pdw], 7L, 7L, 7L);
                    /*                 Apply SH to Q1(i+ib:n,i:m) and Q2(i+ib:n,i:m) from */
                    /*                 the right. */
                    i__3 = *m - i__ + 1;
                    i__4 = *n - i__ - ib + 1;
                    mb04qc_("Zero Structure", "Transpose", "Transpose", "No Transpose", "Forward",
                        "Rowwise", "Rowwise", &i__3, &i__4, &ib, &q1[i__ + i__ * q1_dim1], ldq1,
                        &q2[i__ + i__ * q2_dim1], ldq2, &dwork[pdrs], &nb, &dwork[pdt], &nb,
                        &q2[i__ + ib + i__ * q2_dim1], ldq2, &q1[i__ + ib + i__ * q1_dim1], ldq1,
                        &dwork[pdw], 14L, 9L, 9L, 12L, 7L, 7L, 7L);
                }
                /*              Apply SH to columns i:m of the current block. */
                i__3 = *m - i__ + 1;
                mb04wu_("Transpose", "Transpose", &i__3, &ib, &ib, &q1[i__ + i__ * q1_dim1], ldq1,
                    &q2[i__ + i__ * q2_dim1], ldq2, &cs[(i__ << 1) - 1], &tau[i__], &dwork[1],
                    ldwork, &ierr, 9L, 9L);
                /* L10: */
            }
        } else if (ltrq1) {
            i__2 = -nb;
            for (i__ = ki + 1; i__2 < 0 ? i__ >= 1 : i__ <= 1; i__ += i__2) {
                /* Computing MIN */
                i__3 = nb, i__4 = *k - i__ + 1;
                ib = min(i__3, i__4);
                if (i__ + ib <= *n) {
                    /*                 Form the triangular factors of the symplectic block */
                    /*                 reflector SH. */
                    i__3 = *m - i__ + 1;
                    mb04qf_("Forward", "Rowwise", "Columnwise", &i__3, &ib,
                        &q1[i__ + i__ * q1_dim1], ldq1, &q2[i__ + i__ * q2_dim1], ldq2,
                        &cs[(i__ << 1) - 1], &tau[i__], &dwork[pdrs], &nb, &dwork[pdt], &nb,
                        &dwork[pdw], 7L, 7L, 10L);
                    /*                 Apply SH to Q1(i+ib:n,i:m) from the right and to */
                    /*                 Q2(i:m,i+ib:n) from the left. */
                    i__3 = *m - i__ + 1;
                    i__4 = *n - i__ - ib + 1;
                    mb04qc_("Zero Structure", "No Transpose", "Transpose", "No Transpose",
                        "Forward", "Rowwise", "Columnwise", &i__3, &i__4, &ib,
                        &q1[i__ + i__ * q1_dim1], ldq1, &q2[i__ + i__ * q2_dim1], ldq2,
                        &dwork[pdrs], &nb, &dwork[pdt], &nb, &q2[i__ + (i__ + ib) * q2_dim1], ldq2,
                        &q1[i__ + ib + i__ * q1_dim1], ldq1, &dwork[pdw], 14L, 12L, 9L, 12L, 7L, 7L,
                        10L);
                }
                /*              Apply SH to columns/rows i:m of the current block. */
                i__3 = *m - i__ + 1;
                mb04wu_("Transpose", "No Transpose", &i__3, &ib, &ib, &q1[i__ + i__ * q1_dim1],
                    ldq1, &q2[i__ + i__ * q2_dim1], ldq2, &cs[(i__ << 1) - 1], &tau[i__], &dwork[1],
                    ldwork, &ierr, 9L, 12L);
                /* L20: */
            }
        } else if (ltrq2) {
            i__2 = -nb;
            for (i__ = ki + 1; i__2 < 0 ? i__ >= 1 : i__ <= 1; i__ += i__2) {
                /* Computing MIN */
                i__3 = nb, i__4 = *k - i__ + 1;
                ib = min(i__3, i__4);
                if (i__ + ib <= *n) {
                    /*                 Form the triangular factors of the symplectic block */
                    /*                 reflector SH. */
                    i__3 = *m - i__ + 1;
                    mb04qf_("Forward", "Columnwise", "Rowwise", &i__3, &ib,
                        &q1[i__ + i__ * q1_dim1], ldq1, &q2[i__ + i__ * q2_dim1], ldq2,
                        &cs[(i__ << 1) - 1], &tau[i__], &dwork[pdrs], &nb, &dwork[pdt], &nb,
                        &dwork[pdw], 7L, 10L, 7L);
                    /*                 Apply SH to Q1(i:m,i+ib:n) from the left and to */
                    /*                 Q2(i+ib:n,i:m) from the right. */
                    i__3 = *m - i__ + 1;
                    i__4 = *n - i__ - ib + 1;
                    mb04qc_("Zero Structure", "Transpose", "No Transpose", "No Transpose",
                        "Forward", "Columnwise", "Rowwise", &i__3, &i__4, &ib,
                        &q1[i__ + i__ * q1_dim1], ldq1, &q2[i__ + i__ * q2_dim1], ldq2,
                        &dwork[pdrs], &nb, &dwork[pdt], &nb, &q2[i__ + ib + i__ * q2_dim1], ldq2,
                        &q1[i__ + (i__ + ib) * q1_dim1], ldq1, &dwork[pdw], 14L, 9L, 12L, 12L, 7L,
                        10L, 7L);
                }
                /*              Apply SH to columns/rows i:m of the current block. */
                i__3 = *m - i__ + 1;
                mb04wu_("No Transpose", "Transpose", &i__3, &ib, &ib, &q1[i__ + i__ * q1_dim1],
                    ldq1, &q2[i__ + i__ * q2_dim1], ldq2, &cs[(i__ << 1) - 1], &tau[i__], &dwork[1],
                    ldwork, &ierr, 12L, 9L);
                /* L30: */
            }
        } else {
            i__2 = -nb;
            for (i__ = ki + 1; i__2 < 0 ? i__ >= 1 : i__ <= 1; i__ += i__2) {
                /* Computing MIN */
                i__3 = nb, i__4 = *k - i__ + 1;
                ib = min(i__3, i__4);
                if (i__ + ib <= *n) {
                    /*                 Form the triangular factors of the symplectic block */
                    /*                 reflector SH. */
                    i__3 = *m - i__ + 1;
                    mb04qf_("Forward", "Columnwise", "Columnwise", &i__3, &ib,
                        &q1[i__ + i__ * q1_dim1], ldq1, &q2[i__ + i__ * q2_dim1], ldq2,
                        &cs[(i__ << 1) - 1], &tau[i__], &dwork[pdrs], &nb, &dwork[pdt], &nb,
                        &dwork[pdw], 7L, 10L, 10L);
                    /*                 Apply SH to Q1(i:m,i+ib:n) and Q2(i:m,i+ib:n) from */
                    /*                 the left. */
                    i__3 = *m - i__ + 1;
                    i__4 = *n - i__ - ib + 1;
                    mb04qc_("Zero Structure", "No Transpose", "No Transpose", "No Transpose",
                        "Forward", "Columnwise", "Columnwise", &i__3, &i__4, &ib,
                        &q1[i__ + i__ * q1_dim1], ldq1, &q2[i__ + i__ * q2_dim1], ldq2,
                        &dwork[pdrs], &nb, &dwork[pdt], &nb, &q2[i__ + (i__ + ib) * q2_dim1], ldq2,
                        &q1[i__ + (i__ + ib) * q1_dim1], ldq1, &dwork[pdw], 14L, 12L, 12L, 12L, 7L,
                        10L, 10L);
                }
                /*              Apply SH to rows i:m of the current block. */
                i__3 = *m - i__ + 1;
                mb04wu_("No Transpose", "No Transpose", &i__3, &ib, &ib, &q1[i__ + i__ * q1_dim1],
                    ldq1, &q2[i__ + i__ * q2_dim1], ldq2, &cs[(i__ << 1) - 1], &tau[i__], &dwork[1],
                    ldwork, &ierr, 12L, 12L);
                /* L40: */
            }
        }
    }
    dwork[1] = (doublereal)wrkopt;
    return 0;
    /* *** Last line of MB04WD *** */
} /* mb04wd_ */
