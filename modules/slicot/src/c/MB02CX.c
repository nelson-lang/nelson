/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c__1 = 1;

EXPORTSYMBOL /* Subroutine */ int mb02cx_(
    typet, p, q, k, a, lda, b, ldb, cs, lcs, dwork, ldwork, info, typet_len) char* typet;
integer *p, *q, *k;
doublereal* a;
integer* lda;
doublereal* b;
integer* ldb;
doublereal* cs;
integer* lcs;
doublereal* dwork;
integer *ldwork, *info;
ftnlen typet_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1, i__2, i__3;
    doublereal d__1;
    /* Local variables */
    static doublereal beta;
    static integer ierr;
    static doublereal c__;
    static integer i__;
    extern /* Subroutine */ int ma02fd_();
    static doublereal s, alpha;
    extern /* Subroutine */ int dscal_(), dlarf_();
    extern logical lsame_();
    extern /* Subroutine */ int daxpy_();
    static logical isrow;
    extern /* Subroutine */ int dlarfg_(), dgelqf_(), dgeqrf_(), xerbla_();
    static doublereal maxwrk, tau;
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
    /*     To bring the first blocks of a generator in proper form. */
    /*     The columns / rows of the positive and negative generators */
    /*     are contained in the arrays A and B, respectively. */
    /*     Transformation information will be stored and can be applied */
    /*     via SLICOT Library routine MB02CY. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     TYPET   CHARACTER*1 */
    /*             Specifies the type of the generator, as follows: */
    /*             = 'R':  A and B are the first blocks of the rows of the */
    /*                     positive and negative generators; */
    /*             = 'C':  A and B are the first blocks of the columns of the */
    /*                     positive and negative generators. */
    /*             Note:   in the sequel, the notation x / y means that */
    /*                     x corresponds to TYPET = 'R' and y corresponds to */
    /*                     TYPET = 'C'. */
    /*     Input/Output Parameters */
    /*     P       (input)  INTEGER */
    /*             The number of rows / columns in A containing the positive */
    /*             generators.  P >= 0. */
    /*     Q       (input)  INTEGER */
    /*             The number of rows / columns in B containing the negative */
    /*             generators.  Q >= 0. */
    /*     K       (input)  INTEGER */
    /*             The number of columns / rows in A and B to be processed. */
    /*             Normally, the size of the first block.  P >= K >= 0. */
    /*     A       (input/output)  DOUBLE PRECISION array, dimension */
    /*             (LDA, K) / (LDA, P) */
    /*             On entry, the leading P-by-K upper / K-by-P lower */
    /*             triangular part of this array must contain the rows / */
    /*             columns of the positive part in the first block of the */
    /*             generator. */
    /*             On exit, the leading P-by-K upper / K-by-P lower */
    /*             triangular part of this array contains the rows / columns */
    /*             of the positive part in the first block of the proper */
    /*             generator. */
    /*             The lower / upper trapezoidal part is not referenced. */
    /*     LDA     INTEGER */
    /*             The leading dimension of the array A. */
    /*             LDA >= MAX(1,P),    if TYPET = 'R'; */
    /*             LDA >= MAX(1,K),    if TYPET = 'C'. */
    /*     B       (input/output)  DOUBLE PRECISION array, dimension */
    /*             (LDB, K) / (LDB, Q) */
    /*             On entry, the leading Q-by-K / K-by-Q part of this array */
    /*             must contain the rows / columns of the negative part in */
    /*             the first block of the generator. */
    /*             On exit, the leading Q-by-K / K-by-Q part of this array */
    /*             contains part of the necessary information for the */
    /*             Householder transformations. */
    /*     LDB     INTEGER */
    /*             The leading dimension of the array B. */
    /*             LDB >= MAX(1,Q),    if TYPET = 'R'; */
    /*             LDB >= MAX(1,K),    if TYPET = 'C'. */
    /*     CS      (output)  DOUBLE PRECISION array, dimension (LCS) */
    /*             On exit, the leading 2*K + MIN(K,Q) part of this array */
    /*             contains necessary information for the SLICOT Library */
    /*             routine MB02CY (modified hyperbolic rotation parameters */
    /*             and scalar factors of the Householder transformations). */
    /*     LCS     INTEGER */
    /*             The length of the array CS.  LCS >= 2*K + MIN(K,Q). */
    /*     Workspace */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             On exit, if  INFO = 0,  DWORK(1)  returns the optimal */
    /*             value of LDWORK. */
    /*             On exit, if  INFO = -12,  DWORK(1)  returns the minimum */
    /*             value of LDWORK. */
    /*     LDWORK  INTEGER */
    /*             The length of the array DWORK.  LDWORK >= MAX(1,K). */
    /*             For optimum performance LDWORK should be larger. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  succesful exit; */
    /*             < 0:  if INFO = -i, the i-th argument had an illegal */
    /*                   value; */
    /*             = 1:  the reduction algorithm failed. The matrix */
    /*                   associated with the generator is not (numerically) */
    /*                   positive definite. */
    /*     METHOD */
    /*     If  TYPET = 'R',  a QR decomposition of B is first computed. */
    /*     Then, the elements below the first row of each column i of B */
    /*     are annihilated by a Householder transformation modifying the */
    /*     first element in that column. This first element, in turn, is */
    /*     then annihilated by a modified hyperbolic rotation, acting also */
    /*     on the i-th row of A. */
    /*     If  TYPET = 'C',  an LQ decomposition of B is first computed. */
    /*     Then, the elements on the right of the first column of each row i */
    /*     of B are annihilated by a Householder transformation modifying the */
    /*     first element in that row. This first element, in turn, is */
    /*     then annihilated by a modified hyperbolic rotation, acting also */
    /*     on the i-th column of A. */
    /*     CONTRIBUTOR */
    /*     D. Kressner, Technical Univ. Chemnitz, Germany, June 2000. */
    /*     REVISIONS */
    /*     V. Sima, Research Institute for Informatics, Bucharest, July 2000, */
    /*     February 2004. */
    /*     KEYWORDS */
    /*     Elementary matrix operations, Householder transformation, matrix */
    /*     operations, Toeplitz matrix. */
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
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    --cs;
    --dwork;
    /* Function Body */
    *info = 0;
    isrow = lsame_(typet, "R", 1L, 1L);
    /*     Check the scalar input parameters. */
    if (!(isrow || lsame_(typet, "C", 1L, 1L))) {
        *info = -1;
    } else if (*p < 0) {
        *info = -2;
    } else if (*q < 0) {
        *info = -3;
    } else if (*k < 0 || *k > *p) {
        *info = -4;
    } else if (*lda < 1 || isrow && *lda < *p || !isrow && *lda < *k) {
        *info = -6;
    } else if (*ldb < 1 || isrow && *ldb < *q || !isrow && *ldb < *k) {
        *info = -8;
    } else if (*lcs < (*k << 1) + min(*k, *q)) {
        *info = -10;
    } else if (*ldwork < max(1, *k)) {
        dwork[1] = (doublereal)max(1, *k);
        *info = -12;
    }
    /*     Return if there were illegal values. */
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("MB02CX", &i__1, 6L);
        return 0;
    }
    /*     Quick return if possible. */
    if (min(*q, *k) == 0) {
        dwork[1] = 1.;
        return 0;
    }
    if (isrow) {
        /*        The generator is row wise stored. */
        /*        Step 0: Do QR decomposition of B. */
        dgeqrf_(q, k, &b[b_offset], ldb, &cs[(*k << 1) + 1], &dwork[1], ldwork, &ierr);
        maxwrk = dwork[1];
        i__1 = *k;
        for (i__ = 1; i__ <= i__1; ++i__) {
            /*           Step 1: annihilate the i-th column of B. */
            if (*q > 1) {
                i__2 = min(i__, *q);
                dlarfg_(&i__2, &b[i__ * b_dim1 + 1], &b[i__ * b_dim1 + 2], &c__1, &tau);
                alpha = b[i__ * b_dim1 + 1];
                b[i__ * b_dim1 + 1] = 1.;
                if (*k > i__) {
                    i__2 = min(i__, *q);
                    i__3 = *k - i__;
                    dlarf_("Left", &i__2, &i__3, &b[i__ * b_dim1 + 1], &c__1, &tau,
                        &b[(i__ + 1) * b_dim1 + 1], ldb, &dwork[1], 4L);
                }
                b[i__ * b_dim1 + 1] = alpha;
            } else {
                alpha = b[i__ * b_dim1 + 1];
                tau = 0.;
            }
            /*           Step 2: annihilate the top entry of the column. */
            beta = a[i__ + i__ * a_dim1];
            ma02fd_(&beta, &alpha, &c__, &s, &ierr);
            if (ierr != 0) {
                /*              Error return:  The matrix is not positive definite. */
                *info = 1;
                return 0;
            }
            cs[(i__ << 1) - 1] = c__;
            cs[i__ * 2] = s;
            i__2 = *k - i__ + 1;
            d__1 = 1. / c__;
            dscal_(&i__2, &d__1, &a[i__ + i__ * a_dim1], lda);
            i__2 = *k - i__ + 1;
            d__1 = -s / c__;
            daxpy_(&i__2, &d__1, &b[i__ * b_dim1 + 1], ldb, &a[i__ + i__ * a_dim1], lda);
            i__2 = *k - i__ + 1;
            dscal_(&i__2, &c__, &b[i__ * b_dim1 + 1], ldb);
            i__2 = *k - i__ + 1;
            d__1 = -s;
            daxpy_(&i__2, &d__1, &a[i__ + i__ * a_dim1], lda, &b[i__ * b_dim1 + 1], ldb);
            b[i__ * b_dim1 + 1] = tau;
            /* L10: */
        }
    } else {
        /*        The generator is column wise stored. */
        /*        Step 0: Do LQ decomposition of B. */
        dgelqf_(k, q, &b[b_offset], ldb, &cs[(*k << 1) + 1], &dwork[1], ldwork, &ierr);
        maxwrk = dwork[1];
        i__1 = *k;
        for (i__ = 1; i__ <= i__1; ++i__) {
            /*           Step 1: annihilate the i-th row of B. */
            if (*q > 1) {
                i__2 = min(i__, *q);
                dlarfg_(&i__2, &b[i__ + b_dim1], &b[i__ + (b_dim1 << 1)], ldb, &tau);
                alpha = b[i__ + b_dim1];
                b[i__ + b_dim1] = 1.;
                if (*k > i__) {
                    i__2 = *k - i__;
                    i__3 = min(i__, *q);
                    dlarf_("Right", &i__2, &i__3, &b[i__ + b_dim1], ldb, &tau, &b[i__ + 1 + b_dim1],
                        ldb, &dwork[1], 5L);
                }
                b[i__ + b_dim1] = alpha;
            } else {
                alpha = b[i__ + b_dim1];
                tau = 0.;
            }
            /*           Step 2: annihilate the left entry of the row. */
            beta = a[i__ + i__ * a_dim1];
            ma02fd_(&beta, &alpha, &c__, &s, &ierr);
            if (ierr != 0) {
                /*              Error return:  The matrix is not positive definite. */
                *info = 1;
                return 0;
            }
            cs[(i__ << 1) - 1] = c__;
            cs[i__ * 2] = s;
            i__2 = *k - i__ + 1;
            d__1 = 1. / c__;
            dscal_(&i__2, &d__1, &a[i__ + i__ * a_dim1], &c__1);
            i__2 = *k - i__ + 1;
            d__1 = -s / c__;
            daxpy_(&i__2, &d__1, &b[i__ + b_dim1], &c__1, &a[i__ + i__ * a_dim1], &c__1);
            i__2 = *k - i__ + 1;
            dscal_(&i__2, &c__, &b[i__ + b_dim1], &c__1);
            i__2 = *k - i__ + 1;
            d__1 = -s;
            daxpy_(&i__2, &d__1, &a[i__ + i__ * a_dim1], &c__1, &b[i__ + b_dim1], &c__1);
            b[i__ + b_dim1] = tau;
            /* L20: */
        }
    }
    dwork[1] = maxwrk;
    return 0;
    /* *** Last line of MB02CX *** */
} /* mb02cx_ */
