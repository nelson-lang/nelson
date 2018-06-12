/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static doublereal c_b12 = 0.;

EXPORTSYMBOL /* Subroutine */ int mb02cy_(typet, strucg, p, q, n, k, a, lda, b, ldb, h__, ldh, cs,
    lcs, dwork, ldwork, info, typet_len, strucg_len) char *typet,
    *strucg;
integer *p, *q, *n, *k;
doublereal* a;
integer* lda;
doublereal* b;
integer* ldb;
doublereal* h__;
integer* ldh;
doublereal* cs;
integer* lcs;
doublereal* dwork;
integer *ldwork, *info;
ftnlen typet_len;
ftnlen strucg_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, h_dim1, h_offset, i__1, i__2;
    doublereal d__1;
    /* Local variables */
    static integer ierr;
    static doublereal c__;
    static integer i__;
    static doublereal s;
    extern /* Subroutine */ int dscal_(), dlarf_();
    extern logical lsame_();
    extern /* Subroutine */ int daxpy_();
    static logical islwr, isrow;
    static integer ci;
    extern /* Subroutine */ int dlaset_(), xerbla_(), dormlq_(), dormqr_();
    static integer maxwrk;
    static doublereal tau;
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
    /*     To apply the transformations created by the SLICOT Library */
    /*     routine MB02CX on other columns / rows of the generator, */
    /*     contained in the arrays A and B of positive and negative */
    /*     generators, respectively. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     TYPET   CHARACTER*1 */
    /*             Specifies the type of the generator, as follows: */
    /*             = 'R':  A and B are additional columns of the generator; */
    /*             = 'C':  A and B are additional rows of the generator. */
    /*             Note:   in the sequel, the notation x / y means that */
    /*                     x corresponds to TYPET = 'R' and y corresponds to */
    /*                     TYPET = 'C'. */
    /*     STRUCG  CHARACTER*1 */
    /*             Information about the structure of the two generators, */
    /*             as follows: */
    /*             = 'T':  the trailing block of the positive generator */
    /*                     is lower / upper triangular, and the trailing */
    /*                     block of the negative generator is zero; */
    /*             = 'N':  no special structure to mention. */
    /*     Input/Output Parameters */
    /*     P       (input)  INTEGER */
    /*             The number of rows / columns in A containing the positive */
    /*             generators.  P >= 0. */
    /*     Q       (input)  INTEGER */
    /*             The number of rows / columns in B containing the negative */
    /*             generators.  Q >= 0. */
    /*     N       (input)  INTEGER */
    /*             The number of columns / rows in A and B to be processed. */
    /*             N >= 0. */
    /*     K       (input)  INTEGER */
    /*             The number of columns / rows in H.  P >= K >= 0. */
    /*     A       (input/output)  DOUBLE PRECISION array, dimension */
    /*             (LDA, N) / (LDA, P) */
    /*             On entry, the leading P-by-N / N-by-P part of this array */
    /*             must contain the positive part of the generator. */
    /*             On exit, the leading P-by-N / N-by-P part of this array */
    /*             contains the transformed positive part of the generator. */
    /*     LDA     INTEGER */
    /*             The leading dimension of the array A. */
    /*             LDA >= MAX(1,P),    if TYPET = 'R'; */
    /*             LDA >= MAX(1,N),    if TYPET = 'C'. */
    /*     B       (input/output)  DOUBLE PRECISION array, dimension */
    /*             (LDB, N) / (LDB, Q) */
    /*             On entry, the leading Q-by-N / N-by-Q part of this array */
    /*             must contain the negative part of the generator. */
    /*             On exit, the leading Q-by-N / N-by-Q part of this array */
    /*             contains the transformed negative part of the generator. */
    /*     LDB     INTEGER */
    /*             The leading dimension of the array B. */
    /*             LDB >= MAX(1,Q),    if TYPET = 'R'; */
    /*             LDB >= MAX(1,N),    if TYPET = 'C'. */
    /*     H       (input)  DOUBLE PRECISION array, dimension */
    /*             (LDH, K) / (LDH, Q) */
    /*             The leading Q-by-K / K-by-Q part of this array must */
    /*             contain part of the necessary information for the */
    /*             Householder transformations computed by SLICOT Library */
    /*             routine MB02CX. */
    /*     LDH     INTEGER */
    /*             The leading dimension of the array H. */
    /*             LDH >= MAX(1,Q),    if TYPET = 'R'; */
    /*             LDH >= MAX(1,K),    if TYPET = 'C'. */
    /*     CS      (input)  DOUBLE PRECISION array, dimension (LCS) */
    /*             The leading 2*K + MIN(K,Q) part of this array must */
    /*             contain the necessary information for modified hyperbolic */
    /*             rotations and the scalar factors of the Householder */
    /*             transformations computed by SLICOT Library routine MB02CX. */
    /*     LCS     INTEGER */
    /*             The length of the array CS.  LCS >= 2*K + MIN(K,Q). */
    /*     Workspace */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             On exit, if  INFO = 0,  DWORK(1)  returns the optimal */
    /*             value of LDWORK. */
    /*             On exit, if  INFO = -16,  DWORK(1)  returns the minimum */
    /*             value of LDWORK. */
    /*     LDWORK  INTEGER */
    /*             The length of the array DWORK.  LDWORK >= MAX(1,N). */
    /*             For optimum performance LDWORK should be larger. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  succesful exit; */
    /*             < 0:  if INFO = -i, the i-th argument had an illegal */
    /*                   value. */
    /*     METHOD */
    /*     The Householder transformations and modified hyperbolic rotations */
    /*     computed by SLICOT Library routine MB02CX are applied to the */
    /*     corresponding parts of the matrices A and B. */
    /*     CONTRIBUTOR */
    /*     D. Kressner, Technical Univ. Chemnitz, Germany, June 2000. */
    /*     REVISIONS */
    /*     V. Sima, Research Institute for Informatics, Bucharest, July 2000, */
    /*     February 2004, March 2007. */
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
    h_dim1 = *ldh;
    h_offset = h_dim1 + 1;
    h__ -= h_offset;
    --cs;
    --dwork;
    /* Function Body */
    *info = 0;
    isrow = lsame_(typet, "R", 1L, 1L);
    islwr = lsame_(strucg, "T", 1L, 1L);
    /*     Check the scalar input parameters. */
    if (!(isrow || lsame_(typet, "C", 1L, 1L))) {
        *info = -1;
    } else if (!(islwr || lsame_(strucg, "N", 1L, 1L))) {
        *info = -2;
    } else if (*p < 0) {
        *info = -3;
    } else if (*q < 0) {
        *info = -4;
    } else if (*n < 0) {
        *info = -5;
    } else if (*k < 0 || *k > *p) {
        *info = -6;
    } else if (*lda < 1 || isrow && *lda < *p || !isrow && *lda < *n) {
        *info = -8;
    } else if (*ldb < 1 || isrow && *ldb < *q || !isrow && *ldb < *n) {
        *info = -10;
    } else if (*ldh < 1 || isrow && *ldh < *q || !isrow && *ldh < *k) {
        *info = -12;
    } else if (*lcs < (*k << 1) + min(*k, *q)) {
        *info = -14;
    } else if (*ldwork < max(1, *n)) {
        dwork[1] = (doublereal)max(1, *n);
        *info = -16;
    }
    /*     Return if there were illegal values. */
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("MB02CY", &i__1, 6L);
        return 0;
    }
    /*     Quick return if possible. */
    /* Computing MIN */
    i__1 = min(*n, *k);
    if (min(i__1, *q) == 0) {
        dwork[1] = 1.;
        return 0;
    }
    /*     Applying the transformations. */
    if (isrow) {
        /*        The generator is row wise stored. */
        if (islwr) {
            i__1 = *k;
            for (i__ = 1; i__ <= i__1; ++i__) {
                /*              Apply Householder transformation avoiding touching of */
                /*              zero blocks. */
                ci = *n - *k + i__ - 1;
                tau = h__[i__ * h_dim1 + 1];
                h__[i__ * h_dim1 + 1] = 1.;
                i__2 = min(i__, *q);
                dlarf_("Left", &i__2, &ci, &h__[i__ * h_dim1 + 1], &c__1, &tau, &b[b_offset], ldb,
                    &dwork[1], 4L);
                h__[i__ * h_dim1 + 1] = tau;
                /*              Now apply the hyperbolic rotation under the assumption */
                /*              that A(I, N-K+I+1:N) and B(1, N-K+I:N) are zero. */
                c__ = cs[(i__ << 1) - 1];
                s = cs[i__ * 2];
                d__1 = 1. / c__;
                dscal_(&ci, &d__1, &a[i__ + a_dim1], lda);
                d__1 = -s / c__;
                daxpy_(&ci, &d__1, &b[b_dim1 + 1], ldb, &a[i__ + a_dim1], lda);
                dscal_(&ci, &c__, &b[b_dim1 + 1], ldb);
                d__1 = -s;
                daxpy_(&ci, &d__1, &a[i__ + a_dim1], lda, &b[b_dim1 + 1], ldb);
                b[(*n - *k + i__) * b_dim1 + 1] = -s / c__ * a[i__ + (*n - *k + i__) * a_dim1];
                a[i__ + (*n - *k + i__) * a_dim1] = 1. / c__ * a[i__ + (*n - *k + i__) * a_dim1];
                /*              All below B(1,N-K+I) should be zero. */
                if (*q > 1) {
                    i__2 = *q - 1;
                    dlaset_("All", &i__2, &c__1, &c_b12, &c_b12, &b[(*n - *k + i__) * b_dim1 + 2],
                        &c__1, 3L);
                }
                /* L10: */
            }
        } else {
            /*           Apply the QR reduction on B. */
            i__1 = min(*k, *q);
            dormqr_("Left", "Transpose", q, n, &i__1, &h__[h_offset], ldh, &cs[(*k << 1) + 1],
                &b[b_offset], ldb, &dwork[1], ldwork, &ierr, 4L, 9L);
            maxwrk = (integer)dwork[1];
            i__1 = *k;
            for (i__ = 1; i__ <= i__1; ++i__) {
                /*              Apply Householder transformation. */
                tau = h__[i__ * h_dim1 + 1];
                h__[i__ * h_dim1 + 1] = 1.;
                i__2 = min(i__, *q);
                dlarf_("Left", &i__2, n, &h__[i__ * h_dim1 + 1], &c__1, &tau, &b[b_offset], ldb,
                    &dwork[1], 4L);
                h__[i__ * h_dim1 + 1] = tau;
                /*              Apply Hyperbolic Rotation. */
                c__ = cs[(i__ << 1) - 1];
                s = cs[i__ * 2];
                d__1 = 1. / c__;
                dscal_(n, &d__1, &a[i__ + a_dim1], lda);
                d__1 = -s / c__;
                daxpy_(n, &d__1, &b[b_dim1 + 1], ldb, &a[i__ + a_dim1], lda);
                dscal_(n, &c__, &b[b_dim1 + 1], ldb);
                d__1 = -s;
                daxpy_(n, &d__1, &a[i__ + a_dim1], lda, &b[b_dim1 + 1], ldb);
                /* L20: */
            }
        }
    } else {
        /*        The generator is column wise stored. */
        if (islwr) {
            i__1 = *k;
            for (i__ = 1; i__ <= i__1; ++i__) {
                /*              Apply Householder transformation avoiding touching zeros. */
                ci = *n - *k + i__ - 1;
                tau = h__[i__ + h_dim1];
                h__[i__ + h_dim1] = 1.;
                i__2 = min(i__, *q);
                dlarf_("Right", &ci, &i__2, &h__[i__ + h_dim1], ldh, &tau, &b[b_offset], ldb,
                    &dwork[1], 5L);
                h__[i__ + h_dim1] = tau;
                /*              Apply Hyperbolic Rotation. */
                c__ = cs[(i__ << 1) - 1];
                s = cs[i__ * 2];
                d__1 = 1. / c__;
                dscal_(&ci, &d__1, &a[i__ * a_dim1 + 1], &c__1);
                d__1 = -s / c__;
                daxpy_(&ci, &d__1, &b[b_dim1 + 1], &c__1, &a[i__ * a_dim1 + 1], &c__1);
                dscal_(&ci, &c__, &b[b_dim1 + 1], &c__1);
                d__1 = -s;
                daxpy_(&ci, &d__1, &a[i__ * a_dim1 + 1], &c__1, &b[b_dim1 + 1], &c__1);
                b[*n - *k + i__ + b_dim1] = -s / c__ * a[*n - *k + i__ + i__ * a_dim1];
                a[*n - *k + i__ + i__ * a_dim1] = 1. / c__ * a[*n - *k + i__ + i__ * a_dim1];
                /*              All elements right behind B(N-K+I,1) should be zero. */
                if (*q > 1) {
                    i__2 = *q - 1;
                    dlaset_("All", &c__1, &i__2, &c_b12, &c_b12, &b[*n - *k + i__ + (b_dim1 << 1)],
                        ldb, 3L);
                }
                /* L30: */
            }
        } else {
            /*           Apply the LQ reduction on B. */
            i__1 = min(*k, *q);
            dormlq_("Right", "Transpose", n, q, &i__1, &h__[h_offset], ldh, &cs[(*k << 1) + 1],
                &b[b_offset], ldb, &dwork[1], ldwork, &ierr, 5L, 9L);
            maxwrk = (integer)dwork[1];
            i__1 = *k;
            for (i__ = 1; i__ <= i__1; ++i__) {
                /*              Apply Householder transformation. */
                tau = h__[i__ + h_dim1];
                h__[i__ + h_dim1] = 1.;
                i__2 = min(i__, *q);
                dlarf_("Right", n, &i__2, &h__[i__ + h_dim1], ldh, &tau, &b[b_offset], ldb,
                    &dwork[1], 5L);
                h__[i__ + h_dim1] = tau;
                /*              Apply Hyperbolic Rotation. */
                c__ = cs[(i__ << 1) - 1];
                s = cs[i__ * 2];
                d__1 = 1. / c__;
                dscal_(n, &d__1, &a[i__ * a_dim1 + 1], &c__1);
                d__1 = -s / c__;
                daxpy_(n, &d__1, &b[b_dim1 + 1], &c__1, &a[i__ * a_dim1 + 1], &c__1);
                dscal_(n, &c__, &b[b_dim1 + 1], &c__1);
                d__1 = -s;
                daxpy_(n, &d__1, &a[i__ * a_dim1 + 1], &c__1, &b[b_dim1 + 1], &c__1);
                /* L40: */
            }
        }
    }
    dwork[1] = (doublereal)max(maxwrk, *n);
    return 0;
    /* *** Last line of MB02CY *** */
} /* mb02cy_ */
