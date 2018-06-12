/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c__1 = 1;

EXPORTSYMBOL /* Subroutine */ int sb04ny_(
    rc, ul, m, a, lda, lambda, d__, tol, iwork, dwork, lddwor, info, rc_len, ul_len) char *rc,
    *ul;
integer* m;
doublereal* a;
integer* lda;
doublereal *lambda, *d__, *tol;
integer* iwork;
doublereal* dwork;
integer *lddwor, *info;
ftnlen rc_len;
ftnlen ul_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, dwork_dim1, dwork_offset, i__1, i__2, i__3;
    /* Local variables */
    extern /* Subroutine */ int drot_();
    static doublereal c__;
    static integer j;
    static doublereal r__, s;
    extern logical lsame_();
    static doublereal rcond;
    extern /* Subroutine */ int dcopy_();
    static char trans[1];
    static integer j1;
    extern /* Subroutine */ int dtrsv_();
    static integer mj;
    extern /* Subroutine */ int dlartg_(), dtrcon_();
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
    /*     To solve a system of equations in Hessenberg form with one */
    /*     offdiagonal and one right-hand side. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     RC      CHARACTER*1 */
    /*             Indicates processing by columns or rows, as follows: */
    /*             = 'R':  Row transformations are applied; */
    /*             = 'C':  Column transformations are applied. */
    /*     UL      CHARACTER*1 */
    /*             Indicates whether AB is upper or lower Hessenberg matrix, */
    /*             as follows: */
    /*             = 'U':  AB is upper Hessenberg; */
    /*             = 'L':  AB is lower Hessenberg. */
    /*     Input/Output Parameters */
    /*     M       (input) INTEGER */
    /*             The order of the matrix A.  M >= 0. */
    /*     A       (input) DOUBLE PRECISION array, dimension (LDA,M) */
    /*             The leading M-by-M part of this array must contain a */
    /*             matrix A in Hessenberg form. */
    /*     LDA     INTEGER */
    /*             The leading dimension of array A.  LDA >= MAX(1,M). */
    /*     LAMBDA  (input) DOUBLE PRECISION */
    /*             This variable must contain the value to be added to the */
    /*             diagonal elements of A. */
    /*     D       (input/output) DOUBLE PRECISION array, dimension (M) */
    /*             On entry, this array must contain the right-hand side */
    /*             vector of the Hessenberg system. */
    /*             On exit, if INFO = 0, this array contains the solution */
    /*             vector of the Hessenberg system. */
    /*     Tolerances */
    /*     TOL     DOUBLE PRECISION */
    /*             The tolerance to be used to test for near singularity of */
    /*             the triangular factor R of the Hessenberg matrix. A matrix */
    /*             whose estimated condition number is less than 1/TOL is */
    /*             considered to be nonsingular. */
    /*     Workspace */
    /*     IWORK   INTEGER array, dimension (M) */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDDWOR,M+3) */
    /*             The leading M-by-M part of this array is used for */
    /*             computing the triangular factor of the QR decomposition */
    /*             of the Hessenberg matrix. The remaining 3*M elements are */
    /*             used as workspace for the computation of the reciprocal */
    /*             condition estimate. */
    /*     LDDWOR  INTEGER */
    /*             The leading dimension of array DWORK.  LDDWOR >= MAX(1,M). */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             = 1:  if the Hessenberg matrix is (numerically) singular. */
    /*                   That is, its estimated reciprocal condition number */
    /*                   is less than or equal to TOL. */
    /*     NUMERICAL ASPECTS */
    /*     None. */
    /*     CONTRIBUTORS */
    /*     Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Aug. 1997. */
    /*     Supersedes Release 2.0 routine SB04BY by M. Vanbegin, and */
    /*     P. Van Dooren, Philips Research Laboratory, Brussels, Belgium. */
    /*     REVISIONS */
    /*     - */
    /*     Note that RC, UL, M and LDA must be such that the value of the */
    /*     LOGICAL variable OK in the following statement is true. */
    /*      OK = ( ( UL.EQ.'U' ) .OR. ( UL.EQ.'u' ) .OR. */
    /*             ( UL.EQ.'L' ) .OR. ( UL.EQ.'l' ) ) */
    /*           .AND. */
    /*           ( ( RC.EQ.'R' ) .OR. ( RC.EQ.'r' ) .OR. */
    /*             ( RC.EQ.'C' ) .OR. ( RC.EQ.'c' ) ) */
    /*           .AND. */
    /*           ( M.GE.0 ) */
    /*           .AND. */
    /*           ( LDA.GE.MAX( 1, M ) ) */
    /*           .AND. */
    /*           ( LDDWOR.GE.MAX( 1, M ) ) */
    /*     KEYWORDS */
    /*     Hessenberg form, orthogonal transformation, real Schur form, */
    /*     Sylvester equation. */
    /*     ****************************************************************** */
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
    --d__;
    --iwork;
    dwork_dim1 = *lddwor;
    dwork_offset = dwork_dim1 + 1;
    dwork -= dwork_offset;
    /* Function Body */
    *info = 0;
    /*     For speed, no tests on the input scalar arguments are made. */
    /*     Quick return if possible. */
    if (*m == 0) {
        return 0;
    }
    if (lsame_(ul, "U", 1L, 1L)) {
        i__1 = *m;
        for (j = 1; j <= i__1; ++j) {
            /* Computing MIN */
            i__3 = j + 1;
            i__2 = min(i__3, *m);
            dcopy_(&i__2, &a[j * a_dim1 + 1], &c__1, &dwork[j * dwork_dim1 + 1], &c__1);
            dwork[j + j * dwork_dim1] += *lambda;
            /* L20: */
        }
        if (lsame_(rc, "R", 1L, 1L)) {
            *(unsigned char*)trans = 'N';
            /*           A is an upper Hessenberg matrix, row transformations. */
            i__1 = *m - 1;
            for (j = 1; j <= i__1; ++j) {
                mj = *m - j;
                if (dwork[j + 1 + j * dwork_dim1] != 0.) {
                    dlartg_(
                        &dwork[j + j * dwork_dim1], &dwork[j + 1 + j * dwork_dim1], &c__, &s, &r__);
                    dwork[j + j * dwork_dim1] = r__;
                    dwork[j + 1 + j * dwork_dim1] = 0.;
                    drot_(&mj, &dwork[j + (j + 1) * dwork_dim1], lddwor,
                        &dwork[j + 1 + (j + 1) * dwork_dim1], lddwor, &c__, &s);
                    drot_(&c__1, &d__[j], &c__1, &d__[j + 1], &c__1, &c__, &s);
                }
                /* L40: */
            }
        } else {
            *(unsigned char*)trans = 'T';
            /*           A is an upper Hessenberg matrix, column transformations. */
            i__1 = *m - 1;
            for (j = 1; j <= i__1; ++j) {
                mj = *m - j;
                if (dwork[mj + 1 + mj * dwork_dim1] != 0.) {
                    dlartg_(&dwork[mj + 1 + (mj + 1) * dwork_dim1],
                        &dwork[mj + 1 + mj * dwork_dim1], &c__, &s, &r__);
                    dwork[mj + 1 + (mj + 1) * dwork_dim1] = r__;
                    dwork[mj + 1 + mj * dwork_dim1] = 0.;
                    drot_(&mj, &dwork[(mj + 1) * dwork_dim1 + 1], &c__1,
                        &dwork[mj * dwork_dim1 + 1], &c__1, &c__, &s);
                    drot_(&c__1, &d__[mj + 1], &c__1, &d__[mj], &c__1, &c__, &s);
                }
                /* L60: */
            }
        }
    } else {
        i__1 = *m;
        for (j = 1; j <= i__1; ++j) {
            /* Computing MAX */
            i__2 = j - 1;
            j1 = max(i__2, 1);
            i__2 = *m - j1 + 1;
            dcopy_(&i__2, &a[j1 + j * a_dim1], &c__1, &dwork[j1 + j * dwork_dim1], &c__1);
            dwork[j + j * dwork_dim1] += *lambda;
            /* L80: */
        }
        if (lsame_(rc, "R", 1L, 1L)) {
            *(unsigned char*)trans = 'N';
            /*           A is a lower Hessenberg matrix, row transformations. */
            i__1 = *m - 1;
            for (j = 1; j <= i__1; ++j) {
                mj = *m - j;
                if (dwork[mj + (mj + 1) * dwork_dim1] != 0.) {
                    dlartg_(&dwork[mj + 1 + (mj + 1) * dwork_dim1],
                        &dwork[mj + (mj + 1) * dwork_dim1], &c__, &s, &r__);
                    dwork[mj + 1 + (mj + 1) * dwork_dim1] = r__;
                    dwork[mj + (mj + 1) * dwork_dim1] = 0.;
                    drot_(&mj, &dwork[mj + 1 + dwork_dim1], lddwor, &dwork[mj + dwork_dim1], lddwor,
                        &c__, &s);
                    drot_(&c__1, &d__[mj + 1], &c__1, &d__[mj], &c__1, &c__, &s);
                }
                /* L100: */
            }
        } else {
            *(unsigned char*)trans = 'T';
            /*           A is a lower Hessenberg matrix, column transformations. */
            i__1 = *m - 1;
            for (j = 1; j <= i__1; ++j) {
                mj = *m - j;
                if (dwork[j + (j + 1) * dwork_dim1] != 0.) {
                    dlartg_(&dwork[j + j * dwork_dim1], &dwork[j + (j + 1) * dwork_dim1], &c__, &s,
                        &r__);
                    dwork[j + j * dwork_dim1] = r__;
                    dwork[j + (j + 1) * dwork_dim1] = 0.;
                    drot_(&mj, &dwork[j + 1 + j * dwork_dim1], &c__1,
                        &dwork[j + 1 + (j + 1) * dwork_dim1], &c__1, &c__, &s);
                    drot_(&c__1, &d__[j], &c__1, &d__[j + 1], &c__1, &c__, &s);
                }
                /* L120: */
            }
        }
    }
    dtrcon_("1-norm", ul, "Non-unit", m, &dwork[dwork_offset], lddwor, &rcond,
        &dwork[(*m + 1) * dwork_dim1 + 1], &iwork[1], info, 6L, 1L, 8L);
    if (rcond <= *tol) {
        *info = 1;
    } else {
        dtrsv_(ul, trans, "Non-unit", m, &dwork[dwork_offset], lddwor, &d__[1], &c__1, 1L, 1L, 8L);
    }
    return 0;
    /* *** Last line of SB04NY *** */
} /* sb04ny_ */
