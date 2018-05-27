/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static doublereal c_b4 = 1.;
static doublereal c_b5 = 0.;
static doublereal c_b27 = -1.;

doublereal ma02jd_(ltran1, ltran2, n, q1, ldq1, q2, ldq2, res, ldres) logical *ltran1, *ltran2;
integer* n;
doublereal* q1;
integer* ldq1;
doublereal* q2;
integer* ldq2;
doublereal* res;
integer* ldres;
{
    /* System generated locals */
    integer q1_dim1, q1_offset, q2_dim1, q2_offset, res_dim1, res_offset, i__1;
    doublereal ret_val, d__1;
    /* Builtin functions */
    double sqrt();
    /* Local variables */
    static doublereal temp;
    static integer i__;
    extern EXPORTSYMBOL /* Subroutine */ int dgemm_();
    static doublereal dummy[1];
    extern doublereal dlapy2_(), dlange_();
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
    /*     To compute || Q^T Q - I ||_F for a matrix of the form */
    /*                       [  op( Q1 )  op( Q2 ) ] */
    /*                  Q =  [                     ], */
    /*                       [ -op( Q2 )  op( Q1 ) ] */
    /*     where Q1 and Q2 are N-by-N matrices. This residual can be used to */
    /*     test wether Q is numerically an orthogonal symplectic matrix. */
    /*     FUNCTION VALUE */
    /*     MA02JD  DOUBLE PRECISION */
    /*             The computed residual. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     LTRAN1  LOGICAL */
    /*             Specifies the form of op( Q1 ) as follows: */
    /*             = .FALSE.:  op( Q1 ) = Q1; */
    /*             = .TRUE. :  op( Q1 ) = Q1'. */
    /*     LTRAN2  LOGICAL */
    /*             Specifies the form of op( Q2 ) as follows: */
    /*             = .FALSE.:  op( Q2 ) = Q2; */
    /*             = .TRUE. :  op( Q2 ) = Q2'. */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the matrices Q1 and Q2.  N >= 0. */
    /*     Q1      (input) DOUBLE PRECISION array, dimension (LDQ1,N) */
    /*             On entry, the leading N-by-N part of this array must */
    /*             contain the matrix op( Q1 ). */
    /*     LDQ1    INTEGER */
    /*             The leading dimension of the array Q1.  LDQ1 >= MAX(1,N). */
    /*     Q2      (input) DOUBLE PRECISION array, dimension (LDQ2,N) */
    /*             On entry, the leading N-by-N part of this array must */
    /*             contain the matrix op( Q2 ). */
    /*     LDQ2    INTEGER */
    /*             The leading dimension of the array Q2.  LDQ2 >= MAX(1,N). */
    /*     Workspace */
    /*     RES     DOUBLE PRECISION array, dimension (LDRES,N) */
    /*     LDRES   INTEGER */
    /*             The leading dimension of the array RES.  LDRES >= MAX(1,N). */
    /*     METHOD */
    /*     The routine computes the residual by simple elementary operations. */
    /*     CONTRIBUTORS */
    /*     D. Kressner, Technical Univ. Berlin, Germany, and */
    /*     P. Benner, Technical Univ. Chemnitz, Germany, December 2003. */
    /*     REVISIONS */
    /*     V. Sima, June 2008 (SLICOT version of the HAPACK routine DLAORS). */
    /*     KEYWORDS */
    /*     Elementary operations. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. Local Arrays .. */
    /*     .. External Subroutines .. */
    /*     .. External Functions .. */
    /*     .. Intrinsic Functions .. */
    /*     .. Executable Statements .. */
    /* Parameter adjustments */
    q1_dim1 = *ldq1;
    q1_offset = q1_dim1 + 1;
    q1 -= q1_offset;
    q2_dim1 = *ldq2;
    q2_offset = q2_dim1 + 1;
    q2 -= q2_offset;
    res_dim1 = *ldres;
    res_offset = res_dim1 + 1;
    res -= res_offset;
    /* Function Body */
    if (*ltran1) {
        dgemm_("No Transpose", "Transpose", n, n, n, &c_b4, &q1[q1_offset], ldq1, &q1[q1_offset],
            ldq1, &c_b5, &res[res_offset], ldres, 12L, 9L);
    } else {
        dgemm_("Transpose", "No Transpose", n, n, n, &c_b4, &q1[q1_offset], ldq1, &q1[q1_offset],
            ldq1, &c_b5, &res[res_offset], ldres, 9L, 12L);
    }
    if (*ltran2) {
        dgemm_("No Transpose", "Transpose", n, n, n, &c_b4, &q2[q2_offset], ldq2, &q2[q2_offset],
            ldq2, &c_b4, &res[res_offset], ldres, 12L, 9L);
    } else {
        dgemm_("Transpose", "No Transpose", n, n, n, &c_b4, &q2[q2_offset], ldq2, &q2[q2_offset],
            ldq2, &c_b4, &res[res_offset], ldres, 9L, 12L);
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
        res[i__ + i__ * res_dim1] += -1.;
        /* L10: */
    }
    temp = dlange_("Frobenius", n, n, &res[res_offset], ldres, dummy, 9L);
    if (*ltran1 && *ltran2) {
        dgemm_("No Transpose", "Transpose", n, n, n, &c_b4, &q2[q2_offset], ldq2, &q1[q1_offset],
            ldq1, &c_b5, &res[res_offset], ldres, 12L, 9L);
        dgemm_("No Transpose", "Transpose", n, n, n, &c_b4, &q1[q1_offset], ldq1, &q2[q2_offset],
            ldq2, &c_b27, &res[res_offset], ldres, 12L, 9L);
    } else if (*ltran1) {
        dgemm_("Transpose", "Transpose", n, n, n, &c_b4, &q2[q2_offset], ldq2, &q1[q1_offset], ldq1,
            &c_b5, &res[res_offset], ldres, 9L, 9L);
        dgemm_("No Transpose", "No Transpose", n, n, n, &c_b4, &q1[q1_offset], ldq1, &q2[q2_offset],
            ldq2, &c_b27, &res[res_offset], ldres, 12L, 12L);
    } else if (*ltran2) {
        dgemm_("No Transpose", "No Transpose", n, n, n, &c_b4, &q2[q2_offset], ldq2, &q1[q1_offset],
            ldq1, &c_b5, &res[res_offset], ldres, 12L, 12L);
        dgemm_("Transpose", "Transpose", n, n, n, &c_b4, &q1[q1_offset], ldq1, &q2[q2_offset], ldq2,
            &c_b27, &res[res_offset], ldres, 9L, 9L);
    } else {
        dgemm_("Transpose", "No Transpose", n, n, n, &c_b4, &q2[q2_offset], ldq2, &q1[q1_offset],
            ldq1, &c_b5, &res[res_offset], ldres, 9L, 12L);
        dgemm_("Transpose", "No Transpose", n, n, n, &c_b4, &q1[q1_offset], ldq1, &q2[q2_offset],
            ldq2, &c_b27, &res[res_offset], ldres, 9L, 12L);
    }
    d__1 = dlange_("Frobenius", n, n, &res[res_offset], ldres, dummy, 9L);
    temp = dlapy2_(&temp, &d__1);
    ret_val = sqrt(2.) * temp;
    return ret_val;
    /* *** Last line of MA02JD *** */
} /* ma02jd_ */
