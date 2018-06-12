/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c__1 = 1;

EXPORTSYMBOL /* Subroutine */ int mc01od_(k, rez, imz, rep, imp, dwork, info) integer* k;
doublereal *rez, *imz, *rep, *imp, *dwork;
integer* info;
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;
    /* Local variables */
    static integer i__;
    static doublereal u, v;
    extern /* Subroutine */ int dcopy_(), daxpy_();
    static integer k2;
    extern /* Subroutine */ int xerbla_();
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
    /*     To compute the coefficients of a complex polynomial P(x) from its */
    /*     zeros. */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     K       (input) INTEGER */
    /*             The number of zeros (and hence the degree) of P(x). */
    /*             K >= 0. */
    /*     REZ     (input) DOUBLE PRECISION array, dimension (K) */
    /*     IMZ     (input) DOUBLE PRECISION array, dimension (K) */
    /*             The real and imaginary parts of the i-th zero of P(x) */
    /*             must be stored in REZ(i) and IMZ(i), respectively, where */
    /*             i = 1, 2, ..., K. The zeros may be supplied in any order. */
    /*     REP     (output) DOUBLE PRECISION array, dimension (K+1) */
    /*     IMP     (output) DOUBLE PRECISION array, dimension (K+1) */
    /*             These arrays contain the real and imaginary parts, */
    /*             respectively, of the coefficients of P(x) in increasing */
    /*             powers of x. If K = 0, then REP(1) is set to one and */
    /*             IMP(1) is set to zero. */
    /*     Workspace */
    /*     DWORK   DOUBLE PRECISION array, dimension (2*K+2) */
    /*             If K = 0, this array is not referenced. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -i, the i-th argument had an illegal */
    /*                   value. */
    /*     METHOD */
    /*     The routine computes the coefficients of the complex K-th degree */
    /*     polynomial P(x) as */
    /*        P(x) = (x - r(1)) * (x - r(2)) * ... * (x - r(K)) */
    /*     where r(i) = (REZ(i),IMZ(i)), using real arithmetic. */
    /*     NUMERICAL ASPECTS */
    /*     None. */
    /*     CONTRIBUTORS */
    /*     Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Mar. 1997. */
    /*     Supersedes Release 2.0 routine MC01CD by Alan Brown and */
    /*     A.J. Geurts. */
    /*     REVISIONS */
    /*     V. Sima, May 2002. */
    /*     KEYWORDS */
    /*     Elementary polynomial operations, polynomial operations. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. External Subroutines .. */
    /*     .. Executable Statements .. */
    /*     Test the input scalar arguments. */
    /* Parameter adjustments */
    --dwork;
    --imp;
    --rep;
    --imz;
    --rez;
    /* Function Body */
    if (*k < 0) {
        *info = -1;
        /*        Error return. */
        i__1 = -(*info);
        xerbla_("MC01OD", &i__1, 6L);
        return 0;
    }
    /*     Quick return if possible. */
    *info = 0;
    rep[1] = 1.;
    imp[1] = 0.;
    if (*k == 0) {
        return 0;
    }
    k2 = *k + 2;
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        u = rez[i__];
        v = imz[i__];
        dwork[1] = 0.;
        dwork[k2] = 0.;
        dcopy_(&i__, &rep[1], &c__1, &dwork[2], &c__1);
        dcopy_(&i__, &imp[1], &c__1, &dwork[k2 + 1], &c__1);
        if (u != 0.) {
            d__1 = -u;
            daxpy_(&i__, &d__1, &rep[1], &c__1, &dwork[1], &c__1);
            d__1 = -u;
            daxpy_(&i__, &d__1, &imp[1], &c__1, &dwork[k2], &c__1);
        }
        if (v != 0.) {
            daxpy_(&i__, &v, &imp[1], &c__1, &dwork[1], &c__1);
            d__1 = -v;
            daxpy_(&i__, &d__1, &rep[1], &c__1, &dwork[k2], &c__1);
        }
        i__2 = i__ + 1;
        dcopy_(&i__2, &dwork[1], &c__1, &rep[1], &c__1);
        i__2 = i__ + 1;
        dcopy_(&i__2, &dwork[k2], &c__1, &imp[1], &c__1);
        /* L20: */
    }
    return 0;
    /* *** Last line of MC01OD *** */
} /* mc01od_ */
