/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c__1 = 1;

EXPORTSYMBOL /* Subroutine */ int sb08ny_(da, a, b, epsb) integer* da;
doublereal *a, *b, *epsb;
{
    /* System generated locals */
    integer i__1, i__2;
    /* Local variables */
    extern doublereal ddot_();
    static integer i__;
    extern doublereal dlamch_();
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
    /*     To compute the coefficients of B(z) = A(1/z) * A(z) and a norm for */
    /*     the accuracy of the computed coefficients. */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     DA      (input) INTEGER */
    /*             The degree of the polynomials A(z) and B(z).  DA >= 0. */
    /*     A       (input) DOUBLE PRECISION array, dimension (DA+1) */
    /*             This array must contain the coefficients of the polynomial */
    /*             A(z) in increasing powers of z. */
    /*     B       (output) DOUBLE PRECISION array, dimension (DA+1) */
    /*             This array contains the coefficients of the polynomial */
    /*             B(z). */
    /*     EPSB    (output) DOUBLE PRECISION */
    /*             A value used for checking the accuracy of the computed */
    /*             coefficients. */
    /*     CONTRIBUTOR */
    /*     Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Aug. 1997. */
    /*     Supersedes Release 2.0 routine SB08BZ by A.J. Geurts. */
    /*     REVISIONS */
    /*     - */
    /*     KEYWORDS */
    /*     Laplace transform, polynomial operations, spectral factorization. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. External Functions .. */
    /*     .. Executable Statements .. */
    /* Parameter adjustments */
    --b;
    --a;
    /* Function Body */
    i__1 = *da + 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
        i__2 = *da - i__ + 2;
        b[i__] = ddot_(&i__2, &a[1], &c__1, &a[i__], &c__1);
        /* L20: */
    }
    *epsb = dlamch_("Epsilon", 7L) * 3. * b[1];
    return 0;
    /* *** Last line of SB08NY *** */
} /* sb08ny_ */
