/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

EXPORTSYMBOL /* Subroutine */ int ma01bd_(
    base, lgbas, k, s, a, inca, alpha, beta, scal) doublereal *base,
    *lgbas;
integer *k, *s;
doublereal* a;
integer* inca;
doublereal *alpha, *beta;
integer* scal;
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;
    /* Builtin functions */
    double log(), pow_dd();
    /* Local variables */
    static doublereal temp;
    static integer i__, sl;
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
    /*     To compute the general product of K real scalars without over- */
    /*     or underflow. */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     BASE    (input)  DOUBLE PRECISION */
    /*             Machine base. */
    /*     LGBAS   (input)  DOUBLE PRECISION */
    /*             Logarithm of BASE. */
    /*     K       (input)  INTEGER */
    /*             The number of scalars.  K >= 1. */
    /*     S       (input)  INTEGER array, dimension (K) */
    /*             The signature array. Each entry of S must be 1 or -1. */
    /*     A       (input)  DOUBLE PRECISION array, dimension (K) */
    /*             Vector of real scalars. */
    /*     INCA    (input)  INTEGER */
    /*             Increment for the array A. INCA <> 0. */
    /*     ALPHA   (output)  DOUBLE PRECISION */
    /*             ALPHA is a real scalar such that */
    /*                ALPHA / BETA * BASE**(SCAL) */
    /*             is the general product of the scalars in the array A. */
    /*     BETA    (output)  DOUBLE PRECISION */
    /*             BETA is either 0.0 or 1.0. */
    /*             See also the description of ALPHA. */
    /*     SCAL    (output)  INTEGER */
    /*             Scaling factor exponent, see ALPHA. */
    /*     CONTRIBUTOR */
    /*     D. Kressner, Technical Univ. Berlin, Germany, June 2001. */
    /*     REVISIONS */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Romania, */
    /*     July 2009, SLICOT Library version of the routine PLAPR1. */
    /*     KEYWORDS */
    /*     Computer arithmetic, overflow, underflow. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. Intrinsic Functions .. */
    /*     .. Executable Statements .. */
    /* Parameter adjustments */
    --a;
    --s;
    /* Function Body */
    *alpha = 1.;
    *beta = 1.;
    *scal = 0;
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        temp = a[(i__ - 1) * *inca + 1];
        if (temp != 0.) {
            sl = (integer)(log((abs(temp))) / *lgbas);
            d__1 = (doublereal)sl;
            temp /= pow_dd(base, &d__1);
        }
        if (s[i__] == 1) {
            *alpha *= temp;
            *scal += sl;
        } else {
            *beta *= temp;
            *scal -= sl;
        }
        if (i__ % 10 == 0) {
            if (*alpha != 0.) {
                sl = (integer)(log((abs(*alpha))) / *lgbas);
                *scal += sl;
                d__1 = (doublereal)sl;
                *alpha /= pow_dd(base, &d__1);
            }
            if (*beta != 0.) {
                sl = (integer)(log((abs(*beta))) / *lgbas);
                *scal -= sl;
                d__1 = (doublereal)sl;
                *beta /= pow_dd(base, &d__1);
            }
        }
        /* L10: */
    }
    if (*beta != 0.) {
        *alpha /= *beta;
        *beta = 1.;
    }
    if (*alpha == 0.) {
        *scal = 0;
    } else {
        sl = (integer)(log((abs(*alpha))) / *lgbas);
        d__1 = (doublereal)sl;
        *alpha /= pow_dd(base, &d__1);
        *scal += sl;
    }
    return 0;
    /* *** Last line of MA01BD *** */
} /* ma01bd_ */
