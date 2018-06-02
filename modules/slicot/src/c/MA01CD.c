/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static doublereal c_b2 = 1.;

integer ma01cd_(a, ia, b, ib) doublereal* a;
integer* ia;
doublereal* b;
integer* ib;
{
    /* System generated locals */
    integer ret_val;
    /* Builtin functions */
    double d_sign(), log();
    /* Local variables */
    static doublereal s, sa, sb;
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
    /*     To compute, without over- or underflow, the sign of the sum of two */
    /*     real numbers represented using integer powers of a base (usually, */
    /*     the machine base). Any base can be used, but it should the same */
    /*     for both numbers. The result is an integer with value 1, 0, or -1, */
    /*     depending on the sum being found as positive, zero, or negative, */
    /*     respectively. */
    /*     FUNCTION VALUE */
    /*     MA01CD  INTEGER */
    /*             The sign of the sum of the two numbers, which is usually */
    /*             either 1, or -1. If both numbers are 0, or if they have */
    /*             the same exponent and their sum is 0, the returned value */
    /*             is 0. */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     A       (input)  DOUBLE PRECISION */
    /*             The first real scalar. */
    /*     IA      (input)  INTEGER */
    /*             Exponent of the base for the first real scalar. The scalar */
    /*             is represented as A * BASE**(IA). */
    /*     B       (input)  DOUBLE PRECISION */
    /*             The first real scalar. */
    /*     IB      (input)  INTEGER */
    /*             Exponent of the base for the first real scalar. The scalar */
    /*             is represented as B * BASE**(IB). */
    /*     CONTRIBUTOR */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Romania, */
    /*     Feb. 2010. */
    /*     REVISIONS */
    /*     - */
    /*     KEYWORDS */
    /*     Computer arithmetic, overflow, underflow. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. Intrinsic Functions .. */
    /*     .. Executable Statements .. */
    if (*a == 0. && *b == 0.) {
        ret_val = 0;
    } else if (*a == 0.) {
        ret_val = (integer)d_sign(&c_b2, b);
    } else if (*b == 0.) {
        ret_val = (integer)d_sign(&c_b2, a);
    } else if (*ia == *ib) {
        s = *a + *b;
        if (s == 0.) {
            ret_val = 0;
        } else {
            ret_val = (integer)d_sign(&c_b2, &s);
        }
    } else {
        sa = d_sign(&c_b2, a);
        sb = d_sign(&c_b2, b);
        if (sa == sb) {
            ret_val = (integer)sa;
        } else if (*ia > *ib) {
            if (log((abs(*a))) + *ia - *ib >= log((abs(*b)))) {
                ret_val = (integer)sa;
            } else {
                ret_val = (integer)sb;
            }
        } else {
            if (log((abs(*b))) + *ib - *ia >= log((abs(*a)))) {
                ret_val = (integer)sb;
            } else {
                ret_val = (integer)sa;
            }
        }
    }
    return ret_val;
    /* *** Last line of MA01CD *** */
} /* ma01cd_ */
