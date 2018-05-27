/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

EXPORTSYMBOL /* Subroutine */ int mb04tu_(n, x, incx, y, incy, c__, s) integer* n;
doublereal* x;
integer* incx;
doublereal* y;
integer* incy;
doublereal *c__, *s;
{
    /* System generated locals */
    integer i__1;
    /* Local variables */
    static integer i__;
    static doublereal dtemp;
    static integer ix, iy;
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
    /*     To perform the Givens transformation, defined by C (cos) and S */
    /*     (sin), and interchange the vectors involved, i.e. */
    /*        |X(i)|    | 0   1 |   | C   S |   |X(i)| */
    /*        |    | := |       | x |       | x |    |, i = 1,...N. */
    /*        |Y(i)|    | 1   0 |   |-S   C |   |Y(i)| */
    /*     REMARK. This routine is a modification of DROT from BLAS. */
    /*             This routine is called only by the SLICOT routines MB04TX */
    /*             and MB04VX. */
    /*     NUMERICAL ASPECTS */
    /*     The algorithm is backward stable. */
    /*     CONTRIBUTOR */
    /*     Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Apr. 1997. */
    /*     Supersedes Release 2.0 routine MB04FU by Th.G.J. Beelen, */
    /*     Philips Glass Eindhoven, Holland. */
    /*     REVISIONS */
    /*     January 26, 1998. */
    /*     KEYWORDS */
    /*     Othogonal transformation. */
    /*     ****************************************************************** */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. Executable Statements .. */
    /* Parameter adjustments */
    --y;
    --x;
    /* Function Body */
    if (*n <= 0) {
        return 0;
    }
    if (*incx != 1 || *incy != 1) {
        /*        Code for unequal increments or equal increments not equal to 1. */
        ix = 1;
        iy = 1;
        if (*incx < 0) {
            ix = (-(*n) + 1) * *incx + 1;
        }
        if (*incy < 0) {
            iy = (-(*n) + 1) * *incy + 1;
        }
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {
            dtemp = *c__ * y[iy] - *s * x[ix];
            y[iy] = *c__ * x[ix] + *s * y[iy];
            x[ix] = dtemp;
            ix += *incx;
            iy += *incy;
            /* L20: */
        }
    } else {
        /*        Code for both increments equal to 1. */
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {
            dtemp = *c__ * y[i__] - *s * x[i__];
            y[i__] = *c__ * x[i__] + *s * y[i__];
            x[i__] = dtemp;
            /* L40: */
        }
    }
    return 0;
    /* *** Last line of MB04TU *** */
} /* mb04tu_ */
