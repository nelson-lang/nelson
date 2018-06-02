/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

EXPORTSYMBOL /* Subroutine */ int ma01ad_(xr, xi, yr, yi) doublereal *xr, *xi, *yr, *yi;
{
    /* Builtin functions */
    double sqrt();
    /* Local variables */
    static doublereal s;
    extern doublereal dlapy2_();
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
    /*     To compute the complex square root YR + i*YI of a complex number */
    /*     XR + i*XI  in real arithmetic.  The returned result is so that */
    /*     YR >= 0.0  and  SIGN(YI) = SIGN(XI). */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     XR      (input) DOUBLE PRECISION */
    /*     XI      (input) DOUBLE PRECISION */
    /*             These scalars define the real and imaginary part of the */
    /*             complex number of which the square root is sought. */
    /*     YR      (output) DOUBLE PRECISION */
    /*     YI      (output) DOUBLE PRECISION */
    /*             These scalars define the real and imaginary part of the */
    /*             complex square root. */
    /*     METHOD */
    /*     The complex square root YR + i*YI of the complex number XR + i*XI */
    /*     is computed in real arithmetic, taking care to avoid overflow. */
    /*     REFERENCES */
    /*     Adapted from EISPACK subroutine CSROOT. */
    /*     CONTRIBUTOR */
    /*     P. Benner, Universitaet Bremen, Germany, and */
    /*     R. Byers, University of Kansas, Lawrence, USA, */
    /*     Aug. 1998, routine DCROOT. */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Romania, */
    /*     Oct. 1998, SLICOT Library version. */
    /*     REVISIONS */
    /*     - */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. */
    /*     .. Scalar Arguments .. */
    /*     .. */
    /*     .. Local Scalars .. */
    /*     .. */
    /*     .. External Functions .. */
    /*     .. Intrinsic functions .. */
    /*     .. */
    /*     .. Executable Statements .. */
    s = sqrt((dlapy2_(xr, xi) + abs(*xr)) * .5);
    if (*xr >= 0.) {
        *yr = s;
    }
    if (*xi < 0.) {
        s = -s;
    }
    if (*xr <= 0.) {
        *yi = s;
        if (*xr < 0.) {
            *yr = *xi / s * .5;
        }
    } else {
        *yi = *xi / *yr * .5;
    }
    return 0;
    /*     *** Last line of MA01AD *** */
} /* ma01ad_ */
