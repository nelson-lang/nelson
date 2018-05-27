/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

logical sb02ow_(alphar, alphai, beta) doublereal *alphar, *alphai, *beta;
{
    /* System generated locals */
    logical ret_val;
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
    /*     To select the stable generalized eigenvalues for solving the */
    /*     continuous-time algebraic Riccati equation. */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     ALPHAR  (input) DOUBLE PRECISION */
    /*             The real part of the numerator of the current eigenvalue */
    /*             considered. */
    /*     ALPHAI  (input) DOUBLE PRECISION */
    /*             The imaginary part of the numerator of the current */
    /*             eigenvalue considered. */
    /*     BETA    (input) DOUBLE PRECISION */
    /*             The (real) denominator of the current eigenvalue */
    /*             considered. It is assumed that BETA <> 0 (regular case). */
    /*     METHOD */
    /*     The function value SB02OW is set to .TRUE. for a stable eigenvalue */
    /*     and to .FALSE., otherwise. */
    /*     REFERENCES */
    /*     None. */
    /*     NUMERICAL ASPECTS */
    /*     None. */
    /*     CONTRIBUTOR */
    /*     Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Sep. 1997. */
    /*     Supersedes Release 2.0 routine SB02CW by P. Van Dooren, Philips */
    /*     Research Laboratory, Brussels, Belgium. */
    /*     REVISIONS */
    /*     - */
    /*     KEYWORDS */
    /*     Algebraic Riccati equation, closed loop system, continuous-time */
    /*     system, optimal regulator, Schur form. */
    /*     ****************************************************************** */
    /*     .. Scalar Arguments .. */
    /*     .. Executable Statements .. */
    ret_val = *alphar < 0. && *beta > 0. || *alphar > 0. && *beta < 0.;
    return ret_val;
    /* *** Last line of SB02OW *** */
} /* sb02ow_ */
