/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

EXPORTSYMBOL /* Subroutine */ int mb03qx_(n, t, ldt, wr, wi, info) integer* n;
doublereal* t;
integer* ldt;
doublereal *wr, *wi;
integer* info;
{
    /* System generated locals */
    integer t_dim1, t_offset, i__1;
    /* Local variables */
    static integer i__, inext, i1;
    extern /* Subroutine */ int dlanv2_();
    static doublereal a11, a12, a21, a22, cs, sn;
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
    /*     To compute the eigenvalues of an upper quasi-triangular matrix. */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the matrix T.  N >= 0. */
    /*     T       (input) DOUBLE PRECISION array, dimension(LDT,N) */
    /*             The upper quasi-triangular matrix T. */
    /*     LDT     INTEGER */
    /*             The leading dimension of the array T.  LDT >= max(1,N). */
    /*     WR, WI  (output) DOUBLE PRECISION arrays, dimension (N) */
    /*             The real and imaginary parts, respectively, of the */
    /*             eigenvalues of T. The eigenvalues are stored in the same */
    /*             order as on the diagonal of T. If T(i:i+1,i:i+1) is a */
    /*             2-by-2 diagonal block with complex conjugated eigenvalues */
    /*             then WI(i) > 0 and WI(i+1) = -WI(i). */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -i, the i-th argument had an illegal */
    /*                   value. */
    /*     CONTRIBUTOR */
    /*     A. Varga, German Aerospace Center, DLR Oberpfaffenhofen, */
    /*     March 1998. Based on the RASP routine SEIG. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. External Subroutines .. */
    /*     .. Intrinsic Functions .. */
    /*     .. Executable Statements .. */
    /* Parameter adjustments */
    t_dim1 = *ldt;
    t_offset = t_dim1 + 1;
    t -= t_offset;
    --wr;
    --wi;
    /* Function Body */
    *info = 0;
    /*     Test the input scalar arguments. */
    if (*n < 0) {
        *info = -1;
    } else if (*ldt < max(1, *n)) {
        *info = -3;
    }
    if (*info != 0) {
        /*        Error return. */
        i__1 = -(*info);
        xerbla_("MB03QX", &i__1, 6L);
        return 0;
    }
    inext = 1;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
        if (i__ < inext) {
            goto L10;
        }
        if (i__ != *n) {
            if (t[i__ + 1 + i__ * t_dim1] != 0.) {
                /*              A pair of eigenvalues. */
                inext = i__ + 2;
                i1 = i__ + 1;
                a11 = t[i__ + i__ * t_dim1];
                a12 = t[i__ + i1 * t_dim1];
                a21 = t[i1 + i__ * t_dim1];
                a22 = t[i1 + i1 * t_dim1];
                dlanv2_(&a11, &a12, &a21, &a22, &wr[i__], &wi[i__], &wr[i1], &wi[i1], &cs, &sn);
                goto L10;
            }
        }
        /*        Simple eigenvalue. */
        inext = i__ + 1;
        wr[i__] = t[i__ + i__ * t_dim1];
        wi[i__] = 0.;
    L10:;
    }
    return 0;
    /* *** Last line of MB03QX *** */
} /* mb03qx_ */
