/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

EXPORTSYMBOL /* Subroutine */ int mb03ba_(k, h__, s, smult, amap, qmap) integer *k, *h__, *s,
    *smult, *amap, *qmap;
{
    /* System generated locals */
    integer i__1;
    /* Local variables */
    static integer temp, i__;
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
    /*     To compute the suitable maps for Hessenberg index H and */
    /*     signature array S. Auxiliary routine for the periodic QZ */
    /*     algorithms. */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     K       (input)  INTEGER */
    /*             The number of factors.  K >= 1. */
    /*     H       (input)  INTEGER */
    /*             Index which corresponds to A_1. */
    /*     S       (input)  INTEGER array, dimension (K) */
    /*             The signature array. Each entry of S must be 1 or -1. */
    /*     SMULT   (output)  INTEGER */
    /*             Signature multiplier. Entries of S are virtually */
    /*             multiplied by SMULT. */
    /*     AMAP    (output)  INTEGER array, dimension (K) */
    /*             The map for accessing the factors, that is, */
    /*             if AMAP(I) = J, then the factor A_I is stored at the J-th */
    /*             position in A. */
    /*     QMAP    (output)  INTEGER array, dimension (K) */
    /*             The map for accessing the orthognal transformation */
    /*             matrices, that is, if QMAP(I) = J, then the matrix Q_I is */
    /*             stored at the J-th position in Q. */
    /*     CONTRIBUTOR */
    /*     D. Kressner, Technical Univ. Berlin, Germany, June 2001. */
    /*     REVISIONS */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Romania, */
    /*     July 2009, SLICOT Library version of the routine PLAIND. */
    /*     KEYWORDS */
    /*     Hessenberg matrix, QZ algorithm, periodic QZ algorithm. */
    /*     ****************************************************************** */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. Intrinsic Functions .. */
    /*     .. Executable Statements .. */
    /* Parameter adjustments */
    --qmap;
    --amap;
    --s;
    /* Function Body */
    if (s[*h__] == -1) {
        *smult = -1;
        i__1 = *h__;
        for (i__ = 1; i__ <= i__1; ++i__) {
            amap[i__] = *h__ - i__ + 1;
            /* L10: */
        }
        i__1 = *k;
        for (i__ = *h__ + 1; i__ <= i__1; ++i__) {
            amap[i__] = *h__ + 1 - i__ + *k;
            /* L20: */
        }
        temp = *h__ % *k + 1;
        for (i__ = temp; i__ >= 1; --i__) {
            qmap[temp - i__ + 1] = i__;
            /* L30: */
        }
        i__1 = temp + 1;
        for (i__ = *k; i__ >= i__1; --i__) {
            qmap[temp + *k - i__ + 1] = i__;
            /* L40: */
        }
    } else {
        *smult = 1;
        i__1 = *k;
        for (i__ = *h__; i__ <= i__1; ++i__) {
            amap[i__ - *h__ + 1] = i__;
            qmap[i__ - *h__ + 1] = i__;
            /* L50: */
        }
        i__1 = *h__ - 1;
        for (i__ = 1; i__ <= i__1; ++i__) {
            amap[*k - *h__ + i__ + 1] = i__;
            qmap[*k - *h__ + i__ + 1] = i__;
            /* L60: */
        }
    }
    return 0;
    /* *** Last line of MB03BA *** */
} /* mb03ba_ */
