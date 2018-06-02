/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c__1 = 1;

EXPORTSYMBOL /* Subroutine */ int tb04bx_(
    ip, iz, a, lda, b, c__, d__, pr, pi, zr, zi, gain, iwork) integer *ip,
    *iz;
doublereal* a;
integer* lda;
doublereal *b, *c__, *d__, *pr, *pi, *zr, *zi, *gain;
integer* iwork;
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1;
    doublereal d__1, d__2;
    /* Local variables */
    extern doublereal ddot_();
    static integer info, i__;
    static doublereal s;
    extern /* Subroutine */ int mb02rd_(), mb02sd_();
    static doublereal s0;
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
    /*     To compute the gain of a single-input single-output linear system, */
    /*     given its state-space representation (A,b,c,d), and its poles and */
    /*     zeros. The matrix A is assumed to be in an upper Hessenberg form. */
    /*     The gain is computed using the formula */
    /*                          -1         IP              IZ */
    /*        g = (c*( S0*I - A ) *b + d)*Prod( S0 - Pi )/Prod( S0 - Zi ) , */
    /*                                     i=1             i=1            (1) */
    /*     where Pi, i = 1 : IP, and Zj, j = 1 : IZ, are the poles and zeros, */
    /*     respectively, and S0 is a real scalar different from all poles and */
    /*     zeros. */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     IP      (input) INTEGER */
    /*             The number of the system poles.  IP >= 0. */
    /*     IZ      (input) INTEGER */
    /*             The number of the system zeros.  IZ >= 0. */
    /*     A       (input/output) DOUBLE PRECISION array, dimension (LDA,IP) */
    /*             On entry, the leading IP-by-IP part of this array must */
    /*             contain the state dynamics matrix A in an upper Hessenberg */
    /*             form. The elements below the second diagonal are not */
    /*             referenced. */
    /*             On exit, the leading IP-by-IP upper Hessenberg part of */
    /*             this array contains the LU factorization of the matrix */
    /*             A - S0*I, as computed by SLICOT Library routine MB02SD. */
    /*     LDA     INTEGER */
    /*             The leading dimension of array A.  LDA >= max(1,IP). */
    /*     B       (input/output) DOUBLE PRECISION array, dimension (IP) */
    /*             On entry, this array must contain the system input */
    /*             vector b. */
    /*             On exit, this array contains the solution of the linear */
    /*             system ( A - S0*I )x = b . */
    /*     C       (input) DOUBLE PRECISION array, dimension (IP) */
    /*             This array must contain the system output vector c. */
    /*     D       (input) DOUBLE PRECISION */
    /*             The variable must contain the system feedthrough scalar d. */
    /*     PR      (input) DOUBLE PRECISION array, dimension (IP) */
    /*             This array must contain the real parts of the system */
    /*             poles. Pairs of complex conjugate poles must be stored in */
    /*             consecutive memory locations. */
    /*     PI      (input) DOUBLE PRECISION array, dimension (IP) */
    /*             This array must contain the imaginary parts of the system */
    /*             poles. */
    /*     ZR      (input) DOUBLE PRECISION array, dimension (IZ) */
    /*             This array must contain the real parts of the system */
    /*             zeros. Pairs of complex conjugate zeros must be stored in */
    /*             consecutive memory locations. */
    /*     ZI      (input) DOUBLE PRECISION array, dimension (IZ) */
    /*             This array must contain the imaginary parts of the system */
    /*             zeros. */
    /*     GAIN    (output) DOUBLE PRECISION */
    /*             The gain of the linear system (A,b,c,d), given by (1). */
    /*     Workspace */
    /*     IWORK   INTEGER array, dimension (IP) */
    /*             On exit, it contains the pivot indices; for 1 <= i <= IP, */
    /*             row i of the matrix A - S0*I was interchanged with */
    /*             row IWORK(i). */
    /*     METHOD */
    /*     The routine implements the method presented in [1]. A suitable */
    /*     value of S0 is chosen based on the system poles and zeros. */
    /*     Then, the LU factorization of the upper Hessenberg, nonsingular */
    /*     matrix A - S0*I is computed and used to solve the linear system */
    /*     in (1). */
    /*     REFERENCES */
    /*     [1] Varga, A. and Sima, V. */
    /*         Numerically Stable Algorithm for Transfer Function Matrix */
    /*         Evaluation. */
    /*         Int. J. Control, vol. 33, nr. 6, pp. 1123-1133, 1981. */
    /*     NUMERICAL ASPECTS */
    /*     The algorithm is numerically stable in practice and requires */
    /*     O(IP*IP) floating point operations. */
    /*     CONTRIBUTORS */
    /*     V. Sima, Research Institute for Informatics, Bucharest, May 2002. */
    /*     Partly based on the BIMASC Library routine GAIN by A. Varga. */
    /*     REVISIONS */
    /*     - */
    /*     KEYWORDS */
    /*     Eigenvalue, state-space representation, transfer function, zeros. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. External Functions .. */
    /*     .. External Subroutines .. */
    /*     .. Intrinsic Functions .. */
    /*     .. */
    /*     .. Executable Statements .. */
    /*     For efficiency, the input scalar parameters are not checked. */
    /*     Quick return if possible. */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --b;
    --c__;
    --pr;
    --pi;
    --zr;
    --zi;
    --iwork;
    /* Function Body */
    if (*ip == 0) {
        *gain = 0.;
        return 0;
    }
    /*     Compute a suitable value for S0 . */
    s0 = 0.;
    i__1 = *ip;
    for (i__ = 1; i__ <= i__1; ++i__) {
        s = (d__1 = pr[i__], abs(d__1));
        if (pi[i__] != 0.) {
            s += (d__1 = pi[i__], abs(d__1));
        }
        s0 = max(s0, s);
        /* L10: */
    }
    i__1 = *iz;
    for (i__ = 1; i__ <= i__1; ++i__) {
        s = (d__1 = zr[i__], abs(d__1));
        if (zi[i__] != 0.) {
            s += (d__1 = zi[i__], abs(d__1));
        }
        s0 = max(s0, s);
        /* L20: */
    }
    s0 = s0 * 2. + .1;
    if (s0 <= 1.) {
        s0 = 1.1;
    }
    /*     Form A - S0*I . */
    i__1 = *ip;
    for (i__ = 1; i__ <= i__1; ++i__) {
        a[i__ + i__ * a_dim1] -= s0;
        /* L30: */
    }
    /*     Compute the LU factorization of the matrix A - S0*I */
    /*     (guaranteed to be nonsingular). */
    mb02sd_(ip, &a[a_offset], lda, &iwork[1], &info);
    /*     Solve the linear system (A - S0*I)*x = b . */
    mb02rd_("No Transpose", ip, &c__1, &a[a_offset], lda, &iwork[1], &b[1], ip, &info, 12L);
    /*                        -1 */
    /*     Compute c*(S0*I - A) *b + d . */
    *gain = *d__ - ddot_(ip, &c__[1], &c__1, &b[1], &c__1);
    /*     Multiply by the products in terms of poles and zeros in (1). */
    i__ = 1;
    /*     WHILE ( I <= IP ) DO */
L40:
    if (i__ <= *ip) {
        if (pi[i__] == 0.) {
            *gain *= s0 - pr[i__];
            ++i__;
        } else {
            /* Computing 2nd power */
            d__1 = pr[i__];
            /* Computing 2nd power */
            d__2 = pi[i__];
            *gain *= s0 * (s0 - pr[i__] * 2.) + d__1 * d__1 + d__2 * d__2;
            i__ += 2;
        }
        goto L40;
    }
    /*     END WHILE 40 */
    i__ = 1;
    /*     WHILE ( I <= IZ ) DO */
L50:
    if (i__ <= *iz) {
        if (zi[i__] == 0.) {
            *gain /= s0 - zr[i__];
            ++i__;
        } else {
            /* Computing 2nd power */
            d__1 = zr[i__];
            /* Computing 2nd power */
            d__2 = zi[i__];
            *gain /= s0 * (s0 - zr[i__] * 2.) + d__1 * d__1 + d__2 * d__2;
            i__ += 2;
        }
        goto L50;
    }
    /*     END WHILE 50 */
    return 0;
    /* *** Last line of TB04BX *** */
} /* tb04bx_ */
