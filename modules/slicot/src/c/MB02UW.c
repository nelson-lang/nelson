/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

EXPORTSYMBOL /* Subroutine */ int mb02uw_(
    ltrans, n, m, par, a, lda, b, ldb, scale, iwarn) logical* ltrans;
integer *n, *m;
doublereal *par, *a;
integer* lda;
doublereal* b;
integer* ldb;
doublereal* scale;
integer* iwarn;
{
    /* Initialized data */
    static logical zswap[4] = { FALSE_, FALSE_, TRUE_, TRUE_ };
    static logical rswap[4] = { FALSE_, TRUE_, FALSE_, TRUE_ };
    static integer ipivot[16] /* was [4][4] */ = { 1, 2, 3, 4, 2, 1, 4, 3, 3, 4, 1, 2, 4, 3, 2, 1 };
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1, i__2;
    doublereal d__1, d__2, d__3;
    static doublereal equiv_0[4];
    /* Local variables */
    static doublereal bbnd, cmax, temp, smin;
#define c__ (equiv_0)
    static integer i__, j, icmax;
    static doublereal bnorm, smini, b1, b2, xnorm, x1, x2, c21, c22, l21, cs, u11, u12, u22;
#define cv (equiv_0)
    extern doublereal dlange_();
    extern integer idamax_();
    static doublereal scalep, bignum, smlnum, eps, u11r;
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
    /*     To solve a system of the form  A X = s B  or  A' X = s B  with */
    /*     possible scaling ("s") and perturbation of A.  (A' means */
    /*     A-transpose.)  A is an N-by-N real matrix, and X and B are */
    /*     N-by-M matrices.  N may be 1 or 2.  The scalar "s" is a scaling */
    /*     factor (.LE. 1), computed by this subroutine, which is so chosen */
    /*     that X can be computed without overflow.  X is further scaled if */
    /*     necessary to assure that norm(A)*norm(X) is less than overflow. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     LTRANS  LOGICAL */
    /*             Specifies if A or A-transpose is to be used, as follows: */
    /*             =.TRUE. :  A-transpose will be used; */
    /*             =.FALSE.:  A will be used (not transposed). */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the matrix A.  It may (only) be 1 or 2. */
    /*     M       (input) INTEGER */
    /*             The number of right hand size vectors. */
    /*     PAR     (input) DOUBLE PRECISION array, dimension (3) */
    /*             Machine related parameters: */
    /*             PAR(1) =: PREC  (machine precision)*base, DLAMCH( 'P' ); */
    /*             PAR(2) =: SFMIN safe minimum,             DLAMCH( 'S' ); */
    /*             PAR(3) =: SMIN  The desired lower bound on the singular */
    /*                             values of A.  This should be a safe */
    /*                             distance away from underflow or overflow, */
    /*                             say, between (underflow/machine precision) */
    /*                             and (machine precision * overflow). */
    /*     A       (input) DOUBLE PRECISION array, dimension (LDA,N) */
    /*             The leading N-by-N part of this array must contain the */
    /*             matrix A. */
    /*     LDA     INTEGER */
    /*             The leading dimension of the array A.  LDA >= N. */
    /*     B       (input/output) DOUBLE PRECISION array, dimension (LDB,M) */
    /*             On entry, the leading N-by-M part of this array must */
    /*             contain the matrix B (right-hand side). */
    /*             On exit, the leading N-by-M part of this array contains */
    /*             the N-by-M matrix X (unknowns). */
    /*     LDB     INTEGER */
    /*             The leading dimension of the array B.  LDB >= N. */
    /*     SCALE   (output) DOUBLE PRECISION */
    /*             The scale factor that B must be multiplied by to insure */
    /*             that overflow does not occur when computing X.  Thus, */
    /*             A X  will be SCALE*B, not B (ignoring perturbations of A). */
    /*             SCALE will be at most 1. */
    /*     Warning Indicator */
    /*     IWARN   INTEGER */
    /*             = 0:  no warnings (A did not have to be perturbed); */
    /*             = 1:  A had to be perturbed to make its smallest (or only) */
    /*                   singular value greater than SMIN (see below). */
    /*     METHOD */
    /*     Gaussian elimination with complete pivoting is used. The matrix A */
    /*     is slightly perturbed if it is (close to being) singular. */
    /*     FURTHER COMMENTS */
    /*     If both singular values of A are less than SMIN, SMIN*identity */
    /*     will be used instead of A.  If only one singular value is less */
    /*     than SMIN, one element of A will be perturbed enough to make the */
    /*     smallest singular value roughly SMIN.  If both singular values */
    /*     are at least SMIN, A will not be perturbed.  In any case, the */
    /*     perturbation will be at most some small multiple of */
    /*     max( SMIN, EPS*norm(A) ), where EPS is the machine precision */
    /*     (see LAPACK Library routine DLAMCH).  The singular values are */
    /*     computed by infinity-norm approximations, and thus will only be */
    /*     correct to a factor of 2 or so. */
    /*     Note: all input quantities are assumed to be smaller than overflow */
    /*     by a reasonable factor.  (See BIGNUM.)  In the interests of speed, */
    /*     this routine does not check the inputs for errors. */
    /*     CONTRIBUTOR */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Aug. 2009. */
    /*     Based on the LAPACK Library routine DLALN2. */
    /*     REVISIONS */
    /*     V. Sima, Nov. 2010. */
    /*     KEYWORDS */
    /*     Linear system of equations, matrix operations, matrix algebra. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. Local Arrays .. */
    /*     ..External Functions .. */
    /*     .. Intrinsic Functions .. */
    /*     .. */
    /*     .. Equivalences .. */
    /*     .. */
    /*     .. Data statements .. */
    /* Parameter adjustments */
    --par;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    /* Function Body */
    /*     .. Executable Statements .. */
    /*     For efficiency, the input arguments are not tested. */
    *iwarn = 0;
    /*     Compute BIGNUM. */
    smin = par[3];
    eps = par[1];
    smlnum = par[2] * 2. / eps;
    bignum = 1. / smlnum;
    /*     Standard initializations. */
    *scale = 1.;
    if (*n == 1) {
        /*        1-by-1  (i.e., scalar) systems  C X = B. */
        cs = a[a_dim1 + 1];
        cmax = abs(cs);
        /* Computing MAX */
        d__1 = max(smin, smlnum), d__2 = eps * cmax;
        smini = max(d__1, d__2);
        /*        If | C | < SMINI, use C = SMINI. */
        if (cmax < smini) {
            cs = smini;
            cmax = smini;
            *iwarn = 1;
        }
        /*        Check scaling for  X = B / C. */
        bnorm = (d__1 = b[idamax_(m, &b[b_offset], ldb) * b_dim1 + 1], abs(d__1));
        if (cmax < 1. && bnorm > 1.) {
            if (bnorm > bignum * cmax) {
                *scale = 1. / bnorm;
            }
        }
        /*        Compute X. */
        i__1 = *m;
        for (i__ = 1; i__ <= i__1; ++i__) {
            b[i__ * b_dim1 + 1] = b[i__ * b_dim1 + 1] * *scale / cs;
            /* L10: */
        }
    } else {
        /*        2x2 systems. */
        /*        Compute C = A  (or  A'). */
        c__[0] = a[a_dim1 + 1];
        c__[3] = a[(a_dim1 << 1) + 2];
        if (*ltrans) {
            c__[2] = a[a_dim1 + 2];
            c__[1] = a[(a_dim1 << 1) + 1];
        } else {
            c__[1] = a[a_dim1 + 2];
            c__[2] = a[(a_dim1 << 1) + 1];
        }
        bnorm = dlange_("M", n, m, &b[b_offset], ldb, cv, 1L);
        /*        Find the largest element in C. */
        cmax = 0.;
        icmax = 0;
        for (j = 1; j <= 4; ++j) {
            if ((d__1 = cv[j - 1], abs(d__1)) > cmax) {
                cmax = (d__1 = cv[j - 1], abs(d__1));
                icmax = j;
            }
            /* L20: */
        }
        /* Computing MAX */
        d__1 = max(smin, smlnum), d__2 = eps * cmax;
        smini = max(d__1, d__2);
        /*        If norm(C) < SMINI, use SMINI*identity. */
        if (cmax < smini) {
            if (smini < 1. && bnorm > 1.) {
                if (bnorm > bignum * smini) {
                    *scale = 1. / bnorm;
                }
            }
            temp = *scale / smini;
            i__1 = *m;
            for (i__ = 1; i__ <= i__1; ++i__) {
                b[i__ * b_dim1 + 1] = temp * b[i__ * b_dim1 + 1];
                b[i__ * b_dim1 + 2] = temp * b[i__ * b_dim1 + 2];
                /* L30: */
            }
            *iwarn = 1;
            return 0;
        }
        /*        Gaussian elimination with complete pivoting. */
        u11 = cv[icmax - 1];
        c21 = cv[ipivot[(icmax << 2) - 3] - 1];
        u12 = cv[ipivot[(icmax << 2) - 2] - 1];
        c22 = cv[ipivot[(icmax << 2) - 1] - 1];
        u11r = 1. / u11;
        l21 = u11r * c21;
        u22 = c22 - u12 * l21;
        /*        If smaller pivot < SMINI, use SMINI. */
        if (abs(u22) < smini) {
            u22 = smini;
            *iwarn = 1;
        }
        scalep = 1.;
        i__1 = *m;
        for (i__ = 1; i__ <= i__1; ++i__) {
            if (rswap[icmax - 1]) {
                b1 = b[i__ * b_dim1 + 2];
                b2 = b[i__ * b_dim1 + 1];
            } else {
                b1 = b[i__ * b_dim1 + 1];
                b2 = b[i__ * b_dim1 + 2];
            }
            b2 -= l21 * b1;
            /* Computing MAX */
            d__2 = (d__1 = b1 * (u22 * u11r), abs(d__1)), d__3 = abs(b2);
            bbnd = max(d__2, d__3);
            if (bbnd > 1. && abs(u22) < 1.) {
                if (bbnd >= bignum * abs(u22)) {
                    *scale = 1. / bbnd;
                }
            }
            *scale = min(*scale, scalep);
            if (*scale < scalep) {
                scalep = *scale / scalep;
                i__2 = i__ - 1;
                for (j = 1; j <= i__2; ++j) {
                    b[j * b_dim1 + 1] *= scalep;
                    b[j * b_dim1 + 2] *= scalep;
                    /* L40: */
                }
            }
            x2 = b2 * *scale / u22;
            x1 = *scale * b1 * u11r - x2 * (u11r * u12);
            if (zswap[icmax - 1]) {
                b[i__ * b_dim1 + 1] = x2;
                b[i__ * b_dim1 + 2] = x1;
            } else {
                b[i__ * b_dim1 + 1] = x1;
                b[i__ * b_dim1 + 2] = x2;
            }
            /* Computing MAX */
            d__1 = abs(x1), d__2 = abs(x2);
            xnorm = max(d__1, d__2);
            /*           Further scaling if  norm(A) norm(X) > overflow. */
            if (xnorm > 1. && cmax > 1.) {
                if (xnorm > bignum / cmax) {
                    temp = cmax / bignum;
                    b[i__ * b_dim1 + 1] = temp * b[i__ * b_dim1 + 1];
                    b[i__ * b_dim1 + 2] = temp * b[i__ * b_dim1 + 2];
                    *scale = temp * *scale;
                }
            }
            scalep = *scale;
            /* L50: */
        }
    }
    return 0;
    /* *** Last line of MB02UW *** */
} /* mb02uw_ */

#undef cv
#undef c__
