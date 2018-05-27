/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c__1 = 1;

EXPORTSYMBOL /* Subroutine */ int mb03ke_(trana, tranb, isgn, k, m, n, prec, smin, s, a, b, c__,
    scale, dwork, ldwork, info) logical *trana,
    *tranb;
integer *isgn, *k, *m, *n;
doublereal *prec, *smin;
integer* s;
doublereal *a, *b, *c__, *scale, *dwork;
integer *ldwork, *info;
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;
    /* Local variables */
    static doublereal beta, elem, dmin__, temp, spiv;
    static integer i__, j, l;
    extern /* Subroutine */ int dscal_(), daxpy_();
    static doublereal ac;
    static integer cb;
    static doublereal ad;
    static integer ii, mm, mn, nn, zc, zd;
    extern /* Subroutine */ int dlarfg_();
    static integer iz, zi;
    static logical doscal;
    static doublereal scaloc;
    extern integer idamax_();
    extern /* Subroutine */ int xerbla_(), dlarfx_();
    static doublereal bignum;
    static integer ia1, ib1, ia3, ib3, ic1, im1, minwrk, km2, km3;
    static logical lquery;
    static integer mn6, mn7, zi2, len, ixa, ixb, ixc, kmn, ldw;
    static doublereal sgn, tau;
    static integer zis;
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
    /*     To solve small periodic Sylvester-like equations (PSLE) */
    /*      op(A(i))*X( i ) + isgn*X(i+1)*op(B(i)) = -scale*C(i), S(i) =  1, */
    /*      op(A(i))*X(i+1) + isgn*X( i )*op(B(i)) = -scale*C(i), S(i) = -1. */
    /*     i = 1, ..., K, where op(A) means A or A**T, for the K-periodic */
    /*     matrix sequence X(i) = X(i+K), where A, B and C are K-periodic */
    /*     matrix sequences and A and B are in periodic real Schur form. The */
    /*     matrices A(i) are M-by-M and B(i) are N-by-N, with 1 <= M, N <= 2. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     TRANA   LOGICAL */
    /*             Specifies the form of op(A) to be used, as follows: */
    /*             = .FALSE.:  op(A) = A, */
    /*             = .TRUE. :  op(A) = A**T. */
    /*     TRANB   LOGICAL */
    /*             Specifies the form of op(B) to be used, as follows: */
    /*             = .FALSE.:  op(B) = B, */
    /*             = .TRUE. :  op(B) = B**T. */
    /*     ISGN    INTEGER */
    /*             Specifies which sign variant of the equations to solve. */
    /*             ISGN = 1 or ISGN = -1. */
    /*     Input/Output Parameters */
    /*     K       (input) INTEGER */
    /*             The period of the periodic matrix sequences A, B, C and X. */
    /*             K >= 2. (For K = 1, a standard Sylvester equation is */
    /*             obtained.) */
    /*     M       (input) INTEGER */
    /*             The order of the matrices A(i) and the number of rows of */
    /*             the matrices C(i) and X(i), i = 1, ..., K.  1 <= M <= 2. */
    /*     N       (input) INTEGER */
    /*             The order of the matrices B(i) and the number of columns */
    /*             of the matrices C(i) and X(i), i = 1, ..., K. */
    /*             1 <= N <= 2. */
    /*     PREC    (input) DOUBLE PRECISION */
    /*             The relative machine precision. See the LAPACK Library */
    /*             routine DLAMCH. */
    /*     SMIN    (input) DOUBLE PRECISION */
    /*             The machine safe minimum divided by PREC. */
    /*     S       (input) INTEGER array, dimension (K) */
    /*             The leading K elements of this array must contain the */
    /*             signatures (exponents) of the factors in the K-periodic */
    /*             matrix sequences for A and B. Each entry in S must be */
    /*             either 1 or -1. Notice that it is assumed that the same */
    /*             exponents are tied to both A and B on reduction to the */
    /*             periodic Schur form. */
    /*     A       (input) DOUBLE PRECISION array, dimension (M*M*K) */
    /*             On entry, this array must contain the M-by-M matrices */
    /*             A(i), for i = 1, ..., K, stored with the leading dimension */
    /*             M. Matrix A(i) is stored starting at position M*M*(i-1)+1. */
    /*     B       (input) DOUBLE PRECISION array, dimension (N*N*K) */
    /*             On entry, this array must contain the N-by-N matrices */
    /*             B(i), for i = 1, ..., K, stored with the leading dimension */
    /*             N. Matrix B(i) is stored starting at position N*N*(i-1)+1. */
    /*     C       (input/output) DOUBLE PRECISION array, dimension (M*N*K) */
    /*             On entry, this array must contain the M-by-N matrices */
    /*             C(i), for i = 1, ..., K, stored with the leading dimension */
    /*             M. Matrix C(i) is stored starting at position M*N*(i-1)+1. */
    /*             On exit, the matrices C(i) are overwritten by the solution */
    /*             sequence X(i). */
    /*     SCALE   (output) DOUBLE PRECISION */
    /*             The scale factor, scale, set less than or equal to 1 to */
    /*             avoid overflow in X. */
    /*     Workspace */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             On exit, if INFO = 0, DWORK(1) returns the optimal LDWORK. */
    /*             On exit, if INFO = -21, DWORK(1) returns the minimum value */
    /*             of LDWORK. */
    /*     LDWORK  INTEGER */
    /*             The dimension of the array DWORK. */
    /*             LDWORK >= (4*K-3) * (M*N)**2 + K * M*N. */
    /*             If LDWORK = -1  a workspace query is assumed; the */
    /*             routine only calculates the optimal size of the DWORK */
    /*             array, returns this value as the first entry of the DWORK */
    /*             array, and no error message is issued by XERBLA. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -21, then LDWORK is too small; appropriate */
    /*                   value for LDWORK is returned in DWORK(1); the other */
    /*                   arguments are not tested, for efficiency; */
    /*             = 1:  the solution would overflow with scale = 1, so */
    /*                   SCALE was set less than 1. This is a warning, not */
    /*                   an error. */
    /*     METHOD */
    /*     A version of the algorithm described in [1] is used. The routine */
    /*     uses a sparse Kronecker product representation Z of the PSLE and */
    /*     solves for X(i) from an associated linear system Z*x = c using */
    /*     structured (overlapping) variants of QR factorization and backward */
    /*     substitution. */
    /*     REFERENCES */
    /*     [1] Granat, R., Kagstrom, B. and Kressner, D. */
    /*         Computing periodic deflating subspaces associated with a */
    /*         specified set of eigenvalues. */
    /*         BIT Numerical Mathematics, vol. 47, 763-791, 2007. */
    /*     NUMERICAL ASPECTS */
    /*     The implemented method is numerically backward stable. */
    /*     CONTRIBUTOR */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Romania, */
    /*     Mar. 2010, an essentially new version of the PEP routine */
    /*     PEP_DGESY2, by R. Granat, Umea University, Sweden, Apr. 2008. */
    /*     REVISIONS */
    /*     V. Sima, Apr. 2010, Oct. 2010. */
    /*     KEYWORDS */
    /*     Orthogonal transformation, periodic QZ algorithm, periodic */
    /*     Sylvester-like equations, QZ algorithm. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. */
    /*     .. Scalar Arguments .. */
    /*     .. */
    /*     .. Array Arguments .. */
    /*     .. */
    /*     .. Local Scalars .. */
    /*     .. */
    /*     .. External Functions .. */
    /*     .. */
    /*     .. External Subroutines .. */
    /*     .. */
    /*     .. Intrinsic Functions .. */
    /*     .. */
    /*     .. Executable Statements .. */
    /*     Decode the input parameters. */
    /*     For efficiency reasons, the parameters are not checked. */
    /* Parameter adjustments */
    --dwork;
    --c__;
    --b;
    --a;
    --s;
    /* Function Body */
    *info = 0;
    lquery = *ldwork == -1;
    mn = *m * *n;
    kmn = *k * mn;
    /* Computing 2nd power */
    i__1 = mn;
    minwrk = ((*k << 2) - 3) * (i__1 * i__1) + kmn;
    if (!lquery && *ldwork < minwrk) {
        *info = -21;
    }
    /*     Quick return if possible. */
    dwork[1] = (doublereal)minwrk;
    if (lquery) {
        return 0;
    } else if (*info != 0) {
        i__1 = -(*info);
        xerbla_("MB03KE", &i__1, 6L);
        return 0;
    }
    /*     Find the overflow threshold. */
    bignum = *prec / *smin;
    /*     --- Use QR-factorizations and backward substitution --- */
    /*     This variant does not utilize the sparsity structure of the */
    /*     individual blocks of the matrix Z - storage of each block Z_i,i */
    /*     is compatible with the BLAS. Numerics is stable since excessive */
    /*     pivot growth is avoided. */
    mm = *m * *m;
    nn = *n * *n;
    sgn = (doublereal)(*isgn);
    ldw = mn * 3;
    if (*m == 2 && *n == 2) {
        mn6 = ldw + ldw;
        mn7 = mn6 + ldw;
        km2 = kmn + kmn;
        km3 = km2 + kmn;
    }
    /*     Divide workspace for superdiagonal + diagonal + subdiagonal blocks */
    /*     and right-most block column stored in a "block-packed" format. For */
    /*     simplicity, an additional block Z_{0,1} appears in the first block */
    /*     column in Z. */
    zd = 1;
    zc = zd + ldw * mn * (*k - 1);
    /*     Also give workspace for right hand side in CB. */
    cb = zc + mn * kmn;
    /*     Fill the Z part of the workspace with zeros. */
    i__1 = cb - 1;
    for (j = 1; j <= i__1; ++j) {
        dwork[j] = 0.;
        /* L10: */
    }
    /*     Build matrix Z in ZD and ZC. */
    ixa = 1;
    ixb = 1;
    ixc = 1;
    im1 = *k;
    zi = zd + mn;
    i__1 = *k - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
        /*        Build Z_{i,i}, i = 1,...,K-1. */
        if (s[im1] == -1) {
            ia1 = (im1 - 1) * mm + 1;
            dwork[zi] = a[ia1];
            if (*m == 2) {
                ia3 = ia1 + 2;
                if (!(*trana)) {
                    dwork[zi + 1] = a[ia1 + 1];
                    dwork[zi + ldw] = a[ia3];
                } else {
                    dwork[zi + 1] = a[ia3];
                    dwork[zi + ldw] = a[ia1 + 1];
                }
                dwork[zi + ldw + 1] = a[ia3 + 1];
            }
            if (*n == 2) {
                zi2 = zi + (ldw + 1) * *m;
                dwork[zi2] = dwork[zi];
                if (*m == 2) {
                    dwork[zi2 + 1] = dwork[zi + 1];
                    dwork[zi2 + ldw] = dwork[zi + ldw];
                    dwork[zi2 + ldw + 1] = dwork[zi + ldw + 1];
                }
            }
        } else {
            ib1 = (im1 - 1) * nn + 1;
            dwork[zi] = sgn * b[ib1];
            if (!(*tranb)) {
                if (*m == 2) {
                    dwork[zi + ldw + 1] = dwork[zi];
                    if (*n == 2) {
                        ib3 = ib1 + 2;
                        dwork[zi + 2] = sgn * b[ib3];
                        dwork[zi + ldw + 3] = dwork[zi + 2];
                        dwork[zi + mn6] = sgn * b[ib1 + 1];
                        dwork[zi + mn6 + 2] = sgn * b[ib3 + 1];
                        dwork[zi + mn7 + 1] = dwork[zi + mn6];
                        dwork[zi + mn7 + 3] = dwork[zi + mn6 + 2];
                    }
                } else if (*n == 2) {
                    ib3 = ib1 + 2;
                    dwork[zi + 1] = sgn * b[ib3];
                    dwork[zi + ldw] = sgn * b[ib1 + 1];
                    dwork[zi + ldw + 1] = sgn * b[ib3 + 1];
                }
            } else {
                if (*m == 2) {
                    dwork[zi + ldw + 1] = dwork[zi];
                    if (*n == 2) {
                        ib3 = ib1 + 2;
                        dwork[zi + 2] = sgn * b[ib1 + 1];
                        dwork[zi + ldw + 3] = dwork[zi + 2];
                        dwork[zi + mn6] = sgn * b[ib3];
                        dwork[zi + mn6 + 2] = sgn * b[ib3 + 1];
                        dwork[zi + mn7 + 1] = dwork[zi + mn6];
                        dwork[zi + mn7 + 3] = dwork[zi + mn6 + 2];
                    }
                } else if (*n == 2) {
                    ib3 = ib1 + 2;
                    dwork[zi + 1] = sgn * b[ib1 + 1];
                    dwork[zi + ldw] = sgn * b[ib3];
                    dwork[zi + ldw + 1] = sgn * b[ib3 + 1];
                }
            }
        }
        /*        Build Z_{i+1,i}, i = 1,...,K-1. */
        zi += mn;
        if (s[i__] == 1) {
            ia1 = ixa;
            dwork[zi] = a[ia1];
            if (*m == 2) {
                ia3 = ia1 + 2;
                if (!(*trana)) {
                    dwork[zi + 1] = a[ia1 + 1];
                    dwork[zi + ldw] = a[ia3];
                } else {
                    dwork[zi + 1] = a[ia3];
                    dwork[zi + ldw] = a[ia1 + 1];
                }
                dwork[zi + ldw + 1] = a[ia3 + 1];
            }
            if (*n == 2) {
                zi2 = zi + (ldw + 1) * *m;
                dwork[zi2] = dwork[zi];
                if (*m == 2) {
                    dwork[zi2 + 1] = dwork[zi + 1];
                    dwork[zi2 + ldw] = dwork[zi + ldw];
                    dwork[zi2 + ldw + 1] = dwork[zi + ldw + 1];
                }
            }
        } else {
            ib1 = ixb;
            dwork[zi] = sgn * b[ib1];
            if (!(*tranb)) {
                if (*m == 2) {
                    dwork[zi + ldw + 1] = dwork[zi];
                    if (*n == 2) {
                        ib3 = ib1 + 2;
                        dwork[zi + 2] = sgn * b[ib3];
                        dwork[zi + ldw + 3] = dwork[zi + 2];
                        dwork[zi + mn6] = sgn * b[ib1 + 1];
                        dwork[zi + mn6 + 2] = sgn * b[ib3 + 1];
                        dwork[zi + mn7 + 1] = dwork[zi + mn6];
                        dwork[zi + mn7 + 3] = dwork[zi + mn6 + 2];
                    }
                } else if (*n == 2) {
                    ib3 = ib1 + 2;
                    dwork[zi + 1] = sgn * b[ib3];
                    dwork[zi + ldw] = sgn * b[ib1 + 1];
                    dwork[zi + ldw + 1] = sgn * b[ib3 + 1];
                }
            } else {
                if (*m == 2) {
                    dwork[zi + ldw + 1] = dwork[zi];
                    if (*n == 2) {
                        ib3 = ib1 + 2;
                        dwork[zi + 2] = sgn * b[ib1 + 1];
                        dwork[zi + ldw + 3] = dwork[zi + 2];
                        dwork[zi + mn6] = sgn * b[ib3];
                        dwork[zi + mn6 + 2] = sgn * b[ib3 + 1];
                        dwork[zi + mn7 + 1] = dwork[zi + mn6];
                        dwork[zi + mn7 + 3] = dwork[zi + mn6 + 2];
                    }
                } else if (*n == 2) {
                    ib3 = ib1 + 2;
                    dwork[zi + 1] = sgn * b[ib1 + 1];
                    dwork[zi + ldw] = sgn * b[ib3];
                    dwork[zi + ldw + 1] = sgn * b[ib3 + 1];
                }
            }
        }
        ixa += mm;
        ixb += nn;
        im1 = i__;
        zi += mn * (ldw - 1);
        /* L20: */
    }
    /*     Build Z_{K,K}. */
    ixa -= mm;
    ixb -= nn;
    zi = zc + kmn - mn;
    if (s[*k - 1] == -1) {
        ia1 = ixa;
        dwork[zi] = a[ia1];
        if (*m == 2) {
            ia3 = ia1 + 2;
            if (!(*trana)) {
                dwork[zi + 1] = a[ia1 + 1];
                dwork[zi + kmn] = a[ia3];
            } else {
                dwork[zi + 1] = a[ia3];
                dwork[zi + kmn] = a[ia1 + 1];
            }
            dwork[zi + kmn + 1] = a[ia3 + 1];
        }
        if (*n == 2) {
            zi2 = zi + (kmn + 1) * *m;
            dwork[zi2] = dwork[zi];
            if (*m == 2) {
                dwork[zi2 + 1] = dwork[zi + 1];
                dwork[zi2 + kmn] = dwork[zi + kmn];
                dwork[zi2 + kmn + 1] = dwork[zi + kmn + 1];
            }
        }
    } else {
        ib1 = ixb;
        dwork[zi] = sgn * b[ib1];
        if (!(*tranb)) {
            if (*m == 2) {
                dwork[zi + kmn + 1] = dwork[zi];
                if (*n == 2) {
                    ib3 = ib1 + 2;
                    dwork[zi + 2] = sgn * b[ib3];
                    dwork[zi + kmn + 3] = dwork[zi + 2];
                    dwork[zi + km2] = sgn * b[ib1 + 1];
                    dwork[zi + km2 + 2] = sgn * b[ib3 + 1];
                    dwork[zi + km3 + 1] = dwork[zi + km2];
                    dwork[zi + km3 + 3] = dwork[zi + km2 + 2];
                }
            } else if (*n == 2) {
                ib3 = ib1 + 2;
                dwork[zi + 1] = sgn * b[ib3];
                dwork[zi + kmn] = sgn * b[ib1 + 1];
                dwork[zi + kmn + 1] = sgn * b[ib3 + 1];
            }
        } else {
            if (*m == 2) {
                dwork[zi + kmn + 1] = dwork[zi];
                if (*n == 2) {
                    ib3 = ib1 + 2;
                    dwork[zi + 2] = sgn * b[ib1 + 1];
                    dwork[zi + kmn + 3] = dwork[zi + 2];
                    dwork[zi + km2] = sgn * b[ib3];
                    dwork[zi + km2 + 2] = sgn * b[ib3 + 1];
                    dwork[zi + km3 + 1] = dwork[zi + km2];
                    dwork[zi + km3 + 3] = dwork[zi + km2 + 2];
                }
            } else if (*n == 2) {
                ib3 = ib1 + 2;
                dwork[zi + 1] = sgn * b[ib1 + 1];
                dwork[zi + kmn] = sgn * b[ib3];
                dwork[zi + kmn + 1] = sgn * b[ib3 + 1];
            }
        }
    }
    /*     Build Z_{1,K}. */
    if (s[*k] == 1) {
        ia1 += mm;
        dwork[zc] = a[ia1];
        if (*m == 2) {
            ia3 = ia1 + 2;
            if (!(*trana)) {
                dwork[zc + 1] = a[ia1 + 1];
                dwork[zc + kmn] = a[ia3];
            } else {
                dwork[zc + 1] = a[ia3];
                dwork[zc + kmn] = a[ia1 + 1];
            }
            dwork[zc + kmn + 1] = a[ia3 + 1];
        }
        if (*n == 2) {
            zi2 = zc + (kmn + 1) * *m;
            dwork[zi2] = dwork[zc];
            if (*m == 2) {
                dwork[zi2 + 1] = dwork[zc + 1];
                dwork[zi2 + kmn] = dwork[zc + kmn];
                dwork[zi2 + kmn + 1] = dwork[zc + kmn + 1];
            }
        }
    } else {
        ib1 += nn;
        dwork[zc] = sgn * b[ib1];
        if (!(*tranb)) {
            if (*m == 2) {
                dwork[zc + kmn + 1] = dwork[zc];
                if (*n == 2) {
                    ib3 = ib1 + 2;
                    dwork[zc + 2] = sgn * b[ib3];
                    dwork[zc + kmn + 3] = dwork[zc + 2];
                    dwork[zc + km2] = sgn * b[ib1 + 1];
                    dwork[zc + km2 + 2] = sgn * b[ib3 + 1];
                    dwork[zc + km3 + 1] = dwork[zc + km2];
                    dwork[zc + km3 + 3] = dwork[zc + km2 + 2];
                }
            } else if (*n == 2) {
                ib3 = ib1 + 2;
                dwork[zc + 1] = sgn * b[ib3];
                dwork[zc + kmn] = sgn * b[ib1 + 1];
                dwork[zc + kmn + 1] = sgn * b[ib3 + 1];
            }
        } else {
            if (*m == 2) {
                dwork[zc + kmn + 1] = dwork[zc];
                if (*n == 2) {
                    ib3 = ib1 + 2;
                    dwork[zc + 2] = sgn * b[ib1 + 1];
                    dwork[zc + kmn + 3] = dwork[zc + 2];
                    dwork[zc + km2] = sgn * b[ib3];
                    dwork[zc + km2 + 2] = sgn * b[ib3 + 1];
                    dwork[zc + km3 + 1] = dwork[zc + km2];
                    dwork[zc + km3 + 3] = dwork[zc + km2 + 2];
                }
            } else if (*n == 2) {
                ib3 = ib1 + 2;
                dwork[zc + 1] = sgn * b[ib1 + 1];
                dwork[zc + kmn] = sgn * b[ib3];
                dwork[zc + kmn + 1] = sgn * b[ib3 + 1];
            }
        }
    }
    /*     Prepare right hand side in CB. */
    zi = cb + mn;
    i__1 = *k - 1;
    for (l = 1; l <= i__1; ++l) {
        ic1 = ixc;
        dwork[zi] = -c__[ic1];
        if (*m == 1) {
            if (*n == 2) {
                dwork[zi + 1] = -c__[ic1 + 1];
            }
        } else {
            dwork[zi + 1] = -c__[ic1 + 1];
            if (*n == 2) {
                dwork[zi + 2] = -c__[ic1 + 2];
                dwork[zi + 3] = -c__[ic1 + 3];
            }
        }
        ixc += mn;
        zi += mn;
        /* L30: */
    }
    zi = cb;
    ic1 = ixc;
    dwork[zi] = -c__[ic1];
    if (*m == 1) {
        if (*n == 2) {
            dwork[zi + 1] = -c__[ic1 + 1];
        }
    } else {
        dwork[zi + 1] = -c__[ic1 + 1];
        if (*n == 2) {
            dwork[zi + 2] = -c__[ic1 + 2];
            dwork[zi + 3] = -c__[ic1 + 3];
        }
    }
    /*     Solve the Kronecker product system for X_i, i = 1,...,K */
    /*     using overlapping (structured) QR-factorization and */
    /*     backward substitution. */
    /*     Step 1: Reduce the system to triangular form via overlapping */
    /*             QR-factorizations. */
    /*             The method here is based on successively formed */
    /*             Householder reflections which are applied one by one */
    /*             to the matrix Z and the right hand side c. The size */
    /*             of each reflection is chosen as the number of elements */
    /*             in each column from the last non-zero element up to */
    /*             the diagonal. */
    /*             Notation: */
    /*             L   = current position of the column to work with; */
    /*             I   = corresponding block column in Z; */
    /*             II  = corresponding row and column position in Z-block; */
    /*             LEN = length of the current Householder reflection. */
    i__ = 1;
    ii = 0;
    zis = zd + mn;
    zi2 = zd + mn * ldw;
    /*     Treat Z_{K,K} separately from [Z_{i,i}',Z_{i+1,i}']' (see below). */
    /*     DMIN is the minimum modulus of the final diagonal values. */
    dmin__ = bignum;
    i__1 = kmn - mn;
    for (l = 1; l <= i__1; ++l) {
        ++ii;
        zi = zis + (mn << 1);
        len = (mn << 1) - ii + 1;
        /*        REPEAT */
    L40:
        --zi;
        elem = dwork[zi];
        if (elem == 0.) {
            --len;
            goto L40;
        }
        /*        UNTIL ELEM.NE.ZERO. */
        if (len > 1) {
            /*           Generate Householder reflection to zero out the current */
            /*           column. The new main diagonal value is stored temporarily */
            /*           in BETA. */
            zi = zi - len + 1;
            dlarfg_(&len, &dwork[zi], &dwork[zi + 1], &c__1, &tau);
            beta = dwork[zi];
            dwork[zi] = 1.;
            /*           Apply reflection to Z and c: first to the rest of the */
            /*           corresponding rows and columns of [Z_{i,i}',Z_{i+1,i}']' */
            /*           of size LEN-by-(MN-II) ... */
            i__2 = mn - ii;
            dlarfx_("Left", &len, &i__2, &dwork[zi], &tau, &dwork[zi + ldw], &ldw, &dwork[1], 4L);
            /*           ... then to the corresponding part of */
            /*           [Z_{i,i+1}',Z_{i+1,i+1}']' of size LEN-by-MN ... */
            if (i__ < *k - 1) {
                dlarfx_("Left", &len, &mn, &dwork[zi], &tau, &dwork[zi2], &ldw, &dwork[1], 4L);
            }
            /*           ... next to the corresponding part of */
            /*           [Z_{i,K}',Z_{i+1,K}']' of size LEN-by-MN ... */
            dlarfx_("Left", &len, &mn, &dwork[zi], &tau, &dwork[zc + l - 1], &kmn, &dwork[1], 4L);
            /*           ... and finally to c(L:L+LEN-1). */
            dlarfx_("Left", &len, &c__1, &dwork[zi], &tau, &dwork[cb + l - 1], &kmn, &dwork[1], 4L);
            /*           Store the new diagonal value. */
            dwork[zi] = beta;
            /* Computing MIN */
            d__1 = dmin__, d__2 = abs(beta);
            dmin__ = min(d__1, d__2);
        }
        zis += ldw;
        ++zi2;
        if (l % mn == 0) {
            ++i__;
            ii = 0;
            zi2 = zd + i__ * mn * ldw;
        }
        /* L50: */
    }
    ii = 0;
    zi = zc + kmn - mn;
    /*     Z_{K,K} is treated separately. */
    i__1 = kmn;
    for (l = kmn - mn + 1; l <= i__1; ++l) {
        ++ii;
        len = mn - ii + 1;
        if (len > 1) {
            /*           Generate Householder reflection. */
            dlarfg_(&len, &dwork[zi], &dwork[zi + 1], &c__1, &tau);
            beta = dwork[zi];
            dwork[zi] = 1.;
            /*           Apply reflection to Z and c: first to Z_{i,i} ... */
            i__2 = mn - ii;
            dlarfx_("Left", &len, &i__2, &dwork[zi], &tau, &dwork[zi + kmn], &kmn, &dwork[1], 4L);
            /*           ... and finally to c(L:L+LEN-1). */
            dlarfx_("Left", &len, &c__1, &dwork[zi], &tau, &dwork[cb + l - 1], &kmn, &dwork[1], 4L);
            /*           Store the new diagonal value. */
            dwork[zi] = beta;
            /* Computing MIN */
            d__1 = dmin__, d__2 = abs(beta);
            dmin__ = min(d__1, d__2);
        }
        zi = zi + kmn + 1;
        /* L60: */
    }
    /*     Step 2: Use backward substitution on the computed triangular */
    /*             system. */
    /*             Here, we take the possible irregularities above the */
    /*             diagonal of the resulting R-factor into account by */
    /*             checking the number of elements from the main diagonal */
    /*             to the last non-zero element above the diagonal that */
    /*             resides in the current column. */
    /*             Pivots less than SPIV = MAX( PREC*DMIN, SMIN ) are set */
    /*             to SPIV. */
    *scale = 1.;
    doscal = FALSE_;
    dmin__ = max(dmin__, *smin);
    /* Computing MAX */
    d__1 = *prec * dmin__;
    spiv = max(d__1, *smin);
    /*     Check for scaling. */
    i__ = idamax_(&kmn, &dwork[cb], &c__1);
    ac = (d__1 = dwork[cb + i__ - 1], abs(d__1));
    if (*smin * 2. * ac > dmin__) {
        temp = .5 / ac;
        dscal_(&kmn, &temp, &dwork[cb], &c__1);
        *scale *= temp;
    }
    zi = cb - 1;
    i__1 = kmn - mn + 1;
    for (i__ = kmn; i__ >= i__1; --i__) {
        ad = (d__1 = dwork[zi], abs(d__1));
        ac = (d__1 = dwork[cb + i__ - 1], abs(d__1));
        if (ad < spiv) {
            ad = spiv;
            dwork[zi] = spiv;
        }
        scaloc = 1.;
        if (ad < 1. && ac > 1.) {
            if (ac > bignum * ad) {
                *info = 1;
                scaloc = bignum * ad / ac;
                doscal = TRUE_;
                *scale *= scaloc;
            }
        }
        temp = dwork[cb + i__ - 1] * scaloc / dwork[zi];
        if (doscal) {
            doscal = FALSE_;
            dscal_(&kmn, &scaloc, &dwork[cb], &c__1);
        }
        dwork[cb + i__ - 1] = temp;
        i__2 = i__ - 1;
        d__1 = -temp;
        daxpy_(&i__2, &d__1, &dwork[zi - i__ + 1], &c__1, &dwork[cb], &c__1);
        zi = zi - kmn - 1;
        /* L70: */
    }
    zis = zc - ldw;
    zi = zis + (mn << 1) - 1;
    iz = 0;
    for (i__ = kmn - mn; i__ >= 1; --i__) {
        ad = (d__1 = dwork[zi], abs(d__1));
        ac = (d__1 = dwork[cb + i__ - 1], abs(d__1));
        if (ad < spiv) {
            ad = spiv;
            dwork[zi] = spiv;
        }
        scaloc = 1.;
        if (ad < 1. && ac > 1.) {
            if (ac > bignum * ad) {
                *info = 1;
                scaloc = bignum * ad / ac;
                doscal = TRUE_;
                *scale *= scaloc;
            }
        }
        temp = dwork[cb + i__ - 1] * scaloc / dwork[zi];
        if (doscal) {
            doscal = FALSE_;
            dscal_(&kmn, &scaloc, &dwork[cb], &c__1);
        }
        dwork[cb + i__ - 1] = temp;
        len = mn + (i__ - 1) % mn + 1;
        zi2 = zis;
    L80:
        if (dwork[zi2] == 0.) {
            --len;
            ++zi2;
            goto L80;
        }
        /* Computing MAX */
        i__1 = 1, i__2 = i__ - len + 1;
        j = max(i__1, i__2);
        i__1 = i__ - j;
        d__1 = -temp;
        daxpy_(&i__1, &d__1, &dwork[zi - i__ + j], &c__1, &dwork[cb + j - 1], &c__1);
        if (mn > 1) {
            if (i__ % mn == 1) {
                iz = 1 - mn;
            } else {
                iz = 1;
            }
        }
        zi = zi - ldw - iz;
        zis -= ldw;
        /* L90: */
    }
    /*     Reshape the solution into C. */
    ic1 = 1;
    zi = cb;
    i__1 = *k;
    for (l = 1; l <= i__1; ++l) {
        c__[ic1] = dwork[zi];
        if (*m == 1) {
            if (*n == 2) {
                c__[ic1 + 1] = dwork[zi + 1];
            }
        } else {
            c__[ic1 + 1] = dwork[zi + 1];
            if (*n == 2) {
                c__[ic1 + 2] = dwork[zi + 2];
                c__[ic1 + 3] = dwork[zi + 3];
            }
        }
        ic1 += mn;
        zi += mn;
        /* L100: */
    }
    /*     Store the minimal workspace on output. */
    dwork[1] = (doublereal)minwrk;
    return 0;
    /* *** Last line of MB03KE *** */
} /* mb03ke_ */
