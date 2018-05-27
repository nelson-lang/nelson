/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c__1 = 1;

EXPORTSYMBOL /* Subroutine */ int mb02td_(
    norm, n, hnorm, h__, ldh, ipiv, rcond, iwork, dwork, info, norm_len) char* norm;
integer* n;
doublereal *hnorm, *h__;
integer *ldh, *ipiv;
doublereal* rcond;
integer* iwork;
doublereal* dwork;
integer* info;
ftnlen norm_len;
{
    /* System generated locals */
    integer h_dim1, h_offset, i__1;
    doublereal d__1;
    /* Local variables */
    static integer kase, kase1, j;
    static doublereal t, scale;
    extern logical lsame_();
    extern /* Subroutine */ int drscl_();
    extern doublereal dlamch_();
    static integer jp;
    extern /* Subroutine */ int dlacon_();
    static integer ix;
    extern integer idamax_();
    extern /* Subroutine */ int xerbla_(), dlatrs_();
    static logical onenrm;
    static doublereal hinvnm;
    static char normin[1];
    static doublereal smlnum;
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
    /*     To estimate the reciprocal of the condition number of an upper */
    /*     Hessenberg matrix H, in either the 1-norm or the infinity-norm, */
    /*     using the LU factorization computed by MB02SD. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     NORM    CHARACTER*1 */
    /*             Specifies whether the 1-norm condition number or the */
    /*             infinity-norm condition number is required: */
    /*             = '1' or 'O':  1-norm; */
    /*             = 'I':         Infinity-norm. */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the matrix H.  N >= 0. */
    /*     HNORM   (input) DOUBLE PRECISION */
    /*             If NORM = '1' or 'O', the 1-norm of the original matrix H. */
    /*             If NORM = 'I', the infinity-norm of the original matrix H. */
    /*     H       (input) DOUBLE PRECISION array, dimension (LDH,N) */
    /*             The factors L and U from the factorization H = P*L*U */
    /*             as computed by MB02SD. */
    /*     LDH     INTEGER */
    /*             The leading dimension of the array H.  LDH >= max(1,N). */
    /*     IPIV    (input) INTEGER array, dimension (N) */
    /*             The pivot indices; for 1 <= i <= N, row i of the matrix */
    /*             was interchanged with row IPIV(i). */
    /*     RCOND   (output) DOUBLE PRECISION */
    /*             The reciprocal of the condition number of the matrix H, */
    /*             computed as RCOND = 1/(norm(H) * norm(inv(H))). */
    /*     Workspace */
    /*     IWORK   DOUBLE PRECISION array, dimension (N) */
    /*     DWORK   DOUBLE PRECISION array, dimension (3*N) */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -i, the i-th argument had an illegal */
    /*                   value. */
    /*     METHOD */
    /*     An estimate is obtained for norm(inv(H)), and the reciprocal of */
    /*     the condition number is computed as */
    /*        RCOND = 1 / ( norm(H) * norm(inv(H)) ). */
    /*     REFERENCES */
    /*     - */
    /*     NUMERICAL ASPECTS */
    /*                                2 */
    /*     The algorithm requires 0( N ) operations. */
    /*     CONTRIBUTOR */
    /*     V. Sima, Katholieke Univ. Leuven, Belgium, June 1998. */
    /*     REVISIONS */
    /*     - */
    /*     KEYWORDS */
    /*     Hessenberg form, matrix algebra. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. */
    /*     .. External Functions .. */
    /*     .. */
    /*     .. External Subroutines .. */
    /*     .. */
    /*     .. Intrinsic Functions .. */
    /*     .. */
    /*     .. Executable Statements .. */
    /*     Test the input parameters. */
    /* Parameter adjustments */
    h_dim1 = *ldh;
    h_offset = h_dim1 + 1;
    h__ -= h_offset;
    --ipiv;
    --iwork;
    --dwork;
    /* Function Body */
    *info = 0;
    onenrm = *(unsigned char*)norm == '1' || lsame_(norm, "O", 1L, 1L);
    if (!onenrm && !lsame_(norm, "I", 1L, 1L)) {
        *info = -1;
    } else if (*n < 0) {
        *info = -2;
    } else if (*hnorm < 0.) {
        *info = -3;
    } else if (*ldh < max(1, *n)) {
        *info = -5;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("MB02TD", &i__1, 6L);
        return 0;
    }
    /*     Quick return if possible. */
    *rcond = 0.;
    if (*n == 0) {
        *rcond = 1.;
        return 0;
    } else if (*hnorm == 0.) {
        return 0;
    }
    smlnum = dlamch_("Safe minimum", 12L);
    /*     Estimate the norm of inv(H). */
    hinvnm = 0.;
    *(unsigned char*)normin = 'N';
    if (onenrm) {
        kase1 = 1;
    } else {
        kase1 = 2;
    }
    kase = 0;
L10:
    dlacon_(n, &dwork[*n + 1], &dwork[1], &iwork[1], &hinvnm, &kase);
    if (kase != 0) {
        if (kase == kase1) {
            /*           Multiply by inv(L). */
            i__1 = *n - 1;
            for (j = 1; j <= i__1; ++j) {
                jp = ipiv[j];
                t = dwork[jp];
                if (jp != j) {
                    dwork[jp] = dwork[j];
                    dwork[j] = t;
                }
                dwork[j + 1] -= t * h__[j + 1 + j * h_dim1];
                /* L20: */
            }
            /*           Multiply by inv(U). */
            dlatrs_("Upper", "No transpose", "Non-unit", normin, n, &h__[h_offset], ldh, &dwork[1],
                &scale, &dwork[(*n << 1) + 1], info, 5L, 12L, 8L, 1L);
        } else {
            /*           Multiply by inv(U'). */
            dlatrs_("Upper", "Transpose", "Non-unit", normin, n, &h__[h_offset], ldh, &dwork[1],
                &scale, &dwork[(*n << 1) + 1], info, 5L, 9L, 8L, 1L);
            /*           Multiply by inv(L'). */
            for (j = *n - 1; j >= 1; --j) {
                dwork[j] -= h__[j + 1 + j * h_dim1] * dwork[j + 1];
                jp = ipiv[j];
                if (jp != j) {
                    t = dwork[jp];
                    dwork[jp] = dwork[j];
                    dwork[j] = t;
                }
                /* L30: */
            }
        }
        /*        Divide X by 1/SCALE if doing so will not cause overflow. */
        *(unsigned char*)normin = 'Y';
        if (scale != 1.) {
            ix = idamax_(n, &dwork[1], &c__1);
            if (scale < (d__1 = dwork[ix], abs(d__1)) * smlnum || scale == 0.) {
                goto L40;
            }
            drscl_(n, &scale, &dwork[1], &c__1);
        }
        goto L10;
    }
    /*     Compute the estimate of the reciprocal condition number. */
    if (hinvnm != 0.) {
        *rcond = 1. / hinvnm / *hnorm;
    }
L40:
    return 0;
    /* *** Last line of MB02TD *** */
} /* mb02td_ */
