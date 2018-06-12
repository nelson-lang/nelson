/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static doublecomplex c_b1 = { 1., 0. };
static doublereal c_b17 = -1.;
static doublereal c_b18 = 1.;
static doublereal c_b24 = 0.;

doublereal ab13dx_(dico, jobe, jobd, n, m, p, omega, a, lda, e, lde, b, ldb, c__, ldc, d__, ldd,
    iwork, dwork, ldwork, cwork, lcwork, info, dico_len, jobe_len, jobd_len) char *dico,
    *jobe, *jobd;
integer *n, *m, *p;
doublereal *omega, *a;
integer* lda;
doublereal* e;
integer* lde;
doublereal* b;
integer* ldb;
doublereal* c__;
integer* ldc;
doublereal* d__;
integer *ldd, *iwork;
doublereal* dwork;
integer* ldwork;
doublecomplex* cwork;
integer *lcwork, *info;
ftnlen dico_len;
ftnlen jobe_len;
ftnlen jobd_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, d_dim1, d_offset, e_dim1,
        e_offset, i__1, i__2, i__3;
    doublereal ret_val = 0., d__1, d__2;
    doublecomplex z__1;
    /* Builtin functions */
    double cos(), sin();
    /* Local variables */
    static integer icwk, ierr, iwrk, i__, j;
    extern EXPORTSYMBOL /* Subroutine */ int mb02rd_(), mb02sd_(), dgemm_();
    extern logical lsame_();
    static logical discr, specl, fulle;
    extern /* Subroutine */ int mb02rz_();
    static doublereal bnorm, cnorm;
    static logical withd;
    static integer minpm;
    extern /* Subroutine */ int mb02sz_(), zgemm_();
    static logical nodyn;
    extern /* Subroutine */ int zlacp2_();
    static integer id;
    static doublereal lambdi;
    extern doublereal dlange_();
    static integer is;
    static doublereal lambdr;
    extern /* Subroutine */ int dgesvd_(), xerbla_();
    static integer mincwr;
    extern /* Subroutine */ int zgesvd_();
    static integer minwrk, maxwrk, icb, icc, icd;
    static doublereal upd;
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
    /*     To compute the maximum singular value of a given continuous-time */
    /*     or discrete-time transfer-function matrix, either standard or in */
    /*     the descriptor form, */
    /*                                     -1 */
    /*        G(lambda) = C*( lambda*E - A ) *B + D , */
    /*     for a given complex value lambda, where lambda = j*omega, in the */
    /*     continuous-time case, and lambda = exp(j*omega), in the */
    /*     discrete-time case. The matrices A, E, B, C, and D are real */
    /*     matrices of appropriate dimensions. Matrix A must be in an upper */
    /*     Hessenberg form, and if JOBE ='G', the matrix E must be upper */
    /*     triangular. The matrices B and C must correspond to the system */
    /*     in (generalized) Hessenberg form. */
    /*     FUNCTION VALUE */
    /*     AB13DX   DOUBLE PRECISION */
    /*              The maximum singular value of G(lambda). */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     DICO    CHARACTER*1 */
    /*             Specifies the type of the system, as follows: */
    /*             = 'C':  continuous-time system; */
    /*             = 'D':  discrete-time system. */
    /*     JOBE    CHARACTER*1 */
    /*             Specifies whether E is an upper triangular or an identity */
    /*             matrix, as follows: */
    /*             = 'G':  E is a general upper triangular matrix; */
    /*             = 'I':  E is the identity matrix. */
    /*     JOBD    CHARACTER*1 */
    /*             Specifies whether or not a non-zero matrix D appears in */
    /*             the given state space model: */
    /*             = 'D':  D is present; */
    /*             = 'Z':  D is assumed a zero matrix. */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the system.  N >= 0. */
    /*     M       (input) INTEGER */
    /*             The column size of the matrix B.  M >= 0. */
    /*     P       (input) INTEGER */
    /*             The row size of the matrix C.  P >= 0. */
    /*     OMEGA   (input) DOUBLE PRECISION */
    /*             The frequency value for which the calculations should be */
    /*             done. */
    /*     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) */
    /*             On entry, the leading N-by-N upper Hessenberg part of this */
    /*             array must contain the state dynamics matrix A in upper */
    /*             Hessenberg form. The elements below the subdiagonal are */
    /*             not referenced. */
    /*             On exit, if M > 0, P > 0, OMEGA = 0, DICO = 'C', B <> 0, */
    /*             and C <> 0, the leading N-by-N upper Hessenberg part of */
    /*             this array contains the factors L and U from the LU */
    /*             factorization of A (A = P*L*U); the unit diagonal elements */
    /*             of L are not stored, L is lower bidiagonal, and P is */
    /*             stored in IWORK (see SLICOT Library routine MB02SD). */
    /*             Otherwise, this array is unchanged on exit. */
    /*     LDA     INTEGER */
    /*             The leading dimension of the array A.  LDA >= max(1,N). */
    /*     E       (input) DOUBLE PRECISION array, dimension (LDE,N) */
    /*             If JOBE = 'G', the leading N-by-N upper triangular part of */
    /*             this array must contain the upper triangular descriptor */
    /*             matrix E of the system. The elements of the strict lower */
    /*             triangular part of this array are not referenced. */
    /*             If JOBE = 'I', then E is assumed to be the identity */
    /*             matrix and is not referenced. */
    /*     LDE     INTEGER */
    /*             The leading dimension of the array E. */
    /*             LDE >= MAX(1,N), if JOBE = 'G'; */
    /*             LDE >= 1,        if JOBE = 'I'. */
    /*     B       (input/output) DOUBLE PRECISION array, dimension (LDB,M) */
    /*             On entry, the leading N-by-M part of this array must */
    /*             contain the system input matrix B. */
    /*             On exit, if M > 0, P > 0, OMEGA = 0, DICO = 'C', B <> 0, */
    /*             C <> 0, and INFO = 0 or N+1, the leading N-by-M part of */
    /*             this array contains the solution of the system A*X = B. */
    /*             Otherwise, this array is unchanged on exit. */
    /*     LDB     INTEGER */
    /*             The leading dimension of the array B.  LDB >= max(1,N). */
    /*     C       (input) DOUBLE PRECISION array, dimension (LDC,N) */
    /*             The leading P-by-N part of this array must contain the */
    /*             system output matrix C. */
    /*     LDC     INTEGER */
    /*             The leading dimension of the array C.  LDC >= max(1,P). */
    /*     D       (input/output) DOUBLE PRECISION array, dimension (LDD,M) */
    /*             On entry, if JOBD = 'D', the leading P-by-M part of this */
    /*             array must contain the direct transmission matrix D. */
    /*             On exit, if (N = 0, or B = 0, or C = 0) and JOBD = 'D', */
    /*             or (OMEGA = 0, DICO = 'C', JOBD = 'D', and INFO = 0 or */
    /*             N+1), the contents of this array is destroyed. */
    /*             Otherwise, this array is unchanged on exit. */
    /*             This array is not referenced if JOBD = 'Z'. */
    /*     LDD     INTEGER */
    /*             The leading dimension of array D. */
    /*             LDD >= MAX(1,P), if JOBD = 'D'; */
    /*             LDD >= 1,        if JOBD = 'Z'. */
    /*     Workspace */
    /*     IWORK   INTEGER array, dimension (LIWORK), where */
    /*             LIWORK = N, if N > 0, M > 0, P > 0, B <> 0, and C <> 0; */
    /*             LIWORK = 0, otherwise. */
    /*             This array contains the pivot indices in the LU */
    /*             factorization of the matrix lambda*E - A; for 1 <= i <= N, */
    /*             row i of the matrix was interchanged with row IWORK(i). */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             On exit, if INFO = 0, DWORK(1) contains the optimal value */
    /*             of LDWORK, and DWORK(2), ..., DWORK(MIN(P,M)) contain the */
    /*             singular values of G(lambda), except for the first one, */
    /*             which is returned in the function value AB13DX. */
    /*             If (N = 0, or B = 0, or C = 0) and JOBD = 'Z', the last */
    /*             MIN(P,M)-1 zero singular values of G(lambda) are not */
    /*             stored in DWORK(2), ..., DWORK(MIN(P,M)). */
    /*     LDWORK  INTEGER */
    /*             The dimension of the array DWORK. */
    /*             LDWORK >= MAX(1, LDW1 + LDW2 ), */
    /*             LDW1 = P*M, if N > 0, B <> 0, C <> 0, OMEGA = 0, */
    /*                            DICO = 'C', and JOBD = 'Z'; */
    /*             LDW1 = 0,   otherwise; */
    /*             LDW2 = MIN(P,M) + MAX(3*MIN(P,M) + MAX(P,M), 5*MIN(P,M)), */
    /*                         if (N = 0, or B = 0, or C = 0) and JOBD = 'D', */
    /*                         or (N > 0, B <> 0, C <> 0, OMEGA = 0, and */
    /*                             DICO = 'C'); */
    /*             LDW2 = 0,   if (N = 0, or B = 0, or C = 0) and JOBD = 'Z', */
    /*                         or MIN(P,M) = 0; */
    /*             LDW2 = 6*MIN(P,M), otherwise. */
    /*             For good performance, LDWORK must generally be larger. */
    /*     CWORK   COMPLEX*16 array, dimension (LCWORK) */
    /*             On exit, if INFO = 0, CWORK(1) contains the optimal */
    /*             LCWORK. */
    /*     LCWORK  INTEGER */
    /*             The dimension of the array CWORK. */
    /*             LCWORK >= 1, if N = 0, or B = 0, or C = 0, or (OMEGA = 0 */
    /*                             and DICO = 'C') or MIN(P,M) = 0; */
    /*             LCWORK >= MAX(1, (N+M)*(N+P) + 2*MIN(P,M) + MAX(P,M)), */
    /*                          otherwise. */
    /*             For good performance, LCWORK must generally be larger. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -i, the i-th argument had an illegal */
    /*                   value; */
    /*             > 0:  if INFO = i, U(i,i) is exactly zero; the LU */
    /*                   factorization of the matrix lambda*E - A has been */
    /*                   completed, but the factor U is exactly singular, */
    /*                   i.e., the matrix lambda*E - A is exactly singular; */
    /*             = N+1:  the SVD algorithm for computing singular values */
    /*                   did not converge. */
    /*     METHOD */
    /*     The routine implements standard linear algebra calculations, */
    /*     taking problem structure into account. LAPACK Library routines */
    /*     DGESVD and ZGESVD are used for finding the singular values. */
    /*     CONTRIBUTORS */
    /*     D. Sima, University of Bucharest, May 2001. */
    /*     V. Sima, Research Institute for Informatics, Bucharest, May 2001. */
    /*     REVISIONS */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Sep. 2005. */
    /*     KEYWORDS */
    /*     H-infinity optimal control, robust control, system norm. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. */
    /*     .. Scalar Arguments .. */
    /*     .. */
    /*     .. Array Arguments .. */
    /*     .. */
    /*     .. Local Scalars .. */
    /*     .. External Functions .. */
    /*     .. */
    /*     .. External Subroutines .. */
    /*     .. */
    /*     .. Intrinsic Functions .. */
    /*     .. */
    /*     .. Executable Statements .. */
    /*     Test the input scalar parameters. */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    e_dim1 = *lde;
    e_offset = e_dim1 + 1;
    e -= e_offset;
    b_dim1 = *ldb;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    c_dim1 = *ldc;
    c_offset = c_dim1 + 1;
    c__ -= c_offset;
    d_dim1 = *ldd;
    d_offset = d_dim1 + 1;
    d__ -= d_offset;
    --iwork;
    --dwork;
    --cwork;
    /* Function Body */
    *info = 0;
    discr = lsame_(dico, "D", 1L, 1L);
    fulle = lsame_(jobe, "G", 1L, 1L);
    withd = lsame_(jobd, "D", 1L, 1L);
    if (!(discr || lsame_(dico, "C", 1L, 1L))) {
        *info = -1;
    } else if (!(fulle || lsame_(jobe, "I", 1L, 1L))) {
        *info = -2;
    } else if (!(withd || lsame_(jobd, "Z", 1L, 1L))) {
        *info = -3;
    } else if (*n < 0) {
        *info = -4;
    } else if (*m < 0) {
        *info = -5;
    } else if (*p < 0) {
        *info = -6;
    } else if (*lda < max(1, *n)) {
        *info = -9;
    } else if (*lde < 1 || fulle && *lde < *n) {
        *info = -11;
    } else if (*ldb < max(1, *n)) {
        *info = -13;
    } else if (*ldc < max(1, *p)) {
        *info = -15;
    } else if (*ldd < 1 || withd && *ldd < *p) {
        *info = -17;
    } else {
        bnorm = dlange_("1-norm", n, m, &b[b_offset], ldb, &dwork[1], 6L);
        cnorm = dlange_("1-norm", p, n, &c__[c_offset], ldc, &dwork[1], 6L);
        nodyn = *n == 0 || min(bnorm, cnorm) == 0.;
        specl = !nodyn && *omega == 0. && !discr;
        minpm = min(*p, *m);
        /*        Compute workspace. */
        if (minpm == 0) {
            minwrk = 0;
        } else if (specl || nodyn && withd) {
            /* Computing MAX */
            i__1 = minpm * 3 + max(*p, *m), i__2 = minpm * 5;
            minwrk = minpm + max(i__1, i__2);
            if (specl && !withd) {
                minwrk += *p * *m;
            }
        } else if (nodyn && !withd) {
            minwrk = 0;
        } else {
            minwrk = minpm * 6;
        }
        minwrk = max(1, minwrk);
        if (*ldwork < minwrk) {
            *info = -20;
        } else {
            if (nodyn || *omega == 0. && !discr || minpm == 0) {
                mincwr = 1;
            } else {
                /* Computing MAX */
                i__1 = 1, i__2 = (*n + *m) * (*n + *p) + (minpm << 1) + max(*p, *m);
                mincwr = max(i__1, i__2);
            }
            if (*lcwork < mincwr) {
                *info = -22;
            }
        }
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("AB13DX", &i__1, 6L);
        return ret_val;
    }
    /*     Quick return if possible. */
    if (minpm == 0) {
        ret_val = 0.;
        dwork[1] = 1.;
        cwork[1].r = 1., cwork[1].i = 0.;
        return ret_val;
    }
    /*     (Note: Comments in the code beginning "Workspace:" describe the */
    /*     minimal amount of real workspace needed at that point in the */
    /*     code, as well as the preferred amount for good performance.) */
    is = 1;
    iwrk = is + minpm;
    if (nodyn) {
        /*        No dynamics: Determine the maximum singular value of G = D . */
        if (withd) {
            /*           Workspace: need   MIN(P,M) + MAX(3*MIN(P,M) + MAX(P,M), */
            /*                                            5*MIN(P,M)); */
            /*                      prefer larger. */
            i__1 = *ldwork - iwrk + 1;
            dgesvd_("No Vectors", "No Vectors", p, m, &d__[d_offset], ldd, &dwork[is], &dwork[1], p,
                &dwork[1], m, &dwork[iwrk], &i__1, &ierr, 10L, 10L);
            if (ierr > 0) {
                *info = *n + 1;
                return ret_val;
            }
            ret_val = dwork[is];
            maxwrk = (integer)dwork[iwrk] + iwrk - 1;
        } else {
            ret_val = 0.;
            maxwrk = 1;
        }
        dwork[1] = (doublereal)maxwrk;
        cwork[1].r = 1., cwork[1].i = 0.;
        return ret_val;
    }
    /*     Determine the maximum singular value of */
    /*        G(lambda) = C*inv(lambda*E - A)*B + D. */
    /*     The (generalized) Hessenberg form of the system is used. */
    if (specl) {
        /*        Special continuous-time case: */
        /*        Determine the maximum singular value of the real matrix G(0). */
        /*        Workspace: need   MIN(P,M) + MAX(3*MIN(P,M) + MAX(P,M), */
        /*                                         5*MIN(P,M)); */
        /*                   prefer larger. */
        mb02sd_(n, &a[a_offset], lda, &iwork[1], &ierr);
        if (ierr > 0) {
            *info = ierr;
            dwork[1] = 1.;
            cwork[1].r = 1., cwork[1].i = 0.;
            return ret_val;
        }
        mb02rd_("No Transpose", n, m, &a[a_offset], lda, &iwork[1], &b[b_offset], ldb, &ierr, 12L);
        if (withd) {
            dgemm_("No Transpose", "No Transpose", p, m, n, &c_b17, &c__[c_offset], ldc,
                &b[b_offset], ldb, &c_b18, &d__[d_offset], ldd, 12L, 12L);
            i__1 = *ldwork - iwrk + 1;
            dgesvd_("No Vectors", "No Vectors", p, m, &d__[d_offset], ldd, &dwork[is], &dwork[1], p,
                &dwork[1], m, &dwork[iwrk], &i__1, &ierr, 10L, 10L);
        } else {
            /*           Additional workspace: need   P*M. */
            id = iwrk;
            iwrk = id + *p * *m;
            dgemm_("No Transpose", "No Transpose", p, m, n, &c_b17, &c__[c_offset], ldc,
                &b[b_offset], ldb, &c_b24, &dwork[id], p, 12L, 12L);
            i__1 = *ldwork - iwrk + 1;
            dgesvd_("No Vectors", "No Vectors", p, m, &dwork[id], p, &dwork[is], &dwork[1], p,
                &dwork[1], m, &dwork[iwrk], &i__1, &ierr, 10L, 10L);
        }
        if (ierr > 0) {
            *info = *n + 1;
            return ret_val;
        }
        ret_val = dwork[is];
        dwork[1] = (doublereal)((integer)dwork[iwrk] + iwrk - 1);
        cwork[1].r = 1., cwork[1].i = 0.;
        return ret_val;
    }
    /*     General case: Determine the maximum singular value of G(lambda). */
    /*     Complex workspace:  need   N*N + N*M + P*N + P*M. */
    icb = *n * *n + 1;
    icc = icb + *n * *m;
    icd = icc + *p * *n;
    icwk = icd + *p * *m;
    if (withd) {
        upd = 1.;
    } else {
        upd = 0.;
    }
    if (discr) {
        lambdr = cos(*omega);
        lambdi = sin(*omega);
        /*        Build lambda*E - A . */
        if (fulle) {
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
                i__2 = j;
                for (i__ = 1; i__ <= i__2; ++i__) {
                    i__3 = i__ + (j - 1) * *n;
                    d__1 = lambdr * e[i__ + j * e_dim1] - a[i__ + j * a_dim1];
                    d__2 = lambdi * e[i__ + j * e_dim1];
                    z__1.r = d__1, z__1.i = d__2;
                    cwork[i__3].r = z__1.r, cwork[i__3].i = z__1.i;
                    /* L10: */
                }
                if (j < *n) {
                    i__2 = j + 1 + (j - 1) * *n;
                    d__1 = -a[j + 1 + j * a_dim1];
                    z__1.r = d__1, z__1.i = 0.;
                    cwork[i__2].r = z__1.r, cwork[i__2].i = z__1.i;
                }
                /* L20: */
            }
        } else {
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
                /* Computing MIN */
                i__3 = j + 1;
                i__2 = min(i__3, *n);
                for (i__ = 1; i__ <= i__2; ++i__) {
                    i__3 = i__ + (j - 1) * *n;
                    d__1 = -a[i__ + j * a_dim1];
                    cwork[i__3].r = d__1, cwork[i__3].i = 0.;
                    /* L30: */
                }
                i__2 = j + (j - 1) * *n;
                d__1 = lambdr - a[j + j * a_dim1];
                z__1.r = d__1, z__1.i = lambdi;
                cwork[i__2].r = z__1.r, cwork[i__2].i = z__1.i;
                /* L40: */
            }
        }
    } else {
        /*        Build j*omega*E - A. */
        if (fulle) {
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
                i__2 = j;
                for (i__ = 1; i__ <= i__2; ++i__) {
                    i__3 = i__ + (j - 1) * *n;
                    d__1 = -a[i__ + j * a_dim1];
                    d__2 = *omega * e[i__ + j * e_dim1];
                    z__1.r = d__1, z__1.i = d__2;
                    cwork[i__3].r = z__1.r, cwork[i__3].i = z__1.i;
                    /* L50: */
                }
                if (j < *n) {
                    i__2 = j + 1 + (j - 1) * *n;
                    d__1 = -a[j + 1 + j * a_dim1];
                    z__1.r = d__1, z__1.i = 0.;
                    cwork[i__2].r = z__1.r, cwork[i__2].i = z__1.i;
                }
                /* L60: */
            }
        } else {
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
                /* Computing MIN */
                i__3 = j + 1;
                i__2 = min(i__3, *n);
                for (i__ = 1; i__ <= i__2; ++i__) {
                    i__3 = i__ + (j - 1) * *n;
                    d__1 = -a[i__ + j * a_dim1];
                    cwork[i__3].r = d__1, cwork[i__3].i = 0.;
                    /* L70: */
                }
                i__2 = j + (j - 1) * *n;
                d__1 = -a[j + j * a_dim1];
                z__1.r = d__1, z__1.i = *omega;
                cwork[i__2].r = z__1.r, cwork[i__2].i = z__1.i;
                /* L80: */
            }
        }
    }
    /*     Build G(lambda) . */
    zlacp2_("Full", n, m, &b[b_offset], ldb, &cwork[icb], n, 4L);
    zlacp2_("Full", p, n, &c__[c_offset], ldc, &cwork[icc], p, 4L);
    if (withd) {
        zlacp2_("Full", p, m, &d__[d_offset], ldd, &cwork[icd], p, 4L);
    }
    mb02sz_(n, &cwork[1], n, &iwork[1], &ierr);
    if (ierr > 0) {
        *info = ierr;
        dwork[1] = 1.;
        i__1 = icwk - 1;
        cwork[1].r = (doublereal)i__1, cwork[1].i = 0.;
        return ret_val;
    }
    mb02rz_("No Transpose", n, m, &cwork[1], n, &iwork[1], &cwork[icb], n, &ierr, 12L);
    z__1.r = upd, z__1.i = 0.;
    zgemm_("No Transpose", "No Transpose", p, m, n, &c_b1, &cwork[icc], p, &cwork[icb], n, &z__1,
        &cwork[icd], p, 12L, 12L);
    /*     Additional workspace, complex: need   2*MIN(P,M) + MAX(P,M); */
    /*                                    prefer larger; */
    /*                           real:    need   5*MIN(P,M). */
    i__1 = *lcwork - icwk + 1;
    zgesvd_("No Vectors", "No Vectors", p, m, &cwork[icd], p, &dwork[is], &cwork[1], p, &cwork[1],
        m, &cwork[icwk], &i__1, &dwork[iwrk], &ierr, 10L, 10L);
    if (ierr > 0) {
        *info = *n + 1;
        return ret_val;
    }
    ret_val = dwork[is];
    dwork[1] = (doublereal)(minpm * 6);
    i__2 = icwk;
    i__1 = (integer)cwork[i__2].r + icwk - 1;
    cwork[1].r = (doublereal)i__1, cwork[1].i = 0.;
    return ret_val;
    /* *** Last line of AB13DX *** */
} /* ab13dx_ */
