/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;
static logical c_true = TRUE_;
static integer c__2 = 2;
static doublereal c_b54 = 0.;
static doublereal c_b55 = 1.;

EXPORTSYMBOL /* Subroutine */ int mb03dd_(uplo, n1, n2, prec, a, lda, b, ldb, q1, ldq1, q2, ldq2,
    dwork, ldwork, info, uplo_len) char* uplo;
integer *n1, *n2;
doublereal *prec, *a;
integer* lda;
doublereal* b;
integer* ldb;
doublereal* q1;
integer* ldq1;
doublereal* q2;
integer* ldq2;
doublereal* dwork;
integer *ldwork, *info;
ftnlen uplo_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, q1_dim1, q1_offset, q2_dim1, q2_offset, i__1, i__2;
    doublereal d__1, d__2, d__3;
    /* Local variables */
    static doublereal adif;
    static integer iaev, idum;
    static doublereal tolb;
    static logical slct[4];
    static integer ievs, itmp;
    static doublereal e, g;
    static integer i__, j, m;
    extern /* Subroutine */ int dgges_(), dggev_();
    static doublereal absev;
    extern logical lsame_();
    static logical evinf;
    extern logical sb02ow_();
    static integer evsel;
    extern /* Subroutine */ int dcopy_(), dswap_();
    static logical bwork[1], luplo, ltriu;
    static doublereal absaev;
    static logical aevinf;
    extern /* Subroutine */ int dlacpy_(), dlartg_(), dlaset_(), dhgeqz_(), dtgsen_();
    static doublereal co1, co2, si1, si2;
    static integer idm[1], cnt;
    static doublereal dum[2], tol, tmp;
    static logical out[2];
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
    /*     To compute orthogonal matrices Q1 and Q2 for a real 2-by-2, */
    /*     3-by-3, or 4-by-4 regular block upper triangular pencil */
    /*                    ( A11 A12 )     ( B11 B12 ) */
    /*       aA - bB =  a (         ) - b (         ),                    (1) */
    /*                    (  0  A22 )     (  0  B22 ) */
    /*     such that the pencil a(Q2' A Q1) - b(Q2' B Q1) is still in block */
    /*     upper triangular form, but the eigenvalues in Spec(A11, B11), */
    /*     Spec(A22, B22) are exchanged, where Spec(X,Y) denotes the spectrum */
    /*     of the matrix pencil (X,Y). */
    /*     Optionally, to upper triangularize the real regular pencil in */
    /*     block lower triangular form */
    /*                   ( A11  0  )     ( B11  0  ) */
    /*       aA - bB = a (         ) - b (         ),                     (2) */
    /*                   ( A21 A22 )     ( B21 B22 ) */
    /*     while keeping the eigenvalues in the same diagonal position. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     UPLO    CHARACTER*1 */
    /*             Specifies if the pencil is in lower or upper block */
    /*             triangular form on entry, as follows: */
    /*             = 'U': Upper block triangular, eigenvalues are exchanged */
    /*                    on exit; */
    /*             = 'T': Upper block triangular, B triangular, eigenvalues */
    /*                    are exchanged on exit; */
    /*             = 'L': Lower block triangular, eigenvalues are not */
    /*                    exchanged on exit. */
    /*     Input/Output Parameters */
    /*     N1      (input/output) INTEGER */
    /*             Size of the upper left block, N1 <= 2. */
    /*             If UPLO = 'U' or UPLO = 'T' and INFO = 0, or UPLO = 'L' */
    /*             and INFO <> 0, N1 and N2 are exchanged on exit; otherwise, */
    /*             N1 is unchanged on exit. */
    /*     N2      (input/output) INTEGER */
    /*             Size of the lower right block, N2 <= 2. */
    /*             If UPLO = 'U' or UPLO = 'T' and INFO = 0, or UPLO = 'L' */
    /*             and INFO <> 0, N1 and N2 are exchanged on exit; otherwise, */
    /*             N2 is unchanged on exit. */
    /*     PREC    (input) DOUBLE PRECISION */
    /*             The machine precision, (relative machine precision)*base. */
    /*             See the LAPACK Library routine DLAMCH. */
    /*     A       (input/output) DOUBLE PRECISION array, dimension */
    /*                (LDA, N1+N2) */
    /*             On entry, the leading (N1+N2)-by-(N1+N2) part of this */
    /*             array must contain the matrix A of the pencil aA - bB. */
    /*             On exit, if N1 = N2 = 1, this array is unchanged, if */
    /*             UPLO = 'U' or UPLO = 'T', but, if UPLO = 'L', it contains */
    /*                                          [  0 1 ] */
    /*             the matrix J' A J, where J = [ -1 0 ]; otherwise, this */
    /*             array contains the transformed quasi-triangular matrix in */
    /*             generalized real Schur form. */
    /*     LDA     INTEGER */
    /*             The leading dimension of the array A.  LDA >= N1+N2. */
    /*     B       (input/output) DOUBLE PRECISION array, dimension */
    /*                (LDB, N1+N2) */
    /*             On entry, the leading (N1+N2)-by-(N1+N2) part of this */
    /*             array must contain the matrix B of the pencil aA - bB. */
    /*             On exit, if N1 = N2 = 1, this array is unchanged, if */
    /*             UPLO = 'U' or UPLO = 'T', but, if UPLO = 'L', it contains */
    /*             the matrix J' B J; otherwise, this array contains the */
    /*             transformed upper triangular matrix in generalized real */
    /*             Schur form. */
    /*     LDB     INTEGER */
    /*             The leading dimension of the array B.  LDB >= N1+N2. */
    /*     Q1      (output) DOUBLE PRECISION array, dimension (LDQ1, N1+N2) */
    /*             The leading (N1+N2)-by-(N1+N2) part of this array contains */
    /*             the first orthogonal transformation matrix. */
    /*     LDQ1    INTEGER */
    /*             The leading dimension of the array Q1.  LDQ1 >= N1+N2. */
    /*     Q2      (output) DOUBLE PRECISION array, dimension (LDQ2, N1+N2) */
    /*             The leading (N1+N2)-by-(N1+N2) part of this array contains */
    /*             the second orthogonal transformation matrix. */
    /*     LDQ2    INTEGER */
    /*             The leading dimension of the array Q2.  LDQ2 >= N1+N2. */
    /*     Workspace */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             If N1+N2 = 2 then DWORK is not referenced. */
    /*     LDWORK  INTEGER */
    /*             The dimension of the array DWORK. */
    /*             If N1+N2 = 2, then LDWORK = 0; otherwise, */
    /*             LDWORK >= 16*N1 + 10*N2 + 23, if UPLO = 'U'; */
    /*             LDWORK >=  7*N1 +  7*N2 + 16, if UPLO = 'T'; */
    /*             LDWORK >= 10*N1 + 16*N2 + 23, if UPLO = 'L'. */
    /*             For good performance LDWORK should be generally larger. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0: succesful exit; */
    /*             = 1: the QZ iteration failed in the LAPACK routine DGGEV; */
    /*             = 2: another error occured while executing a routine in */
    /*                  DGGEV; */
    /*             = 3: the QZ iteration failed in the LAPACK routine DGGES */
    /*                  (if UPLO <> 'T') or DHGEQZ (if UPLO = 'T'); */
    /*             = 4: another error occured during execution of DGGES or */
    /*                  DHGEQZ; */
    /*             = 5: reordering of aA - bB in the LAPACK routine DTGSEN */
    /*                  failed because the transformed matrix pencil aA - bB */
    /*                  would be too far from generalized Schur form; */
    /*                  the problem is very ill-conditioned. */
    /*     METHOD */
    /*     The algorithm uses orthogonal transformations as described in [2] */
    /*     (page 30). The QZ algorithm is used for N1 = 2 or N2 = 2, but it */
    /*     always acts on an upper block triangular pencil. */
    /*     REFERENCES */
    /*     [1] Benner, P., Byers, R., Mehrmann, V. and Xu, H. */
    /*         Numerical computation of deflating subspaces of skew- */
    /*         Hamiltonian/Hamiltonian pencils. */
    /*         SIAM J. Matrix Anal. Appl., 24 (1), pp. 165-190, 2002. */
    /*     [2] Benner, P., Byers, R., Losse, P., Mehrmann, V. and Xu, H. */
    /*         Numerical Solution of Real Skew-Hamiltonian/Hamiltonian */
    /*         Eigenproblems. */
    /*         Tech. Rep., Technical University Chemnitz, Germany, */
    /*         Nov. 2007. */
    /*     NUMERICAL ASPECTS */
    /*     The algorithm is numerically backward stable. */
    /*     CONTRIBUTOR */
    /*     Matthias Voigt, Fakultaet fuer Mathematik, Technische Universitaet */
    /*     Chemnitz, October 16, 2008. */
    /*     REVISIONS */
    /*     V. Sima, July 2009 (SLICOT version of the routine DBTUEX). */
    /*     V. Sima, Nov. 2009, Oct. 2010, Nov. 2010. */
    /*     KEYWORDS */
    /*     Block triangular pencil, eigenvalue exchange. */
    /*     ****************************************************************** */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. Local Arrays .. */
    /*     .. External Functions .. */
    /*     .. External Subroutines .. */
    /*     .. Intrinsic Functions .. */
    /*     .. Executable Statements .. */
    /*     Decode the input arguments. */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    q1_dim1 = *ldq1;
    q1_offset = q1_dim1 + 1;
    q1 -= q1_offset;
    q2_dim1 = *ldq2;
    q2_offset = q2_dim1 + 1;
    q2 -= q2_offset;
    --dwork;
    /* Function Body */
    ltriu = lsame_(uplo, "T", 1L, 1L);
    luplo = lsame_(uplo, "U", 1L, 1L) || ltriu;
    /*     For efficiency, the input arguments are not tested. */
    *info = 0;
    /*     Computations. */
    /*     (Note: Comments in the code beginning "Workspace:" describe the */
    /*     minimal amount of real workspace needed at that point in the */
    /*     code, as well as the preferred amount for good performance.) */
    m = *n1 + *n2;
    if (m > 2) {
        if (!luplo) {
            /*           Make the pencil upper block triangular. */
            if (*n1 == 1) {
                dum[0] = a[a_dim1 + 1];
                dum[1] = a[a_dim1 + 2];
                a[a_dim1 + 1] = a[(a_dim1 << 1) + 2];
                a[a_dim1 + 2] = a[(a_dim1 << 1) + 3];
                a[(a_dim1 << 1) + 1] = a[a_dim1 * 3 + 2];
                a[(a_dim1 << 1) + 2] = a[a_dim1 * 3 + 3];
                a[a_dim1 * 3 + 1] = dum[1];
                a[a_dim1 * 3 + 2] = a[a_dim1 + 3];
                a[a_dim1 * 3 + 3] = dum[0];
                a[a_dim1 + 3] = 0.;
                a[(a_dim1 << 1) + 3] = 0.;
                dum[0] = b[b_dim1 + 1];
                dum[1] = b[b_dim1 + 2];
                b[b_dim1 + 1] = b[(b_dim1 << 1) + 2];
                b[b_dim1 + 2] = b[(b_dim1 << 1) + 3];
                b[(b_dim1 << 1) + 1] = b[b_dim1 * 3 + 2];
                b[(b_dim1 << 1) + 2] = b[b_dim1 * 3 + 3];
                b[b_dim1 * 3 + 1] = dum[1];
                b[b_dim1 * 3 + 2] = b[b_dim1 + 3];
                b[b_dim1 * 3 + 3] = dum[0];
                b[b_dim1 + 3] = 0.;
                b[(b_dim1 << 1) + 3] = 0.;
            } else if (*n2 == 1) {
                dum[0] = a[(a_dim1 << 1) + 3];
                dum[1] = a[a_dim1 * 3 + 3];
                a[a_dim1 * 3 + 2] = a[(a_dim1 << 1) + 1];
                a[a_dim1 * 3 + 3] = a[(a_dim1 << 1) + 2];
                a[(a_dim1 << 1) + 2] = a[a_dim1 + 1];
                a[(a_dim1 << 1) + 3] = a[a_dim1 + 2];
                a[a_dim1 + 1] = dum[1];
                a[(a_dim1 << 1) + 1] = a[a_dim1 + 3];
                a[a_dim1 * 3 + 1] = dum[0];
                a[a_dim1 + 2] = 0.;
                a[a_dim1 + 3] = 0.;
                dum[0] = b[(b_dim1 << 1) + 3];
                dum[1] = b[b_dim1 * 3 + 3];
                b[b_dim1 * 3 + 2] = b[(b_dim1 << 1) + 1];
                b[b_dim1 * 3 + 3] = b[(b_dim1 << 1) + 2];
                b[(b_dim1 << 1) + 2] = b[b_dim1 + 1];
                b[(b_dim1 << 1) + 3] = b[b_dim1 + 2];
                b[b_dim1 + 1] = dum[1];
                b[(b_dim1 << 1) + 1] = b[b_dim1 + 3];
                b[b_dim1 * 3 + 1] = dum[0];
                b[b_dim1 + 2] = 0.;
                b[b_dim1 + 3] = 0.;
            } else {
                i__1 = *n1;
                for (j = 1; j <= i__1; ++j) {
                    dswap_(n1, &a[j * a_dim1 + 1], &c__1, &a[*n1 + 1 + (*n1 + j) * a_dim1], &c__1);
                    dswap_(n1, &a[(*n1 + j) * a_dim1 + 1], &c__1, &a[*n1 + 1 + j * a_dim1], &c__1);
                    dswap_(n1, &b[j * b_dim1 + 1], &c__1, &b[*n1 + 1 + (*n1 + j) * b_dim1], &c__1);
                    dswap_(n1, &b[(*n1 + j) * b_dim1 + 1], &c__1, &b[*n1 + 1 + j * b_dim1], &c__1);
                    /* L10: */
                }
            }
            itmp = *n1;
            *n1 = *n2;
            *n2 = itmp;
        }
        /*        Apply the QZ algorithm and order the eigenvalues in */
        /*        DWORK(1:3*N1) to the top. */
        /*        Note that N1 and N2 are interchanged for UPLO = 'L'. */
        ievs = *n1 * 3 + 1;
        iaev = ievs + *n1 * 3;
        dlacpy_("Full", &m, &m, &a[a_offset], lda, &q1[q1_offset], ldq1, 4L);
        dlacpy_("Full", &m, &m, &b[b_offset], ldb, &q2[q2_offset], ldq2, 4L);
        if (ltriu) {
            /*           Workspace: need   4*N1. */
            i__1 = *ldwork - ievs + 1;
            dhgeqz_("Eigenvalues", "No Vector", "No Vector", n1, &c__1, n1, &q1[q1_offset], ldq1,
                &q2[q2_offset], ldq2, &dwork[1], &dwork[*n1 + 1], &dwork[(*n1 << 1) + 1], dum,
                &c__1, dum, &c__1, &dwork[ievs], &i__1, info, 11L, 9L, 9L);
        } else {
            /*           Workspace: need   11*N1; */
            /*                      prefer larger. */
            i__1 = *ldwork - ievs + 1;
            dggev_("No Vector", "No Vector", n1, &q1[q1_offset], ldq1, &q2[q2_offset], ldq2,
                &dwork[1], &dwork[*n1 + 1], &dwork[(*n1 << 1) + 1], dum, &c__1, dum, &c__1,
                &dwork[ievs], &i__1, info, 9L, 9L);
        }
        if (*info >= 1 && *info <= *n1) {
            *info = 1;
            return 0;
        } else if (*info > *n1) {
            *info = 2;
            return 0;
        }
        itmp = iaev + m * 3;
        i__1 = *n1 * 3;
        dcopy_(&i__1, &dwork[1], &c__1, &dwork[ievs], &c__1);
        if (ltriu) {
            /*           Workspace: need   10*N1 + 4*N2. */
            i__1 = *ldwork - itmp + 1;
            dhgeqz_("Schur", "Identity", "Identity", &m, &c__1, &m, &a[a_offset], lda, &b[b_offset],
                ldb, &dwork[iaev], &dwork[iaev + m], &dwork[iaev + (m << 1)], &q2[q2_offset], ldq2,
                &q1[q1_offset], ldq1, &dwork[itmp], &i__1, info, 5L, 8L, 8L);
            if (*info >= 1 && *info <= m) {
                *info = 3;
                return 0;
            } else if (*info != 0) {
                *info = 4;
                return 0;
            }
        } else {
            /*           Workspace: need   16*N1 + 10*N2 + 23; */
            /*                      prefer larger. */
            i__1 = *ldwork - itmp + 1;
            dgges_("Vectors", "Vectors", "Not sorted", sb02ow_, &m, &a[a_offset], lda, &b[b_offset],
                ldb, &idum, &dwork[iaev], &dwork[iaev + m], &dwork[iaev + (m << 1)], &q2[q2_offset],
                ldq2, &q1[q1_offset], ldq1, &dwork[itmp], &i__1, bwork, info, 7L, 7L, 10L);
            if (*info != 0) {
                if (*info >= 1 && *info <= m) {
                    *info = 3;
                    return 0;
                } else if (*info != m + 2) {
                    *info = 4;
                    return 0;
                } else {
                    *info = 0;
                }
            }
        }
        tol = *prec;
        tolb = *prec * 10.;
        evsel = 0;
        i__1 = m;
        for (i__ = 1; i__ <= i__1; ++i__) {
            slct[i__ - 1] = TRUE_;
            /* L20: */
        }
        /*        WHILE( EVSEL.EQ.0 ) DO */
    L30:
        if (evsel == 0) {
            cnt = 0;
            out[0] = FALSE_;
            out[1] = FALSE_;
            i__1 = iaev + m - 1;
            for (i__ = iaev; i__ <= i__1; ++i__) {
                aevinf = (d__1 = dwork[(m << 1) + i__], abs(d__1))
                    < *prec * ((d__2 = dwork[i__], abs(d__2)) + (d__3 = dwork[m + i__], abs(d__3)));
                i__2 = *n1;
                for (j = 1; j <= i__2; ++j) {
                    /*                 Check if an eigenvalue is selected and check if it */
                    /*                 is infinite. */
                    evinf = (d__1 = dwork[(*n1 << 1) + j], abs(d__1)) < *prec
                            * ((d__2 = dwork[j], abs(d__2)) + (d__3 = dwork[*n1 + j], abs(d__3)));
                    if ((!evinf || aevinf) && (!aevinf || evinf) && !out[j - 1]) {
                        if (!evinf || !aevinf) {
                            adif = (d__1 = dwork[j] / dwork[(*n1 << 1) + j]
                                           - dwork[i__] / dwork[(m << 1) + i__],
                                       abs(d__1))
                                + (d__2 = dwork[*n1 + j] / dwork[(*n1 << 1) + j]
                                          - dwork[m + i__] / dwork[(m << 1) + i__],
                                      abs(d__2));
                            absev = (d__1 = dwork[j] / dwork[(*n1 << 1) + j], abs(d__1))
                                + (d__2 = dwork[*n1 + j] / dwork[(*n1 << 1) + j], abs(d__2));
                            absaev = (d__1 = dwork[i__] / dwork[(m << 1) + i__], abs(d__1))
                                + (d__2 = dwork[m + i__] / dwork[(m << 1) + i__], abs(d__2));
                            /* Computing MAX */
                            d__1 = max(tolb, absev);
                            if (adif <= tol * max(d__1, absaev)) {
                                slct[i__ - iaev] = FALSE_;
                                out[j - 1] = TRUE_;
                                ++cnt;
                            }
                        } else {
                            slct[i__ - iaev] = FALSE_;
                            out[j - 1] = TRUE_;
                            ++cnt;
                        }
                    }
                    /* L40: */
                }
                /* L50: */
            }
            if (cnt == *n1) {
                evsel = 1;
            } else {
                /*              CNT < N1, too few eigenvalues selected. */
                tol *= 10.;
                i__1 = *n1 * 3;
                dcopy_(&i__1, &dwork[ievs], &c__1, &dwork[1], &c__1);
            }
            goto L30;
        }
        /*        END WHILE 30 */
        /*        Workspace: need   7*N1 + 7*N2 + 16; */
        /*                   prefer larger. */
        itmp = m * 3 + 1;
        i__1 = *ldwork - itmp + 1;
        dtgsen_(&c__0, &c_true, &c_true, slct, &m, &a[a_offset], lda, &b[b_offset], ldb, &dwork[1],
            &dwork[m + 1], &dwork[(m << 1) + 1], &q2[q2_offset], ldq2, &q1[q1_offset], ldq1, &idum,
            &tmp, &tmp, dum, &dwork[itmp], &i__1, idm, &c__1, info);
        if (*info == 1) {
            *info = 5;
            return 0;
        }
        /*        Interchange N1 and N2. */
        itmp = *n1;
        *n1 = *n2;
        *n2 = itmp;
        if (!luplo) {
            /*           Permute the rows of Q1 and Q2. */
            if (*n1 == 1) {
                i__1 = m;
                for (j = 1; j <= i__1; ++j) {
                    tmp = q1[j * q1_dim1 + 3];
                    q1[j * q1_dim1 + 3] = q1[j * q1_dim1 + 2];
                    q1[j * q1_dim1 + 2] = q1[j * q1_dim1 + 1];
                    q1[j * q1_dim1 + 1] = tmp;
                    tmp = q2[j * q2_dim1 + 3];
                    q2[j * q2_dim1 + 3] = q2[j * q2_dim1 + 2];
                    q2[j * q2_dim1 + 2] = q2[j * q2_dim1 + 1];
                    q2[j * q2_dim1 + 1] = tmp;
                    /* L60: */
                }
            } else if (*n2 == 1) {
                i__1 = m;
                for (j = 1; j <= i__1; ++j) {
                    tmp = q1[j * q1_dim1 + 1];
                    q1[j * q1_dim1 + 1] = q1[j * q1_dim1 + 2];
                    q1[j * q1_dim1 + 2] = q1[j * q1_dim1 + 3];
                    q1[j * q1_dim1 + 3] = tmp;
                    tmp = q2[j * q2_dim1 + 1];
                    q2[j * q2_dim1 + 1] = q2[j * q2_dim1 + 2];
                    q2[j * q2_dim1 + 2] = q2[j * q2_dim1 + 3];
                    q2[j * q2_dim1 + 3] = tmp;
                    /* L70: */
                }
            } else {
                i__1 = m;
                for (j = 1; j <= i__1; ++j) {
                    dswap_(n1, &q1[j * q1_dim1 + 1], &c__1, &q1[*n1 + 1 + j * q1_dim1], &c__1);
                    dswap_(n1, &q2[j * q2_dim1 + 1], &c__1, &q2[*n1 + 1 + j * q2_dim1], &c__1);
                    /* L80: */
                }
            }
        }
    } else {
        /*        2-by-2 case. */
        if (!luplo) {
            tmp = a[a_dim1 + 1];
            a[a_dim1 + 1] = a[(a_dim1 << 1) + 2];
            a[(a_dim1 << 1) + 2] = tmp;
            a[(a_dim1 << 1) + 1] = -a[a_dim1 + 2];
            a[a_dim1 + 2] = 0.;
            tmp = b[b_dim1 + 1];
            b[b_dim1 + 1] = b[(b_dim1 << 1) + 2];
            b[(b_dim1 << 1) + 2] = tmp;
            b[(b_dim1 << 1) + 1] = -b[b_dim1 + 2];
            b[b_dim1 + 2] = 0.;
        }
        g = a[a_dim1 + 1] * b[(b_dim1 << 1) + 2] - a[(a_dim1 << 1) + 2] * b[b_dim1 + 1];
        if (abs(g) < *prec * 100. * (d__1 = a[a_dim1 + 1] * b[(b_dim1 << 1) + 2], abs(d__1))) {
            /*           The eigenvalues might be too close to interchange them. */
            if (luplo) {
                dlaset_("Full", &c__2, &c__2, &c_b54, &c_b55, &q1[q1_offset], ldq1, 4L);
                dlaset_("Full", &c__2, &c__2, &c_b54, &c_b55, &q2[q2_offset], ldq2, 4L);
            } else {
                q1[q1_dim1 + 1] = 0.;
                q1[q1_dim1 + 2] = -1.;
                q1[(q1_dim1 << 1) + 1] = 1.;
                q1[(q1_dim1 << 1) + 2] = 0.;
                q2[q2_dim1 + 1] = 0.;
                q2[q2_dim1 + 2] = -1.;
                q2[(q2_dim1 << 1) + 1] = 1.;
                q2[(q2_dim1 << 1) + 2] = 0.;
            }
        } else {
            e = a[(a_dim1 << 1) + 1] * b[(b_dim1 << 1) + 2]
                - a[(a_dim1 << 1) + 2] * b[(b_dim1 << 1) + 1];
            dlartg_(&e, &g, &co1, &si1, &tmp);
            e = a[(a_dim1 << 1) + 1] * b[b_dim1 + 1] - a[a_dim1 + 1] * b[(b_dim1 << 1) + 1];
            dlartg_(&e, &g, &co2, &si2, &tmp);
            if (luplo) {
                q1[q1_dim1 + 1] = co1;
                q1[q1_dim1 + 2] = -si1;
                q1[(q1_dim1 << 1) + 1] = si1;
                q1[(q1_dim1 << 1) + 2] = co1;
                q2[q2_dim1 + 1] = co2;
                q2[q2_dim1 + 2] = -si2;
                q2[(q2_dim1 << 1) + 1] = si2;
                q2[(q2_dim1 << 1) + 2] = co2;
            } else {
                q1[q1_dim1 + 1] = -si1;
                q1[q1_dim1 + 2] = -co1;
                q1[(q1_dim1 << 1) + 1] = co1;
                q1[(q1_dim1 << 1) + 2] = -si1;
                q2[q2_dim1 + 1] = -si2;
                q2[q2_dim1 + 2] = -co2;
                q2[(q2_dim1 << 1) + 1] = co2;
                q2[(q2_dim1 << 1) + 2] = -si2;
            }
        }
    }
    return 0;
    /* *** Last line of MB03DD *** */
} /* mb03dd_ */
