/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c_n1 = -1;
static integer c__2 = 2;
static integer c__1 = 1;
static integer c__0 = 0;
static logical c_true = TRUE_;
static doublereal c_b47 = 200.;
static doublereal c_b50 = 1.;
static doublereal c_b51 = 0.;

EXPORTSYMBOL /* Subroutine */ int mb04hd_(compq1, compq2, n, a, lda, b, ldb, q1, ldq1, q2, ldq2,
    bwork, iwork, liwork, dwork, ldwork, info, compq1_len, compq2_len) char *compq1,
    *compq2;
integer* n;
doublereal* a;
integer* lda;
doublereal* b;
integer* ldb;
doublereal* q1;
integer* ldq1;
doublereal* q2;
integer* ldq2;
logical* bwork;
integer *iwork, *liwork;
doublereal* dwork;
integer *ldwork, *info;
ftnlen compq1_len;
ftnlen compq2_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, q1_dim1, q1_offset, q2_dim1, q2_offset, i__1, i__2,
        i__3;
    /* Builtin functions */
    double log();
    /* Local variables */
    static doublereal base;
    static integer sdim, idum[1], itmp, iwrk, nrow, itmp2, itmp3;
    extern /* Subroutine */ int ma01bd_(), mb03ba_();
    static integer i__, j, k;
    extern /* Subroutine */ int mb03dd_();
    static integer m;
    extern /* Subroutine */ int mb03fd_(), mb03kd_();
    static integer r__;
    extern /* Subroutine */ int dscal_();
    static doublereal lgbas;
    extern /* Subroutine */ int dgemm_(), dgges_(), dggev_();
    extern logical lsame_();
    extern /* Subroutine */ int dgemv_();
    extern logical sb02ow_();
    extern /* Subroutine */ int dcopy_(), daxpy_();
    static integer i1, i2, i3, m1, m2, m4, i1lole, i2lole;
    static logical lcmpq1, lcmpq2, liniq1, liniq2;
    static integer i1lori, i1uple, i2lori;
    static logical lupdq1, lupdq2;
    static integer i2uple, i1upri, i2upri, ia, ib;
    extern doublereal dlamch_();
    static integer mm, nr, ialole, iblole;
    extern /* Subroutine */ int dlacpy_(), dlaset_(), xerbla_();
    static integer ialori, iauple, iblori, ibuple;
    extern /* Subroutine */ int dtgsen_();
    static integer iaupri, ibupri, ib1, ib2, kschur, ij1, ij2, minwrk, mp1, iv1;
    static logical lquery;
    static integer iv2, iv3, iv4, optwrk, ia11, ia22, ib12, ib21;
    static doublereal dum[1], ulp;
    static integer dim1, dim2;
    static doublereal tmp2, tmp3;
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
    /*     To compute the transformed matrices A and B, using orthogonal */
    /*     matrices Q1 and Q2 for a real N-by-N regular pencil */
    /*                    ( A11   0  )     (  0   B12 ) */
    /*       aA - bB =  a (          ) - b (          ),                  (1) */
    /*                    (  0   A22 )     ( B21   0  ) */
    /*     where A11, A22 and B12 are upper triangular and the generalized */
    /*                       -1        -1 */
    /*     matrix product A11   B12 A22   B21 is upper quasi-triangular, */
    /*     such that the matrix Q2' A Q1 is upper triangular and Q2' B Q1 is */
    /*     upper quasi-triangular. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     COMPQ1  CHARACTER*1 */
    /*             Specifies whether to compute the orthogonal transformation */
    /*             matrix Q1, as follows: */
    /*             = 'N':  Q1 is not computed; */
    /*             = 'I':  the array Q1 is initialized internally to the unit */
    /*                     matrix, and the orthogonal matrix Q1 is returned; */
    /*             = 'U':  the array Q1 contains an orthogonal matrix Q01 on */
    /*                     entry, and the matrix Q01*Q1 is returned, where Q1 */
    /*                     is the product of the orthogonal transformations */
    /*                     that are applied on the right to the pencil */
    /*                     aA - bB in (1). */
    /*     COMPQ2  CHARACTER*1 */
    /*             Specifies whether to compute the orthogonal transformation */
    /*             matrix Q2, as follows: */
    /*             = 'N':  Q2 is not computed; */
    /*             = 'I':  the array Q2 is initialized internally to the unit */
    /*                     matrix, and the orthogonal matrix Q2 is returned; */
    /*             = 'U':  the array Q2 contains an orthogonal matrix Q02 on */
    /*                     entry, and the matrix Q02*Q2 is returned, where Q2 */
    /*                     is the product of the orthogonal transformations */
    /*                     that are applied on the left to the pencil aA - bB */
    /*                     in (1). */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             Order of the pencil aA - bB, N has to be even. */
    /*     A       (input/output) DOUBLE PRECISION array, dimension (LDA, N) */
    /*             On entry, the leading N-by-N block diagonal part of this */
    /*             array must contain the matrix A in (1). The off-diagonal */
    /*             blocks need not be set to zero. */
    /*             On exit, the leading N-by-N part of this array contains */
    /*             the transformed upper triangular matrix. */
    /*     LDA     INTEGER */
    /*             The leading dimension of the array A.  LDA >= MAX(1, N). */
    /*     B       (input/output) DOUBLE PRECISION array, dimension (LDB, N) */
    /*             On entry, the leading N-by-N block anti-diagonal part of */
    /*             this array must contain the matrix B in (1). The diagonal */
    /*             blocks need not be set to zero. */
    /*             On exit, the leading N-by-N part of this array contains */
    /*             the transformed upper quasi-triangular matrix. */
    /*     LDB     INTEGER */
    /*             The leading dimension of the array B.  LDB >= MAX(1, N). */
    /*     Q1      (input/output) DOUBLE PRECISION array, dimension (LDQ1, N) */
    /*             On entry, if COMPQ1 = 'U', then the leading N-by-N part of */
    /*             this array must contain a given matrix Q01, and on exit, */
    /*             the leading N-by-N part of this array contains the product */
    /*             of the input matrix Q01 and the transformation matrix Q1 */
    /*             used to transform the matrices A and B. */
    /*             On exit, if COMPQ1 = 'I', then the leading N-by-N part of */
    /*             this array contains the orthogonal transformation matrix */
    /*             Q1. */
    /*             If COMPQ1 = 'N' this array is not referenced. */
    /*     LDQ1    INTEGER */
    /*             The leading dimension of the array Q1. */
    /*             LDQ1 >= 1,         if COMPQ1 = 'N'; */
    /*             LDQ1 >= MAX(1, N), if COMPQ1 = 'I' or COMPQ1 = 'U'. */
    /*     Q2      (input/output) DOUBLE PRECISION array, dimension (LDQ2, N) */
    /*             On entry, if COMPQ2 = 'U', then the leading N-by-N part of */
    /*             this array must contain a given matrix Q02, and on exit, */
    /*             the leading N-by-N part of this array contains the product */
    /*             of the input matrix Q02 and the transformation matrix Q2 */
    /*             used to transform the matrices A and B. */
    /*             On exit, if COMPQ2 = 'I', then the leading N-by-N part of */
    /*             this array contains the orthogonal transformation matrix */
    /*             Q2. */
    /*             If COMPQ2 = 'N' this array is not referenced. */
    /*     LDQ2    INTEGER */
    /*             The leading dimension of the array Q2. */
    /*             LDQ2 >= 1,         if COMPQ2 = 'N'; */
    /*             LDQ2 >= MAX(1, N), if COMPQ2 = 'I' or COMPQ2 = 'U'. */
    /*     Workspace */
    /*     BWORK   LOGICAL array, dimension (N/2) */
    /*     IWORK   INTEGER array, dimension (LIWORK) */
    /*     LIWORK  INTEGER */
    /*             The dimension of the array IWORK. */
    /*             LIWORK >= N/2 + 32. */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             On exit, if INFO = 0, DWORK(1) returns the optimal LDWORK. */
    /*             On exit, if INFO = -16, DWORK(1) returns the minimum value */
    /*             of LDWORK. */
    /*     LDWORK  INTEGER */
    /*             The dimension of the array DWORK. */
    /*             LDWORK >= 2*N*N + MAX( N/2 + 168, 272 ). */
    /*             For good performance LDWORK should be generally larger. */
    /*             If LDWORK = -1  a workspace query is assumed; the */
    /*             routine only calculates the optimal size of the DWORK */
    /*             array, returns this value as the first entry of the DWORK */
    /*             array, and no error message is issued by XERBLA. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0: succesful exit; */
    /*             < 0: if INFO = -i, the i-th argument had an illegal value; */
    /*             = 1: the periodic QZ algorithm failed to reorder the */
    /*                  eigenvalues (the problem is very ill-conditioned) in */
    /*                  the SLICOT Library routine MB03KD; */
    /*             = 2: the standard QZ algorithm failed in the LAPACK */
    /*                  routine DGGEV, called by the SLICOT routine MB03DD; */
    /*             = 3: the standard QZ algorithm failed in the LAPACK */
    /*                  routines DGGES or DHGEQZ, called by the SLICOT */
    /*                  routines MB03DD or MB03FD; */
    /*             = 4: the standard QZ algorithm failed to reorder the */
    /*                  eigenvalues in the LAPACK routine DTGSEN, called by */
    /*                  the SLICOT routine MB03DD. */
    /*     METHOD */
    /*     First, the periodic QZ algorithm (see also [2] and [3]) is applied */
    /*                                     -1        -1 */
    /*     to the formal matrix product A11   B12 A22   B21 to reorder the */
    /*     eigenvalues, i.e., orthogonal matrices V1, V2, V3 and V4 are */
    /*     computed such that V2' A11 V1, V2' B12 V3, V4' A22 V3 and */
    /*     V4' B21 V1 keep the triangular form, but they can be partitioned */
    /*     into 2-by-2 block forms and the last diagonal blocks correspond to */
    /*     all nonpositive real eigenvalues of the formal product, and the */
    /*     first diagonal blocks correspond to the remaining eigenvalues. */
    /*     Second, Q1 = diag(V1, V3), Q2 = diag(V2, V4) and */
    /*                      ( AA11 AA12   0    0  ) */
    /*                      (                     ) */
    /*                      (   0  AA22   0    0  ) */
    /*     A := Q2' A Q1 =: (                     ), */
    /*                      (   0    0  AA33 AA34 ) */
    /*                      (                     ) */
    /*                      (   0    0    0  AA44 ) */
    /*                      (   0    0  BB13 BB14 ) */
    /*                      (                     ) */
    /*                      (   0    0    0  BB24 ) */
    /*     B := Q2' B Q1 =: (                     ), */
    /*                      ( BB31 BB32   0    0  ) */
    /*                      (                     ) */
    /*                      (   0  BB42   0    0  ) */
    /*                            -1          -1 */
    /*     are set, such that AA22   BB24 AA44   BB42 has only nonpositive */
    /*     real eigenvalues. */
    /*     Third, the permutation matrix */
    /*         (  I  0  0  0  ) */
    /*         (              ) */
    /*         (  0  0  I  0  ) */
    /*     P = (              ), */
    /*         (  0  I  0  0  ) */
    /*         (              ) */
    /*         (  0  0  0  I  ) */
    /*     where I denotes the identity matrix of appropriate size, is used */
    /*     to transform aA - bB to block upper triangular form */
    /*                   ( AA11   0  | AA12   0  ) */
    /*                   (           |           ) */
    /*                   (   0  AA33 |   0  AA34 )   ( AA1  *  ) */
    /*     A := P' A P = (-----------+-----------) = (         ), */
    /*                   (   0    0  | AA22   0  )   (  0  AA2 ) */
    /*                   (           |           ) */
    /*                   (   0    0  |   0  AA44 ) */
    /*                   (   0  BB13 |   0  BB14 ) */
    /*                   (           |           ) */
    /*                   ( BB31   0  | BB32   0  )   ( BB1  *  ) */
    /*     B := P' B P = (-----------+-----------) = (         ). */
    /*                   (   0    0  |   0  BB24 )   (  0  BB2 ) */
    /*                   (           |           ) */
    /*                   (   0    0  | BB42   0  ) */
    /*     Then, further orthogonal transformations that are provided by */
    /*     MB03FD and MB03DD are used to triangularize the subpencil */
    /*     aAA1 - bBB1. */
    /*     Finally, the subpencil aAA2 - bBB2 is triangularized by applying a */
    /*     special permutation matrix. */
    /*     See also page 31 in [1] for more details. */
    /*     REFERENCES */
    /*     [1] Benner, P., Byers, R., Losse, P., Mehrmann, V. and Xu, H. */
    /*         Numerical Solution of Real Skew-Hamiltonian/Hamiltonian */
    /*         Eigenproblems. */
    /*         Tech. Rep., Technical University Chemnitz, Germany, */
    /*         Nov. 2007. */
    /*     [2] Bojanczyk, A., Golub, G. H. and Van Dooren, P. */
    /*         The periodic Schur decomposition: algorithms and applications. */
    /*         In F.T. Luk (editor), Advanced Signal Processing Algorithms, */
    /*         Architectures, and Implementations III, Proc. SPIE Conference, */
    /*         vol. 1770, pp. 31-42, 1992. */
    /*     [3] Hench, J. J. and Laub, A. J. */
    /*         Numerical Solution of the discrete-time periodic Riccati */
    /*         equation. IEEE Trans. Automat. Control, 39, 1197-1210, 1994. */
    /*     NUMERICAL ASPECTS */
    /*                                                               3 */
    /*     The algorithm is numerically backward stable and needs O(N ) real */
    /*     floating point operations. */
    /*     CONTRIBUTOR */
    /*     Matthias Voigt, Fakultaet fuer Mathematik, Technische Universitaet */
    /*     Chemnitz, December 08, 2008. */
    /*     V. Sima, Dec. 2009 (SLICOT version of the routine DBTUMT). */
    /*     REVISIONS */
    /*     V. Sima, Aug. 2009, Feb. 2010, Jul. 2010, Sep.-Nov. 2010. */
    /*     KEYWORDS */
    /*     Eigenvalue reordering, matrix pencil, periodic QZ algorithm, */
    /*     upper (quasi-)triangular matrix. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
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
    --bwork;
    --iwork;
    --dwork;
    /* Function Body */
    m = *n / 2;
    mm = m * m;
    liniq1 = lsame_(compq1, "I", 1L, 1L);
    lupdq1 = lsame_(compq1, "U", 1L, 1L);
    liniq2 = lsame_(compq2, "I", 1L, 1L);
    lupdq2 = lsame_(compq2, "U", 1L, 1L);
    lcmpq1 = liniq1 || lupdq1;
    lcmpq2 = liniq2 || lupdq2;
    lquery = *ldwork == -1;
    /* Computing MAX */
    i__1 = m + 168;
    minwrk = (*n << 1) * *n + max(i__1, 272);
    /*     Test the input arguments. */
    *info = 0;
    if (!(lsame_(compq1, "N", 1L, 1L) || lcmpq1)) {
        *info = -1;
    } else if (!(lsame_(compq2, "N", 1L, 1L) || lcmpq2)) {
        *info = -2;
    } else if (*n < 0 || *n % 2 != 0) {
        *info = -3;
    } else if (*lda < max(1, *n)) {
        *info = -5;
    } else if (*ldb < max(1, *n)) {
        *info = -7;
    } else if (*ldq1 < 1 || lcmpq1 && *ldq1 < max(1, *n)) {
        *info = -9;
    } else if (*ldq2 < 1 || lcmpq2 && *ldq2 < max(1, *n)) {
        *info = -11;
    } else if (*liwork < m + 32) {
        *info = -14;
    } else if (!lquery && *ldwork < minwrk) {
        dwork[1] = (doublereal)minwrk;
        *info = -16;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("MB04HD", &i__1, 6L);
        return 0;
    } else {
        /*        Compute optimal workspace. */
        /* Computing MAX */
        i__1 = 1, i__2 = min(4, *n);
        i__ = max(i__1, i__2);
        dgges_("Vectors", "Vectors", "Sorted", sb02ow_, &i__, &a[a_offset], lda, &b[b_offset], ldb,
            idum, &dwork[1], &dwork[1], &dwork[1], &q1[q1_offset], &i__, &q2[q2_offset], &i__,
            &dwork[1], &c_n1, &bwork[1], info, 7L, 7L, 6L);
        dgges_("Vectors", "Vectors", "Not sorted", sb02ow_, &i__, &a[a_offset], lda, &b[b_offset],
            ldb, idum, &dwork[1], &dwork[1], &dwork[1], &q1[q1_offset], &i__, &q2[q2_offset], &i__,
            &dwork[2], &c_n1, &bwork[1], info, 7L, 7L, 10L);
        dggev_("No Vector", "No Vector", &c__2, &a[a_offset], lda, &b[b_offset], ldb, &dwork[1],
            &dwork[1], &dwork[1], dum, &c__1, dum, &c__1, &dwork[3], &c_n1, info, 9L, 9L);
        dtgsen_(&c__0, &c_true, &c_true, &bwork[1], &i__, &a[a_offset], lda, &b[b_offset], ldb,
            &dwork[1], &dwork[1], &dwork[1], &q1[q1_offset], &i__, &q2[q2_offset], &i__, idum,
            &tmp2, &tmp2, dum, &dwork[4], &c_n1, idum, &c__1, info);
        /* Computing MAX */
        /* Computing MAX */
        i__2 = (integer)dwork[1] + 12, i__3 = (m << 2) + 8, i__2 = max(i__2, i__3),
        i__3 = (integer)dwork[2] + 24, i__2 = max(i__2, i__3), i__3 = (integer)dwork[3] + 6,
        i__2 = max(i__2, i__3), i__3 = (integer)dwork[4] + 12, i__2 = max(i__2, i__3),
        i__3 = *n << 2;
        i__1 = max(i__2, i__3) + 64;
        optwrk = max(i__1, minwrk);
        if (lquery) {
            dwork[1] = (doublereal)optwrk;
            return 0;
        }
    }
    /*     Quick return if possible. */
    if (*n == 0) {
        dwork[1] = 1.;
        return 0;
    }
    /*     Computations. Note that MB03KD needs reverse ordering of the */
    /*     factors in the formal matrix product, compared to MA01BD, MB03BA. */
    ia11 = 1;
    ib12 = ia11 + mm;
    ia22 = ib12 + mm;
    ib21 = ia22 + mm;
    iv1 = ib21 + mm;
    iv2 = iv1 + mm;
    iv3 = iv2 + mm;
    iv4 = iv3 + mm;
    mp1 = m + 1;
    /*     Get the machine parameters. */
    ulp = dlamch_("Precision", 9L);
    base = dlamch_("Base", 4L);
    lgbas = log(base);
    /*     Compute maps to access the factors of the formal matrix product. */
    k = 4;
    kschur = 4;
    iwork[(k << 1) + 1] = -1;
    iwork[(k << 1) + 2] = 1;
    iwork[(k << 1) + 3] = -1;
    iwork[(k << 1) + 4] = 1;
    mb03ba_(&k, &kschur, &iwork[(k << 1) + 1], &i__, &iwork[1], &iwork[k + 1]);
    /*     Store the factors of the formal matrix product. */
    dum[0] = 0.;
    i__1 = mm << 2;
    dcopy_(&i__1, dum, &c__0, &dwork[1], &c__1);
    dlacpy_("Upper", &m, &m, &a[a_offset], lda, &dwork[1], &m, 5L);
    dlacpy_("Upper", &m, &m, &a[mp1 + mp1 * a_dim1], lda, &dwork[ia22], &m, 5L);
    dlacpy_("Upper", &m, &m, &b[mp1 * b_dim1 + 1], ldb, &dwork[ib12], &m, 5L);
    dlacpy_("Upper", &m, &m, &b[mp1 + b_dim1], ldb, &dwork[ib21], &m, 5L);
    if (m > 1) {
        i__1 = m - 1;
        i__2 = *ldb + 1;
        dcopy_(&i__1, &b[m + 2 + b_dim1], &i__2, &dwork[ib21 + 1], &mp1);
    }
    /*     Set BWORK according to the eigenvalues of the formal matrix */
    /*     product in Schur-triangular form. */
    /*     Workspace:   need   4*M*M + 2. */
    j = 1;
    ia = iv1;
    ib = ia + 1;
    /*     WHILE( J.LE.M ) DO */
L10:
    if (j < m) {
        if (dwork[ib21 + j + (j - 1) * m] == 0.) {
            ma01bd_(&base, &lgbas, &k, &iwork[(k << 1) + 1], &dwork[(j - 1) * m + j], &mm,
                &dwork[ia], &dwork[ib], &iwork[k * 3 + 1]);
            bwork[j] = dwork[ia] > 0.;
            ++j;
            goto L10;
        } else {
            bwork[j] = TRUE_;
            bwork[j + 1] = TRUE_;
            j += 2;
            goto L10;
        }
    } else if (j == m) {
        ma01bd_(&base, &lgbas, &k, &iwork[(k << 1) + 1], &dwork[mm], &mm, &dwork[ia], &dwork[ib],
            &iwork[k * 3 + 1]);
        bwork[j] = dwork[ia] > 0.;
    }
    /*     END WHILE 10 */
    /*     Check if BWORK(J) = .TRUE. for all J. */
    j = 1;
    /*     WHILE( J.LE.M and BWORK(J) ) DO */
L20:
    if (j <= m && bwork[j]) {
        ++j;
        goto L20;
    }
    /*     END WHILE 20 */
    if (j != mp1) {
        /*        Apply periodic QZ algorithm for reordering the eigenvalues. */
        /*        Workspace:   need   8*M*M + MAX(42*K + M, 80*K - 48), K = 4, */
        /*                     if there is at least a pair of adjacent blocks */
        /*                     of order 2 involved in reordering, and M > 10. */
        /*                     Otherwise, the MAX term is slightly smaller. */
        iwrk = (iv1 << 1) - 1;
        ib21 = 1;
        ia22 = ib21 + mm;
        ib12 = ia22 + mm;
        ia11 = ib12 + mm;
        kschur = 1;
        iwork[(k << 1) + 1] = 1;
        iwork[(k << 1) + 2] = -1;
        iwork[(k << 1) + 3] = 1;
        iwork[(k << 1) + 4] = -1;
        i__1 = k;
        for (i__ = 1; i__ <= i__1; ++i__) {
            iwork[i__] = m;
            iwork[k + i__] = 0;
            iwork[k * 3 + i__] = (i__ - 1) * mm + 1;
            /* L30: */
        }
        i__1 = mm * k;
        dcopy_(&i__1, dum, &c__0, &dwork[ib21], &c__1);
        dlacpy_("Upper", &m, &m, &b[mp1 + b_dim1], ldb, &dwork[ib21], &m, 5L);
        dlacpy_("Upper", &m, &m, &b[mp1 * b_dim1 + 1], ldb, &dwork[ib12], &m, 5L);
        dlacpy_("Upper", &m, &m, &a[a_offset], lda, &dwork[ia11], &m, 5L);
        dlacpy_("Upper", &m, &m, &a[mp1 + mp1 * a_dim1], lda, &dwork[ia22], &m, 5L);
        if (m > 1) {
            i__1 = m - 1;
            i__2 = *ldb + 1;
            dcopy_(&i__1, &b[m + 2 + b_dim1], &i__2, &dwork[ib21 + 1], &mp1);
        }
        i__1 = *ldwork - iwrk + 1;
        mb03kd_("Initialize", idum, "NotStrong", &k, &m, &kschur, &iwork[1], &iwork[k + 1],
            &iwork[(k << 1) + 1], &bwork[1], &dwork[1], &iwork[1], &iwork[k * 3 + 1], &dwork[iv1],
            &iwork[1], &iwork[k * 3 + 1], &m1, &c_b47, &iwork[(k << 2) + 1], &dwork[iwrk], &i__1,
            info, 10L, 9L);
        if (*info > 0) {
            return 0;
        }
        m2 = m - m1;
        i1 = m1 + 1;
        i2 = i1 + m1;
        i3 = i2 + m2;
        m4 = m2 << 1;
        /*        If Q1 and/or Q2 are user-initialized, update them. */
        /*        The (2,1) block of A is used as workspace. */
        if (lupdq1) {
            dgemm_("No Transpose", "No Transpose", &m, &m, &m, &c_b50, &q1[q1_offset], ldq1,
                &dwork[iv1], &m, &c_b51, &a[mp1 + a_dim1], lda, 12L, 12L);
            dlacpy_("Full", &m, &m, &a[mp1 + a_dim1], lda, &q1[q1_offset], ldq1, 4L);
            dgemm_("No Transpose", "No Transpose", &m, &m, &m, &c_b50, &q1[mp1 + q1_dim1], ldq1,
                &dwork[iv1], &m, &c_b51, &a[mp1 + a_dim1], lda, 12L, 12L);
            dlacpy_("Full", &m, &m, &a[mp1 + a_dim1], lda, &q1[mp1 + q1_dim1], ldq1, 4L);
            dgemm_("No Transpose", "No Transpose", &m, &m, &m, &c_b50, &q1[mp1 * q1_dim1 + 1], ldq1,
                &dwork[iv3], &m, &c_b51, &a[mp1 + a_dim1], lda, 12L, 12L);
            dlacpy_("Full", &m, &m, &a[mp1 + a_dim1], lda, &q1[mp1 * q1_dim1 + 1], ldq1, 4L);
            dgemm_("No Transpose", "No Transpose", &m, &m, &m, &c_b50, &q1[mp1 + mp1 * q1_dim1],
                ldq1, &dwork[iv3], &m, &c_b51, &a[mp1 + a_dim1], lda, 12L, 12L);
            dlacpy_("Full", &m, &m, &a[mp1 + a_dim1], lda, &q1[mp1 + mp1 * q1_dim1], ldq1, 4L);
            if (m2 > 0) {
                dlacpy_("Full", &m, &m, &q1[i1 * q1_dim1 + 1], ldq1, &a[mp1 + a_dim1], lda, 4L);
                dlacpy_("Full", &m, &m1, &a[mp1 + (m2 + 1) * a_dim1], lda, &q1[i1 * q1_dim1 + 1],
                    ldq1, 4L);
                dlacpy_("Full", &m, &m2, &a[mp1 + a_dim1], lda, &q1[i2 * q1_dim1 + 1], ldq1, 4L);
                dlacpy_("Full", &m, &m, &q1[mp1 + i1 * q1_dim1], ldq1, &a[mp1 + a_dim1], lda, 4L);
                dlacpy_("Full", &m, &m1, &a[mp1 + (m2 + 1) * a_dim1], lda, &q1[mp1 + i1 * q1_dim1],
                    ldq1, 4L);
                dlacpy_("Full", &m, &m2, &a[mp1 + a_dim1], lda, &q1[mp1 + i2 * q1_dim1], ldq1, 4L);
            }
        }
        if (lupdq2) {
            dgemm_("No Transpose", "No Transpose", &m, &m, &m, &c_b50, &q2[q2_offset], ldq2,
                &dwork[iv4], &m, &c_b51, &a[mp1 + a_dim1], lda, 12L, 12L);
            dlacpy_("Full", &m, &m, &a[mp1 + a_dim1], lda, &q2[q2_offset], ldq2, 4L);
            dgemm_("No Transpose", "No Transpose", &m, &m, &m, &c_b50, &q2[mp1 + q2_dim1], ldq2,
                &dwork[iv4], &m, &c_b51, &a[mp1 + a_dim1], lda, 12L, 12L);
            dlacpy_("Full", &m, &m, &a[mp1 + a_dim1], lda, &q2[mp1 + q2_dim1], ldq2, 4L);
            dgemm_("No Transpose", "No Transpose", &m, &m, &m, &c_b50, &q2[mp1 * q2_dim1 + 1], ldq2,
                &dwork[iv2], &m, &c_b51, &a[mp1 + a_dim1], lda, 12L, 12L);
            dlacpy_("Full", &m, &m, &a[mp1 + a_dim1], lda, &q2[mp1 * q2_dim1 + 1], ldq2, 4L);
            dgemm_("No Transpose", "No Transpose", &m, &m, &m, &c_b50, &q2[mp1 + mp1 * q2_dim1],
                ldq2, &dwork[iv2], &m, &c_b51, &a[mp1 + a_dim1], lda, 12L, 12L);
            dlacpy_("Full", &m, &m, &a[mp1 + a_dim1], lda, &q2[mp1 + mp1 * q2_dim1], ldq2, 4L);
            if (m2 > 0) {
                dlacpy_("Full", &m, &m, &q2[i1 * q2_dim1 + 1], ldq2, &a[mp1 + a_dim1], lda, 4L);
                dlacpy_("Full", &m, &m1, &a[mp1 + (m2 + 1) * a_dim1], lda, &q2[i1 * q2_dim1 + 1],
                    ldq2, 4L);
                dlacpy_("Full", &m, &m2, &a[mp1 + a_dim1], lda, &q2[i2 * q2_dim1 + 1], ldq2, 4L);
                dlacpy_("Full", &m, &m, &q2[mp1 + i1 * q2_dim1], ldq2, &a[mp1 + a_dim1], lda, 4L);
                dlacpy_("Full", &m, &m1, &a[mp1 + (m2 + 1) * a_dim1], lda, &q2[mp1 + i1 * q2_dim1],
                    ldq2, 4L);
                dlacpy_("Full", &m, &m2, &a[mp1 + a_dim1], lda, &q2[mp1 + i2 * q2_dim1], ldq2, 4L);
            }
        }
        /*        Make permutations of the corresponding matrices. */
        if (m2 > 0) {
            dlaset_("Full", &m, &m, &c_b51, &c_b51, &a[mp1 + a_dim1], lda, 4L);
            dlacpy_("Upper", &m1, &m1, &dwork[ia11], &m, &a[a_offset], lda, 5L);
            dlaset_("Full", &m1, &m1, &c_b51, &c_b51, &a[i1 * a_dim1 + 1], lda, 4L);
            dlacpy_("Upper", &m1, &m1, &dwork[ia22], &m, &a[i1 + i1 * a_dim1], lda, 5L);
            dlacpy_("Full", &m1, &m2, &dwork[ia11 + m * m1], &m, &a[i2 * a_dim1 + 1], lda, 4L);
            dlaset_("Full", &m1, &m2, &c_b51, &c_b51, &a[i1 + i2 * a_dim1], lda, 4L);
            dlacpy_(
                "Upper", &m2, &m2, &dwork[ia11 + m * m1 + m1], &m, &a[i2 + i2 * a_dim1], lda, 5L);
            dlaset_("Full", &m1, &m2, &c_b51, &c_b51, &a[i3 * a_dim1 + 1], lda, 4L);
            dlacpy_("Full", &m1, &m2, &dwork[ia22 + m * m1], &m, &a[i1 + i3 * a_dim1], lda, 4L);
            dlaset_("Full", &m2, &m2, &c_b51, &c_b51, &a[i2 + i3 * a_dim1], lda, 4L);
            dlacpy_(
                "Upper", &m2, &m2, &dwork[ia22 + m * m1 + m1], &m, &a[i3 + i3 * a_dim1], lda, 5L);
            dlaset_("Full", &m1, &m1, &c_b51, &c_b51, &b[b_offset], ldb, 4L);
            dlacpy_("Upper", &m1, &m1, &dwork[ib21], &m, &b[i1 + b_dim1], ldb, 5L);
            i__1 = m1 - 1;
            i__2 = *ldb + 1;
            dcopy_(&i__1, &dwork[ib21 + 1], &mp1, &b[i1 + 1 + b_dim1], &i__2);
            if (m1 > 2) {
                i__1 = m1 - 2;
                i__2 = m1 - 2;
                dlaset_("Lower", &i__1, &i__2, &c_b51, &c_b51, &b[i1 + 2 + b_dim1], ldb, 5L);
            }
            dlaset_("Full", &m4, &m1, &c_b51, &c_b51, &b[i2 + b_dim1], ldb, 4L);
            dlacpy_("Upper", &m1, &m1, &dwork[ib12], &m, &b[i1 * b_dim1 + 1], ldb, 5L);
            if (m1 > 1) {
                i__1 = m1 - 1;
                i__2 = m1 - 1;
                dlaset_("Lower", &i__1, &i__2, &c_b51, &c_b51, &b[i1 * b_dim1 + 2], ldb, 5L);
            }
            i__1 = *n - m1;
            dlaset_("Full", &i__1, &m1, &c_b51, &c_b51, &b[i1 + i1 * b_dim1], ldb, 4L);
            dlaset_("Full", &m1, &m2, &c_b51, &c_b51, &b[i2 * b_dim1 + 1], ldb, 4L);
            dlacpy_("Full", &m1, &m2, &dwork[ib21 + m * m1], &m, &b[i1 + i2 * b_dim1], ldb, 4L);
            dlaset_("Full", &m2, &m2, &c_b51, &c_b51, &b[i2 + i2 * b_dim1], ldb, 4L);
            dlacpy_(
                "Upper", &m2, &m2, &dwork[ib21 + m * m1 + m1], &m, &b[i3 + i2 * b_dim1], ldb, 5L);
            i__1 = m2 - 1;
            i__2 = *ldb + 1;
            dcopy_(&i__1, &dwork[ib21 + m * m1 + i1], &mp1, &b[i3 + 1 + i2 * b_dim1], &i__2);
            if (m2 > 2) {
                i__1 = m2 - 2;
                i__2 = m2 - 2;
                dlaset_("Lower", &i__1, &i__2, &c_b51, &c_b51, &b[i3 + 2 + i2 * b_dim1], ldb, 5L);
            }
            dlacpy_("Full", &m1, &m2, &dwork[ib12 + m * m1], &m, &b[i3 * b_dim1 + 1], ldb, 4L);
            dlaset_("Full", &m1, &m2, &c_b51, &c_b51, &b[i1 + i3 * b_dim1], ldb, 4L);
            dlacpy_(
                "Full", &m2, &m2, &dwork[ib12 + m * m1 + m1], &m, &b[i2 + i3 * b_dim1], ldb, 4L);
            dlaset_("Full", &m2, &m2, &c_b51, &c_b51, &b[i3 + i3 * b_dim1], ldb, 4L);
        } else {
            dlaset_("Full", &m, &m, &c_b51, &c_b51, &a[mp1 + a_dim1], lda, 4L);
            dlaset_("Full", &m, &m, &c_b51, &c_b51, &a[mp1 * a_dim1 + 1], lda, 4L);
            dlaset_("Full", &m, &m, &c_b51, &c_b51, &b[b_offset], ldb, 4L);
            dlaset_("Full", &m, &m, &c_b51, &c_b51, &b[mp1 + mp1 * b_dim1], ldb, 4L);
        }
        if (liniq1) {
            dlacpy_("Full", &m, &m1, &dwork[iv1], &m, &q1[q1_offset], ldq1, 4L);
            dlaset_("Full", &m, &m1, &c_b51, &c_b51, &q1[mp1 + q1_dim1], ldq1, 4L);
            dlaset_("Full", &m, &m1, &c_b51, &c_b51, &q1[i1 * q1_dim1 + 1], ldq1, 4L);
            dlacpy_("Full", &m, &m1, &dwork[iv3], &m, &q1[mp1 + i1 * q1_dim1], ldq1, 4L);
            if (m2 > 0) {
                dlacpy_("Full", &m, &m2, &dwork[iv1 + m * m1], &m, &q1[i2 * q1_dim1 + 1], ldq1, 4L);
                dlaset_("Full", &m, &m2, &c_b51, &c_b51, &q1[mp1 + i2 * q1_dim1], ldq1, 4L);
                dlaset_("Full", &m, &m2, &c_b51, &c_b51, &q1[i3 * q1_dim1 + 1], ldq1, 4L);
                dlacpy_(
                    "Full", &m, &m2, &dwork[iv3 + m * m1], &m, &q1[mp1 + i3 * q1_dim1], ldq1, 4L);
            }
        }
        if (liniq2) {
            dlacpy_("Full", &m, &m1, &dwork[iv4], &m, &q2[q2_offset], ldq2, 4L);
            dlaset_("Full", &m, &m1, &c_b51, &c_b51, &q2[mp1 + q2_dim1], ldq2, 4L);
            dlaset_("Full", &m, &m1, &c_b51, &c_b51, &q2[i1 * q2_dim1 + 1], ldq2, 4L);
            dlacpy_("Full", &m, &m1, &dwork[iv2], &m, &q2[mp1 + i1 * q2_dim1], ldq2, 4L);
            if (m2 > 0) {
                dlacpy_("Full", &m, &m2, &dwork[iv4 + m * m1], &m, &q2[i2 * q2_dim1 + 1], ldq2, 4L);
                dlaset_("Full", &m, &m2, &c_b51, &c_b51, &q2[mp1 + i2 * q2_dim1], ldq2, 4L);
                dlaset_("Full", &m, &m2, &c_b51, &c_b51, &q2[i3 * q2_dim1 + 1], ldq2, 4L);
                dlacpy_(
                    "Full", &m, &m2, &dwork[iv2 + m * m1], &m, &q2[mp1 + i3 * q2_dim1], ldq2, 4L);
            }
        }
    } else {
        m1 = m;
        m2 = 0;
        i1 = m1 + 1;
        i2 = i1 + m1;
        i3 = i2;
        m4 = m2 << 1;
        dlaset_("Full", &m, &m, &c_b51, &c_b51, &a[mp1 + a_dim1], lda, 4L);
        dlaset_("Full", &m, &m, &c_b51, &c_b51, &a[mp1 * a_dim1 + 1], lda, 4L);
        dlaset_("Full", &m, &m, &c_b51, &c_b51, &b[b_offset], ldb, 4L);
        dlaset_("Full", &m, &m, &c_b51, &c_b51, &b[mp1 + mp1 * b_dim1], ldb, 4L);
        if (liniq1) {
            dlaset_("Full", n, n, &c_b51, &c_b50, &q1[q1_offset], ldq1, 4L);
        }
        if (liniq2) {
            dlaset_("Full", n, n, &c_b51, &c_b50, &q2[q2_offset], ldq2, 4L);
        }
    }
    /*     Count the number of blocks in BB31. */
    r__ = 0;
    j = 1;
    /*     WHILE( J.LE.M1 ) DO */
L40:
    if (j < m1) {
        ++r__;
        iwork[r__] = j;
        if (b[m1 + j + 1 + j * b_dim1] == 0.) {
            ++j;
        } else {
            j += 2;
        }
        goto L40;
    } else if (j == m1) {
        ++r__;
        iwork[r__] = j;
        ++j;
    }
    /*     END WHILE 40 */
    iwork[r__ + 1] = j;
    /*     Triangularize the upper left subpencil aAA1 - bBB1. */
    i__1 = r__;
    for (i__ = 1; i__ <= i__1; ++i__) {
        /*        Calculate position of submatrices in DWORK. */
        /*        IB1 and IB2 are pointers to 2 consecutive blocks. */
        ib1 = iwork[i__];
        ib2 = iwork[i__ + 1];
        dim1 = ib2 - ib1;
        sdim = dim1 << 1;
        iauple = 1;
        ialole = iauple + dim1;
        iaupri = dim1 * sdim + 1;
        ialori = iaupri + dim1;
        ibuple = sdim * sdim + 1;
        iblole = ibuple + dim1;
        ibupri = dim1 * 3 * sdim + 1;
        iblori = ibupri + dim1;
        i1uple = (sdim << 1) * sdim + 1;
        i1lole = i1uple + dim1;
        i1upri = dim1 * 5 * sdim + 1;
        i1lori = i1upri + dim1;
        i2uple = sdim * 3 * sdim + 1;
        i2lole = i2uple + dim1;
        i2upri = dim1 * 7 * sdim + 1;
        i2lori = i2upri + dim1;
        /*        Generate input matrices for MB03FD, built of submatrices of A */
        /*        and B. */
        /*        Workspace:   need    32. */
        if (dim1 == 1) {
            i__2 = (*lda + 1) * m1;
            i__3 = sdim + 1;
            dcopy_(&sdim, &a[ib1 + ib1 * a_dim1], &i__2, &dwork[iauple], &i__3);
            i__2 = (*ldb - 1) * m1;
            dcopy_(&sdim, &b[m1 + ib1 + ib1 * b_dim1], &i__2, &dwork[iblole], &c__1);
        } else {
            dlacpy_("Upper", &dim1, &dim1, &a[ib1 + ib1 * a_dim1], lda, &dwork[iauple], &sdim, 5L);
            i__2 = sdim - 1;
            i__3 = sdim - 1;
            dlaset_("Lower", &i__2, &i__3, &c_b51, &c_b51, &dwork[iauple + 1], &sdim, 5L);
            dlaset_("Full", &dim1, &dim1, &c_b51, &c_b51, &dwork[iaupri], &sdim, 4L);
            dlacpy_("Upper", &dim1, &dim1, &a[m1 + ib1 + (m1 + ib1) * a_dim1], lda, &dwork[ialori],
                &sdim, 5L);
            dwork[ialori + 1] = 0.;
            dlaset_("Full", &dim1, &dim1, &c_b51, &c_b51, &dwork[ibuple], &sdim, 4L);
            dlacpy_(
                "Full", &dim1, &dim1, &b[m1 + ib1 + ib1 * b_dim1], ldb, &dwork[iblole], &sdim, 4L);
            dlacpy_("Full", &dim1, &dim1, &b[ib1 + (m1 + ib1) * b_dim1], ldb, &dwork[ibupri], &sdim,
                4L);
            dlaset_("Full", &dim1, &dim1, &c_b51, &c_b51, &dwork[iblori], &sdim, 4L);
        }
        /*        Perform eigenvalue exchange. */
        /*        Workspace:   need   64 + max( 63, 4*M + 8 ). */
        iwrk = (sdim << 2) * sdim + 1;
        itmp = iwrk + m * dim1;
        itmp2 = itmp + m * dim1;
        itmp3 = itmp2 + dim1 * dim1;
        i__2 = *ldwork - iwrk + 1;
        mb03fd_(&sdim, &ulp, &dwork[iauple], &sdim, &dwork[ibuple], &sdim, &dwork[i1uple], &sdim,
            &dwork[i2uple], &sdim, &dwork[iwrk], &i__2, info);
        if (*info > 0) {
            *info = 3;
            return 0;
        }
        nr = ib2 - 1;
        if (dim1 == 2) {
            /*           Update A. */
            dlacpy_("Full", &nr, &dim1, &a[ib1 * a_dim1 + 1], lda, &dwork[iwrk], &nr, 4L);
            dgemm_("No Transpose", "No Transpose", &nr, &dim1, &dim1, &c_b50, &dwork[iwrk], &nr,
                &dwork[i1uple], &sdim, &c_b51, &a[ib1 * a_dim1 + 1], lda, 12L, 12L);
            dgemm_("No Transpose", "No Transpose", &nr, &dim1, &dim1, &c_b50,
                &a[(m1 + ib1) * a_dim1 + 1], lda, &dwork[i1lole], &sdim, &c_b50,
                &a[ib1 * a_dim1 + 1], lda, 12L, 12L);
            dgemm_("No Transpose", "No Transpose", &nr, &dim1, &dim1, &c_b50, &dwork[iwrk], &nr,
                &dwork[i1upri], &sdim, &c_b51, &dwork[itmp], &nr, 12L, 12L);
            dgemm_("No Transpose", "No Transpose", &nr, &dim1, &dim1, &c_b50,
                &a[(m1 + ib1) * a_dim1 + 1], lda, &dwork[i1lori], &sdim, &c_b50, &dwork[itmp], &nr,
                12L, 12L);
            dlacpy_("Full", &nr, &dim1, &dwork[itmp], &nr, &a[(m1 + ib1) * a_dim1 + 1], lda, 4L);
            dlacpy_("Full", &nr, &dim1, &a[i1 + ib1 * a_dim1], lda, &dwork[iwrk], &nr, 4L);
            dgemm_("No Transpose", "No Transpose", &nr, &dim1, &dim1, &c_b50, &dwork[iwrk], &nr,
                &dwork[i1uple], &sdim, &c_b51, &a[i1 + ib1 * a_dim1], lda, 12L, 12L);
            dgemm_("No Transpose", "No Transpose", &nr, &dim1, &dim1, &c_b50,
                &a[i1 + (m1 + ib1) * a_dim1], lda, &dwork[i1lole], &sdim, &c_b50,
                &a[i1 + ib1 * a_dim1], lda, 12L, 12L);
            dgemm_("No Transpose", "No Transpose", &nr, &dim1, &dim1, &c_b50, &dwork[iwrk], &nr,
                &dwork[i1upri], &sdim, &c_b51, &dwork[itmp], &nr, 12L, 12L);
            dgemm_("No Transpose", "No Transpose", &nr, &dim1, &dim1, &c_b50,
                &a[i1 + (m1 + ib1) * a_dim1], lda, &dwork[i1lori], &sdim, &c_b50, &dwork[itmp], &nr,
                12L, 12L);
            dlacpy_("Full", &nr, &dim1, &dwork[itmp], &nr, &a[i1 + (m1 + ib1) * a_dim1], lda, 4L);
            dlacpy_(
                "Full", &dim1, &dim1, &a[m1 + ib1 + ib1 * a_dim1], lda, &dwork[itmp2], &dim1, 4L);
            dlacpy_(
                "Full", &dim1, &dim1, &a[ib1 + (m1 + ib1) * a_dim1], lda, &dwork[itmp3], &dim1, 4L);
            dlaset_("Full", &dim1, &dim1, &c_b51, &c_b51, &a[m1 + ib1 + ib1 * a_dim1], lda, 4L);
            i__2 = m1 - ib2 + 1;
            dgemm_("Transpose", "No Transpose", &dim1, &i__2, &dim1, &c_b50, &dwork[i2upri], &sdim,
                &a[ib1 + ib2 * a_dim1], lda, &c_b51, &a[m1 + ib1 + ib2 * a_dim1], lda, 9L, 12L);
            i__2 = m1 - ib1 + 1;
            dgemm_("Transpose", "No Transpose", &dim1, &i__2, &dim1, &c_b50, &dwork[i2uple], &sdim,
                &a[ib1 + ib1 * a_dim1], lda, &c_b51, &dwork[itmp], &dim1, 9L, 12L);
            dgemm_("Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b50, &dwork[i2lole], &sdim,
                &dwork[itmp2], &dim1, &c_b50, &dwork[itmp], &dim1, 9L, 12L);
            i__2 = m1 - ib1 + 1;
            dlacpy_("Full", &dim1, &i__2, &dwork[itmp], &dim1, &a[ib1 + ib1 * a_dim1], lda, 4L);
            i__2 = m1 - ib1 + 1;
            dgemm_("Transpose", "No Transpose", &dim1, &i__2, &dim1, &c_b50, &dwork[i2lole], &sdim,
                &a[m1 + ib1 + (m1 + ib1) * a_dim1], lda, &c_b51, &a[ib1 + (m1 + ib1) * a_dim1], lda,
                9L, 12L);
            dgemm_("Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b50, &dwork[i2uple], &sdim,
                &dwork[itmp3], &dim1, &c_b50, &a[ib1 + (m1 + ib1) * a_dim1], lda, 9L, 12L);
            i__2 = m1 - ib1 + 1;
            dgemm_("Transpose", "No Transpose", &dim1, &i__2, &dim1, &c_b50, &dwork[i2lori], &sdim,
                &a[m1 + ib1 + (m1 + ib1) * a_dim1], lda, &c_b51, &dwork[itmp], &dim1, 9L, 12L);
            dgemm_("Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b50, &dwork[i2upri], &sdim,
                &dwork[itmp3], &dim1, &c_b50, &dwork[itmp], &dim1, 9L, 12L);
            i__2 = m1 - ib1 + 1;
            dlacpy_("Full", &dim1, &i__2, &dwork[itmp], &dim1, &a[m1 + ib1 + (m1 + ib1) * a_dim1],
                lda, 4L);
            if (m2 > 0) {
                dgemm_("Transpose", "No Transpose", &dim1, &m2, &dim1, &c_b50, &dwork[i2upri],
                    &sdim, &a[ib1 + i2 * a_dim1], lda, &c_b51, &a[m1 + ib1 + i2 * a_dim1], lda, 9L,
                    12L);
                dgemm_("Transpose", "No Transpose", &dim1, &m2, &dim1, &c_b50, &dwork[i2uple],
                    &sdim, &a[ib1 + i2 * a_dim1], lda, &c_b51, &dwork[itmp], &dim1, 9L, 12L);
                dlacpy_("Full", &dim1, &m2, &dwork[itmp], &dim1, &a[ib1 + i2 * a_dim1], lda, 4L);
                dgemm_("Transpose", "No Transpose", &dim1, &m2, &dim1, &c_b50, &dwork[i2lole],
                    &sdim, &a[m1 + ib1 + i3 * a_dim1], lda, &c_b51, &a[ib1 + i3 * a_dim1], lda, 9L,
                    12L);
                dgemm_("Transpose", "No Transpose", &dim1, &m2, &dim1, &c_b50, &dwork[i2lori],
                    &sdim, &a[m1 + ib1 + i3 * a_dim1], lda, &c_b51, &dwork[itmp], &dim1, 9L, 12L);
                dlacpy_(
                    "Full", &dim1, &m2, &dwork[itmp], &dim1, &a[m1 + ib1 + i3 * a_dim1], lda, 4L);
            }
            /*           Update B. */
            dlacpy_("Full", &nr, &dim1, &b[ib1 * b_dim1 + 1], ldb, &dwork[iwrk], &nr, 4L);
            dgemm_("No Transpose", "No Transpose", &nr, &dim1, &dim1, &c_b50, &dwork[iwrk], &nr,
                &dwork[i1uple], &sdim, &c_b51, &b[ib1 * b_dim1 + 1], ldb, 12L, 12L);
            dgemm_("No Transpose", "No Transpose", &nr, &dim1, &dim1, &c_b50,
                &b[(m1 + ib1) * b_dim1 + 1], ldb, &dwork[i1lole], &sdim, &c_b50,
                &b[ib1 * b_dim1 + 1], ldb, 12L, 12L);
            dgemm_("No Transpose", "No Transpose", &nr, &dim1, &dim1, &c_b50, &dwork[iwrk], &nr,
                &dwork[i1upri], &sdim, &c_b51, &dwork[itmp], &nr, 12L, 12L);
            dgemm_("No Transpose", "No Transpose", &nr, &dim1, &dim1, &c_b50,
                &b[(m1 + ib1) * b_dim1 + 1], ldb, &dwork[i1lori], &sdim, &c_b50, &dwork[itmp], &nr,
                12L, 12L);
            dlacpy_("Full", &nr, &dim1, &dwork[itmp], &nr, &b[(m1 + ib1) * b_dim1 + 1], ldb, 4L);
            dlacpy_("Full", &nr, &dim1, &b[i1 + ib1 * b_dim1], ldb, &dwork[iwrk], &nr, 4L);
            dgemm_("No Transpose", "No Transpose", &nr, &dim1, &dim1, &c_b50, &dwork[iwrk], &nr,
                &dwork[i1uple], &sdim, &c_b51, &b[i1 + ib1 * b_dim1], ldb, 12L, 12L);
            dgemm_("No Transpose", "No Transpose", &nr, &dim1, &dim1, &c_b50,
                &b[i1 + (m1 + ib1) * b_dim1], ldb, &dwork[i1lole], &sdim, &c_b50,
                &b[i1 + ib1 * b_dim1], ldb, 12L, 12L);
            dgemm_("No Transpose", "No Transpose", &nr, &dim1, &dim1, &c_b50, &dwork[iwrk], &nr,
                &dwork[i1upri], &sdim, &c_b51, &dwork[itmp], &nr, 12L, 12L);
            dgemm_("No Transpose", "No Transpose", &nr, &dim1, &dim1, &c_b50,
                &b[i1 + (m1 + ib1) * b_dim1], ldb, &dwork[i1lori], &sdim, &c_b50, &dwork[itmp], &nr,
                12L, 12L);
            dlacpy_("Full", &nr, &dim1, &dwork[itmp], &nr, &b[i1 + (m1 + ib1) * b_dim1], ldb, 4L);
            dlacpy_("Full", &dim1, &dim1, &b[ib1 + ib1 * b_dim1], ldb, &dwork[itmp2], &dim1, 4L);
            dlacpy_("Full", &dim1, &dim1, &b[m1 + ib1 + (m1 + ib1) * b_dim1], ldb, &dwork[itmp3],
                &dim1, 4L);
            i__2 = m1 - ib1 + 1;
            dgemm_("Transpose", "No Transpose", &dim1, &i__2, &dim1, &c_b50, &dwork[i2lole], &sdim,
                &b[m1 + ib1 + ib1 * b_dim1], ldb, &c_b51, &b[ib1 + ib1 * b_dim1], ldb, 9L, 12L);
            dgemm_("Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b50, &dwork[i2uple], &sdim,
                &dwork[itmp2], &dim1, &c_b50, &b[ib1 + ib1 * b_dim1], ldb, 9L, 12L);
            dlaset_("Full", &dim1, &dim1, &c_b51, &c_b51, &b[m1 + ib1 + ib1 * b_dim1], ldb, 4L);
            i__2 = m1 - ib1 + 1;
            dgemm_("Transpose", "No Transpose", &dim1, &i__2, &dim1, &c_b50, &dwork[i2lori], &sdim,
                &b[m1 + ib1 + (ib1 + 1) * b_dim1], ldb, &c_b51, &dwork[itmp], &dim1, 9L, 12L);
            i__2 = m1 - ib1 + 1;
            dlacpy_("Full", &dim1, &i__2, &dwork[itmp], &dim1, &b[m1 + ib1 + (ib1 + 1) * b_dim1],
                ldb, 4L);
            i__2 = m1 - ib1 + 1;
            dgemm_("Transpose", "No Transpose", &dim1, &i__2, &dim1, &c_b50, &dwork[i2upri], &sdim,
                &b[ib1 + (m1 + ib1) * b_dim1], ldb, &c_b51, &b[m1 + ib1 + (m1 + ib1) * b_dim1], ldb,
                9L, 12L);
            dgemm_("Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b50, &dwork[i2lori], &sdim,
                &dwork[itmp3], &dim1, &c_b50, &b[m1 + ib1 + (m1 + ib1) * b_dim1], ldb, 9L, 12L);
            i__2 = m1 - ib1 + 1;
            dgemm_("Transpose", "No Transpose", &dim1, &i__2, &dim1, &c_b50, &dwork[i2uple], &sdim,
                &b[ib1 + (m1 + ib1) * b_dim1], ldb, &c_b51, &dwork[itmp], &dim1, 9L, 12L);
            dgemm_("Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b50, &dwork[i2lole], &sdim,
                &dwork[itmp3], &dim1, &c_b50, &dwork[itmp], &dim1, 9L, 12L);
            i__2 = m1 - ib1 + 1;
            dlacpy_(
                "Full", &dim1, &i__2, &dwork[itmp], &dim1, &b[ib1 + (m1 + ib1) * b_dim1], ldb, 4L);
            if (m2 > 0) {
                dgemm_("Transpose", "No Transpose", &dim1, &m2, &dim1, &c_b50, &dwork[i2lole],
                    &sdim, &b[m1 + ib1 + i2 * b_dim1], ldb, &c_b51, &b[ib1 + i2 * b_dim1], ldb, 9L,
                    12L);
                dgemm_("Transpose", "No Transpose", &dim1, &m2, &dim1, &c_b50, &dwork[i2lori],
                    &sdim, &b[m1 + ib1 + i2 * b_dim1], ldb, &c_b51, &dwork[itmp], &dim1, 9L, 12L);
                dlacpy_(
                    "Full", &dim1, &m2, &dwork[itmp], &dim1, &b[m1 + ib1 + i2 * b_dim1], ldb, 4L);
                dgemm_("Transpose", "No Transpose", &dim1, &m2, &dim1, &c_b50, &dwork[i2upri],
                    &sdim, &b[ib1 + i3 * b_dim1], ldb, &c_b51, &b[m1 + ib1 + i3 * b_dim1], ldb, 9L,
                    12L);
                dgemm_("Transpose", "No Transpose", &dim1, &m2, &dim1, &c_b50, &dwork[i2uple],
                    &sdim, &b[ib1 + i3 * b_dim1], ldb, &c_b51, &dwork[itmp], &dim1, 9L, 12L);
                dlacpy_("Full", &dim1, &m2, &dwork[itmp], &dim1, &b[ib1 + i3 * b_dim1], ldb, 4L);
            }
            itmp = iwrk + *n * dim1;
            if (lcmpq1) {
                /*              Update Q1. */
                dlacpy_("Full", n, &dim1, &q1[ib1 * q1_dim1 + 1], ldq1, &dwork[iwrk], n, 4L);
                dgemm_("No Transpose", "No Transpose", n, &dim1, &dim1, &c_b50, &dwork[iwrk], n,
                    &dwork[i1uple], &sdim, &c_b51, &q1[ib1 * q1_dim1 + 1], ldq1, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", n, &dim1, &dim1, &c_b50,
                    &q1[(m1 + ib1) * q1_dim1 + 1], ldq1, &dwork[i1lole], &sdim, &c_b50,
                    &q1[ib1 * q1_dim1 + 1], ldq1, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", n, &dim1, &dim1, &c_b50, &dwork[iwrk], n,
                    &dwork[i1upri], &sdim, &c_b51, &dwork[itmp], n, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", n, &dim1, &dim1, &c_b50,
                    &q1[(m1 + ib1) * q1_dim1 + 1], ldq1, &dwork[i1lori], &sdim, &c_b50,
                    &dwork[itmp], n, 12L, 12L);
                dlacpy_("Full", n, &dim1, &dwork[itmp], n, &q1[(m1 + ib1) * q1_dim1 + 1], ldq1, 4L);
            }
            if (lcmpq2) {
                /*              Update Q2. */
                dlacpy_("Full", n, &dim1, &q2[ib1 * q2_dim1 + 1], ldq2, &dwork[iwrk], n, 4L);
                dgemm_("No Transpose", "No Transpose", n, &dim1, &dim1, &c_b50, &dwork[iwrk], n,
                    &dwork[i2uple], &sdim, &c_b51, &q2[ib1 * q2_dim1 + 1], ldq2, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", n, &dim1, &dim1, &c_b50,
                    &q2[(m1 + ib1) * q2_dim1 + 1], ldq2, &dwork[i2lole], &sdim, &c_b50,
                    &q2[ib1 * q2_dim1 + 1], ldq2, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", n, &dim1, &dim1, &c_b50, &dwork[iwrk], n,
                    &dwork[i2upri], &sdim, &c_b51, &dwork[itmp], n, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", n, &dim1, &dim1, &c_b50,
                    &q2[(m1 + ib1) * q2_dim1 + 1], ldq2, &dwork[i2lori], &sdim, &c_b50,
                    &dwork[itmp], n, 12L, 12L);
                dlacpy_("Full", n, &dim1, &dwork[itmp], n, &q2[(m1 + ib1) * q2_dim1 + 1], ldq2, 4L);
            }
        } else {
            /*           Update A. */
            dcopy_(&nr, &a[ib1 * a_dim1 + 1], &c__1, &dwork[iwrk], &c__1);
            dscal_(&nr, &dwork[i1uple], &a[ib1 * a_dim1 + 1], &c__1);
            daxpy_(&nr, &dwork[i1lole], &a[(m1 + ib1) * a_dim1 + 1], &c__1, &a[ib1 * a_dim1 + 1],
                &c__1);
            dscal_(&nr, &dwork[i1lori], &a[(m1 + ib1) * a_dim1 + 1], &c__1);
            daxpy_(&nr, &dwork[i1upri], &dwork[iwrk], &c__1, &a[(m1 + ib1) * a_dim1 + 1], &c__1);
            dcopy_(&nr, &a[i1 + ib1 * a_dim1], &c__1, &dwork[iwrk], &c__1);
            dscal_(&nr, &dwork[i1uple], &a[i1 + ib1 * a_dim1], &c__1);
            daxpy_(&nr, &dwork[i1lole], &a[i1 + (m1 + ib1) * a_dim1], &c__1, &a[i1 + ib1 * a_dim1],
                &c__1);
            dscal_(&nr, &dwork[i1lori], &a[i1 + (m1 + ib1) * a_dim1], &c__1);
            daxpy_(&nr, &dwork[i1upri], &dwork[iwrk], &c__1, &a[i1 + (m1 + ib1) * a_dim1], &c__1);
            tmp2 = a[m1 + ib1 + ib1 * a_dim1];
            tmp3 = a[ib1 + (m1 + ib1) * a_dim1];
            if (m1 > ib1) {
                i__2 = m1 - ib1;
                dcopy_(&i__2, &a[ib1 + (ib1 + 1) * a_dim1], lda, &a[m1 + ib1 + (ib1 + 1) * a_dim1],
                    lda);
                i__2 = m1 - ib1;
                dscal_(&i__2, &dwork[i2upri], &a[m1 + ib1 + (ib1 + 1) * a_dim1], lda);
            }
            a[m1 + ib1 + ib1 * a_dim1] = 0.;
            i__2 = m1 - ib1 + 1;
            dscal_(&i__2, &dwork[i2uple], &a[ib1 + ib1 * a_dim1], lda);
            a[ib1 + ib1 * a_dim1] += dwork[i2lole] * tmp2;
            i__2 = m1 - ib1 + 1;
            dcopy_(
                &i__2, &a[m1 + ib1 + (m1 + ib1) * a_dim1], lda, &a[ib1 + (m1 + ib1) * a_dim1], lda);
            i__2 = m1 - ib1 + 1;
            dscal_(&i__2, &dwork[i2lole], &a[ib1 + (m1 + ib1) * a_dim1], lda);
            a[ib1 + (m1 + ib1) * a_dim1] += dwork[i2uple] * tmp3;
            i__2 = m1 - ib1 + 1;
            dscal_(&i__2, &dwork[i2lori], &a[m1 + ib1 + (m1 + ib1) * a_dim1], lda);
            a[m1 + ib1 + (m1 + ib1) * a_dim1] += dwork[i2upri] * tmp3;
            if (m2 > 0) {
                dcopy_(&m2, &a[ib1 + i2 * a_dim1], lda, &a[m1 + ib1 + i2 * a_dim1], lda);
                dscal_(&m2, &dwork[i2upri], &a[m1 + ib1 + i2 * a_dim1], lda);
                dscal_(&m2, &dwork[i2uple], &a[ib1 + i2 * a_dim1], lda);
                dcopy_(&m2, &a[m1 + ib1 + i3 * a_dim1], lda, &a[ib1 + i3 * a_dim1], lda);
                dscal_(&m2, &dwork[i2lole], &a[ib1 + i3 * a_dim1], lda);
                dscal_(&m2, &dwork[i2lori], &a[m1 + ib1 + i3 * a_dim1], lda);
            }
            /*           Update B. */
            dcopy_(&nr, &b[ib1 * b_dim1 + 1], &c__1, &dwork[iwrk], &c__1);
            dscal_(&nr, &dwork[i1uple], &b[ib1 * b_dim1 + 1], &c__1);
            daxpy_(&nr, &dwork[i1lole], &b[(m1 + ib1) * b_dim1 + 1], &c__1, &b[ib1 * b_dim1 + 1],
                &c__1);
            dscal_(&nr, &dwork[i1lori], &b[(m1 + ib1) * b_dim1 + 1], &c__1);
            daxpy_(&nr, &dwork[i1upri], &dwork[iwrk], &c__1, &b[(m1 + ib1) * b_dim1 + 1], &c__1);
            dcopy_(&nr, &b[i1 + ib1 * b_dim1], &c__1, &dwork[iwrk], &c__1);
            dscal_(&nr, &dwork[i1uple], &b[i1 + ib1 * b_dim1], &c__1);
            daxpy_(&nr, &dwork[i1lole], &b[i1 + (m1 + ib1) * b_dim1], &c__1, &b[i1 + ib1 * b_dim1],
                &c__1);
            dscal_(&nr, &dwork[i1lori], &b[i1 + (m1 + ib1) * b_dim1], &c__1);
            daxpy_(&nr, &dwork[i1upri], &dwork[iwrk], &c__1, &b[i1 + (m1 + ib1) * b_dim1], &c__1);
            tmp2 = b[ib1 + ib1 * b_dim1];
            tmp3 = b[m1 + ib1 + (m1 + ib1) * b_dim1];
            i__2 = m1 - ib1 + 1;
            dcopy_(&i__2, &b[m1 + ib1 + ib1 * b_dim1], ldb, &b[ib1 + ib1 * b_dim1], ldb);
            i__2 = m1 - ib1 + 1;
            dscal_(&i__2, &dwork[i2lole], &b[ib1 + ib1 * b_dim1], ldb);
            b[ib1 + ib1 * b_dim1] += dwork[i2uple] * tmp2;
            b[m1 + ib1 + ib1 * b_dim1] = 0.;
            i__2 = m1 - ib1 + 1;
            dscal_(&i__2, &dwork[i2lori], &b[m1 + ib1 + (ib1 + 1) * b_dim1], ldb);
            i__2 = m1 - ib1 + 1;
            dcopy_(
                &i__2, &b[ib1 + (m1 + ib1) * b_dim1], ldb, &b[m1 + ib1 + (m1 + ib1) * b_dim1], ldb);
            i__2 = m1 - ib1 + 1;
            dscal_(&i__2, &dwork[i2upri], &b[m1 + ib1 + (m1 + ib1) * b_dim1], ldb);
            b[m1 + ib1 + (m1 + ib1) * b_dim1] += dwork[i2lori] * tmp3;
            i__2 = m1 - ib1 + 1;
            dscal_(&i__2, &dwork[i2uple], &b[ib1 + (m1 + ib1) * b_dim1], ldb);
            b[ib1 + (m1 + ib1) * b_dim1] += dwork[i2lole] * tmp3;
            if (m2 > 0) {
                dcopy_(&m2, &b[m1 + ib1 + i2 * b_dim1], ldb, &b[ib1 + i2 * b_dim1], ldb);
                dscal_(&m2, &dwork[i2lole], &b[ib1 + i2 * b_dim1], ldb);
                dscal_(&m2, &dwork[i2lori], &b[m1 + ib1 + i2 * b_dim1], ldb);
                dcopy_(&m2, &b[ib1 + i3 * b_dim1], ldb, &b[m1 + ib1 + i3 * b_dim1], ldb);
                dscal_(&m2, &dwork[i2upri], &b[m1 + ib1 + i3 * b_dim1], ldb);
                dscal_(&m2, &dwork[i2uple], &b[ib1 + i3 * b_dim1], ldb);
            }
            itmp = iwrk + *n;
            if (lcmpq1) {
                /*              Update Q1. */
                dcopy_(n, &q1[ib1 * q1_dim1 + 1], &c__1, &dwork[iwrk], &c__1);
                dscal_(n, &dwork[i1uple], &q1[ib1 * q1_dim1 + 1], &c__1);
                daxpy_(n, &dwork[i1lole], &q1[(m1 + ib1) * q1_dim1 + 1], &c__1,
                    &q1[ib1 * q1_dim1 + 1], &c__1);
                dscal_(n, &dwork[i1lori], &q1[(m1 + ib1) * q1_dim1 + 1], &c__1);
                daxpy_(
                    n, &dwork[i1upri], &dwork[iwrk], &c__1, &q1[(m1 + ib1) * q1_dim1 + 1], &c__1);
            }
            if (lcmpq2) {
                /*              Update Q2. */
                dcopy_(n, &q2[ib1 * q2_dim1 + 1], &c__1, &dwork[iwrk], &c__1);
                dscal_(n, &dwork[i2uple], &q2[ib1 * q2_dim1 + 1], &c__1);
                daxpy_(n, &dwork[i2lole], &q2[(m1 + ib1) * q2_dim1 + 1], &c__1,
                    &q2[ib1 * q2_dim1 + 1], &c__1);
                dscal_(n, &dwork[i2lori], &q2[(m1 + ib1) * q2_dim1 + 1], &c__1);
                daxpy_(
                    n, &dwork[i2upri], &dwork[iwrk], &c__1, &q2[(m1 + ib1) * q2_dim1 + 1], &c__1);
            }
        }
        for (j = i__ - 1; j >= 1; --j) {
            /*           Calculate position of submatrices in DWORK. */
            ij1 = iwork[j];
            ij2 = iwork[j + 1];
            dim1 = iwork[i__ + 1] - iwork[i__];
            dim2 = ij2 - ij1;
            sdim = dim1 + dim2;
            ialole = iauple + dim1;
            iaupri = dim1 * sdim + 1;
            ialori = iaupri + dim1;
            ibuple = sdim * sdim + 1;
            iblole = ibuple + dim1;
            ibupri = sdim * sdim + dim1 * sdim + 1;
            iblori = ibupri + dim1;
            i1uple = (sdim << 1) * sdim + 1;
            i1lole = i1uple + dim1;
            i1upri = (sdim << 1) * sdim + dim1 * sdim + 1;
            i1lori = i1upri + dim1;
            i2uple = sdim * 3 * sdim + 1;
            i2lole = i2uple + dim1;
            i2upri = sdim * 3 * sdim + dim1 * sdim + 1;
            i2lori = i2upri + dim1;
            /*           Generate input matrices for MB03DD built of submatrices of A */
            /*           and B. */
            /*           Workspace:   need   32. */
            if (dim1 == 2 && dim2 == 2) {
                dlacpy_(
                    "Full", &dim1, &dim1, &a[ib1 + ib1 * a_dim1], lda, &dwork[iauple], &sdim, 4L);
                dlacpy_("Full", &dim2, &dim1, &a[m1 + ij1 + ib1 * a_dim1], lda, &dwork[ialole],
                    &sdim, 4L);
                dlaset_("Full", &dim1, &dim2, &c_b51, &c_b51, &dwork[iaupri], &sdim, 4L);
                dlacpy_("Full", &dim2, &dim2, &a[m1 + ij1 + (m1 + ij1) * a_dim1], lda,
                    &dwork[ialori], &sdim, 4L);
                dlacpy_(
                    "Full", &dim1, &dim1, &b[ib1 + ib1 * b_dim1], ldb, &dwork[ibuple], &sdim, 4L);
                dlacpy_("Full", &dim2, &dim1, &b[m1 + ij1 + ib1 * b_dim1], ldb, &dwork[iblole],
                    &sdim, 4L);
                dlaset_("Full", &dim1, &dim2, &c_b51, &c_b51, &dwork[ibupri], &sdim, 4L);
                dlacpy_("Full", &dim2, &dim2, &b[m1 + ij1 + (m1 + ij1) * b_dim1], ldb,
                    &dwork[iblori], &sdim, 4L);
            } else if (dim1 == 1 && dim2 == 2) {
                dwork[iauple] = a[ib1 + ib1 * a_dim1];
                dcopy_(&dim2, &a[m1 + ij1 + ib1 * a_dim1], &c__1, &dwork[ialole], &c__1);
                dcopy_(&dim2, dum, &c__0, &dwork[iaupri], &sdim);
                dlacpy_("Full", &dim2, &dim2, &a[m1 + ij1 + (m1 + ij1) * a_dim1], lda,
                    &dwork[ialori], &sdim, 4L);
                dwork[ibuple] = b[ib1 + ib1 * b_dim1];
                dcopy_(&dim2, &b[m1 + ij1 + ib1 * b_dim1], &c__1, &dwork[iblole], &c__1);
                dcopy_(&dim2, dum, &c__0, &dwork[ibupri], &sdim);
                dlacpy_("Full", &dim2, &dim2, &b[m1 + ij1 + (m1 + ij1) * b_dim1], ldb,
                    &dwork[iblori], &sdim, 4L);
            } else if (dim1 == 2 && dim2 == 1) {
                dlacpy_(
                    "Full", &dim1, &dim1, &a[ib1 + ib1 * a_dim1], lda, &dwork[iauple], &sdim, 4L);
                dcopy_(&dim1, &a[m1 + ij1 + ib1 * a_dim1], lda, &dwork[ialole], &sdim);
                dcopy_(&dim1, dum, &c__0, &dwork[iaupri], &c__1);
                dwork[ialori] = a[m1 + ij1 + (m1 + ij1) * a_dim1];
                dlacpy_(
                    "Full", &dim1, &dim1, &b[ib1 + ib1 * b_dim1], ldb, &dwork[ibuple], &sdim, 4L);
                dcopy_(&dim1, &b[m1 + ij1 + ib1 * b_dim1], ldb, &dwork[iblole], &sdim);
                dcopy_(&dim1, dum, &c__0, &dwork[ibupri], &c__1);
                dwork[iblori] = b[m1 + ij1 + (m1 + ij1) * b_dim1];
            } else {
                dwork[iauple] = a[ib1 + ib1 * a_dim1];
                dwork[ialole] = a[m1 + ij1 + ib1 * a_dim1];
                dwork[iaupri] = 0.;
                dwork[ialori] = a[m1 + ij1 + (m1 + ij1) * a_dim1];
                dwork[ibuple] = b[ib1 + ib1 * b_dim1];
                dwork[iblole] = b[m1 + ij1 + ib1 * b_dim1];
                dwork[ibupri] = 0.;
                dwork[iblori] = b[m1 + ij1 + (m1 + ij1) * b_dim1];
            }
            /*           Perform upper triangularization. */
            /*           Workspace:   need   64 + max( 75, 4*N ). */
            iwrk = (sdim << 2) * sdim + 1;
            itmp = iwrk + (*n << 1);
            i__2 = *ldwork - iwrk + 1;
            mb03dd_("Lower", &dim1, &dim2, &ulp, &dwork[ibuple], &sdim, &dwork[iauple], &sdim,
                &dwork[i1uple], &sdim, &dwork[i2uple], &sdim, &dwork[iwrk], &i__2, info, 5L);
            if (*info > 0) {
                if (*info <= 2) {
                    *info = 2;
                } else if (*info <= 4) {
                    *info = 3;
                } else {
                    *info = 4;
                }
                return 0;
            }
            nrow = ij2 - 1;
            if (dim1 == 2 && dim2 == 2) {
                /*              Update A. */
                dlacpy_("Full", &nr, &dim1, &a[ib1 * a_dim1 + 1], lda, &dwork[iwrk], &nr, 4L);
                dgemm_("No Transpose", "No Transpose", &nr, &dim1, &dim1, &c_b50, &dwork[iwrk], &nr,
                    &dwork[i1uple], &sdim, &c_b51, &a[ib1 * a_dim1 + 1], lda, 12L, 12L);
                i__2 = nr - dim1;
                dgemm_("No Transpose", "No Transpose", &i__2, &dim1, &dim2, &c_b50,
                    &a[(m1 + ij1) * a_dim1 + 1], lda, &dwork[i1lole], &sdim, &c_b50,
                    &a[ib1 * a_dim1 + 1], lda, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", &nr, &dim2, &dim1, &c_b50, &dwork[iwrk], &nr,
                    &dwork[i1upri], &sdim, &c_b51, &dwork[itmp], &nr, 12L, 12L);
                i__2 = nr - dim1;
                dgemm_("No Transpose", "No Transpose", &i__2, &dim2, &dim2, &c_b50,
                    &a[(m1 + ij1) * a_dim1 + 1], lda, &dwork[i1lori], &sdim, &c_b50, &dwork[itmp],
                    &nr, 12L, 12L);
                dlacpy_(
                    "Full", &nr, &dim2, &dwork[itmp], &nr, &a[(m1 + ij1) * a_dim1 + 1], lda, 4L);
                dlacpy_("Full", &nrow, &dim1, &a[i1 + ib1 * a_dim1], lda, &dwork[iwrk], &nrow, 4L);
                dgemm_("No Transpose", "No Transpose", &nrow, &dim1, &dim1, &c_b50, &dwork[iwrk],
                    &nrow, &dwork[i1uple], &sdim, &c_b51, &a[i1 + ib1 * a_dim1], lda, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", &nrow, &dim1, &dim2, &c_b50,
                    &a[i1 + (m1 + ij1) * a_dim1], lda, &dwork[i1lole], &sdim, &c_b50,
                    &a[i1 + ib1 * a_dim1], lda, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", &nrow, &dim2, &dim1, &c_b50, &dwork[iwrk],
                    &nrow, &dwork[i1upri], &sdim, &c_b51, &dwork[itmp], &nrow, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", &nrow, &dim2, &dim2, &c_b50,
                    &a[i1 + (m1 + ij1) * a_dim1], lda, &dwork[i1lori], &sdim, &c_b50, &dwork[itmp],
                    &nrow, 12L, 12L);
                dlacpy_("Full", &nrow, &dim2, &dwork[itmp], &nrow, &a[i1 + (m1 + ij1) * a_dim1],
                    lda, 4L);
                i__2 = m1 - ib1 + 1;
                dlacpy_("Full", &dim1, &i__2, &a[ib1 + ib1 * a_dim1], lda, &dwork[iwrk], &dim1, 4L);
                i__2 = m1 - ib1 + 1;
                dgemm_("Transpose", "No Transpose", &dim1, &i__2, &dim1, &c_b50, &dwork[i2uple],
                    &sdim, &dwork[iwrk], &dim1, &c_b51, &a[ib1 + ib1 * a_dim1], lda, 9L, 12L);
                i__2 = m1 - ib1 + 1;
                dgemm_("Transpose", "No Transpose", &dim1, &i__2, &dim2, &c_b50, &dwork[i2lole],
                    &sdim, &a[m1 + ij1 + ib1 * a_dim1], lda, &c_b50, &a[ib1 + ib1 * a_dim1], lda,
                    9L, 12L);
                i__2 = m1 - ib1 + 1;
                dgemm_("Transpose", "No Transpose", &dim2, &i__2, &dim1, &c_b50, &dwork[i2upri],
                    &sdim, &dwork[iwrk], &dim1, &c_b51, &dwork[itmp], &dim2, 9L, 12L);
                i__2 = m1 - ib1 + 1;
                dgemm_("Transpose", "No Transpose", &dim2, &i__2, &dim2, &c_b50, &dwork[i2lori],
                    &sdim, &a[m1 + ij1 + ib1 * a_dim1], lda, &c_b50, &dwork[itmp], &dim2, 9L, 12L);
                i__2 = m1 - ib1 + 1;
                dlacpy_("Full", &dim2, &i__2, &dwork[itmp], &dim2, &a[m1 + ij1 + ib1 * a_dim1], lda,
                    4L);
                i__2 = m1 - ij1 + 1;
                dlacpy_("Full", &dim1, &i__2, &a[ib1 + (m1 + ij1) * a_dim1], lda, &dwork[iwrk],
                    &dim1, 4L);
                i__2 = m1 - ij1 + 1;
                dgemm_("Transpose", "No Transpose", &dim1, &i__2, &dim1, &c_b50, &dwork[i2uple],
                    &sdim, &dwork[iwrk], &dim1, &c_b51, &a[ib1 + (m1 + ij1) * a_dim1], lda, 9L,
                    12L);
                i__2 = m1 - ij1 + 1;
                dgemm_("Transpose", "No Transpose", &dim1, &i__2, &dim2, &c_b50, &dwork[i2lole],
                    &sdim, &a[m1 + ij1 + (m1 + ij1) * a_dim1], lda, &c_b50,
                    &a[ib1 + (m1 + ij1) * a_dim1], lda, 9L, 12L);
                i__2 = m1 - ij1 + 1;
                dgemm_("Transpose", "No Transpose", &dim2, &i__2, &dim1, &c_b50, &dwork[i2upri],
                    &sdim, &dwork[iwrk], &dim1, &c_b51, &dwork[itmp], &dim2, 9L, 12L);
                i__2 = m1 - ij1 + 1;
                dgemm_("Transpose", "No Transpose", &dim2, &i__2, &dim2, &c_b50, &dwork[i2lori],
                    &sdim, &a[m1 + ij1 + (m1 + ij1) * a_dim1], lda, &c_b50, &dwork[itmp], &dim2, 9L,
                    12L);
                i__2 = m1 - ij1 + 1;
                dlacpy_("Full", &dim2, &i__2, &dwork[itmp], &dim2,
                    &a[m1 + ij1 + (m1 + ij1) * a_dim1], lda, 4L);
                if (m2 > 0) {
                    dlacpy_(
                        "Full", &dim1, &m4, &a[ib1 + i2 * a_dim1], lda, &dwork[iwrk], &dim1, 4L);
                    dgemm_("Transpose", "No Transpose", &dim1, &m4, &dim1, &c_b50, &dwork[i2uple],
                        &sdim, &dwork[iwrk], &dim1, &c_b51, &a[ib1 + i2 * a_dim1], lda, 9L, 12L);
                    dgemm_("Transpose", "No Transpose", &dim1, &m4, &dim2, &c_b50, &dwork[i2lole],
                        &sdim, &a[m1 + ij1 + i2 * a_dim1], lda, &c_b50, &a[ib1 + i2 * a_dim1], lda,
                        9L, 12L);
                    dgemm_("Transpose", "No Transpose", &dim2, &m4, &dim1, &c_b50, &dwork[i2upri],
                        &sdim, &dwork[iwrk], &dim1, &c_b51, &dwork[itmp], &dim2, 9L, 12L);
                    dgemm_("Transpose", "No Transpose", &dim2, &m4, &dim2, &c_b50, &dwork[i2lori],
                        &sdim, &a[m1 + ij1 + i2 * a_dim1], lda, &c_b50, &dwork[itmp], &dim2, 9L,
                        12L);
                    dlacpy_("Full", &dim2, &m4, &dwork[itmp], &dim2, &a[m1 + ij1 + i2 * a_dim1],
                        lda, 4L);
                }
                /*              Update B. */
                dlacpy_("Full", &nr, &dim1, &b[ib1 * b_dim1 + 1], ldb, &dwork[iwrk], &nr, 4L);
                dgemm_("No Transpose", "No Transpose", &nr, &dim1, &dim1, &c_b50, &dwork[iwrk], &nr,
                    &dwork[i1uple], &sdim, &c_b51, &b[ib1 * b_dim1 + 1], ldb, 12L, 12L);
                i__2 = nr - dim1;
                dgemm_("No Transpose", "No Transpose", &i__2, &dim1, &dim2, &c_b50,
                    &b[(m1 + ij1) * b_dim1 + 1], ldb, &dwork[i1lole], &sdim, &c_b50,
                    &b[ib1 * b_dim1 + 1], ldb, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", &nr, &dim2, &dim1, &c_b50, &dwork[iwrk], &nr,
                    &dwork[i1upri], &sdim, &c_b51, &dwork[itmp], &nr, 12L, 12L);
                i__2 = nr - dim1;
                dgemm_("No Transpose", "No Transpose", &i__2, &dim2, &dim2, &c_b50,
                    &b[(m1 + ij1) * b_dim1 + 1], ldb, &dwork[i1lori], &sdim, &c_b50, &dwork[itmp],
                    &nr, 12L, 12L);
                dlacpy_(
                    "Full", &nr, &dim2, &dwork[itmp], &nr, &b[(m1 + ij1) * b_dim1 + 1], ldb, 4L);
                dlacpy_("Full", &nrow, &dim1, &b[i1 + ib1 * b_dim1], ldb, &dwork[iwrk], &nrow, 4L);
                dgemm_("No Transpose", "No Transpose", &nrow, &dim1, &dim1, &c_b50, &dwork[iwrk],
                    &nrow, &dwork[i1uple], &sdim, &c_b51, &b[i1 + ib1 * b_dim1], ldb, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", &nrow, &dim1, &dim2, &c_b50,
                    &b[i1 + (m1 + ij1) * b_dim1], ldb, &dwork[i1lole], &sdim, &c_b50,
                    &b[i1 + ib1 * b_dim1], ldb, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", &nrow, &dim2, &dim1, &c_b50, &dwork[iwrk],
                    &nrow, &dwork[i1upri], &sdim, &c_b51, &dwork[itmp], &nrow, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", &nrow, &dim2, &dim2, &c_b50,
                    &b[i1 + (m1 + ij1) * b_dim1], ldb, &dwork[i1lori], &sdim, &c_b50, &dwork[itmp],
                    &nrow, 12L, 12L);
                dlacpy_("Full", &nrow, &dim2, &dwork[itmp], &nrow, &b[i1 + (m1 + ij1) * b_dim1],
                    ldb, 4L);
                i__2 = m1 - ib1 + 1;
                dlacpy_("Full", &dim1, &i__2, &b[ib1 + ib1 * b_dim1], ldb, &dwork[iwrk], &dim1, 4L);
                i__2 = m1 - ib1 + 1;
                dgemm_("Transpose", "No Transpose", &dim1, &i__2, &dim1, &c_b50, &dwork[i2uple],
                    &sdim, &dwork[iwrk], &dim1, &c_b51, &b[ib1 + ib1 * b_dim1], ldb, 9L, 12L);
                i__2 = m1 - ib1 + 1;
                dgemm_("Transpose", "No Transpose", &dim1, &i__2, &dim2, &c_b50, &dwork[i2lole],
                    &sdim, &b[m1 + ij1 + ib1 * b_dim1], ldb, &c_b50, &b[ib1 + ib1 * b_dim1], ldb,
                    9L, 12L);
                i__2 = m1 - ib1 + 1;
                dgemm_("Transpose", "No Transpose", &dim2, &i__2, &dim1, &c_b50, &dwork[i2upri],
                    &sdim, &dwork[iwrk], &dim1, &c_b51, &dwork[itmp], &dim2, 9L, 12L);
                i__2 = m1 - ib1 + 1;
                dgemm_("Transpose", "No Transpose", &dim2, &i__2, &dim2, &c_b50, &dwork[i2lori],
                    &sdim, &b[m1 + ij1 + ib1 * b_dim1], ldb, &c_b50, &dwork[itmp], &dim2, 9L, 12L);
                i__2 = m1 - ib1 + 1;
                dlacpy_("Full", &dim2, &i__2, &dwork[itmp], &dim2, &b[m1 + ij1 + ib1 * b_dim1], ldb,
                    4L);
                i__2 = m1 - ij1 + 1;
                dlacpy_("Full", &dim1, &i__2, &b[ib1 + (m1 + ij1) * b_dim1], ldb, &dwork[iwrk],
                    &dim1, 4L);
                i__2 = m1 - ij1 + 1;
                dgemm_("Transpose", "No Transpose", &dim1, &i__2, &dim1, &c_b50, &dwork[i2uple],
                    &sdim, &dwork[iwrk], &dim1, &c_b51, &b[ib1 + (m1 + ij1) * b_dim1], ldb, 9L,
                    12L);
                i__2 = m1 - ij1 + 1;
                dgemm_("Transpose", "No Transpose", &dim1, &i__2, &dim2, &c_b50, &dwork[i2lole],
                    &sdim, &b[m1 + ij1 + (m1 + ij1) * b_dim1], ldb, &c_b50,
                    &b[ib1 + (m1 + ij1) * b_dim1], ldb, 9L, 12L);
                i__2 = m1 - ij1 + 1;
                dgemm_("Transpose", "No Transpose", &dim2, &i__2, &dim1, &c_b50, &dwork[i2upri],
                    &sdim, &dwork[iwrk], &dim1, &c_b51, &dwork[itmp], &dim2, 9L, 12L);
                i__2 = m1 - ij1 + 1;
                dgemm_("Transpose", "No Transpose", &dim2, &i__2, &dim2, &c_b50, &dwork[i2lori],
                    &sdim, &b[m1 + ij1 + (m1 + ij1) * b_dim1], ldb, &c_b50, &dwork[itmp], &dim2, 9L,
                    12L);
                i__2 = m1 - ij1 + 1;
                dlacpy_("Full", &dim2, &i__2, &dwork[itmp], &dim2,
                    &b[m1 + ij1 + (m1 + ij1) * b_dim1], ldb, 4L);
                if (m2 > 0) {
                    dlacpy_(
                        "Full", &dim1, &m4, &b[ib1 + i2 * b_dim1], ldb, &dwork[iwrk], &dim1, 4L);
                    dgemm_("Transpose", "No Transpose", &dim1, &m4, &dim1, &c_b50, &dwork[i2uple],
                        &sdim, &dwork[iwrk], &dim1, &c_b51, &b[ib1 + i2 * b_dim1], ldb, 9L, 12L);
                    dgemm_("Transpose", "No Transpose", &dim1, &m4, &dim2, &c_b50, &dwork[i2lole],
                        &sdim, &b[m1 + ij1 + i2 * b_dim1], ldb, &c_b50, &b[ib1 + i2 * b_dim1], ldb,
                        9L, 12L);
                    dgemm_("Transpose", "No Transpose", &dim2, &m4, &dim1, &c_b50, &dwork[i2upri],
                        &sdim, &dwork[iwrk], &dim1, &c_b51, &dwork[itmp], &dim2, 9L, 12L);
                    dgemm_("Transpose", "No Transpose", &dim2, &m4, &dim2, &c_b50, &dwork[i2lori],
                        &sdim, &b[m1 + ij1 + i2 * b_dim1], ldb, &c_b50, &dwork[itmp], &dim2, 9L,
                        12L);
                    dlacpy_("Full", &dim2, &m4, &dwork[itmp], &dim2, &b[m1 + ij1 + i2 * b_dim1],
                        ldb, 4L);
                }
                /*              Update Q1. */
                if (lcmpq1) {
                    dlacpy_("Full", n, &dim1, &q1[ib1 * q1_dim1 + 1], ldq1, &dwork[iwrk], n, 4L);
                    dgemm_("No Transpose", "No Transpose", n, &dim1, &dim1, &c_b50, &dwork[iwrk], n,
                        &dwork[i1uple], &sdim, &c_b51, &q1[ib1 * q1_dim1 + 1], ldq1, 12L, 12L);
                    dgemm_("No Transpose", "No Transpose", n, &dim1, &dim2, &c_b50,
                        &q1[(m1 + ij1) * q1_dim1 + 1], ldq1, &dwork[i1lole], &sdim, &c_b50,
                        &q1[ib1 * q1_dim1 + 1], ldq1, 12L, 12L);
                    dgemm_("No Transpose", "No Transpose", n, &dim2, &dim1, &c_b50, &dwork[iwrk], n,
                        &dwork[i1upri], &sdim, &c_b51, &dwork[itmp], n, 12L, 12L);
                    dgemm_("No Transpose", "No Transpose", n, &dim2, &dim2, &c_b50,
                        &q1[(m1 + ij1) * q1_dim1 + 1], ldq1, &dwork[i1lori], &sdim, &c_b50,
                        &dwork[itmp], n, 12L, 12L);
                    dlacpy_(
                        "Full", n, &dim2, &dwork[itmp], n, &q1[(m1 + ij1) * q1_dim1 + 1], ldq1, 4L);
                }
                /*              Update Q2. */
                if (lcmpq2) {
                    dlacpy_("Full", n, &dim1, &q2[ib1 * q2_dim1 + 1], ldq2, &dwork[iwrk], n, 4L);
                    dgemm_("No Transpose", "No Transpose", n, &dim1, &dim1, &c_b50, &dwork[iwrk], n,
                        &dwork[i2uple], &sdim, &c_b51, &q2[ib1 * q2_dim1 + 1], ldq2, 12L, 12L);
                    dgemm_("No Transpose", "No Transpose", n, &dim1, &dim2, &c_b50,
                        &q2[(m1 + ij1) * q2_dim1 + 1], ldq2, &dwork[i2lole], &sdim, &c_b50,
                        &q2[ib1 * q2_dim1 + 1], ldq2, 12L, 12L);
                    dgemm_("No Transpose", "No Transpose", n, &dim2, &dim1, &c_b50, &dwork[iwrk], n,
                        &dwork[i2upri], &sdim, &c_b51, &dwork[itmp], n, 12L, 12L);
                    dgemm_("No Transpose", "No Transpose", n, &dim2, &dim2, &c_b50,
                        &q2[(m1 + ij1) * q2_dim1 + 1], ldq2, &dwork[i2lori], &sdim, &c_b50,
                        &dwork[itmp], n, 12L, 12L);
                    dlacpy_(
                        "Full", n, &dim2, &dwork[itmp], n, &q2[(m1 + ij1) * q2_dim1 + 1], ldq2, 4L);
                }
            } else if (dim1 == 1 && dim2 == 2) {
                /*              Update A. */
                dcopy_(&nr, &a[ib1 * a_dim1 + 1], &c__1, &dwork[iwrk], &c__1);
                i__2 = nr - 1;
                dgemv_("No Transpose", &i__2, &dim2, &c_b50, &a[(m1 + ij1) * a_dim1 + 1], lda,
                    &dwork[i1lole], &c__1, &dwork[i1uple], &a[ib1 * a_dim1 + 1], &c__1, 12L);
                a[nr + ib1 * a_dim1] = dwork[i1uple] * a[nr + ib1 * a_dim1];
                i__2 = nr - 1;
                dgemm_("No Transpose", "No Transpose", &i__2, &dim2, &dim2, &c_b50,
                    &a[(m1 + ij1) * a_dim1 + 1], lda, &dwork[i1lori], &sdim, &c_b51, &dwork[itmp],
                    &nr, 12L, 12L);
                dwork[itmp + nr - 1] = 0.;
                dwork[itmp + (nr << 1) - 1] = 0.;
                daxpy_(&nr, &dwork[i1upri], &dwork[iwrk], &c__1, &dwork[itmp], &c__1);
                daxpy_(&nr, &dwork[i1upri + sdim], &dwork[iwrk], &c__1, &dwork[itmp + nr], &c__1);
                dlacpy_(
                    "Full", &nr, &dim2, &dwork[itmp], &nr, &a[(m1 + ij1) * a_dim1 + 1], lda, 4L);
                dcopy_(&nrow, &a[i1 + ib1 * a_dim1], &c__1, &dwork[iwrk], &c__1);
                dgemv_("No Transpose", &nrow, &dim2, &c_b50, &a[i1 + (m1 + ij1) * a_dim1], lda,
                    &dwork[i1lole], &c__1, &dwork[i1uple], &a[i1 + ib1 * a_dim1], &c__1, 12L);
                dgemm_("No Transpose", "No Transpose", &nrow, &dim2, &dim2, &c_b50,
                    &a[i1 + (m1 + ij1) * a_dim1], lda, &dwork[i1lori], &sdim, &c_b51, &dwork[itmp],
                    &nrow, 12L, 12L);
                daxpy_(&nrow, &dwork[i1upri], &dwork[iwrk], &c__1, &dwork[itmp], &c__1);
                daxpy_(
                    &nrow, &dwork[i1upri + sdim], &dwork[iwrk], &c__1, &dwork[itmp + nrow], &c__1);
                dlacpy_("Full", &nrow, &dim2, &dwork[itmp], &nrow, &a[i1 + (m1 + ij1) * a_dim1],
                    lda, 4L);
                i__2 = m1 - ib1 + 1;
                dcopy_(&i__2, &a[ib1 + ib1 * a_dim1], lda, &dwork[iwrk], &c__1);
                i__2 = m1 - ib1 + 1;
                dgemv_("Transpose", &dim2, &i__2, &c_b50, &a[m1 + ij1 + ib1 * a_dim1], lda,
                    &dwork[i2lole], &c__1, &dwork[i2uple], &a[ib1 + ib1 * a_dim1], lda, 9L);
                i__2 = m1 - ib1 + 1;
                dgemm_("Transpose", "No Transpose", &dim2, &i__2, &dim2, &c_b50, &dwork[i2lori],
                    &sdim, &a[m1 + ij1 + ib1 * a_dim1], lda, &c_b51, &dwork[itmp], &dim2, 9L, 12L);
                i__2 = m1 - ib1 + 1;
                daxpy_(&i__2, &dwork[i2upri], &dwork[iwrk], &c__1, &dwork[itmp], &dim2);
                i__2 = m1 - ib1 + 1;
                daxpy_(&i__2, &dwork[i2upri + sdim], &dwork[iwrk], &c__1, &dwork[itmp + 1], &dim2);
                i__2 = m1 - ib1 + 1;
                dlacpy_("Full", &dim2, &i__2, &dwork[itmp], &dim2, &a[m1 + ij1 + ib1 * a_dim1], lda,
                    4L);
                i__2 = m1 - ij1 + 1;
                dcopy_(&i__2, &a[ib1 + (m1 + ij1) * a_dim1], lda, &dwork[iwrk], &c__1);
                i__2 = m1 - ij1 + 1;
                dgemv_("Transpose", &dim2, &i__2, &c_b50, &a[m1 + ij1 + (m1 + ij1) * a_dim1], lda,
                    &dwork[i2lole], &c__1, &dwork[i2uple], &a[ib1 + (m1 + ij1) * a_dim1], lda, 9L);
                i__2 = m1 - ij1 + 1;
                dgemm_("Transpose", "No Transpose", &dim2, &i__2, &dim2, &c_b50, &dwork[i2lori],
                    &sdim, &a[m1 + ij1 + (m1 + ij1) * a_dim1], lda, &c_b51, &dwork[itmp], &dim2, 9L,
                    12L);
                i__2 = m1 - ij1 + 1;
                daxpy_(&i__2, &dwork[i2upri], &dwork[iwrk], &c__1, &dwork[itmp], &dim2);
                i__2 = m1 - ij1 + 1;
                daxpy_(&i__2, &dwork[i2upri + sdim], &dwork[iwrk], &c__1, &dwork[itmp + 1], &dim2);
                i__2 = m1 - ij1 + 1;
                dlacpy_("Full", &dim2, &i__2, &dwork[itmp], &dim2,
                    &a[m1 + ij1 + (m1 + ij1) * a_dim1], lda, 4L);
                if (m2 > 0) {
                    dcopy_(&m4, &a[ib1 + i2 * a_dim1], lda, &dwork[iwrk], &c__1);
                    dgemv_("Transpose", &dim2, &m4, &c_b50, &a[m1 + ij1 + i2 * a_dim1], lda,
                        &dwork[i2lole], &c__1, &dwork[i2uple], &a[ib1 + i2 * a_dim1], lda, 9L);
                    dgemm_("Transpose", "No Transpose", &dim2, &m4, &dim2, &c_b50, &dwork[i2lori],
                        &sdim, &a[m1 + ij1 + i2 * a_dim1], lda, &c_b51, &dwork[itmp], &dim2, 9L,
                        12L);
                    daxpy_(&m4, &dwork[i2upri], &dwork[iwrk], &c__1, &dwork[itmp], &dim2);
                    daxpy_(
                        &m4, &dwork[i2upri + sdim], &dwork[iwrk], &c__1, &dwork[itmp + 1], &dim2);
                    dlacpy_("Full", &dim2, &m4, &dwork[itmp], &dim2, &a[m1 + ij1 + i2 * a_dim1],
                        lda, 4L);
                }
                /*              Update B. */
                dcopy_(&nr, &b[ib1 * b_dim1 + 1], &c__1, &dwork[iwrk], &c__1);
                i__2 = nr - 1;
                dgemv_("No Transpose", &i__2, &dim2, &c_b50, &b[(m1 + ij1) * b_dim1 + 1], ldb,
                    &dwork[i1lole], &c__1, &dwork[i1uple], &b[ib1 * b_dim1 + 1], &c__1, 12L);
                b[nr + ib1 * b_dim1] = dwork[i1uple] * b[nr + ib1 * b_dim1];
                i__2 = nr - 1;
                dgemm_("No Transpose", "No Transpose", &i__2, &dim2, &dim2, &c_b50,
                    &b[(m1 + ij1) * b_dim1 + 1], ldb, &dwork[i1lori], &sdim, &c_b51, &dwork[itmp],
                    &nr, 12L, 12L);
                dwork[itmp + nr - 1] = 0.;
                dwork[itmp + (nr << 1) - 1] = 0.;
                daxpy_(&nr, &dwork[i1upri], &dwork[iwrk], &c__1, &dwork[itmp], &c__1);
                daxpy_(&nr, &dwork[i1upri + sdim], &dwork[iwrk], &c__1, &dwork[itmp + nr], &c__1);
                dlacpy_(
                    "Full", &nr, &dim2, &dwork[itmp], &nr, &b[(m1 + ij1) * b_dim1 + 1], ldb, 4L);
                dcopy_(&nrow, &b[i1 + ib1 * b_dim1], &c__1, &dwork[iwrk], &c__1);
                dgemv_("No Transpose", &nrow, &dim2, &c_b50, &b[i1 + (m1 + ij1) * b_dim1], ldb,
                    &dwork[i1lole], &c__1, &dwork[i1uple], &b[i1 + ib1 * b_dim1], &c__1, 12L);
                dgemm_("No Transpose", "No Transpose", &nrow, &dim2, &dim2, &c_b50,
                    &b[i1 + (m1 + ij1) * b_dim1], ldb, &dwork[i1lori], &sdim, &c_b51, &dwork[itmp],
                    &nrow, 12L, 12L);
                daxpy_(&nrow, &dwork[i1upri], &dwork[iwrk], &c__1, &dwork[itmp], &c__1);
                daxpy_(
                    &nrow, &dwork[i1upri + sdim], &dwork[iwrk], &c__1, &dwork[itmp + nrow], &c__1);
                dlacpy_("Full", &nrow, &dim2, &dwork[itmp], &nrow, &b[i1 + (m1 + ij1) * b_dim1],
                    ldb, 4L);
                i__2 = m1 - ib1 + 1;
                dcopy_(&i__2, &b[ib1 + ib1 * b_dim1], ldb, &dwork[iwrk], &c__1);
                i__2 = m1 - ib1 + 1;
                dgemv_("Transpose", &dim2, &i__2, &c_b50, &b[m1 + ij1 + ib1 * b_dim1], ldb,
                    &dwork[i2lole], &c__1, &dwork[i2uple], &b[ib1 + ib1 * b_dim1], ldb, 9L);
                i__2 = m1 - ib1 + 1;
                dgemm_("Transpose", "No Transpose", &dim2, &i__2, &dim2, &c_b50, &dwork[i2lori],
                    &sdim, &b[m1 + ij1 + ib1 * b_dim1], ldb, &c_b51, &dwork[itmp], &dim2, 9L, 12L);
                i__2 = m1 - ib1 + 1;
                daxpy_(&i__2, &dwork[i2upri], &dwork[iwrk], &c__1, &dwork[itmp], &dim2);
                i__2 = m1 - ib1 + 1;
                daxpy_(&i__2, &dwork[i2upri + sdim], &dwork[iwrk], &c__1, &dwork[itmp + 1], &dim2);
                i__2 = m1 - ib1 + 1;
                dlacpy_("Full", &dim2, &i__2, &dwork[itmp], &dim2, &b[m1 + ij1 + ib1 * b_dim1], ldb,
                    4L);
                i__2 = m1 - ij1 + 1;
                dcopy_(&i__2, &b[ib1 + (m1 + ij1) * b_dim1], ldb, &dwork[iwrk], &c__1);
                i__2 = m1 - ij1 + 1;
                dgemv_("Transpose", &dim2, &i__2, &c_b50, &b[m1 + ij1 + (m1 + ij1) * b_dim1], ldb,
                    &dwork[i2lole], &c__1, &dwork[i2uple], &b[ib1 + (m1 + ij1) * b_dim1], ldb, 9L);
                i__2 = m1 - ij1 + 1;
                dgemm_("Transpose", "No Transpose", &dim2, &i__2, &dim2, &c_b50, &dwork[i2lori],
                    &sdim, &b[m1 + ij1 + (m1 + ij1) * b_dim1], ldb, &c_b51, &dwork[itmp], &dim2, 9L,
                    12L);
                i__2 = m1 - ij1 + 1;
                daxpy_(&i__2, &dwork[i2upri], &dwork[iwrk], &c__1, &dwork[itmp], &dim2);
                i__2 = m1 - ij1 + 1;
                daxpy_(&i__2, &dwork[i2upri + sdim], &dwork[iwrk], &c__1, &dwork[itmp + 1], &dim2);
                i__2 = m1 - ij1 + 1;
                dlacpy_("Full", &dim2, &i__2, &dwork[itmp], &dim2,
                    &b[m1 + ij1 + (m1 + ij1) * b_dim1], ldb, 4L);
                if (m2 > 0) {
                    dcopy_(&m4, &b[ib1 + i2 * b_dim1], ldb, &dwork[iwrk], &c__1);
                    dgemv_("Transpose", &dim2, &m4, &c_b50, &b[m1 + ij1 + i2 * b_dim1], ldb,
                        &dwork[i2lole], &c__1, &dwork[i2uple], &b[ib1 + i2 * b_dim1], ldb, 9L);
                    dgemm_("Transpose", "No Transpose", &dim2, &m4, &dim2, &c_b50, &dwork[i2lori],
                        &sdim, &b[m1 + ij1 + i2 * b_dim1], ldb, &c_b51, &dwork[itmp], &dim2, 9L,
                        12L);
                    daxpy_(&m4, &dwork[i2upri], &dwork[iwrk], &c__1, &dwork[itmp], &dim2);
                    daxpy_(
                        &m4, &dwork[i2upri + sdim], &dwork[iwrk], &c__1, &dwork[itmp + 1], &dim2);
                    dlacpy_("Full", &dim2, &m4, &dwork[itmp], &dim2, &b[m1 + ij1 + i2 * b_dim1],
                        ldb, 4L);
                }
                /*              Update Q1. */
                if (lcmpq1) {
                    dcopy_(n, &q1[ib1 * q1_dim1 + 1], &c__1, &dwork[iwrk], &c__1);
                    dgemv_("No Transpose", n, &dim2, &c_b50, &q1[(m1 + ij1) * q1_dim1 + 1], ldq1,
                        &dwork[i1lole], &c__1, &dwork[i1uple], &q1[ib1 * q1_dim1 + 1], &c__1, 12L);
                    dgemm_("No Transpose", "No Transpose", n, &dim2, &dim2, &c_b50,
                        &q1[(m1 + ij1) * q1_dim1 + 1], ldq1, &dwork[i1lori], &sdim, &c_b51,
                        &dwork[itmp], n, 12L, 12L);
                    daxpy_(n, &dwork[i1upri], &dwork[iwrk], &c__1, &dwork[itmp], &c__1);
                    daxpy_(n, &dwork[i1upri + sdim], &dwork[iwrk], &c__1, &dwork[itmp + *n], &c__1);
                    dlacpy_(
                        "Full", n, &dim2, &dwork[itmp], n, &q1[(m1 + ij1) * q1_dim1 + 1], ldq1, 4L);
                }
                /*              Update Q2. */
                if (lcmpq2) {
                    dcopy_(n, &q2[ib1 * q2_dim1 + 1], &c__1, &dwork[iwrk], &c__1);
                    dgemv_("No Transpose", n, &dim2, &c_b50, &q2[(m1 + ij1) * q2_dim1 + 1], ldq2,
                        &dwork[i2lole], &c__1, &dwork[i2uple], &q2[ib1 * q2_dim1 + 1], &c__1, 12L);
                    dgemm_("No Transpose", "No Transpose", n, &dim2, &dim2, &c_b50,
                        &q2[(m1 + ij1) * q2_dim1 + 1], ldq2, &dwork[i2lori], &sdim, &c_b51,
                        &dwork[itmp], n, 12L, 12L);
                    daxpy_(n, &dwork[i2upri], &dwork[iwrk], &c__1, &dwork[itmp], &c__1);
                    daxpy_(n, &dwork[i2upri + sdim], &dwork[iwrk], &c__1, &dwork[itmp + *n], &c__1);
                    dlacpy_(
                        "Full", n, &dim2, &dwork[itmp], n, &q2[(m1 + ij1) * q2_dim1 + 1], ldq2, 4L);
                }
            } else if (dim1 == 2 && dim2 == 1) {
                /*              Update A. */
                dlacpy_("Full", &nr, &dim1, &a[ib1 * a_dim1 + 1], lda, &dwork[iwrk], &nr, 4L);
                dgemm_("No Transpose", "No Transpose", &nr, &dim1, &dim1, &c_b50, &dwork[iwrk], &nr,
                    &dwork[i1uple], &sdim, &c_b51, &a[ib1 * a_dim1 + 1], lda, 12L, 12L);
                i__2 = nr - 1;
                daxpy_(&i__2, &dwork[i1lole], &a[(m1 + ij1) * a_dim1 + 1], &c__1,
                    &a[ib1 * a_dim1 + 1], &c__1);
                i__2 = nr - 1;
                daxpy_(&i__2, &dwork[i1lole + sdim], &a[(m1 + ij1) * a_dim1 + 1], &c__1,
                    &a[(ib1 + 1) * a_dim1 + 1], &c__1);
                a[nr + (m1 + ij1) * a_dim1] = 0.;
                dgemv_("No Transpose", &nr, &dim1, &c_b50, &dwork[iwrk], &nr, &dwork[i1upri], &c__1,
                    &dwork[i1lori], &a[(m1 + ij1) * a_dim1 + 1], &c__1, 12L);
                dlacpy_("Full", &nrow, &dim1, &a[i1 + ib1 * a_dim1], lda, &dwork[iwrk], &nrow, 4L);
                dgemm_("No Transpose", "No Transpose", &nrow, &dim1, &dim1, &c_b50, &dwork[iwrk],
                    &nrow, &dwork[i1uple], &sdim, &c_b51, &a[i1 + ib1 * a_dim1], lda, 12L, 12L);
                daxpy_(&nrow, &dwork[i1lole], &a[i1 + (m1 + ij1) * a_dim1], &c__1,
                    &a[i1 + ib1 * a_dim1], &c__1);
                daxpy_(&nrow, &dwork[i1lole + sdim], &a[i1 + (m1 + ij1) * a_dim1], &c__1,
                    &a[i1 + (ib1 + 1) * a_dim1], &c__1);
                dgemv_("No Transpose", &nrow, &dim1, &c_b50, &dwork[iwrk], &nrow, &dwork[i1upri],
                    &c__1, &dwork[i1lori], &a[i1 + (m1 + ij1) * a_dim1], &c__1, 12L);
                i__2 = m1 - ib1 + 1;
                dlacpy_("Full", &dim1, &i__2, &a[ib1 + ib1 * a_dim1], lda, &dwork[iwrk], &dim1, 4L);
                i__2 = m1 - ib1 + 1;
                dgemm_("Transpose", "No Transpose", &dim1, &i__2, &dim1, &c_b50, &dwork[i2uple],
                    &sdim, &dwork[iwrk], &dim1, &c_b51, &a[ib1 + ib1 * a_dim1], lda, 9L, 12L);
                i__2 = m1 - ib1 + 1;
                daxpy_(&i__2, &dwork[i2lole], &a[m1 + ij1 + ib1 * a_dim1], lda,
                    &a[ib1 + ib1 * a_dim1], lda);
                i__2 = m1 - ib1 + 1;
                daxpy_(&i__2, &dwork[i2lole + sdim], &a[m1 + ij1 + ib1 * a_dim1], lda,
                    &a[ib1 + 1 + ib1 * a_dim1], lda);
                i__2 = m1 - ib1 + 1;
                dgemv_("Transpose", &dim1, &i__2, &c_b50, &dwork[iwrk], &dim1, &dwork[i2upri],
                    &c__1, &dwork[i2lori], &a[m1 + ij1 + ib1 * a_dim1], lda, 9L);
                i__2 = m1 - ij1 + 1;
                dlacpy_("Full", &dim1, &i__2, &a[ib1 + (m1 + ij1) * a_dim1], lda, &dwork[iwrk],
                    &dim1, 4L);
                i__2 = m1 - ij1 + 1;
                dgemm_("Transpose", "No Transpose", &dim1, &i__2, &dim1, &c_b50, &dwork[i2uple],
                    &sdim, &dwork[iwrk], &dim1, &c_b51, &a[ib1 + (m1 + ij1) * a_dim1], lda, 9L,
                    12L);
                i__2 = m1 - ij1 + 1;
                daxpy_(&i__2, &dwork[i2lole], &a[m1 + ij1 + (m1 + ij1) * a_dim1], lda,
                    &a[ib1 + (m1 + ij1) * a_dim1], lda);
                i__2 = m1 - ij1 + 1;
                daxpy_(&i__2, &dwork[i2lole + sdim], &a[m1 + ij1 + (m1 + ij1) * a_dim1], lda,
                    &a[ib1 + 1 + (m1 + ij1) * a_dim1], lda);
                i__2 = m1 - ij1 + 1;
                dgemv_("Transpose", &dim1, &i__2, &c_b50, &dwork[iwrk], &dim1, &dwork[i2upri],
                    &c__1, &dwork[i2lori], &a[m1 + ij1 + (m1 + ij1) * a_dim1], lda, 9L);
                if (m2 > 0) {
                    dlacpy_(
                        "Full", &dim1, &m4, &a[ib1 + i2 * a_dim1], lda, &dwork[iwrk], &dim1, 4L);
                    dgemm_("Transpose", "No Transpose", &dim1, &m4, &dim1, &c_b50, &dwork[i2uple],
                        &sdim, &dwork[iwrk], &dim1, &c_b51, &a[ib1 + i2 * a_dim1], lda, 9L, 12L);
                    daxpy_(&m4, &dwork[i2lole], &a[m1 + ij1 + i2 * a_dim1], lda,
                        &a[ib1 + i2 * a_dim1], lda);
                    daxpy_(&m4, &dwork[i2lole + sdim], &a[m1 + ij1 + i2 * a_dim1], lda,
                        &a[ib1 + 1 + i2 * a_dim1], lda);
                    dgemv_("Transpose", &dim1, &m4, &c_b50, &dwork[iwrk], &dim1, &dwork[i2upri],
                        &c__1, &dwork[i2lori], &a[m1 + ij1 + i2 * a_dim1], lda, 9L);
                }
                /*              Update B. */
                dlacpy_("Full", &nr, &dim1, &b[ib1 * b_dim1 + 1], ldb, &dwork[iwrk], &nr, 4L);
                dgemm_("No Transpose", "No Transpose", &nr, &dim1, &dim1, &c_b50, &dwork[iwrk], &nr,
                    &dwork[i1uple], &sdim, &c_b51, &b[ib1 * b_dim1 + 1], ldb, 12L, 12L);
                i__2 = nr - 1;
                daxpy_(&i__2, &dwork[i1lole], &b[(m1 + ij1) * b_dim1 + 1], &c__1,
                    &b[ib1 * b_dim1 + 1], &c__1);
                i__2 = nr - 1;
                daxpy_(&i__2, &dwork[i1lole + sdim], &b[(m1 + ij1) * b_dim1 + 1], &c__1,
                    &b[(ib1 + 1) * b_dim1 + 1], &c__1);
                b[nr + (m1 + ij1) * b_dim1] = 0.;
                dgemv_("No Transpose", &nr, &dim1, &c_b50, &dwork[iwrk], &nr, &dwork[i1upri], &c__1,
                    &dwork[i1lori], &b[(m1 + ij1) * b_dim1 + 1], &c__1, 12L);
                dlacpy_("Full", &nrow, &dim1, &b[i1 + ib1 * b_dim1], ldb, &dwork[iwrk], &nrow, 4L);
                dgemm_("No Transpose", "No Transpose", &nrow, &dim1, &dim1, &c_b50, &dwork[iwrk],
                    &nrow, &dwork[i1uple], &sdim, &c_b51, &b[i1 + ib1 * b_dim1], ldb, 12L, 12L);
                daxpy_(&nrow, &dwork[i1lole], &b[i1 + (m1 + ij1) * b_dim1], &c__1,
                    &b[i1 + ib1 * b_dim1], &c__1);
                daxpy_(&nrow, &dwork[i1lole + sdim], &b[i1 + (m1 + ij1) * b_dim1], &c__1,
                    &b[i1 + (ib1 + 1) * b_dim1], &c__1);
                dgemv_("No Transpose", &nrow, &dim1, &c_b50, &dwork[iwrk], &nrow, &dwork[i1upri],
                    &c__1, &dwork[i1lori], &b[i1 + (m1 + ij1) * b_dim1], &c__1, 12L);
                i__2 = m1 - ib1 + 1;
                dlacpy_("Full", &dim1, &i__2, &b[ib1 + ib1 * b_dim1], ldb, &dwork[iwrk], &dim1, 4L);
                i__2 = m1 - ib1 + 1;
                dgemm_("Transpose", "No Transpose", &dim1, &i__2, &dim1, &c_b50, &dwork[i2uple],
                    &sdim, &dwork[iwrk], &dim1, &c_b51, &b[ib1 + ib1 * b_dim1], ldb, 9L, 12L);
                i__2 = m1 - ib1 + 1;
                daxpy_(&i__2, &dwork[i2lole], &b[m1 + ij1 + ib1 * b_dim1], ldb,
                    &b[ib1 + ib1 * b_dim1], ldb);
                i__2 = m1 - ib1 + 1;
                daxpy_(&i__2, &dwork[i2lole + sdim], &b[m1 + ij1 + ib1 * b_dim1], ldb,
                    &b[ib1 + 1 + ib1 * b_dim1], ldb);
                i__2 = m1 - ib1 + 1;
                dgemv_("Transpose", &dim1, &i__2, &c_b50, &dwork[iwrk], &dim1, &dwork[i2upri],
                    &c__1, &dwork[i2lori], &b[m1 + ij1 + ib1 * b_dim1], ldb, 9L);
                i__2 = m1 - ij1 + 1;
                dlacpy_("Full", &dim1, &i__2, &b[ib1 + (m1 + ij1) * b_dim1], ldb, &dwork[iwrk],
                    &dim1, 4L);
                i__2 = m1 - ij1 + 1;
                dgemm_("Transpose", "No Transpose", &dim1, &i__2, &dim1, &c_b50, &dwork[i2uple],
                    &sdim, &dwork[iwrk], &dim1, &c_b51, &b[ib1 + (m1 + ij1) * b_dim1], ldb, 9L,
                    12L);
                i__2 = m1 - ij1 + 1;
                daxpy_(&i__2, &dwork[i2lole], &b[m1 + ij1 + (m1 + ij1) * b_dim1], ldb,
                    &b[ib1 + (m1 + ij1) * b_dim1], ldb);
                i__2 = m1 - ij1 + 1;
                daxpy_(&i__2, &dwork[i2lole + sdim], &b[m1 + ij1 + (m1 + ij1) * b_dim1], ldb,
                    &b[ib1 + 1 + (m1 + ij1) * b_dim1], ldb);
                i__2 = m1 - ij1 + 1;
                dgemv_("Transpose", &dim1, &i__2, &c_b50, &dwork[iwrk], &dim1, &dwork[i2upri],
                    &c__1, &dwork[i2lori], &b[m1 + ij1 + (m1 + ij1) * b_dim1], ldb, 9L);
                if (m2 > 0) {
                    dlacpy_(
                        "Full", &dim1, &m4, &b[ib1 + i2 * b_dim1], ldb, &dwork[iwrk], &dim1, 4L);
                    dgemm_("Transpose", "No Transpose", &dim1, &m4, &dim1, &c_b50, &dwork[i2uple],
                        &sdim, &dwork[iwrk], &dim1, &c_b51, &b[ib1 + i2 * b_dim1], ldb, 9L, 12L);
                    daxpy_(&m4, &dwork[i2lole], &b[m1 + ij1 + i2 * b_dim1], ldb,
                        &b[ib1 + i2 * b_dim1], ldb);
                    daxpy_(&m4, &dwork[i2lole + sdim], &b[m1 + ij1 + i2 * b_dim1], ldb,
                        &b[ib1 + 1 + i2 * b_dim1], ldb);
                    dgemv_("Transpose", &dim1, &m4, &c_b50, &dwork[iwrk], &dim1, &dwork[i2upri],
                        &c__1, &dwork[i2lori], &b[m1 + ij1 + i2 * b_dim1], ldb, 9L);
                }
                /*              Update Q1. */
                if (lcmpq1) {
                    dlacpy_("Full", n, &dim1, &q1[ib1 * q1_dim1 + 1], ldq1, &dwork[iwrk], n, 4L);
                    dgemm_("No Transpose", "No Transpose", n, &dim1, &dim1, &c_b50, &dwork[iwrk], n,
                        &dwork[i1uple], &sdim, &c_b51, &q1[ib1 * q1_dim1 + 1], ldq1, 12L, 12L);
                    daxpy_(n, &dwork[i1lole], &q1[(m1 + ij1) * q1_dim1 + 1], &c__1,
                        &q1[ib1 * q1_dim1 + 1], &c__1);
                    daxpy_(n, &dwork[i1lole + sdim], &q1[(m1 + ij1) * q1_dim1 + 1], &c__1,
                        &q1[(ib1 + 1) * q1_dim1 + 1], &c__1);
                    dgemv_("No Transpose", n, &dim1, &c_b50, &dwork[iwrk], n, &dwork[i1upri], &c__1,
                        &dwork[i1lori], &q1[(m1 + ij1) * q1_dim1 + 1], &c__1, 12L);
                }
                /*              Update Q2. */
                if (lcmpq2) {
                    dlacpy_("Full", n, &dim1, &q2[ib1 * q2_dim1 + 1], ldq2, &dwork[iwrk], n, 4L);
                    dgemm_("No Transpose", "No Transpose", n, &dim1, &dim1, &c_b50, &dwork[iwrk], n,
                        &dwork[i2uple], &sdim, &c_b51, &q2[ib1 * q2_dim1 + 1], ldq2, 12L, 12L);
                    daxpy_(n, &dwork[i2lole], &q2[(m1 + ij1) * q2_dim1 + 1], &c__1,
                        &q2[ib1 * q2_dim1 + 1], &c__1);
                    daxpy_(n, &dwork[i2lole + sdim], &q2[(m1 + ij1) * q2_dim1 + 1], &c__1,
                        &q2[(ib1 + 1) * q2_dim1 + 1], &c__1);
                    dgemv_("No Transpose", n, &dim1, &c_b50, &dwork[iwrk], n, &dwork[i2upri], &c__1,
                        &dwork[i2lori], &q2[(m1 + ij1) * q2_dim1 + 1], &c__1, 12L);
                }
            } else {
                /*              Update A. */
                dcopy_(&nr, &a[ib1 * a_dim1 + 1], &c__1, &dwork[iwrk], &c__1);
                dscal_(&nr, &dwork[i1uple], &a[ib1 * a_dim1 + 1], &c__1);
                i__2 = nr - 1;
                daxpy_(&i__2, &dwork[i1lole], &a[(m1 + ij1) * a_dim1 + 1], &c__1,
                    &a[ib1 * a_dim1 + 1], &c__1);
                i__2 = nr - 1;
                dscal_(&i__2, &dwork[i1lori], &a[(m1 + ij1) * a_dim1 + 1], &c__1);
                i__2 = nr - 1;
                daxpy_(
                    &i__2, &dwork[i1upri], &dwork[iwrk], &c__1, &a[(m1 + ij1) * a_dim1 + 1], &c__1);
                a[nr + (m1 + ij1) * a_dim1] = dwork[i1upri] * dwork[iwrk + nr - 1];
                dcopy_(&nrow, &a[i1 + ib1 * a_dim1], &c__1, &dwork[iwrk], &c__1);
                dscal_(&nrow, &dwork[i1uple], &a[i1 + ib1 * a_dim1], &c__1);
                daxpy_(&nrow, &dwork[i1lole], &a[i1 + (m1 + ij1) * a_dim1], &c__1,
                    &a[i1 + ib1 * a_dim1], &c__1);
                dscal_(&nrow, &dwork[i1lori], &a[i1 + (m1 + ij1) * a_dim1], &c__1);
                daxpy_(&nrow, &dwork[i1upri], &dwork[iwrk], &c__1, &a[i1 + (m1 + ij1) * a_dim1],
                    &c__1);
                i__2 = m1 - ib1 + 1;
                dcopy_(&i__2, &a[ib1 + ib1 * a_dim1], lda, &dwork[iwrk], &c__1);
                i__2 = m1 - ib1 + 1;
                dscal_(&i__2, &dwork[i2uple], &a[ib1 + ib1 * a_dim1], lda);
                i__2 = m1 - ib1 + 1;
                daxpy_(&i__2, &dwork[i2lole], &a[m1 + ij1 + ib1 * a_dim1], lda,
                    &a[ib1 + ib1 * a_dim1], lda);
                i__2 = m1 - ib1 + 1;
                dscal_(&i__2, &dwork[i2lori], &a[m1 + ij1 + ib1 * a_dim1], lda);
                i__2 = m1 - ib1 + 1;
                daxpy_(
                    &i__2, &dwork[i2upri], &dwork[iwrk], &c__1, &a[m1 + ij1 + ib1 * a_dim1], lda);
                i__2 = m1 - ij1 + 1;
                dcopy_(&i__2, &a[ib1 + (m1 + ij1) * a_dim1], lda, &dwork[iwrk], &c__1);
                i__2 = m1 - ij1 + 1;
                dscal_(&i__2, &dwork[i2uple], &a[ib1 + (m1 + ij1) * a_dim1], lda);
                i__2 = m1 - ij1 + 1;
                daxpy_(&i__2, &dwork[i2lole], &a[m1 + ij1 + (m1 + ij1) * a_dim1], lda,
                    &a[ib1 + (m1 + ij1) * a_dim1], lda);
                i__2 = m1 - ij1 + 1;
                dscal_(&i__2, &dwork[i2lori], &a[m1 + ij1 + (m1 + ij1) * a_dim1], lda);
                i__2 = m1 - ij1 + 1;
                daxpy_(&i__2, &dwork[i2upri], &dwork[iwrk], &c__1,
                    &a[m1 + ij1 + (m1 + ij1) * a_dim1], lda);
                if (m2 > 0) {
                    dcopy_(&m4, &a[ib1 + i2 * a_dim1], lda, &dwork[iwrk], &c__1);
                    dscal_(&m4, &dwork[i2uple], &a[ib1 + i2 * a_dim1], lda);
                    daxpy_(&m4, &dwork[i2lole], &a[m1 + ij1 + i2 * a_dim1], lda,
                        &a[ib1 + i2 * a_dim1], lda);
                    dscal_(&m4, &dwork[i2lori], &a[m1 + ij1 + i2 * a_dim1], lda);
                    daxpy_(
                        &m4, &dwork[i2upri], &dwork[iwrk], &c__1, &a[m1 + ij1 + i2 * a_dim1], lda);
                }
                /*              Update B. */
                dcopy_(&nr, &b[ib1 * b_dim1 + 1], &c__1, &dwork[iwrk], &c__1);
                dscal_(&nr, &dwork[i1uple], &b[ib1 * b_dim1 + 1], &c__1);
                i__2 = nr - 1;
                daxpy_(&i__2, &dwork[i1lole], &b[(m1 + ij1) * b_dim1 + 1], &c__1,
                    &b[ib1 * b_dim1 + 1], &c__1);
                i__2 = nr - 1;
                dscal_(&i__2, &dwork[i1lori], &b[(m1 + ij1) * b_dim1 + 1], &c__1);
                i__2 = nr - 1;
                daxpy_(
                    &i__2, &dwork[i1upri], &dwork[iwrk], &c__1, &b[(m1 + ij1) * b_dim1 + 1], &c__1);
                b[nr + (m1 + ij1) * b_dim1] = dwork[i1upri] * dwork[iwrk + nr - 1];
                dcopy_(&nrow, &b[i1 + ib1 * b_dim1], &c__1, &dwork[iwrk], &c__1);
                dscal_(&nrow, &dwork[i1uple], &b[i1 + ib1 * b_dim1], &c__1);
                daxpy_(&nrow, &dwork[i1lole], &b[i1 + (m1 + ij1) * b_dim1], &c__1,
                    &b[i1 + ib1 * b_dim1], &c__1);
                dscal_(&nrow, &dwork[i1lori], &b[i1 + (m1 + ij1) * b_dim1], &c__1);
                daxpy_(&nrow, &dwork[i1upri], &dwork[iwrk], &c__1, &b[i1 + (m1 + ij1) * b_dim1],
                    &c__1);
                i__2 = m1 - ib1 + 1;
                dcopy_(&i__2, &b[ib1 + ib1 * b_dim1], ldb, &dwork[iwrk], &c__1);
                i__2 = m1 - ib1 + 1;
                dscal_(&i__2, &dwork[i2uple], &b[ib1 + ib1 * b_dim1], ldb);
                i__2 = m1 - ib1 + 1;
                daxpy_(&i__2, &dwork[i2lole], &b[m1 + ij1 + ib1 * b_dim1], ldb,
                    &b[ib1 + ib1 * b_dim1], ldb);
                i__2 = m1 - ib1 + 1;
                dscal_(&i__2, &dwork[i2lori], &b[m1 + ij1 + ib1 * b_dim1], ldb);
                i__2 = m1 - ib1 + 1;
                daxpy_(
                    &i__2, &dwork[i2upri], &dwork[iwrk], &c__1, &b[m1 + ij1 + ib1 * b_dim1], ldb);
                i__2 = m1 - ij1 + 1;
                dcopy_(&i__2, &b[ib1 + (m1 + ij1) * b_dim1], ldb, &dwork[iwrk], &c__1);
                i__2 = m1 - ij1 + 1;
                dscal_(&i__2, &dwork[i2uple], &b[ib1 + (m1 + ij1) * b_dim1], ldb);
                i__2 = m1 - ij1 + 1;
                daxpy_(&i__2, &dwork[i2lole], &b[m1 + ij1 + (m1 + ij1) * b_dim1], ldb,
                    &b[ib1 + (m1 + ij1) * b_dim1], ldb);
                i__2 = m1 - ij1 + 1;
                dscal_(&i__2, &dwork[i2lori], &b[m1 + ij1 + (m1 + ij1) * b_dim1], ldb);
                i__2 = m1 - ij1 + 1;
                daxpy_(&i__2, &dwork[i2upri], &dwork[iwrk], &c__1,
                    &b[m1 + ij1 + (m1 + ij1) * b_dim1], ldb);
                if (m2 > 0) {
                    dcopy_(&m4, &b[ib1 + i2 * b_dim1], ldb, &dwork[iwrk], &c__1);
                    dscal_(&m4, &dwork[i2uple], &b[ib1 + i2 * b_dim1], ldb);
                    daxpy_(&m4, &dwork[i2lole], &b[m1 + ij1 + i2 * b_dim1], ldb,
                        &b[ib1 + i2 * b_dim1], ldb);
                    dscal_(&m4, &dwork[i2lori], &b[m1 + ij1 + i2 * b_dim1], ldb);
                    daxpy_(
                        &m4, &dwork[i2upri], &dwork[iwrk], &c__1, &b[m1 + ij1 + i2 * b_dim1], ldb);
                }
                /*              Update Q1. */
                if (lcmpq1) {
                    dcopy_(n, &q1[ib1 * q1_dim1 + 1], &c__1, &dwork[iwrk], &c__1);
                    dscal_(n, &dwork[i1uple], &q1[ib1 * q1_dim1 + 1], &c__1);
                    daxpy_(n, &dwork[i1lole], &q1[(m1 + ij1) * q1_dim1 + 1], &c__1,
                        &q1[ib1 * q1_dim1 + 1], &c__1);
                    dscal_(n, &dwork[i1lori], &q1[(m1 + ij1) * q1_dim1 + 1], &c__1);
                    daxpy_(n, &dwork[i1upri], &dwork[iwrk], &c__1, &q1[(m1 + ij1) * q1_dim1 + 1],
                        &c__1);
                }
                /*              Update Q2. */
                if (lcmpq2) {
                    dcopy_(n, &q2[ib1 * q2_dim1 + 1], &c__1, &dwork[iwrk], &c__1);
                    dscal_(n, &dwork[i2uple], &q2[ib1 * q2_dim1 + 1], &c__1);
                    daxpy_(n, &dwork[i2lole], &q2[(m1 + ij1) * q2_dim1 + 1], &c__1,
                        &q2[ib1 * q2_dim1 + 1], &c__1);
                    dscal_(n, &dwork[i2lori], &q2[(m1 + ij1) * q2_dim1 + 1], &c__1);
                    daxpy_(n, &dwork[i2upri], &dwork[iwrk], &c__1, &q2[(m1 + ij1) * q2_dim1 + 1],
                        &c__1);
                }
            }
            /* L50: */
        }
        /* L60: */
    }
    /*     Triangularize the lower right subpencil aAA2 - bBB2. */
    if (m2 > 1) {
        i__1 = m4 - 2;
        dlacpy_("Full", n, &i__1, &a[(i2 + 1) * a_dim1 + 1], lda, &dwork[1], n, 4L);
        i__1 = m2 - 1;
        for (i__ = 1; i__ <= i__1; ++i__) {
            dcopy_(n, &dwork[*n * (i__ - 1) + 1], &c__1, &a[((m1 + i__ << 1) + 1) * a_dim1 + 1],
                &c__1);
            dcopy_(
                n, &dwork[*n * (m2 + i__ - 2) + 1], &c__1, &a[(m1 + i__ << 1) * a_dim1 + 1], &c__1);
            /* L70: */
        }
        i__1 = m4 - 2;
        i__2 = m4 - 2;
        dlacpy_("Full", &i__1, &m4, &a[i2 + 1 + i2 * a_dim1], lda, &dwork[1], &i__2, 4L);
        i__1 = m2 - 1;
        for (i__ = 1; i__ <= i__1; ++i__) {
            i__2 = m4 - 2;
            dcopy_(&m4, &dwork[i__], &i__2, &a[(m1 + i__ << 1) + 1 + i2 * a_dim1], lda);
            i__2 = m4 - 2;
            dcopy_(&m4, &dwork[m2 + i__ - 1], &i__2, &a[(m1 + i__ << 1) + i2 * a_dim1], lda);
            /* L80: */
        }
        i__1 = m4 - 2;
        dlacpy_("Full", n, &i__1, &b[(i2 + 1) * b_dim1 + 1], ldb, &dwork[1], n, 4L);
        i__1 = m2 - 1;
        for (i__ = 1; i__ <= i__1; ++i__) {
            dcopy_(n, &dwork[*n * (i__ - 1) + 1], &c__1, &b[((m1 + i__ << 1) + 1) * b_dim1 + 1],
                &c__1);
            dcopy_(
                n, &dwork[*n * (m2 + i__ - 2) + 1], &c__1, &b[(m1 + i__ << 1) * b_dim1 + 1], &c__1);
            /* L90: */
        }
        i__1 = m4 - 2;
        i__2 = m4 - 2;
        dlacpy_("Full", &i__1, &m4, &b[i2 + 1 + i2 * b_dim1], ldb, &dwork[1], &i__2, 4L);
        i__1 = m2 - 1;
        for (i__ = 1; i__ <= i__1; ++i__) {
            i__2 = m4 - 2;
            dcopy_(&m4, &dwork[i__], &i__2, &b[(m1 + i__ << 1) + 1 + i2 * b_dim1], ldb);
            i__2 = m4 - 2;
            dcopy_(&m4, &dwork[m2 + i__ - 1], &i__2, &b[(m1 + i__ << 1) + i2 * b_dim1], ldb);
            /* L100: */
        }
        if (lcmpq1) {
            i__1 = m4 - 2;
            dlacpy_("Full", n, &i__1, &q1[(i2 + 1) * q1_dim1 + 1], ldq1, &dwork[1], n, 4L);
            i__1 = m2 - 1;
            for (i__ = 1; i__ <= i__1; ++i__) {
                dcopy_(n, &dwork[*n * (i__ - 1) + 1], &c__1,
                    &q1[((m1 + i__ << 1) + 1) * q1_dim1 + 1], &c__1);
                dcopy_(n, &dwork[*n * (m2 + i__ - 2) + 1], &c__1,
                    &q1[(m1 + i__ << 1) * q1_dim1 + 1], &c__1);
                /* L110: */
            }
        }
        if (lcmpq2) {
            i__1 = m4 - 2;
            dlacpy_("Full", n, &i__1, &q2[(i2 + 1) * q2_dim1 + 1], ldq2, &dwork[1], n, 4L);
            i__1 = m2 - 1;
            for (i__ = 1; i__ <= i__1; ++i__) {
                dcopy_(n, &dwork[*n * (i__ - 1) + 1], &c__1,
                    &q2[((m1 + i__ << 1) + 1) * q2_dim1 + 1], &c__1);
                dcopy_(n, &dwork[*n * (m2 + i__ - 2) + 1], &c__1,
                    &q2[(m1 + i__ << 1) * q2_dim1 + 1], &c__1);
                /* L120: */
            }
        }
    }
    dwork[1] = (doublereal)optwrk;
    return 0;
    /* *** Last line of MB04HD *** */
} /* mb04hd_ */
