/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static doublereal c_b16 = 0.;
static doublereal c_b17 = 1.;
static integer c__1 = 1;
static doublereal c_b43 = -1.;
static integer c__0 = 0;
static integer c__4 = 4;

EXPORTSYMBOL /* Subroutine */ int mb04bd_(job, compq1, compq2, n, a, lda, de, ldde, c1, ldc1, vw,
    ldvw, q1, ldq1, q2, ldq2, b, ldb, f, ldf, c2, ldc2, alphar, alphai, beta, iwork, liwork, dwork,
    ldwork, info, job_len, compq1_len, compq2_len) char *job,
    *compq1, *compq2;
integer* n;
doublereal* a;
integer* lda;
doublereal* de;
integer* ldde;
doublereal* c1;
integer* ldc1;
doublereal* vw;
integer* ldvw;
doublereal* q1;
integer* ldq1;
doublereal* q2;
integer* ldq2;
doublereal* b;
integer* ldb;
doublereal* f;
integer* ldf;
doublereal* c2;
integer* ldc2;
doublereal *alphar, *alphai, *beta;
integer *iwork, *liwork;
doublereal* dwork;
integer *ldwork, *info;
ftnlen job_len;
ftnlen compq1_len;
ftnlen compq2_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, c1_dim1, c1_offset, c2_dim1, c2_offset, de_dim1,
        de_offset, f_dim1, f_offset, q1_dim1, q1_offset, q2_dim1, q2_offset, vw_dim1, vw_offset,
        i__1, i__2, i__3, i__4;
    doublereal d__1, d__2;
    doublecomplex z__1, z__2;
    /* Builtin functions */
    /* Subroutine */ int s_copy();
    double pow_dd();
    void z_sqrt();
    double d_imag();
    /* Local variables */
    static doublereal base, emin, prec;
    static integer imat;
    static doublereal emax;
    extern doublereal ddot_();
    static integer idum[1];
    static char cmpq[16];
    extern /* Subroutine */ int drot_();
    static logical ltri;
    static integer iwrk;
    extern /* Subroutine */ int dsyr2_(), ma02ad_(), mb03bd_();
    static integer i__, j, k, m;
    extern /* Subroutine */ int mb01ld_(), mb01md_(), mb01nd_(), dlarf_(), dgemm_();
    extern logical lsame_();
    static char cmpsc[16];
    extern /* Subroutine */ int dcopy_();
    static integer iwarn;
    extern /* Subroutine */ int daxpy_();
    static integer optdw;
    extern /* Subroutine */ int dsymv_();
    extern doublereal dlapy2_();
    static logical lcmpq1, lcmpq2, liniq1, liniq2, lupdq1, lupdq2;
    static doublereal co;
    extern doublereal dlamch_();
    static integer mm;
    static doublereal si;
    extern /* Subroutine */ int dlarfg_();
    static doublereal mu, nu;
    extern /* Subroutine */ int dlacpy_(), dlartg_(), dlaset_(), xerbla_();
    static integer mj1, mj2, mj3, mk1, mk2, mk3;
    static doublecomplex eig;
    static doublereal dum[1], tmp1, tmp2;
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
    /*     To compute the eigenvalues of a real N-by-N skew-Hamiltonian/ */
    /*     Hamiltonian pencil aS - bH with */
    /*           (  A  D  )         (  C  V  ) */
    /*       S = (        ) and H = (        ).                           (1) */
    /*           (  E  A' )         (  W -C' ) */
    /*     Optionally, if JOB = 'T', decompositions of S and H will be */
    /*     computed via orthogonal transformations Q1 and Q2 as follows: */
    /*                       (  Aout  Dout  ) */
    /*       Q1' S J Q1 J' = (              ), */
    /*                       (   0    Aout' ) */
    /*                       (  Bout  Fout  ) */
    /*       J' Q2' J S Q2 = (              ) =: T,                       (2) */
    /*                       (   0    Bout' ) */
    /*                  (  C1out  Vout  )            (  0  I  ) */
    /*       Q1' H Q2 = (               ), where J = (        ) */
    /*                  (  0     C2out' )            ( -I  0  ) */
    /*     and Aout, Bout, C1out are upper triangular, C2out is upper quasi- */
    /*     triangular and Dout and Fout are skew-symmetric. The notation M' */
    /*     denotes the transpose of the matrix M. */
    /*     Optionally, if COMPQ1 = 'I', the orthogonal transformation matrix */
    /*     Q1 will be computed. */
    /*     Optionally, if COMPQ2 = 'I', the orthogonal transformation matrix */
    /*     Q2 will be computed. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     JOB     CHARACTER*1 */
    /*             Specifies the computation to be performed, as follows: */
    /*             = 'E': compute the eigenvalues only; S and H will not */
    /*                    necessarily be transformed as in (2). */
    /*             = 'T': put S and H into the forms in (2) and return the */
    /*                    eigenvalues in ALPHAR, ALPHAI and BETA. */
    /*     COMPQ1  CHARACTER*1 */
    /*             Specifies whether to compute the orthogonal transformation */
    /*             matrix Q1, as follows: */
    /*             = 'N':  Q1 is not computed; */
    /*             = 'I':  the array Q1 is initialized internally to the unit */
    /*                     matrix, and the orthogonal matrix Q1 is returned; */
    /*             = 'U':  the array Q1 contains an orthogonal matrix Q on */
    /*                     entry, and the product Q*Q1 is returned, where Q1 */
    /*                     is the product of the orthogonal transformations */
    /*                     that are applied to the pencil aS - bH to reduce */
    /*                     S and H to the forms in (2), for COMPQ1 = 'I'. */
    /*     COMPQ2  CHARACTER*1 */
    /*             Specifies whether to compute the orthogonal transformation */
    /*             matrix Q2, as follows: */
    /*             = 'N':  Q2 is not computed; */
    /*             = 'I':  on exit, the array Q2 contains the orthogonal */
    /*                     matrix Q2; */
    /*             = 'U':  on exit, the array Q2 contains the matrix product */
    /*                     J*Q*J'*Q2, where Q2 is the product of the */
    /*                     orthogonal transformations that are applied to */
    /*                     the pencil aS - bH to reduce S and H to the forms */
    /*                     in (2), for COMPQ2 = 'I'. */
    /*                     Setting COMPQ2 <> 'N' assumes COMPQ2 = COMPQ1. */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the pencil aS - bH.  N has to be even. */
    /*     A       (input/output) DOUBLE PRECISION array, dimension */
    /*                            (LDA, N/2) */
    /*             On entry, the leading N/2-by-N/2 part of this array must */
    /*             contain the matrix A. */
    /*             On exit, if JOB = 'T', the leading N/2-by-N/2 part of this */
    /*             array contains the matrix Aout; otherwise, it contains the */
    /*             upper triangular matrix A obtained just before the */
    /*             application of the periodic QZ algorithm. */
    /*     LDA     INTEGER */
    /*             The leading dimension of the array A.  LDA >= MAX(1, N/2). */
    /*     DE      (input/output) DOUBLE PRECISION array, dimension */
    /*                            (LDDE, N/2+1) */
    /*             On entry, the leading N/2-by-N/2 strictly lower triangular */
    /*             part of this array must contain the strictly lower */
    /*             triangular part of the skew-symmetric matrix E, and the */
    /*             N/2-by-N/2 strictly upper triangular part of the submatrix */
    /*             in the columns 2 to N/2+1 of this array must contain the */
    /*             strictly upper triangular part of the skew-symmetric */
    /*             matrix D. */
    /*             The entries on the diagonal and the first superdiagonal of */
    /*             this array need not be set, but are assumed to be zero. */
    /*             On exit, if JOB = 'T', the leading N/2-by-N/2 strictly */
    /*             upper triangular part of the submatrix in the columns */
    /*             2 to N/2+1 of this array contains the strictly upper */
    /*             triangular part of the skew-symmetric matrix Dout. */
    /*             If JOB = 'E', the leading N/2-by-N/2 strictly upper */
    /*             triangular part of the submatrix in the columns 2 to N/2+1 */
    /*             of this array contains the strictly upper triangular part */
    /*             of the skew-symmetric matrix D just before the application */
    /*             of the periodic QZ algorithm. The remaining entries are */
    /*             meaningless. */
    /*     LDDE    INTEGER */
    /*             The leading dimension of the array DE. */
    /*             LDDE >= MAX(1, N/2). */
    /*     C1      (input/output) DOUBLE PRECISION array, dimension */
    /*                           (LDC1, N/2) */
    /*             On entry, the leading N/2-by-N/2 part of this array must */
    /*             contain the matrix C1 = C. */
    /*             On exit, if JOB = 'T', the leading N/2-by-N/2 part of this */
    /*             array contains the matrix C1out; otherwise, it contains the */
    /*             upper triangular matrix C1 obtained just before the */
    /*             application of the periodic QZ algorithm. */
    /*     LDC1    INTEGER */
    /*             The leading dimension of the array C1. */
    /*             LDC1 >= MAX(1, N/2). */
    /*     VW      (input/output) DOUBLE PRECISION array, dimension */
    /*                            (LDVW, N/2+1) */
    /*             On entry, the leading N/2-by-N/2 lower triangular part of */
    /*             this array must contain the lower triangular part of the */
    /*             symmetric matrix W, and the N/2-by-N/2 upper triangular */
    /*             part of the submatrix in the columns 2 to N/2+1 of this */
    /*             array must contain the upper triangular part of the */
    /*             symmetric matrix V. */
    /*             On exit, if JOB = 'T', the N/2-by-N/2 part in the columns */
    /*             2 to N/2+1 of this array contains the matrix Vout. */
    /*             If JOB = 'E', the N/2-by-N/2 part in the columns 2 to */
    /*             N/2+1 of this array contains the matrix V just before the */
    /*             application of the periodic QZ algorithm. */
    /*     LDVW    INTEGER */
    /*             The leading dimension of the array VW. */
    /*             LDVW >= MAX(1, N/2). */
    /*     Q1      (input/output) DOUBLE PRECISION array, dimension (LDQ1, N) */
    /*             On entry, if COMPQ1 = 'U', then the leading N-by-N part of */
    /*             this array must contain a given matrix Q, and on exit, */
    /*             the leading N-by-N part of this array contains the product */
    /*             of the input matrix Q and the transformation matrix Q1 */
    /*             used to transform the matrices S and H. */
    /*             On exit, if COMPQ1 = 'I', then the leading N-by-N part of */
    /*             this array contains the orthogonal transformation matrix */
    /*             Q1. */
    /*             If COMPQ1 = 'N', this array is not referenced. */
    /*     LDQ1    INTEGER */
    /*             The leading dimension of the array Q1. */
    /*             LDQ1 >= 1,         if COMPQ1 = 'N'; */
    /*             LDQ1 >= MAX(1, N), if COMPQ1 = 'I' or COMPQ1 = 'U'. */
    /*     Q2      (output) DOUBLE PRECISION array, dimension (LDQ2, N) */
    /*             On exit, if COMPQ2 = 'U', then the leading N-by-N part of */
    /*             this array contains the product of the matrix J*Q*J' and */
    /*             the transformation matrix Q2 used to transform the */
    /*             matrices S and H. */
    /*             On exit, if COMPQ2 = 'I', then the leading N-by-N part of */
    /*             this array contains the orthogonal transformation matrix */
    /*             Q2. */
    /*             If COMPQ2 = 'N', this array is not referenced. */
    /*     LDQ2    INTEGER */
    /*             The leading dimension of the array Q2. */
    /*             LDQ2 >= 1,         if COMPQ2 = 'N'; */
    /*             LDQ2 >= MAX(1, N), if COMPQ2 = 'I' or COMPQ2 = 'U'. */
    /*     B       (output) DOUBLE PRECISION array, dimension (LDB, N/2) */
    /*             On exit, if JOB = 'T', the leading N/2-by-N/2 part of this */
    /*             array contains the matrix Bout; otherwise, it contains the */
    /*             upper triangular matrix B obtained just before the */
    /*             application of the periodic QZ algorithm. */
    /*     LDB     INTEGER */
    /*             The leading dimension of the array B.  LDB >= MAX(1, N/2). */
    /*     F       (output) DOUBLE PRECISION array, dimension (LDF, N/2) */
    /*             On exit, if JOB = 'T', the leading N/2-by-N/2 strictly */
    /*             upper triangular part of this array contains the strictly */
    /*             upper triangular part of the skew-symmetric matrix Fout. */
    /*             If JOB = 'E', the leading N/2-by-N/2 strictly upper */
    /*             triangular part of this array contains the strictly upper */
    /*             triangular part of the skew-symmetric matrix F just before */
    /*             the application of the periodic QZ algorithm. */
    /*             The entries on the leading N/2-by-N/2 lower triangular */
    /*             part of this array are not referenced. */
    /*     LDF     INTEGER */
    /*             The leading dimension of the array F.  LDF >= MAX(1, N/2). */
    /*     C2      (output) DOUBLE PRECISION array, dimension (LDC2, N/2) */
    /*             On exit, if JOB = 'T', the leading N/2-by-N/2 part of this */
    /*             array contains the matrix C2out; otherwise, it contains */
    /*             the upper Hessenberg matrix C2 obtained just before the */
    /*             application of the periodic QZ algorithm. */
    /*     LDC2    INTEGER */
    /*             The leading dimension of the array C2. */
    /*             LDC2 >= MAX(1, N/2). */
    /*     ALPHAR  (output) DOUBLE PRECISION array, dimension (N/2) */
    /*             The real parts of each scalar alpha defining an eigenvalue */
    /*             of the pencil aS - bH. */
    /*     ALPHAI  (output) DOUBLE PRECISION array, dimension (N/2) */
    /*             The imaginary parts of each scalar alpha defining an */
    /*             eigenvalue of the pencil aS - bH. */
    /*             If ALPHAI(j) is zero, then the j-th eigenvalue is real. */
    /*     BETA    (output) DOUBLE PRECISION array, dimension (N/2) */
    /*             The scalars beta that define the eigenvalues of the pencil */
    /*             aS - bH. */
    /*             Together, the quantities alpha = (ALPHAR(j),ALPHAI(j)) and */
    /*             beta = BETA(j) represent the j-th eigenvalue of the pencil */
    /*             aS - bH, in the form lambda = alpha/beta. Since lambda may */
    /*             overflow, the ratios should not, in general, be computed. */
    /*             Due to the skew-Hamiltonian/Hamiltonian structure of the */
    /*             pencil, for every eigenvalue lambda, -lambda is also an */
    /*             eigenvalue, and thus it has only to be saved once in */
    /*             ALPHAR, ALPHAI and BETA. */
    /*             Specifically, only eigenvalues with imaginary parts */
    /*             greater than or equal to zero are stored; their conjugate */
    /*             eigenvalues are not stored. If imaginary parts are zero */
    /*             (i.e., for real eigenvalues), only positive eigenvalues */
    /*             are stored. */
    /*     Workspace */
    /*     IWORK   INTEGER array, dimension (LIWORK) */
    /*     LIWORK  INTEGER */
    /*             The dimension of the array IWORK. */
    /*             LIWORK >= N/2+12. */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             On exit, if INFO = 0, DWORK(1) returns the optimal LDWORK. */
    /*             On exit, if INFO = -27, DWORK(1) returns the minimum */
    /*             value of LDWORK. */
    /*     LDWORK  INTEGER */
    /*             The dimension of the array DWORK. */
    /*             If JOB = 'E' and COMPQ1 = 'N' and COMPQ2 = 'N', */
    /*                LDWORK >= N**2 + MAX(N,32); */
    /*             if JOB = 'T' or COMPQ1 <> 'N' or COMPQ2 <> 'N', */
    /*                LDWORK >= 2*N**2 + MAX(N,32). */
    /*             For good performance LDWORK should generally be larger. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0: succesful exit; */
    /*             < 0: if INFO = -i, the i-th argument had an illegal value; */
    /*             = 1: problem during computation of the eigenvalues; */
    /*             = 2: periodic QZ algorithm did not converge in the SLICOT */
    /*                  Library subroutine MB03BD. */
    /*     METHOD */
    /*     The algorithm uses Givens rotations and Householder reflections to */
    /*     annihilate elements in S, T, and H such that A, B, and C1 are */
    /*     upper triangular and C2 is upper Hessenberg. Finally, the periodic */
    /*     QZ algorithm is applied to transform C2 to upper quasi-triangular */
    /*     form while A, B, and C1 stay in upper triangular form. */
    /*     See also page 27 in [1] for more details. */
    /*     REFERENCES */
    /*     [1] Benner, P., Byers, R., Losse, P., Mehrmann, V. and Xu, H. */
    /*         Numerical Solution of Real Skew-Hamiltonian/Hamiltonian */
    /*         Eigenproblems. */
    /*         Tech. Rep., Technical University Chemnitz, Germany, */
    /*         Nov. 2007. */
    /*     NUMERICAL ASPECTS */
    /*                                                               3 */
    /*     The algorithm is numerically backward stable and needs O(N ) real */
    /*     floating point operations. */
    /*     CONTRIBUTOR */
    /*     Matthias Voigt, Fakultaet fuer Mathematik, Technische Universitaet */
    /*     Chemnitz, October 16, 2008. */
    /*     REVISIONS */
    /*     V. Sima, Oct. 2009 (SLICOT version of the routine DHAUTR). */
    /*     V. Sima, Nov. 2010. */
    /*     KEYWORDS */
    /*     periodic QZ algorithm, upper (quasi-)triangular matrix, */
    /*     skew-Hamiltonian/Hamiltonian pencil. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. Local Arrays .. */
    /*     .. External Functions .. */
    /*     .. External Subroutines .. */
    /*     ... Intrinsic Functions .. */
    /*     .. Executable Statements .. */
    /*     Decode the input arguments. */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    de_dim1 = *ldde;
    de_offset = de_dim1 + 1;
    de -= de_offset;
    c1_dim1 = *ldc1;
    c1_offset = c1_dim1 + 1;
    c1 -= c1_offset;
    vw_dim1 = *ldvw;
    vw_offset = vw_dim1 + 1;
    vw -= vw_offset;
    q1_dim1 = *ldq1;
    q1_offset = q1_dim1 + 1;
    q1 -= q1_offset;
    q2_dim1 = *ldq2;
    q2_offset = q2_dim1 + 1;
    q2 -= q2_offset;
    b_dim1 = *ldb;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    f_dim1 = *ldf;
    f_offset = f_dim1 + 1;
    f -= f_offset;
    c2_dim1 = *ldc2;
    c2_offset = c2_dim1 + 1;
    c2 -= c2_offset;
    --alphar;
    --alphai;
    --beta;
    --iwork;
    --dwork;
    /* Function Body */
    m = *n / 2;
    mm = m * m;
    ltri = lsame_(job, "T", 1L, 1L);
    liniq1 = lsame_(compq1, "I", 1L, 1L);
    liniq2 = lsame_(compq2, "I", 1L, 1L);
    lupdq1 = lsame_(compq1, "U", 1L, 1L);
    lupdq2 = lsame_(compq2, "U", 1L, 1L);
    lcmpq1 = lupdq1 || liniq1;
    lcmpq2 = lupdq2 || liniq2;
    if (ltri || lcmpq1 || lcmpq2) {
        optdw = (mm << 3) + max(*n, 32);
    } else {
        optdw = (mm << 2) + max(*n, 32);
    }
    /*     Test the input arguments. */
    *info = 0;
    if (!(lsame_(job, "E", 1L, 1L) || ltri)) {
        *info = -1;
    } else if (!(lsame_(compq1, "N", 1L, 1L) || lcmpq1)) {
        *info = -2;
    } else if (!(lsame_(compq2, "N", 1L, 1L) || lcmpq2)) {
        *info = -3;
    } else if (liniq2 && !liniq1 || lupdq2 && !lupdq1) {
        *info = -3;
    } else if (*n < 0 || *n % 2 != 0) {
        *info = -4;
    } else if (*lda < max(1, m)) {
        *info = -6;
    } else if (*ldde < max(1, m)) {
        *info = -8;
    } else if (*ldc1 < max(1, m)) {
        *info = -10;
    } else if (*ldvw < max(1, m)) {
        *info = -12;
    } else if (*ldq1 < 1 || lcmpq1 && *ldq1 < *n) {
        *info = -14;
    } else if (*ldq2 < 1 || lcmpq2 && *ldq2 < *n) {
        *info = -16;
    } else if (*ldb < max(1, m)) {
        *info = -18;
    } else if (*ldf < max(1, m)) {
        *info = -20;
    } else if (*ldc2 < max(1, m)) {
        *info = -22;
    } else if (*liwork < m + 12) {
        *info = -27;
    } else if (*ldwork < optdw) {
        *info = -29;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("MB04BD", &i__1, 6L);
        return 0;
    }
    /*     Quick return if possible. */
    if (*n == 0) {
        dwork[1] = 1.;
        return 0;
    }
    /*     Determine machine constants. */
    base = dlamch_("Base", 4L);
    emin = dlamch_("Minimum Exponent", 16L);
    emax = dlamch_("Largest Exponent", 16L);
    prec = dlamch_("Precision", 9L);
    /*     STEP 1: Reduce S to skew-Hamiltonian triangular form. */
    if (liniq1) {
        dlaset_("Full", n, n, &c_b16, &c_b17, &q1[q1_offset], ldq1, 4L);
    }
    dum[0] = 0.;
    i__1 = m - 1;
    for (k = 1; k <= i__1; ++k) {
        /*        Generate elementary reflector H(k) = I - nu * v * v' to */
        /*        annihilate E(k+2:m,k). */
        /* Computing MIN */
        i__2 = k + 2;
        mk2 = min(i__2, m);
        mk3 = mk2 + 1;
        tmp1 = de[k + 1 + k * de_dim1];
        i__2 = m - k;
        dlarfg_(&i__2, &tmp1, &de[mk2 + k * de_dim1], &c__1, &nu);
        if (nu != 0.) {
            de[k + 1 + k * de_dim1] = 1.;
            /*           Apply H(k) from both sides to E(k+1:m,k+1:m). */
            /*           Compute  x := nu * E(k+1:m,k+1:m) * v. */
            i__2 = m - k;
            mb01md_("Lower", &i__2, &nu, &de[k + 1 + (k + 1) * de_dim1], ldde,
                &de[k + 1 + k * de_dim1], &c__1, &c_b16, &dwork[1], &c__1, 5L);
            /*           Compute  w := x - 1/2 * nu * (x'*v) * v in x. */
            i__2 = m - k;
            mu = nu * -.5 * ddot_(&i__2, &dwork[1], &c__1, &de[k + 1 + k * de_dim1], &c__1);
            i__2 = m - k;
            daxpy_(&i__2, &mu, &de[k + 1 + k * de_dim1], &c__1, &dwork[1], &c__1);
            /*           Apply the transformation as a skew-symmetric rank-2 update: */
            /*                E := E + v * w' - w * v'. */
            i__2 = m - k;
            mb01nd_("Lower", &i__2, &c_b17, &de[k + 1 + k * de_dim1], &c__1, &dwork[1], &c__1,
                &de[k + 1 + (k + 1) * de_dim1], ldde, 5L);
            /*           Apply H(k) to W(k+1:m,1:k) from the left (and implicitly to */
            /*           W(1:k,k+1:m) from the right). */
            i__2 = m - k;
            dlarf_("Left", &i__2, &k, &de[k + 1 + k * de_dim1], &c__1, &nu, &vw[k + 1 + vw_dim1],
                ldvw, &dwork[1], 4L);
            /*           Apply H(k) from both sides to W(k+1:m,k+1:m). */
            /*           Compute  x := nu * W(k+1:m,k+1:m) * v. */
            i__2 = m - k;
            dsymv_("Lower", &i__2, &nu, &vw[k + 1 + (k + 1) * vw_dim1], ldvw,
                &de[k + 1 + k * de_dim1], &c__1, &c_b16, &dwork[1], &c__1, 5L);
            /*           Compute  w := x - 1/2 * nu * (x'*v) * v. */
            i__2 = m - k;
            mu = nu * -.5 * ddot_(&i__2, &dwork[1], &c__1, &de[k + 1 + k * de_dim1], &c__1);
            i__2 = m - k;
            daxpy_(&i__2, &mu, &de[k + 1 + k * de_dim1], &c__1, &dwork[1], &c__1);
            /*           Apply the transformation as a rank-2 update: */
            /*                W := W - v * w' - w * v'. */
            i__2 = m - k;
            dsyr2_("Lower", &i__2, &c_b43, &de[k + 1 + k * de_dim1], &c__1, &dwork[1], &c__1,
                &vw[k + 1 + (k + 1) * vw_dim1], ldvw, 5L);
            /*           Apply H(k) from the right hand side to A(1:m,k+1:m) and */
            /*           C1(1:m,k+1:m). */
            i__2 = m - k;
            dlarf_("Right", &m, &i__2, &de[k + 1 + k * de_dim1], &c__1, &nu,
                &a[(k + 1) * a_dim1 + 1], lda, &dwork[1], 5L);
            i__2 = m - k;
            dlarf_("Right", &m, &i__2, &de[k + 1 + k * de_dim1], &c__1, &nu,
                &c1[(k + 1) * c1_dim1 + 1], ldc1, &dwork[1], 5L);
            if (lcmpq1) {
                /*              Apply H(k) from the right hand side to Q1(1:n,m+k+1:n). */
                i__2 = m - k;
                dlarf_("Right", n, &i__2, &de[k + 1 + k * de_dim1], &c__1, &nu,
                    &q1[(m + k + 1) * q1_dim1 + 1], ldq1, &dwork[1], 5L);
            }
            de[k + 1 + k * de_dim1] = tmp1;
        }
        /*        Determine a Givens rotation to annihilate E(k+1,k) from the */
        /*        left. */
        tmp2 = a[k + 1 + k * a_dim1];
        dlartg_(&tmp2, &de[k + 1 + k * de_dim1], &co, &si, &a[k + 1 + k * a_dim1]);
        /*        Update A, E and D. */
        i__2 = m - k - 1;
        drot_(&i__2, &de[mk2 + (k + 1) * de_dim1], &c__1, &a[k + 1 + mk2 * a_dim1], lda, &co, &si);
        drot_(&k, &a[(k + 1) * a_dim1 + 1], &c__1, &de[(k + 2) * de_dim1 + 1], &c__1, &co, &si);
        i__2 = m - k - 1;
        drot_(&i__2, &de[k + 1 + mk3 * de_dim1], ldde, &a[mk2 + (k + 1) * a_dim1], &c__1, &co, &si);
        /*        Update C1, W and V. */
        d__1 = -si;
        drot_(&k, &vw[k + 1 + vw_dim1], ldvw, &c1[k + 1 + c1_dim1], ldc1, &co, &d__1);
        i__2 = m - k - 1;
        d__1 = -si;
        drot_(&i__2, &vw[mk2 + (k + 1) * vw_dim1], &c__1, &c1[k + 1 + mk2 * c1_dim1], ldc1, &co,
            &d__1);
        drot_(&k, &c1[(k + 1) * c1_dim1 + 1], &c__1, &vw[(k + 2) * vw_dim1 + 1], &c__1, &co, &si);
        i__2 = m - k - 1;
        d__1 = -si;
        drot_(&i__2, &vw[k + 1 + mk3 * vw_dim1], ldvw, &c1[mk2 + (k + 1) * c1_dim1], &c__1, &co,
            &d__1);
        /*        Fix the diagonal part. */
        tmp1 = c1[k + 1 + (k + 1) * c1_dim1];
        tmp2 = vw[k + 1 + (k + 2) * vw_dim1];
        c1[k + 1 + (k + 1) * c1_dim1]
            = (co - si) * (co + si) * tmp1 + co * si * (vw[k + 1 + (k + 1) * vw_dim1] + tmp2);
        tmp1 = co * 2. * si * tmp1;
        /* Computing 2nd power */
        d__1 = co;
        /* Computing 2nd power */
        d__2 = si;
        vw[k + 1 + (k + 2) * vw_dim1]
            = d__1 * d__1 * tmp2 - d__2 * d__2 * vw[k + 1 + (k + 1) * vw_dim1] - tmp1;
        /* Computing 2nd power */
        d__1 = co;
        /* Computing 2nd power */
        d__2 = si;
        vw[k + 1 + (k + 1) * vw_dim1]
            = d__1 * d__1 * vw[k + 1 + (k + 1) * vw_dim1] - d__2 * d__2 * tmp2 - tmp1;
        if (lcmpq1) {
            /*           Update Q1. */
            drot_(n, &q1[(k + 1) * q1_dim1 + 1], &c__1, &q1[(m + k + 1) * q1_dim1 + 1], &c__1, &co,
                &si);
        }
        /*        Generate elementary reflector P(k) to annihilate A(k+1:m,k). */
        tmp1 = a[k + k * a_dim1];
        i__2 = m - k + 1;
        dlarfg_(&i__2, &tmp1, &a[k + 1 + k * a_dim1], &c__1, &nu);
        if (nu != 0.) {
            a[k + k * a_dim1] = 1.;
            /*           Apply P(k) from the left hand side to A(k:m,k+1:m). */
            i__2 = m - k + 1;
            i__3 = m - k;
            dlarf_("Left", &i__2, &i__3, &a[k + k * a_dim1], &c__1, &nu, &a[k + (k + 1) * a_dim1],
                lda, &dwork[1], 4L);
            /*           Apply P(k) to D(1:k-1,k:m) from the right (and implicitly */
            /*           to D(k:m,1:k-1) from the left). */
            i__2 = k - 1;
            i__3 = m - k + 1;
            dlarf_("Right", &i__2, &i__3, &a[k + k * a_dim1], &c__1, &nu,
                &de[(k + 1) * de_dim1 + 1], ldde, &dwork[1], 5L);
            /*           Apply P(k) from both sides to D(k:m,k:m). */
            /*           Compute  x := nu * D(k:m,k:m) * v. */
            i__2 = m - k + 1;
            mb01md_("Upper", &i__2, &nu, &de[k + (k + 1) * de_dim1], ldde, &a[k + k * a_dim1],
                &c__1, &c_b16, &dwork[1], &c__1, 5L);
            /*           Compute  w := x - 1/2 * nu * (x'*v) * v in x. */
            i__2 = m - k + 1;
            mu = nu * -.5 * ddot_(&i__2, &dwork[1], &c__1, &a[k + k * a_dim1], &c__1);
            i__2 = m - k + 1;
            daxpy_(&i__2, &mu, &a[k + k * a_dim1], &c__1, &dwork[1], &c__1);
            /*           Apply the transformation as a skew-symmetric rank-2 update: */
            /*                D := D + v * w' - w * v'. */
            i__2 = m - k + 1;
            mb01nd_("Upper", &i__2, &c_b17, &a[k + k * a_dim1], &c__1, &dwork[1], &c__1,
                &de[k + (k + 1) * de_dim1], ldde, 5L);
            /*           Apply P(k) from the left hand side to C1(k:m,k+1:m). */
            i__2 = m - k + 1;
            dlarf_("Left", &i__2, &m, &a[k + k * a_dim1], &c__1, &nu, &c1[k + c1_dim1], ldc1,
                &dwork[1], 4L);
            /*           Apply P(k) to V(1:k-1,k:m) from the right (and implicitly */
            /*           to V(k:m,1:k-1) from the left). */
            i__2 = k - 1;
            i__3 = m - k + 1;
            dlarf_("Right", &i__2, &i__3, &a[k + k * a_dim1], &c__1, &nu,
                &vw[(k + 1) * vw_dim1 + 1], ldvw, &dwork[1], 5L);
            /*           Apply P(k) from both sides to V(k:m,k:m). */
            /*           Compute  x := nu * V(k:m,k:m) * v. */
            i__2 = m - k + 1;
            dsymv_("Upper", &i__2, &nu, &vw[k + (k + 1) * vw_dim1], ldvw, &a[k + k * a_dim1], &c__1,
                &c_b16, &dwork[1], &c__1, 5L);
            /*           Compute  w := x - 1/2 * nu * (x'*v) * v. */
            i__2 = m - k + 1;
            mu = nu * -.5 * ddot_(&i__2, &dwork[1], &c__1, &a[k + k * a_dim1], &c__1);
            i__2 = m - k + 1;
            daxpy_(&i__2, &mu, &a[k + k * a_dim1], &c__1, &dwork[1], &c__1);
            /*           Apply the transformation as a rank-2 update: */
            /*                V := V - v * w' - w * v'. */
            i__2 = m - k + 1;
            dsyr2_("Upper", &i__2, &c_b43, &a[k + k * a_dim1], &c__1, &dwork[1], &c__1,
                &vw[k + (k + 1) * vw_dim1], ldvw, 5L);
            if (lcmpq1) {
                /*              Apply P(k) from the right hand side to Q1(1:n,k:m). */
                i__2 = m - k + 1;
                dlarf_("Right", n, &i__2, &a[k + k * a_dim1], &c__1, &nu, &q1[k * q1_dim1 + 1],
                    ldq1, &dwork[1], 5L);
            }
            a[k + k * a_dim1] = tmp1;
        }
        /*        Set A(k+1:m,k) to zero in order to be able to apply MB03BD. */
        i__2 = m - k;
        dcopy_(&i__2, dum, &c__0, &a[k + 1 + k * a_dim1], &c__1);
        /* L10: */
    }
    /*     The following operations do not preserve the Hamiltonian structure */
    /*     of H. -C1 is copied to C2. The lower triangular part of W(1:m,1:m) */
    /*     and its transpose are stored in DWORK. Then, the transpose of the */
    /*     upper triangular part of V(1:m,1:m) is saved in the lower */
    /*     triangular part of VW(1:m,2:m+1). */
    dlacpy_("Full", &m, &m, &a[a_offset], lda, &b[b_offset], ldb, 4L);
    dlacpy_("Upper", &m, &m, &de[(de_dim1 << 1) + 1], ldde, &f[f_offset], ldf, 5L);
    i__1 = m;
    for (j = 1; j <= i__1; ++j) {
        i__2 = m;
        for (i__ = 1; i__ <= i__2; ++i__) {
            c2[i__ + j * c2_dim1] = -c1[i__ + j * c1_dim1];
            /* L20: */
        }
        /* L30: */
    }
    dlacpy_("Lower", &m, &m, &vw[vw_offset], ldvw, &dwork[1], &m, 5L);
    ma02ad_("Lower", &m, &m, &vw[vw_offset], ldvw, &dwork[1], &m, 5L);
    ma02ad_("Upper", &m, &m, &vw[(vw_dim1 << 1) + 1], ldvw, &vw[(vw_dim1 << 1) + 1], ldvw, 5L);
    if (lcmpq2) {
        dlacpy_("Full", &m, &m, &q1[m + 1 + (m + 1) * q1_dim1], ldq1, &q2[q2_offset], ldq2, 4L);
        i__1 = m;
        for (j = 1; j <= i__1; ++j) {
            i__2 = *n;
            for (i__ = m + 1; i__ <= i__2; ++i__) {
                q2[i__ + j * q2_dim1] = -q1[i__ - m + (j + m) * q1_dim1];
                /* L40: */
            }
            /* L50: */
        }
        i__1 = *n;
        for (j = m + 1; j <= i__1; ++j) {
            i__2 = m;
            for (i__ = 1; i__ <= i__2; ++i__) {
                q2[i__ + j * q2_dim1] = -q1[i__ + m + (j - m) * q1_dim1];
                /* L60: */
            }
            /* L70: */
        }
        dlacpy_("Full", &m, &m, &q1[q1_offset], ldq1, &q2[m + 1 + (m + 1) * q2_dim1], ldq2, 4L);
    }
    /*     STEP 2: Eliminations in H. */
    i__1 = m;
    for (k = 1; k <= i__1; ++k) {
        /* Computing MIN */
        i__2 = k + 1;
        mk1 = min(i__2, m);
        /*        I. Annihilate W(k:m-1,k). */
        i__2 = m - 1;
        for (j = k; j <= i__2; ++j) {
            /* Computing MIN */
            i__3 = j + 3, i__4 = m + 1;
            mj3 = min(i__3, i__4);
            /*           Determine a Givens rotation to annihilate W(j,k) from the */
            /*           left. */
            dlartg_(&dwork[(k - 1) * m + j + 1], &dwork[(k - 1) * m + j], &co, &si, &tmp1);
            /*           Update C2 and W. */
            drot_(&m, &c2[(j + 1) * c2_dim1 + 1], &c__1, &c2[j * c2_dim1 + 1], &c__1, &co, &si);
            dwork[(k - 1) * m + j + 1] = tmp1;
            dwork[(k - 1) * m + j] = 0.;
            i__3 = m - k;
            drot_(&i__3, &dwork[k * m + j + 1], &m, &dwork[k * m + j], &m, &co, &si);
            /*           Update A. */
            drot_(&j, &a[(j + 1) * a_dim1 + 1], &c__1, &a[j * a_dim1 + 1], &c__1, &co, &si);
            tmp1 = -si * a[j + 1 + (j + 1) * a_dim1];
            a[j + 1 + (j + 1) * a_dim1] = co * a[j + 1 + (j + 1) * a_dim1];
            if (lcmpq1) {
                /*              Update Q1. */
                drot_(n, &q1[(m + j + 1) * q1_dim1 + 1], &c__1, &q1[(m + j) * q1_dim1 + 1], &c__1,
                    &co, &si);
            }
            /*           Determine a Givens rotation to annihilate A(j+1,j) from the */
            /*           left. */
            dlartg_(&a[j + j * a_dim1], &tmp1, &co, &si, &tmp2);
            /*           Update A and D. */
            a[j + j * a_dim1] = tmp2;
            i__3 = m - j;
            drot_(
                &i__3, &a[j + (j + 1) * a_dim1], lda, &a[j + 1 + (j + 1) * a_dim1], lda, &co, &si);
            i__3 = j - 1;
            drot_(&i__3, &de[(j + 1) * de_dim1 + 1], &c__1, &de[(j + 2) * de_dim1 + 1], &c__1, &co,
                &si);
            i__3 = m - j - 1;
            drot_(&i__3, &de[j + mj3 * de_dim1], ldde, &de[j + 1 + mj3 * de_dim1], ldde, &co, &si);
            /*           Update C1 and V. */
            i__3 = m - k + 1;
            drot_(&i__3, &c1[j + k * c1_dim1], ldc1, &c1[j + 1 + k * c1_dim1], ldc1, &co, &si);
            drot_(&m, &vw[j + (vw_dim1 << 1)], ldvw, &vw[j + 1 + (vw_dim1 << 1)], ldvw, &co, &si);
            if (lcmpq1) {
                /*              Update Q1. */
                drot_(n, &q1[j * q1_dim1 + 1], &c__1, &q1[(j + 1) * q1_dim1 + 1], &c__1, &co, &si);
            }
            /* L80: */
        }
        /*        II. Annihilate W(m,k). */
        /*        Determine a Givens rotation to annihilate W(m,k) from the left. */
        dlartg_(&c1[m + k * c1_dim1], &dwork[m * k], &co, &si, &tmp1);
        /*        Update C1 and W. */
        c1[m + k * c1_dim1] = tmp1;
        dwork[m * k] = 0.;
        i__2 = m - k;
        drot_(&i__2, &c1[m + mk1 * c1_dim1], ldc1, &dwork[m * mk1], &m, &co, &si);
        drot_(&m, &vw[m + (vw_dim1 << 1)], ldvw, &c2[m * c2_dim1 + 1], &c__1, &co, &si);
        /*        Update A and D. */
        i__2 = m - 1;
        drot_(&i__2, &a[m * a_dim1 + 1], &c__1, &de[(m + 1) * de_dim1 + 1], &c__1, &co, &si);
        if (lcmpq1) {
            /*           Update Q1. */
            drot_(n, &q1[m * q1_dim1 + 1], &c__1, &q1[*n * q1_dim1 + 1], &c__1, &co, &si);
        }
        /*        III. Annihilate C1(k+1:m,k). */
        i__2 = k + 1;
        for (j = m; j >= i__2; --j) {
            /* Computing MIN */
            i__3 = j + 2, i__4 = m + 1;
            mj2 = min(i__3, i__4);
            /*           Determine a Givens rotation to annihilate C1(j,k) from the */
            /*           left. */
            dlartg_(&c1[j - 1 + k * c1_dim1], &c1[j + k * c1_dim1], &co, &si, &tmp1);
            /*           Update C1 and V. */
            c1[j - 1 + k * c1_dim1] = tmp1;
            c1[j + k * c1_dim1] = 0.;
            i__3 = m - k;
            drot_(&i__3, &c1[j - 1 + mk1 * c1_dim1], ldc1, &c1[j + mk1 * c1_dim1], ldc1, &co, &si);
            drot_(&m, &vw[j - 1 + (vw_dim1 << 1)], ldvw, &vw[j + (vw_dim1 << 1)], ldvw, &co, &si);
            /*           Update A and D. */
            tmp1 = -si * a[j - 1 + (j - 1) * a_dim1];
            a[j - 1 + (j - 1) * a_dim1] = co * a[j - 1 + (j - 1) * a_dim1];
            i__3 = m - j + 1;
            drot_(&i__3, &a[j - 1 + j * a_dim1], lda, &a[j + j * a_dim1], lda, &co, &si);
            i__3 = j - 2;
            drot_(&i__3, &de[j * de_dim1 + 1], &c__1, &de[(j + 1) * de_dim1 + 1], &c__1, &co, &si);
            i__3 = m - j;
            drot_(&i__3, &de[j - 1 + mj2 * de_dim1], ldde, &de[j + mj2 * de_dim1], ldde, &co, &si);
            if (lcmpq1) {
                /*              Update Q1. */
                drot_(n, &q1[(j - 1) * q1_dim1 + 1], &c__1, &q1[j * q1_dim1 + 1], &c__1, &co, &si);
            }
            /*           Determine a Givens rotation to annihilate A(j,j-1) from the */
            /*           right. */
            dlartg_(&a[j + j * a_dim1], &tmp1, &co, &si, &tmp2);
            /*           Update A. */
            a[j + j * a_dim1] = tmp2;
            i__3 = j - 1;
            drot_(&i__3, &a[j * a_dim1 + 1], &c__1, &a[(j - 1) * a_dim1 + 1], &c__1, &co, &si);
            /*           Update C2 and W. */
            drot_(&m, &c2[j * c2_dim1 + 1], &c__1, &c2[(j - 1) * c2_dim1 + 1], &c__1, &co, &si);
            i__3 = m - k + 1;
            drot_(&i__3, &dwork[(k - 1) * m + j], &m, &dwork[(k - 1) * m + j - 1], &m, &co, &si);
            if (lcmpq1) {
                /*              Update Q1. */
                drot_(n, &q1[(m + j) * q1_dim1 + 1], &c__1, &q1[(m + j - 1) * q1_dim1 + 1], &c__1,
                    &co, &si);
            }
            /* L90: */
        }
        /*        IV. Annihilate W(k,k+1:m-1). */
        i__2 = m - 1;
        for (j = k + 1; j <= i__2; ++j) {
            /* Computing MIN */
            i__3 = j + 2;
            mj2 = min(i__3, m);
            /*           Determine a Givens rotation to annihilate W(k,j) from the */
            /*           right. */
            dlartg_(&dwork[j * m + k], &dwork[(j - 1) * m + k], &co, &si, &tmp1);
            /*           Update C1 and W. */
            drot_(&m, &c1[(j + 1) * c1_dim1 + 1], &c__1, &c1[j * c1_dim1 + 1], &c__1, &co, &si);
            dwork[(j - 1) * m + k] = 0.;
            dwork[j * m + k] = tmp1;
            i__3 = m - k;
            drot_(&i__3, &dwork[j * m + mk1], &c__1, &dwork[(j - 1) * m + k + 1], &c__1, &co, &si);
            /*           Update B. */
            drot_(&j, &b[(j + 1) * b_dim1 + 1], &c__1, &b[j * b_dim1 + 1], &c__1, &co, &si);
            tmp1 = -si * b[j + 1 + (j + 1) * b_dim1];
            b[j + 1 + (j + 1) * b_dim1] = co * b[j + 1 + (j + 1) * b_dim1];
            if (lcmpq2) {
                /*              Update Q2. */
                drot_(n, &q2[(j + 1) * q2_dim1 + 1], &c__1, &q2[j * q2_dim1 + 1], &c__1, &co, &si);
            }
            /*           Determine a Givens rotation to annihilate B(j+1,j) from the */
            /*           left. */
            dlartg_(&b[j + j * b_dim1], &tmp1, &co, &si, &tmp2);
            /*           Update B and F. */
            b[j + j * b_dim1] = tmp2;
            i__3 = m - j;
            drot_(
                &i__3, &b[j + (j + 1) * b_dim1], ldb, &b[j + 1 + (j + 1) * b_dim1], ldb, &co, &si);
            i__3 = j - 1;
            drot_(&i__3, &f[j * f_dim1 + 1], &c__1, &f[(j + 1) * f_dim1 + 1], &c__1, &co, &si);
            i__3 = m - j - 1;
            drot_(&i__3, &f[j + mj2 * f_dim1], ldf, &f[j + 1 + mj2 * f_dim1], ldf, &co, &si);
            /*           Update C2 and V. */
            i__3 = m - k + 1;
            drot_(&i__3, &c2[j + k * c2_dim1], ldc2, &c2[j + 1 + k * c2_dim1], ldc2, &co, &si);
            drot_(
                &m, &vw[(j + 1) * vw_dim1 + 1], &c__1, &vw[(j + 2) * vw_dim1 + 1], &c__1, &co, &si);
            if (lcmpq2) {
                /*              Update Q2. */
                drot_(n, &q2[(m + j) * q2_dim1 + 1], &c__1, &q2[(m + j + 1) * q2_dim1 + 1], &c__1,
                    &co, &si);
            }
            /* L100: */
        }
        /*        V. Annihilate W(k,m). */
        if (k < m) {
            /*           Determine a Givens rotation to annihilate W(k,m) from the */
            /*           right. */
            dlartg_(&c2[m + k * c2_dim1], &dwork[(m - 1) * m + k], &co, &si, &tmp1);
            /*           Update C1, C2, W and V. */
            drot_(&m, &vw[(m + 1) * vw_dim1 + 1], &c__1, &c1[m * c1_dim1 + 1], &c__1, &co, &si);
            c2[m + k * c2_dim1] = tmp1;
            dwork[(m - 1) * m + k] = 0.;
            i__2 = m - k;
            drot_(&i__2, &c2[m + (k + 1) * c2_dim1], ldc2, &dwork[(m - 1) * m + k + 1], &c__1, &co,
                &si);
            /*           Update B and F. */
            i__2 = m - 1;
            drot_(&i__2, &f[m * f_dim1 + 1], &c__1, &b[m * b_dim1 + 1], &c__1, &co, &si);
            if (lcmpq2) {
                /*               Update Q2. */
                drot_(n, &q2[*n * q2_dim1 + 1], &c__1, &q2[m * q2_dim1 + 1], &c__1, &co, &si);
            }
        } else {
            /*           Determine a Givens rotation to annihilate W(m,m) from the */
            /*           left. */
            dlartg_(&c1[m + m * c1_dim1], &dwork[mm], &co, &si, &tmp1);
            /*           Update C1, C2, W and V. */
            c1[m + m * c1_dim1] = tmp1;
            dwork[mm] = 0.;
            drot_(&m, &vw[m + (vw_dim1 << 1)], ldvw, &c2[m * c2_dim1 + 1], &c__1, &co, &si);
            /*           Update A and D. */
            i__2 = m - 1;
            drot_(&i__2, &a[m * a_dim1 + 1], &c__1, &de[(m + 1) * de_dim1 + 1], &c__1, &co, &si);
            if (lcmpq1) {
                /*              Update Q1. */
                drot_(n, &q1[m * q1_dim1 + 1], &c__1, &q1[*n * q1_dim1 + 1], &c__1, &co, &si);
            }
        }
        /*        VI. Annihilate C2(k+2:m,k). */
        i__2 = k + 2;
        for (j = m; j >= i__2; --j) {
            /* Computing MIN */
            i__3 = j + 1;
            mj1 = min(i__3, m);
            /*           Determine a Givens rotation to annihilate C2(j,k) from the */
            /*           left. */
            dlartg_(&c2[j - 1 + k * c2_dim1], &c2[j + k * c2_dim1], &co, &si, &tmp1);
            /*           Update C2 and V. */
            c2[j - 1 + k * c2_dim1] = tmp1;
            c2[j + k * c2_dim1] = 0.;
            i__3 = m - k;
            drot_(&i__3, &c2[j - 1 + mk1 * c2_dim1], ldc2, &c2[j + mk1 * c2_dim1], ldc2, &co, &si);
            drot_(&m, &vw[j * vw_dim1 + 1], &c__1, &vw[(j + 1) * vw_dim1 + 1], &c__1, &co, &si);
            /*           Update B and F. */
            i__3 = m - j + 1;
            drot_(&i__3, &b[j - 1 + j * b_dim1], ldb, &b[j + j * b_dim1], ldb, &co, &si);
            tmp1 = -si * b[j - 1 + (j - 1) * b_dim1];
            b[j - 1 + (j - 1) * b_dim1] = co * b[j - 1 + (j - 1) * b_dim1];
            i__3 = j - 2;
            drot_(&i__3, &f[(j - 1) * f_dim1 + 1], &c__1, &f[j * f_dim1 + 1], &c__1, &co, &si);
            i__3 = m - j;
            drot_(&i__3, &f[j - 1 + mj1 * f_dim1], ldf, &f[j + mj1 * f_dim1], ldf, &co, &si);
            if (lcmpq2) {
                /*              Update Q2. */
                drot_(n, &q2[(m + j - 1) * q2_dim1 + 1], &c__1, &q2[(m + j) * q2_dim1 + 1], &c__1,
                    &co, &si);
            }
            /*           Determine a Givens rotation to annihilate B(j,j-1) from the */
            /*           right. */
            dlartg_(&b[j + j * b_dim1], &tmp1, &co, &si, &tmp2);
            b[j + j * b_dim1] = tmp2;
            /*           Update B. */
            i__3 = j - 1;
            drot_(&i__3, &b[j * b_dim1 + 1], &c__1, &b[(j - 1) * b_dim1 + 1], &c__1, &co, &si);
            /*           Update C1 and W. */
            drot_(&m, &c1[j * c1_dim1 + 1], &c__1, &c1[(j - 1) * c1_dim1 + 1], &c__1, &co, &si);
            i__3 = m - k + 1;
            drot_(&i__3, &dwork[(j - 1) * m + k], &c__1, &dwork[(j - 2) * m + k], &c__1, &co, &si);
            if (lcmpq2) {
                /*              Update Q2. */
                drot_(n, &q2[j * q2_dim1 + 1], &c__1, &q2[(j - 1) * q2_dim1 + 1], &c__1, &co, &si);
            }
            /* L110: */
        }
        /* L120: */
    }
    /*                     (  A1  D1  )      (  B1  F1  )      (  C11  V1  ) */
    /*     Now we have S = (          ), T = (          ), H = (           ), */
    /*                     (   0  A1' )      (   0  B1' )      (   0  C21' ) */
    /*     where A1, B1, and C11 are upper triangular, C21 is upper */
    /*     Hessenberg, and D1 and F1 are skew-symmetric. */
    /*     STEP 3: Apply the periodic QZ algorithm to the generalized matrix */
    /*                           -1       -1 */
    /*             product C21 A1   C11 B1   in order to make C21 upper */
    /*             quasi-triangular. */
    /*     Determine the mode of computations. */
    if (ltri || lcmpq1 || lcmpq2) {
        s_copy(cmpq, "Initialize", 16L, 10L);
        imat = (mm << 2) + 1;
        iwrk = (mm << 3) + 1;
    } else {
        s_copy(cmpq, "No Computation", 16L, 14L);
        imat = 1;
        iwrk = (mm << 2) + 1;
    }
    if (ltri) {
        s_copy(cmpsc, "Schur Form", 16L, 10L);
    } else {
        s_copy(cmpsc, "Eigenvalues Only", 16L, 16L);
    }
    /*     Save matrices in the form that is required by MB03BD. */
    dlacpy_("Full", &m, &m, &c2[c2_offset], ldc2, &dwork[imat], &m, 4L);
    dlacpy_("Full", &m, &m, &a[a_offset], lda, &dwork[imat + mm], &m, 4L);
    dlacpy_("Full", &m, &m, &c1[c1_offset], ldc1, &dwork[imat + (mm << 1)], &m, 4L);
    dlacpy_("Full", &m, &m, &b[b_offset], ldb, &dwork[imat + mm * 3], &m, 4L);
    iwork[1] = 1;
    iwork[2] = -1;
    iwork[3] = 1;
    iwork[4] = -1;
    /*     Apply periodic QZ algorithm. */
    /*     Workspace:    need   OPTDW; */
    /*                   prefer larger. */
    i__1 = *liwork - (m + 4);
    i__2 = *ldwork - iwrk + 1;
    mb03bd_(cmpsc, "Careful", cmpq, idum, &c__4, &m, &c__1, &c__1, &m, &iwork[1], &dwork[imat], &m,
        &m, &dwork[1], &m, &m, &alphar[1], &alphai[1], &beta[1], &iwork[5], &iwork[m + 5], &i__1,
        &dwork[iwrk], &i__2, &iwarn, info, 16L, 7L, 16L);
    if (iwarn > 0) {
        *info = 1;
        return 0;
    } else if (*info > 0) {
        *info = 2;
        return 0;
    }
    /* Computing MAX */
    i__1 = optdw, i__2 = (integer)dwork[iwrk] + iwrk - 1;
    optdw = max(i__1, i__2);
    /*     Compute the eigenvalues with nonnegative imaginary parts of the */
    /*     pencil aS - bH. */
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
        if ((doublereal)iwork[i__ + 4] >= emin * 2 && (doublereal)iwork[i__ + 4] <= emax * 2) {
            /*           B = SQRT(BASE**IWORK(i+4)) is between underflow and overflow */
            /*           threshold, BETA(i) is divided by B. */
            /*           Set to zero negligible real and imaginary parts. */
            d__1 = iwork[i__ + 4] * .5;
            beta[i__] /= pow_dd(&base, &d__1);
            i__2 = i__;
            i__3 = i__;
            z__2.r = alphar[i__2], z__2.i = alphai[i__3];
            z_sqrt(&z__1, &z__2);
            eig.r = z__1.r, eig.i = z__1.i;
            alphar[i__] = d_imag(&eig);
            alphai[i__] = eig.r;
            tmp2 = prec * dlapy2_(&alphar[i__], &alphai[i__]);
            if ((d__1 = alphar[i__], abs(d__1)) < tmp2) {
                alphar[i__] = 0.;
                if (alphai[i__] < 0.) {
                    alphai[i__] = -alphai[i__];
                }
            }
            if ((d__1 = alphai[i__], abs(d__1)) < tmp2) {
                alphai[i__] = 0.;
                if (alphar[i__] < 0.) {
                    alphar[i__] = -alphar[i__];
                }
            }
        } else {
            /*           Error. */
            *info = 1;
            return 0;
        }
        /* L130: */
    }
    if (ltri) {
        /*        Update C1 and C2. */
        dlacpy_("Upper", &m, &m, &dwork[imat + (mm << 1)], &m, &c1[c1_offset], ldc1, 5L);
        dlacpy_("Full", &m, &m, &dwork[imat], &m, &c2[c2_offset], ldc2, 4L);
        /*        Update V. */
        dgemm_("Transpose", "No Transpose", &m, &m, &m, &c_b17, &dwork[(mm << 1) + 1], &m,
            &vw[(vw_dim1 << 1) + 1], ldvw, &c_b16, &dwork[imat], &m, 9L, 12L);
        dgemm_("No Transpose", "No Transpose", &m, &m, &m, &c_b17, &dwork[imat], &m, &dwork[1], &m,
            &c_b16, &vw[(vw_dim1 << 1) + 1], ldvw, 12L, 12L);
        /*        Update A. */
        dlacpy_("Upper", &m, &m, &dwork[imat + mm], &m, &a[a_offset], lda, 5L);
        /*        Skew-symmetric update of D. */
        i__1 = *ldwork - imat + 1;
        mb01ld_("Upper", "Transpose", &m, &m, &c_b16, &c_b17, &de[(de_dim1 << 1) + 1], ldde,
            &dwork[(mm << 1) + 1], &m, &de[(de_dim1 << 1) + 1], ldde, &dwork[imat], &i__1, info, 5L,
            9L);
        /*        Update B. */
        dlacpy_("Upper", &m, &m, &dwork[imat + mm * 3], &m, &b[b_offset], ldb, 5L);
        /*        Skew-symmetric update of F. */
        i__1 = *ldwork - imat + 1;
        mb01ld_("Upper", "Transpose", &m, &m, &c_b16, &c_b17, &f[f_offset], ldf, &dwork[1], &m,
            &f[f_offset], ldf, &dwork[imat], &i__1, info, 5L, 9L);
        if (lcmpq1) {
            /*           Update Q1. */
            dgemm_("No Transpose", "No Transpose", n, &m, &m, &c_b17, &q1[q1_offset], ldq1,
                &dwork[(mm << 1) + 1], &m, &c_b16, &dwork[imat], n, 12L, 12L);
            dlacpy_("Full", n, &m, &dwork[imat], n, &q1[q1_offset], ldq1, 4L);
            dgemm_("No Transpose", "No Transpose", n, &m, &m, &c_b17, &q1[(m + 1) * q1_dim1 + 1],
                ldq1, &dwork[mm + 1], &m, &c_b16, &dwork[imat], n, 12L, 12L);
            dlacpy_("Full", n, &m, &dwork[imat], n, &q1[(m + 1) * q1_dim1 + 1], ldq1, 4L);
        }
        if (lcmpq2) {
            /*           Update Q2. */
            dgemm_("No Transpose", "No Transpose", n, &m, &m, &c_b17, &q2[q2_offset], ldq2,
                &dwork[mm * 3 + 1], &m, &c_b16, &dwork[imat], n, 12L, 12L);
            dlacpy_("Full", n, &m, &dwork[imat], n, &q2[q2_offset], ldq2, 4L);
            dgemm_("No Transpose", "No Transpose", n, &m, &m, &c_b17, &q2[(m + 1) * q2_dim1 + 1],
                ldq2, &dwork[1], &m, &c_b16, &dwork[imat], n, 12L, 12L);
            dlacpy_("Full", n, &m, &dwork[imat], n, &q2[(m + 1) * q2_dim1 + 1], ldq2, 4L);
        }
    }
    dwork[1] = (doublereal)optdw;
    return 0;
    /* *** Last line of MB04BD *** */
} /* mb04bd_ */
