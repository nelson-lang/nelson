/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c_n1 = -1;
static integer c__1 = 1;
static doublereal c_b25 = -1.;
static doublereal c_b35 = 1.;
static integer c__0 = 0;
static doublereal c_b65 = 0.;

EXPORTSYMBOL /* Subroutine */ int mb03ld_(compq, orth, n, a, lda, de, ldde, b, ldb, fg, ldfg, neig,
    q, ldq, alphar, alphai, beta, bwork, iwork, liwork, dwork, ldwork, info, compq_len,
    orth_len) char *compq,
    *orth;
integer* n;
doublereal* a;
integer* lda;
doublereal* de;
integer* ldde;
doublereal* b;
integer* ldb;
doublereal* fg;
integer *ldfg, *neig;
doublereal* q;
integer* ldq;
doublereal *alphar, *alphai, *beta;
logical* bwork;
integer *iwork, *liwork;
doublereal* dwork;
integer *ldwork, *info;
ftnlen compq_len;
ftnlen orth_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, de_dim1, de_offset, fg_dim1, fg_offset, q_dim1,
        q_offset, i__1, i__2, i__3;
    doublereal d__1;
    /* Builtin functions */
    /* Subroutine */ int s_copy();
    double sqrt();
    /* Local variables */
    static char cmpq[14];
    static integer iwrk;
    extern /* Subroutine */ int ma02ad_(), mb04bd_();
    static integer j, m;
    extern /* Subroutine */ int mb01ld_(), mb03jd_(), mb04hd_(), dscal_(), dgemm_();
    extern logical lsame_();
    static logical liniq;
    static integer mindw;
    extern /* Subroutine */ int dcopy_();
    static integer miniw;
    extern /* Subroutine */ int daxpy_(), dtrmm_();
    static integer optdw, n2;
    extern /* Subroutine */ int dgeqp3_();
    static integer ib, mm, nm, nn;
    static logical qr;
    extern /* Subroutine */ int dgeqrf_(), dgesvd_(), dlacpy_(), xerbla_();
    static integer ic2;
    extern /* Subroutine */ int dorgqr_();
    static integer iq1, iq2, iq3, iq4;
    static logical lquery;
    static integer ih11, ih12, ifo, is11, is12;
    static doublereal dum[3];
    static integer nmm;
    static logical svd;
    static integer irt;
    static logical qrp;
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
    /*     To compute the relevant eigenvalues of a real N-by-N skew- */
    /*     Hamiltonian/Hamiltonian pencil aS - bH, with */
    /*           (  A  D  )         (  B  F  ) */
    /*       S = (        ) and H = (        ),                           (1) */
    /*           (  E  A' )         (  G -B' ) */
    /*     where the notation M' denotes the transpose of the matrix M. */
    /*     Optionally, if COMPQ = 'C', an orthogonal basis of the right */
    /*     deflating subspace of aS - bH corresponding to the eigenvalues */
    /*     with strictly negative real part is computed. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     COMPQ   CHARACTER*1 */
    /*             Specifies whether to compute the right deflating subspace */
    /*             corresponding to the strictly negative eigenvalues of */
    /*             aS - bH. */
    /*             = 'N':  do not compute the deflating subspace; */
    /*             = 'C':  compute the deflating subspace and store it in the */
    /*                     leading subarray of Q. */
    /*     ORTH    CHARACTER*1 */
    /*             If COMPQ = 'C', specifies the technique for computing the */
    /*             orthogonal basis of the deflating subspace, as follows: */
    /*             = 'Q':  QR factorization (the fastest technique); */
    /*             = 'P':  QR factorization with column pivoting; */
    /*             = 'S':  singular value decomposition. */
    /*             If COMPQ = 'N', the ORTH value is not used. */
    /*             Usually, ORTH = 'Q' gives acceptable results, but badly */
    /*             scaled or ill-conditioned problems might need to set */
    /*             ORTH = 'P' or even ORTH = 'S'. */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the pencil aS - bH.  N has to be even. */
    /*     A       (input/output) DOUBLE PRECISION array, dimension */
    /*                            (LDA, N/2) */
    /*             On entry, the leading N/2-by-N/2 part of this array must */
    /*             contain the matrix A. */
    /*             On exit, the leading N/2-by-N/2 part of this array */
    /*             contains the upper triangular matrix Aout (see METHOD). */
    /*     LDA     INTEGER */
    /*             The leading dimension of the array A.  LDA >= MAX(1, N/2). */
    /*     DE      (input/output) DOUBLE PRECISION array, dimension */
    /*                            (LDDE, N/2+1) */
    /*             On entry, the leading N/2-by-N/2 lower triangular part of */
    /*             this array must contain the lower triangular part of the */
    /*             skew-symmetric matrix E, and the N/2-by-N/2 upper */
    /*             triangular part of the submatrix in the columns 2 to N/2+1 */
    /*             of this array must contain the upper triangular part of the */
    /*             skew-symmetric matrix D. */
    /*             The entries on the diagonal and the first superdiagonal of */
    /*             this array need not be set, but are assumed to be zero. */
    /*             On exit, the leading N/2-by-N/2 lower triangular part and */
    /*             the first superdiagonal contains the transpose of the */
    /*             upper quasi-triangular matrix C2out (see METHOD), and the */
    /*             (N/2-1)-by-(N/2-1) upper triangular part of the submatrix */
    /*             in the columns 3 to N/2+1 of this array contains the */
    /*             strictly upper triangular part of the skew-symmetric */
    /*             matrix Dout (see METHOD), without the main diagonal, which */
    /*             is zero. */
    /*     LDDE    INTEGER */
    /*             The leading dimension of the array DE. */
    /*             LDDE >= MAX(1, N/2). */
    /*     B       (input/output) DOUBLE PRECISION array, dimension */
    /*                            (LDB, N/2) */
    /*             On entry, the leading N/2-by-N/2 part of this array must */
    /*             contain the matrix B. */
    /*             On exit, the leading N/2-by-N/2 part of this array */
    /*             contains the upper triangular matrix C1out (see METHOD). */
    /*     LDB     INTEGER */
    /*             The leading dimension of the array B.  LDB >= MAX(1, N/2). */
    /*     FG      (input/output) DOUBLE PRECISION array, dimension */
    /*                            (LDFG, N/2+1) */
    /*             On entry, the leading N/2-by-N/2 lower triangular part of */
    /*             this array must contain the lower triangular part of the */
    /*             symmetric matrix G, and the N/2-by-N/2 upper triangular */
    /*             part of the submatrix in the columns 2 to N/2+1 of this */
    /*             array must contain the upper triangular part of the */
    /*             symmetric matrix F. */
    /*             On exit, the leading N/2-by-N/2 part of the submatrix in */
    /*             the columns 2 to N/2+1 of this array contains the matrix */
    /*             Vout (see METHOD). */
    /*     LDFG    INTEGER */
    /*             The leading dimension of the array FG. */
    /*             LDFG >= MAX(1, N/2). */
    /*     NEIG    (output) INTEGER */
    /*             If COMPQ = 'C', the number of eigenvalues in aS - bH with */
    /*             strictly negative real part. */
    /*     Q       (output) DOUBLE PRECISION array, dimension (LDQ, 2*N) */
    /*             On exit, if COMPQ = 'C', the leading N-by-NEIG part of */
    /*             this array contains an orthogonal basis of the right */
    /*             deflating subspace corresponding to the eigenvalues of */
    /*             aA - bB with strictly negative real part. The remaining */
    /*             part of this array is used as workspace. */
    /*             If COMPQ = 'N', this array is not referenced. */
    /*     LDQ     INTEGER */
    /*             The leading dimension of the array Q. */
    /*             LDQ >= 1,           if COMPQ = 'N'; */
    /*             LDQ >= MAX(1, 2*N), if COMPQ = 'C'. */
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
    /*             aS - bT, in the form lambda = alpha/beta. Since lambda may */
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
    /*     BWORK   LOGICAL array, dimension (N/2) */
    /*     IWORK   INTEGER array, dimension (LIWORK) */
    /*             On exit, if INFO = -20, IWORK(1) returns the minimum value */
    /*             of LIWORK. */
    /*     LIWORK  INTEGER */
    /*             The dimension of the array IWORK. */
    /*             LIWORK >= MAX( N/2 + 32, 2*N + 1 ). */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             On exit, if INFO = 0, DWORK(1) returns the optimal LDWORK. */
    /*             On exit, if INFO = -22, DWORK(1) returns the minimum value */
    /*             of LDWORK. */
    /*     LDWORK  INTEGER */
    /*             The dimension of the array DWORK. */
    /*             LDWORK >= 3*(N/2)**2 + 2*N**2 + MAX( N, 32 ), */
    /*                                                        if COMPQ = 'N'; */
    /*             LDWORK >= 8*N**2 + MAX( 8*N + 32, N/2 + 168, 272 ), */
    /*                                                        if COMPQ = 'C'. */
    /*             For good performance LDWORK should be generally larger. */
    /*             If LDWORK = -1  a workspace query is assumed; the */
    /*             routine only calculates the optimal size of the DWORK */
    /*             array, returns this value as the first entry of the DWORK */
    /*             array, and no error message is issued by XERBLA. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0: succesful exit; */
    /*             < 0: if INFO = -i, the i-th argument had an illegal value; */
    /*             = 1: periodic QZ iteration failed in the SLICOT Library */
    /*                  routines MB04BD or MB04HD (QZ iteration did not */
    /*                  converge or computation of the shifts failed); */
    /*             = 2: standard QZ iteration failed in the SLICOT Library */
    /*                  routines MB04HD or MB03DD (called by MB03JD); */
    /*             = 3: a numerically singular matrix was found in the SLICOT */
    /*                  Library routine MB03HD (called by MB03JD). */
    /*     METHOD */
    /*     First, the decompositions of S and H are computed via orthogonal */
    /*     transformations Q1 and Q2 as follows: */
    /*                       (  Aout  Dout  ) */
    /*       Q1' S J Q1 J' = (              ), */
    /*                       (   0    Aout' ) */
    /*                       (  Bout  Fout  ) */
    /*       J' Q2' J S Q2 = (              ) =: T,                       (2) */
    /*                       (   0    Bout' ) */
    /*                  (  C1out  Vout  )            (  0  I  ) */
    /*       Q1' H Q2 = (               ), where J = (        ), */
    /*                  (  0     C2out' )            ( -I  0  ) */
    /*     and Aout, Bout, C1out are upper triangular, C2out is upper quasi- */
    /*     triangular and Dout and Fout are skew-symmetric. */
    /*     Then, orthogonal matrices Q3 and Q4 are found, for the extended */
    /*     matrices */
    /*            (  Aout   0  )          (    0   C1out ) */
    /*       Se = (            ) and He = (              ), */
    /*            (   0   Bout )          ( -C2out   0   ) */
    /*     such that S11 := Q4' Se Q3 is upper triangular and */
    /*     H11 := Q4' He Q3 is upper quasi-triangular. The following matrices */
    /*     are computed: */
    /*                  (  Dout   0  )                   (   0   Vout ) */
    /*       S12 := Q4' (            ) Q4 and H12 := Q4' (            ) Q4. */
    /*                  (   0   Fout )                   ( Vout'   0  ) */
    /*     Then, an orthogonal matrix Q is found such that the eigenvalues */
    /*     with strictly negative real parts of the pencil */
    /*         (  S11  S12  )     (  H11  H12  ) */
    /*       a (            ) - b (            ) */
    /*         (   0   S11' )     (   0  -H11' ) */
    /*     are moved to the top of this pencil. */
    /*     Finally, an orthogonal basis of the right deflating subspace */
    /*     corresponding to the eigenvalues with strictly negative real part */
    /*     is computed. See also page 12 in [1] for more details. */
    /*     REFERENCES */
    /*     [1] Benner, P., Byers, R., Losse, P., Mehrmann, V. and Xu, H. */
    /*         Numerical Solution of Real Skew-Hamiltonian/Hamiltonian */
    /*         Eigenproblems. */
    /*         Tech. Rep., Technical University Chemnitz, Germany, */
    /*         Nov. 2007. */
    /*     NUMERICAL ASPECTS */
    /*                                                               3 */
    /*     The algorithm is numerically backward stable and needs O(N ) */
    /*     floating point operations. */
    /*     FURTHER COMMENTS */
    /*     This routine does not perform any scaling of the matrices. Scaling */
    /*     might sometimes be useful, and it should be done externally. */
    /*     CONTRIBUTOR */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Romania, */
    /*     Oct. 2010. */
    /*     REVISIONS */
    /*     V. Sima, Nov. 2010. */
    /*     KEYWORDS */
    /*     Deflating subspace, embedded pencil, skew-Hamiltonian/Hamiltonian */
    /*     pencil, structured Schur form. */
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
    de_dim1 = *ldde;
    de_offset = de_dim1 + 1;
    de -= de_offset;
    b_dim1 = *ldb;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    fg_dim1 = *ldfg;
    fg_offset = fg_dim1 + 1;
    fg -= fg_offset;
    q_dim1 = *ldq;
    q_offset = q_dim1 + 1;
    q -= q_offset;
    --alphar;
    --alphai;
    --beta;
    --bwork;
    --iwork;
    --dwork;
    /* Function Body */
    m = *n / 2;
    n2 = *n << 1;
    nn = *n * *n;
    mm = m * m;
    *neig = 0;
    liniq = lsame_(compq, "C", 1L, 1L);
    if (liniq) {
        qr = lsame_(orth, "Q", 1L, 1L);
        qrp = lsame_(orth, "P", 1L, 1L);
        svd = lsame_(orth, "S", 1L, 1L);
    }
    /* Computing MAX */
    i__1 = m + 32, i__2 = n2 + 1;
    miniw = max(i__1, i__2);
    if (*n == 0) {
        mindw = 1;
    } else if (liniq) {
        /* Computing MAX */
        i__1 = (*n << 3) + 32, i__2 = m + 168, i__1 = max(i__1, i__2);
        mindw = (nn << 3) + max(i__1, 272);
    } else {
        /* Computing 2nd power */
        i__1 = m;
        mindw = i__1 * i__1 * 3 + (nn << 1) + max(*n, 32);
    }
    lquery = *ldwork == -1;
    /*     Test the input arguments. */
    *info = 0;
    if (!(lsame_(compq, "N", 1L, 1L) || liniq)) {
        *info = -1;
    } else if (liniq && !(qr || qrp || svd)) {
        *info = -2;
    } else if (*n < 0 || *n % 2 != 0) {
        *info = -3;
    } else if (*lda < max(1, m)) {
        *info = -5;
    } else if (*ldde < max(1, m)) {
        *info = -7;
    } else if (*ldb < max(1, m)) {
        *info = -9;
    } else if (*ldfg < max(1, m)) {
        *info = -11;
    } else if (*ldq < 1 || liniq && *ldq < n2) {
        *info = -14;
    } else if (*liwork < miniw) {
        iwork[1] = miniw;
        *info = -20;
    } else if (!lquery && *ldwork < mindw) {
        dwork[1] = (doublereal)mindw;
        *info = -22;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("MB03LD", &i__1, 6L);
        return 0;
    } else if (*n > 0) {
        /*        Compute optimal workspace. */
        if (liniq) {
            mb04hd_("I", "I", n, &dwork[1], n, &dwork[1], n, &dwork[1], n, &dwork[1], n, &bwork[1],
                &iwork[1], liwork, dum, &c_n1, info, 1L, 1L);
            if (svd) {
                dgesvd_("O", "N", n, n, &q[q_offset], ldq, &dwork[1], &dwork[1], ldq, &dwork[1],
                    &c__1, &dum[1], &c_n1, info, 1L, 1L);
                j = *n + (integer)dum[1];
            } else {
                if (qr) {
                    dgeqrf_(n, &m, &q[q_offset], ldq, &dwork[1], &dum[1], &c_n1, info);
                    j = m;
                } else {
                    dgeqp3_(n, n, &q[q_offset], ldq, &iwork[1], &dwork[1], &dum[1], &c_n1, info);
                    j = *n;
                }
                dorgqr_(n, &j, &j, &q[q_offset], ldq, &dwork[1], &dum[2], &c_n1, info);
                /* Computing MAX */
                i__1 = (integer)dum[1], i__2 = (integer)dum[2];
                j += max(i__1, i__2);
            }
            /* Computing MAX */
            i__1 = mindw, i__2 = nn * 6 + (integer)dum[0], i__1 = max(i__1, i__2);
            optdw = max(i__1, j);
        } else {
            optdw = mindw;
        }
        if (lquery) {
            dwork[1] = (doublereal)optdw;
            return 0;
        }
    }
    /*     Quick return if possible. */
    if (*n == 0) {
        iwork[1] = 1;
        dwork[1] = 1.;
        return 0;
    }
    ifo = 1;
    /*     STEP 1: Apply MB04BD to transform the pencil to real */
    /*             skew-Hamiltonian/Hamiltonian Schur form. */
    /*     Set the computation option and pointers for the inputs and outputs */
    /*     of MB04BD. If possible, array Q is used as vectorized workspace. */
    /*     Real workspace:     need   w1 + 2*N**2 + MAX(N,32), where */
    /*                                w1 = 2*N**2, if COMPQ = 'C'; */
    /*                                w1 = 3*M**2, if COMPQ = 'N'. */
    /*     Integer workspace:  need   M + 12. */
    if (liniq) {
        s_copy(cmpq, "Initialize", 14L, 10L);
        iq1 = 1;
        iq2 = iq1 + nn;
        iwrk = iq2 + nn;
        if (m % 4 == 0) {
            ic2 = m / 4;
        } else {
            ic2 = m / 4 + 1;
        }
        ib = (ic2 << 1) + 1;
        ++ic2;
        i__1 = *ldwork - iwrk + 1;
        mb04bd_("Triangularize", cmpq, cmpq, n, &a[a_offset], lda, &de[de_offset], ldde,
            &b[b_offset], ldb, &fg[fg_offset], ldfg, &dwork[iq1], n, &dwork[iq2], n,
            &q[ib * q_dim1 + 1], &m, &q[ifo * q_dim1 + 1], &m, &q[ic2 * q_dim1 + 1], &m, &alphar[1],
            &alphai[1], &beta[1], &iwork[1], liwork, &dwork[iwrk], &i__1, info, 13L, 14L, 14L);
    } else {
        s_copy(cmpq, "No Computation", 14L, 14L);
        ib = ifo + mm;
        ic2 = ib + mm;
        iwrk = ic2 + mm;
        i__1 = *ldwork - iwrk + 1;
        mb04bd_("Triangularize", cmpq, cmpq, n, &a[a_offset], lda, &de[de_offset], ldde,
            &b[b_offset], ldb, &fg[fg_offset], ldfg, &dwork[1], n, &dwork[1], n, &dwork[ib], &m,
            &dwork[ifo], &m, &dwork[ic2], &m, &alphar[1], &alphai[1], &beta[1], &iwork[1], liwork,
            &dwork[iwrk], &i__1, info, 13L, 14L, 14L);
    }
    if (*info > 0) {
        *info = 1;
        return 0;
    }
    if (!liniq) {
        ma02ad_("Upper", &m, &m, &dwork[ic2], &m, &de[de_offset], ldde, 5L);
        i__1 = m - 1;
        i__2 = m + 1;
        i__3 = *ldde + 1;
        dcopy_(&i__1, &dwork[ic2 + 1], &i__2, &de[(de_dim1 << 1) + 1], &i__3);
        return 0;
    }
    /*     STEP 2: Build the needed parts of the extended matrices Se and He, */
    /*     and compute the transformed matrices and the orthogonal matrices */
    /*     Q3 and Q4. */
    /*     Real workspace:     need   w1 + w2 + 2*N**2 + MAX(M+168,272), with */
    /*                                w2 = 4*N**2 (COMPQ = 'C'); */
    /*                         prefer larger. */
    /*     Integer workspace:  need   M + 32. */
    nm = *n * m;
    nmm = nm + m;
    iq3 = iwrk;
    iq4 = iq3 + nn;
    is11 = iq4 + nn;
    ih11 = is11 + nn;
    iwrk = ih11 + nn;
    dlacpy_("Full", &m, &m, &a[a_offset], lda, &dwork[is11], n, 4L);
    dlacpy_("Full", &m, &m, &q[ib * q_dim1 + 1], &m, &dwork[is11 + nmm], n, 4L);
    dscal_(&mm, &c_b25, &q[ic2 * q_dim1 + 1], &c__1);
    dlacpy_("Full", &m, &m, &q[ic2 * q_dim1 + 1], &m, &dwork[ih11 + m], n, 4L);
    dlacpy_("Full", &m, &m, &b[b_offset], ldb, &dwork[ih11 + nm], n, 4L);
    i__1 = *ldwork - iwrk + 1;
    mb04hd_(cmpq, cmpq, n, &dwork[is11], n, &dwork[ih11], n, &dwork[iq3], n, &dwork[iq4], n,
        &bwork[1], &iwork[1], liwork, &dwork[iwrk], &i__1, info, 14L, 14L);
    if (*info > 0) {
        if (*info > 2) {
            *info = 2;
        }
        return 0;
    }
    /*     STEP 3: Update S12 and H12, building the upper triangular parts, */
    /*     and exploiting the structure. Note that S12 is skew-symmetric and */
    /*     H12 is symmetric. */
    /*     Real workspace:     need   w1 + w2 + w3, where */
    /*                                w3 = N**2 + M**2. */
    is12 = iwrk;
    ih12 = is12 + nn;
    iwrk = ih12;
    if (m > 1) {
        /*                                                   [ Qa  Qc ] */
        /*        Compute Qa'*Do*Qc + Qb'*Fo*Qd, where Q4 =: [        ], */
        /*                                                   [ Qb  Qd ] */
        /*        with Do := Dout, etc. */
        /*        Part of the array Q and DWORK(IS12) are used as workspace. */
        i__1 = m - 1;
        dlacpy_("Full", &i__1, &m, &dwork[iq4 + nm + 1], n, &dwork[is12], &m, 4L);
        i__1 = m - 1;
        dlacpy_("Full", &i__1, &m, &dwork[iq4 + nm], n, &q[ib * q_dim1 + 2], &m, 4L);
        i__1 = m - 1;
        dtrmm_("Left", "Upper", "No Transpose", "Non-Unit", &i__1, &m, &c_b35, &de[de_dim1 * 3 + 1],
            ldde, &dwork[is12], &m, 4L, 5L, 12L, 8L);
        i__1 = m - 1;
        dtrmm_("Left", "Upper", "Transpose", "Non-Unit", &i__1, &m, &c_b25, &de[de_dim1 * 3 + 1],
            ldde, &q[ib * q_dim1 + 2], &m, 4L, 5L, 9L, 8L);
        dum[0] = 0.;
        dcopy_(&m, dum, &c__0, &dwork[is12 + m - 1], &m);
        dcopy_(&m, dum, &c__0, &q[ib * q_dim1 + 1], &m);
        daxpy_(&mm, &c_b35, &q[ib * q_dim1 + 1], &c__1, &dwork[is12], &c__1);
        i__1 = m - 1;
        dlacpy_("Full", &i__1, &m, &dwork[iq4 + nmm + 1], n, &dwork[iwrk], &m, 4L);
        i__1 = m - 1;
        dlacpy_("Full", &i__1, &m, &dwork[iq4 + nmm], n, &q[ib * q_dim1 + 2], &m, 4L);
        i__1 = m - 1;
        dtrmm_("Left", "Upper", "No Transpose", "Non-Unit", &i__1, &m, &c_b35,
            &q[m + 1 + ifo * q_dim1], &m, &dwork[iwrk], &m, 4L, 5L, 12L, 8L);
        i__1 = m - 1;
        dtrmm_("Left", "Upper", "Transpose", "Non-Unit", &i__1, &m, &c_b25,
            &q[m + 1 + ifo * q_dim1], &m, &q[ib * q_dim1 + 2], &m, 4L, 5L, 9L, 8L);
        dcopy_(&m, dum, &c__0, &dwork[iwrk + m - 1], &m);
        daxpy_(&mm, &c_b35, &q[ib * q_dim1 + 1], &c__1, &dwork[iwrk], &c__1);
        dgemm_("Transpose", "No Transpose", &m, &m, &m, &c_b35, &dwork[iq4], n, &dwork[is12], &m,
            &c_b65, &dwork[is12 + nm], n, 9L, 12L);
        dgemm_("Transpose", "No Transpose", &m, &m, &m, &c_b35, &dwork[iq4 + m], n, &dwork[iwrk],
            &m, &c_b35, &dwork[is12 + nm], n, 9L, 12L);
        /*        Compute Qa'*Do*Qa + Qb'*Fo*Qb. */
        i__1 = *ldwork - iwrk + 1;
        mb01ld_("Upper", "Transpose", &m, &m, &c_b65, &c_b35, &dwork[is12], n, &dwork[iq4], n,
            &de[(de_dim1 << 1) + 1], ldde, &dwork[iwrk], &i__1, info, 5L, 9L);
        i__1 = *ldwork - iwrk + 1;
        mb01ld_("Upper", "Transpose", &m, &m, &c_b35, &c_b35, &dwork[is12], n, &dwork[iq4 + m], n,
            &q[ifo * q_dim1 + 1], &m, &dwork[iwrk], &i__1, info, 5L, 9L);
        /*        Compute Qc'*Do*Qc + Qd'*Fo*Qd. */
        i__1 = *ldwork - iwrk + 1;
        mb01ld_("Upper", "Transpose", &m, &m, &c_b65, &c_b35, &dwork[is12 + nmm], n,
            &dwork[iq4 + nm], n, &de[(de_dim1 << 1) + 1], ldde, &dwork[iwrk], &i__1, info, 5L, 9L);
        i__1 = *ldwork - iwrk + 1;
        mb01ld_("Upper", "Transpose", &m, &m, &c_b35, &c_b35, &dwork[is12 + nmm], n,
            &dwork[iq4 + nmm], n, &q[ifo * q_dim1 + 1], &m, &dwork[iwrk], &i__1, info, 5L, 9L);
    }
    /*     Compute Qb'*Vo'*Qc + Qa'*Vo*Qd. */
    /*     Real workspace:     need   w1 + w2 + w3, where */
    /*                                w3 = 2*N**2. */
    dgemm_("Transpose", "No Transpose", &m, &m, &m, &c_b35, &fg[(fg_dim1 << 1) + 1], ldfg,
        &dwork[iq4 + nm], n, &c_b65, &q[ifo * q_dim1 + 1], &m, 9L, 12L);
    dgemm_("Transpose", "No Transpose", &m, &m, &m, &c_b35, &dwork[iq4 + m], n,
        &q[ifo * q_dim1 + 1], &m, &c_b65, &dwork[ih12 + nm], n, 9L, 12L);
    dgemm_("Transpose", "No Transpose", &m, &m, &m, &c_b35, &dwork[iq4], n, &fg[(fg_dim1 << 1) + 1],
        ldfg, &c_b65, &dwork[ih12], &m, 9L, 12L);
    dgemm_("No Transpose", "No Transpose", &m, &m, &m, &c_b35, &dwork[ih12], &m, &dwork[iq4 + nmm],
        n, &c_b65, &dwork[ih12 + nmm], n, 12L, 12L);
    i__1 = m;
    for (j = 1; j <= i__1; ++j) {
        daxpy_(&m, &c_b35, &dwork[ih12 + (m + j - 1) * *n + m], &c__1,
            &dwork[ih12 + (m + j - 1) * *n], &c__1);
        /* L10: */
    }
    /*     Compute the upper triangle of Qa'*Vo*Qb + (Qa'*Vo*Qb)'. */
    dgemm_("Transpose", "No Transpose", &m, &m, &m, &c_b35, &dwork[iq4], n, &fg[(fg_dim1 << 1) + 1],
        ldfg, &c_b65, &q[ifo * q_dim1 + 1], &m, 9L, 12L);
    dgemm_("No Transpose", "No Transpose", &m, &m, &m, &c_b35, &q[ifo * q_dim1 + 1], &m,
        &dwork[iq4 + m], n, &c_b65, &dwork[ih12], n, 12L, 12L);
    i__1 = m;
    for (j = 1; j <= i__1; ++j) {
        daxpy_(&j, &c_b35, &dwork[ih12 + j - 1], n, &dwork[ih12 + (j - 1) * *n], &c__1);
        /* L20: */
    }
    /*     Compute the upper triangle of Qc'*Vo*Qd + (Qc'*Vo*Qd)'. */
    dgemm_("Transpose", "No Transpose", &m, &m, &m, &c_b35, &dwork[iq4 + nm], n,
        &fg[(fg_dim1 << 1) + 1], ldfg, &c_b65, &q[ifo * q_dim1 + 1], &m, 9L, 12L);
    dgemm_("No Transpose", "No Transpose", &m, &m, &m, &c_b35, &q[ifo * q_dim1 + 1], &m,
        &dwork[iq4 + nmm], n, &c_b65, &dwork[ih12 + nmm], n, 12L, 12L);
    i__1 = m;
    for (j = 1; j <= i__1; ++j) {
        daxpy_(&j, &c_b35, &dwork[ih12 + nmm + j - 1], n, &dwork[ih12 + nmm + (j - 1) * *n], &c__1);
        /* L30: */
    }
    /*     Return C2out. */
    dscal_(&mm, &c_b25, &q[ic2 * q_dim1 + 1], &c__1);
    ma02ad_("Upper", &m, &m, &q[ic2 * q_dim1 + 1], &m, &de[de_offset], ldde, 5L);
    i__1 = m - 1;
    i__2 = m + 1;
    i__3 = *ldde + 1;
    dcopy_(&i__1, &q[ic2 * q_dim1 + 2], &i__2, &de[(de_dim1 << 1) + 1], &i__3);
    /*     STEP 4: Apply MB03JD to reorder the eigenvalues with strictly */
    /*             negative real part to the top. */
    /*     Real workspace:     need   w1 + w2 + w3 + MAX(8*N+32,108), */
    /*                                w3 = 2*N**2. */
    /*     Integer workspace:  need   2*N + 1. */
    iwrk = ih12 + nn;
    i__1 = *ldwork - iwrk + 1;
    mb03jd_(cmpq, &n2, &dwork[is11], n, &dwork[is12], n, &dwork[ih11], n, &dwork[ih12], n,
        &q[q_offset], ldq, neig, &iwork[1], liwork, &dwork[iwrk], &i__1, info, 14L);
    if (*info > 0) {
        ++(*info);
        return 0;
    }
    /*     STEP 5: Compute the deflating subspace corresponding to the */
    /*             eigenvalues with strictly negative real part. */
    /*     Real workspace:     need   w2 + 3*N**2, if ORTH = 'QR'. */
    /*                                w2 + 4*N**2, otherwise. */
    iwrk = is11;
    if (qr) {
        *neig /= 2;
    }
    /*     Compute [ J*Q1*J' Q2 ]. */
    dlacpy_("Full", &m, &m, &dwork[iq1 + nmm], n, &dwork[iwrk], n, 4L);
    dlacpy_("Full", &m, &m, &dwork[iq1 + nm], n, &dwork[iwrk + m], n, 4L);
    i__1 = m;
    for (j = 1; j <= i__1; ++j) {
        dscal_(&m, &c_b25, &dwork[iwrk + m + (j - 1) * *n], &c__1);
        /* L40: */
    }
    dlacpy_("Full", &m, &m, &dwork[iq1 + m], n, &dwork[iwrk + nm], n, 4L);
    i__1 = m;
    for (j = 1; j <= i__1; ++j) {
        dscal_(&m, &c_b25, &dwork[iwrk + nm + (j - 1) * *n], &c__1);
        /* L50: */
    }
    dlacpy_("Full", &m, &m, &dwork[iq1], n, &dwork[iwrk + nmm], n, 4L);
    dlacpy_("Full", n, n, &dwork[iq2], n, &dwork[iwrk + nn], n, 4L);
    /*     Compute the first NEIG columns of P*[ Q3  0; 0 Q4 ]*Q. */
    irt = iwrk + *n * n2;
    dgemm_("No Transpose", "No Transpose", &m, neig, n, &c_b35, &dwork[iq3], n, &q[q_offset], ldq,
        &c_b65, &dwork[irt], &n2, 12L, 12L);
    dgemm_("No Transpose", "No Transpose", &m, neig, n, &c_b35, &dwork[iq4], n, &q[*n + 1 + q_dim1],
        ldq, &c_b65, &dwork[irt + m], &n2, 12L, 12L);
    dgemm_("No Transpose", "No Transpose", &m, neig, n, &c_b35, &dwork[iq3 + m], n, &q[q_offset],
        ldq, &c_b65, &dwork[irt + *n], &n2, 12L, 12L);
    dgemm_("No Transpose", "No Transpose", &m, neig, n, &c_b35, &dwork[iq4 + m], n,
        &q[*n + 1 + q_dim1], ldq, &c_b65, &dwork[irt + *n + m], &n2, 12L, 12L);
    /*     Compute the deflating subspace. */
    d__1 = sqrt(2.) / 2.;
    dgemm_("No Transpose", "No Transpose", n, neig, &n2, &d__1, &dwork[iwrk], n, &dwork[irt], &n2,
        &c_b65, &q[q_offset], ldq, 12L, 12L);
    /*     Orthogonalize the basis given in Q(1:n,1:neig). */
    iwrk = *neig + 1;
    if (svd) {
        /*        Real workspace:     need   N + MAX(1,5*M); */
        /*                            prefer larger. */
        i__1 = *ldwork - iwrk + 1;
        dgesvd_("Overwrite", "No V", n, neig, &q[q_offset], ldq, &dwork[1], &dwork[1], &c__1,
            &dwork[1], &c__1, &dwork[iwrk], &i__1, info, 9L, 4L);
        *neig /= 2;
    } else {
        if (qr) {
            /*           Real workspace:     need   N; */
            /*                               prefer M+M*NB, where NB is the optimal */
            /*                                              blocksize. */
            i__1 = *ldwork - iwrk + 1;
            dgeqrf_(n, neig, &q[q_offset], ldq, &dwork[1], &dwork[iwrk], &i__1, info);
        } else {
            /*           Real workspace:     need   4*N+1; */
            /*                               prefer 3*N+(N+1)*NB. */
            i__1 = *neig;
            for (j = 1; j <= i__1; ++j) {
                iwork[j] = 0;
                /* L60: */
            }
            i__1 = *ldwork - iwrk + 1;
            dgeqp3_(n, neig, &q[q_offset], ldq, &iwork[1], &dwork[1], &dwork[iwrk], &i__1, info);
        }
        /*        Real workspace:     need   2*NEIG; */
        /*                            prefer NEIG + NEIG*NB. */
        i__1 = *ldwork - iwrk + 1;
        dorgqr_(n, neig, neig, &q[q_offset], ldq, &dwork[1], &dwork[iwrk], &i__1, info);
        if (qrp) {
            *neig /= 2;
        }
    }
    dwork[1] = (doublereal)optdw;
    return 0;
    /* *** Last line of MB03LD *** */
} /* mb03ld_ */
