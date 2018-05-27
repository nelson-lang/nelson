/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c_n1 = -1;
static doublereal c_b54 = -1.;
static doublereal c_b62 = 0.;
static doublereal c_b63 = 1.;
static integer c__0 = 0;
static integer c__6 = 6;

EXPORTSYMBOL /* Subroutine */ int mb04ad_(job, compq1, compq2, compu1, compu2, n, z__, ldz, h__,
    ldh, t, ldt, q1, ldq1, q2, ldq2, u11, ldu11, u12, ldu12, u21, ldu21, u22, ldu22, alphar, alphai,
    beta, iwork, liwork, dwork, ldwork, info, job_len, compq1_len, compq2_len, compu1_len,
    compu2_len) char *job,
    *compq1, *compq2, *compu1, *compu2;
integer* n;
doublereal* z__;
integer* ldz;
doublereal* h__;
integer* ldh;
doublereal* t;
integer* ldt;
doublereal* q1;
integer* ldq1;
doublereal* q2;
integer* ldq2;
doublereal* u11;
integer* ldu11;
doublereal* u12;
integer* ldu12;
doublereal* u21;
integer* ldu21;
doublereal* u22;
integer* ldu22;
doublereal *alphar, *alphai, *beta;
integer *iwork, *liwork;
doublereal* dwork;
integer *ldwork, *info;
ftnlen job_len;
ftnlen compq1_len;
ftnlen compq2_len;
ftnlen compu1_len;
ftnlen compu2_len;
{
    /* System generated locals */
    integer h_dim1, h_offset, q1_dim1, q1_offset, q2_dim1, q2_offset, t_dim1, t_offset, u11_dim1,
        u11_offset, u12_dim1, u12_offset, u21_dim1, u21_offset, u22_dim1, u22_offset, z_dim1,
        z_offset, i__1, i__2, i__3;
    doublereal d__1;
    doublecomplex z__1, z__2;
    /* Builtin functions */
    double sqrt();
    /* Subroutine */ int s_copy();
    void z_sqrt();
    double d_imag(), pow_dd();
    /* Local variables */
    static doublereal base;
    static integer imat;
    static doublereal emax, emin, prec;
    static integer idum[1];
    static char cmpq[16];
    static integer itau;
    static doublereal sqrb;
    extern /* Subroutine */ int drot_();
    static logical ltri;
    static integer iwrk;
    extern /* Subroutine */ int ma02ad_();
    static integer i__, j, k;
    extern /* Subroutine */ int mb03bd_();
    static integer m;
    extern /* Subroutine */ int dscal_(), dgemm_();
    extern logical lsame_();
    static char cmpsc[16];
    static integer mindw;
    extern /* Subroutine */ int dswap_();
    static integer iwarn;
    extern /* Subroutine */ int dcopy_();
    static integer optdw;
    extern doublereal dlapy2_();
    static logical lcmpq1, lcmpq2, liniq1, lcmpu1, lcmpu2, liniq2, liniu1, liniu2, lupdq1, lupdq2,
        lupdu1, lupdu2;
    static integer nb;
    static doublereal co;
    static integer iq, mm;
    static doublereal si;
    extern doublereal dlamch_();
    extern /* Subroutine */ int dgeqrf_(), dgerqf_(), dlacpy_(), dlartg_();
    extern integer ilaenv_();
    extern /* Subroutine */ int dlaset_(), xerbla_(), dormqr_(), dormrq_();
    static logical lquery;
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
    /*                                      (  0  I  ) */
    /*       S = T Z = J Z' J' Z, where J = (        ),                   (1) */
    /*                                      ( -I  0  ) */
    /*     via generalized symplectic URV decomposition. That is, orthogonal */
    /*     matrices Q1 and Q2 and orthogonal symplectic matrices U1 and U2 */
    /*     are computed such that */
    /*                                   (  T11  T12 ) */
    /*       Q1' T U1 = Q1' J Z' J' U1 = (           ) = Tout, */
    /*                                   (   0   T22 ) */
    /*                  (  Z11  Z12 ) */
    /*       U2' Z Q2 = (           ) = Zout,                             (2) */
    /*                  (   0   Z22 ) */
    /*                  ( H11  H12 ) */
    /*       Q1' H Q2 = (          ) = Hout, */
    /*                  (  0   H22 ) */
    /*     where T11, T22', Z11, Z22', H11 are upper triangular and H22' is */
    /*     upper quasi-triangular. */
    /*     Optionally, if COMPQ1 = 'C' the orthogonal transformation matrix */
    /*     Q1 will be computed. */
    /*     Optionally, if COMPQ2 = 'C' the orthogonal transformation matrix */
    /*     Q2 will be computed. */
    /*     Optionally, if COMPU1 = 'C' the orthogonal symplectic */
    /*     transformation matrix */
    /*            (  U11  U12  ) */
    /*       U1 = (            ) */
    /*            ( -U12  U11  ) */
    /*     will be computed. */
    /*     Optionally, if COMPU2 = 'C' the orthogonal symplectic */
    /*     transformation matrix */
    /*            (  U21  U22  ) */
    /*       U2 = (            ) */
    /*            ( -U22  U21  ) */
    /*     will be computed. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     JOB     CHARACTER*1 */
    /*             Specifies the computation to be performed, as follows: */
    /*             = 'E': compute the eigenvalues only; Z, T, and H will not */
    /*                    necessarily be put into the forms in (2); H22' is */
    /*                    upper Hessenberg; */
    /*             = 'T': put Z, T, and H into the forms in (2), and return */
    /*                    the eigenvalues in ALPHAR, ALPHAI and BETA. */
    /*     COMPQ1  CHARACTER*1 */
    /*             Specifies whether to compute the orthogonal transformation */
    /*             matrix Q1, as follows: */
    /*             = 'N': Q1 is not computed; */
    /*             = 'C': compute the matrix Q1 of the orthogonal */
    /*                    transformations applied on the left to the pencil */
    /*                    aTZ - bH to reduce its matrices to the form (2). */
    /*                    The array Q1 is initialized internally to the */
    /*                    identity matrix. */
    /*     COMPQ2  CHARACTER*1 */
    /*             Specifies whether to compute the orthogonal transformation */
    /*             matrix Q2, as follows: */
    /*             = 'N': Q2 is not computed; */
    /*             = 'C': compute the matrix Q2 of the orthogonal */
    /*                    transformations applied on the right to the pencil */
    /*                    aTZ - bH to reduce its matrices to the form (2). */
    /*                    The array Q2 is initialized internally to the */
    /*                    identity matrix. */
    /*     COMPU1  CHARACTER*1 */
    /*             Specifies whether to compute the orthogonal symplectic */
    /*             transformation matrix U1, as follows: */
    /*             = 'N': U1 is not computed; */
    /*             = 'C': compute the matrices U11 and U12 of the orthogonal */
    /*                    symplectic transformations applied to the pencil */
    /*                    aTZ - bT to reduce its matrices to the form (2). */
    /*                    The arrays U11 and U12 are initialized internally */
    /*                    to correspond to an identity matrix U1. */
    /*     COMPU2  CHARACTER*1 */
    /*             Specifies whether to compute the orthogonal symplectic */
    /*             transformation matrix U2, as follows: */
    /*             = 'N': U2 is not computed; */
    /*             = 'C': compute the matrices U21 and U22 of the orthogonal */
    /*                    symplectic transformations applied to the pencil */
    /*                    aTZ - bT to reduce its matrices to the form (2). */
    /*                    The arrays U21 and U22 are initialized internally */
    /*                    to correspond to an identity matrix U2. */
    /*     Input/output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the pencil aS - bH.  N >= 0, even. */
    /*     Z       (input/output) DOUBLE PRECISION array, dimension (LDZ, N) */
    /*             On entry, the leading N-by-N part of this array must */
    /*             contain the matrix Z. */
    /*             On exit, if JOB = 'T', the leading N-by-N part of this */
    /*             array contains the matrix Zout; otherwise, it contains the */
    /*             matrix Z obtained just before the application of the */
    /*             periodic QZ algorithm. */
    /*             The elements of the (2,1) block, i.e., in the rows N/2+1 */
    /*             to N and in the columns 1 to N/2 are not set to zero, but */
    /*             are unchanged on exit. */
    /*     LDZ     INTEGER */
    /*             The leading dimension of the array Z.  LDZ >= MAX(1, N). */
    /*     H       (input/output) DOUBLE PRECISION array, dimension (LDH, N) */
    /*             On entry, the leading N-by-N part of this array must */
    /*             contain the Hamiltonian matrix H (H22 = -H11', H12 = H12', */
    /*             H21 = H21'). */
    /*             On exit, if JOB = 'T', the leading N-by-N part of this */
    /*             array contains the matrix Hout; otherwise, it contains the */
    /*             matrix H obtained just before the application of the */
    /*             periodic QZ algorithm. */
    /*     LDH     INTEGER */
    /*             The leading dimension of the array H.  LDH >= MAX(1, N). */
    /*     T       (output) DOUBLE PRECISION array, dimension (LDT, N) */
    /*             If JOB = 'T', the leading N-by-N part of this array */
    /*             contains the matrix Tout; otherwise, it contains the */
    /*             matrix T obtained just before the application of the */
    /*             periodic QZ algorithm. */
    /*     LDT     INTEGER */
    /*             The leading dimension of the array T.  LDT >= MAX(1, N). */
    /*     Q1      (output) DOUBLE PRECISION array, dimension (LDQ1, N) */
    /*             On exit, if COMPQ1 = 'C', the leading N-by-N part of this */
    /*             array contains the orthogonal transformation matrix Q1. */
    /*             If COMPQ1 = 'N', this array is not referenced. */
    /*     LDQ1    INTEGER */
    /*             The leading dimension of the array Q1. */
    /*             LDQ1 >= 1,         if COMPQ1 = 'N'; */
    /*             LDQ1 >= MAX(1, N), if COMPQ1 = 'C'. */
    /*     Q2      (output) DOUBLE PRECISION array, dimension (LDQ2, N) */
    /*             On exit, if COMPQ2 = 'C', the leading N-by-N part of this */
    /*             array contains the orthogonal transformation matrix Q2. */
    /*             If COMPQ2 = 'N', this array is not referenced. */
    /*     LDQ2    INTEGER */
    /*             The leading dimension of the array Q2. */
    /*             LDQ2 >= 1,         if COMPQ2 = 'N'; */
    /*             LDQ2 >= MAX(1, N), if COMPQ2 = 'C'. */
    /*     U11     (output) DOUBLE PRECISION array, dimension (LDU11, N/2) */
    /*             On exit, if COMPU1 = 'C', the leading N/2-by-N/2 part of */
    /*             this array contains the upper left block U11 of the */
    /*             orthogonal symplectic transformation matrix U1. */
    /*             If COMPU1 = 'N', this array is not referenced. */
    /*     LDU11   INTEGER */
    /*             The leading dimension of the array U11. */
    /*             LDU11 >= 1,           if COMPU1 = 'N'; */
    /*             LDU11 >= MAX(1, N/2), if COMPU1 = 'C'. */
    /*     U12     (output) DOUBLE PRECISION array, dimension (LDU12, N/2) */
    /*             On exit, if COMPU1 = 'C', the leading N/2-by-N/2 part of */
    /*             this array contains the upper right block U12 of the */
    /*             orthogonal symplectic transformation matrix U1. */
    /*             If COMPU1 = 'N', this array is not referenced. */
    /*     LDU12   INTEGER */
    /*             The leading dimension of the array U12. */
    /*             LDU12 >= 1,           if COMPU1 = 'N'; */
    /*             LDU12 >= MAX(1, N/2), if COMPU1 = 'C'. */
    /*     U21     (output) DOUBLE PRECISION array, dimension (LDU21, N/2) */
    /*             On exit, if COMPU2 = 'C', the leading N/2-by-N/2 part of */
    /*             this array contains the upper left block U21 of the */
    /*             orthogonal symplectic transformation matrix U2. */
    /*             If COMPU2 = 'N', this array is not referenced. */
    /*     LDU21   INTEGER */
    /*             The leading dimension of the array U21. */
    /*             LDU21 >= 1,           if COMPU2 = 'N'; */
    /*             LDU21 >= MAX(1, N/2), if COMPU2 = 'C'. */
    /*     U22     (output) DOUBLE PRECISION array, dimension (LDU22, N/2) */
    /*             On exit, if COMPU2 = 'C', the leading N/2-by-N/2 part of */
    /*             this array contains the upper right block U22 of the */
    /*             orthogonal symplectic transformation matrix U2. */
    /*             If COMPU2 = 'N', this array is not referenced. */
    /*     LDU22   INTEGER */
    /*             The leading dimension of the array U22. */
    /*             LDU22 >= 1,           if COMPU2 = 'N'; */
    /*             LDU22 >= MAX(1, N/2), if COMPU2 = 'C'. */
    /*     ALPHAR  (output) DOUBLE PRECISION array, dimension (N/2) */
    /*             The real parts of each scalar alpha defining an eigenvalue */
    /*             of the pencil aS - bH. */
    /*     ALPHAI  (output) DOUBLE PRECISION array, dimension (N/2) */
    /*             The imaginary parts of each scalar alpha defining an */
    /*             eigenvalue of the pencil aS - bH. */
    /*             If ALPHAI(j) is zero, then the j-th eigenvalue is real. */
    /*     BETA    (output) DOUBLE PRECISION array, dimension (N/2) */
    /*             The scalars beta defining the eigenvalues of the pencil */
    /*             aS - bH. */
    /*             If INFO = 0, the quantities alpha = (ALPHAR(j),ALPHAI(j)), */
    /*             and beta = BETA(j) represent together the j-th eigenvalue */
    /*             of the pencil aS - bH, in the form lambda = alpha/beta. */
    /*             Since lambda may overflow, the ratios should not, in */
    /*             general, be computed. Due to the skew-Hamiltonian/ */
    /*             Hamiltonian structure of the pencil, only half of the */
    /*             spectrum is saved in ALPHAR, ALPHAI and BETA. */
    /*             Specifically, the eigenvalues with positive real parts or */
    /*             with non-negative imaginary parts, when real parts are */
    /*             zero, are returned. The remaining eigenvalues have */
    /*             opposite signs. */
    /*             If INFO = 3, one or more BETA(j) is not representable, and */
    /*             the eigenvalues are returned as described below. */
    /*     Workspace */
    /*     IWORK   INTEGER array, dimension (LIWORK) */
    /*             On exit, if INFO = 3, IWORK(1), ..., IWORK(N/2) return the */
    /*             scaling parameters for the eigenvalues of the pencil */
    /*             aS - bH (see INFO = 3). */
    /*     LIWORK  INTEGER */
    /*             The dimension of the array IWORK. */
    /*             LIWORK >= N/2+18. */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             On exit, if INFO = 0, DWORK(1) returns the optimal value */
    /*             of LDWORK, and DWORK(2) returns the machine base, b. */
    /*             On exit, if INFO = -31, DWORK(1) returns the minimum value */
    /*             of LDWORK. */
    /*     LDWORK  INTEGER */
    /*             The dimension of the array DWORK. */
    /*             If JOB = 'E' and COMPQ1 = 'N' and COMPQ2 = 'N' and */
    /*             COMPU1 = 'N' and COMPU2 = 'N', then */
    /*                   LDWORK >= 3/2*N**2+MAX(N, 48); */
    /*             else, LDWORK >=   3*N**2+MAX(N, 48). */
    /*             For good performance LDWORK should generally be larger. */
    /*             If LDWORK = -1, then a workspace query is assumed; the */
    /*             routine only calculates the optimal size of the DWORK */
    /*             array, returns this value as the first entry of the DWORK */
    /*             array, and no error message related to LDWORK is issued by */
    /*             XERBLA. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0: succesful exit. */
    /*             < 0: if INFO = -i, the i-th argument had an illegal value. */
    /*             = 1: the periodic QZ algorithm was not able to reveal */
    /*                  information about the eigenvalues from the 2-by-2 */
    /*                  blocks in the SLICOT Library routine MB03BD; */
    /*             = 2: the periodic QZ algorithm did not converge in the */
    /*                  SLICOT Library routine MB03BD; */
    /*             = 3: the eigenvalues will under- or overflow if evaluated; */
    /*                  therefore, the j-th eigenvalue is represented by */
    /*                  the quantities alpha = (ALPHAR(j),ALPHAI(j)), */
    /*                  beta = BETA(j), and gamma = IWORK(j) in the form */
    /*                  lambda = (alpha/beta) * b**gamma, where b is the */
    /*                  machine base (often 2.0). This is not an error. */
    /*     METHOD */
    /*     The algorithm uses Givens rotations and Householder reflections to */
    /*     annihilate elements in T, Z, and H such that T11, T22', Z11, Z22' */
    /*     and H11 are upper triangular and H22' is upper Hessenberg. Finally */
    /*     the periodic QZ algorithm is applied to transform H22' to upper */
    /*     quasi-triangular form while T11, T22', Z11, Z22', and H11 stay in */
    /*     upper triangular form. */
    /*     See also page 17 in [1] for more details. */
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
    /*     Chemnitz, December 03, 2008. */
    /*     V. Sima, Dec. 2009 (SLICOT version of the routine DGEURV). */
    /*     REVISIONS */
    /*     V. Sima, Feb. 2010, Nov. 2010. */
    /*     KEYWORDS */
    /*     generalized symplectic URV decomposition, periodic QZ algorithm, */
    /*     upper (quasi-)triangular matrix. */
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
    /*     The code is able to update some given matrices Q1, Q2, U1, and U2, */
    /*     but this feature is not documented. */
    /* Parameter adjustments */
    z_dim1 = *ldz;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;
    h_dim1 = *ldh;
    h_offset = h_dim1 + 1;
    h__ -= h_offset;
    t_dim1 = *ldt;
    t_offset = t_dim1 + 1;
    t -= t_offset;
    q1_dim1 = *ldq1;
    q1_offset = q1_dim1 + 1;
    q1 -= q1_offset;
    q2_dim1 = *ldq2;
    q2_offset = q2_dim1 + 1;
    q2 -= q2_offset;
    u11_dim1 = *ldu11;
    u11_offset = u11_dim1 + 1;
    u11 -= u11_offset;
    u12_dim1 = *ldu12;
    u12_offset = u12_dim1 + 1;
    u12 -= u12_offset;
    u21_dim1 = *ldu21;
    u21_offset = u21_dim1 + 1;
    u21 -= u21_offset;
    u22_dim1 = *ldu22;
    u22_offset = u22_dim1 + 1;
    u22 -= u22_offset;
    --alphar;
    --alphai;
    --beta;
    --iwork;
    --dwork;
    /* Function Body */
    m = *n / 2;
    mm = m * m;
    ltri = lsame_(job, "T", 1L, 1L);
    liniq1 = lsame_(compq1, "C", 1L, 1L);
    lupdq1 = lsame_(compq1, "U", 1L, 1L);
    liniq2 = lsame_(compq2, "C", 1L, 1L);
    lupdq2 = lsame_(compq2, "U", 1L, 1L);
    liniu1 = lsame_(compu1, "C", 1L, 1L);
    lupdu1 = lsame_(compu1, "U", 1L, 1L);
    liniu2 = lsame_(compu2, "C", 1L, 1L);
    lupdu2 = lsame_(compu2, "U", 1L, 1L);
    lcmpq1 = liniq1 || lupdq1;
    lcmpq2 = liniq2 || lupdq2;
    lcmpu1 = liniu1 || lupdu1;
    lcmpu2 = liniu2 || lupdu2;
    lquery = *ldwork == -1;
    /*     Test the input arguments. */
    *info = 0;
    if (!(lsame_(job, "E", 1L, 1L) || ltri)) {
        *info = -1;
    } else if (!(lsame_(compq1, "N", 1L, 1L) || lcmpq1)) {
        *info = -2;
    } else if (!(lsame_(compq2, "N", 1L, 1L) || lcmpq2)) {
        *info = -3;
    } else if (!(lsame_(compu1, "N", 1L, 1L) || lcmpu1)) {
        *info = -4;
    } else if (!(lsame_(compu2, "N", 1L, 1L) || lcmpu2)) {
        *info = -5;
    } else if (*n < 0 || *n % 2 != 0) {
        *info = -6;
    } else if (*ldz < max(1, *n)) {
        *info = -8;
    } else if (*ldh < max(1, *n)) {
        *info = -10;
    } else if (*ldt < max(1, *n)) {
        *info = -12;
    } else if (*ldq1 < 1 || lcmpq1 && *ldq1 < *n) {
        *info = -14;
    } else if (*ldq2 < 1 || lcmpq2 && *ldq2 < *n) {
        *info = -16;
    } else if (*ldu11 < 1 || lcmpu1 && *ldu11 < m) {
        *info = -18;
    } else if (*ldu12 < 1 || lcmpu1 && *ldu12 < m) {
        *info = -20;
    } else if (*ldu21 < 1 || lcmpu2 && *ldu21 < m) {
        *info = -22;
    } else if (*ldu22 < 1 || lcmpu2 && *ldu22 < m) {
        *info = -24;
    } else if (*liwork < m + 18) {
        *info = -29;
    } else {
        if (ltri || lcmpq1 || lcmpq2 || lcmpu1 || lcmpu2) {
            mindw = mm * 12 + max(*n, 48);
        } else {
            mindw = mm * 6 + max(*n, 48);
        }
        iwrk = m;
        /* Computing MIN */
        i__1 = 64, i__2 = ilaenv_(&c__1, "DORMQR", "LT", n, n, &m, &c_n1, 6L, 2L);
        nb = min(i__1, i__2);
        /* Computing MAX */
        i__1 = mindw, i__2 = iwrk + *n * nb;
        optdw = max(i__1, i__2);
        nb = ilaenv_(&c__1, "DGERQF", " ", &m, &m, &c_n1, &c_n1, 6L, 1L);
        /* Computing MAX */
        i__1 = optdw, i__2 = iwrk + mm + m * nb;
        optdw = max(i__1, i__2);
        /* Computing MIN */
        i__1 = 64, i__2 = ilaenv_(&c__1, "DORMRQ", "LN", &m, n, &m, &c_n1, 6L, 2L);
        nb = min(i__1, i__2);
        /* Computing MAX */
        i__1 = optdw, i__2 = iwrk + m * m + *n * nb;
        optdw = max(i__1, i__2);
        if (lcmpq1) {
            /* Computing MIN */
            i__1 = 64, i__2 = ilaenv_(&c__1, "DORMRQ", "RT", n, &m, &m, &c_n1, 6L, 2L);
            nb = min(i__1, i__2);
            /* Computing MAX */
            i__1 = optdw, i__2 = iwrk + m * m + *n * nb;
            optdw = max(i__1, i__2);
        }
        nb = ilaenv_(&c__1, "DGEQRF", " ", n, &m, &c_n1, &c_n1, 6L, 1L);
        /* Computing MAX */
        i__1 = optdw, i__2 = iwrk + m * *n + m * nb;
        optdw = max(i__1, i__2);
        /* Computing MIN */
        i__1 = 64, i__2 = ilaenv_(&c__1, "DORMQR", "RN", n, n, &m, &c_n1, 6L, 2L);
        nb = min(i__1, i__2);
        /* Computing MAX */
        i__1 = optdw, i__2 = iwrk + m * *n + *n * nb;
        optdw = max(i__1, i__2);
        if (lcmpq2) {
            /* Computing MIN */
            i__1 = 64, i__2 = ilaenv_(&c__1, "DORMQR", "LN", n, n, &m, &c_n1, 6L, 2L);
            nb = min(i__1, i__2);
            /* Computing MAX */
            i__1 = optdw, i__2 = iwrk + m * *n + *n * nb;
            optdw = max(i__1, i__2);
        }
        if (*ldwork < mindw && !lquery) {
            dwork[1] = (doublereal)mindw;
            *info = -31;
        }
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("MB04AD", &i__1, 6L);
        return 0;
    } else if (lquery) {
        dwork[1] = (doublereal)optdw;
        return 0;
    }
    /*     Determine machine constants. */
    base = dlamch_("Base", 4L);
    emin = dlamch_("Minimum Exponent", 16L);
    emax = dlamch_("Largest Exponent", 16L);
    prec = dlamch_("Precision", 9L);
    sqrb = sqrt(base);
    /*     Quick return if possible. */
    if (*n == 0) {
        dwork[1] = 2.;
        dwork[2] = base;
        return 0;
    }
    /*     Initializations. */
    /*     Set T = J Z' J'. */
    ma02ad_("Full", &m, &m, &z__[m + 1 + (m + 1) * z_dim1], ldz, &t[t_offset], ldt, 4L);
    ma02ad_("Full", &m, &m, &z__[(m + 1) * z_dim1 + 1], ldz, &t[(m + 1) * t_dim1 + 1], ldt, 4L);
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
        dscal_(&m, &c_b54, &t[(m + i__) * t_dim1 + 1], &c__1);
        /* L10: */
    }
    ma02ad_("Full", &m, &m, &z__[m + 1 + z_dim1], ldz, &t[m + 1 + t_dim1], ldt, 4L);
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
        dscal_(&m, &c_b54, &t[m + 1 + i__ * t_dim1], &c__1);
        /* L20: */
    }
    ma02ad_("Full", &m, &m, &z__[z_offset], ldz, &t[m + 1 + (m + 1) * t_dim1], ldt, 4L);
    if (liniq1) {
        dlaset_("Full", n, n, &c_b62, &c_b63, &q1[q1_offset], ldq1, 4L);
    }
    if (liniq2) {
        dlaset_("Full", &m, &m, &c_b62, &c_b62, &q2[q2_offset], ldq2, 4L);
        dlaset_("Full", &m, &m, &c_b62, &c_b63, &q2[m + 1 + q2_dim1], ldq2, 4L);
        dlaset_("Full", &m, &m, &c_b62, &c_b63, &q2[(m + 1) * q2_dim1 + 1], ldq2, 4L);
        dlaset_("Full", &m, &m, &c_b62, &c_b62, &q2[m + 1 + (m + 1) * q2_dim1], ldq2, 4L);
    }
    if (liniu1) {
        dlaset_("Full", &m, &m, &c_b62, &c_b63, &u11[u11_offset], ldu11, 4L);
        dlaset_("Full", &m, &m, &c_b62, &c_b62, &u12[u12_offset], ldu12, 4L);
    }
    if (liniu2) {
        dlaset_("Full", &m, &m, &c_b62, &c_b63, &u21[u21_offset], ldu21, 4L);
        dlaset_("Full", &m, &m, &c_b62, &c_b62, &u22[u22_offset], ldu22, 4L);
    }
    /*     STEP 1: Block triangularize T and Z. */
    /*     (Note: Comments in the code beginning "Workspace:" describe the */
    /*     minimal amount of real workspace needed at that point in the */
    /*     code, as well as the preferred amount for good performance. */
    /*     NB refers to the optimal block size for the immediately */
    /*     following subroutine, as returned by ILAENV.) */
    itau = 1;
    iwrk = itau + m;
    /*                                 ( T11 ) */
    /*     Perform a QR decomposition, (     ) = Q1*R1. */
    /*                                 ( T21 ) */
    /*     Workspace:    need   IWRK + M - 1; */
    /*                   prefer IWRK + M*NB - 1. */
    i__1 = *ldwork - iwrk + 1;
    dgeqrf_(n, &m, &t[t_offset], ldt, &dwork[itau], &dwork[iwrk], &i__1, info);
    /*            ( T12 ) */
    /*     Update (     ). */
    /*            ( T22 ) */
    /*     Workspace:    need   IWRK + M - 1; */
    /*                   prefer IWRK + M*NB - 1. */
    i__1 = *ldwork - iwrk + 1;
    dormqr_("Left", "Transpose", n, &m, &m, &t[t_offset], ldt, &dwork[itau],
        &t[(m + 1) * t_dim1 + 1], ldt, &dwork[iwrk], &i__1, info, 4L, 9L);
    /*     Update H. */
    /*     Workspace:    need   IWRK + N - 1; */
    /*                   prefer IWRK + N*NB - 1. */
    i__1 = *ldwork - iwrk + 1;
    dormqr_("Left", "Transpose", n, n, &m, &t[t_offset], ldt, &dwork[itau], &h__[h_offset], ldh,
        &dwork[iwrk], &i__1, info, 4L, 9L);
    if (lcmpq1) {
        /*        Update Q1. */
        i__1 = *ldwork - iwrk + 1;
        dormqr_("Right", "No Transpose", n, n, &m, &t[t_offset], ldt, &dwork[itau], &q1[q1_offset],
            ldq1, &dwork[iwrk], &i__1, info, 5L, 12L);
    }
    /*     Set the strictly lower triangular part of [ T11; T21 ] to zero. */
    i__1 = *n - 1;
    dlaset_("Lower", &i__1, &m, &c_b62, &c_b62, &t[t_dim1 + 2], ldt, 5L);
    /*     Perform an RQ decomposition, T22 = R2*Q2. */
    /*     Workspace:    need   IWRK + M - 1; */
    /*                   prefer IWRK + M*NB - 1. */
    itau = mm + 1;
    iwrk = itau + m;
    ma02ad_("Full", &m, &m, &t[m + 1 + (m + 1) * t_dim1], ldt, &dwork[1], &m, 4L);
    i__1 = *ldwork - iwrk + 1;
    dgerqf_(&m, &m, &dwork[1], &m, &dwork[itau], &dwork[iwrk], &i__1, info);
    ma02ad_("Upper", &m, &m, &dwork[1], &m, &t[m + 1 + (m + 1) * t_dim1], ldt, 5L);
    /*     Set the strictly upper triangular part of T22 to zero. */
    if (m > 1) {
        i__1 = m - 1;
        i__2 = m - 1;
        dlaset_("Upper", &i__1, &i__2, &c_b62, &c_b62, &t[m + 1 + (m + 2) * t_dim1], ldt, 5L);
    }
    /*     Update H. */
    /*     Workspace:    need   IWRK + N - 1; */
    /*                   prefer IWRK + N*NB - 1. */
    i__1 = *ldwork - iwrk + 1;
    dormrq_("Left", "No Transpose", &m, n, &m, &dwork[1], &m, &dwork[itau], &h__[m + 1 + h_dim1],
        ldh, &dwork[iwrk], &i__1, info, 4L, 12L);
    if (lcmpq1) {
        /*        Update Q1. */
        i__1 = *ldwork - iwrk + 1;
        dormrq_("Right", "Transpose", n, &m, &m, &dwork[1], &m, &dwork[itau],
            &q1[(m + 1) * q1_dim1 + 1], ldq1, &dwork[iwrk], &i__1, info, 5L, 9L);
    }
    /*     Perform a QR decomposition, ( Z21  Z22 )' = Q3*R3. */
    /*     Workspace:    need   IWRK + M - 1; */
    /*                   prefer IWRK + M*NB - 1. */
    itau = m * *n + 1;
    iwrk = itau + m;
    ma02ad_("Full", &m, n, &z__[m + 1 + z_dim1], ldz, &dwork[1], n, 4L);
    i__1 = *ldwork - iwrk + 1;
    dgeqrf_(n, &m, &dwork[1], n, &dwork[itau], &dwork[iwrk], &i__1, info);
    /*     Update ( Z11  Z12 ). */
    i__1 = *ldwork - iwrk + 1;
    dormqr_("Right", "No Transpose", &m, n, &m, &dwork[1], n, &dwork[itau], &z__[z_offset], ldz,
        &dwork[iwrk], &i__1, info, 5L, 12L);
    ma02ad_("Upper", &m, &m, &dwork[1], n, &z__[m + 1 + (m + 1) * z_dim1], ldz, 5L);
    /*     Set the strictly upper triangular part of Z22 to zero. */
    if (m > 1) {
        i__1 = m - 1;
        i__2 = m - 1;
        dlaset_("Upper", &i__1, &i__2, &c_b62, &c_b62, &z__[m + 1 + (m + 2) * z_dim1], ldz, 5L);
    }
    /*     Update H. */
    i__1 = *ldwork - iwrk + 1;
    dormqr_("Right", "No Transpose", n, n, &m, &dwork[1], n, &dwork[itau], &h__[h_offset], ldh,
        &dwork[iwrk], &i__1, info, 5L, 12L);
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
        dswap_(n, &h__[i__ * h_dim1 + 1], &c__1, &h__[(m + i__) * h_dim1 + 1], &c__1);
        /* L30: */
    }
    if (lcmpq2) {
        /*        Update Q2. */
        i__1 = *ldwork - iwrk + 1;
        dormqr_("Left", "No Transpose", n, n, &m, &dwork[1], n, &dwork[itau], &q2[q2_offset], ldq2,
            &dwork[iwrk], &i__1, info, 4L, 12L);
    }
    /*     Perform an RQ decomposition Z12 = R4*Q4. */
    itau = 1;
    iwrk = itau + m;
    i__1 = *ldwork - iwrk + 1;
    dgerqf_(&m, &m, &z__[(m + 1) * z_dim1 + 1], ldz, &dwork[itau], &dwork[iwrk], &i__1, info);
    /*     Update H. */
    i__1 = *ldwork - iwrk + 1;
    dormrq_("Right", "Transpose", n, &m, &m, &z__[(m + 1) * z_dim1 + 1], ldz, &dwork[itau],
        &h__[h_offset], ldh, &dwork[iwrk], &i__1, info, 5L, 9L);
    if (lcmpq2) {
        /*        Update Q2. */
        i__1 = *ldwork - iwrk + 1;
        dormrq_("Right", "Transpose", n, &m, &m, &z__[(m + 1) * z_dim1 + 1], ldz, &dwork[itau],
            &q2[q2_offset], ldq2, &dwork[iwrk], &i__1, info, 5L, 9L);
    }
    /*     Exchange Z11 and Z12 and set the strictly lower triangular part */
    /*     of Z11 to zero. */
    dum[0] = 0.;
    i__1 = m - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
        dswap_(&m, &z__[i__ * z_dim1 + 1], &c__1, &z__[(m + i__) * z_dim1 + 1], &c__1);
        i__2 = m - i__;
        dcopy_(&i__2, dum, &c__0, &z__[i__ + 1 + i__ * z_dim1], &c__1);
        /* L40: */
    }
    dswap_(&m, &z__[m * z_dim1 + 1], &c__1, &z__[*n * z_dim1 + 1], &c__1);
    /*     STEP 2: Eliminations in H. */
    i__1 = m;
    for (k = 1; k <= i__1; ++k) {
        /*        I. Annihilate H(m+k:n-1,k). */
        i__2 = m - 1;
        for (j = k; j <= i__2; ++j) {
            /*           Determine a Givens rotation to annihilate H(m+j,k) from the */
            /*           left. */
            dlartg_(&h__[m + j + 1 + k * h_dim1], &h__[m + j + k * h_dim1], &co, &si, &tmp1);
            /*           Update H. */
            h__[m + j + 1 + k * h_dim1] = tmp1;
            h__[m + j + k * h_dim1] = 0.;
            i__3 = *n - k;
            drot_(&i__3, &h__[m + j + 1 + (k + 1) * h_dim1], ldh, &h__[m + j + (k + 1) * h_dim1],
                ldh, &co, &si);
            /*           Update T. */
            i__3 = j + 1;
            drot_(&i__3, &t[m + j + 1 + (m + 1) * t_dim1], ldt, &t[m + j + (m + 1) * t_dim1], ldt,
                &co, &si);
            if (lcmpq1) {
                /*              Update Q1. */
                drot_(n, &q1[(m + j + 1) * q1_dim1 + 1], &c__1, &q1[(m + j) * q1_dim1 + 1], &c__1,
                    &co, &si);
            }
            /*           Determine a Givens rotation to annihilate T(m+j,m+j+1) from */
            /*           the right. */
            dlartg_(
                &t[m + j + (m + j) * t_dim1], &t[m + j + (m + j + 1) * t_dim1], &co, &si, &tmp1);
            /*           Update T. */
            drot_(
                &m, &t[(m + j) * t_dim1 + 1], &c__1, &t[(m + j + 1) * t_dim1 + 1], &c__1, &co, &si);
            t[m + j + (m + j) * t_dim1] = tmp1;
            t[m + j + (m + j + 1) * t_dim1] = 0.;
            i__3 = m - j;
            drot_(&i__3, &t[m + j + 1 + (m + j) * t_dim1], &c__1,
                &t[m + j + 1 + (m + j + 1) * t_dim1], &c__1, &co, &si);
            i__3 = j + 1;
            drot_(&i__3, &t[j * t_dim1 + 1], &c__1, &t[(j + 1) * t_dim1 + 1], &c__1, &co, &si);
            if (lcmpu1) {
                /*              Update U11 and U12. */
                drot_(&m, &u11[j * u11_dim1 + 1], &c__1, &u11[(j + 1) * u11_dim1 + 1], &c__1, &co,
                    &si);
                drot_(&m, &u12[j * u12_dim1 + 1], &c__1, &u12[(j + 1) * u12_dim1 + 1], &c__1, &co,
                    &si);
            }
            /*           Determine a Givens rotation to annihilate T(j+1,j) from the */
            /*           left. */
            dlartg_(&t[j + j * t_dim1], &t[j + 1 + j * t_dim1], &co, &si, &tmp1);
            /*           Update T. */
            t[j + j * t_dim1] = tmp1;
            t[j + 1 + j * t_dim1] = 0.;
            i__3 = *n - j;
            drot_(
                &i__3, &t[j + (j + 1) * t_dim1], ldt, &t[j + 1 + (j + 1) * t_dim1], ldt, &co, &si);
            /*           Update H. */
            i__3 = *n - k + 1;
            drot_(&i__3, &h__[j + k * h_dim1], ldh, &h__[j + 1 + k * h_dim1], ldh, &co, &si);
            if (lcmpq1) {
                /*              Update Q1. */
                drot_(n, &q1[j * q1_dim1 + 1], &c__1, &q1[(j + 1) * q1_dim1 + 1], &c__1, &co, &si);
            }
            /* L50: */
        }
        /*        II. Annihilate H(n,k). */
        /*        Determine a Givens rotation to annihilate H(n,k) form the left. */
        dlartg_(&h__[m + k * h_dim1], &h__[*n + k * h_dim1], &co, &si, &tmp1);
        /*        Update H. */
        h__[m + k * h_dim1] = tmp1;
        h__[*n + k * h_dim1] = 0.;
        i__2 = *n - k;
        drot_(&i__2, &h__[m + (k + 1) * h_dim1], ldh, &h__[*n + (k + 1) * h_dim1], ldh, &co, &si);
        /*        Update T. */
        drot_(&m, &t[m + (m + 1) * t_dim1], ldt, &t[*n + (m + 1) * t_dim1], ldt, &co, &si);
        tmp1 = -si * t[m + m * t_dim1];
        t[m + m * t_dim1] = co * t[m + m * t_dim1];
        if (lcmpq1) {
            /*           Update Q1. */
            drot_(n, &q1[m * q1_dim1 + 1], &c__1, &q1[*n * q1_dim1 + 1], &c__1, &co, &si);
        }
        /*        Determine a Givens rotation to annihilate T(n,m) from the */
        /*        right. */
        dlartg_(&t[*n + *n * t_dim1], &tmp1, &co, &si, &tmp2);
        /*        Update T. */
        drot_(&m, &t[*n * t_dim1 + 1], &c__1, &t[m * t_dim1 + 1], &c__1, &co, &si);
        t[*n + *n * t_dim1] = tmp2;
        if (lcmpu1) {
            /*           Update U11 and U12. */
            drot_(&m, &u12[m * u12_dim1 + 1], &c__1, &u11[m * u11_dim1 + 1], &c__1, &co, &si);
        }
        /*        III. Annihilate H(k+1:m,k). */
        i__2 = k + 1;
        for (j = m; j >= i__2; --j) {
            /*           Determine a Givens rotation to annihilate H(j,k) from the */
            /*           left. */
            dlartg_(&h__[j - 1 + k * h_dim1], &h__[j + k * h_dim1], &co, &si, &tmp1);
            /*           Update H. */
            h__[j - 1 + k * h_dim1] = tmp1;
            h__[j + k * h_dim1] = 0.;
            i__3 = *n - k;
            drot_(&i__3, &h__[j - 1 + (k + 1) * h_dim1], ldh, &h__[j + (k + 1) * h_dim1], ldh, &co,
                &si);
            /*           Update T. */
            i__3 = *n - j + 2;
            drot_(
                &i__3, &t[j - 1 + (j - 1) * t_dim1], ldt, &t[j + (j - 1) * t_dim1], ldt, &co, &si);
            if (lcmpq1) {
                /*              Update Q1. */
                drot_(n, &q1[(j - 1) * q1_dim1 + 1], &c__1, &q1[j * q1_dim1 + 1], &c__1, &co, &si);
            }
            /*           Determine a Givens rotation to annihilate T(j,j-1) from the */
            /*           right. */
            dlartg_(&t[j + j * t_dim1], &t[j + (j - 1) * t_dim1], &co, &si, &tmp1);
            /*           Update T. */
            drot_(
                &m, &t[(m + j) * t_dim1 + 1], &c__1, &t[(m + j - 1) * t_dim1 + 1], &c__1, &co, &si);
            i__3 = m - j + 2;
            drot_(&i__3, &t[m + j - 1 + (m + j) * t_dim1], &c__1,
                &t[m + j - 1 + (m + j - 1) * t_dim1], &c__1, &co, &si);
            t[j + j * t_dim1] = tmp1;
            t[j + (j - 1) * t_dim1] = 0.;
            i__3 = j - 1;
            drot_(&i__3, &t[j * t_dim1 + 1], &c__1, &t[(j - 1) * t_dim1 + 1], &c__1, &co, &si);
            if (lcmpu1) {
                /*              Update U11 and U12. */
                drot_(&m, &u11[j * u11_dim1 + 1], &c__1, &u11[(j - 1) * u11_dim1 + 1], &c__1, &co,
                    &si);
                drot_(&m, &u12[j * u12_dim1 + 1], &c__1, &u12[(j - 1) * u12_dim1 + 1], &c__1, &co,
                    &si);
            }
            /*           Determine a Givens rotation to annihilate T(m+j-1,m-j) from */
            /*           the left. */
            dlartg_(
                &t[m + j + (m + j) * t_dim1], &t[m + j - 1 + (m + j) * t_dim1], &co, &si, &tmp1);
            /*           Update T. */
            t[m + j + (m + j) * t_dim1] = tmp1;
            t[m + j - 1 + (m + j) * t_dim1] = 0.;
            i__3 = j - 1;
            drot_(&i__3, &t[m + j + (m + 1) * t_dim1], ldt, &t[m + j - 1 + (m + 1) * t_dim1], ldt,
                &co, &si);
            /*           Update H. */
            i__3 = *n - k + 1;
            drot_(
                &i__3, &h__[m + j + k * h_dim1], ldh, &h__[m + j - 1 + k * h_dim1], ldh, &co, &si);
            if (lcmpq1) {
                /*              Update Q1. */
                drot_(n, &q1[(m + j) * q1_dim1 + 1], &c__1, &q1[(m + j - 1) * q1_dim1 + 1], &c__1,
                    &co, &si);
            }
            /* L60: */
        }
        /*        IV. Annihilate H(m+k,k+1:m-1). */
        i__2 = m - 1;
        for (j = k + 1; j <= i__2; ++j) {
            /*           Determine a Givens rotation to annihilate H(m+k,j) from the */
            /*           right. */
            dlartg_(&h__[m + k + (j + 1) * h_dim1], &h__[m + k + j * h_dim1], &co, &si, &tmp1);
            /*           Update H. */
            drot_(&m, &h__[(j + 1) * h_dim1 + 1], &c__1, &h__[j * h_dim1 + 1], &c__1, &co, &si);
            h__[m + k + (j + 1) * h_dim1] = tmp1;
            h__[m + k + j * h_dim1] = 0.;
            i__3 = m - k;
            drot_(&i__3, &h__[m + k + 1 + (j + 1) * h_dim1], &c__1, &h__[m + k + 1 + j * h_dim1],
                &c__1, &co, &si);
            /*           Update Z. */
            i__3 = j + 1;
            drot_(&i__3, &z__[(j + 1) * z_dim1 + 1], &c__1, &z__[j * z_dim1 + 1], &c__1, &co, &si);
            if (lcmpq2) {
                /*              Update Q2. */
                drot_(n, &q2[(j + 1) * q2_dim1 + 1], &c__1, &q2[j * q2_dim1 + 1], &c__1, &co, &si);
            }
            /*           Determine a Givens rotation to annihilate Z(j+1,j) from the */
            /*           left. */
            dlartg_(&z__[j + j * z_dim1], &z__[j + 1 + j * z_dim1], &co, &si, &tmp1);
            /*           Update Z. */
            z__[j + j * z_dim1] = tmp1;
            z__[j + 1 + j * z_dim1] = 0.;
            i__3 = *n - j;
            drot_(&i__3, &z__[j + (j + 1) * z_dim1], ldz, &z__[j + 1 + (j + 1) * z_dim1], ldz, &co,
                &si);
            i__3 = j + 1;
            drot_(&i__3, &z__[m + j + (m + 1) * z_dim1], ldz, &z__[m + j + 1 + (m + 1) * z_dim1],
                ldz, &co, &si);
            if (lcmpu2) {
                /*              Update U21 and U22. */
                drot_(&m, &u21[j * u21_dim1 + 1], &c__1, &u21[(j + 1) * u21_dim1 + 1], &c__1, &co,
                    &si);
                drot_(&m, &u22[j * u22_dim1 + 1], &c__1, &u22[(j + 1) * u22_dim1 + 1], &c__1, &co,
                    &si);
            }
            /*           Determine a Givens rotation to annihilate Z(m+j,m+j+1) from */
            /*           the right. */
            dlartg_(&z__[m + j + (m + j) * z_dim1], &z__[m + j + (m + j + 1) * z_dim1], &co, &si,
                &tmp1);
            /*           Update Z. */
            z__[m + j + (m + j) * z_dim1] = tmp1;
            z__[m + j + (m + j + 1) * z_dim1] = 0.;
            drot_(&m, &z__[(m + j) * z_dim1 + 1], &c__1, &z__[(m + j + 1) * z_dim1 + 1], &c__1, &co,
                &si);
            i__3 = m - j;
            drot_(&i__3, &z__[m + j + 1 + (m + j) * z_dim1], &c__1,
                &z__[m + j + 1 + (m + j + 1) * z_dim1], &c__1, &co, &si);
            /*           Update H. */
            drot_(&m, &h__[(m + j) * h_dim1 + 1], &c__1, &h__[(m + j + 1) * h_dim1 + 1], &c__1, &co,
                &si);
            i__3 = m - k + 1;
            drot_(&i__3, &h__[m + k + (m + j) * h_dim1], &c__1, &h__[m + k + (m + j + 1) * h_dim1],
                &c__1, &co, &si);
            if (lcmpq2) {
                /*              Update Q2. */
                drot_(n, &q2[(m + j) * q2_dim1 + 1], &c__1, &q2[(m + j + 1) * q2_dim1 + 1], &c__1,
                    &co, &si);
            }
            /* L70: */
        }
        /*        V. Annihilate H(m+k,m). */
        if (k < m) {
            /*           Determine a Givens rotation to annihilate H(m+k,m) from the */
            /*           right. */
            dlartg_(&h__[m + k + *n * h_dim1], &h__[m + k + m * h_dim1], &co, &si, &tmp1);
            /*           Update H. */
            h__[m + k + *n * h_dim1] = tmp1;
            h__[m + k + m * h_dim1] = 0.;
            drot_(&m, &h__[*n * h_dim1 + 1], &c__1, &h__[m * h_dim1 + 1], &c__1, &co, &si);
            i__2 = m - k;
            drot_(&i__2, &h__[m + k + 1 + *n * h_dim1], &c__1, &h__[m + k + 1 + m * h_dim1], &c__1,
                &co, &si);
            /*           Update Z. */
            drot_(&m, &z__[*n * z_dim1 + 1], &c__1, &z__[m * z_dim1 + 1], &c__1, &co, &si);
            tmp1 = -si * z__[*n + *n * z_dim1];
            z__[*n + *n * z_dim1] = co * z__[*n + *n * z_dim1];
            if (lcmpq2) {
                /*              Update Q2. */
                drot_(n, &q2[*n * q2_dim1 + 1], &c__1, &q2[m * q2_dim1 + 1], &c__1, &co, &si);
            }
            /*           Determine a Givens rotation to annihilate Z(n,m) from the */
            /*           left. */
            dlartg_(&z__[m + m * z_dim1], &tmp1, &co, &si, &tmp2);
            /*           Update Z. */
            drot_(&m, &z__[m + (m + 1) * z_dim1], ldz, &z__[*n + (m + 1) * z_dim1], ldz, &co, &si);
            z__[m + m * z_dim1] = tmp2;
            if (lcmpu2) {
                /*              Update U2. */
                drot_(&m, &u21[m * u21_dim1 + 1], &c__1, &u22[m * u22_dim1 + 1], &c__1, &co, &si);
            }
        } else {
            /*           Determine a Givens rotation to annihilate H(n,m) from the */
            /*           left. */
            dlartg_(&h__[m + m * h_dim1], &h__[*n + m * h_dim1], &co, &si, &tmp1);
            /*           Update H. */
            h__[m + m * h_dim1] = tmp1;
            h__[*n + m * h_dim1] = 0.;
            drot_(&m, &h__[m + (m + 1) * h_dim1], ldh, &h__[*n + (m + 1) * h_dim1], ldh, &co, &si);
            /*           Update T. */
            drot_(&m, &t[m + (m + 1) * t_dim1], ldt, &t[*n + (m + 1) * t_dim1], ldt, &co, &si);
            t[m + m * t_dim1] = co * t[m + m * t_dim1];
            if (lcmpq1) {
                /*              Update Q1. */
                drot_(n, &q1[m * q1_dim1 + 1], &c__1, &q1[*n * q1_dim1 + 1], &c__1, &co, &si);
            }
            /*           Determine a Givens rotation to annihilate T( N, M ) from the */
            /*           right. */
            d__1 = -si * t[m + m * t_dim1];
            dlartg_(&t[*n + *n * t_dim1], &d__1, &co, &si, &tmp2);
            /*           Update T. */
            drot_(&m, &t[*n * t_dim1 + 1], &c__1, &t[m * t_dim1 + 1], &c__1, &co, &si);
            t[*n + *n * t_dim1] = tmp2;
            if (lcmpu1) {
                /*              Update U1. */
                drot_(&m, &u12[m * u12_dim1 + 1], &c__1, &u11[m * u11_dim1 + 1], &c__1, &co, &si);
            }
        }
        /*        VI. Annihilate H(m+k,m+k+2:n). */
        i__2 = k + 2;
        for (j = m; j >= i__2; --j) {
            /*           Determine a Givens rotation to annihilate H(m+k,m+j) from */
            /*           the right. */
            dlartg_(&h__[m + k + (m + j - 1) * h_dim1], &h__[m + k + (m + j) * h_dim1], &co, &si,
                &tmp1);
            /*           Update H. */
            drot_(&m, &h__[(m + j - 1) * h_dim1 + 1], &c__1, &h__[(m + j) * h_dim1 + 1], &c__1, &co,
                &si);
            h__[m + k + (m + j - 1) * h_dim1] = tmp1;
            h__[m + k + (m + j) * h_dim1] = 0.;
            i__3 = m - k;
            drot_(&i__3, &h__[m + k + 1 + (m + j - 1) * h_dim1], &c__1,
                &h__[m + k + 1 + (m + j) * h_dim1], &c__1, &co, &si);
            /*           Update Z. */
            drot_(&m, &z__[(m + j - 1) * z_dim1 + 1], &c__1, &z__[(m + j) * z_dim1 + 1], &c__1, &co,
                &si);
            i__3 = m - j + 2;
            drot_(&i__3, &z__[m + j - 1 + (m + j - 1) * z_dim1], &c__1,
                &z__[m + j - 1 + (m + j) * z_dim1], &c__1, &co, &si);
            if (lcmpq2) {
                /*              Update Q2. */
                drot_(n, &q2[(m + j - 1) * q2_dim1 + 1], &c__1, &q2[(m + j) * q2_dim1 + 1], &c__1,
                    &co, &si);
            }
            /*           Determine a Givens rotation to annihilate Z(m+j-1,m+j) from */
            /*           the left. */
            dlartg_(&z__[m + j + (m + j) * z_dim1], &z__[m + j - 1 + (m + j) * z_dim1], &co, &si,
                &tmp1);
            /*           Update Z. */
            z__[m + j + (m + j) * z_dim1] = tmp1;
            z__[m + j - 1 + (m + j) * z_dim1] = 0.;
            i__3 = j - 1;
            drot_(&i__3, &z__[m + j + (m + 1) * z_dim1], ldz, &z__[m + j - 1 + (m + 1) * z_dim1],
                ldz, &co, &si);
            i__3 = *n - j + 2;
            drot_(&i__3, &z__[j + (j - 1) * z_dim1], ldz, &z__[j - 1 + (j - 1) * z_dim1], ldz, &co,
                &si);
            if (lcmpu2) {
                /*              Update U2. */
                drot_(&m, &u21[j * u21_dim1 + 1], &c__1, &u21[(j - 1) * u21_dim1 + 1], &c__1, &co,
                    &si);
                drot_(&m, &u22[j * u22_dim1 + 1], &c__1, &u22[(j - 1) * u22_dim1 + 1], &c__1, &co,
                    &si);
            }
            /*           Determine a Givens rotation to annihilate Z(j,j-1) from the */
            /*           right. */
            dlartg_(&z__[j + j * z_dim1], &z__[j + (j - 1) * z_dim1], &co, &si, &tmp1);
            /*           Update Z. */
            z__[j + j * z_dim1] = tmp1;
            z__[j + (j - 1) * z_dim1] = 0.;
            i__3 = j - 1;
            drot_(&i__3, &z__[j * z_dim1 + 1], &c__1, &z__[(j - 1) * z_dim1 + 1], &c__1, &co, &si);
            /*           Update H. */
            drot_(&m, &h__[j * h_dim1 + 1], &c__1, &h__[(j - 1) * h_dim1 + 1], &c__1, &co, &si);
            i__3 = m - k + 1;
            drot_(&i__3, &h__[m + k + j * h_dim1], &c__1, &h__[m + k + (j - 1) * h_dim1], &c__1,
                &co, &si);
            if (lcmpq2) {
                /*              Update Q2. */
                drot_(n, &q2[j * q2_dim1 + 1], &c__1, &q2[(j - 1) * q2_dim1 + 1], &c__1, &co, &si);
            }
            /* L80: */
        }
        /* L90: */
    }
    /*     Now T, Z, H are in block forms (1) and H22' is upper Hessenberg. */
    /*     STEP 3: Apply periodic QZ algorithm to the generalized matrix */
    /*                            -1    -1        -1    -1 */
    /*             product H22 T22   T11   H11 Z11   Z22   to transform H22' */
    /*             to upper quasi-triangular form while T11, T22', Z11, Z22', */
    /*             and H11 stay in upper triangular form. */
    /*     Determine mode of computations. */
    if (ltri || lcmpq1 || lcmpq2 || lcmpu1 || lcmpu2) {
        s_copy(cmpq, "Initialize", 16L, 10L);
        iq = 1;
        imat = mm * 6 + 1;
        iwrk = mm * 12 + 1;
    } else {
        s_copy(cmpq, "No Computation", 16L, 14L);
        imat = 1;
        iwrk = mm * 6 + 1;
    }
    if (ltri) {
        s_copy(cmpsc, "Schur Form", 16L, 10L);
    } else {
        s_copy(cmpsc, "Eigenvalues Only", 16L, 16L);
    }
    /*     Save matrices in structure that is required by MB03BD. */
    ma02ad_("Lower", &m, &m, &h__[m + 1 + (m + 1) * h_dim1], ldh, &dwork[imat], &m, 5L);
    i__1 = m - 1;
    i__2 = *ldh + 1;
    i__3 = m + 1;
    dcopy_(&i__1, &h__[m + 1 + (m + 2) * h_dim1], &i__2, &dwork[imat + 1], &i__3);
    i__1 = m - 2;
    i__2 = m - 2;
    dlaset_("Lower", &i__1, &i__2, &c_b62, &c_b62, &dwork[imat + 2], &m, 5L);
    ma02ad_("Lower", &m, &m, &t[m + 1 + (m + 1) * t_dim1], ldt, &dwork[imat + mm], &m, 5L);
    i__1 = m - 1;
    i__2 = m - 1;
    dlaset_("Lower", &i__1, &i__2, &c_b62, &c_b62, &dwork[imat + mm + 1], &m, 5L);
    dlacpy_("Upper", &m, &m, &t[t_offset], ldt, &dwork[imat + (mm << 1)], &m, 5L);
    i__1 = m - 1;
    i__2 = m - 1;
    dlaset_("Lower", &i__1, &i__2, &c_b62, &c_b62, &dwork[imat + (mm << 1) + 1], &m, 5L);
    dlacpy_("Upper", &m, &m, &h__[h_offset], ldh, &dwork[imat + mm * 3], &m, 5L);
    i__1 = m - 1;
    i__2 = m - 1;
    dlaset_("Lower", &i__1, &i__2, &c_b62, &c_b62, &dwork[imat + mm * 3 + 1], &m, 5L);
    dlacpy_("Upper", &m, &m, &z__[z_offset], ldz, &dwork[imat + (mm << 2)], &m, 5L);
    i__1 = m - 1;
    i__2 = m - 1;
    dlaset_("Lower", &i__1, &i__2, &c_b62, &c_b62, &dwork[imat + (mm << 2) + 1], &m, 5L);
    ma02ad_("Lower", &m, &m, &z__[m + 1 + (m + 1) * z_dim1], ldz, &dwork[imat + mm * 5], &m, 5L);
    i__1 = m - 1;
    i__2 = m - 1;
    dlaset_("Lower", &i__1, &i__2, &c_b62, &c_b62, &dwork[imat + mm * 5 + 1], &m, 5L);
    iwork[m + 1] = 1;
    iwork[m + 2] = -1;
    iwork[m + 3] = -1;
    iwork[m + 4] = 1;
    iwork[m + 5] = -1;
    iwork[m + 6] = -1;
    /*     Apply periodic QZ algorithm. */
    /*     Workspace:          need   IWRK + MAX( 2*M, 48 ) - 1. */
    /*     Integer workspace:  need   M + 18. */
    i__1 = *liwork - (m + 6);
    i__2 = *ldwork - iwrk + 1;
    mb03bd_(cmpsc, "Careful", cmpq, idum, &c__6, &m, &c__1, &c__1, &m, &iwork[m + 1], &dwork[imat],
        &m, &m, &dwork[iq], &m, &m, &alphar[1], &alphai[1], &beta[1], &iwork[1], &iwork[m + 7],
        &i__1, &dwork[iwrk], &i__2, &iwarn, info, 16L, 7L, 16L);
    if (iwarn > 0) {
        *info = 1;
        return 0;
    } else if (*info > 0) {
        *info = 2;
        return 0;
    }
    /*     Compute the "non-negative" eigenvalues of the pencil aTZ - bH. */
    /*     These are the eigenvalues with positive real parts or with */
    /*     non-negative imaginary parts, when real parts are zero. */
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
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
        if ((doublereal)iwork[i__] >= emin * 2 && (doublereal)iwork[i__] <= emax * 2) {
            /*           If B = SQRT(BASE**IWORK(i)) is between underflow and */
            /*           overflow threshold, BETA(i) is divided by B. */
            d__1 = iwork[i__] * .5;
            beta[i__] /= pow_dd(&base, &d__1);
            iwork[i__] = 0;
        } else {
            /*           The eigenvalues are defined by ALPHAR, ALPHAI, BETA, and */
            /*           IWORK, as for the SLICOT Library routine MB03BD. */
            *info = 3;
            if (iwork[i__] % 2 != 0) {
                beta[i__] /= sqrb;
            }
            iwork[i__] /= 2;
        }
        /* L100: */
    }
    if (ltri) {
        /*        Update H. */
        dlacpy_("Upper", &m, &m, &dwork[imat + mm * 3], &m, &h__[h_offset], ldh, 5L);
        ma02ad_("Full", &m, &m, &dwork[imat], &m, &h__[m + 1 + (m + 1) * h_dim1], ldh, 4L);
        dgemm_("Transpose", "No Transpose", &m, &m, &m, &c_b63, &dwork[iq + mm * 3], &m,
            &h__[(m + 1) * h_dim1 + 1], ldh, &c_b62, &dwork[imat], &m, 9L, 12L);
        dgemm_("No Transpose", "No Transpose", &m, &m, &m, &c_b63, &dwork[imat], &m, &dwork[1], &m,
            &c_b62, &h__[(m + 1) * h_dim1 + 1], ldh, 12L, 12L);
        /*        Update T. */
        dlacpy_("Upper", &m, &m, &dwork[imat + (mm << 1)], &m, &t[t_offset], ldt, 5L);
        dgemm_("Transpose", "No Transpose", &m, &m, &m, &c_b63, &dwork[iq + mm * 3], &m,
            &t[(m + 1) * t_dim1 + 1], ldt, &c_b62, &dwork[imat], &m, 9L, 12L);
        dgemm_("No Transpose", "No Transpose", &m, &m, &m, &c_b63, &dwork[imat], &m,
            &dwork[iq + (mm << 1)], &m, &c_b62, &t[(m + 1) * t_dim1 + 1], ldt, 12L, 12L);
        ma02ad_("Upper", &m, &m, &dwork[imat + mm], &m, &t[m + 1 + (m + 1) * t_dim1], ldt, 5L);
        /*        Update Z. */
        dlacpy_("Upper", &m, &m, &dwork[imat + (mm << 2)], &m, &z__[z_offset], ldz, 5L);
        dgemm_("Transpose", "No Transpose", &m, &m, &m, &c_b63, &dwork[iq + mm * 5], &m,
            &z__[(m + 1) * z_dim1 + 1], ldz, &c_b62, &dwork[imat], &m, 9L, 12L);
        dgemm_("No Transpose", "No Transpose", &m, &m, &m, &c_b63, &dwork[imat], &m, &dwork[1], &m,
            &c_b62, &z__[(m + 1) * z_dim1 + 1], ldz, 12L, 12L);
        ma02ad_(
            "Upper", &m, &m, &dwork[imat + mm * 5], &m, &z__[m + 1 + (m + 1) * z_dim1], ldz, 5L);
        if (lcmpq1) {
            /*           Update Q1. */
            dgemm_("No Transpose", "No Transpose", n, &m, &m, &c_b63, &q1[q1_offset], ldq1,
                &dwork[iq + mm * 3], &m, &c_b62, &dwork[imat], n, 12L, 12L);
            dlacpy_("Full", n, &m, &dwork[imat], n, &q1[q1_offset], ldq1, 4L);
            dgemm_("No Transpose", "No Transpose", n, &m, &m, &c_b63, &q1[(m + 1) * q1_dim1 + 1],
                ldq1, &dwork[iq + mm], &m, &c_b62, &dwork[imat], n, 12L, 12L);
            dlacpy_("Full", n, &m, &dwork[imat], n, &q1[(m + 1) * q1_dim1 + 1], ldq1, 4L);
        }
        if (lcmpq2) {
            /*           Update Q2. */
            dgemm_("No Transpose", "No Transpose", n, &m, &m, &c_b63, &q2[q2_offset], ldq2,
                &dwork[iq + (mm << 2)], &m, &c_b62, &dwork[imat], n, 12L, 12L);
            dlacpy_("Full", n, &m, &dwork[imat], n, &q2[q2_offset], ldq2, 4L);
            dgemm_("No Transpose", "No Transpose", n, &m, &m, &c_b63, &q2[(m + 1) * q2_dim1 + 1],
                ldq2, &dwork[1], &m, &c_b62, &dwork[imat], n, 12L, 12L);
            dlacpy_("Full", n, &m, &dwork[imat], n, &q2[(m + 1) * q2_dim1 + 1], ldq2, 4L);
        }
        if (lcmpu1) {
            /*           Update U11 and U12. */
            dgemm_("No Transpose", "No Transpose", &m, &m, &m, &c_b63, &u11[u11_offset], ldu11,
                &dwork[iq + (mm << 1)], &m, &c_b62, &dwork[imat], &m, 12L, 12L);
            dlacpy_("Full", &m, &m, &dwork[imat], &m, &u11[u11_offset], ldu11, 4L);
            dgemm_("No Transpose", "No Transpose", &m, &m, &m, &c_b63, &u12[u12_offset], ldu12,
                &dwork[iq + (mm << 1)], &m, &c_b62, &dwork[imat], &m, 12L, 12L);
            dlacpy_("Full", &m, &m, &dwork[imat], &m, &u12[u12_offset], ldu12, 4L);
        }
        if (lcmpu2) {
            /*           Update U21 and U22. */
            dgemm_("No Transpose", "No Transpose", &m, &m, &m, &c_b63, &u21[u21_offset], ldu21,
                &dwork[iq + mm * 5], &m, &c_b62, &dwork[imat], &m, 12L, 12L);
            dlacpy_("Full", &m, &m, &dwork[imat], &m, &u21[u21_offset], ldu21, 4L);
            dgemm_("No Transpose", "No Transpose", &m, &m, &m, &c_b63, &u22[u22_offset], ldu22,
                &dwork[iq + mm * 5], &m, &c_b62, &dwork[imat], &m, 12L, 12L);
            dlacpy_("Full", &m, &m, &dwork[imat], &m, &u22[u22_offset], ldu22, 4L);
        }
    }
    dwork[1] = (doublereal)optdw;
    dwork[2] = base;
    return 0;
    /* *** Last line of MB04AD *** */
} /* mb04ad_ */
