/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static doublereal c_b14 = 1.;
static integer c__2 = 2;
static integer c__3 = 3;
static integer c__1 = 1;
static doublereal c_b35 = 0.;
static doublereal c_b118 = .5;
static doublereal c_b274 = -1.;

EXPORTSYMBOL /* Subroutine */ int mb03id_(compq, compu, n, a, lda, c__, ldc, d__, ldd, b, ldb, f,
    ldf, q, ldq, u1, ldu1, u2, ldu2, neig, iwork, liwork, dwork, ldwork, info, compq_len,
    compu_len) char *compq,
    *compu;
integer* n;
doublereal* a;
integer* lda;
doublereal* c__;
integer* ldc;
doublereal* d__;
integer* ldd;
doublereal* b;
integer* ldb;
doublereal* f;
integer* ldf;
doublereal* q;
integer* ldq;
doublereal* u1;
integer* ldu1;
doublereal* u2;
integer *ldu2, *neig, *iwork, *liwork;
doublereal* dwork;
integer *ldwork, *info;
ftnlen compq_len;
ftnlen compu_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, d_dim1, d_offset, f_dim1,
        f_offset, q_dim1, q_offset, u1_dim1, u1_offset, u2_dim1, u2_offset, i__1, i__2, i__3, i__4,
        i__5;
    doublereal d__1;
    /* Builtin functions */
    double log(), d_sign();
    /* Local variables */
    static doublereal base, prec;
    static integer ncol, sdim;
    static doublereal nrmb;
    static integer idum[8], iupd;
    static doublereal tmpa, tmpc;
    static integer upds, nrow, itmp1, itmp2, itmp3, iwrk1, iwrk2, iwrk3, iwrk4, iwrk5;
    extern integer ma01cd_();
    extern /* Subroutine */ int ma02ad_();
    static integer i__, j, k;
    extern /* Subroutine */ int mb03bb_();
    static integer m;
    extern /* Subroutine */ int mb03cd_();
    static integer r__;
    extern /* Subroutine */ int mb03gd_(), dscal_();
    static doublereal lgbas;
    extern /* Subroutine */ int dgemm_();
    extern logical lsame_();
    extern /* Subroutine */ int mb01ru_(), mb01rx_();
    static logical lcmpq, liniq;
    static integer ncols;
    static logical lcmpu, liniu;
    extern /* Subroutine */ int dcopy_();
    static doublereal a2;
    static logical lupdq;
    static doublereal c2, f2;
    static logical lupdu;
    static integer i1;
    extern /* Subroutine */ int daxpy_();
    static integer optdw, nrows, ia, ib, ic;
    static doublereal q11, q12, q21, q22, u11, u12;
    extern doublereal dlamch_();
    static integer mm, ir, is, mp;
    extern doublereal dlanhs_();
    extern /* Subroutine */ int dlacpy_(), dlaset_(), xerbla_();
    static integer iqlole, ihuple, ib1, ib2, ib3, iqlori, iquple, iuuple, izlori, iqupri, izuple,
        iq1, iq2, iq3, iuupri, izupri, ibs;
    static doublereal par[2];
    static integer hlp;
    static doublereal dum[12] /* was [3][4] */;
    static integer ldw;
    static doublereal prd[12] /* was [2][2][3] */, tol;
    static integer dim1, dim2;
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
    /*     To move the eigenvalues with strictly negative real parts of an */
    /*     N-by-N real skew-Hamiltonian/Hamiltonian pencil aS - bH in */
    /*     structured Schur form, with */
    /*                          (  0  I  )      (  A  D  )      (  B  F  ) */
    /*       S = J Z' J' Z, J = (        ), Z = (        ), H = (        ), */
    /*                          ( -I  0  )      (  0  C  )      (  0 -B' ) */
    /*     to the leading principal subpencil, while keeping the triangular */
    /*     form. Above, A is upper triangular, B is upper quasi-triangular, */
    /*     and C is lower triangular. */
    /*     The matrices Z and H are transformed by an orthogonal symplectic */
    /*     matrix U and an orthogonal matrix Q such that */
    /*                       (  Aout  Dout  ) */
    /*       Zout = U' Z Q = (              ), and */
    /*                       (    0   Cout  ) */
    /*                                                                    (1) */
    /*                            (  Bout  Fout  ) */
    /*       Hout = J Q' J' H Q = (              ), */
    /*                            (    0  -Bout' ) */
    /*     where Aout, Bout and Cout remain in triangular form. */
    /*     Optionally, if COMPQ = 'I' or COMPQ = 'U', the orthogonal matrix Q */
    /*     that fulfills (1) is computed. */
    /*     Optionally, if COMPU = 'I' or COMPU = 'U', the orthogonal */
    /*     symplectic matrix */
    /*           (  U1  U2  ) */
    /*       U = (          ) */
    /*           ( -U2  U1  ) */
    /*     that fulfills (1) is computed. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     COMPQ   CHARACTER*1 */
    /*             Specifies whether or not the orthogonal transformations */
    /*             should be accumulated in the array Q, as follows: */
    /*             = 'N':  Q is not computed; */
    /*             = 'I':  the array Q is initialized internally to the unit */
    /*                     matrix, and the orthogonal matrix Q is returned; */
    /*             = 'U':  the array Q contains an orthogonal matrix Q0 on */
    /*                     entry, and the matrix Q0*Q is returned, where Q */
    /*                     is the product of the orthogonal transformations */
    /*                     that are applied to the pencil aS - bH to reorder */
    /*                     the eigenvalues. */
    /*     COMPU   CHARACTER*1 */
    /*             Specifies whether or not the orthogonal symplectic */
    /*             transformations should be accumulated in the arrays U1 and */
    /*             U2, as follows: */
    /*             = 'N':  U1 and U2 are not computed; */
    /*             = 'I':  the arrays U1 and U2 are initialized internally, */
    /*                     and the submatrices U1 and U2 defining the */
    /*                     orthogonal symplectic matrix U are returned; */
    /*             = 'U':  the arrays U1 and U2 contain the corresponding */
    /*                     submatrices of an orthogonal symplectic matrix U0 */
    /*                     on entry, and the updated submatrices U1 and U2 */
    /*                     of the matrix product U0*U are returned, where U */
    /*                     is the product of the orthogonal symplectic */
    /*                     transformations that are applied to the pencil */
    /*                     aS - bH to reorder the eigenvalues. */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the pencil aS - bH.  N >= 0, even. */
    /*     A       (input/output) DOUBLE PRECISION array, dimension */
    /*                            (LDA, N/2) */
    /*             On entry, the leading N/2-by-N/2 part of this array must */
    /*             contain the upper triangular matrix A. The elements of the */
    /*             strictly lower triangular part of this array are not used. */
    /*             On exit, the leading  N/2-by-N/2 part of this array */
    /*             contains the transformed matrix Aout. */
    /*     LDA     INTEGER */
    /*             The leading dimension of the array A.  LDA >= MAX(1, N/2). */
    /*     C       (input/output) DOUBLE PRECISION array, dimension */
    /*                            (LDC, N/2) */
    /*             On entry, the leading N/2-by-N/2 part of this array must */
    /*             contain the lower triangular matrix C. The elements of the */
    /*             strictly upper triangular part of this array are not used. */
    /*             On exit, the leading  N/2-by-N/2 part of this array */
    /*             contains the transformed matrix Cout. */
    /*     LDC     INTEGER */
    /*             The leading dimension of the array C.  LDC >= MAX(1, N/2). */
    /*     D       (input/output) DOUBLE PRECISION array, dimension */
    /*                            (LDD, N/2) */
    /*             On entry, the leading N/2-by-N/2 part of this array must */
    /*             contain the matrix D. */
    /*             On exit, the leading  N/2-by-N/2 part of this array */
    /*             contains the transformed matrix Dout. */
    /*     LDD     INTEGER */
    /*             The leading dimension of the array D.  LDD >= MAX(1, N/2). */
    /*     B       (input/output) DOUBLE PRECISION array, dimension */
    /*                            (LDB, N/2) */
    /*             On entry, the leading N/2-by-N/2 part of this array must */
    /*             contain the upper quasi-triangular matrix B. */
    /*             On exit, the leading  N/2-by-N/2 part of this array */
    /*             contains the transformed upper quasi-triangular part of */
    /*             the matrix Bout. */
    /*             The part below the first subdiagonal of this array is */
    /*             not referenced. */
    /*     LDB     INTEGER */
    /*             The leading dimension of the array B.  LDB >= MAX(1, N/2). */
    /*     F       (input/output) DOUBLE PRECISION array, dimension */
    /*                            (LDF, N/2) */
    /*             On entry, the leading N/2-by-N/2 part of this array must */
    /*             contain the upper triangular part of the symmetric matrix */
    /*             F. */
    /*             On exit, the leading  N/2-by-N/2 part of this array */
    /*             contains the transformed upper triangular part of the */
    /*             matrix Fout. */
    /*             The strictly lower triangular part of this array is not */
    /*             referenced, except for the element F(N/2,N/2-1), but its */
    /*             initial value is preserved. */
    /*     LDF     INTEGER */
    /*             The leading dimension of the array F.  LDF >= MAX(1, N/2). */
    /*     Q       (input/output) DOUBLE PRECISION array, dimension (LDQ, N) */
    /*             On entry, if COMPQ = 'U', then the leading N-by-N part of */
    /*             this array must contain a given matrix Q0, and on exit, */
    /*             the leading N-by-N part of this array contains the product */
    /*             of the input matrix Q0 and the transformation matrix Q */
    /*             used to transform the matrices S and H. */
    /*             On exit, if COMPQ = 'I', then the leading N-by-N part of */
    /*             this array contains the orthogonal transformation matrix */
    /*             Q. */
    /*             If COMPQ = 'N' this array is not referenced. */
    /*     LDQ     INTEGER */
    /*             The leading dimension of of the array Q. */
    /*             LDQ >= 1,         if COMPQ = 'N'; */
    /*             LDQ >= MAX(1, N), if COMPQ = 'I' or COMPQ = 'U'. */
    /*     U1      (input/output) DOUBLE PRECISION array, dimension */
    /*                            (LDU1, N/2) */
    /*             On entry, if COMPU = 'U', then the leading N/2-by-N/2 part */
    /*             of this array must contain the upper left block of a */
    /*             given matrix U0, and on exit, the leading N/2-by-N/2 part */
    /*             of this array contains the updated upper left block U1 of */
    /*             the product of the input matrix U0 and the transformation */
    /*             matrix U used to transform the matrices S and H. */
    /*             On exit, if COMPU = 'I', then the leading N/2-by-N/2 part */
    /*             of this array contains the upper left block U1 of the */
    /*             orthogonal symplectic transformation matrix U. */
    /*             If COMPU = 'N' this array is not referenced. */
    /*     LDU1    INTEGER */
    /*             The leading dimension of the array U1. */
    /*             LDU1 >= 1,           if COMPU = 'N'; */
    /*             LDU1 >= MAX(1, N/2), if COMPU = 'I' or COMPU = 'U'. */
    /*     U2      (input/output) DOUBLE PRECISION array, dimension */
    /*                            (LDU2, N/2) */
    /*             On entry, if COMPU = 'U', then the leading N/2-by-N/2 part */
    /*             of this array must contain the upper right block of a */
    /*             given matrix U0, and on exit, the leading N/2-by-N/2 part */
    /*             of this array contains the updated upper right block U2 of */
    /*             the product of the input matrix U0 and the transformation */
    /*             matrix U used to transform the matrices S and H. */
    /*             On exit, if COMPU = 'I', then the leading N/2-by-N/2 part */
    /*             of this array contains the upper right block U2 of the */
    /*             orthogonal symplectic transformation matrix U. */
    /*             If COMPU = 'N' this array is not referenced. */
    /*     LDU2    INTEGER */
    /*             The leading dimension of the array U2. */
    /*             LDU2 >= 1,           if COMPU = 'N'; */
    /*             LDU2 >= MAX(1, N/2), if COMPU = 'U' or COMPU = 'I'. */
    /*     NEIG    (output) INTEGER */
    /*             The number of eigenvalues in aS - bH with strictly */
    /*             negative real part. */
    /*     Workspace */
    /*     IWORK   INTEGER array, dimension (LIWORK) */
    /*     LIWORK  INTEGER */
    /*             The dimension of the array IWORK. */
    /*             LIWORK >= N+1. */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*     LDWORK  INTEGER */
    /*             The dimension of the array DWORK. */
    /*             If COMPQ = 'N', */
    /*                LDWORK >= MAX(2*N+48,171); */
    /*             if COMPQ = 'I' or COMPQ = 'U', */
    /*                LDWORK >= MAX(4*N+48,171). */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0: succesful exit; */
    /*             < 0: if INFO = -i, the i-th argument had an illegal value; */
    /*             = 1: the periodic QZ algorithm did not converge in SLICOT */
    /*                  Library routine MB03BB; */
    /*             = 2: an error occured during the execution of MB03CD; */
    /*             = 3: an error occured during the execution of MB03GD. */
    /*     METHOD */
    /*     The algorithm reorders the eigenvalues like the following scheme: */
    /*     Step 1: Reorder the eigenvalues in the subpencil aA - bB. */
    /*          I. Reorder the eigenvalues with negative real parts to the */
    /*             top. */
    /*         II. Reorder the eigenvalues with positive real parts to the */
    /*             bottom. */
    /*     Step 2: Reorder the remaining eigenvalues with negative real */
    /*             parts in the pencil aS - bH. */
    /*          I. Exchange the eigenvalues between the last diagonal block */
    /*             in aA - bB and the last diagonal block in aS - bH. */
    /*         II. Move the eigenvalues of the R-th block to the (MM+1)-th */
    /*             block, where R denotes the number of upper quasi- */
    /*             triangular blocks in aA - bB and MM denotes the current */
    /*             number of blocks in aA - bB with eigenvalues with negative */
    /*             real parts. */
    /*     The algorithm uses a sequence of orthogonal transformations as */
    /*     described on page 25 in [1]. To achieve those transformations the */
    /*     elementary subroutines MB03CD and MB03GD are called for the */
    /*     corresponding matrix structures. */
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
    /*     Chemnitz, November 21, 2008. */
    /*     V. Sima, Dec. 2009 (SLICOT version of the routine DHAFNX). */
    /*     REVISIONS */
    /*     V. Sima, Aug. 2009; Feb. 2010; Oct. 2010; Nov. 2010. */
    /*     KEYWORDS */
    /*     Eigenvalue reordering, upper (quasi-)triangular matrix, */
    /*     skew-Hamiltonian/Hamiltonian pencil, structured Schur form. */
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
    c_dim1 = *ldc;
    c_offset = c_dim1 + 1;
    c__ -= c_offset;
    d_dim1 = *ldd;
    d_offset = d_dim1 + 1;
    d__ -= d_offset;
    b_dim1 = *ldb;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    f_dim1 = *ldf;
    f_offset = f_dim1 + 1;
    f -= f_offset;
    q_dim1 = *ldq;
    q_offset = q_dim1 + 1;
    q -= q_offset;
    u1_dim1 = *ldu1;
    u1_offset = u1_dim1 + 1;
    u1 -= u1_offset;
    u2_dim1 = *ldu2;
    u2_offset = u2_dim1 + 1;
    u2 -= u2_offset;
    --iwork;
    --dwork;
    /* Function Body */
    m = *n / 2;
    liniq = lsame_(compq, "I", 1L, 1L);
    lupdq = lsame_(compq, "U", 1L, 1L);
    liniu = lsame_(compu, "I", 1L, 1L);
    lupdu = lsame_(compu, "U", 1L, 1L);
    lcmpq = liniq || lupdq;
    lcmpu = liniu || lupdu;
    if (lcmpq) {
        /* Computing MAX */
        i__1 = (*n << 2) + 48;
        optdw = max(i__1, 171);
    } else {
        /* Computing MAX */
        i__1 = (*n << 1) + 48;
        optdw = max(i__1, 171);
    }
    /*     Test the input arguments. */
    *info = 0;
    if (!(lsame_(compq, "N", 1L, 1L) || lcmpq)) {
        *info = -1;
    } else if (!(lsame_(compu, "N", 1L, 1L) || lcmpu)) {
        *info = -2;
    } else if (*n < 0 || *n % 2 != 0) {
        *info = -3;
    } else if (*lda < max(1, m)) {
        *info = -5;
    } else if (*ldc < max(1, m)) {
        *info = -7;
    } else if (*ldd < max(1, m)) {
        *info = -9;
    } else if (*ldb < max(1, m)) {
        *info = -11;
    } else if (*ldf < max(1, m)) {
        *info = -13;
    } else if (*ldq < 1 || lcmpq && *ldq < *n) {
        *info = -15;
    } else if (*ldu1 < 1 || lcmpu && *ldu1 < m) {
        *info = -17;
    } else if (*ldu2 < 1 || lcmpu && *ldu2 < m) {
        *info = -19;
    } else if (*liwork < *n + 1) {
        *info = -22;
    } else if (*ldwork < optdw) {
        *info = -24;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("MB03ID", &i__1, 6L);
        return 0;
    }
    /*     Quick return if possible. */
    if (*n == 0) {
        *neig = 0;
        return 0;
    }
    /*     Determine machine constants. */
    prec = dlamch_("Precision", 9L);
    base = dlamch_("Base", 4L);
    lgbas = log(base);
    /* Computing MIN */
    d__1 = (doublereal)m;
    tol = min(d__1, 10.) * prec;
    par[0] = prec;
    par[1] = dlamch_("Safe minimum", 12L);
    /*     STEP 0: Determine location and size of diagonal blocks. */
    /*             IWORK(J) and IWORK(IS+J) are used to indicate the */
    /*             beginning index and the kind of eigenvalues of the */
    /*             J-th diagonal block of the subpencil aA - bB. */
    /*             To find IWORK(IS+J) for the block J of size dim, compute */
    /*                                      -T                      -1 */
    /*                sign( trace(C(rng,rng)  *B(rng,rng)*A(rng,rng)  ) ), */
    /*             where rng = J:J+dim-1. For dim = 2, it is assumed that */
    /*             both eigenvalues of the matrix above have real parts with */
    /*             the same sign (true for a structured Schur form). */
    i__ = 1;
    j = 1;
    is = m + 1;
    /*     Partition blocks. */
    nrmb = dlanhs_("One", &m, &b[b_offset], ldb, &dwork[1], 3L);
    /*     WHILE( I.LE.M-1 ) DO */
L10:
    if (i__ <= m - 1) {
        iwork[j] = i__;
        if ((d__1 = b[i__ + 1 + i__ * b_dim1], abs(d__1)) <= tol * nrmb) {
            /*           1-by-1 block. */
            b[i__ + 1 + i__ * b_dim1] = 0.;
            d__1 = a[i__ + i__ * a_dim1] * b[i__ + i__ * b_dim1] * c__[i__ + i__ * c_dim1];
            iwork[is + j] = (integer)d_sign(&c_b14, &d__1);
            ++i__;
        } else {
            /*           2-by-2 block. */
            u11 = b[i__ + 1 + i__ * b_dim1] * a[i__ + (i__ + 1) * a_dim1];
            u12 = b[i__ + 1 + i__ * b_dim1] * c__[i__ + 1 + i__ * c_dim1];
            tmpa = b[i__ + 1 + (i__ + 1) * b_dim1] * a[i__ + i__ * a_dim1] - u11;
            tmpc = b[i__ + i__ * b_dim1] * c__[i__ + 1 + (i__ + 1) * c_dim1] - u12;
            if (abs(tmpa) <= prec * abs(u11) && abs(tmpc) <= prec * abs(u12)) {
                /*              Severe cancellation. Use the periodic QZ algorithm. */
                /*              Workspace: 30. */
                idum[0] = 1;
                idum[1] = 2;
                idum[2] = 3;
                idum[3] = 1;
                idum[4] = -1;
                idum[5] = -1;
                dlacpy_("Full", &c__2, &c__2, &b[i__ + i__ * b_dim1], ldb, prd, &c__2, 4L);
                dlacpy_("Upper", &c__2, &c__2, &a[i__ + i__ * a_dim1], lda, &prd[4], &c__2, 5L);
                ma02ad_("Lower", &c__2, &c__2, &c__[i__ + i__ * c_dim1], ldc, &prd[8], &c__2, 5L);
                prd[5] = 0.;
                prd[9] = 0.;
                mb03bb_(&base, &lgbas, &prec, &c__3, idum, &idum[3], &c__1, prd, &c__2, &c__2,
                    &dwork[1], &dwork[3], &dwork[5], &idum[6], &dwork[7], info);
                if (*info != 0) {
                    return 0;
                }
                if (dwork[5] == 0. || dwork[6] == 0.) {
                    iwork[is + j] = 0;
                } else {
                    iwork[is + j] = ma01cd_(&dwork[1], &idum[6], &dwork[2], &idum[7]);
                }
            } else if (c__[i__ + i__ * c_dim1] == 0. || a[i__ + 1 + (i__ + 1) * a_dim1] == 0.) {
                /*              The pencil has infinite eigenvalues or it is singular. */
                iwork[is + j] = 0;
            } else {
                u11 = tmpa / a[i__ + 1 + (i__ + 1) * a_dim1] + tmpc / c__[i__ + i__ * c_dim1];
                if (u11 == 0.) {
                    iwork[is + j] = 0;
                } else {
                    d__1 = a[i__ + i__ * a_dim1] * c__[i__ + 1 + (i__ + 1) * c_dim1];
                    iwork[is + j] = (integer)(d_sign(&c_b14, &u11) * d_sign(&c_b14, &d__1));
                }
            }
            i__ += 2;
        }
        ++j;
        goto L10;
        /*     END WHILE 10 */
    }
    if (i__ == m) {
        /*        1-by-1 block */
        iwork[j] = i__;
        d__1 = a[i__ + i__ * a_dim1] * b[i__ + i__ * b_dim1] * c__[i__ + i__ * c_dim1];
        iwork[is + j] = (integer)d_sign(&c_b14, &d__1);
        ++j;
    }
    r__ = j - 1;
    /*     Initialize Q if appropriate. */
    if (liniq) {
        iupd = m + 1;
        upds = m;
        dlaset_("Full", n, n, &c_b35, &c_b14, &q[q_offset], ldq, 4L);
    } else if (lupdq) {
        iupd = 1;
        upds = *n;
    }
    /*     Initialize U1 and U2 if appropriate. */
    if (liniu) {
        dlaset_("Full", &m, &m, &c_b35, &c_b14, &u1[u1_offset], ldu1, 4L);
        dlaset_("Full", &m, &m, &c_b35, &c_b35, &u2[u2_offset], ldu2, 4L);
    }
    if (m > 1) {
        /*        Save the elements A(M,M-1), C(M-1,M), and F(M,M-1), which might */
        /*        be overwritten. */
        a2 = a[m + (m - 1) * a_dim1];
        c2 = c__[m - 1 + m * c_dim1];
        f2 = f[m + (m - 1) * f_dim1];
    }
    /*     STEP 1: Reorder the eigenvalues in the subpencil aA - bB. */
    mm = 0;
    mp = j;
    /*     I. Reorder the eigenvalues with negative real parts to the top. */
    /*     Set pointers for the inputs and outputs of MB03CD. */
    iq1 = 1;
    iq2 = iq1 + 16;
    iq3 = iq2 + 16;
    ia = iq3 + 16;
    ib = ia + 16;
    ic = ib + 16;
    iwrk1 = ic + 16;
    iwrk2 = ia;
    k = 1;
    ib3 = m + 1;
    iwork[r__ + 1] = ib3;
    /*     WHILE( K.LE.R ) DO */
L20:
    if (k <= r__) {
        if (iwork[is + k] < 0) {
            i__1 = mm + 1;
            for (j = k - 1; j >= i__1; --j) {
                /*              IB1, IB2, and IB3 are pointers to 3 consecutive blocks. */
                ib1 = iwork[j];
                ib2 = iwork[j + 1];
                ib3 = iwork[j + 2];
                dim1 = ib2 - ib1;
                dim2 = ib3 - ib2;
                sdim = dim1 + dim2;
                /*              Copy the relevant part of A(ib1:ib3-1,ib1:ib3-1), */
                /*              C(ib1:ib3-1,ib1:ib3-1), and B(ib1:ib3-1,ib1:ib3-1) to */
                /*              DWORK as inputs for MB03CD. Also, set the additional */
                /*              zero elements. */
                dlacpy_("Upper", &sdim, &sdim, &a[ib1 + ib1 * a_dim1], lda, &dwork[ia], &sdim, 5L);
                ma02ad_(
                    "Lower", &sdim, &sdim, &c__[ib1 + ib1 * c_dim1], ldc, &dwork[ic], &sdim, 5L);
                dlacpy_("Upper", &sdim, &sdim, &b[ib1 + ib1 * b_dim1], ldb, &dwork[ib], &sdim, 5L);
                i__2 = sdim - 1;
                i__3 = *ldb + 1;
                i__4 = sdim + 1;
                dcopy_(&i__2, &b[ib1 + 1 + ib1 * b_dim1], &i__3, &dwork[ib + 1], &i__4);
                if (dim1 == 2) {
                    dwork[ia + 1] = 0.;
                    dwork[ic + 1] = 0.;
                }
                if (dim2 == 2) {
                    i1 = sdim * (sdim - 1) - 1;
                    dwork[ia + i1] = 0.;
                    dwork[ic + i1] = 0.;
                }
                dwork[ib + sdim - 1] = 0.;
                if (sdim == 4) {
                    dwork[ib + 2] = 0.;
                    dwork[ib + 7] = 0.;
                }
                /*              Perform eigenvalue/matrix block exchange. */
                /*              Workspace: IWRK1 + 16*DIM1 + 10*DIM2 + 22 <= IWRK1 + 74, */
                /*              if SDIM > 2, and IWRK1 - 1, otherwise. */
                i__2 = *ldwork - iwrk1 + 1;
                mb03cd_("Upper", &dim1, &dim2, &prec, &dwork[ic], &sdim, &dwork[ia], &sdim,
                    &dwork[ib], &sdim, &dwork[iq1], &sdim, &dwork[iq2], &sdim, &dwork[iq3], &sdim,
                    &dwork[iwrk1], &i__2, info, 5L);
                if (*info > 0) {
                    *info = 2;
                    return 0;
                }
                /*              Copy the transformed diagonal block of B, if sdim > 2. */
                if (sdim > 2) {
                    dlacpy_(
                        "Upper", &sdim, &sdim, &dwork[ib], &sdim, &b[ib1 + ib1 * b_dim1], ldb, 5L);
                    i__2 = sdim - 1;
                    i__3 = sdim + 1;
                    i__4 = *ldb + 1;
                    dcopy_(&i__2, &dwork[ib + 1], &i__3, &b[ib1 + 1 + ib1 * b_dim1], &i__4);
                }
                nrows = ib1 - 1;
                ncols = m - ib3 + 1;
                nrow = ib3 - 1;
                ncol = m - ib1 + 1;
                i__2 = sdim - 1;
                i__3 = sdim - 1;
                dlacpy_("Lower", &i__2, &i__3, &a[ib1 + 1 + ib1 * a_dim1], lda, dum, &c__3, 5L);
                i__2 = sdim - 1;
                i__3 = sdim - 1;
                dlaset_("Lower", &i__2, &i__3, &c_b35, &c_b35, &a[ib1 + 1 + ib1 * a_dim1], lda, 5L);
                i__2 = sdim - 1;
                i__3 = sdim - 1;
                dlacpy_(
                    "Upper", &i__2, &i__3, &c__[ib1 + (ib1 + 1) * c_dim1], ldc, &dum[3], &c__3, 5L);
                i__2 = sdim - 1;
                i__3 = sdim - 1;
                dlaset_(
                    "Upper", &i__2, &i__3, &c_b35, &c_b35, &c__[ib1 + (ib1 + 1) * c_dim1], ldc, 5L);
                /*              Update A. */
                /*              Workspace: IWRK2 + 2*N - 1. */
                dgemm_("No Transpose", "No Transpose", &nrow, &sdim, &sdim, &c_b14,
                    &a[ib1 * a_dim1 + 1], lda, &dwork[iq1], &sdim, &c_b35, &dwork[iwrk2], &nrow,
                    12L, 12L);
                dlacpy_("Full", &nrow, &sdim, &dwork[iwrk2], &nrow, &a[ib1 * a_dim1 + 1], lda, 4L);
                dgemm_("Transpose", "No Transpose", &sdim, &ncol, &sdim, &c_b14, &dwork[iq2], &sdim,
                    &a[ib1 + ib1 * a_dim1], lda, &c_b35, &dwork[iwrk2], &sdim, 9L, 12L);
                dlacpy_(
                    "Full", &sdim, &ncol, &dwork[iwrk2], &sdim, &a[ib1 + ib1 * a_dim1], lda, 4L);
                i__2 = sdim - 1;
                i__3 = sdim - 1;
                dlacpy_("Lower", &i__2, &i__3, dum, &c__3, &a[ib1 + 1 + ib1 * a_dim1], lda, 5L);
                /*              Update C. */
                dgemm_("No Transpose", "No Transpose", &ncol, &sdim, &sdim, &c_b14,
                    &c__[ib1 + ib1 * c_dim1], ldc, &dwork[iq3], &sdim, &c_b35, &dwork[iwrk2], &ncol,
                    12L, 12L);
                dlacpy_(
                    "Full", &ncol, &sdim, &dwork[iwrk2], &ncol, &c__[ib1 + ib1 * c_dim1], ldc, 4L);
                dgemm_("Transpose", "No Transpose", &sdim, &nrow, &sdim, &c_b14, &dwork[iq2], &sdim,
                    &c__[ib1 + c_dim1], ldc, &c_b35, &dwork[iwrk2], &sdim, 9L, 12L);
                dlacpy_("Full", &sdim, &nrow, &dwork[iwrk2], &sdim, &c__[ib1 + c_dim1], ldc, 4L);
                i__2 = sdim - 1;
                i__3 = sdim - 1;
                dlacpy_(
                    "Upper", &i__2, &i__3, &dum[3], &c__3, &c__[ib1 + (ib1 + 1) * c_dim1], ldc, 5L);
                /*              Update D. */
                dgemm_("No Transpose", "No Transpose", &m, &sdim, &sdim, &c_b14,
                    &d__[ib1 * d_dim1 + 1], ldd, &dwork[iq3], &sdim, &c_b35, &dwork[iwrk2], &m, 12L,
                    12L);
                dlacpy_("Full", &m, &sdim, &dwork[iwrk2], &m, &d__[ib1 * d_dim1 + 1], ldd, 4L);
                dgemm_("Transpose", "No Transpose", &sdim, &m, &sdim, &c_b14, &dwork[iq2], &sdim,
                    &d__[ib1 + d_dim1], ldd, &c_b35, &dwork[iwrk2], &sdim, 9L, 12L);
                dlacpy_("Full", &sdim, &m, &dwork[iwrk2], &sdim, &d__[ib1 + d_dim1], ldd, 4L);
                /*              Update B. */
                if (sdim > 2) {
                    nrow = nrows;
                    ncol = ncols;
                    ibs = ib3;
                    ldw = max(1, nrow);
                } else {
                    ibs = ib1;
                    ldw = nrow;
                }
                dgemm_("No Transpose", "No Transpose", &nrow, &sdim, &sdim, &c_b14,
                    &b[ib1 * b_dim1 + 1], ldb, &dwork[iq1], &sdim, &c_b35, &dwork[iwrk2], &ldw, 12L,
                    12L);
                dlacpy_("Full", &nrow, &sdim, &dwork[iwrk2], &ldw, &b[ib1 * b_dim1 + 1], ldb, 4L);
                dgemm_("Transpose", "No Transpose", &sdim, &ncol, &sdim, &c_b14, &dwork[iq3], &sdim,
                    &b[ib1 + ibs * b_dim1], ldb, &c_b35, &dwork[iwrk2], &sdim, 9L, 12L);
                dlacpy_(
                    "Full", &sdim, &ncol, &dwork[iwrk2], &sdim, &b[ib1 + ibs * b_dim1], ldb, 4L);
                /*              Update F. */
                dgemm_("No Transpose", "No Transpose", &nrows, &sdim, &sdim, &c_b14,
                    &f[ib1 * f_dim1 + 1], ldf, &dwork[iq3], &sdim, &c_b35, &dwork[iwrk2], &ldw, 12L,
                    12L);
                dlacpy_("Full", &nrows, &sdim, &dwork[iwrk2], &ldw, &f[ib1 * f_dim1 + 1], ldf, 4L);
                dgemm_("Transpose", "No Transpose", &sdim, &ncols, &sdim, &c_b14, &dwork[iq3],
                    &sdim, &f[ib1 + ib3 * f_dim1], ldf, &c_b35, &dwork[iwrk2], &sdim, 9L, 12L);
                dlacpy_(
                    "Full", &sdim, &ncols, &dwork[iwrk2], &sdim, &f[ib1 + ib3 * f_dim1], ldf, 4L);
                i__2 = *ldwork - iwrk2 + 1;
                mb01ru_("Upper", "Transpose", &sdim, &sdim, &c_b35, &c_b14, &f[ib1 + ib1 * f_dim1],
                    ldf, &dwork[iq3], &sdim, &f[ib1 + ib1 * f_dim1], ldf, &dwork[iwrk2], &i__2,
                    info, 5L, 9L);
                i__2 = *ldf + 1;
                dscal_(&sdim, &c_b118, &f[ib1 + ib1 * f_dim1], &i__2);
                if (lcmpq) {
                    /*                 Update Q. */
                    /*                 Workspace: IWRK2 + 2*N - 1, if COMPQ = 'I'; */
                    /*                            IWRK2 + 4*N - 1, if COMPQ = 'U'. */
                    dgemm_("No Transpose", "No Transpose", &upds, &sdim, &sdim, &c_b14,
                        &q[ib1 * q_dim1 + 1], ldq, &dwork[iq1], &sdim, &c_b35, &dwork[iwrk2], &upds,
                        12L, 12L);
                    dlacpy_(
                        "Full", &upds, &sdim, &dwork[iwrk2], &upds, &q[ib1 * q_dim1 + 1], ldq, 4L);
                    dgemm_("No Transpose", "No Transpose", &upds, &sdim, &sdim, &c_b14,
                        &q[iupd + (m + ib1) * q_dim1], ldq, &dwork[iq3], &sdim, &c_b35,
                        &dwork[iwrk2], &upds, 12L, 12L);
                    dlacpy_("Full", &upds, &sdim, &dwork[iwrk2], &upds,
                        &q[iupd + (m + ib1) * q_dim1], ldq, 4L);
                }
                if (lcmpu) {
                    /*                 Update U1. */
                    dgemm_("No Transpose", "No Transpose", &m, &sdim, &sdim, &c_b14,
                        &u1[ib1 * u1_dim1 + 1], ldu1, &dwork[iq2], &sdim, &c_b35, &dwork[iwrk2], &m,
                        12L, 12L);
                    dlacpy_("Full", &m, &sdim, &dwork[iwrk2], &m, &u1[ib1 * u1_dim1 + 1], ldu1, 4L);
                }
                if (lupdu) {
                    /*                 Update U2. */
                    dgemm_("No Transpose", "No Transpose", &m, &sdim, &sdim, &c_b14,
                        &u2[ib1 * u2_dim1 + 1], ldu2, &dwork[iq2], &sdim, &c_b35, &dwork[iwrk2], &m,
                        12L, 12L);
                    dlacpy_("Full", &m, &sdim, &dwork[iwrk2], &m, &u2[ib1 * u2_dim1 + 1], ldu2, 4L);
                }
                /*              Update index lists IWORK(1:M) and IWORK(M+2:N+1) if a */
                /*              1-by-1 and 2-by-2 block have been swapped. */
                hlp = dim2 - dim1;
                if (hlp == 1) {
                    /*                 First block was 2-by-2. */
                    iwork[j + 1] = ib1 + 1;
                } else if (hlp == -1) {
                    /*                 Second block was 2-by-2. */
                    iwork[j + 1] = ib1 + 2;
                }
                /*              Update IWORK(M+2:N+1). */
                hlp = iwork[is + j];
                iwork[is + j] = iwork[is + j + 1];
                iwork[is + j + 1] = hlp;
                /* L30: */
            }
            ++mm;
        }
        ++k;
        goto L20;
        /*     END WHILE 20 */
    }
    /*     II. Reorder the eigenvalues with positive real parts to the bottom. */
    k = r__;
    /*     WHILE( K.GE.MM+1 ) DO */
L40:
    if (k >= mm + 1) {
        if (iwork[is + k] > 0) {
            i__1 = mp - 2;
            for (j = k; j <= i__1; ++j) {
                ib1 = iwork[j];
                ib2 = iwork[j + 1];
                ib3 = iwork[j + 2];
                dim1 = ib2 - ib1;
                dim2 = ib3 - ib2;
                sdim = dim1 + dim2;
                /*              Copy the relevant part of A(ib1:ib3-1,ib1:ib3-1), */
                /*              C(ib1:ib3-1,ib1:ib3-1), and B(ib1:ib3-1,ib1:ib3-1) to */
                /*              DWORK as inputs for MB03CD. Also, set the additional */
                /*              zero elements. */
                dlacpy_("Upper", &sdim, &sdim, &a[ib1 + ib1 * a_dim1], lda, &dwork[ia], &sdim, 5L);
                ma02ad_(
                    "Lower", &sdim, &sdim, &c__[ib1 + ib1 * c_dim1], ldc, &dwork[ic], &sdim, 5L);
                dlacpy_("Upper", &sdim, &sdim, &b[ib1 + ib1 * b_dim1], ldb, &dwork[ib], &sdim, 5L);
                i__2 = sdim - 1;
                i__3 = *ldb + 1;
                i__4 = sdim + 1;
                dcopy_(&i__2, &b[ib1 + 1 + ib1 * b_dim1], &i__3, &dwork[ib + 1], &i__4);
                if (dim1 == 2) {
                    dwork[ia + 1] = 0.;
                    dwork[ic + 1] = 0.;
                }
                if (dim2 == 2) {
                    i1 = sdim * (sdim - 1) - 1;
                    dwork[ia + i1] = 0.;
                    dwork[ic + i1] = 0.;
                }
                dwork[ib + sdim - 1] = 0.;
                if (sdim == 4) {
                    dwork[ib + 2] = 0.;
                    dwork[ib + 7] = 0.;
                }
                /*              Perform eigenvalue/matrix block exchange. */
                i__2 = *ldwork - iwrk1 + 1;
                mb03cd_("Upper", &dim1, &dim2, &prec, &dwork[ic], &sdim, &dwork[ia], &sdim,
                    &dwork[ib], &sdim, &dwork[iq1], &sdim, &dwork[iq2], &sdim, &dwork[iq3], &sdim,
                    &dwork[iwrk1], &i__2, info, 5L);
                if (*info > 0) {
                    *info = 2;
                    return 0;
                }
                /*              Copy the transformed diagonal block of B, if sdim > 2. */
                if (sdim > 2) {
                    dlacpy_(
                        "Upper", &sdim, &sdim, &dwork[ib], &sdim, &b[ib1 + ib1 * b_dim1], ldb, 5L);
                    i__2 = sdim - 1;
                    i__3 = sdim + 1;
                    i__4 = *ldb + 1;
                    dcopy_(&i__2, &dwork[ib + 1], &i__3, &b[ib1 + 1 + ib1 * b_dim1], &i__4);
                }
                nrows = ib1 - 1;
                ncols = m - ib3 + 1;
                nrow = ib3 - 1;
                ncol = m - ib1 + 1;
                i__2 = sdim - 1;
                i__3 = sdim - 1;
                dlacpy_("Lower", &i__2, &i__3, &a[ib1 + 1 + ib1 * a_dim1], lda, dum, &c__3, 5L);
                i__2 = sdim - 1;
                i__3 = sdim - 1;
                dlaset_("Lower", &i__2, &i__3, &c_b35, &c_b35, &a[ib1 + 1 + ib1 * a_dim1], lda, 5L);
                i__2 = sdim - 1;
                i__3 = sdim - 1;
                dlacpy_(
                    "Upper", &i__2, &i__3, &c__[ib1 + (ib1 + 1) * c_dim1], ldc, &dum[3], &c__3, 5L);
                i__2 = sdim - 1;
                i__3 = sdim - 1;
                dlaset_(
                    "Upper", &i__2, &i__3, &c_b35, &c_b35, &c__[ib1 + (ib1 + 1) * c_dim1], ldc, 5L);
                /*              Update A. */
                dgemm_("No Transpose", "No Transpose", &nrow, &sdim, &sdim, &c_b14,
                    &a[ib1 * a_dim1 + 1], lda, &dwork[iq1], &sdim, &c_b35, &dwork[iwrk2], &nrow,
                    12L, 12L);
                dlacpy_("Full", &nrow, &sdim, &dwork[iwrk2], &nrow, &a[ib1 * a_dim1 + 1], lda, 4L);
                dgemm_("Transpose", "No Transpose", &sdim, &ncol, &sdim, &c_b14, &dwork[iq2], &sdim,
                    &a[ib1 + ib1 * a_dim1], lda, &c_b35, &dwork[iwrk2], &sdim, 9L, 12L);
                dlacpy_(
                    "Full", &sdim, &ncol, &dwork[iwrk2], &sdim, &a[ib1 + ib1 * a_dim1], lda, 4L);
                i__2 = sdim - 1;
                i__3 = sdim - 1;
                dlacpy_("Lower", &i__2, &i__3, dum, &c__3, &a[ib1 + 1 + ib1 * a_dim1], lda, 5L);
                /*              Update C. */
                dgemm_("No Transpose", "No Transpose", &ncol, &sdim, &sdim, &c_b14,
                    &c__[ib1 + ib1 * c_dim1], ldc, &dwork[iq3], &sdim, &c_b35, &dwork[iwrk2], &ncol,
                    12L, 12L);
                dlacpy_(
                    "Full", &ncol, &sdim, &dwork[iwrk2], &ncol, &c__[ib1 + ib1 * c_dim1], ldc, 4L);
                dgemm_("Transpose", "No Transpose", &sdim, &nrow, &sdim, &c_b14, &dwork[iq2], &sdim,
                    &c__[ib1 + c_dim1], ldc, &c_b35, &dwork[iwrk2], &sdim, 9L, 12L);
                dlacpy_("Full", &sdim, &nrow, &dwork[iwrk2], &sdim, &c__[ib1 + c_dim1], ldc, 4L);
                i__2 = sdim - 1;
                i__3 = sdim - 1;
                dlacpy_(
                    "Upper", &i__2, &i__3, &dum[3], &c__3, &c__[ib1 + (ib1 + 1) * c_dim1], ldc, 5L);
                /*              Update D. */
                dgemm_("No Transpose", "No Transpose", &m, &sdim, &sdim, &c_b14,
                    &d__[ib1 * d_dim1 + 1], ldd, &dwork[iq3], &sdim, &c_b35, &dwork[iwrk2], &m, 12L,
                    12L);
                dlacpy_("Full", &m, &sdim, &dwork[iwrk2], &m, &d__[ib1 * d_dim1 + 1], ldd, 4L);
                dgemm_("Transpose", "No Transpose", &sdim, &m, &sdim, &c_b14, &dwork[iq2], &sdim,
                    &d__[ib1 + d_dim1], ldd, &c_b35, &dwork[iwrk2], &sdim, 9L, 12L);
                dlacpy_("Full", &sdim, &m, &dwork[iwrk2], &sdim, &d__[ib1 + d_dim1], ldd, 4L);
                /*              Update B. */
                if (sdim > 2) {
                    nrow = nrows;
                    ncol = ncols;
                    ibs = ib3;
                    ldw = max(1, nrow);
                } else {
                    ibs = ib1;
                    ldw = nrow;
                }
                dgemm_("No Transpose", "No Transpose", &nrow, &sdim, &sdim, &c_b14,
                    &b[ib1 * b_dim1 + 1], ldb, &dwork[iq1], &sdim, &c_b35, &dwork[iwrk2], &ldw, 12L,
                    12L);
                dlacpy_("Full", &nrow, &sdim, &dwork[iwrk2], &ldw, &b[ib1 * b_dim1 + 1], ldb, 4L);
                dgemm_("Transpose", "No Transpose", &sdim, &ncol, &sdim, &c_b14, &dwork[iq3], &sdim,
                    &b[ib1 + ibs * b_dim1], ldb, &c_b35, &dwork[iwrk2], &sdim, 9L, 12L);
                dlacpy_(
                    "Full", &sdim, &ncol, &dwork[iwrk2], &sdim, &b[ib1 + ibs * b_dim1], ldb, 4L);
                /*              Update F. */
                dgemm_("No Transpose", "No Transpose", &nrows, &sdim, &sdim, &c_b14,
                    &f[ib1 * f_dim1 + 1], ldf, &dwork[iq3], &sdim, &c_b35, &dwork[iwrk2], &ldw, 12L,
                    12L);
                dlacpy_("Full", &nrows, &sdim, &dwork[iwrk2], &ldw, &f[ib1 * f_dim1 + 1], ldf, 4L);
                dgemm_("Transpose", "No Transpose", &sdim, &ncols, &sdim, &c_b14, &dwork[iq3],
                    &sdim, &f[ib1 + ib3 * f_dim1], ldf, &c_b35, &dwork[iwrk2], &sdim, 9L, 12L);
                dlacpy_(
                    "Full", &sdim, &ncols, &dwork[iwrk2], &sdim, &f[ib1 + ib3 * f_dim1], ldf, 4L);
                i__2 = *ldwork - iwrk2 + 1;
                mb01ru_("Upper", "Transpose", &sdim, &sdim, &c_b35, &c_b14, &f[ib1 + ib1 * f_dim1],
                    ldf, &dwork[iq3], &sdim, &f[ib1 + ib1 * f_dim1], ldf, &dwork[iwrk2], &i__2,
                    info, 5L, 9L);
                i__2 = *ldf + 1;
                dscal_(&sdim, &c_b118, &f[ib1 + ib1 * f_dim1], &i__2);
                if (lcmpq) {
                    /*                 Update Q. */
                    dgemm_("No Transpose", "No Transpose", &upds, &sdim, &sdim, &c_b14,
                        &q[ib1 * q_dim1 + 1], ldq, &dwork[iq1], &sdim, &c_b35, &dwork[iwrk2], &upds,
                        12L, 12L);
                    dlacpy_(
                        "Full", &upds, &sdim, &dwork[iwrk2], &upds, &q[ib1 * q_dim1 + 1], ldq, 4L);
                    dgemm_("No Transpose", "No Transpose", &upds, &sdim, &sdim, &c_b14,
                        &q[iupd + (m + ib1) * q_dim1], ldq, &dwork[iq3], &sdim, &c_b35,
                        &dwork[iwrk2], &upds, 12L, 12L);
                    dlacpy_("Full", &upds, &sdim, &dwork[iwrk2], &upds,
                        &q[iupd + (m + ib1) * q_dim1], ldq, 4L);
                }
                if (lcmpu) {
                    /*                 Update U1. */
                    dgemm_("No Transpose", "No Transpose", &m, &sdim, &sdim, &c_b14,
                        &u1[ib1 * u1_dim1 + 1], ldu1, &dwork[iq2], &sdim, &c_b35, &dwork[iwrk2], &m,
                        12L, 12L);
                    dlacpy_("Full", &m, &sdim, &dwork[iwrk2], &m, &u1[ib1 * u1_dim1 + 1], ldu1, 4L);
                }
                if (lupdu) {
                    /*                 Update U2. */
                    dgemm_("No Transpose", "No Transpose", &m, &sdim, &sdim, &c_b14,
                        &u2[ib1 * u2_dim1 + 1], ldu2, &dwork[iq2], &sdim, &c_b35, &dwork[iwrk2], &m,
                        12L, 12L);
                    dlacpy_("Full", &m, &sdim, &dwork[iwrk2], &m, &u2[ib1 * u2_dim1 + 1], ldu2, 4L);
                }
                /*              Update index list IWORK(1:M) if a 1-by-1 and 2-by-2 block */
                /*              have been swapped. IWORK(M+2:N+1) is not needed anymore, */
                /*              so it is not necessary to update it. */
                hlp = dim2 - dim1;
                if (hlp == 1) {
                    /*                 First block was 2-by-2. */
                    iwork[j + 1] = ib1 + 1;
                } else if (hlp == -1) {
                    /*                 Second block was 2-by-2. */
                    iwork[j + 1] = ib1 + 2;
                }
                /* L50: */
            }
            --mp;
        }
        --k;
        goto L40;
        /*     END WHILE 40 */
    }
    /*     STEP 2: Reorder the remaining eigenvalues with negative real parts. */
    /*     Set pointers for the inputs and outputs of MB03GD. */
    iquple = 1;
    iuuple = iquple + 16;
    izuple = iuuple + 16;
    ihuple = izuple + 16;
    iwrk5 = ihuple + 16;
    iwrk3 = izuple;
    iwrk4 = iwrk3 + (*n << 1);
    itmp1 = iwrk3 + *n;
    itmp2 = itmp1 + 4;
    itmp3 = itmp2 + 4;
    i__1 = mp;
    for (k = r__; k >= i__1; --k) {
        /*        I. Exchange the eigenvalues between two diagonal blocks. */
        ir = iwork[r__];
        dim1 = iwork[r__ + 1] - ir;
        sdim = dim1 << 1;
        if (dim1 == 2) {
            a[m + ir * a_dim1] = 0.;
            c__[ir + m * c_dim1] = 0.;
            /*           Build the (small) symmetric matrix F(M-1:M,M-1:M). */
            f[m + ir * f_dim1] = f[ir + m * f_dim1];
        }
        /*        Calculate position of submatrices in DWORK. */
        izupri = izuple + dim1 * sdim;
        izlori = izupri + dim1;
        iuupri = iuuple + dim1 * sdim;
        iqlole = iquple + dim1;
        iqupri = iquple + dim1 * sdim;
        iqlori = iqupri + dim1;
        /*        Generate input matrices for MB03GD built of submatrices of A, */
        /*        D, C, B, and F. */
        dlacpy_("Upper", &dim1, &dim1, &a[ir + ir * a_dim1], lda, &dwork[izuple], &sdim, 5L);
        dlacpy_("Full", &dim1, &dim1, &d__[ir + ir * d_dim1], ldd, &dwork[izupri], &sdim, 4L);
        dlacpy_("Lower", &dim1, &dim1, &c__[ir + ir * c_dim1], ldc, &dwork[izlori], &sdim, 5L);
        dlacpy_("Full", &dim1, &dim1, &b[ir + ir * b_dim1], ldb, &dwork[ihuple], &sdim, 4L);
        dlacpy_("Upper", &dim1, &dim1, &f[ir + ir * f_dim1], ldb, &dwork[ihuple + dim1 * sdim],
            &sdim, 5L);
        if (dim1 == 2) {
            dwork[izuple + 1] = 0.;
            dwork[izlori + sdim] = 0.;
        }
        /*        Perform eigenvalue exchange. */
        /*        Workspace: IWRK5 + 11, if SDIM = 4. */
        i__2 = *ldwork - iwrk5 + 1;
        mb03gd_(&sdim, &dwork[izuple], &sdim, &dwork[ihuple], &sdim, par, &dwork[iquple], &sdim,
            &dwork[iuuple], &sdim, &dwork[iwrk5], &i__2, info);
        if (*info > 0) {
            *info = 3;
            return 0;
        }
        if (dim1 == 2) {
            /*           Update A by transformations from the right. */
            /*           Workspace: IWRK3 + N - 1. */
            dlacpy_("Full", &m, &dim1, &a[ir * a_dim1 + 1], lda, &dwork[iwrk3], &m, 4L);
            dgemm_("No Transpose", "No Transpose", &m, &dim1, &dim1, &c_b14, &dwork[iwrk3], &m,
                &dwork[iquple], &sdim, &c_b35, &a[ir * a_dim1 + 1], lda, 12L, 12L);
            dgemm_("No Transpose", "No Transpose", &m, &dim1, &dim1, &c_b14, &d__[ir * d_dim1 + 1],
                ldd, &dwork[iqlole], &sdim, &c_b14, &a[ir * a_dim1 + 1], lda, 12L, 12L);
            /*           Update D by transformations from the right. */
            dgemm_("No Transpose", "No Transpose", &m, &dim1, &dim1, &c_b14, &dwork[iwrk3], &m,
                &dwork[iqupri], &sdim, &c_b35, &dwork[itmp1], &m, 12L, 12L);
            dgemm_("No Transpose", "No Transpose", &m, &dim1, &dim1, &c_b14, &d__[ir * d_dim1 + 1],
                ldd, &dwork[iqlori], &sdim, &c_b14, &dwork[itmp1], &m, 12L, 12L);
            dlacpy_("Full", &m, &dim1, &dwork[itmp1], &m, &d__[ir * d_dim1 + 1], ldd, 4L);
            /*           Compute intermediate product Cf*Q21, with */
            /*           Cf = C(M-1:M,M-1:M). */
            dgemm_("No Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b14,
                &c__[ir + ir * c_dim1], ldc, &dwork[iqlole], &sdim, &c_b35, &dwork[itmp1], &dim1,
                12L, 12L);
            /*           Update C by transformations from the right. */
            dgemm_("No Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b14,
                &c__[ir + ir * c_dim1], ldc, &dwork[iqlori], &sdim, &c_b35, &dwork[iwrk3], &dim1,
                12L, 12L);
            dlacpy_("Full", &dim1, &dim1, &dwork[iwrk3], &dim1, &c__[ir + ir * c_dim1], ldc, 4L);
            /*           Update A by transformations from the left. */
            dgemm_("Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b14, &dwork[iuuple], &sdim,
                &a[ir + ir * a_dim1], lda, &c_b35, &dwork[iwrk3], &dim1, 9L, 12L);
            dgemm_("Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b274, &dwork[iuupri], &sdim,
                &dwork[itmp1], &dim1, &c_b14, &dwork[iwrk3], &dim1, 9L, 12L);
            dlacpy_("Full", &dim1, &dim1, &dwork[iwrk3], &dim1, &a[ir + ir * a_dim1], lda, 4L);
            /*           Update D by transformations from the left. */
            dlacpy_("Full", &dim1, &m, &d__[ir + d_dim1], ldd, &dwork[iwrk3], &dim1, 4L);
            dgemm_("Transpose", "No Transpose", &dim1, &m, &dim1, &c_b14, &dwork[iuuple], &sdim,
                &dwork[iwrk3], &dim1, &c_b35, &d__[ir + d_dim1], ldd, 9L, 12L);
            dgemm_("Transpose", "No Transpose", &dim1, &m, &dim1, &c_b274, &dwork[iuupri], &sdim,
                &c__[ir + c_dim1], ldc, &c_b14, &d__[ir + d_dim1], ldd, 9L, 12L);
            /*           Update C by transformations from the left. */
            dgemm_("Transpose", "No Transpose", &dim1, &m, &dim1, &c_b14, &dwork[iuupri], &sdim,
                &dwork[iwrk3], &dim1, &c_b35, &dwork[itmp1], &dim1, 9L, 12L);
            dgemm_("Transpose", "No Transpose", &dim1, &m, &dim1, &c_b14, &dwork[iuuple], &sdim,
                &c__[ir + c_dim1], ldc, &c_b14, &dwork[itmp1], &dim1, 9L, 12L);
            dlacpy_("Full", &dim1, &m, &dwork[itmp1], &dim1, &c__[ir + c_dim1], ldc, 4L);
            /*           Update B by transformations from the right. */
            dlacpy_("Full", &m, &dim1, &b[ir * b_dim1 + 1], ldb, &dwork[iwrk3], &m, 4L);
            dgemm_("No Transpose", "No Transpose", &m, &dim1, &dim1, &c_b14, &dwork[iwrk3], &m,
                &dwork[iquple], &sdim, &c_b35, &b[ir * b_dim1 + 1], ldb, 12L, 12L);
            dgemm_("No Transpose", "No Transpose", &m, &dim1, &dim1, &c_b14, &f[ir * f_dim1 + 1],
                ldf, &dwork[iqlole], &sdim, &c_b14, &b[ir * b_dim1 + 1], ldb, 12L, 12L);
            /*           Update F by transformations from the right. */
            dgemm_("No Transpose", "No Transpose", &m, &dim1, &dim1, &c_b14, &dwork[iwrk3], &m,
                &dwork[iqupri], &sdim, &c_b35, &dwork[itmp1], &m, 12L, 12L);
            dgemm_("No Transpose", "No Transpose", &m, &dim1, &dim1, &c_b14, &f[ir * f_dim1 + 1],
                ldf, &dwork[iqlori], &sdim, &c_b14, &dwork[itmp1], &m, 12L, 12L);
            dlacpy_("Full", &m, &dim1, &dwork[itmp1], &m, &f[ir * f_dim1 + 1], ldf, 4L);
            /*           Compute intermediate products Bf'*Q21 and Bf'*Q22, with */
            /*           Bf = B(M-1:M,M-1:M). */
            dgemm_("Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b14,
                &dwork[iwrk3 + m - dim1], &m, &dwork[iqlole], &sdim, &c_b35, &dwork[itmp1], &dim1,
                9L, 12L);
            dgemm_("Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b14,
                &dwork[iwrk3 + m - dim1], &m, &dwork[iqlori], &sdim, &c_b35, &dwork[itmp2], &dim1,
                9L, 12L);
            /*           Update B by transformations from the left. */
            dlacpy_("Full", &dim1, &dim1, &b[ir + ir * b_dim1], ldb, &dwork[itmp3], &dim1, 4L);
            dgemm_("Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b14, &dwork[iqupri], &sdim,
                &dwork[itmp1], &dim1, &c_b35, &b[ir + ir * b_dim1], ldb, 9L, 12L);
            dgemm_("Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b14, &dwork[iqlori], &sdim,
                &dwork[itmp3], &dim1, &c_b14, &b[ir + ir * b_dim1], ldb, 9L, 12L);
            /*           Update F by transformations from the left. */
            mb01rx_("Left", "Upper", "Transpose", &dim1, &dim1, &c_b35, &c_b14, &dwork[itmp1],
                &dim1, &dwork[iqlori], &sdim, &f[ir + ir * f_dim1], ldf, info, 4L, 5L, 9L);
            mb01rx_("Left", "Upper", "Transpose", &dim1, &dim1, &c_b14, &c_b14, &dwork[itmp1],
                &dim1, &dwork[iqupri], &sdim, &dwork[itmp2], &dim1, info, 4L, 5L, 9L);
            dlacpy_("Upper", &dim1, &dim1, &dwork[itmp1], &dim1, &f[ir + ir * f_dim1], ldf, 5L);
            if (lcmpq) {
                /*              Update Q. */
                /*              Workspace: IWRK4 + 2*N - 1. */
                dlacpy_("Full", n, &dim1, &q[ir * q_dim1 + 1], ldq, &dwork[iwrk4], n, 4L);
                dgemm_("No Transpose", "No Transpose", n, &dim1, &dim1, &c_b14, &dwork[iwrk4], n,
                    &dwork[iquple], &sdim, &c_b35, &q[ir * q_dim1 + 1], ldq, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", n, &dim1, &dim1, &c_b14,
                    &q[(m + ir) * q_dim1 + 1], ldq, &dwork[iqlole], &sdim, &c_b14,
                    &q[ir * q_dim1 + 1], ldq, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", n, &dim1, &dim1, &c_b14, &dwork[iwrk4], n,
                    &dwork[iqupri], &sdim, &c_b35, &dwork[iwrk3], n, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", n, &dim1, &dim1, &c_b14,
                    &q[(m + ir) * q_dim1 + 1], ldq, &dwork[iqlori], &sdim, &c_b14, &dwork[iwrk3], n,
                    12L, 12L);
                dlacpy_("Full", n, &dim1, &dwork[iwrk3], n, &q[(m + ir) * q_dim1 + 1], ldq, 4L);
            }
            if (lcmpu) {
                /*              Update U. */
                /*              Workspace: ITMP1 + N - 1. */
                dlacpy_("Full", &m, &dim1, &u1[ir * u1_dim1 + 1], ldu1, &dwork[itmp1], &m, 4L);
                dgemm_("No Transpose", "No Transpose", &m, &dim1, &dim1, &c_b14, &dwork[itmp1], &m,
                    &dwork[iuuple], &sdim, &c_b35, &u1[ir * u1_dim1 + 1], ldu1, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", &m, &dim1, &dim1, &c_b274,
                    &u2[ir * u2_dim1 + 1], ldu2, &dwork[iuupri], &sdim, &c_b14,
                    &u1[ir * u1_dim1 + 1], ldu1, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", &m, &dim1, &dim1, &c_b14, &dwork[itmp1], &m,
                    &dwork[iuupri], &sdim, &c_b35, &dwork[iwrk3], &m, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", &m, &dim1, &dim1, &c_b14,
                    &u2[ir * u2_dim1 + 1], ldu2, &dwork[iuuple], &sdim, &c_b14, &dwork[iwrk3], &m,
                    12L, 12L);
                dlacpy_("Full", &m, &dim1, &dwork[iwrk3], &m, &u2[ir * u2_dim1 + 1], ldu2, 4L);
            }
        } else {
            u11 = dwork[iuuple];
            u12 = dwork[iuupri];
            q11 = dwork[iquple];
            q21 = dwork[iqlole];
            q12 = dwork[iqupri];
            q22 = dwork[iqlori];
            /*           Update A by transformations from the right. */
            dcopy_(&m, &a[ir * a_dim1 + 1], &c__1, &dwork[iwrk3], &c__1);
            dscal_(&m, &q11, &a[ir * a_dim1 + 1], &c__1);
            daxpy_(&m, &q21, &d__[ir * d_dim1 + 1], &c__1, &a[ir * a_dim1 + 1], &c__1);
            /*           Update D by transformations from the right. */
            dscal_(&m, &q22, &d__[ir * d_dim1 + 1], &c__1);
            daxpy_(&m, &q12, &dwork[iwrk3], &c__1, &d__[ir * d_dim1 + 1], &c__1);
            /*           Compute intermediate product C(M,M)*Q21. */
            tmpc = c__[ir + ir * c_dim1] * q21;
            /*           Update C by transformations from the right. */
            c__[ir + ir * c_dim1] *= q22;
            /*           Update A by transformations from the left. */
            a[ir + ir * a_dim1] = u11 * a[ir + ir * a_dim1] - u12 * tmpc;
            /*           Update D by transformations from the left. */
            dcopy_(&m, &d__[ir + d_dim1], ldd, &dwork[iwrk3], &c__1);
            dscal_(&m, &u11, &d__[ir + d_dim1], ldd);
            d__1 = -u12;
            daxpy_(&m, &d__1, &c__[ir + c_dim1], ldc, &d__[ir + d_dim1], ldd);
            /*           Update C by transformations from the left. */
            dscal_(&m, &u11, &c__[ir + c_dim1], ldc);
            daxpy_(&m, &u12, &dwork[iwrk3], &c__1, &c__[ir + c_dim1], ldc);
            /*           Update B by transformations from the right. */
            i__2 = m - 1;
            dcopy_(&i__2, &b[ir * b_dim1 + 1], &c__1, &dwork[iwrk3], &c__1);
            i__2 = m - 1;
            dscal_(&i__2, &q11, &b[ir * b_dim1 + 1], &c__1);
            i__2 = m - 1;
            daxpy_(&i__2, &q21, &f[ir * f_dim1 + 1], &c__1, &b[ir * b_dim1 + 1], &c__1);
            /*           Update F by transformations from the right. */
            i__2 = m - 1;
            dscal_(&i__2, &q22, &f[ir * f_dim1 + 1], &c__1);
            i__2 = m - 1;
            daxpy_(&i__2, &q12, &dwork[iwrk3], &c__1, &f[ir * f_dim1 + 1], &c__1);
            /*           Update B by transformations from the left. */
            b[m + m * b_dim1] = -b[m + m * b_dim1];
            if (lcmpq) {
                /*              Update Q. */
                dcopy_(n, &q[ir * q_dim1 + 1], &c__1, &dwork[iwrk4], &c__1);
                dscal_(n, &q11, &q[ir * q_dim1 + 1], &c__1);
                daxpy_(n, &q21, &q[(ir + m) * q_dim1 + 1], &c__1, &q[ir * q_dim1 + 1], &c__1);
                dscal_(n, &q22, &q[(ir + m) * q_dim1 + 1], &c__1);
                daxpy_(n, &q12, &dwork[iwrk4], &c__1, &q[(ir + m) * q_dim1 + 1], &c__1);
            }
            if (lcmpu) {
                /*              Update U. */
                dcopy_(&m, &u1[ir * u1_dim1 + 1], &c__1, &dwork[iwrk4], &c__1);
                dscal_(&m, &u11, &u1[ir * u1_dim1 + 1], &c__1);
                d__1 = -u12;
                daxpy_(&m, &d__1, &u2[ir * u2_dim1 + 1], &c__1, &u1[ir * u1_dim1 + 1], &c__1);
                dscal_(&m, &u11, &u2[ir * u2_dim1 + 1], &c__1);
                daxpy_(&m, &u12, &dwork[iwrk4], &c__1, &u2[ir * u2_dim1 + 1], &c__1);
            }
        }
        ++mm;
        i__2 = mm;
        for (j = r__ - 1; j >= i__2; --j) {
            ib1 = iwork[j];
            ib2 = iwork[j + 1];
            ib3 = iwork[j + 2];
            dim1 = ib2 - ib1;
            dim2 = ib3 - ib2;
            sdim = dim1 + dim2;
            /*           Copy the relevant part of A(ib1:ib3-1,ib1:ib3-1), */
            /*           C(ib1:ib3-1,ib1:ib3-1), and B(ib1:ib3-1,ib1:ib3-1) to */
            /*           DWORK as inputs for MB03CD. Also, set the additional */
            /*           zero elements. */
            dlacpy_("Upper", &sdim, &sdim, &a[ib1 + ib1 * a_dim1], lda, &dwork[ia], &sdim, 5L);
            ma02ad_("Lower", &sdim, &sdim, &c__[ib1 + ib1 * c_dim1], ldc, &dwork[ic], &sdim, 5L);
            dlacpy_("Upper", &sdim, &sdim, &b[ib1 + ib1 * b_dim1], ldb, &dwork[ib], &sdim, 5L);
            i__3 = sdim - 1;
            i__4 = *ldb + 1;
            i__5 = sdim + 1;
            dcopy_(&i__3, &b[ib1 + 1 + ib1 * b_dim1], &i__4, &dwork[ib + 1], &i__5);
            if (dim1 == 2) {
                dwork[ia + 1] = 0.;
                dwork[ic + 1] = 0.;
            }
            if (dim2 == 2) {
                i1 = sdim * (sdim - 1) - 1;
                dwork[ia + i1] = 0.;
                dwork[ic + i1] = 0.;
            }
            dwork[ib + sdim - 1] = 0.;
            if (sdim == 4) {
                dwork[ib + 2] = 0.;
                dwork[ib + 7] = 0.;
            }
            /*           Perform eigenvalue/matrix block exchange. */
            i__3 = *ldwork - iwrk1 + 1;
            mb03cd_("Upper", &dim1, &dim2, &prec, &dwork[ic], &sdim, &dwork[ia], &sdim, &dwork[ib],
                &sdim, &dwork[iq1], &sdim, &dwork[iq2], &sdim, &dwork[iq3], &sdim, &dwork[iwrk1],
                &i__3, info, 5L);
            if (*info > 0) {
                *info = 2;
                return 0;
            }
            /*           Copy the transformed diagonal block of B, if sdim > 2. */
            if (sdim > 2) {
                dlacpy_("Upper", &sdim, &sdim, &dwork[ib], &sdim, &b[ib1 + ib1 * b_dim1], ldb, 5L);
                i__3 = sdim - 1;
                i__4 = sdim + 1;
                i__5 = *ldb + 1;
                dcopy_(&i__3, &dwork[ib + 1], &i__4, &b[ib1 + 1 + ib1 * b_dim1], &i__5);
            }
            nrows = ib1 - 1;
            ncols = m - ib3 + 1;
            nrow = ib3 - 1;
            ncol = m - ib1 + 1;
            i__3 = sdim - 1;
            i__4 = sdim - 1;
            dlacpy_("Lower", &i__3, &i__4, &a[ib1 + 1 + ib1 * a_dim1], lda, dum, &c__3, 5L);
            i__3 = sdim - 1;
            i__4 = sdim - 1;
            dlaset_("Lower", &i__3, &i__4, &c_b35, &c_b35, &a[ib1 + 1 + ib1 * a_dim1], lda, 5L);
            i__3 = sdim - 1;
            i__4 = sdim - 1;
            dlacpy_("Upper", &i__3, &i__4, &c__[ib1 + (ib1 + 1) * c_dim1], ldc, &dum[3], &c__3, 5L);
            i__3 = sdim - 1;
            i__4 = sdim - 1;
            dlaset_("Upper", &i__3, &i__4, &c_b35, &c_b35, &c__[ib1 + (ib1 + 1) * c_dim1], ldc, 5L);
            /*           Update A. */
            dgemm_("No Transpose", "No Transpose", &nrow, &sdim, &sdim, &c_b14,
                &a[ib1 * a_dim1 + 1], lda, &dwork[iq1], &sdim, &c_b35, &dwork[iwrk2], &nrow, 12L,
                12L);
            dlacpy_("Full", &nrow, &sdim, &dwork[iwrk2], &nrow, &a[ib1 * a_dim1 + 1], lda, 4L);
            dgemm_("Transpose", "No Transpose", &sdim, &ncol, &sdim, &c_b14, &dwork[iq2], &sdim,
                &a[ib1 + ib1 * a_dim1], lda, &c_b35, &dwork[iwrk2], &sdim, 9L, 12L);
            dlacpy_("Full", &sdim, &ncol, &dwork[iwrk2], &sdim, &a[ib1 + ib1 * a_dim1], lda, 4L);
            i__3 = sdim - 1;
            i__4 = sdim - 1;
            dlacpy_("Lower", &i__3, &i__4, dum, &c__3, &a[ib1 + 1 + ib1 * a_dim1], lda, 5L);
            /*           Update C. */
            dgemm_("No Transpose", "No Transpose", &ncol, &sdim, &sdim, &c_b14,
                &c__[ib1 + ib1 * c_dim1], ldc, &dwork[iq3], &sdim, &c_b35, &dwork[iwrk2], &ncol,
                12L, 12L);
            dlacpy_("Full", &ncol, &sdim, &dwork[iwrk2], &ncol, &c__[ib1 + ib1 * c_dim1], ldc, 4L);
            dgemm_("Transpose", "No Transpose", &sdim, &nrow, &sdim, &c_b14, &dwork[iq2], &sdim,
                &c__[ib1 + c_dim1], ldc, &c_b35, &dwork[iwrk2], &sdim, 9L, 12L);
            dlacpy_("Full", &sdim, &nrow, &dwork[iwrk2], &sdim, &c__[ib1 + c_dim1], ldc, 4L);
            i__3 = sdim - 1;
            i__4 = sdim - 1;
            dlacpy_("Upper", &i__3, &i__4, &dum[3], &c__3, &c__[ib1 + (ib1 + 1) * c_dim1], ldc, 5L);
            /*           Update D. */
            dgemm_("No Transpose", "No Transpose", &m, &sdim, &sdim, &c_b14, &d__[ib1 * d_dim1 + 1],
                ldd, &dwork[iq3], &sdim, &c_b35, &dwork[iwrk2], &m, 12L, 12L);
            dlacpy_("Full", &m, &sdim, &dwork[iwrk2], &m, &d__[ib1 * d_dim1 + 1], ldd, 4L);
            dgemm_("Transpose", "No Transpose", &sdim, &m, &sdim, &c_b14, &dwork[iq2], &sdim,
                &d__[ib1 + d_dim1], ldd, &c_b35, &dwork[iwrk2], &sdim, 9L, 12L);
            dlacpy_("Full", &sdim, &m, &dwork[iwrk2], &sdim, &d__[ib1 + d_dim1], ldd, 4L);
            /*           Update B. */
            if (sdim > 2) {
                nrow = nrows;
                ncol = ncols;
                ibs = ib3;
                ldw = max(1, nrow);
            } else {
                ibs = ib1;
                ldw = nrow;
            }
            dgemm_("No Transpose", "No Transpose", &nrow, &sdim, &sdim, &c_b14,
                &b[ib1 * b_dim1 + 1], ldb, &dwork[iq1], &sdim, &c_b35, &dwork[iwrk2], &ldw, 12L,
                12L);
            dlacpy_("Full", &nrow, &sdim, &dwork[iwrk2], &ldw, &b[ib1 * b_dim1 + 1], ldb, 4L);
            dgemm_("Transpose", "No Transpose", &sdim, &ncol, &sdim, &c_b14, &dwork[iq3], &sdim,
                &b[ib1 + ibs * b_dim1], ldb, &c_b35, &dwork[iwrk2], &sdim, 9L, 12L);
            dlacpy_("Full", &sdim, &ncol, &dwork[iwrk2], &sdim, &b[ib1 + ibs * b_dim1], ldb, 4L);
            /*           Update F. */
            dgemm_("No Transpose", "No Transpose", &nrows, &sdim, &sdim, &c_b14,
                &f[ib1 * f_dim1 + 1], ldf, &dwork[iq3], &sdim, &c_b35, &dwork[iwrk2], &ldw, 12L,
                12L);
            dlacpy_("Full", &nrows, &sdim, &dwork[iwrk2], &ldw, &f[ib1 * f_dim1 + 1], ldf, 4L);
            dgemm_("Transpose", "No Transpose", &sdim, &ncols, &sdim, &c_b14, &dwork[iq3], &sdim,
                &f[ib1 + ib3 * f_dim1], ldf, &c_b35, &dwork[iwrk2], &sdim, 9L, 12L);
            dlacpy_("Full", &sdim, &ncols, &dwork[iwrk2], &sdim, &f[ib1 + ib3 * f_dim1], ldf, 4L);
            i__3 = *ldwork - iwrk2 + 1;
            mb01ru_("Upper", "Transpose", &sdim, &sdim, &c_b35, &c_b14, &f[ib1 + ib1 * f_dim1], ldf,
                &dwork[iq3], &sdim, &f[ib1 + ib1 * f_dim1], ldf, &dwork[iwrk2], &i__3, info, 5L,
                9L);
            i__3 = *ldf + 1;
            dscal_(&sdim, &c_b118, &f[ib1 + ib1 * f_dim1], &i__3);
            if (lcmpq) {
                /*              Update Q. */
                dgemm_("No Transpose", "No Transpose", n, &sdim, &sdim, &c_b14,
                    &q[ib1 * q_dim1 + 1], ldq, &dwork[iq1], &sdim, &c_b35, &dwork[iwrk2], n, 12L,
                    12L);
                dlacpy_("Full", n, &sdim, &dwork[iwrk2], n, &q[ib1 * q_dim1 + 1], ldq, 4L);
                dgemm_("No Transpose", "No Transpose", n, &sdim, &sdim, &c_b14,
                    &q[(m + ib1) * q_dim1 + 1], ldq, &dwork[iq3], &sdim, &c_b35, &dwork[iwrk2], n,
                    12L, 12L);
                dlacpy_("Full", n, &sdim, &dwork[iwrk2], n, &q[(m + ib1) * q_dim1 + 1], ldq, 4L);
            }
            if (lcmpu) {
                /*              Update U. */
                dgemm_("No Transpose", "No Transpose", &m, &sdim, &sdim, &c_b14,
                    &u1[ib1 * u1_dim1 + 1], ldu1, &dwork[iq2], &sdim, &c_b35, &dwork[iwrk2], &m,
                    12L, 12L);
                dlacpy_("Full", &m, &sdim, &dwork[iwrk2], &m, &u1[ib1 * u1_dim1 + 1], ldu1, 4L);
                dgemm_("No Transpose", "No Transpose", &m, &sdim, &sdim, &c_b14,
                    &u2[ib1 * u2_dim1 + 1], ldu2, &dwork[iq2], &sdim, &c_b35, &dwork[iwrk2], &m,
                    12L, 12L);
                dlacpy_("Full", &m, &sdim, &dwork[iwrk2], &m, &u2[ib1 * u2_dim1 + 1], ldu2, 4L);
            }
            /*           Update index list IWORK(1:M)if a 1-by-1 and 2-by-2 block */
            /*           have been swapped. */
            hlp = dim2 - dim1;
            if (hlp == 1) {
                /*              First block was 2-by-2. */
                iwork[j + 1] = ib1 + 1;
            } else if (hlp == -1) {
                /*              Second block was 2-by-2. */
                iwork[j + 1] = ib1 + 2;
            }
            /* L60: */
        }
        /* L70: */
    }
    if (m > 1) {
        /*        Restore the elements A(M,M-1), C(M-1,M), and F(M,M-1). */
        a[m + (m - 1) * a_dim1] = a2;
        c__[m - 1 + m * c_dim1] = c2;
        f[m + (m - 1) * f_dim1] = f2;
    }
    if (mm > 0) {
        *neig = iwork[mm + 1] - 1;
    } else {
        *neig = 0;
    }
    return 0;
    /* *** Last line of MB03ID *** */
} /* mb03id_ */
