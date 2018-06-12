/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static doublereal c_b13 = 1.;
static doublereal c_b17 = 0.;
static doublereal c_b80 = .5;
static integer c__1 = 1;
static doublereal c_b197 = -1.;

EXPORTSYMBOL /* Subroutine */ int mb03jd_(compq, n, a, lda, d__, ldd, b, ldb, f, ldf, q, ldq, neig,
    iwork, liwork, dwork, ldwork, info, compq_len) char* compq;
integer* n;
doublereal* a;
integer* lda;
doublereal* d__;
integer* ldd;
doublereal* b;
integer* ldb;
doublereal* f;
integer* ldf;
doublereal* q;
integer *ldq, *neig, *iwork, *liwork;
doublereal* dwork;
integer *ldwork, *info;
ftnlen compq_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, d_dim1, d_offset, f_dim1, f_offset, q_dim1,
        q_offset, i__1, i__2, i__3, i__4, i__5;
    doublereal d__1, d__2;
    /* Builtin functions */
    double d_sign();
    /* Local variables */
    static doublereal prec;
    extern doublereal ddot_();
    static integer ncol, sdim;
    static doublereal nrma, nrmb;
    static integer iupd, upds, nrow, itmp1, itmp2, itmp3, iwrk1, iwrk2, iwrk3, iwrk4, iwrk5, i__, j,
        k;
    extern /* Subroutine */ int mb03dd_();
    static integer m;
    extern /* Subroutine */ int mb01ld_();
    static integer r__;
    extern /* Subroutine */ int mb03hd_(), dscal_(), dgemm_();
    extern logical lsame_();
    extern /* Subroutine */ int dgemv_(), mb01ru_(), mb01rx_();
    static logical lcmpq, liniq;
    static integer ncols;
    extern /* Subroutine */ int dcopy_();
    static doublereal a2;
    static logical lupdq;
    static doublereal d1, d2, d3, f2;
    extern /* Subroutine */ int daxpy_();
    static integer optdw, nrows, ia, ib, ic;
    static doublereal q11, q12, q21, q22;
    extern doublereal dlamch_();
    static integer mm, ir, is, mp;
    extern doublereal dlanhs_();
    extern /* Subroutine */ int dlacpy_(), dlaset_(), xerbla_();
    static integer iauple, ibuple;
    extern doublereal dlantr_();
    static integer iqlole, ibupri, ib1, ib2, ib3, iqlori, iquple, iqupri, iq1, iq2, ics;
    static doublereal par[2];
    static integer hlp, ldw;
    static doublereal tol, tmp;
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
    /*     structured Schur form, */
    /*           (  A  D  )      (  B  F  ) */
    /*       S = (        ), H = (        ), */
    /*           (  0  A' )      (  0 -B' ) */
    /*     with A upper triangular and B upper quasi-triangular, to the */
    /*     leading principal subpencil, while keeping the triangular form: */
    /*              (  Aout  Dout  )         (  Bout  Fout  ) */
    /*       Sout = (              ), Hout = (              ), where */
    /*              (    0   Aout' )         (  0    -Bout' ) */
    /*     Aout is upper triangular and Bout is upper quasi-triangular. */
    /*     Optionally, if COMPQ = 'I' or COMPQ = 'U', an orthogonal matrix Q */
    /*     is determined such that the pencil */
    /*                                                     (  0  I  ) */
    /*       J Q' J' (aS - bH) Q = aSout - bHout, with J = (        ), */
    /*                                                     ( -I  0  ) */
    /*     keeps the triangular form, but all eigenvalues with strictly */
    /*     negative real part are in the leading principal subpencil. */
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
    /*     D       (input/output) DOUBLE PRECISION array, dimension */
    /*                           (LDD, N/2) */
    /*             On entry, the leading N/2-by-N/2 part of this array must */
    /*             contain the upper triangular part of the skew-symmetric */
    /*             matrix D. The diagonal need not be set to zero. */
    /*             On exit, the leading  N/2-by-N/2 part of this array */
    /*             contains the transformed upper triangular part of the */
    /*             matrix Dout. */
    /*             The strictly lower triangular part of this array is */
    /*             not referenced, except for the element D(N/2,N/2-1), but */
    /*             its initial value is preserved. */
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
    /*                           (LDF, N/2) */
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
    /*                LDWORK >= MAX(2*N+32,108); */
    /*             if COMPQ = 'I' or COMPQ = 'U', */
    /*                LDWORK >= MAX(4*N+32,108). */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0: succesful exit; */
    /*             < 0: if INFO = -i, the i-th argument had an illegal value; */
    /*             = 1: error occured during execution of MB03DD; */
    /*             = 2: error occured during execution of MB03HD. */
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
    /*     described on page 33 in [1]. To achieve those transformations the */
    /*     elementary subroutines MB03DD and MB03HD are called for the */
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
    /*     Chemnitz, October 16, 2008. */
    /*     V. Sima, Dec. 2009 (SLICOT version of the routine DHAUNX). */
    /*     REVISIONS */
    /*     V. Sima, Aug. 2009; Jan. 2010, Oct. 2010, Nov. 2010. */
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
    --iwork;
    --dwork;
    /* Function Body */
    m = *n / 2;
    liniq = lsame_(compq, "I", 1L, 1L);
    lupdq = lsame_(compq, "U", 1L, 1L);
    lcmpq = liniq || lupdq;
    if (lcmpq) {
        /* Computing MAX */
        i__1 = (*n << 2) + 32;
        optdw = max(i__1, 108);
    } else {
        /* Computing MAX */
        i__1 = (*n << 1) + 32;
        optdw = max(i__1, 108);
    }
    /*     Test the input arguments. */
    *info = 0;
    if (!(lsame_(compq, "N", 1L, 1L) || lcmpq)) {
        *info = -1;
    } else if (*n < 0 || *n % 2 != 0) {
        *info = -2;
    } else if (*lda < max(1, m)) {
        *info = -4;
    } else if (*ldd < max(1, m)) {
        *info = -6;
    } else if (*ldb < max(1, m)) {
        *info = -8;
    } else if (*ldf < max(1, m)) {
        *info = -10;
    } else if (*ldq < 1 || lcmpq && *ldq < *n) {
        *info = -12;
    } else if (*liwork < *n + 1) {
        *info = -15;
    } else if (*ldwork < optdw) {
        *info = -17;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("MB03JD", &i__1, 6L);
        return 0;
    }
    /*     Quick return if possible. */
    if (*n == 0) {
        *neig = 0;
        return 0;
    }
    /*     Determine machine constants. */
    prec = dlamch_("Precision", 9L);
    /* Computing MIN */
    d__1 = (doublereal)(*n);
    tol = min(d__1, 10.) * prec;
    par[0] = prec;
    par[1] = dlamch_("Safe minimum", 12L);
    /*     STEP 0: Determine location and size of diagonal blocks. */
    /*             IWORK(J) and IWORK(IS+J) are used to indicate the */
    /*             beginning index and the kind of eigenvalues of the */
    /*             J-th diagonal block of the subpencil aA - bB. For a */
    /*             2-by-2 block, it is assumed that both eigenvalues have */
    /*             real parts with the same sign (true for a structured */
    /*             Schur form). */
    i__ = 1;
    j = 1;
    is = m + 1;
    nrma = dlantr_("One", "Upper", "Non-diag", &m, &m, &a[a_offset], lda, &dwork[1], 3L, 5L, 8L);
    nrmb = dlanhs_("One", &m, &b[b_offset], ldb, &dwork[1], 3L);
    /*     Partition blocks. */
    /*     WHILE( I.LE.M-1 ) DO */
L10:
    if (i__ <= m - 1) {
        iwork[j] = i__;
        if ((d__1 = b[i__ + 1 + i__ * b_dim1], abs(d__1)) <= tol * nrmb) {
            /*           1-by-1 block. */
            b[i__ + 1 + i__ * b_dim1] = 0.;
            if ((d__1 = a[i__ + i__ * a_dim1], abs(d__1)) <= tol * nrma
                || (d__2 = b[i__ + i__ * b_dim1], abs(d__2)) <= tol * nrmb) {
                /*              Eigenvalue is infinite, 0, or 0/0. */
                iwork[is + j] = 0;
            } else {
                d__1 = a[i__ + i__ * a_dim1] * b[i__ + i__ * b_dim1];
                iwork[is + j] = (integer)d_sign(&c_b13, &d__1);
            }
            ++i__;
        } else {
            /*           2-by-2 block. */
            if (a[i__ + i__ * a_dim1] == 0. || a[i__ + 1 + (i__ + 1) * a_dim1] == 0.) {
                /*              Eigenvalue is infinite. */
                iwork[is + j] = 0;
            } else {
                tmp = (b[i__ + i__ * b_dim1]
                          - b[i__ + 1 + i__ * b_dim1] / a[i__ + 1 + (i__ + 1) * a_dim1]
                              * a[i__ + (i__ + 1) * a_dim1])
                        / a[i__ + i__ * a_dim1]
                    + b[i__ + 1 + (i__ + 1) * b_dim1] / a[i__ + 1 + (i__ + 1) * a_dim1];
                if (tmp == 0.) {
                    iwork[is + j] = 0;
                } else {
                    iwork[is + j] = (integer)d_sign(&c_b13, &tmp);
                }
            }
            i__ += 2;
        }
        ++j;
        goto L10;
        /*     END WHILE 10 */
    }
    if (i__ == m) {
        iwork[j] = i__;
        if ((d__1 = a[i__ + i__ * a_dim1], abs(d__1)) <= tol * nrma
            || (d__2 = b[i__ + i__ * b_dim1], abs(d__2)) <= tol * nrmb) {
            /*           Eigenvalue is infinite or zero. */
            iwork[is + j] = 0;
        } else {
            d__1 = a[i__ + i__ * a_dim1] * b[i__ + i__ * b_dim1];
            iwork[is + j] = (integer)d_sign(&c_b13, &d__1);
        }
        ++j;
    }
    r__ = j - 1;
    /*     Initialize Q if appropriate. */
    if (liniq) {
        iupd = m + 1;
        upds = m;
        dlaset_("Full", n, n, &c_b17, &c_b13, &q[q_offset], ldq, 4L);
    } else if (lupdq) {
        iupd = 1;
        upds = *n;
    }
    if (m > 1) {
        /*        Save the lower triangle of the submatrix D(M-1:M,M-1:M) and the */
        /*        elements A(M,M-1), F(M,M-1), which might be overwritten. */
        d1 = d__[m - 1 + (m - 1) * d_dim1];
        d2 = d__[m + (m - 1) * d_dim1];
        d3 = d__[m + m * d_dim1];
        a2 = a[m + (m - 1) * a_dim1];
        f2 = f[m + (m - 1) * f_dim1];
    }
    /*     STEP 1: Reorder the eigenvalues in the subpencil aA - bB. */
    mm = 0;
    mp = j;
    /*     I. Reorder the eigenvalues with negative real parts to the top. */
    /*     Set pointers for the inputs and outputs of MB03DD. */
    iq1 = 1;
    iq2 = iq1 + 16;
    ia = iq2 + 16;
    ib = ia + 16;
    iwrk1 = ib + 16;
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
                /*              Copy the relevant part of A(ib1:ib3-1,ib1:ib3-1) and */
                /*              B(ib1:ib3-1,ib1:ib3-1) to DWORK as inputs for MB03DD. */
                /*              Also, set the additional zero elements. */
                dlacpy_("Upper", &sdim, &sdim, &a[ib1 + ib1 * a_dim1], lda, &dwork[ia], &sdim, 5L);
                i__2 = sdim - 1;
                i__3 = sdim - 1;
                dlaset_("Lower", &i__2, &i__3, &c_b17, &c_b17, &dwork[ia + 1], &sdim, 5L);
                dlacpy_("Upper", &sdim, &sdim, &b[ib1 + ib1 * b_dim1], ldb, &dwork[ib], &sdim, 5L);
                i__2 = sdim - 1;
                i__3 = *ldb + 1;
                i__4 = sdim + 1;
                dcopy_(&i__2, &b[ib1 + 1 + ib1 * b_dim1], &i__3, &dwork[ib + 1], &i__4);
                i__2 = sdim - 2;
                i__3 = sdim - 2;
                dlaset_("Lower", &i__2, &i__3, &c_b17, &c_b17, &dwork[ib + 2], &sdim, 5L);
                /*              Perform eigenvalue/matrix block exchange. */
                /*              Workspace: IWRK1 + 43. */
                i__2 = *ldwork - iwrk1 + 1;
                mb03dd_("Triangular", &dim1, &dim2, &prec, &dwork[ib], &sdim, &dwork[ia], &sdim,
                    &dwork[iq1], &sdim, &dwork[iq2], &sdim, &dwork[iwrk1], &i__2, info, 10L);
                if (*info > 0) {
                    *info = 1;
                    return 0;
                }
                /*              Copy the transformed diagonal blocks, if sdim > 2. */
                nrows = ib1 - 1;
                ncols = m - ib3 + 1;
                ics = ib3;
                if (sdim > 2) {
                    dlacpy_(
                        "Upper", &sdim, &sdim, &dwork[ia], &sdim, &a[ib1 + ib1 * a_dim1], lda, 5L);
                    dlacpy_(
                        "Upper", &sdim, &sdim, &dwork[ib], &sdim, &b[ib1 + ib1 * b_dim1], ldb, 5L);
                    i__2 = sdim - 1;
                    i__3 = sdim + 1;
                    i__4 = *ldb + 1;
                    dcopy_(&i__2, &dwork[ib + 1], &i__3, &b[ib1 + 1 + ib1 * b_dim1], &i__4);
                    nrow = nrows;
                    ncol = ncols;
                    ic = ics;
                    ldw = max(1, nrow);
                } else {
                    tmp = a[ib1 + 1 + ib1 * a_dim1];
                    a[ib1 + 1 + ib1 * a_dim1] = 0.;
                    nrow = ib3 - 1;
                    ncol = m - ib1 + 1;
                    ic = ib1;
                    ldw = nrow;
                }
                /*              Update A. */
                /*              Workspace: IWRK2 + 2*N - 1. */
                dgemm_("No Transpose", "No Transpose", &nrow, &sdim, &sdim, &c_b13,
                    &a[ib1 * a_dim1 + 1], lda, &dwork[iq1], &sdim, &c_b17, &dwork[iwrk2], &ldw, 12L,
                    12L);
                dlacpy_("Full", &nrow, &sdim, &dwork[iwrk2], &ldw, &a[ib1 * a_dim1 + 1], lda, 4L);
                dgemm_("Transpose", "No Transpose", &sdim, &ncol, &sdim, &c_b13, &dwork[iq2], &sdim,
                    &a[ib1 + ic * a_dim1], lda, &c_b17, &dwork[iwrk2], &sdim, 9L, 12L);
                dlacpy_("Full", &sdim, &ncol, &dwork[iwrk2], &sdim, &a[ib1 + ic * a_dim1], lda, 4L);
                if (sdim == 2) {
                    a[ib1 + 1 + ib1 * a_dim1] = tmp;
                }
                /*              Update D. */
                dgemm_("No Transpose", "No Transpose", &nrows, &sdim, &sdim, &c_b13,
                    &d__[ib1 * d_dim1 + 1], ldd, &dwork[iq2], &sdim, &c_b17, &dwork[iwrk2], &ldw,
                    12L, 12L);
                dlacpy_(
                    "Full", &nrows, &sdim, &dwork[iwrk2], &ldw, &d__[ib1 * d_dim1 + 1], ldd, 4L);
                dgemm_("Transpose", "No Transpose", &sdim, &ncols, &sdim, &c_b13, &dwork[iq2],
                    &sdim, &d__[ib1 + ics * d_dim1], ldd, &c_b17, &dwork[iwrk2], &sdim, 9L, 12L);
                dlacpy_(
                    "Full", &sdim, &ncols, &dwork[iwrk2], &sdim, &d__[ib1 + ics * d_dim1], ldd, 4L);
                i__2 = *ldwork - iwrk2 + 1;
                mb01ld_("Upper", "Transpose", &sdim, &sdim, &c_b17, &c_b13,
                    &d__[ib1 + ib1 * d_dim1], ldd, &dwork[iq2], &sdim, &d__[ib1 + ib1 * d_dim1],
                    ldd, &dwork[iwrk2], &i__2, info, 5L, 9L);
                /*              Update B. */
                dgemm_("No Transpose", "No Transpose", &nrow, &sdim, &sdim, &c_b13,
                    &b[ib1 * b_dim1 + 1], ldb, &dwork[iq1], &sdim, &c_b17, &dwork[iwrk2], &ldw, 12L,
                    12L);
                dlacpy_("Full", &nrow, &sdim, &dwork[iwrk2], &ldw, &b[ib1 * b_dim1 + 1], ldb, 4L);
                dgemm_("Transpose", "No Transpose", &sdim, &ncol, &sdim, &c_b13, &dwork[iq2], &sdim,
                    &b[ib1 + ic * b_dim1], ldb, &c_b17, &dwork[iwrk2], &sdim, 9L, 12L);
                dlacpy_("Full", &sdim, &ncol, &dwork[iwrk2], &sdim, &b[ib1 + ic * b_dim1], ldb, 4L);
                /*              Update F. */
                dgemm_("No Transpose", "No Transpose", &nrows, &sdim, &sdim, &c_b13,
                    &f[ib1 * f_dim1 + 1], ldf, &dwork[iq2], &sdim, &c_b17, &dwork[iwrk2], &ldw, 12L,
                    12L);
                dlacpy_("Full", &nrows, &sdim, &dwork[iwrk2], &ldw, &f[ib1 * f_dim1 + 1], ldf, 4L);
                dgemm_("Transpose", "No Transpose", &sdim, &ncols, &sdim, &c_b13, &dwork[iq2],
                    &sdim, &f[ib1 + ics * f_dim1], ldf, &c_b17, &dwork[iwrk2], &sdim, 9L, 12L);
                dlacpy_(
                    "Full", &sdim, &ncols, &dwork[iwrk2], &sdim, &f[ib1 + ics * f_dim1], ldf, 4L);
                i__2 = *ldwork - iwrk2 + 1;
                mb01ru_("Upper", "Transpose", &sdim, &sdim, &c_b17, &c_b13, &f[ib1 + ib1 * f_dim1],
                    ldf, &dwork[iq2], &sdim, &f[ib1 + ib1 * f_dim1], ldf, &dwork[iwrk2], &i__2,
                    info, 5L, 9L);
                i__2 = *ldf + 1;
                dscal_(&sdim, &c_b80, &f[ib1 + ib1 * f_dim1], &i__2);
                if (lcmpq) {
                    /*                 Update Q. */
                    /*                 Workspace: IWRK2 + 2*N - 1, if COMPQ = 'I'; */
                    /*                            IWRK2 + 4*N - 1, if COMPQ = 'U'. */
                    dgemm_("No Transpose", "No Transpose", &upds, &sdim, &sdim, &c_b13,
                        &q[ib1 * q_dim1 + 1], ldq, &dwork[iq1], &sdim, &c_b17, &dwork[iwrk2], &upds,
                        12L, 12L);
                    dlacpy_(
                        "Full", &upds, &sdim, &dwork[iwrk2], &upds, &q[ib1 * q_dim1 + 1], ldq, 4L);
                    dgemm_("No Transpose", "No Transpose", &upds, &sdim, &sdim, &c_b13,
                        &q[iupd + (m + ib1) * q_dim1], ldq, &dwork[iq2], &sdim, &c_b17,
                        &dwork[iwrk2], &upds, 12L, 12L);
                    dlacpy_("Full", &upds, &sdim, &dwork[iwrk2], &upds,
                        &q[iupd + (m + ib1) * q_dim1], ldq, 4L);
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
                /*              Copy the relevant part of A(ib1:ib3-1,ib1:ib3-1) and */
                /*              B(ib1:ib3-1,ib1:ib3-1) to DWORK as inputs for MB03DD. */
                /*              Also, set the additional zero elements. */
                dlacpy_("Upper", &sdim, &sdim, &a[ib1 + ib1 * a_dim1], lda, &dwork[ia], &sdim, 5L);
                i__2 = sdim - 1;
                i__3 = sdim - 1;
                dlaset_("Lower", &i__2, &i__3, &c_b17, &c_b17, &dwork[ia + 1], &sdim, 5L);
                dlacpy_("Upper", &sdim, &sdim, &b[ib1 + ib1 * b_dim1], ldb, &dwork[ib], &sdim, 5L);
                i__2 = sdim - 1;
                i__3 = *ldb + 1;
                i__4 = sdim + 1;
                dcopy_(&i__2, &b[ib1 + 1 + ib1 * b_dim1], &i__3, &dwork[ib + 1], &i__4);
                i__2 = sdim - 2;
                i__3 = sdim - 2;
                dlaset_("Lower", &i__2, &i__3, &c_b17, &c_b17, &dwork[ib + 2], &sdim, 5L);
                /*              Perform eigenvalue/matrix block exchange. */
                i__2 = *ldwork - iwrk1 + 1;
                mb03dd_("Triangular", &dim1, &dim2, &prec, &dwork[ib], &sdim, &dwork[ia], &sdim,
                    &dwork[iq1], &sdim, &dwork[iq2], &sdim, &dwork[iwrk1], &i__2, info, 10L);
                if (*info > 0) {
                    *info = 1;
                    return 0;
                }
                /*              Copy the transformed diagonal blocks, if sdim > 2. */
                nrows = ib1 - 1;
                ncols = m - ib3 + 1;
                ics = ib3;
                if (sdim > 2) {
                    dlacpy_(
                        "Upper", &sdim, &sdim, &dwork[ia], &sdim, &a[ib1 + ib1 * a_dim1], lda, 5L);
                    dlacpy_(
                        "Upper", &sdim, &sdim, &dwork[ib], &sdim, &b[ib1 + ib1 * b_dim1], ldb, 5L);
                    i__2 = sdim - 1;
                    i__3 = sdim + 1;
                    i__4 = *ldb + 1;
                    dcopy_(&i__2, &dwork[ib + 1], &i__3, &b[ib1 + 1 + ib1 * b_dim1], &i__4);
                    nrow = nrows;
                    ncol = ncols;
                    ic = ics;
                    ldw = max(1, nrow);
                } else {
                    tmp = a[ib1 + 1 + ib1 * a_dim1];
                    a[ib1 + 1 + ib1 * a_dim1] = 0.;
                    nrow = ib3 - 1;
                    ncol = m - ib1 + 1;
                    ic = ib1;
                    ldw = nrow;
                }
                /*              Update A. */
                dgemm_("No Transpose", "No Transpose", &nrow, &sdim, &sdim, &c_b13,
                    &a[ib1 * a_dim1 + 1], lda, &dwork[iq1], &sdim, &c_b17, &dwork[iwrk2], &ldw, 12L,
                    12L);
                dlacpy_("Full", &nrow, &sdim, &dwork[iwrk2], &ldw, &a[ib1 * a_dim1 + 1], lda, 4L);
                dgemm_("Transpose", "No Transpose", &sdim, &ncol, &sdim, &c_b13, &dwork[iq2], &sdim,
                    &a[ib1 + ic * a_dim1], lda, &c_b17, &dwork[iwrk2], &sdim, 9L, 12L);
                dlacpy_("Full", &sdim, &ncol, &dwork[iwrk2], &sdim, &a[ib1 + ic * a_dim1], lda, 4L);
                if (sdim == 2) {
                    a[ib1 + 1 + ib1 * a_dim1] = tmp;
                }
                /*              Update D. */
                dgemm_("No Transpose", "No Transpose", &nrows, &sdim, &sdim, &c_b13,
                    &d__[ib1 * d_dim1 + 1], ldd, &dwork[iq2], &sdim, &c_b17, &dwork[iwrk2], &ldw,
                    12L, 12L);
                dlacpy_(
                    "Full", &nrows, &sdim, &dwork[iwrk2], &ldw, &d__[ib1 * d_dim1 + 1], ldd, 4L);
                dgemm_("Transpose", "No Transpose", &sdim, &ncols, &sdim, &c_b13, &dwork[iq2],
                    &sdim, &d__[ib1 + ics * d_dim1], ldd, &c_b17, &dwork[iwrk2], &sdim, 9L, 12L);
                dlacpy_(
                    "Full", &sdim, &ncols, &dwork[iwrk2], &sdim, &d__[ib1 + ics * d_dim1], ldd, 4L);
                i__2 = *ldwork - iwrk2 + 1;
                mb01ld_("Upper", "Transpose", &sdim, &sdim, &c_b17, &c_b13,
                    &d__[ib1 + ib1 * d_dim1], ldd, &dwork[iq2], &sdim, &d__[ib1 + ib1 * d_dim1],
                    ldd, &dwork[iwrk2], &i__2, info, 5L, 9L);
                /*              Update B. */
                dgemm_("No Transpose", "No Transpose", &nrow, &sdim, &sdim, &c_b13,
                    &b[ib1 * b_dim1 + 1], ldb, &dwork[iq1], &sdim, &c_b17, &dwork[iwrk2], &ldw, 12L,
                    12L);
                dlacpy_("Full", &nrow, &sdim, &dwork[iwrk2], &ldw, &b[ib1 * b_dim1 + 1], ldb, 4L);
                dgemm_("Transpose", "No Transpose", &sdim, &ncol, &sdim, &c_b13, &dwork[iq2], &sdim,
                    &b[ib1 + ic * b_dim1], ldb, &c_b17, &dwork[iwrk2], &sdim, 9L, 12L);
                dlacpy_("Full", &sdim, &ncol, &dwork[iwrk2], &sdim, &b[ib1 + ic * b_dim1], ldb, 4L);
                /*              Update F. */
                dgemm_("No Transpose", "No Transpose", &nrows, &sdim, &sdim, &c_b13,
                    &f[ib1 * f_dim1 + 1], ldf, &dwork[iq2], &sdim, &c_b17, &dwork[iwrk2], &ldw, 12L,
                    12L);
                dlacpy_("Full", &nrows, &sdim, &dwork[iwrk2], &ldw, &f[ib1 * f_dim1 + 1], ldf, 4L);
                dgemm_("Transpose", "No Transpose", &sdim, &ncols, &sdim, &c_b13, &dwork[iq2],
                    &sdim, &f[ib1 + ics * f_dim1], ldf, &c_b17, &dwork[iwrk2], &sdim, 9L, 12L);
                dlacpy_(
                    "Full", &sdim, &ncols, &dwork[iwrk2], &sdim, &f[ib1 + ics * f_dim1], ldf, 4L);
                i__2 = *ldwork - iwrk2 + 1;
                mb01ru_("Upper", "Transpose", &sdim, &sdim, &c_b17, &c_b13, &f[ib1 + ib1 * f_dim1],
                    ldf, &dwork[iq2], &sdim, &f[ib1 + ib1 * f_dim1], ldf, &dwork[iwrk2], &i__2,
                    info, 5L, 9L);
                i__2 = *ldf + 1;
                dscal_(&sdim, &c_b80, &f[ib1 + ib1 * f_dim1], &i__2);
                if (lcmpq) {
                    /*                 Update Q. */
                    dgemm_("No Transpose", "No Transpose", &upds, &sdim, &sdim, &c_b13,
                        &q[ib1 * q_dim1 + 1], ldq, &dwork[iq1], &sdim, &c_b17, &dwork[iwrk2], &upds,
                        12L, 12L);
                    dlacpy_(
                        "Full", &upds, &sdim, &dwork[iwrk2], &upds, &q[ib1 * q_dim1 + 1], ldq, 4L);
                    dgemm_("No Transpose", "No Transpose", &upds, &sdim, &sdim, &c_b13,
                        &q[iupd + (m + ib1) * q_dim1], ldq, &dwork[iq2], &sdim, &c_b17,
                        &dwork[iwrk2], &upds, 12L, 12L);
                    dlacpy_("Full", &upds, &sdim, &dwork[iwrk2], &upds,
                        &q[iupd + (m + ib1) * q_dim1], ldq, 4L);
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
    /*     Set pointers for the inputs and outputs of MB03HD. */
    iquple = 1;
    iauple = iquple + 16;
    ibuple = iauple + 8;
    iwrk5 = ibuple + 8;
    iwrk3 = iauple;
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
            /*           Build the (small) full skew-symmetric matrix D(M-1:M,M-1:M) */
            /*           and the (small) symmetric matrix F(M-1:M,M-1:M). */
            d__[ir + ir * d_dim1] = 0.;
            d__[m + ir * d_dim1] = -d__[ir + m * d_dim1];
            d__[m + m * d_dim1] = 0.;
            f[m + ir * f_dim1] = f[ir + m * f_dim1];
        }
        /*        Calculate position of submatrices in DWORK. */
        ibupri = ibuple + dim1 * dim1;
        iqlole = iquple + dim1;
        iqupri = iquple + dim1 * sdim;
        iqlori = iqupri + dim1;
        /*        Generate input matrices for MB03HD built of submatrices of A, */
        /*        D, B, and F. */
        if (dim1 == 2) {
            dlacpy_("Upper", &dim1, &dim1, &a[ir + ir * a_dim1], lda, &dwork[iauple], &dim1, 5L);
            dwork[iauple + 6] = d__[ir + (ir + 1) * d_dim1];
            dlacpy_("Full", &dim1, &dim1, &b[ir + ir * b_dim1], ldb, &dwork[ibuple], &dim1, 4L);
            dlacpy_("Upper", &dim1, &dim1, &f[ir + ir * f_dim1], ldf, &dwork[ibupri], &dim1, 5L);
        } else {
            dwork[ibuple] = b[ir + ir * b_dim1];
            dwork[ibupri] = f[ir + ir * f_dim1];
        }
        /*        Perform eigenvalue exchange. */
        /*        Workspace: IWRK5 + 22, if SDIM = 4. */
        mb03hd_(&sdim, &dwork[iauple], &dim1, &dwork[ibuple], &dim1, par, &dwork[iquple], &sdim,
            &dwork[iwrk5], info);
        if (*info > 0) {
            *info = 2;
            return 0;
        }
        if (dim1 == 2) {
            /*           Update A by transformations from the right. */
            /*           Workspace: IWRK3 + N - 1. */
            dlacpy_("Full", &m, &dim1, &a[ir * a_dim1 + 1], lda, &dwork[iwrk3], &m, 4L);
            dgemm_("No Transpose", "No Transpose", &m, &dim1, &dim1, &c_b13, &dwork[iwrk3], &m,
                &dwork[iquple], &sdim, &c_b17, &a[ir * a_dim1 + 1], lda, 12L, 12L);
            dgemm_("No Transpose", "No Transpose", &m, &dim1, &dim1, &c_b13, &d__[ir * d_dim1 + 1],
                ldd, &dwork[iqlole], &sdim, &c_b13, &a[ir * a_dim1 + 1], lda, 12L, 12L);
            /*           Update D by transformations from the right. */
            dgemm_("No Transpose", "No Transpose", &m, &dim1, &dim1, &c_b13, &dwork[iwrk3], &m,
                &dwork[iqupri], &sdim, &c_b17, &dwork[itmp1], &m, 12L, 12L);
            dgemm_("No Transpose", "No Transpose", &m, &dim1, &dim1, &c_b13, &d__[ir * d_dim1 + 1],
                ldd, &dwork[iqlori], &sdim, &c_b13, &dwork[itmp1], &m, 12L, 12L);
            dlacpy_("Full", &m, &dim1, &dwork[itmp1], &m, &d__[ir * d_dim1 + 1], ldd, 4L);
            /*           Compute the intermediate product Af'*Q21 and the second */
            /*           column of Af'*Q22, with Af = A(M-1:M,M-1:M). */
            dgemm_("Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b13,
                &dwork[iwrk3 + m - dim1], &m, &dwork[iqlole], &sdim, &c_b17, &dwork[itmp1], &dim1,
                9L, 12L);
            dgemv_("Transpose", &dim1, &dim1, &c_b13, &dwork[iwrk3 + m - dim1], &m,
                &dwork[iqlori + sdim], &c__1, &c_b17, &dwork[itmp2], &c__1, 9L);
            /*           Update A by transformations from the left. */
            dlacpy_("Full", &dim1, &dim1, &a[ir + ir * a_dim1], lda, &dwork[iwrk3], &dim1, 4L);
            dgemm_("Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b197, &dwork[iqupri], &sdim,
                &dwork[itmp1], &dim1, &c_b17, &a[ir + ir * a_dim1], lda, 9L, 12L);
            dgemm_("Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b13, &dwork[iqlori], &sdim,
                &dwork[iwrk3], &dim1, &c_b13, &a[ir + ir * a_dim1], lda, 9L, 12L);
            /*           Update D by transformations from the left. */
            d__[ir + m * d_dim1] = ddot_(&dim1, &dwork[iqlori], &c__1, &d__[ir + m * d_dim1], &c__1)
                - ddot_(&dim1, &dwork[iqupri], &c__1, &dwork[itmp2], &c__1);
            /*           Update B by transformations from the right. */
            dlacpy_("Full", &m, &dim1, &b[ir * b_dim1 + 1], ldb, &dwork[iwrk3], &m, 4L);
            dgemm_("No Transpose", "No Transpose", &m, &dim1, &dim1, &c_b13, &dwork[iwrk3], &m,
                &dwork[iquple], &sdim, &c_b17, &b[ir * b_dim1 + 1], ldb, 12L, 12L);
            dgemm_("No Transpose", "No Transpose", &m, &dim1, &dim1, &c_b13, &f[ir * f_dim1 + 1],
                ldf, &dwork[iqlole], &sdim, &c_b13, &b[ir * b_dim1 + 1], ldb, 12L, 12L);
            /*           Update F by transformations from the right. */
            dgemm_("No Transpose", "No Transpose", &m, &dim1, &dim1, &c_b13, &dwork[iwrk3], &m,
                &dwork[iqupri], &sdim, &c_b17, &dwork[itmp1], &m, 12L, 12L);
            dgemm_("No Transpose", "No Transpose", &m, &dim1, &dim1, &c_b13, &f[ir * f_dim1 + 1],
                ldf, &dwork[iqlori], &sdim, &c_b13, &dwork[itmp1], &m, 12L, 12L);
            dlacpy_("Full", &m, &dim1, &dwork[itmp1], &m, &f[ir * f_dim1 + 1], ldf, 4L);
            /*           Compute intermediate products Bf'*Q21 and Bf'*Q22, with */
            /*           Bf = B(M-1:M,M-1:M). */
            dgemm_("Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b13,
                &dwork[iwrk3 + m - dim1], &m, &dwork[iqlole], &sdim, &c_b17, &dwork[itmp1], &dim1,
                9L, 12L);
            dgemm_("Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b13,
                &dwork[iwrk3 + m - dim1], &m, &dwork[iqlori], &sdim, &c_b17, &dwork[itmp2], &dim1,
                9L, 12L);
            /*           Update B by transformations from the left. */
            dlacpy_("Full", &dim1, &dim1, &b[ir + ir * b_dim1], ldb, &dwork[itmp3], &dim1, 4L);
            dgemm_("Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b13, &dwork[iqupri], &sdim,
                &dwork[itmp1], &dim1, &c_b17, &b[ir + ir * b_dim1], ldb, 9L, 12L);
            dgemm_("Transpose", "No Transpose", &dim1, &dim1, &dim1, &c_b13, &dwork[iqlori], &sdim,
                &dwork[itmp3], &dim1, &c_b13, &b[ir + ir * b_dim1], ldb, 9L, 12L);
            /*           Update F by transformations from the left. */
            mb01rx_("Left", "Upper", "Transpose", &dim1, &dim1, &c_b17, &c_b13, &dwork[itmp1],
                &dim1, &dwork[iqlori], &sdim, &f[ir + ir * f_dim1], ldf, info, 4L, 5L, 9L);
            mb01rx_("Left", "Upper", "Transpose", &dim1, &dim1, &c_b13, &c_b13, &dwork[itmp1],
                &dim1, &dwork[iqupri], &sdim, &dwork[itmp2], &dim1, info, 4L, 5L, 9L);
            dlacpy_("Upper", &dim1, &dim1, &dwork[itmp1], &dim1, &f[ir + ir * f_dim1], ldf, 5L);
            if (lcmpq) {
                /*              Update Q. */
                /*              Workspace: IWRK4 + 2*N - 1. */
                dlacpy_("Full", n, &dim1, &q[ir * q_dim1 + 1], ldq, &dwork[iwrk4], n, 4L);
                dgemm_("No Transpose", "No Transpose", n, &dim1, &dim1, &c_b13, &dwork[iwrk4], n,
                    &dwork[iquple], &sdim, &c_b17, &q[ir * q_dim1 + 1], ldq, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", n, &dim1, &dim1, &c_b13,
                    &q[(m + ir) * q_dim1 + 1], ldq, &dwork[iqlole], &sdim, &c_b13,
                    &q[ir * q_dim1 + 1], ldq, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", n, &dim1, &dim1, &c_b13, &dwork[iwrk4], n,
                    &dwork[iqupri], &sdim, &c_b17, &dwork[iwrk3], n, 12L, 12L);
                dgemm_("No Transpose", "No Transpose", n, &dim1, &dim1, &c_b13,
                    &q[(m + ir) * q_dim1 + 1], ldq, &dwork[iqlori], &sdim, &c_b13, &dwork[iwrk3], n,
                    12L, 12L);
                dlacpy_("Full", n, &dim1, &dwork[iwrk3], n, &q[(m + ir) * q_dim1 + 1], ldq, 4L);
            }
        } else {
            q11 = dwork[iquple];
            q21 = dwork[iqlole];
            q12 = dwork[iqupri];
            q22 = dwork[iqlori];
            /*           Update A by transformations from the right. */
            i__2 = m - 1;
            dcopy_(&i__2, &a[ir * a_dim1 + 1], &c__1, &dwork[iwrk3], &c__1);
            i__2 = m - 1;
            dscal_(&i__2, &q11, &a[ir * a_dim1 + 1], &c__1);
            i__2 = m - 1;
            daxpy_(&i__2, &q21, &d__[ir * d_dim1 + 1], &c__1, &a[ir * a_dim1 + 1], &c__1);
            /*           Update D by transformations from the right. */
            i__2 = m - 1;
            dscal_(&i__2, &q22, &d__[ir * d_dim1 + 1], &c__1);
            i__2 = m - 1;
            daxpy_(&i__2, &q12, &dwork[iwrk3], &c__1, &d__[ir * d_dim1 + 1], &c__1);
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
            /*           Copy the relevant part of A(ib1:ib3-1,ib1:ib3-1) and */
            /*           B(ib1:ib3-1,ib1:ib3-1) to DWORK as inputs for MB03DD. */
            /*           Also, set the additional zero elements. */
            dlacpy_("Upper", &sdim, &sdim, &a[ib1 + ib1 * a_dim1], lda, &dwork[ia], &sdim, 5L);
            i__3 = sdim - 1;
            i__4 = sdim - 1;
            dlaset_("Lower", &i__3, &i__4, &c_b17, &c_b17, &dwork[ia + 1], &sdim, 5L);
            dlacpy_("Upper", &sdim, &sdim, &b[ib1 + ib1 * b_dim1], ldb, &dwork[ib], &sdim, 5L);
            i__3 = sdim - 1;
            i__4 = *ldb + 1;
            i__5 = sdim + 1;
            dcopy_(&i__3, &b[ib1 + 1 + ib1 * b_dim1], &i__4, &dwork[ib + 1], &i__5);
            i__3 = sdim - 2;
            i__4 = sdim - 2;
            dlaset_("Lower", &i__3, &i__4, &c_b17, &c_b17, &dwork[ib + 2], &sdim, 5L);
            /*           Perform eigenvalue/matrix block exchange. */
            i__3 = *ldwork - iwrk1 + 1;
            mb03dd_("Triangular", &dim1, &dim2, &prec, &dwork[ib], &sdim, &dwork[ia], &sdim,
                &dwork[iq1], &sdim, &dwork[iq2], &sdim, &dwork[iwrk1], &i__3, info, 10L);
            if (*info > 0) {
                *info = 1;
                return 0;
            }
            /*           Copy the transformed diagonal blocks, if sdim > 2. */
            nrows = ib1 - 1;
            ncols = m - ib3 + 1;
            ics = ib3;
            if (sdim > 2) {
                dlacpy_("Upper", &sdim, &sdim, &dwork[ia], &sdim, &a[ib1 + ib1 * a_dim1], lda, 5L);
                dlacpy_("Upper", &sdim, &sdim, &dwork[ib], &sdim, &b[ib1 + ib1 * b_dim1], ldb, 5L);
                i__3 = sdim - 1;
                i__4 = sdim + 1;
                i__5 = *ldb + 1;
                dcopy_(&i__3, &dwork[ib + 1], &i__4, &b[ib1 + 1 + ib1 * b_dim1], &i__5);
                nrow = nrows;
                ncol = ncols;
                ic = ics;
                ldw = max(1, nrow);
            } else {
                tmp = a[ib1 + 1 + ib1 * a_dim1];
                a[ib1 + 1 + ib1 * a_dim1] = 0.;
                nrow = ib3 - 1;
                ncol = m - ib1 + 1;
                ic = ib1;
                ldw = nrow;
            }
            /*           Update A. */
            dgemm_("No Transpose", "No Transpose", &nrow, &sdim, &sdim, &c_b13,
                &a[ib1 * a_dim1 + 1], lda, &dwork[iq1], &sdim, &c_b17, &dwork[iwrk2], &ldw, 12L,
                12L);
            dlacpy_("Full", &nrow, &sdim, &dwork[iwrk2], &ldw, &a[ib1 * a_dim1 + 1], lda, 4L);
            dgemm_("Transpose", "No Transpose", &sdim, &ncol, &sdim, &c_b13, &dwork[iq2], &sdim,
                &a[ib1 + ic * a_dim1], lda, &c_b17, &dwork[iwrk2], &sdim, 9L, 12L);
            dlacpy_("Full", &sdim, &ncol, &dwork[iwrk2], &sdim, &a[ib1 + ic * a_dim1], lda, 4L);
            if (sdim == 2) {
                a[ib1 + 1 + ib1 * a_dim1] = tmp;
            }
            /*           Update D. */
            dgemm_("No Transpose", "No Transpose", &nrows, &sdim, &sdim, &c_b13,
                &d__[ib1 * d_dim1 + 1], ldd, &dwork[iq2], &sdim, &c_b17, &dwork[iwrk2], &ldw, 12L,
                12L);
            dlacpy_("Full", &nrows, &sdim, &dwork[iwrk2], &ldw, &d__[ib1 * d_dim1 + 1], ldd, 4L);
            dgemm_("Transpose", "No Transpose", &sdim, &ncols, &sdim, &c_b13, &dwork[iq2], &sdim,
                &d__[ib1 + ics * d_dim1], ldd, &c_b17, &dwork[iwrk2], &sdim, 9L, 12L);
            dlacpy_("Full", &sdim, &ncols, &dwork[iwrk2], &sdim, &d__[ib1 + ics * d_dim1], ldd, 4L);
            i__3 = *ldwork - iwrk2 + 1;
            mb01ld_("Upper", "Transpose", &sdim, &sdim, &c_b17, &c_b13, &d__[ib1 + ib1 * d_dim1],
                ldd, &dwork[iq2], &sdim, &d__[ib1 + ib1 * d_dim1], ldd, &dwork[iwrk2], &i__3, info,
                5L, 9L);
            /*           Update B. */
            dgemm_("No Transpose", "No Transpose", &nrow, &sdim, &sdim, &c_b13,
                &b[ib1 * b_dim1 + 1], ldb, &dwork[iq1], &sdim, &c_b17, &dwork[iwrk2], &ldw, 12L,
                12L);
            dlacpy_("Full", &nrow, &sdim, &dwork[iwrk2], &ldw, &b[ib1 * b_dim1 + 1], ldb, 4L);
            dgemm_("Transpose", "No Transpose", &sdim, &ncol, &sdim, &c_b13, &dwork[iq2], &sdim,
                &b[ib1 + ic * b_dim1], ldb, &c_b17, &dwork[iwrk2], &sdim, 9L, 12L);
            dlacpy_("Full", &sdim, &ncol, &dwork[iwrk2], &sdim, &b[ib1 + ic * b_dim1], ldb, 4L);
            /*           Update F. */
            dgemm_("No Transpose", "No Transpose", &nrows, &sdim, &sdim, &c_b13,
                &f[ib1 * f_dim1 + 1], ldf, &dwork[iq2], &sdim, &c_b17, &dwork[iwrk2], &ldw, 12L,
                12L);
            dlacpy_("Full", &nrows, &sdim, &dwork[iwrk2], &ldw, &f[ib1 * f_dim1 + 1], ldf, 4L);
            dgemm_("Transpose", "No Transpose", &sdim, &ncols, &sdim, &c_b13, &dwork[iq2], &sdim,
                &f[ib1 + ics * f_dim1], ldf, &c_b17, &dwork[iwrk2], &sdim, 9L, 12L);
            dlacpy_("Full", &sdim, &ncols, &dwork[iwrk2], &sdim, &f[ib1 + ics * f_dim1], ldf, 4L);
            i__3 = *ldwork - iwrk2 + 1;
            mb01ru_("Upper", "Transpose", &sdim, &sdim, &c_b17, &c_b13, &f[ib1 + ib1 * f_dim1], ldf,
                &dwork[iq2], &sdim, &f[ib1 + ib1 * f_dim1], ldf, &dwork[iwrk2], &i__3, info, 5L,
                9L);
            i__3 = *ldf + 1;
            dscal_(&sdim, &c_b80, &f[ib1 + ib1 * f_dim1], &i__3);
            if (lcmpq) {
                /*              Update Q. */
                /*              Workspace: IWRK2 + 4*N - 1. */
                dgemm_("No Transpose", "No Transpose", n, &sdim, &sdim, &c_b13,
                    &q[ib1 * q_dim1 + 1], ldq, &dwork[iq1], &sdim, &c_b17, &dwork[iwrk2], n, 12L,
                    12L);
                dlacpy_("Full", n, &sdim, &dwork[iwrk2], n, &q[ib1 * q_dim1 + 1], ldq, 4L);
                dgemm_("No Transpose", "No Transpose", n, &sdim, &sdim, &c_b13,
                    &q[(m + ib1) * q_dim1 + 1], ldq, &dwork[iq2], &sdim, &c_b17, &dwork[iwrk2], n,
                    12L, 12L);
                dlacpy_("Full", n, &sdim, &dwork[iwrk2], n, &q[(m + ib1) * q_dim1 + 1], ldq, 4L);
            }
            /*           Update index list IWORK(1:M) if a 1-by-1 and 2-by-2 block */
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
        /*        Restore the lower triangle of the submatrix D(M-1:M,M-1:M) and */
        /*        the elements A(M,M-1) and F(M,M-1). */
        d__[m - 1 + (m - 1) * d_dim1] = d1;
        d__[m + (m - 1) * d_dim1] = d2;
        d__[m + m * d_dim1] = d3;
        a[m + (m - 1) * a_dim1] = a2;
        f[m + (m - 1) * f_dim1] = f2;
    }
    if (mm > 0) {
        *neig = iwork[mm + 1] - 1;
    } else {
        *neig = 0;
    }
    return 0;
    /* *** Last line of MB03JD *** */
} /* mb03jd_ */
