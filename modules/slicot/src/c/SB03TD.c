/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static doublereal c_b23 = 0.;
static doublereal c_b24 = 1.;
static doublereal c_b25 = .5;

EXPORTSYMBOL /* Subroutine */ int sb03td_(job, fact, trana, uplo, lyapun, n, scale, a, lda, t, ldt,
    u, ldu, c__, ldc, x, ldx, sep, rcond, ferr, wr, wi, iwork, dwork, ldwork, info, job_len,
    fact_len, trana_len, uplo_len, lyapun_len) char *job,
    *fact, *trana, *uplo, *lyapun;
integer* n;
doublereal *scale, *a;
integer* lda;
doublereal* t;
integer* ldt;
doublereal* u;
integer* ldu;
doublereal* c__;
integer* ldc;
doublereal* x;
integer* ldx;
doublereal *sep, *rcond, *ferr, *wr, *wi;
integer* iwork;
doublereal* dwork;
integer *ldwork, *info;
ftnlen job_len;
ftnlen fact_len;
ftnlen trana_len;
ftnlen uplo_len;
ftnlen lyapun_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, c_dim1, c_offset, t_dim1, t_offset, u_dim1, u_offset, x_dim1,
        x_offset, i__1, i__2;
    /* Local variables */
    static logical joba, jobc, jobe;
    static char jobl[1];
    static integer sdim;
    static logical jobs;
    static char sjob[1];
    static logical jobx;
    extern /* Subroutine */ int ma02ed_();
    static char cfact[1];
    extern /* Subroutine */ int dscal_(), dgees_(), sb03qd_();
    extern logical lsame_();
    extern /* Subroutine */ int mb01ru_(), sb03my_(), sb03qy_();
    static logical bwork[1], lower;
    static integer nn;
    static logical nofact;
    extern /* Subroutine */ int dlacpy_(), xerbla_();
    extern logical select_();
    static logical update, notrna;
    static doublereal thnorm;
    static integer ldw;
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
    /*     To solve the real continuous-time Lyapunov matrix equation */
    /*            op(A)'*X + X*op(A) = scale*C, */
    /*     estimate the conditioning, and compute an error bound on the */
    /*     solution X, where op(A) = A or A' (A**T), the matrix A is N-by-N, */
    /*     the right hand side C and the solution X are N-by-N symmetric */
    /*     matrices (C = C', X = X'), and scale is an output scale factor, */
    /*     set less than or equal to 1 to avoid overflow in X. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     JOB     CHARACTER*1 */
    /*             Specifies the computation to be performed, as follows: */
    /*             = 'X':  Compute the solution only; */
    /*             = 'S':  Compute the separation only; */
    /*             = 'C':  Compute the reciprocal condition number only; */
    /*             = 'E':  Compute the error bound only; */
    /*             = 'A':  Compute all: the solution, separation, reciprocal */
    /*                     condition number, and the error bound. */
    /*     FACT    CHARACTER*1 */
    /*             Specifies whether or not the real Schur factorization */
    /*             of the matrix A is supplied on entry, as follows: */
    /*             = 'F':  On entry, T and U (if LYAPUN = 'O') contain the */
    /*                     factors from the real Schur factorization of the */
    /*                     matrix A; */
    /*             = 'N':  The Schur factorization of A will be computed */
    /*                     and the factors will be stored in T and U (if */
    /*                     LYAPUN = 'O'). */
    /*     TRANA   CHARACTER*1 */
    /*             Specifies the form of op(A) to be used, as follows: */
    /*             = 'N':  op(A) = A    (No transpose); */
    /*             = 'T':  op(A) = A**T (Transpose); */
    /*             = 'C':  op(A) = A**T (Conjugate transpose = Transpose). */
    /*     UPLO    CHARACTER*1 */
    /*             Specifies which part of the symmetric matrix C is to be */
    /*             used, as follows: */
    /*             = 'U':  Upper triangular part; */
    /*             = 'L':  Lower triangular part. */
    /*     LYAPUN  CHARACTER*1 */
    /*             Specifies whether or not the original or "reduced" */
    /*             Lyapunov equations should be solved, as follows: */
    /*             = 'O':  Solve the original Lyapunov equations, updating */
    /*                     the right-hand sides and solutions with the */
    /*                     matrix U, e.g., X <-- U'*X*U; */
    /*             = 'R':  Solve reduced Lyapunov equations only, without */
    /*                     updating the right-hand sides and solutions. */
    /*                     This means that a real Schur form T of A appears */
    /*                     in the equation, instead of A. */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the matrices A, X, and C.  N >= 0. */
    /*     SCALE   (input or output) DOUBLE PRECISION */
    /*             If JOB = 'C' or JOB = 'E', SCALE is an input argument: */
    /*             the scale factor, set by a Lyapunov solver. */
    /*             0 <= SCALE <= 1. */
    /*             If JOB = 'X' or JOB = 'A', SCALE is an output argument: */
    /*             the scale factor, scale, set less than or equal to 1 to */
    /*             prevent the solution overflowing. */
    /*             If JOB = 'S', this argument is not used. */
    /*     A       (input) DOUBLE PRECISION array, dimension (LDA,N) */
    /*             If FACT = 'N' or (LYAPUN = 'O' and JOB <> 'X'), the */
    /*             leading N-by-N part of this array must contain the */
    /*             original matrix A. */
    /*             If FACT = 'F' and (LYAPUN = 'R' or JOB = 'X'), A is */
    /*             not referenced. */
    /*     LDA     INTEGER */
    /*             The leading dimension of the array A. */
    /*             LDA >= MAX(1,N), if FACT = 'N' or LYAPUN = 'O' and */
    /*                                               JOB <> 'X'; */
    /*             LDA >= 1,        otherwise. */
    /*     T       (input/output) DOUBLE PRECISION array, dimension */
    /*             (LDT,N) */
    /*             If FACT = 'F', then on entry the leading N-by-N upper */
    /*             Hessenberg part of this array must contain the upper */
    /*             quasi-triangular matrix T in Schur canonical form from a */
    /*             Schur factorization of A. */
    /*             If FACT = 'N', then this array need not be set on input. */
    /*             On exit, (if INFO = 0 or INFO = N+1, for FACT = 'N') the */
    /*             leading N-by-N upper Hessenberg part of this array */
    /*             contains the upper quasi-triangular matrix T in Schur */
    /*             canonical form from a Schur factorization of A. */
    /*             The contents of array T is not modified if FACT = 'F'. */
    /*     LDT     INTEGER */
    /*             The leading dimension of the array T.  LDT >= MAX(1,N). */
    /*     U       (input or output) DOUBLE PRECISION array, dimension */
    /*             (LDU,N) */
    /*             If LYAPUN = 'O' and FACT = 'F', then U is an input */
    /*             argument and on entry, the leading N-by-N part of this */
    /*             array must contain the orthogonal matrix U from a real */
    /*             Schur factorization of A. */
    /*             If LYAPUN = 'O' and FACT = 'N', then U is an output */
    /*             argument and on exit, if INFO = 0 or INFO = N+1, it */
    /*             contains the orthogonal N-by-N matrix from a real Schur */
    /*             factorization of A. */
    /*             If LYAPUN = 'R', the array U is not referenced. */
    /*     LDU     INTEGER */
    /*             The leading dimension of the array U. */
    /*             LDU >= 1,        if LYAPUN = 'R'; */
    /*             LDU >= MAX(1,N), if LYAPUN = 'O'. */
    /*     C       (input) DOUBLE PRECISION array, dimension (LDC,N) */
    /*             If JOB <> 'S' and UPLO = 'U', the leading N-by-N upper */
    /*             triangular part of this array must contain the upper */
    /*             triangular part of the matrix C of the original Lyapunov */
    /*             equation (with matrix A), if LYAPUN = 'O', or of the */
    /*             reduced Lyapunov equation (with matrix T), if */
    /*             LYAPUN = 'R'. */
    /*             If JOB <> 'S' and UPLO = 'L', the leading N-by-N lower */
    /*             triangular part of this array must contain the lower */
    /*             triangular part of the matrix C of the original Lyapunov */
    /*             equation (with matrix A), if LYAPUN = 'O', or of the */
    /*             reduced Lyapunov equation (with matrix T), if */
    /*             LYAPUN = 'R'. */
    /*             The remaining strictly triangular part of this array is */
    /*             used as workspace. */
    /*             If JOB = 'X', then this array may be identified with X */
    /*             in the call of this routine. */
    /*             If JOB = 'S', the array C is not referenced. */
    /*     LDC     INTEGER */
    /*             The leading dimension of the array C. */
    /*             LDC >= 1,        if JOB = 'S'; */
    /*             LDC >= MAX(1,N), otherwise. */
    /*     X       (input or output) DOUBLE PRECISION array, dimension */
    /*             (LDX,N) */
    /*             If JOB = 'C' or 'E', then X is an input argument and on */
    /*             entry, the leading N-by-N part of this array must contain */
    /*             the symmetric solution matrix X of the original Lyapunov */
    /*             equation (with matrix A), if LYAPUN = 'O', or of the */
    /*             reduced Lyapunov equation (with matrix T), if */
    /*             LYAPUN = 'R'. */
    /*             If JOB = 'X' or 'A', then X is an output argument and on */
    /*             exit, if INFO = 0 or INFO = N+1, the leading N-by-N part */
    /*             of this array contains the symmetric solution matrix X of */
    /*             of the original Lyapunov equation (with matrix A), if */
    /*             LYAPUN = 'O', or of the reduced Lyapunov equation (with */
    /*             matrix T), if LYAPUN = 'R'. */
    /*             If JOB = 'S', the array X is not referenced. */
    /*     LDX     INTEGER */
    /*             The leading dimension of the array X. */
    /*             LDX >= 1,        if JOB = 'S'; */
    /*             LDX >= MAX(1,N), otherwise. */
    /*     SEP     (output) DOUBLE PRECISION */
    /*             If JOB = 'S' or JOB = 'C' or JOB = 'A', and INFO = 0 or */
    /*             INFO = N+1, SEP contains the estimated separation of the */
    /*             matrices op(A) and -op(A)', sep(op(A),-op(A)'). */
    /*             If N = 0, or X = 0, or JOB = 'X' or JOB = 'E', SEP is not */
    /*             referenced. */
    /*     RCOND   (output) DOUBLE PRECISION */
    /*             If JOB = 'C' or JOB = 'A', an estimate of the reciprocal */
    /*             condition number of the continuous-time Lyapunov equation. */
    /*             If N = 0 or X = 0, RCOND is set to 1 or 0, respectively. */
    /*             If JOB = 'X' or JOB = 'S' or JOB = 'E', RCOND is not */
    /*             referenced. */
    /*     FERR    (output) DOUBLE PRECISION */
    /*             If JOB = 'E' or JOB = 'A', and INFO = 0 or INFO = N+1, */
    /*             FERR contains an estimated forward error bound for the */
    /*             solution X. If XTRUE is the true solution, FERR bounds the */
    /*             relative error in the computed solution, measured in the */
    /*             Frobenius norm:  norm(X - XTRUE)/norm(XTRUE). */
    /*             If N = 0 or X = 0, FERR is set to 0. */
    /*             If JOB = 'X' or JOB = 'S' or JOB = 'C', FERR is not */
    /*             referenced. */
    /*     WR      (output) DOUBLE PRECISION array, dimension (N) */
    /*     WI      (output) DOUBLE PRECISION array, dimension (N) */
    /*             If FACT = 'N', and INFO = 0 or INFO = N+1, WR and WI */
    /*             contain the real and imaginary parts, respectively, of the */
    /*             eigenvalues of A. */
    /*             If FACT = 'F', WR and WI are not referenced. */
    /*     Workspace */
    /*     IWORK   INTEGER array, dimension (N*N) */
    /*             This array is not referenced if JOB = 'X'. */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             On exit, if INFO = 0 or INFO = N+1, DWORK(1) returns the */
    /*             optimal value of LDWORK. */
    /*     LDWORK  INTEGER */
    /*             The length of the array DWORK. */
    /*             If JOB = 'X', then */
    /*             LDWORK >= MAX(1,N*N),           if FACT = 'F'; */
    /*             LDWORK >= MAX(1,MAX(N*N,3*N)),  if FACT = 'N'. */
    /*             If JOB = 'S' or JOB = 'C', then */
    /*             LDWORK >= MAX(1,2*N*N),         if FACT = 'F'; */
    /*             LDWORK >= MAX(1,2*N*N,3*N),     if FACT = 'N'. */
    /*             If JOB = 'E', or JOB = 'A', and LYAPUN  = 'O', then */
    /*             LDWORK >= MAX(1,3*N*N); */
    /*             If JOB = 'E', or JOB = 'A', and LYAPUN  = 'R', then */
    /*             LDWORK >= MAX(1,3*N*N+N-1). */
    /*             For optimum performance LDWORK should sometimes be larger. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -i, the i-th argument had an illegal */
    /*                   value; */
    /*             > 0:  if INFO = i, i <= N, the QR algorithm failed to */
    /*                   complete the reduction to Schur canonical form (see */
    /*                   LAPACK Library routine DGEES); on exit, the matrix */
    /*                   T(i+1:N,i+1:N) contains the partially converged */
    /*                   Schur form, and the elements i+1:n of WR and WI */
    /*                   contain the real and imaginary parts, respectively, */
    /*                   of the converged eigenvalues; this error is unlikely */
    /*                   to appear; */
    /*             = N+1:  if the matrices T and -T' have common or very */
    /*                   close eigenvalues; perturbed values were used to */
    /*                   solve Lyapunov equations, but the matrix T, if given */
    /*                   (for FACT = 'F'), is unchanged. */
    /*     METHOD */
    /*     After reducing matrix A to real Schur canonical form (if needed), */
    /*     the Bartels-Stewart algorithm is used. A set of equivalent linear */
    /*     algebraic systems of equations of order at most four are formed */
    /*     and solved using Gaussian elimination with complete pivoting. */
    /*     The condition number of the continuous-time Lyapunov equation is */
    /*     estimated as */
    /*     cond = (norm(Theta)*norm(A) + norm(inv(Omega))*norm(C))/norm(X), */
    /*     where Omega and Theta are linear operators defined by */
    /*     Omega(W) = op(A)'*W + W*op(A), */
    /*     Theta(W) = inv(Omega(op(W)'*X + X*op(W))). */
    /*     The routine estimates the quantities */
    /*     sep(op(A),-op(A)') = 1 / norm(inv(Omega)) */
    /*     and norm(Theta) using 1-norm condition estimators. */
    /*     The forward error bound is estimated using a practical error bound */
    /*     similar to the one proposed in [2]. */
    /*     REFERENCES */
    /*     [1] Bartels, R.H. and Stewart, G.W.  T */
    /*         Solution of the matrix equation A X + XB = C. */
    /*         Comm. A.C.M., 15, pp. 820-826, 1972. */
    /*     [2] Higham, N.J. */
    /*         Perturbation theory and backward error for AX-XB=C. */
    /*         BIT, vol. 33, pp. 124-136, 1993. */
    /*     NUMERICAL ASPECTS */
    /*                               3 */
    /*     The algorithm requires 0(N ) operations. */
    /*     The accuracy of the estimates obtained depends on the solution */
    /*     accuracy and on the properties of the 1-norm estimator. */
    /*     FURTHER COMMENTS */
    /*     The separation of op(A) and -op(A)' can also be defined as */
    /*            sep( op(A), -op(A)' ) = sigma_min( T ), */
    /*     where sigma_min(T) is the smallest singular value of the */
    /*     N*N-by-N*N matrix */
    /*        T = kprod( I(N), op(A)' ) + kprod( op(A)', I(N) ). */
    /*     I(N) is an N-by-N identity matrix, and kprod denotes the Kronecker */
    /*     product. The routine estimates sigma_min(T) by the reciprocal of */
    /*     an estimate of the 1-norm of inverse(T). The true reciprocal */
    /*     1-norm of inverse(T) cannot differ from sigma_min(T) by more */
    /*     than a factor of N. */
    /*     CONTRIBUTOR */
    /*     V. Sima, Katholieke Univ. Leuven, Belgium, February 1999. */
    /*     This is an extended and improved version of Release 3.0 routine */
    /*     SB03RD. */
    /*     REVISIONS */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Oct. 2004. */
    /*     KEYWORDS */
    /*     Lyapunov equation, orthogonal transformation, real Schur form. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. */
    /*     .. Scalar Arguments .. */
    /*     .. */
    /*     .. Array Arguments .. */
    /*     .. */
    /*     .. Local Scalars .. */
    /*     .. */
    /*     .. Local Arrays .. */
    /*     .. */
    /*     .. External Functions .. */
    /*     .. */
    /*     .. External Subroutines .. */
    /*     .. */
    /*     .. Intrinsic Functions .. */
    /*     .. */
    /*     .. Executable Statements .. */
    /*     Decode option parameters. */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    t_dim1 = *ldt;
    t_offset = t_dim1 + 1;
    t -= t_offset;
    u_dim1 = *ldu;
    u_offset = u_dim1 + 1;
    u -= u_offset;
    c_dim1 = *ldc;
    c_offset = c_dim1 + 1;
    c__ -= c_offset;
    x_dim1 = *ldx;
    x_offset = x_dim1 + 1;
    x -= x_offset;
    --wr;
    --wi;
    --iwork;
    --dwork;
    /* Function Body */
    jobx = lsame_(job, "X", 1L, 1L);
    jobs = lsame_(job, "S", 1L, 1L);
    jobc = lsame_(job, "C", 1L, 1L);
    jobe = lsame_(job, "E", 1L, 1L);
    joba = lsame_(job, "A", 1L, 1L);
    nofact = lsame_(fact, "N", 1L, 1L);
    notrna = lsame_(trana, "N", 1L, 1L);
    lower = lsame_(uplo, "L", 1L, 1L);
    update = lsame_(lyapun, "O", 1L, 1L);
    /*     Compute workspace. */
    nn = *n * *n;
    if (jobx) {
        ldw = nn;
    } else if (jobs || jobc) {
        ldw = nn << 1;
    } else {
        ldw = nn * 3;
    }
    if ((jobe || joba) && !update) {
        ldw = ldw + *n - 1;
    }
    if (nofact) {
        /* Computing MAX */
        i__1 = ldw, i__2 = *n * 3;
        ldw = max(i__1, i__2);
    }
    /*     Test the scalar input parameters. */
    *info = 0;
    if (!(jobx || jobs || jobc || jobe || joba)) {
        *info = -1;
    } else if (!(nofact || lsame_(fact, "F", 1L, 1L))) {
        *info = -2;
    } else if (!(notrna || lsame_(trana, "T", 1L, 1L) || lsame_(trana, "C", 1L, 1L))) {
        *info = -3;
    } else if (!(lower || lsame_(uplo, "U", 1L, 1L))) {
        *info = -4;
    } else if (!(update || lsame_(lyapun, "R", 1L, 1L))) {
        *info = -5;
    } else if (*n < 0) {
        *info = -6;
    } else if ((jobc || jobe) && (*scale < 0. || *scale > 1.)) {
        *info = -7;
    } else if (*lda < 1 || *lda < *n && (update && !jobx || nofact)) {
        *info = -9;
    } else if (*ldt < max(1, *n)) {
        *info = -11;
    } else if (*ldu < 1 || *ldu < *n && update) {
        *info = -13;
    } else if (*ldc < 1 || !jobs && *ldc < *n) {
        *info = -15;
    } else if (*ldx < 1 || !jobs && *ldx < *n) {
        *info = -17;
    } else if (*ldwork < 1 || *ldwork < ldw) {
        *info = -25;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("SB03TD", &i__1, 6L);
        return 0;
    }
    /*     Quick return if possible. */
    if (*n == 0) {
        if (jobx || joba) {
            *scale = 1.;
        }
        if (jobc || joba) {
            *rcond = 1.;
        }
        if (jobe || joba) {
            *ferr = 0.;
        }
        dwork[1] = 1.;
        return 0;
    }
    if (nofact) {
        /*        Compute the Schur factorization of A. */
        /*        Workspace:  need   3*N; */
        /*                    prefer larger. */
        dlacpy_("Full", n, n, &a[a_offset], lda, &t[t_offset], ldt, 4L);
        if (update) {
            *(unsigned char*)sjob = 'V';
        } else {
            *(unsigned char*)sjob = 'N';
        }
        dgees_(sjob, "Not ordered", select_, n, &t[t_offset], ldt, &sdim, &wr[1], &wi[1],
            &u[u_offset], ldu, &dwork[1], ldwork, bwork, info, 1L, 11L);
        if (*info > 0) {
            return 0;
        }
        *(unsigned char*)cfact = 'F';
    } else {
        *(unsigned char*)cfact = *(unsigned char*)fact;
    }
    if (jobx || joba) {
        /*        Copy the right-hand side in X. */
        dlacpy_(uplo, n, n, &c__[c_offset], ldc, &x[x_offset], ldx, 1L);
        if (update) {
            /*           Transform the right-hand side. */
            /*           Workspace:  need   N*N. */
            mb01ru_(uplo, "Transpose", n, n, &c_b23, &c_b24, &x[x_offset], ldx, &u[u_offset], ldu,
                &x[x_offset], ldx, &dwork[1], ldwork, info, 1L, 9L);
            i__1 = *ldx + 1;
            dscal_(n, &c_b25, &x[x_offset], &i__1);
        }
        /*        Fill in the remaining triangle of X. */
        ma02ed_(uplo, n, &x[x_offset], ldx, 1L);
        /*        Solve the transformed equation. */
        sb03my_(trana, n, &t[t_offset], ldt, &x[x_offset], ldx, scale, info, 1L);
        if (*info > 0) {
            *info = *n + 1;
        }
        if (update) {
            /*           Transform back the solution. */
            mb01ru_(uplo, "No transpose", n, n, &c_b23, &c_b24, &x[x_offset], ldx, &u[u_offset],
                ldu, &x[x_offset], ldx, &dwork[1], ldwork, info, 1L, 12L);
            i__1 = *ldx + 1;
            dscal_(n, &c_b25, &x[x_offset], &i__1);
            /*           Fill in the remaining triangle of X. */
            ma02ed_(uplo, n, &x[x_offset], ldx, 1L);
        }
    }
    if (jobs) {
        /*        Estimate sep(op(A),-op(A)'). */
        /*        Workspace:  2*N*N. */
        sb03qy_("Separation", trana, lyapun, n, &t[t_offset], ldt, &u[u_offset], ldu, &x[x_offset],
            ldx, sep, &thnorm, &iwork[1], &dwork[1], ldwork, info, 10L, 1L, 1L);
    } else if (!jobx) {
        /*        Estimate the reciprocal condition and/or the error bound. */
        /*        Workspace:  2*N*N, if JOB = 'C'; */
        /*                    3*N*N + a*(N-1), where: */
        /*                    a = 1, if JOB = 'E' or JOB = 'A', and LYAPUN = 'R'; */
        /*                    a = 0, otherwise. */
        if (joba) {
            *(unsigned char*)jobl = 'B';
        } else {
            *(unsigned char*)jobl = *(unsigned char*)job;
        }
        sb03qd_(jobl, cfact, trana, uplo, lyapun, n, scale, &a[a_offset], lda, &t[t_offset], ldt,
            &u[u_offset], ldu, &c__[c_offset], ldc, &x[x_offset], ldx, sep, rcond, ferr, &iwork[1],
            &dwork[1], ldwork, info, 1L, 1L, 1L, 1L, 1L);
        /* Computing MAX */
        i__1 = ldw, i__2 = (integer)dwork[1];
        ldw = max(i__1, i__2);
    }
    dwork[1] = (doublereal)ldw;
    return 0;
    /* *** Last line of SB03TD *** */
} /* sb03td_ */
