/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__1 = 1;
static doublereal c_b19 = 1.;
static doublereal c_b20 = 0.;

EXPORTSYMBOL /* Subroutine */ int ab13ed_(
    n, a, lda, low, high, tol, dwork, ldwork, info) integer* n;
doublereal* a;
integer* lda;
doublereal *low, *high, *tol, *dwork;
integer *ldwork, *info;
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1;
    /* Builtin functions */
    double sqrt();
    /* Local variables */
    static logical rneg;
    static doublereal anrm, sfmn, temp, seps;
    extern /* Subroutine */ int ma02ed_();
    static integer i__;
    extern /* Subroutine */ int dgemm_();
    static doublereal sigma;
    extern /* Subroutine */ int mb04zd_(), dcopy_(), dsymm_();
    static doublereal dummy[1];
    static integer jwork, n2;
    extern /* Subroutine */ int dsymv_();
    static doublereal dummy2[1] /* was [1][1] */;
    extern /* Subroutine */ int dgebal_();
    extern doublereal dlamch_(), dlange_();
    extern /* Subroutine */ int dlacpy_(), xerbla_(), dhseqr_();
    static integer ia2, minwrk;
    static logical sufwrk;
    static integer iaa, igf, ihi, ilo, iwi;
    static doublereal tau;
    static integer iwk, iwr;
    static doublereal tol1, tol2;
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
    /*     To estimate beta(A), the 2-norm distance from a real matrix A to */
    /*     the nearest complex matrix with an eigenvalue on the imaginary */
    /*     axis. The estimate is given as */
    /*            LOW <= beta(A) <= HIGH, */
    /*     where either */
    /*            (1 + TOL) * LOW >= HIGH, */
    /*     or */
    /*            LOW = 0   and   HIGH = delta, */
    /*     and delta is a small number approximately equal to the square root */
    /*     of machine precision times the Frobenius norm (Euclidean norm) */
    /*     of A. If A is stable in the sense that all eigenvalues of A lie */
    /*     in the open left half complex plane, then beta(A) is the distance */
    /*     to the nearest unstable complex matrix, i.e., the complex */
    /*     stability radius. */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The order of the matrix A.  N >= 0. */
    /*     A       (input) DOUBLE PRECISION array, dimension (LDA,N) */
    /*             The leading N-by-N part of this array must contain the */
    /*             matrix A. */
    /*     LDA     INTEGER */
    /*             The leading dimension of array A.  LDA >= MAX(1,N). */
    /*     LOW     (output) DOUBLE PRECISION */
    /*             A lower bound for beta(A). */
    /*     HIGH    (output) DOUBLE PRECISION */
    /*             An upper bound for beta(A). */
    /*     Tolerances */
    /*     TOL     DOUBLE PRECISION */
    /*             Specifies the accuracy with which LOW and HIGH approximate */
    /*             beta(A). If the user sets TOL to be less than SQRT(EPS), */
    /*             where EPS is the machine precision (see LAPACK Library */
    /*             Routine DLAMCH), then the tolerance is taken to be */
    /*             SQRT(EPS). */
    /*             The recommended value is TOL = 9, which gives an estimate */
    /*             of beta(A) correct to within an order of magnitude. */
    /*     Workspace */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             On exit, if INFO = 0, DWORK(1) returns the optimal value */
    /*             of LDWORK. */
    /*     LDWORK  INTEGER */
    /*             The length of the array DWORK. */
    /*             LDWORK >= MAX( 1, 3*N*(N+1) ). */
    /*             For optimum performance LDWORK should be larger. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -i, the i-th argument had an illegal */
    /*                   value; */
    /*             = 1:  the QR algorithm (LAPACK Library routine DHSEQR) */
    /*                   fails to converge; this error is very rare. */
    /*     METHOD */
    /*     Let beta(A) be the 2-norm distance from a real matrix A to the */
    /*     nearest complex matrix with an eigenvalue on the imaginary axis. */
    /*     It is known that beta(A) = minimum of the smallest singular */
    /*     value of (A - jwI), where I is the identity matrix and j**2 = -1, */
    /*     and the minimum is taken over all real w. */
    /*     The algorithm computes a lower bound LOW and an upper bound HIGH */
    /*     for beta(A) by a bisection method in the following way. Given a */
    /*     non-negative real number sigma, the Hamiltonian matrix H(sigma) */
    /*     is constructed: */
    /*                       |   A      -sigma*I |     | A   G  | */
    /*           H(sigma) =  |                   | :=  |        | . */
    /*                       | sigma*I    -A'    |     | F  -A' | */
    /*     It can be shown [1] that H(sigma) has an eigenvalue whose real */
    /*     part is zero if and only if sigma >= beta. Any lower and upper */
    /*     bounds on beta(A) can be improved by choosing a number between */
    /*     them and checking to see if H(sigma) has an eigenvalue with zero */
    /*     real part.  This decision is made by computing the eigenvalues of */
    /*     H(sigma) using the square reduced algorithm of Van Loan [2]. */
    /*     REFERENCES */
    /*     [1] Byers, R. */
    /*         A bisection method for measuring the distance of a stable */
    /*         matrix to the unstable matrices. */
    /*         SIAM J. Sci. Stat. Comput., Vol. 9, No. 5, pp. 875-880, 1988. */
    /*     [2] Van Loan, C.F. */
    /*         A symplectic method for approximating all the eigenvalues of a */
    /*         Hamiltonian matrix. */
    /*         Linear Algebra and its Applications, Vol 61, 233-251, 1984. */
    /*     NUMERICAL ASPECTS */
    /*     Due to rounding errors the computed values of LOW and HIGH can be */
    /*     proven to satisfy */
    /*            LOW - p(n) * sqrt(e) * norm(A) <= beta(A) */
    /*     and */
    /*            beta(A) <= HIGH + p(n) * sqrt(e) * norm(A), */
    /*     where p(n) is a modest polynomial of degree 3, e is the machine */
    /*     precision and norm(A) is the Frobenius norm of A, see [1]. */
    /*     The recommended value for TOL is 9 which gives an estimate of */
    /*     beta(A) correct to within an order of magnitude. */
    /*     AB13ED requires approximately 38*N**3 flops for TOL = 9. */
    /*     CONTRIBUTOR */
    /*     R. Byers, the routines BISEC and BISEC0 (January, 1995). */
    /*     REVISIONS */
    /*     Release 4.0: V. Sima, Katholieke Univ. Leuven, Belgium, Dec. 1999. */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Jan. 2003. */
    /*     KEYWORDS */
    /*     Distances, eigenvalue, eigenvalue perturbation, norms, stability */
    /*     radius. */
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
    /*     Test the input scalar arguments. */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --dwork;
    /* Function Body */
    *info = 0;
    minwrk = *n * 3 * (*n + 1);
    if (*n < 0) {
        *info = -1;
    } else if (*lda < max(1, *n)) {
        *info = -3;
    } else if (*ldwork < max(1, minwrk)) {
        *info = -8;
    }
    if (*info != 0) {
        /*        Error return. */
        i__1 = -(*info);
        xerbla_("AB13ED", &i__1, 6L);
        return 0;
    }
    /*     Quick return if possible. */
    *low = 0.;
    if (*n == 0) {
        *high = 0.;
        dwork[1] = 1.;
        return 0;
    }
    /*     Indices for splitting the work array. */
    /*     (Note: Comments in the code beginning "Workspace:" describe the */
    /*     minimal amount of real workspace needed at that point in the */
    /*     code, as well as the preferred amount for good performance.) */
    n2 = *n * *n;
    igf = 1;
    ia2 = igf + n2 + *n;
    iaa = ia2 + n2;
    iwk = iaa + n2;
    iwr = iaa;
    iwi = iwr + *n;
    sufwrk = *ldwork - iwk >= n2;
    /*     Computation of the tolerances and the treshold for termination of */
    /*     the bisection method. SEPS is the square root of the machine */
    /*     precision. */
    sfmn = dlamch_("Safe minimum", 12L);
    seps = sqrt(dlamch_("Epsilon", 7L));
    tau = max(*tol, seps) + 1.;
    anrm = dlange_("Frobenius", n, n, &a[a_offset], lda, &dwork[1], 9L);
    tol1 = seps * anrm;
    tol2 = tol1 * (doublereal)(*n << 1);
    /*     Initialization of the bisection method. */
    *high = anrm;
    /*     WHILE ( HIGH > TAU*MAX( TOL1, LOW ) ) DO */
L10:
    if (*high > tau * max(tol1, *low)) {
        sigma = sqrt(*high) * sqrt((max(tol1, *low)));
        /*        Set up H(sigma). */
        /*        Workspace: N*(N+1)+2*N*N. */
        dlacpy_("Full", n, n, &a[a_offset], lda, &dwork[iaa], n, 4L);
        dwork[igf] = sigma;
        dwork[igf + *n] = -sigma;
        dummy[0] = 0.;
        i__1 = *n - 1;
        dcopy_(&i__1, dummy, &c__0, &dwork[igf + 1], &c__1);
        i__1 = ia2 - *n - 2;
        i__2 = *n + 1;
        for (i__ = igf; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
            i__3 = *n + 1;
            dcopy_(&i__3, &dwork[i__], &c__1, &dwork[i__ + *n + 1], &c__1);
            /* L20: */
        }
        /*        Computation of the eigenvalues by the square reduced algorithm. */
        /*        Workspace: N*(N+1)+2*N*N+2*N. */
        mb04zd_(
            "No vectors", n, &dwork[iaa], n, &dwork[igf], n, dummy2, &c__1, &dwork[iwk], info, 10L);
        /*        Form the matrix A*A + F*G. */
        /*        Workspace: need   N*(N+1)+2*N*N+N; */
        /*                   prefer N*(N+1)+3*N*N. */
        jwork = ia2;
        if (sufwrk) {
            jwork = iwk;
        }
        dlacpy_("Lower", n, n, &dwork[igf], n, &dwork[jwork], n, 5L);
        ma02ed_("Lower", n, &dwork[jwork], n, 5L);
        if (sufwrk) {
            /*           Use BLAS 3 calculation. */
            dsymm_("Left", "Upper", n, n, &c_b19, &dwork[igf + *n], n, &dwork[jwork], n, &c_b20,
                &dwork[ia2], n, 4L, 5L);
        } else {
            /*           Use BLAS 2 calculation. */
            i__2 = *n;
            for (i__ = 1; i__ <= i__2; ++i__) {
                dsymv_("Upper", n, &c_b19, &dwork[igf + *n], n, &dwork[ia2 + *n * (i__ - 1)], &c__1,
                    &c_b20, &dwork[iwk], &c__1, 5L);
                dcopy_(n, &dwork[iwk], &c__1, &dwork[ia2 + *n * (i__ - 1)], &c__1);
                /* L30: */
            }
        }
        dgemm_("NoTranspose", "NoTranspose", n, n, n, &c_b19, &dwork[iaa], n, &dwork[iaa], n,
            &c_b19, &dwork[ia2], n, 11L, 11L);
        /*        Find the eigenvalues of A*A + F*G. */
        /*        Workspace: N*(N+1)+N*N+3*N. */
        jwork = iwi + *n;
        dgebal_("Scale", n, &dwork[ia2], n, &ilo, &ihi, &dwork[jwork], &i__, 5L);
        dhseqr_("Eigenvalues", "NoSchurVectors", n, &ilo, &ihi, &dwork[ia2], n, &dwork[iwr],
            &dwork[iwi], dummy2, &c__1, &dwork[jwork], n, info, 11L, 14L);
        if (*info != 0) {
            *info = 1;
            return 0;
        }
        /*        (DWORK(IWR+i), DWORK(IWI+i)), i = 0,...,N-1, contain the */
        /*        squares of the eigenvalues of H(sigma). */
        i__ = 0;
        rneg = FALSE_;
        /*        WHILE ( ( DWORK(IWR+i),DWORK(IWI+i) ) not real positive */
        /*                .AND. I < N ) DO */
    L40:
        if (!rneg && i__ < *n) {
            temp = (d__1 = dwork[iwi + i__], abs(d__1));
            if (tol1 > sfmn) {
                temp /= tol1;
            }
            rneg = dwork[iwr + i__] < 0. && temp <= tol2;
            ++i__;
            goto L40;
            /*           END WHILE 40 */
        }
        if (rneg) {
            *high = sigma;
        } else {
            *low = sigma;
        }
        goto L10;
        /*        END WHILE 10 */
    }
    /*     Set optimal workspace dimension. */
    /* Computing MAX */
    i__2 = (n2 << 2) + *n;
    dwork[1] = (doublereal)max(i__2, minwrk);
    /* *** Last line of AB13ED *** */
} /* ab13ed_ */
