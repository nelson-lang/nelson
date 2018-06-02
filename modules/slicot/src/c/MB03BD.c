/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static doublereal c_b14 = 0.;
static doublereal c_b15 = 1.;
static integer c__1 = 1;
static integer c__2 = 2;
static integer c__3 = 3;

EXPORTSYMBOL /* Subroutine */ int mb03bd_(job, defl, compq, qind, k, n, h__, ilo, ihi, s, a, lda1,
    lda2, q, ldq1, ldq2, alphar, alphai, beta, scal, iwork, liwork, dwork, ldwork, iwarn, info,
    job_len, defl_len, compq_len) char *job,
    *defl, *compq;
integer *qind, *k, *n, *h__, *ilo, *ihi, *s;
doublereal* a;
integer *lda1, *lda2;
doublereal* q;
integer *ldq1, *ldq2;
doublereal *alphar, *alphai, *beta;
integer *scal, *iwork, *liwork;
doublereal* dwork;
integer *ldwork, *iwarn, *info;
ftnlen job_len;
ftnlen defl_len;
ftnlen compq_len;
{
    /* System generated locals */
    integer a_dim1, a_dim2, a_offset, q_dim1, q_dim2, q_offset, i__1, i__2, i__3, i__4;
    doublereal d__1, d__2, d__3, d__4, d__5, d__6;
    /* Builtin functions */
    double log();
    /* Local variables */
    static integer jdef, aind, ldef;
    static doublereal base;
    static integer mapa, mapq, ierr, ntra;
    static doublereal temp;
    static logical lsvd;
    extern /* Subroutine */ int drot_();
    static integer sinv;
    static doublereal temp2, temp3;
    extern /* Subroutine */ int ma01bd_(), mb03ba_(), mb03ad_();
    static integer i__, j;
    static logical adefl;
    static integer l;
    extern /* Subroutine */ int mb03bb_(), mb03bc_(), mb03be_();
    static doublereal lgbas;
    static integer iseed[4], pfree;
    extern logical lsame_();
    static logical lschr, lcmpq, liniq;
    static integer iiter;
    static logical lparq;
    static integer ilast, jiter, maxit, titer, j1, optdw, ziter, pnorm, optiw;
    extern /* Subroutine */ int dlabad_();
    static doublereal cs;
    static integer in;
    extern doublereal dlamch_();
    static integer lm, qi;
    static doublereal sn, macpar[5];
    extern doublereal dlanhs_();
    extern /* Subroutine */ int dlaset_();
    static doublereal safmin;
    extern /* Subroutine */ int dlartg_();
    static doublereal safmax;
    extern /* Subroutine */ int xerbla_(), dlarnv_();
    static integer ilastm, ifirst, ifrstm;
    static doublereal cs1, cs2, smlnum, sn1, sn2;
    static integer jlo, pdw;
    static doublereal tol, ulp;
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
    /*     To find the eigenvalues of the generalized matrix product */
    /*                  S(1)           S(2)                 S(K) */
    /*          A(:,:,1)     * A(:,:,2)     * ... * A(:,:,K) */
    /*     where A(:,:,H) is upper Hessenberg and A(:,:,i), i <> H, is upper */
    /*     triangular, using a double-shift version of the periodic */
    /*     QZ method. In addition, A may be reduced to periodic Schur form: */
    /*     A(:,:,H) is upper quasi-triangular and all the other factors */
    /*     A(:,:,I) are upper triangular. Optionally, the 2-by-2 triangular */
    /*     matrices corresponding to 2-by-2 diagonal blocks in A(:,:,H) */
    /*     are so reduced that their product is a 2-by-2 diagonal matrix. */
    /*     If COMPQ = 'U' or COMPQ = 'I', then the orthogonal factors are */
    /*     computed and stored in the array Q so that for S(I) = 1, */
    /*                         T */
    /*             Q(:,:,I)(in)   A(:,:,I)(in)   Q(:,:,MOD(I,K)+1)(in) */
    /*                          T                                        (1) */
    /*         =   Q(:,:,I)(out)  A(:,:,I)(out)  Q(:,:,MOD(I,K)+1)(out), */
    /*     and for S(I) = -1, */
    /*                                  T */
    /*             Q(:,:,MOD(I,K)+1)(in)   A(:,:,I)(in)   Q(:,:,I)(in) */
    /*                                   T                               (2) */
    /*         =   Q(:,:,MOD(I,K)+1)(out)  A(:,:,I)(out)  Q(:,:,I)(out). */
    /*     A partial generation of the orthogonal factors can be realized */
    /*     via the array QIND. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     JOB     CHARACTER*1 */
    /*             Specifies the computation to be performed, as follows: */
    /*             = 'E': compute the eigenvalues only; A will not */
    /*                    necessarily be put into periodic Schur form; */
    /*             = 'S': put A into periodic Schur form, and return the */
    /*                    eigenvalues in ALPHAR, ALPHAI, BETA, and SCAL; */
    /*             = 'T': as JOB = 'S', but A is put into standardized */
    /*                    periodic Schur form, that is, the general product */
    /*                    of the 2-by-2 triangular matrices corresponding to */
    /*                    a complex eigenvalue is diagonal. */
    /*     DEFL    CHARACTER*1 */
    /*             Specifies the deflation strategy to be used, as follows: */
    /*             = 'C': apply a careful deflation strategy, that is, */
    /*                    the criteria are based on the magnitudes of */
    /*                    neighboring elements and infinite eigenvalues are */
    /*                    only deflated at the top; this is the recommended */
    /*                    option; */
    /*             = 'A': apply a more aggressive strategy, that is, */
    /*                    elements on the subdiagonal or diagonal are set */
    /*                    to zero as soon as they become smaller in magnitude */
    /*                    than eps times the norm of the corresponding */
    /*                    factor; this option is only recommended if */
    /*                    balancing is applied beforehand and convergence */
    /*                    problems are observed. */
    /*     COMPQ   CHARACTER*1 */
    /*             Specifies whether or not the orthogonal transformations */
    /*             should be accumulated in the array Q, as follows: */
    /*             = 'N': do not modify Q; */
    /*             = 'U': modify (update) the array Q by the orthogonal */
    /*                    transformations that are applied to the matrices in */
    /*                    the array A to reduce them to periodic Schur form; */
    /*             = 'I': like COMPQ = 'U', except that each matrix in the */
    /*                    array Q will be first initialized to the identity */
    /*                    matrix; */
    /*             = 'P': use the parameters as encoded in QIND. */
    /*     QIND    INTEGER array, dimension (K) */
    /*             If COMPQ = 'P', then this array describes the generation */
    /*             of the orthogonal factors as follows: */
    /*                If QIND(I) > 0, then the array Q(:,:,QIND(I)) is */
    /*             modified by the transformations corresponding to the */
    /*             i-th orthogonal factor in (1) and (2). */
    /*                If QIND(I) < 0, then the array Q(:,:,-QIND(I)) is */
    /*             initialized to the identity and modified by the */
    /*             transformations corresponding to the i-th orthogonal */
    /*             factor in (1) and (2). */
    /*                If QIND(I) = 0, then the transformations corresponding */
    /*             to the i-th orthogonal factor in (1), (2) are not applied. */
    /*     Input/Output Parameters */
    /*     K       (input)  INTEGER */
    /*             The number of factors.  K >= 1. */
    /*     N       (input)  INTEGER */
    /*             The order of each factor in the array A.  N >= 0. */
    /*     H       (input)  INTEGER */
    /*             Hessenberg index. The factor A(:,:,H) is on entry in upper */
    /*             Hessenberg form.  1 <= H <= K. */
    /*     ILO     (input)  INTEGER */
    /*     IHI     (input)  INTEGER */
    /*             It is assumed that each factor in A is already upper */
    /*             triangular in rows and columns 1:ILO-1 and IHI+1:N. */
    /*             1 <= ILO <= IHI <= N, if N > 0; */
    /*             ILO = 1 and IHI  = 0, if N = 0. */
    /*     S       (input)  INTEGER array, dimension (K) */
    /*             The leading K elements of this array must contain the */
    /*             signatures of the factors. Each entry in S must be either */
    /*             1 or -1. */
    /*     A       (input/output)  DOUBLE PRECISION array, dimension */
    /*                             (LDA1,LDA2,K) */
    /*             On entry, the leading N-by-N-by-K part of this array */
    /*             must contain the factors in upper Hessenberg-triangular */
    /*             form, that is, A(:,:,H) is upper Hessenberg and the other */
    /*             factors are upper triangular. */
    /*             On exit, if JOB = 'S' and INFO = 0, the leading */
    /*             N-by-N-by-K part of this array contains the factors of */
    /*             A in periodic Schur form, that is, A(:,:,H) is upper quasi */
    /*             triangular and the other factors are upper triangular. */
    /*             On exit, if JOB = 'T' and INFO = 0, the leading */
    /*             N-by-N-by-K part of this array contains the factors of */
    /*             A as for the option JOB = 'S', but the product of the */
    /*             triangular factors corresponding to a 2-by-2 block in */
    /*             A(:,:,H) is diagonal. */
    /*             On exit, if JOB = 'E', then the leading N-by-N-by-K part */
    /*             of this array contains meaningless elements. */
    /*     LDA1    INTEGER */
    /*             The first leading dimension of the array A. */
    /*             LDA1 >= MAX(1,N). */
    /*     LDA2    INTEGER */
    /*             The second leading dimension of the array A. */
    /*             LDA2 >= MAX(1,N). */
    /*     Q       (input/output)  DOUBLE PRECISION array, dimension */
    /*                             (LDQ1,LDQ2,K) */
    /*             On entry, if COMPQ = 'U', the leading N-by-N-by-K part */
    /*             of this array must contain the initial orthogonal factors */
    /*             as described in (1) and (2). */
    /*             On entry, if COMPQ = 'P', only parts of the leading */
    /*             N-by-N-by-K part of this array must contain some */
    /*             orthogonal factors as described by the parameters QIND. */
    /*             If COMPQ = 'I', this array should not set on entry. */
    /*             On exit, if COMPQ = 'U' or COMPQ = 'I', the leading */
    /*             N-by-N-by-K part of this array contains the modified */
    /*             orthogonal factors as described in (1) and (2). */
    /*             On exit, if COMPQ = 'P', only parts of the leading */
    /*             N-by-N-by-K part contain some modified orthogonal factors */
    /*             as described by the parameters QIND. */
    /*             This array is not referenced if COMPQ = 'N'. */
    /*     LDQ1    INTEGER */
    /*             The first leading dimension of the array Q.  LDQ1 >= 1, */
    /*             and, if COMPQ <> 'N', LDQ1 >= MAX(1,N). */
    /*     LDQ2    INTEGER */
    /*             The second leading dimension of the array Q.  LDQ2 >= 1, */
    /*             and, if COMPQ <> 'N', LDQ2 >= MAX(1,N). */
    /*     ALPHAR  (output) DOUBLE PRECISION array, dimension (N) */
    /*             On exit, if IWARN = 0 and INFO = 0, the leading N elements */
    /*             of this array contain the scaled real parts of the */
    /*             eigenvalues of the matrix product A. The i-th eigenvalue */
    /*             of A is given by */
    /*             (ALPHAR(I) + ALPHAI(I)*SQRT(-1))/BETA(I) * BASE**SCAL(I), */
    /*             where BASE is the machine base (often 2.0). */
    /*     ALPHAI  (output) DOUBLE PRECISION array, dimension (N) */
    /*             On exit, if IWARN = 0 and INFO = 0, the leading N elements */
    /*             of this array contain the scaled imaginary parts of the */
    /*             eigenvalues of A. */
    /*     BETA    (output) DOUBLE PRECISION array, dimension (N) */
    /*             On exit, if IWARN = 0 and INFO = 0, the leading N elements */
    /*             of this array contain indicators for infinite eigenvalues. */
    /*             That is, if BETA(I) = 0.0, then the i-th eigenvalue is */
    /*             infinite. Otherwise BETA(I) is set to 1.0. */
    /*     SCAL    (output) INTEGER array, dimension (N) */
    /*             On exit, if IWARN = 0 and INFO = 0, the leading N elements */
    /*             of this array contain the scaling parameters for the */
    /*             eigenvalues of A. */
    /*     Workspace */
    /*     IWORK   INTEGER array, dimension (LIWORK) */
    /*             On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK. */
    /*             On exit, if INFO = -22, IWORK(1) returns the minimum value */
    /*             of LIWORK. */
    /*     LIWORK  INTEGER */
    /*             The length of the array IWORK.  LIWORK  >= MAX( 1,2*K ). */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             On exit, if INFO = 0, DWORK(1) returns the optimal LDWORK. */
    /*             On exit, if INFO = -24, DWORK(1) returns the minimum value */
    /*             of LDWORK. */
    /*     LDWORK  INTEGER */
    /*             The length of the array DWORK. */
    /*             If DEFL = 'C', LDWORK >= MAX( 1,MAX( 2*N,8*K ) ); */
    /*             if DEFL = 'A', LDWORK >= MAX( 1,K + MAX( 2*N,8*K ) ). */
    /*     Warning Indicator */
    /*     IWARN   INTEGER */
    /*             = 0        :  no warnings; */
    /*             = 1,..,N-1 :  A is in periodic Schur form, but the */
    /*                           algorithm was not able to reveal information */
    /*                           about the eigenvalues from the 2-by-2 */
    /*                           blocks. */
    /*                           ALPHAR(i), ALPHAI(i), BETA(i) and SCAL(i), */
    /*                           can be incorrect for i = 1, ..., IWARN+1. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0      :  succesful exit; */
    /*             < 0      :  if INFO = -i, the i-th argument had an illegal */
    /*                         value; */
    /*             = 1,..,N :  the periodic QZ iteration did not converge. */
    /*                         A is not in periodic Schur form, but */
    /*                         ALPHAR(i), ALPHAI(i), BETA(i) and SCAL(i), for */
    /*                         i = INFO+1,...,N should be correct. */
    /*     METHOD */
    /*     A modified version of the periodic QZ algorithm is used [1], [2]. */
    /*     REFERENCES */
    /*     [1] Bojanczyk, A., Golub, G. H. and Van Dooren, P. */
    /*         The periodic Schur decomposition: algorithms and applications. */
    /*         In F.T. Luk (editor), Advanced Signal Processing Algorithms, */
    /*         Architectures, and Implementations III, Proc. SPIE Conference, */
    /*         vol. 1770, pp. 31-42, 1992. */
    /*     [2] Kressner, D. */
    /*         An efficient and reliable implementation of the periodic QZ */
    /*         algorithm. IFAC Workshop on Periodic Control Systems (PSYCO */
    /*         2001), Como (Italy), August 27-28 2001. Periodic Control */
    /*         Systems 2001 (IFAC Proceedings Volumes), Pergamon. */
    /*     NUMERICAL ASPECTS */
    /*     The implemented method is numerically backward stable. */
    /*                                 3 */
    /*     The algorithm requires 0(K N ) floating point operations. */
    /*     CONTRIBUTOR */
    /*     D. Kressner, Technical Univ. Berlin, Germany, June 2001. */
    /*     REVISIONS */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Romania, */
    /*     July 2009, SLICOT Library version of the routine PHGEQZ. */
    /*     V. Sima, June 2010, July 2010, Nov. 2010. */
    /*     KEYWORDS */
    /*     Eigenvalues, QZ algorithm, periodic QZ algorithm, orthogonal */
    /*     transformation. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Arrays .. */
    /*     .. Local Scalars .. */
    /*     .. Workspace Pointers .. */
    /*     .. External Functions .. */
    /*     .. External Subroutines .. */
    /*     .. Intrinsic Functions .. */
    /*     .. Executable Statements .. */
    /*     Decode the scalar input parameters. */
    /* Parameter adjustments */
    --qind;
    --s;
    a_dim1 = *lda1;
    a_dim2 = *lda2;
    a_offset = a_dim1 * (a_dim2 + 1) + 1;
    a -= a_offset;
    q_dim1 = *ldq1;
    q_dim2 = *ldq2;
    q_offset = q_dim1 * (q_dim2 + 1) + 1;
    q -= q_offset;
    --alphar;
    --alphai;
    --beta;
    --scal;
    --iwork;
    --dwork;
    /* Function Body */
    lsvd = lsame_(job, "T", 1L, 1L);
    lschr = lsame_(job, "S", 1L, 1L) || lsvd;
    liniq = lsame_(compq, "I", 1L, 1L);
    lcmpq = lsame_(compq, "U", 1L, 1L) || liniq;
    lparq = lsame_(compq, "P", 1L, 1L);
    adefl = lsame_(defl, "A", 1L, 1L);
    *iwarn = 0;
    if (adefl) {
        /* Computing MAX */
        /* Computing MAX */
        i__3 = *n << 1, i__4 = *k << 3;
        i__1 = 1, i__2 = *k + max(i__3, i__4);
        optdw = max(i__1, i__2);
    } else {
        /* Computing MAX */
        /* Computing MAX */
        i__3 = *n << 1, i__4 = *k << 3;
        i__1 = 1, i__2 = max(i__3, i__4);
        optdw = max(i__1, i__2);
    }
    /* Computing MAX */
    i__1 = 1, i__2 = *k << 1;
    optiw = max(i__1, i__2);
    /*     Check the scalar input parameters. */
    *info = 0;
    if (!(lschr || lsame_(job, "E", 1L, 1L))) {
        *info = -1;
    } else if (!(adefl || lsame_(defl, "C", 1L, 1L))) {
        *info = -2;
    } else if (!(lcmpq || lparq || lsame_(compq, "N", 1L, 1L))) {
        *info = -3;
    } else if (*k < 0) {
        *info = -5;
    } else if (*n < 0) {
        *info = -6;
    } else if (*h__ < 1 || *h__ > *k) {
        *info = -7;
    } else if (*ilo < 1) {
        *info = -8;
    } else if (*ihi > *n || *ihi < *ilo - 1) {
        *info = -9;
    } else if (*lda1 < max(1, *n)) {
        *info = -12;
    } else if (*lda2 < max(1, *n)) {
        *info = -13;
    } else if (*ldq1 < 1 || (lcmpq || lparq) && *ldq1 < *n) {
        *info = -15;
    } else if (*ldq2 < 1 || (lcmpq || lparq) && *ldq2 < *n) {
        *info = -16;
    } else if (*liwork < optiw) {
        iwork[1] = optiw;
        *info = -22;
    } else if (*ldwork < optdw) {
        dwork[1] = (doublereal)optdw;
        *info = -24;
    }
    /*     Return if there were illegal values. */
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("MB03BD", &i__1, 6L);
        return 0;
    }
    /*     Quick return if possible. */
    if (*n == 0) {
        dwork[1] = 1.;
        iwork[1] = 1;
        return 0;
    }
    /*     Initialize Q. */
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
        j = 0;
        if (liniq) {
            j = i__;
        } else if (lparq) {
            j = -qind[i__];
        }
        if (j != 0) {
            dlaset_("Full", n, n, &c_b14, &c_b15, &q[(j * q_dim2 + 1) * q_dim1 + 1], ldq1, 4L);
        }
        /* L10: */
    }
    /*     Compute Maps for accessing A and Q. */
    mapa = 0;
    mapq = *k;
    qi = 0;
    mb03ba_(k, h__, &s[1], &sinv, &iwork[mapa + 1], &iwork[mapq + 1]);
    /*     Machine Constants. */
    in = *ihi + 1 - *ilo;
    safmin = dlamch_("SafeMinimum", 11L);
    safmax = 1. / safmin;
    ulp = dlamch_("Precision", 9L);
    dlabad_(&safmin, &safmax);
    smlnum = safmin * (in / ulp);
    base = dlamch_("Base", 4L);
    lgbas = log(base);
    macpar[1] = dlamch_("Underflow", 9L);
    if (lsvd) {
        macpar[0] = dlamch_("ORmax", 5L);
        macpar[2] = safmin;
        macpar[3] = dlamch_("Epsilon", 7L);
        macpar[4] = base;
    }
    if (*k >= (integer)(log(macpar[1]) / log(ulp))) {
        /*        Start Iteration with a controlled zero shift. */
        ziter = -1;
    } else {
        ziter = 0;
    }
    /*     Compute norms. */
    if (adefl) {
        pnorm = 0;
        pfree = *k;
        i__1 = *k;
        for (i__ = 1; i__ <= i__1; ++i__) {
            aind = iwork[mapa + i__];
            dwork[i__] = dlanhs_("Frobenius", &in, &a[*ilo + (*ilo + aind * a_dim2) * a_dim1], lda1,
                &dwork[pfree + 1], 9L);
            /* L20: */
        }
    } else {
        pfree = 0;
    }
    /*     Set Eigenvalues IHI+1:N. */
    i__1 = *n;
    for (j = *ihi + 1; j <= i__1; ++j) {
        i__2 = *lda1 * *lda2;
        ma01bd_(&base, &lgbas, k, &s[1], &a[j + (j + a_dim2) * a_dim1], &i__2, &alphar[j], &beta[j],
            &scal[j]);
        alphai[j] = 0.;
        /* L30: */
    }
    /*     If IHI < ILO, skip QZ steps. */
    if (*ihi < *ilo) {
        goto L500;
    }
    /*     MAIN PERIODIC QZ ITERATION LOOP. */
    /*     Initialize dynamic indices. */
    /*     Eigenvalues ILAST+1:N have been found. */
    /*        Column operations modify rows IFRSTM:whatever. */
    /*        Row operations modify columns whatever:ILASTM. */
    /*     If only eigenvalues are being computed, then */
    /*        IFRSTM is the row of the last splitting row above row ILAST; */
    /*        this is always at least ILO. */
    /*     IITER counts iterations since the last eigenvalue was found, */
    /*        to tell when to use an observed zero or random shift. */
    /*     MAXIT is the maximum number of QZ sweeps allowed. */
    ilast = *ihi;
    if (lschr) {
        ifrstm = 1;
        ilastm = *n;
    } else {
        ifrstm = *ilo;
        ilastm = *ihi;
    }
    iiter = 0;
    titer = 0;
    iseed[0] = 1;
    iseed[1] = 0;
    iseed[2] = 0;
    iseed[3] = 1;
    maxit = in * 30;
    i__1 = maxit;
    for (jiter = 1; jiter <= i__1; ++jiter) {
        /*        Special Case: ILAST = ILO. */
        if (ilast == *ilo) {
            goto L390;
        }
        /*        ************************************************************** */
        /*        *                     CHECK FOR DEFLATION                    * */
        /*        ************************************************************** */
        /*        Test 1:  Deflation in the Hessenberg matrix. */
        if (adefl) {
            /* Computing MAX */
            d__1 = safmin, d__2 = dwork[pnorm + 1] * ulp;
            tol = max(d__1, d__2);
        }
        aind = iwork[mapa + 1];
        jlo = *ilo;
        i__2 = *ilo + 1;
        for (j = ilast; j >= i__2; --j) {
            if (!adefl) {
                tol = (d__1 = a[j - 1 + (j - 1 + aind * a_dim2) * a_dim1], abs(d__1))
                    + (d__2 = a[j + (j + aind * a_dim2) * a_dim1], abs(d__2));
                if (tol == 0.) {
                    i__3 = j - *ilo + 1;
                    tol = dlanhs_("1", &i__3, &a[*ilo + (*ilo + aind * a_dim2) * a_dim1], lda1,
                        &dwork[pfree + 1], 1L);
                }
                /* Computing MAX */
                d__1 = ulp * tol;
                tol = max(d__1, smlnum);
            }
            if ((d__1 = a[j + (j - 1 + aind * a_dim2) * a_dim1], abs(d__1)) <= tol) {
                a[j + (j - 1 + aind * a_dim2) * a_dim1] = 0.;
                jlo = j;
                if (j == ilast) {
                    goto L390;
                }
                goto L50;
            }
            /* L40: */
        }
    L50:
        /*        Test 2:  Deflation in the triangular matrices with index 1. */
        i__2 = *k;
        for (ldef = 2; ldef <= i__2; ++ldef) {
            aind = iwork[mapa + ldef];
            if (s[aind] == sinv) {
                if (adefl) {
                    /* Computing MAX */
                    d__1 = safmin, d__2 = dwork[pnorm + ldef] * ulp;
                    tol = max(d__1, d__2);
                }
                i__3 = jlo;
                for (j = ilast; j >= i__3; --j) {
                    if (!adefl) {
                        if (j == ilast) {
                            tol = (d__1 = a[j - 1 + (j + aind * a_dim2) * a_dim1], abs(d__1));
                        } else if (j == jlo) {
                            tol = (d__1 = a[j + (j + 1 + aind * a_dim2) * a_dim1], abs(d__1));
                        } else {
                            tol = (d__1 = a[j - 1 + (j + aind * a_dim2) * a_dim1], abs(d__1))
                                + (d__2 = a[j + (j + 1 + aind * a_dim2) * a_dim1], abs(d__2));
                        }
                        if (tol == 0.) {
                            i__4 = j - jlo + 1;
                            tol = dlanhs_("1", &i__4, &a[jlo + (jlo + aind * a_dim2) * a_dim1],
                                lda1, &dwork[pfree + 1], 1L);
                        }
                        /* Computing MAX */
                        d__1 = ulp * tol;
                        tol = max(d__1, smlnum);
                    }
                    if ((d__1 = a[j + (j + aind * a_dim2) * a_dim1], abs(d__1)) <= tol) {
                        a[j + (j + aind * a_dim2) * a_dim1] = 0.;
                        goto L170;
                    }
                    /* L60: */
                }
            }
            /* L70: */
        }
        /*        Test 3:  Deflation in the triangular matrices with index -1. */
        i__2 = *k;
        for (ldef = 2; ldef <= i__2; ++ldef) {
            aind = iwork[mapa + ldef];
            if (s[aind] != sinv) {
                if (adefl) {
                    /* Computing MAX */
                    d__1 = safmin, d__2 = dwork[pnorm + ldef] * ulp;
                    tol = max(d__1, d__2);
                }
                i__3 = jlo;
                for (j = ilast; j >= i__3; --j) {
                    if (!adefl) {
                        if (j == ilast) {
                            tol = (d__1 = a[j - 1 + (j + aind * a_dim2) * a_dim1], abs(d__1));
                        } else if (j == jlo) {
                            tol = (d__1 = a[j + (j + 1 + aind * a_dim2) * a_dim1], abs(d__1));
                        } else {
                            tol = (d__1 = a[j - 1 + (j + aind * a_dim2) * a_dim1], abs(d__1))
                                + (d__2 = a[j + (j + 1 + aind * a_dim2) * a_dim1], abs(d__2));
                        }
                        if (tol == 0.) {
                            i__4 = j - jlo + 1;
                            tol = dlanhs_("1", &i__4, &a[jlo + (jlo + aind * a_dim2) * a_dim1],
                                lda1, &dwork[pfree + 1], 1L);
                        }
                        /* Computing MAX */
                        d__1 = ulp * tol;
                        tol = max(d__1, smlnum);
                    }
                    if ((d__1 = a[j + (j + aind * a_dim2) * a_dim1], abs(d__1)) <= tol) {
                        a[j + (j + aind * a_dim2) * a_dim1] = 0.;
                        goto L320;
                    }
                    /* L80: */
                }
            }
            /* L90: */
        }
        /*        Test 4:  Controlled zero shift. */
        if (ziter >= 7 || ziter < 0) {
            /*           Make Hessenberg matrix upper triangular. */
            aind = iwork[mapa + 1];
            pdw = pfree + 1;
            i__2 = ilast - 1;
            for (j = jlo; j <= i__2; ++j) {
                temp = a[j + (j + aind * a_dim2) * a_dim1];
                dlartg_(&temp, &a[j + 1 + (j + aind * a_dim2) * a_dim1], &cs, &sn,
                    &a[j + (j + aind * a_dim2) * a_dim1]);
                a[j + 1 + (j + aind * a_dim2) * a_dim1] = 0.;
                i__3 = ilastm - j;
                drot_(&i__3, &a[j + (j + 1 + aind * a_dim2) * a_dim1], lda1,
                    &a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
                dwork[pdw] = cs;
                dwork[pdw + 1] = sn;
                pdw += 2;
                /* L100: */
            }
            if (lcmpq) {
                qi = iwork[mapq + 1];
            } else if (lparq) {
                qi = (i__2 = qind[iwork[mapq + 1]], abs(i__2));
            }
            if (qi != 0) {
                pdw = pfree + 1;
                i__2 = ilast - 1;
                for (j = jlo; j <= i__2; ++j) {
                    cs = dwork[pdw];
                    sn = dwork[pdw + 1];
                    pdw += 2;
                    drot_(n, &q[(j + qi * q_dim2) * q_dim1 + 1], &c__1,
                        &q[(j + 1 + qi * q_dim2) * q_dim1 + 1], &c__1, &cs, &sn);
                    /* L110: */
                }
            }
            /*           Propagate Transformations back to A_1. */
            for (l = *k; l >= 2; --l) {
                aind = iwork[mapa + l];
                pdw = pfree + 1;
                if (adefl) {
                    /* Computing MAX */
                    d__1 = safmin, d__2 = dwork[pnorm + l] * ulp;
                    tol = max(d__1, d__2);
                }
                if (s[aind] == sinv) {
                    i__2 = ilast - 1;
                    for (j = jlo; j <= i__2; ++j) {
                        cs = dwork[pdw];
                        sn = dwork[pdw + 1];
                        if (sn != 0.) {
                            i__3 = j + 2 - ifrstm;
                            drot_(&i__3, &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1,
                                &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
                            /*                       Check for deflation. */
                            if (!adefl) {
                                tol = (d__1 = a[j + (j + aind * a_dim2) * a_dim1], abs(d__1))
                                    + (d__2 = a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1],
                                          abs(d__2));
                                if (tol == 0.) {
                                    i__3 = j - jlo + 2;
                                    tol = dlanhs_("1", &i__3,
                                        &a[jlo + (jlo + aind * a_dim2) * a_dim1], lda1,
                                        &dwork[pfree + (*n << 1) + 1], 1L);
                                }
                                /* Computing MAX */
                                d__1 = ulp * tol;
                                tol = max(d__1, smlnum);
                                if ((d__1 = a[j + 1 + (j + aind * a_dim2) * a_dim1], abs(d__1))
                                    <= tol) {
                                    cs = 1.;
                                    sn = 0.;
                                    a[j + 1 + (j + aind * a_dim2) * a_dim1] = 0.;
                                }
                            }
                        }
                        if (sn != 0.) {
                            temp = a[j + (j + aind * a_dim2) * a_dim1];
                            dlartg_(&temp, &a[j + 1 + (j + aind * a_dim2) * a_dim1], &cs, &sn,
                                &a[j + (j + aind * a_dim2) * a_dim1]);
                            a[j + 1 + (j + aind * a_dim2) * a_dim1] = 0.;
                            i__3 = ilastm - j;
                            drot_(&i__3, &a[j + (j + 1 + aind * a_dim2) * a_dim1], lda1,
                                &a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
                        }
                        dwork[pdw] = cs;
                        dwork[pdw + 1] = sn;
                        pdw += 2;
                        /* L120: */
                    }
                } else {
                    i__2 = ilast - 1;
                    for (j = jlo; j <= i__2; ++j) {
                        cs = dwork[pdw];
                        sn = dwork[pdw + 1];
                        if (sn != 0.) {
                            i__3 = ilastm - j + 1;
                            drot_(&i__3, &a[j + (j + aind * a_dim2) * a_dim1], lda1,
                                &a[j + 1 + (j + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
                            /*                       Check for deflation. */
                            if (!adefl) {
                                tol = (d__1 = a[j + (j + aind * a_dim2) * a_dim1], abs(d__1))
                                    + (d__2 = a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1],
                                          abs(d__2));
                                if (tol == 0.) {
                                    i__3 = j - jlo + 2;
                                    tol = dlanhs_("1", &i__3,
                                        &a[jlo + (jlo + aind * a_dim2) * a_dim1], lda1,
                                        &dwork[pfree + (*n << 1) + 1], 1L);
                                }
                                /* Computing MAX */
                                d__1 = ulp * tol;
                                tol = max(d__1, smlnum);
                                if ((d__1 = a[j + 1 + (j + aind * a_dim2) * a_dim1], abs(d__1))
                                    <= tol) {
                                    cs = 1.;
                                    sn = 0.;
                                    a[j + 1 + (j + aind * a_dim2) * a_dim1] = 0.;
                                }
                            }
                        }
                        if (sn != 0.) {
                            temp = a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1];
                            dlartg_(&temp, &a[j + 1 + (j + aind * a_dim2) * a_dim1], &cs, &sn,
                                &a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1]);
                            a[j + 1 + (j + aind * a_dim2) * a_dim1] = 0.;
                            sn = -sn;
                            i__3 = j + 1 - ifrstm;
                            drot_(&i__3, &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1,
                                &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
                        }
                        dwork[pdw] = cs;
                        dwork[pdw + 1] = sn;
                        pdw += 2;
                        /* L130: */
                    }
                }
                if (lcmpq) {
                    qi = iwork[mapq + l];
                } else if (lparq) {
                    qi = (i__2 = qind[iwork[mapq + l]], abs(i__2));
                }
                if (qi != 0) {
                    pdw = pfree + 1;
                    i__2 = ilast - 1;
                    for (j = jlo; j <= i__2; ++j) {
                        cs = dwork[pdw];
                        sn = dwork[pdw + 1];
                        pdw += 2;
                        if (sn != 0.) {
                            drot_(n, &q[(j + qi * q_dim2) * q_dim1 + 1], &c__1,
                                &q[(j + 1 + qi * q_dim2) * q_dim1 + 1], &c__1, &cs, &sn);
                        }
                        /* L140: */
                    }
                }
                /* L150: */
            }
            /*           Apply the transformations to the right hand side of the */
            /*           Hessenberg factor. */
            aind = iwork[mapa + 1];
            pdw = pfree + 1;
            ziter = 0;
            i__2 = ilast - 1;
            for (j = jlo; j <= i__2; ++j) {
                cs = dwork[pdw];
                sn = dwork[pdw + 1];
                if (sn != 0.) {
                    i__3 = j + 2 - ifrstm;
                    drot_(&i__3, &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1,
                        &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
                } else {
                    ziter = -1;
                }
                pdw += 2;
                /* L160: */
            }
            /*           No QZ iteration. */
            goto L480;
        }
        /*        ************************************************************** */
        /*        *                     HANDLE DEFLATIONS                      * */
        /*        ************************************************************** */
        /*        Case I: Deflation occurs in the Hessenberg matrix. The QZ */
        /*                iteration is only applied to the JLO:ILAST part. */
        ifirst = jlo;
        /*        Go to the periodic QZ steps. */
        goto L400;
        /*        Case II: Deflation occurs in a triangular matrix with index 1. */
        /*        Do an unshifted periodic QZ step. */
    L170:
        jdef = j;
        aind = iwork[mapa + 1];
        pdw = pfree + 1;
        i__2 = jdef - 1;
        for (j = jlo; j <= i__2; ++j) {
            temp = a[j + (j + aind * a_dim2) * a_dim1];
            dlartg_(&temp, &a[j + 1 + (j + aind * a_dim2) * a_dim1], &cs, &sn,
                &a[j + (j + aind * a_dim2) * a_dim1]);
            a[j + 1 + (j + aind * a_dim2) * a_dim1] = 0.;
            i__3 = ilastm - j;
            drot_(&i__3, &a[j + (j + 1 + aind * a_dim2) * a_dim1], lda1,
                &a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
            dwork[pdw] = cs;
            dwork[pdw + 1] = sn;
            pdw += 2;
            /* L180: */
        }
        if (lcmpq) {
            qi = iwork[mapq + 1];
        } else if (lparq) {
            qi = (i__2 = qind[iwork[mapq + 1]], abs(i__2));
        }
        if (qi != 0) {
            pdw = pfree + 1;
            i__2 = jdef - 1;
            for (j = jlo; j <= i__2; ++j) {
                cs = dwork[pdw];
                sn = dwork[pdw + 1];
                pdw += 2;
                drot_(n, &q[(j + qi * q_dim2) * q_dim1 + 1], &c__1,
                    &q[(j + 1 + qi * q_dim2) * q_dim1 + 1], &c__1, &cs, &sn);
                /* L190: */
            }
        }
        /*        Propagate the transformations through the triangular matrices. */
        /*        Due to the zero element on the diagonal of the LDEF-th factor, */
        /*        the number of transformations drops by one. */
        for (l = *k; l >= 2; --l) {
            aind = iwork[mapa + l];
            if (l < ldef) {
                ntra = jdef - 2;
            } else {
                ntra = jdef - 1;
            }
            pdw = pfree + 1;
            if (s[aind] == sinv) {
                i__2 = ntra;
                for (j = jlo; j <= i__2; ++j) {
                    cs = dwork[pdw];
                    sn = dwork[pdw + 1];
                    i__3 = j + 2 - ifrstm;
                    drot_(&i__3, &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1,
                        &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
                    temp = a[j + (j + aind * a_dim2) * a_dim1];
                    dlartg_(&temp, &a[j + 1 + (j + aind * a_dim2) * a_dim1], &cs, &sn,
                        &a[j + (j + aind * a_dim2) * a_dim1]);
                    a[j + 1 + (j + aind * a_dim2) * a_dim1] = 0.;
                    i__3 = ilastm - j;
                    drot_(&i__3, &a[j + (j + 1 + aind * a_dim2) * a_dim1], lda1,
                        &a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
                    dwork[pdw] = cs;
                    dwork[pdw + 1] = sn;
                    pdw += 2;
                    /* L200: */
                }
            } else {
                i__2 = ntra;
                for (j = jlo; j <= i__2; ++j) {
                    cs = dwork[pdw];
                    sn = dwork[pdw + 1];
                    i__3 = ilastm - j + 1;
                    drot_(&i__3, &a[j + (j + aind * a_dim2) * a_dim1], lda1,
                        &a[j + 1 + (j + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
                    temp = a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1];
                    dlartg_(&temp, &a[j + 1 + (j + aind * a_dim2) * a_dim1], &cs, &sn,
                        &a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1]);
                    a[j + 1 + (j + aind * a_dim2) * a_dim1] = 0.;
                    sn = -sn;
                    i__3 = j + 1 - ifrstm;
                    drot_(&i__3, &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1,
                        &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
                    dwork[pdw] = cs;
                    dwork[pdw + 1] = sn;
                    pdw += 2;
                    /* L210: */
                }
            }
            if (lcmpq) {
                qi = iwork[mapq + l];
            } else if (lparq) {
                qi = (i__2 = qind[iwork[mapq + l]], abs(i__2));
            }
            if (qi != 0) {
                pdw = pfree + 1;
                i__2 = ntra;
                for (j = jlo; j <= i__2; ++j) {
                    cs = dwork[pdw];
                    sn = dwork[pdw + 1];
                    pdw += 2;
                    drot_(n, &q[(j + qi * q_dim2) * q_dim1 + 1], &c__1,
                        &q[(j + 1 + qi * q_dim2) * q_dim1 + 1], &c__1, &cs, &sn);
                    /* L220: */
                }
            }
            /* L230: */
        }
        /*        Apply the transformations to the right hand side of the */
        /*        Hessenberg factor. */
        aind = iwork[mapa + 1];
        pdw = pfree + 1;
        i__2 = jdef - 2;
        for (j = jlo; j <= i__2; ++j) {
            cs = dwork[pdw];
            sn = dwork[pdw + 1];
            i__3 = j + 2 - ifrstm;
            drot_(&i__3, &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1,
                &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
            pdw += 2;
            /* L240: */
        }
        /*        Do an unshifted periodic QZ step. */
        pdw = pfree + 1;
        i__2 = jdef + 1;
        for (j = ilast; j >= i__2; --j) {
            temp = a[j + (j + aind * a_dim2) * a_dim1];
            dlartg_(&temp, &a[j + (j - 1 + aind * a_dim2) * a_dim1], &cs, &sn,
                &a[j + (j + aind * a_dim2) * a_dim1]);
            a[j + (j - 1 + aind * a_dim2) * a_dim1] = 0.;
            sn = -sn;
            i__3 = j - ifrstm;
            drot_(&i__3, &a[ifrstm + (j - 1 + aind * a_dim2) * a_dim1], &c__1,
                &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
            dwork[pdw] = cs;
            dwork[pdw + 1] = sn;
            pdw += 2;
            /* L250: */
        }
        if (lcmpq) {
            qi = iwork[mapq + 2];
        } else if (lparq) {
            qi = (i__2 = qind[iwork[mapq + 2]], abs(i__2));
        }
        if (qi != 0) {
            pdw = pfree + 1;
            i__2 = jdef + 1;
            for (j = ilast; j >= i__2; --j) {
                cs = dwork[pdw];
                sn = dwork[pdw + 1];
                pdw += 2;
                drot_(n, &q[(j - 1 + qi * q_dim2) * q_dim1 + 1], &c__1,
                    &q[(j + qi * q_dim2) * q_dim1 + 1], &c__1, &cs, &sn);
                /* L260: */
            }
        }
        /*        Propagate the transformations through the triangular matrices. */
        i__2 = *k;
        for (l = 2; l <= i__2; ++l) {
            aind = iwork[mapa + l];
            if (l > ldef) {
                ntra = jdef + 2;
            } else {
                ntra = jdef + 1;
            }
            pdw = pfree + 1;
            if (s[aind] != sinv) {
                i__3 = ntra;
                for (j = ilast; j >= i__3; --j) {
                    cs = dwork[pdw];
                    sn = dwork[pdw + 1];
                    i__4 = j + 1 - ifrstm;
                    drot_(&i__4, &a[ifrstm + (j - 1 + aind * a_dim2) * a_dim1], &c__1,
                        &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
                    temp = a[j - 1 + (j - 1 + aind * a_dim2) * a_dim1];
                    dlartg_(&temp, &a[j + (j - 1 + aind * a_dim2) * a_dim1], &cs, &sn,
                        &a[j - 1 + (j - 1 + aind * a_dim2) * a_dim1]);
                    a[j + (j - 1 + aind * a_dim2) * a_dim1] = 0.;
                    i__4 = ilastm - j + 1;
                    drot_(&i__4, &a[j - 1 + (j + aind * a_dim2) * a_dim1], lda1,
                        &a[j + (j + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
                    dwork[pdw] = cs;
                    dwork[pdw + 1] = sn;
                    pdw += 2;
                    /* L270: */
                }
            } else {
                i__3 = ntra;
                for (j = ilast; j >= i__3; --j) {
                    cs = dwork[pdw];
                    sn = dwork[pdw + 1];
                    i__4 = ilastm - j + 2;
                    drot_(&i__4, &a[j - 1 + (j - 1 + aind * a_dim2) * a_dim1], lda1,
                        &a[j + (j - 1 + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
                    temp = a[j + (j + aind * a_dim2) * a_dim1];
                    dlartg_(&temp, &a[j + (j - 1 + aind * a_dim2) * a_dim1], &cs, &sn,
                        &a[j + (j + aind * a_dim2) * a_dim1]);
                    a[j + (j - 1 + aind * a_dim2) * a_dim1] = 0.;
                    sn = -sn;
                    i__4 = j - ifrstm;
                    drot_(&i__4, &a[ifrstm + (j - 1 + aind * a_dim2) * a_dim1], &c__1,
                        &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
                    dwork[pdw] = cs;
                    dwork[pdw + 1] = sn;
                    pdw += 2;
                    /* L280: */
                }
            }
            lm = l + 1;
            if (l == *k) {
                lm = 1;
            }
            if (lcmpq) {
                qi = iwork[mapq + lm];
            } else if (lparq) {
                qi = (i__3 = qind[iwork[mapq + lm]], abs(i__3));
            }
            if (qi != 0) {
                pdw = pfree + 1;
                i__3 = ntra;
                for (j = ilast; j >= i__3; --j) {
                    cs = dwork[pdw];
                    sn = dwork[pdw + 1];
                    pdw += 2;
                    drot_(n, &q[(j - 1 + qi * q_dim2) * q_dim1 + 1], &c__1,
                        &q[(j + qi * q_dim2) * q_dim1 + 1], &c__1, &cs, &sn);
                    /* L290: */
                }
            }
            /* L300: */
        }
        /*        Apply the transformations to the left hand side of the */
        /*        Hessenberg factor. */
        aind = iwork[mapa + 1];
        pdw = pfree + 1;
        i__2 = jdef + 2;
        for (j = ilast; j >= i__2; --j) {
            cs = dwork[pdw];
            sn = dwork[pdw + 1];
            pdw += 2;
            i__3 = ilastm - j + 2;
            drot_(&i__3, &a[j - 1 + (j - 1 + aind * a_dim2) * a_dim1], lda1,
                &a[j + (j - 1 + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
            /* L310: */
        }
        /*        No QZ iteration. */
        goto L480;
        /*        Case III: Deflation occurs in a triangular matrix with */
        /*                  index -1. */
    L320:
        jdef = j;
        pdw = pfree + 1;
        if (jdef > (ilast - jlo + 1) / 2) {
            /*           Chase the zero downwards to the last position. */
            i__2 = ilast - 1;
            for (j1 = jdef; j1 <= i__2; ++j1) {
                j = j1;
                aind = iwork[mapa + ldef];
                temp = a[j + (j + 1 + aind * a_dim2) * a_dim1];
                dlartg_(&temp, &a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1], &cs, &sn,
                    &a[j + (j + 1 + aind * a_dim2) * a_dim1]);
                a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1] = 0.;
                i__3 = ilastm - j - 1;
                drot_(&i__3, &a[j + (j + 2 + aind * a_dim2) * a_dim1], lda1,
                    &a[j + 1 + (j + 2 + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
                lm = ldef + 1;
                if (ldef == *k) {
                    lm = 1;
                }
                if (lcmpq) {
                    qi = iwork[mapq + lm];
                } else if (lparq) {
                    qi = (i__3 = qind[iwork[mapq + lm]], abs(i__3));
                }
                if (qi != 0) {
                    drot_(n, &q[(j + qi * q_dim2) * q_dim1 + 1], &c__1,
                        &q[(j + 1 + qi * q_dim2) * q_dim1 + 1], &c__1, &cs, &sn);
                }
                i__3 = *k - 1;
                for (l = 1; l <= i__3; ++l) {
                    aind = iwork[mapa + lm];
                    if (lm == 1) {
                        i__4 = ilastm - j + 2;
                        drot_(&i__4, &a[j + (j - 1 + aind * a_dim2) * a_dim1], lda1,
                            &a[j + 1 + (j - 1 + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
                        temp = a[j + 1 + (j + aind * a_dim2) * a_dim1];
                        dlartg_(&temp, &a[j + 1 + (j - 1 + aind * a_dim2) * a_dim1], &cs, &sn,
                            &a[j + 1 + (j + aind * a_dim2) * a_dim1]);
                        a[j + 1 + (j - 1 + aind * a_dim2) * a_dim1] = 0.;
                        sn = -sn;
                        i__4 = j - ifrstm + 1;
                        drot_(&i__4, &a[ifrstm + (j - 1 + aind * a_dim2) * a_dim1], &c__1,
                            &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
                        --j;
                    } else if (s[aind] == sinv) {
                        i__4 = ilastm - j + 1;
                        drot_(&i__4, &a[j + (j + aind * a_dim2) * a_dim1], lda1,
                            &a[j + 1 + (j + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
                        temp = a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1];
                        dlartg_(&temp, &a[j + 1 + (j + aind * a_dim2) * a_dim1], &cs, &sn,
                            &a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1]);
                        a[j + 1 + (j + aind * a_dim2) * a_dim1] = 0.;
                        sn = -sn;
                        i__4 = j - ifrstm + 1;
                        drot_(&i__4, &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1,
                            &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
                    } else {
                        i__4 = j - ifrstm + 2;
                        drot_(&i__4, &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1,
                            &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
                        temp = a[j + (j + aind * a_dim2) * a_dim1];
                        dlartg_(&temp, &a[j + 1 + (j + aind * a_dim2) * a_dim1], &cs, &sn,
                            &a[j + (j + aind * a_dim2) * a_dim1]);
                        a[j + 1 + (j + aind * a_dim2) * a_dim1] = 0.;
                        i__4 = ilastm - j;
                        drot_(&i__4, &a[j + (j + 1 + aind * a_dim2) * a_dim1], lda1,
                            &a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
                    }
                    ++lm;
                    if (lm > *k) {
                        lm = 1;
                    }
                    if (lcmpq) {
                        qi = iwork[mapq + lm];
                    } else if (lparq) {
                        qi = (i__4 = qind[iwork[mapq + lm]], abs(i__4));
                    }
                    if (qi != 0) {
                        drot_(n, &q[(j + qi * q_dim2) * q_dim1 + 1], &c__1,
                            &q[(j + 1 + qi * q_dim2) * q_dim1 + 1], &c__1, &cs, &sn);
                    }
                    /* L330: */
                }
                aind = iwork[mapa + ldef];
                i__3 = j - ifrstm + 1;
                drot_(&i__3, &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1,
                    &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
                /* L340: */
            }
            /*           Deflate the last element in the Hessenberg matrix. */
            aind = iwork[mapa + 1];
            j = ilast;
            temp = a[j + (j + aind * a_dim2) * a_dim1];
            dlartg_(&temp, &a[j + (j - 1 + aind * a_dim2) * a_dim1], &cs, &sn,
                &a[j + (j + aind * a_dim2) * a_dim1]);
            a[j + (j - 1 + aind * a_dim2) * a_dim1] = 0.;
            sn = -sn;
            i__2 = j - ifrstm;
            drot_(&i__2, &a[ifrstm + (j - 1 + aind * a_dim2) * a_dim1], &c__1,
                &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
            if (lcmpq) {
                qi = iwork[mapq + 2];
            } else if (lparq) {
                qi = (i__2 = qind[iwork[mapq + 2]], abs(i__2));
            }
            if (qi != 0) {
                drot_(n, &q[(j - 1 + qi * q_dim2) * q_dim1 + 1], &c__1,
                    &q[(j + qi * q_dim2) * q_dim1 + 1], &c__1, &cs, &sn);
            }
            i__2 = ldef - 1;
            for (l = 2; l <= i__2; ++l) {
                aind = iwork[mapa + l];
                if (s[aind] != sinv) {
                    i__3 = j + 1 - ifrstm;
                    drot_(&i__3, &a[ifrstm + (j - 1 + aind * a_dim2) * a_dim1], &c__1,
                        &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
                    temp = a[j - 1 + (j - 1 + aind * a_dim2) * a_dim1];
                    dlartg_(&temp, &a[j + (j - 1 + aind * a_dim2) * a_dim1], &cs, &sn,
                        &a[j - 1 + (j - 1 + aind * a_dim2) * a_dim1]);
                    a[j + (j - 1 + aind * a_dim2) * a_dim1] = 0.;
                    i__3 = ilastm - j + 1;
                    drot_(&i__3, &a[j - 1 + (j + aind * a_dim2) * a_dim1], lda1,
                        &a[j + (j + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
                } else {
                    i__3 = ilastm - j + 2;
                    drot_(&i__3, &a[j - 1 + (j - 1 + aind * a_dim2) * a_dim1], lda1,
                        &a[j + (j - 1 + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
                    temp = a[j + (j + aind * a_dim2) * a_dim1];
                    dlartg_(&temp, &a[j + (j - 1 + aind * a_dim2) * a_dim1], &cs, &sn,
                        &a[j + (j + aind * a_dim2) * a_dim1]);
                    a[j + (j - 1 + aind * a_dim2) * a_dim1] = 0.;
                    sn = -sn;
                    i__3 = j - ifrstm;
                    drot_(&i__3, &a[ifrstm + (j - 1 + aind * a_dim2) * a_dim1], &c__1,
                        &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
                }
                lm = l + 1;
                if (l == *k) {
                    lm = 1;
                }
                if (lcmpq) {
                    qi = iwork[mapq + lm];
                } else if (lparq) {
                    qi = (i__3 = qind[iwork[mapq + lm]], abs(i__3));
                }
                if (qi != 0) {
                    drot_(n, &q[(j - 1 + qi * q_dim2) * q_dim1 + 1], &c__1,
                        &q[(j + qi * q_dim2) * q_dim1 + 1], &c__1, &cs, &sn);
                }
                /* L350: */
            }
            aind = iwork[mapa + ldef];
            i__2 = j + 1 - ifrstm;
            drot_(&i__2, &a[ifrstm + (j - 1 + aind * a_dim2) * a_dim1], &c__1,
                &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
        } else {
            /*           Chase the zero upwards to the first position. */
            i__2 = jlo + 1;
            for (j1 = jdef; j1 >= i__2; --j1) {
                j = j1;
                aind = iwork[mapa + ldef];
                temp = a[j - 1 + (j + aind * a_dim2) * a_dim1];
                dlartg_(&temp, &a[j - 1 + (j - 1 + aind * a_dim2) * a_dim1], &cs, &sn,
                    &a[j - 1 + (j + aind * a_dim2) * a_dim1]);
                a[j - 1 + (j - 1 + aind * a_dim2) * a_dim1] = 0.;
                sn = -sn;
                i__3 = j - ifrstm - 1;
                drot_(&i__3, &a[ifrstm + (j - 1 + aind * a_dim2) * a_dim1], &c__1,
                    &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
                if (lcmpq) {
                    qi = iwork[mapq + ldef];
                } else if (lparq) {
                    qi = (i__3 = qind[iwork[mapq + ldef]], abs(i__3));
                }
                if (qi != 0) {
                    drot_(n, &q[(j - 1 + qi * q_dim2) * q_dim1 + 1], &c__1,
                        &q[(j + qi * q_dim2) * q_dim1 + 1], &c__1, &cs, &sn);
                }
                lm = ldef - 1;
                i__3 = *k - 1;
                for (l = 1; l <= i__3; ++l) {
                    aind = iwork[mapa + lm];
                    if (lm == 1) {
                        i__4 = j - ifrstm + 2;
                        drot_(&i__4, &a[ifrstm + (j - 1 + aind * a_dim2) * a_dim1], &c__1,
                            &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
                        temp = a[j + (j - 1 + aind * a_dim2) * a_dim1];
                        dlartg_(&temp, &a[j + 1 + (j - 1 + aind * a_dim2) * a_dim1], &cs, &sn,
                            &a[j + (j - 1 + aind * a_dim2) * a_dim1]);
                        a[j + 1 + (j - 1 + aind * a_dim2) * a_dim1] = 0.;
                        i__4 = ilastm - j + 1;
                        drot_(&i__4, &a[j + (j + aind * a_dim2) * a_dim1], lda1,
                            &a[j + 1 + (j + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
                        ++j;
                    } else if (s[aind] != sinv) {
                        i__4 = ilastm - j + 2;
                        drot_(&i__4, &a[j - 1 + (j - 1 + aind * a_dim2) * a_dim1], lda1,
                            &a[j + (j - 1 + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
                        temp = a[j + (j + aind * a_dim2) * a_dim1];
                        dlartg_(&temp, &a[j + (j - 1 + aind * a_dim2) * a_dim1], &cs, &sn,
                            &a[j + (j + aind * a_dim2) * a_dim1]);
                        a[j + (j - 1 + aind * a_dim2) * a_dim1] = 0.;
                        sn = -sn;
                        i__4 = j - ifrstm;
                        drot_(&i__4, &a[ifrstm + (j - 1 + aind * a_dim2) * a_dim1], &c__1,
                            &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
                    } else {
                        i__4 = j - ifrstm + 1;
                        drot_(&i__4, &a[ifrstm + (j - 1 + aind * a_dim2) * a_dim1], &c__1,
                            &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
                        temp = a[j - 1 + (j - 1 + aind * a_dim2) * a_dim1];
                        dlartg_(&temp, &a[j + (j - 1 + aind * a_dim2) * a_dim1], &cs, &sn,
                            &a[j - 1 + (j - 1 + aind * a_dim2) * a_dim1]);
                        a[j + (j - 1 + aind * a_dim2) * a_dim1] = 0.;
                        i__4 = ilastm - j + 1;
                        drot_(&i__4, &a[j - 1 + (j + aind * a_dim2) * a_dim1], lda1,
                            &a[j + (j + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
                    }
                    if (lcmpq) {
                        qi = iwork[mapq + lm];
                    } else if (lparq) {
                        qi = (i__4 = qind[iwork[mapq + lm]], abs(i__4));
                    }
                    if (qi != 0) {
                        drot_(n, &q[(j - 1 + qi * q_dim2) * q_dim1 + 1], &c__1,
                            &q[(j + qi * q_dim2) * q_dim1 + 1], &c__1, &cs, &sn);
                    }
                    --lm;
                    if (lm <= 0) {
                        lm = *k;
                    }
                    /* L360: */
                }
                aind = iwork[mapa + ldef];
                i__3 = ilastm - j + 1;
                drot_(&i__3, &a[j - 1 + (j + aind * a_dim2) * a_dim1], lda1,
                    &a[j + (j + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
                /* L370: */
            }
            /*           Deflate the first element in the Hessenberg matrix. */
            aind = iwork[mapa + 1];
            j = jlo;
            temp = a[j + (j + aind * a_dim2) * a_dim1];
            dlartg_(&temp, &a[j + 1 + (j + aind * a_dim2) * a_dim1], &cs, &sn,
                &a[j + (j + aind * a_dim2) * a_dim1]);
            a[j + 1 + (j + aind * a_dim2) * a_dim1] = 0.;
            i__2 = ilastm - j;
            drot_(&i__2, &a[j + (j + 1 + aind * a_dim2) * a_dim1], lda1,
                &a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
            if (lcmpq) {
                qi = iwork[mapq + 1];
            } else if (lparq) {
                qi = (i__2 = qind[iwork[mapq + 1]], abs(i__2));
            }
            if (qi != 0) {
                drot_(n, &q[(j + qi * q_dim2) * q_dim1 + 1], &c__1,
                    &q[(j + 1 + qi * q_dim2) * q_dim1 + 1], &c__1, &cs, &sn);
            }
            i__2 = ldef + 1;
            for (l = *k; l >= i__2; --l) {
                aind = iwork[mapa + l];
                if (s[aind] == sinv) {
                    i__3 = j + 2 - ifrstm;
                    drot_(&i__3, &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1,
                        &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
                    temp = a[j + (j + aind * a_dim2) * a_dim1];
                    dlartg_(&temp, &a[j + 1 + (j + aind * a_dim2) * a_dim1], &cs, &sn,
                        &a[j + (j + aind * a_dim2) * a_dim1]);
                    a[j + 1 + (j + aind * a_dim2) * a_dim1] = 0.;
                    i__3 = ilastm - j;
                    drot_(&i__3, &a[j + (j + 1 + aind * a_dim2) * a_dim1], lda1,
                        &a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
                } else {
                    i__3 = ilastm - j + 1;
                    drot_(&i__3, &a[j + (j + aind * a_dim2) * a_dim1], lda1,
                        &a[j + 1 + (j + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
                    temp = a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1];
                    dlartg_(&temp, &a[j + 1 + (j + aind * a_dim2) * a_dim1], &cs, &sn,
                        &a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1]);
                    a[j + 1 + (j + aind * a_dim2) * a_dim1] = 0.;
                    sn = -sn;
                    i__3 = j + 1 - ifrstm;
                    drot_(&i__3, &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1,
                        &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1, &cs, &sn);
                }
                if (lcmpq) {
                    qi = iwork[mapq + l];
                } else if (lparq) {
                    qi = (i__3 = qind[iwork[mapq + l]], abs(i__3));
                }
                if (qi != 0) {
                    drot_(n, &q[(j + qi * q_dim2) * q_dim1 + 1], &c__1,
                        &q[(j + 1 + qi * q_dim2) * q_dim1 + 1], &c__1, &cs, &sn);
                }
                /* L380: */
            }
            aind = iwork[mapa + ldef];
            i__2 = ilastm - j;
            drot_(&i__2, &a[j + (j + 1 + aind * a_dim2) * a_dim1], lda1,
                &a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1], lda1, &cs, &sn);
        }
        /*        No QZ iteration. */
        goto L480;
        /*        Special case: A 1x1 block splits off at the bottom. */
    L390:
        i__2 = *lda1 * *lda2;
        ma01bd_(&base, &lgbas, k, &s[1], &a[ilast + (ilast + a_dim2) * a_dim1], &i__2,
            &alphar[ilast], &beta[ilast], &scal[ilast]);
        alphai[ilast] = 0.;
        /*        Go to next block - exit if finished. */
        --ilast;
        if (ilast < *ilo) {
            goto L500;
        }
        /*        Reset iteration counters. */
        iiter = 0;
        titer = 0;
        if (ziter != -1) {
            ziter = 0;
        }
        if (!lschr) {
            ilastm = ilast;
            if (ifrstm > ilast) {
                ifrstm = *ilo;
            }
        }
        /*        No QZ iteration. */
        goto L480;
        /*        ************************************************************** */
        /*        *                      PERIODIC QZ STEP                      * */
        /*        ************************************************************** */
        /*        It is assumed that IFIRST < ILAST. */
    L400:
        ++iiter;
        ++ziter;
        if (!lschr) {
            ifrstm = ifirst;
        }
        if (ifirst + 1 == ilast) {
            /*           Special case -- 2x2 block. */
            j = ilast - 1;
            if (titer < 2) {
                ++titer;
                /*              Try to deflate the 2-by-2 problem. */
                pdw = pfree + 1;
                i__2 = *k;
                for (l = 1; l <= i__2; ++l) {
                    dwork[pdw] = a[j + (j + l * a_dim2) * a_dim1];
                    dwork[pdw + 1] = a[j + 1 + (j + l * a_dim2) * a_dim1];
                    dwork[pdw + 2] = a[j + (j + 1 + l * a_dim2) * a_dim1];
                    dwork[pdw + 3] = a[j + 1 + (j + 1 + l * a_dim2) * a_dim1];
                    pdw += 4;
                    /* L410: */
                }
                mb03be_(k, &iwork[mapa + 1], &s[1], &sinv, &dwork[pfree + 1], &c__2, &c__2);
                /* Computing MAX */
                d__5 = (d__2 = dwork[pfree + 1], abs(d__2)),
                d__6 = (d__3 = dwork[pfree + 3], abs(d__3)), d__5 = max(d__5, d__6),
                d__6 = (d__4 = dwork[pfree + 4], abs(d__4));
                if ((d__1 = dwork[pfree + 2], abs(d__1)) < ulp * max(d__5, d__6)) {
                    /*                 Construct a perfect shift polynomial. This may fail, */
                    /*                 so we try it twice (indicated by TITER). */
                    cs1 = 1.;
                    sn1 = 1.;
                    for (l = *k; l >= 2; --l) {
                        aind = iwork[mapa + l];
                        temp = dwork[pfree + (aind << 2)];
                        if (s[aind] == sinv) {
                            d__1 = cs1 * a[j + (j + aind * a_dim2) * a_dim1];
                            d__2 = sn1 * temp;
                            dlartg_(&d__1, &d__2, &cs1, &sn1, &temp);
                        } else {
                            d__1 = cs1 * temp;
                            d__2 = sn1 * a[j + (j + aind * a_dim2) * a_dim1];
                            dlartg_(&d__1, &d__2, &cs1, &sn1, &temp);
                        }
                        /* L420: */
                    }
                    aind = iwork[mapa + 1];
                    temp = dwork[pfree + (aind << 2)];
                    d__1 = a[j + (j + aind * a_dim2) * a_dim1] * cs1 - temp * sn1;
                    d__2 = a[j + 1 + (j + aind * a_dim2) * a_dim1] * cs1;
                    dlartg_(&d__1, &d__2, &cs1, &sn1, &temp);
                    goto L460;
                }
            }
            /*           Looks like a complex block. */
            /*           1. Compute the product SVD of the triangular matrices */
            /*             (optionally). */
            if (lsvd) {
                mb03bc_(k, &iwork[mapa + 1], &s[1], &sinv, &a[j + (j + a_dim2) * a_dim1], lda1,
                    lda2, macpar, &dwork[pfree + 1], &dwork[pfree + *k + 1],
                    &dwork[pfree + (*k << 1) + 1]);
                /*              Update factors and transformations. */
                aind = iwork[mapa + 1];
                cs2 = dwork[pfree + 1];
                sn2 = dwork[pfree + *k + 1];
                i__2 = ilastm - ifrstm + 1;
                drot_(&i__2, &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1,
                    &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1, &cs2, &sn2);
                i__2 = *k;
                for (l = 2; l <= i__2; ++l) {
                    aind = iwork[mapa + l];
                    if (lcmpq) {
                        qi = iwork[mapq + l];
                    } else if (lparq) {
                        qi = (i__3 = qind[iwork[mapq + l]], abs(i__3));
                    }
                    if (qi != 0) {
                        drot_(n, &q[(j + qi * q_dim2) * q_dim1 + 1], &c__1,
                            &q[(j + 1 + qi * q_dim2) * q_dim1 + 1], &c__1, &cs2, &sn2);
                    }
                    cs1 = cs2;
                    sn1 = sn2;
                    cs2 = dwork[pfree + l];
                    sn2 = dwork[pfree + *k + l];
                    if (s[aind] == sinv) {
                        i__3 = ilastm - j - 1;
                        drot_(&i__3, &a[j + (j + 2 + aind * a_dim2) * a_dim1], lda1,
                            &a[j + 1 + (j + 2 + aind * a_dim2) * a_dim1], lda1, &cs1, &sn1);
                        i__3 = j - ifrstm;
                        drot_(&i__3, &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1,
                            &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1, &cs2, &sn2);
                    } else {
                        i__3 = ilastm - j - 1;
                        drot_(&i__3, &a[j + (j + 2 + aind * a_dim2) * a_dim1], lda1,
                            &a[j + 1 + (j + 2 + aind * a_dim2) * a_dim1], lda1, &cs2, &sn2);
                        i__3 = j - ifrstm;
                        drot_(&i__3, &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1,
                            &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1, &cs1, &sn1);
                    }
                    /* L430: */
                }
                if (lcmpq) {
                    qi = iwork[mapq + 1];
                } else if (lparq) {
                    qi = (i__2 = qind[iwork[mapq + 1]], abs(i__2));
                }
                if (qi != 0) {
                    drot_(n, &q[(j + qi * q_dim2) * q_dim1 + 1], &c__1,
                        &q[(j + 1 + qi * q_dim2) * q_dim1 + 1], &c__1, &cs2, &sn2);
                }
                aind = iwork[mapa + 1];
                i__2 = ilastm - j + 1;
                drot_(&i__2, &a[j + (j + aind * a_dim2) * a_dim1], lda1,
                    &a[j + 1 + (j + aind * a_dim2) * a_dim1], lda1, &cs2, &sn2);
            }
            /*           2. Compute complex eigenvalues. */
            mb03bb_(&base, &lgbas, &ulp, k, &iwork[mapa + 1], &s[1], &sinv,
                &a[j + (j + a_dim2) * a_dim1], lda1, lda2, &alphar[j], &alphai[j], &beta[j],
                &scal[j], &dwork[pfree + 1], &ierr);
            if (ierr == 1) {
                /*              The single shift periodic QZ did not converge, set */
                /*              IWARN = J to indicate that the eigenvalues are not */
                /*              assigned. */
                *iwarn = max(j, *iwarn);
            }
            /*           Go to next block and reset counters. */
            ilast = ifirst - 1;
            if (ilast < *ilo) {
                goto L500;
            }
            iiter = 0;
            titer = 0;
            if (ziter != -1) {
                ziter = 0;
            }
            if (!lschr) {
                ilastm = ilast;
                if (ifrstm > ilast) {
                    ifrstm = *ilo;
                }
            }
            goto L480;
        }
        /*        Now, it is assumed that ILAST-IFIRST+1 >= 3. */
        /*        Complex double shift. */
        if (iiter % 30 == 0) {
            /*           Exceptional shift. */
            dlarnv_(&c__3, iseed, &c__1, &temp);
            dlarnv_(&c__3, iseed, &c__1, &temp2);
            dlartg_(&temp, &temp2, &cs1, &sn1, &temp3);
            dlarnv_(&c__3, iseed, &c__1, &temp);
            dlarnv_(&c__3, iseed, &c__1, &temp2);
            dlartg_(&temp, &temp2, &cs2, &sn2, &temp3);
        } else {
            i__2 = ilast - ifirst + 1;
            mb03ad_("Double", k, &i__2, &iwork[mapa + 1], &s[1], &sinv,
                &a[ifirst + (ifirst + a_dim2) * a_dim1], lda1, lda2, &cs1, &sn1, &cs2, &sn2, 6L);
        }
        /*        Do the sweeps. */
        i__2 = ilast - 3;
        for (j1 = ifirst - 1; j1 <= i__2; ++j1) {
            j = j1 + 1;
            aind = iwork[mapa + 1];
            if (lcmpq) {
                qi = iwork[mapq + 1];
            } else if (lparq) {
                qi = (i__3 = qind[iwork[mapq + 1]], abs(i__3));
            }
            /*           Create a bulge if J1 = IFIRST - 1, otherwise chase the */
            /*           bulge. */
            if (j1 < ifirst) {
                i__3 = ilastm - j + 1;
                drot_(&i__3, &a[j + 1 + (j + aind * a_dim2) * a_dim1], lda1,
                    &a[j + 2 + (j + aind * a_dim2) * a_dim1], lda1, &cs2, &sn2);
                i__3 = ilastm - j + 1;
                drot_(&i__3, &a[j + (j + aind * a_dim2) * a_dim1], lda1,
                    &a[j + 1 + (j + aind * a_dim2) * a_dim1], lda1, &cs1, &sn1);
            } else {
                temp = a[j + 1 + (j - 1 + aind * a_dim2) * a_dim1];
                dlartg_(&temp, &a[j + 2 + (j - 1 + aind * a_dim2) * a_dim1], &cs2, &sn2, &temp2);
                temp = a[j + (j - 1 + aind * a_dim2) * a_dim1];
                dlartg_(&temp, &temp2, &cs1, &sn1, &a[j + (j - 1 + aind * a_dim2) * a_dim1]);
                a[j + 1 + (j - 1 + aind * a_dim2) * a_dim1] = 0.;
                a[j + 2 + (j - 1 + aind * a_dim2) * a_dim1] = 0.;
                i__3 = ilastm - j + 1;
                drot_(&i__3, &a[j + 1 + (j + aind * a_dim2) * a_dim1], lda1,
                    &a[j + 2 + (j + aind * a_dim2) * a_dim1], lda1, &cs2, &sn2);
                i__3 = ilastm - j + 1;
                drot_(&i__3, &a[j + (j + aind * a_dim2) * a_dim1], lda1,
                    &a[j + 1 + (j + aind * a_dim2) * a_dim1], lda1, &cs1, &sn1);
            }
            if (qi != 0) {
                drot_(n, &q[(j + 1 + qi * q_dim2) * q_dim1 + 1], &c__1,
                    &q[(j + 2 + qi * q_dim2) * q_dim1 + 1], &c__1, &cs2, &sn2);
                drot_(n, &q[(j + qi * q_dim2) * q_dim1 + 1], &c__1,
                    &q[(j + 1 + qi * q_dim2) * q_dim1 + 1], &c__1, &cs1, &sn1);
            }
            /*           Propagate information from the right to A_1. */
            for (l = *k; l >= 2; --l) {
                aind = iwork[mapa + l];
                if (s[aind] == sinv) {
                    i__3 = j + 3 - ifrstm;
                    drot_(&i__3, &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1,
                        &a[ifrstm + (j + 2 + aind * a_dim2) * a_dim1], &c__1, &cs2, &sn2);
                    temp = a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1];
                    dlartg_(&temp, &a[j + 2 + (j + 1 + aind * a_dim2) * a_dim1], &cs2, &sn2,
                        &a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1]);
                    a[j + 2 + (j + 1 + aind * a_dim2) * a_dim1] = 0.;
                    i__3 = ilastm - j - 1;
                    drot_(&i__3, &a[j + 1 + (j + 2 + aind * a_dim2) * a_dim1], lda1,
                        &a[j + 2 + (j + 2 + aind * a_dim2) * a_dim1], lda1, &cs2, &sn2);
                    i__3 = j + 2 - ifrstm;
                    drot_(&i__3, &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1,
                        &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1, &cs1, &sn1);
                    temp = a[j + (j + aind * a_dim2) * a_dim1];
                    dlartg_(&temp, &a[j + 1 + (j + aind * a_dim2) * a_dim1], &cs1, &sn1,
                        &a[j + (j + aind * a_dim2) * a_dim1]);
                    a[j + 1 + (j + aind * a_dim2) * a_dim1] = 0.;
                    i__3 = ilastm - j;
                    drot_(&i__3, &a[j + (j + 1 + aind * a_dim2) * a_dim1], lda1,
                        &a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1], lda1, &cs1, &sn1);
                } else {
                    i__3 = ilastm - j + 1;
                    drot_(&i__3, &a[j + 1 + (j + aind * a_dim2) * a_dim1], lda1,
                        &a[j + 2 + (j + aind * a_dim2) * a_dim1], lda1, &cs2, &sn2);
                    temp = a[j + 2 + (j + 2 + aind * a_dim2) * a_dim1];
                    dlartg_(&temp, &a[j + 2 + (j + 1 + aind * a_dim2) * a_dim1], &cs2, &sn2,
                        &a[j + 2 + (j + 2 + aind * a_dim2) * a_dim1]);
                    a[j + 2 + (j + 1 + aind * a_dim2) * a_dim1] = 0.;
                    sn2 = -sn2;
                    i__3 = j + 2 - ifrstm;
                    drot_(&i__3, &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1,
                        &a[ifrstm + (j + 2 + aind * a_dim2) * a_dim1], &c__1, &cs2, &sn2);
                    i__3 = ilastm - j + 1;
                    drot_(&i__3, &a[j + (j + aind * a_dim2) * a_dim1], lda1,
                        &a[j + 1 + (j + aind * a_dim2) * a_dim1], lda1, &cs1, &sn1);
                    temp = a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1];
                    dlartg_(&temp, &a[j + 1 + (j + aind * a_dim2) * a_dim1], &cs1, &sn1,
                        &a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1]);
                    a[j + 1 + (j + aind * a_dim2) * a_dim1] = 0.;
                    sn1 = -sn1;
                    i__3 = j + 1 - ifrstm;
                    drot_(&i__3, &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1,
                        &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1, &cs1, &sn1);
                }
                if (lcmpq) {
                    qi = iwork[mapq + l];
                } else if (lparq) {
                    qi = (i__3 = qind[iwork[mapq + l]], abs(i__3));
                }
                if (qi != 0) {
                    drot_(n, &q[(j + 1 + qi * q_dim2) * q_dim1 + 1], &c__1,
                        &q[(j + 2 + qi * q_dim2) * q_dim1 + 1], &c__1, &cs2, &sn2);
                    drot_(n, &q[(j + qi * q_dim2) * q_dim1 + 1], &c__1,
                        &q[(j + 1 + qi * q_dim2) * q_dim1 + 1], &c__1, &cs1, &sn1);
                }
                /* L440: */
            }
            aind = iwork[mapa + 1];
            /* Computing MIN */
            i__3 = j + 3;
            lm = min(i__3, ilastm) - ifrstm + 1;
            drot_(&lm, &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1,
                &a[ifrstm + (j + 2 + aind * a_dim2) * a_dim1], &c__1, &cs2, &sn2);
            drot_(&lm, &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1,
                &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1, &cs1, &sn1);
            /* L450: */
        }
        /*        To avoid IF statements, there is an extra piece of code for */
        /*        the last step. */
        j = ilast - 1;
        temp = a[j + (j - 1 + aind * a_dim2) * a_dim1];
        dlartg_(&temp, &a[j + 1 + (j - 1 + aind * a_dim2) * a_dim1], &cs1, &sn1,
            &a[j + (j - 1 + aind * a_dim2) * a_dim1]);
        a[j + 1 + (j - 1 + aind * a_dim2) * a_dim1] = 0.;
    L460:
        i__2 = ilastm - j + 1;
        drot_(&i__2, &a[j + (j + aind * a_dim2) * a_dim1], lda1,
            &a[j + 1 + (j + aind * a_dim2) * a_dim1], lda1, &cs1, &sn1);
        if (lcmpq) {
            qi = iwork[mapq + 1];
        } else if (lparq) {
            qi = (i__2 = qind[iwork[mapq + 1]], abs(i__2));
        }
        if (qi != 0) {
            drot_(n, &q[(j + qi * q_dim2) * q_dim1 + 1], &c__1,
                &q[(j + 1 + qi * q_dim2) * q_dim1 + 1], &c__1, &cs1, &sn1);
        }
        /*        Propagate information from the right to A_1. */
        for (l = *k; l >= 2; --l) {
            aind = iwork[mapa + l];
            if (s[aind] == sinv) {
                i__2 = j + 2 - ifrstm;
                drot_(&i__2, &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1,
                    &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1, &cs1, &sn1);
                temp = a[j + (j + aind * a_dim2) * a_dim1];
                dlartg_(&temp, &a[j + 1 + (j + aind * a_dim2) * a_dim1], &cs1, &sn1,
                    &a[j + (j + aind * a_dim2) * a_dim1]);
                a[j + 1 + (j + aind * a_dim2) * a_dim1] = 0.;
                i__2 = ilastm - j;
                drot_(&i__2, &a[j + (j + 1 + aind * a_dim2) * a_dim1], lda1,
                    &a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1], lda1, &cs1, &sn1);
            } else {
                i__2 = ilastm - j + 1;
                drot_(&i__2, &a[j + (j + aind * a_dim2) * a_dim1], lda1,
                    &a[j + 1 + (j + aind * a_dim2) * a_dim1], lda1, &cs1, &sn1);
                temp = a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1];
                dlartg_(&temp, &a[j + 1 + (j + aind * a_dim2) * a_dim1], &cs1, &sn1,
                    &a[j + 1 + (j + 1 + aind * a_dim2) * a_dim1]);
                a[j + 1 + (j + aind * a_dim2) * a_dim1] = 0.;
                sn1 = -sn1;
                i__2 = j + 1 - ifrstm;
                drot_(&i__2, &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1,
                    &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1, &cs1, &sn1);
            }
            if (lcmpq) {
                qi = iwork[mapq + l];
            } else if (lparq) {
                qi = (i__2 = qind[iwork[mapq + l]], abs(i__2));
            }
            if (qi != 0) {
                drot_(n, &q[(j + qi * q_dim2) * q_dim1 + 1], &c__1,
                    &q[(j + 1 + qi * q_dim2) * q_dim1 + 1], &c__1, &cs1, &sn1);
            }
            /* L470: */
        }
        aind = iwork[mapa + 1];
        i__2 = ilastm - ifrstm + 1;
        drot_(&i__2, &a[ifrstm + (j + aind * a_dim2) * a_dim1], &c__1,
            &a[ifrstm + (j + 1 + aind * a_dim2) * a_dim1], &c__1, &cs1, &sn1);
        /*        End of iteration loop. */
    L480:
        /* L490: */
        ;
    }
    /*     Drop through = non-convergence. */
    *info = ilast;
    goto L520;
    /*     Successful completion of all QZ steps. */
L500:
    /*     Set eigenvalues 1:ILO-1. */
    i__1 = *ilo - 1;
    for (j = 1; j <= i__1; ++j) {
        i__2 = *lda1 * *lda2;
        ma01bd_(&base, &lgbas, k, &s[1], &a[j + (j + a_dim2) * a_dim1], &i__2, &alphar[j], &beta[j],
            &scal[j]);
        alphai[j] = 0.;
        /* L510: */
    }
L520:
    dwork[1] = (doublereal)optdw;
    iwork[1] = optiw;
    return 0;
    /* *** Last line of MB03BD *** */
} /* mb03bd_ */
