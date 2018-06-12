/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static doublereal c_b17 = -1.;
static doublereal c_b18 = 0.;
static integer c_n1 = -1;
static doublereal c_b24 = 1.;
static integer c__1 = 1;
static integer c__0 = 0;

EXPORTSYMBOL /* Subroutine */ int ab09jv_(job, dico, jobev, stbchk, n, m, p, nv, pv, a, lda, b, ldb,
    c__, ldc, d__, ldd, av, ldav, ev, ldev, bv, ldbv, cv, ldcv, dv, lddv, iwork, dwork, ldwork,
    info, job_len, dico_len, jobev_len, stbchk_len) char *job,
    *dico, *jobev, *stbchk;
integer *n, *m, *p, *nv, *pv;
doublereal* a;
integer* lda;
doublereal* b;
integer* ldb;
doublereal* c__;
integer* ldc;
doublereal* d__;
integer* ldd;
doublereal* av;
integer* ldav;
doublereal* ev;
integer* ldev;
doublereal* bv;
integer* ldbv;
doublereal* cv;
integer* ldcv;
doublereal* dv;
integer *lddv, *iwork;
doublereal* dwork;
integer *ldwork, *info;
ftnlen job_len;
ftnlen dico_len;
ftnlen jobev_len;
ftnlen stbchk_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, av_dim1, av_offset, b_dim1, b_offset, bv_dim1, bv_offset, c_dim1,
        c_offset, cv_dim1, cv_offset, d_dim1, d_offset, dv_dim1, dv_offset, ev_dim1, ev_offset,
        i__1, i__2, i__3, i__4, i__5, i__6;
    doublereal d__1, d__2;
    /* Builtin functions */
    double sqrt();
    /* Local variables */
    static integer sdim, ierr, ldwn;
    static doublereal work;
    static integer i__;
    static doublereal alpha, scale;
    extern /* Subroutine */ int dgemm_(), dgges_(), ab09jx_();
    extern logical lsame_();
    extern /* Subroutine */ int tb01wd_();
    static logical discr, conjs;
    extern /* Subroutine */ int dswap_(), sb04py_();
    static logical bwork[1];
    static char stdom[1];
    static integer ia, kb, kc, ke, kf, kq;
    extern doublereal dlamch_(), dlange_();
    static integer kw, lw, kz;
    static logical stabck;
    extern logical delctg_();
    extern /* Subroutine */ int dlacpy_(), dlaset_(), xerbla_();
    static doublereal tolinf;
    extern /* Subroutine */ int dtgsyl_();
    static logical unitev;
    static char evtype[1];
    extern /* Subroutine */ int dtrsyl_();
    static doublereal dif;
    static integer kai, kar, ldw;
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
    /*     To construct a state-space representation (A,BS,CS,DS) of the */
    /*     projection of V*G or conj(V)*G containing the poles of G, from the */
    /*     state-space representations (A,B,C,D) and (AV-lambda*EV,BV,CV,DV), */
    /*     of the transfer-function matrices G and V, respectively. */
    /*     G is assumed to be a stable transfer-function matrix and */
    /*     the state matrix A must be in a real Schur form. */
    /*     When computing the stable projection of V*G, it is assumed */
    /*     that G and V have completely distinct poles. */
    /*     When computing the stable projection of conj(V)*G, it is assumed */
    /*     that G and conj(V) have completely distinct poles. */
    /*     Note: For a transfer-function matrix G, conj(G) denotes the */
    /*     conjugate of G given by G'(-s) for a continuous-time system or */
    /*     G'(1/z) for a discrete-time system. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     JOB     CHARACTER*1 */
    /*             Specifies the projection to be computed as follows: */
    /*             = 'V':  compute the projection of V*G containing */
    /*                     the poles of G; */
    /*             = 'C':  compute the projection of conj(V)*G containing */
    /*                     the poles of G. */
    /*     DICO    CHARACTER*1 */
    /*             Specifies the type of the systems as follows: */
    /*             = 'C':  G and V are continuous-time systems; */
    /*             = 'D':  G and V are discrete-time systems. */
    /*     JOBEV   CHARACTER*1 */
    /*             Specifies whether EV is a general square or an identity */
    /*             matrix as follows: */
    /*             = 'G':  EV is a general square matrix; */
    /*             = 'I':  EV is the identity matrix. */
    /*     STBCHK  CHARACTER*1 */
    /*             Specifies whether stability/antistability of V is to be */
    /*             checked as follows: */
    /*             = 'C':  check stability if JOB = 'C' or antistability if */
    /*                     JOB = 'V'; */
    /*             = 'N':  do not check stability or antistability. */
    /*     Input/Output Parameters */
    /*     N       (input) INTEGER */
    /*             The dimension of the state vector of the system with */
    /*             the transfer-function matrix G.  N >= 0. */
    /*     M       (input) INTEGER */
    /*             The dimension of the input vector of the system with */
    /*             the transfer-function matrix G.  M >= 0. */
    /*     P       (input) INTEGER */
    /*             The dimension of the output vector of the system with the */
    /*             transfer-function matrix G, and also the dimension of */
    /*             the input vector if JOB = 'V', or of the output vector */
    /*             if JOB = 'C', of the system with the transfer-function */
    /*             matrix V.  P >= 0. */
    /*     NV      (input) INTEGER */
    /*             The dimension of the state vector of the system with */
    /*             the transfer-function matrix V.  NV >= 0. */
    /*     PV      (input) INTEGER */
    /*             The dimension of the output vector, if JOB = 'V', or */
    /*             of the input vector, if JOB = 'C', of the system with */
    /*             the transfer-function matrix V.  PV >= 0. */
    /*     A       (input) DOUBLE PRECISION array, dimension (LDA,N) */
    /*             The leading N-by-N part of this array must contain the */
    /*             state matrix A of the system with the transfer-function */
    /*             matrix G in a real Schur form. */
    /*     LDA     INTEGER */
    /*             The leading dimension of the array A.  LDA >= MAX(1,N). */
    /*     B       (input) DOUBLE PRECISION array, dimension (LDB,M) */
    /*             The leading N-by-M part of this array must contain */
    /*             the input/state matrix B of the system with the */
    /*             transfer-function matrix G. The matrix BS is equal to B. */
    /*     LDB     INTEGER */
    /*             The leading dimension of the array B.  LDB >= MAX(1,N). */
    /*     C       (input/output) DOUBLE PRECISION array, dimension (LDC,N) */
    /*             On entry, the leading P-by-N part of this array must */
    /*             contain the output matrix C of the system with the */
    /*             transfer-function matrix G. */
    /*             On exit, if INFO = 0, the leading PV-by-N part of this */
    /*             array contains the output matrix CS of the projection of */
    /*             V*G, if JOB = 'V', or of conj(V)*G, if JOB = 'C'. */
    /*     LDC     INTEGER */
    /*             The leading dimension of the array C.  LDC >= MAX(1,P,PV). */
    /*     D       (input/output) DOUBLE PRECISION array, dimension (LDD,M) */
    /*             On entry, the leading P-by-M part of this array must */
    /*             contain the feedthrough matrix D of the system with the */
    /*             transfer-function matrix G. */
    /*             On exit, if INFO = 0, the leading PV-by-M part of */
    /*             this array contains the feedthrough matrix DS of the */
    /*             projection of V*G, if JOB = 'V', or of conj(V)*G, */
    /*             if JOB = 'C'. */
    /*     LDD     INTEGER */
    /*             The leading dimension of the array D.  LDD >= MAX(1,P,PV). */
    /*     AV      (input/output) DOUBLE PRECISION array, dimension (LDAV,NV) */
    /*             On entry, the leading NV-by-NV part of this array must */
    /*             contain the state matrix AV of the system with the */
    /*             transfer-function matrix V. */
    /*             On exit, if INFO = 0, the leading NV-by-NV part of this */
    /*             array contains a condensed matrix as follows: */
    /*             if JOBEV = 'I', it contains the real Schur form of AV; */
    /*             if JOBEV = 'G' and JOB = 'V', it contains a quasi-upper */
    /*             triangular matrix representing the real Schur matrix */
    /*             in the real generalized Schur form of the pair (AV,EV); */
    /*             if JOBEV = 'G', JOB = 'C' and DICO = 'C', it contains a */
    /*             quasi-upper triangular matrix corresponding to the */
    /*             generalized real Schur form of the pair (AV',EV'); */
    /*             if JOBEV = 'G', JOB = 'C' and DICO = 'D', it contains an */
    /*             upper triangular matrix corresponding to the generalized */
    /*             real Schur form of the pair (EV',AV'). */
    /*     LDAV    INTEGER */
    /*             The leading dimension of the array AV.  LDAV >= MAX(1,NV). */
    /*     EV      (input/output) DOUBLE PRECISION array, dimension (LDEV,NV) */
    /*             On entry, if JOBEV = 'G', the leading NV-by-NV part of */
    /*             this array must contain the descriptor matrix EV of the */
    /*             system with the transfer-function matrix V. */
    /*             If JOBEV = 'I', EV is assumed to be an identity matrix */
    /*             and is not referenced. */
    /*             On exit, if INFO = 0 and JOBEV = 'G', the leading NV-by-NV */
    /*             part of this array contains a condensed matrix as follows: */
    /*             if JOB = 'V', it contains an upper triangular matrix */
    /*             corresponding to the real generalized Schur form of the */
    /*             pair (AV,EV); */
    /*             if JOB = 'C' and DICO = 'C', it contains an upper */
    /*             triangular matrix corresponding to the generalized real */
    /*             Schur form of the pair (AV',EV'); */
    /*             if JOB = 'C' and DICO = 'D', it contains a quasi-upper */
    /*             triangular matrix corresponding to the generalized */
    /*             real Schur form of the pair (EV',AV'). */
    /*     LDEV    INTEGER */
    /*             The leading dimension of the array EV. */
    /*             LDEV >= MAX(1,NV), if JOBEV = 'G'; */
    /*             LDEV >= 1,         if JOBEV = 'I'. */
    /*     BV      (input/output) DOUBLE PRECISION array, */
    /*             dimension (LDBV,MBV), where MBV = P, if JOB = 'V', and */
    /*             MBV = PV, if JOB = 'C'. */
    /*             On entry, the leading NV-by-MBV part of this array must */
    /*             contain the input matrix BV of the system with the */
    /*             transfer-function matrix V. */
    /*             On exit, if INFO = 0, the leading NV-by-MBV part of this */
    /*             array contains Q'*BV, where Q is the orthogonal matrix */
    /*             that reduces AV to the real Schur form or the left */
    /*             orthogonal matrix used to reduce the pair (AV,EV), */
    /*             (AV',EV') or (EV',AV') to the generalized real Schur form. */
    /*     LDBV    INTEGER */
    /*             The leading dimension of the array BV.  LDBV >= MAX(1,NV). */
    /*     CV      (input/output) DOUBLE PRECISION array, dimension (LDCV,NV) */
    /*             On entry, the leading PCV-by-NV part of this array must */
    /*             contain the output matrix CV of the system with the */
    /*             transfer-function matrix V, where PCV = PV, if JOB = 'V', */
    /*             or PCV = P, if JOB = 'C'. */
    /*             On exit, if INFO = 0, the leading PCV-by-NV part of this */
    /*             array contains CV*Q, where Q is the orthogonal matrix that */
    /*             reduces AV to the real Schur form, or CV*Z, where Z is the */
    /*             right orthogonal matrix used to reduce the pair (AV,EV), */
    /*             (AV',EV') or (EV',AV') to the generalized real Schur form. */
    /*     LDCV    INTEGER */
    /*             The leading dimension of the array CV. */
    /*             LDCV >= MAX(1,PV) if JOB = 'V'; */
    /*             LDCV >= MAX(1,P)  if JOB = 'C'. */
    /*     DV      (input) DOUBLE PRECISION array, */
    /*             dimension (LDDV,MBV), where MBV = P, if JOB = 'V', and */
    /*             MBV = PV, if JOB = 'C'. */
    /*             The leading PCV-by-MBV part of this array must contain */
    /*             the feedthrough matrix DV of the system with the */
    /*             transfer-function matrix V, where PCV = PV, if JOB = 'V', */
    /*             or PCV = P, if JOB = 'C'. */
    /*     LDDV    INTEGER */
    /*             The leading dimension of the array DV. */
    /*             LDDV >= MAX(1,PV) if JOB = 'V'; */
    /*             LDDV >= MAX(1,P)  if JOB = 'C'. */
    /*     Workspace */
    /*     IWORK   INTEGER array, dimension (LIWORK) */
    /*             LIWORK =   0,    if JOBEV = 'I'; */
    /*             LIWORK = NV+N+6, if JOBEV = 'G'. */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             On exit, if INFO = 0, DWORK(1) returns the optimal value */
    /*             of LDWORK. */
    /*     LDWORK  INTEGER */
    /*             The length of the array DWORK. */
    /*             LDWORK >= LW1, if JOBEV = 'I', */
    /*             LDWORK >= LW2, if JOBEV = 'G', where */
    /*               LW1 = MAX( 1, NV*(NV+5), NV*N + MAX( a, PV*N, PV*M ) ) */
    /*                     a = 0,    if DICO = 'C' or  JOB = 'V', */
    /*                     a = 2*NV, if DICO = 'D' and JOB = 'C'; */
    /*               LW2 = MAX( 2*NV*NV + MAX( 11*NV+16, P*NV, PV*NV ), */
    /*                          NV*N + MAX( NV*N+N*N, PV*N, PV*M ) ). */
    /*             For good performance, LDWORK should be larger. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             =  0:  successful exit; */
    /*             <  0:  if INFO = -i, the i-th argument had an illegal */
    /*                    value; */
    /*             =  1:  the reduction of the pair (AV,EV) to the real */
    /*                    generalized Schur form failed (JOBEV = 'G'), */
    /*                    or the reduction of the matrix AV to the real */
    /*                    Schur form failed (JOBEV = 'I); */
    /*             =  2:  the solution of the Sylvester equation failed */
    /*                    because the matrix A and the pencil AV-lambda*EV */
    /*                    have common eigenvalues (if JOB = 'V'), or the */
    /*                    pencil -AV-lambda*EV and A have common eigenvalues */
    /*                    (if JOB = 'C' and DICO = 'C'), or the pencil */
    /*                    AV-lambda*EV has an eigenvalue which is the */
    /*                    reciprocal of one of eigenvalues of A */
    /*                    (if JOB = 'C' and DICO = 'D'); */
    /*             =  3:  the solution of the Sylvester equation failed */
    /*                    because the matrices A and AV have common */
    /*                    eigenvalues (if JOB = 'V'), or the matrices A */
    /*                    and -AV have common eigenvalues (if JOB = 'C' and */
    /*                    DICO = 'C'), or the matrix A has an eigenvalue */
    /*                    which is the reciprocal of one of eigenvalues of AV */
    /*                    (if JOB = 'C' and DICO = 'D'); */
    /*             =  4:  JOB = 'V' and the pair (AV,EV) has not completely */
    /*                    unstable generalized eigenvalues, or JOB = 'C' and */
    /*                    the pair (AV,EV) has not completely stable */
    /*                    generalized eigenvalues. */
    /*     METHOD */
    /*     If JOB = 'V', the matrices of the stable projection of V*G are */
    /*     computed as */
    /*       BS = B,  CS = CV*X + DV*C,  DS = DV*D, */
    /*     where X satisfies the generalized Sylvester equation */
    /*       AV*X - EV*X*A + BV*C = 0. */
    /*     If JOB = 'C', the matrices of the stable projection of conj(V)*G */
    /*     are computed using the following formulas: */
    /*     - for a continuous-time system, the matrices BS, CS and DS of */
    /*       the stable projection are computed as */
    /*         BS = B,  CS = BV'*X + DV'*C,  DS = DV'*D, */
    /*       where X satisfies the generalized Sylvester equation */
    /*         AV'*X + EV'*X*A + CV'*C = 0. */
    /*     - for a discrete-time system, the matrices BS, CS and DS of */
    /*       the stable projection are computed as */
    /*         BS = B,  CS = BV'*X*A + DV'*C,  DS = DV'*D + BV'*X*B, */
    /*       where X satisfies the generalized Sylvester equation */
    /*         EV'*X - AV'*X*A = CV'*C. */
    /*     REFERENCES */
    /*     [1] Varga, A. */
    /*         Efficient and numerically reliable implementation of the */
    /*         frequency-weighted Hankel-norm approximation model reduction */
    /*         approach. */
    /*         Proc. 2001 ECC, Porto, Portugal, 2001. */
    /*     [2] Zhou, K. */
    /*         Frequency-weighted H-infinity norm and optimal Hankel norm */
    /*         model reduction. */
    /*         IEEE Trans. Autom. Control, vol. 40, pp. 1687-1699, 1995. */
    /*     NUMERICAL ASPECTS */
    /*     The implemented methods rely on numerically stable algorithms. */
    /*     CONTRIBUTORS */
    /*     A. Varga, German Aerospace Center, Oberpfaffenhofen, July 2000. */
    /*     D. Sima, University of Bucharest, March 2001. */
    /*     V. Sima, Research Institute for Informatics, Bucharest, Mar. 2001. */
    /*     REVISIONS */
    /*     A. Varga, German Aerospace Center, Oberpfaffenhofen, May 2001. */
    /*     V. Sima, Research Institute for Informatics, Bucharest, June 2001. */
    /*     A. Varga, German Aerospace Center, Oberpfaffenhofen, Nov. 2003. */
    /*     V. Sima, Research Institute for Informatics, Bucharest, May 2010. */
    /*     KEYWORDS */
    /*     Frequency weighting, model reduction, multivariable system, */
    /*     state-space model, state-space representation. */
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
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    c_dim1 = *ldc;
    c_offset = c_dim1 + 1;
    c__ -= c_offset;
    d_dim1 = *ldd;
    d_offset = d_dim1 + 1;
    d__ -= d_offset;
    av_dim1 = *ldav;
    av_offset = av_dim1 + 1;
    av -= av_offset;
    ev_dim1 = *ldev;
    ev_offset = ev_dim1 + 1;
    ev -= ev_offset;
    bv_dim1 = *ldbv;
    bv_offset = bv_dim1 + 1;
    bv -= bv_offset;
    cv_dim1 = *ldcv;
    cv_offset = cv_dim1 + 1;
    cv -= cv_offset;
    dv_dim1 = *lddv;
    dv_offset = dv_dim1 + 1;
    dv -= dv_offset;
    --iwork;
    --dwork;
    /* Function Body */
    conjs = lsame_(job, "C", 1L, 1L);
    discr = lsame_(dico, "D", 1L, 1L);
    unitev = lsame_(jobev, "I", 1L, 1L);
    stabck = lsame_(stbchk, "C", 1L, 1L);
    *info = 0;
    if (unitev) {
        if (discr && conjs) {
            ia = *nv << 1;
        } else {
            ia = 0;
        }
        /* Computing MAX */
        /* Computing MAX */
        i__3 = ia, i__4 = *pv * *n, i__3 = max(i__3, i__4), i__4 = *pv * *m;
        i__1 = 1, i__2 = *nv * (*nv + 5), i__1 = max(i__1, i__2), i__2 = *nv * *n + max(i__3, i__4);
        lw = max(i__1, i__2);
    } else {
        /* Computing MAX */
        /* Computing MAX */
        i__3 = *nv * 11 + 16, i__4 = *p * *nv, i__3 = max(i__3, i__4), i__4 = *pv * *nv;
        /* Computing MAX */
        i__5 = *nv * *n + *n * *n, i__6 = *pv * *n, i__5 = max(i__5, i__6), i__6 = *pv * *m;
        i__1 = (*nv << 1) * *nv + max(i__3, i__4), i__2 = *nv * *n + max(i__5, i__6);
        lw = max(i__1, i__2);
    }
    /*     Test the input scalar arguments. */
    ldwn = max(1, *n);
    ldw = max(1, *nv);
    if (!(lsame_(job, "V", 1L, 1L) || conjs)) {
        *info = -1;
    } else if (!(lsame_(dico, "C", 1L, 1L) || discr)) {
        *info = -2;
    } else if (!(lsame_(jobev, "G", 1L, 1L) || unitev)) {
        *info = -3;
    } else if (!(lsame_(stbchk, "N", 1L, 1L) || stabck)) {
        *info = -4;
    } else if (*n < 0) {
        *info = -5;
    } else if (*m < 0) {
        *info = -6;
    } else if (*p < 0) {
        *info = -7;
    } else if (*nv < 0) {
        *info = -8;
    } else if (*pv < 0) {
        *info = -9;
    } else if (*lda < ldwn) {
        *info = -11;
    } else if (*ldb < ldwn) {
        *info = -13;
    } else /* if(complicated condition) */
    {
        /* Computing MAX */
        i__1 = max(1, *p);
        if (*ldc < max(i__1, *pv)) {
            *info = -15;
        } else /* if(complicated condition) */
        {
            /* Computing MAX */
            i__1 = max(1, *p);
            if (*ldd < max(i__1, *pv)) {
                *info = -17;
            } else if (*ldav < ldw) {
                *info = -19;
            } else if (*ldev < 1 || !unitev && *ldev < *nv) {
                *info = -21;
            } else if (*ldbv < ldw) {
                *info = -23;
            } else if (!conjs && *ldcv < max(1, *pv) || conjs && *ldcv < max(1, *p)) {
                *info = -25;
            } else if (!conjs && *lddv < max(1, *pv) || conjs && *lddv < max(1, *p)) {
                *info = -27;
            } else if (*ldwork < lw) {
                *info = -30;
            }
        }
    }
    if (*info != 0) {
        /*        Error return. */
        i__1 = -(*info);
        xerbla_("AB09JV", &i__1, 6L);
        return 0;
    }
    /*     Quick return if possible. */
    if (*p == 0 || *pv == 0) {
        dwork[1] = 1.;
        return 0;
    }
    /*     Set options for stability/antistability checking. */
    if (discr) {
        alpha = 1.;
    } else {
        alpha = 0.;
    }
    work = 1.;
    tolinf = dlamch_("Precision", 9L);
    if (unitev) {
        /*        EV is the identity matrix. */
        if (*nv > 0) {
            /*           Reduce AV to the real Schur form using an orthogonal */
            /*           similarity transformation AV <- Q'*AV*Q and apply the */
            /*           transformation to BV and CV: BV <- Q'*BV and CV <- CV*Q. */
            /*           Workspace needed:  NV*(NV+5); */
            /*                              prefer larger. */
            kw = *nv * (*nv + 2) + 1;
            if (conjs) {
                *(unsigned char*)stdom = 'S';
                alpha += sqrt(tolinf);
                i__1 = *ldwork - kw + 1;
                tb01wd_(nv, pv, p, &av[av_offset], ldav, &bv[bv_offset], ldbv, &cv[cv_offset], ldcv,
                    &dwork[(*nv << 1) + 1], nv, &dwork[1], &dwork[*nv + 1], &dwork[kw], &i__1,
                    &ierr);
            } else {
                *(unsigned char*)stdom = 'U';
                alpha -= sqrt(tolinf);
                i__1 = *ldwork - kw + 1;
                tb01wd_(nv, p, pv, &av[av_offset], ldav, &bv[bv_offset], ldbv, &cv[cv_offset], ldcv,
                    &dwork[(*nv << 1) + 1], nv, &dwork[1], &dwork[*nv + 1], &dwork[kw], &i__1,
                    &ierr);
            }
            if (ierr != 0) {
                *info = 1;
                return 0;
            }
            if (stabck) {
                /*              Check stability/antistability of eigenvalues of AV. */
                ab09jx_(dico, stdom, "S", nv, &alpha, &dwork[1], &dwork[*nv + 1], &dwork[1],
                    &tolinf, &ierr, 1L, 1L, 1L);
                if (ierr != 0) {
                    *info = 4;
                    return 0;
                }
            }
            /* Computing MAX */
            d__1 = work, d__2 = dwork[kw] + (doublereal)(kw - 1);
            work = max(d__1, d__2);
        }
        kw = *nv * *n + 1;
        if (conjs) {
            /*           Compute the projection of conj(V)*G. */
            /*           Total workspace needed:  NV*N + MAX( a, PV*N, PV*M ), where */
            /*                                    a = 0,    if DICO = 'C', */
            /*                                    a = 2*NV, if DICO = 'D'. */
            /*           Compute -CV'*C. */
            /*           Workspace needed: NV*N. */
            dgemm_("T", "N", nv, n, p, &c_b17, &cv[cv_offset], ldcv, &c__[c_offset], ldc, &c_b18,
                &dwork[1], &ldw, 1L, 1L);
            if (discr) {
                /*              Compute X and SCALE satisfying */
                /*              AV'*X*A - X = -SCALE*CV'*C. */
                /*              Additional workspace needed: 2*NV. */
                sb04py_("T", "N", &c_n1, nv, n, &av[av_offset], ldav, &a[a_offset], lda, &dwork[1],
                    &ldw, &scale, &dwork[kw], &ierr, 1L, 1L);
                if (ierr != 0) {
                    *info = 3;
                    return 0;
                }
                /*              Construct CS = DV'*C + BV'*X*A/SCALE, */
                /*                        DS = DV'*D + BV'*X*B/SCALE. */
                /*              Additional workspace needed: MAX( PV*N, PV*M ). */
                /*              C <- DV'*C. */
                dgemm_("T", "N", pv, n, p, &c_b24, &dv[dv_offset], lddv, &c__[c_offset], ldc,
                    &c_b18, &dwork[kw], pv, 1L, 1L);
                dlacpy_("Full", pv, n, &dwork[kw], pv, &c__[c_offset], ldc, 4L);
                /*              D <- DV'*D. */
                dgemm_("T", "N", pv, m, p, &c_b24, &dv[dv_offset], lddv, &d__[d_offset], ldd,
                    &c_b18, &dwork[kw], pv, 1L, 1L);
                dlacpy_("Full", pv, m, &dwork[kw], pv, &d__[d_offset], ldd, 4L);
                /*              C <- C + BV'*X*A/SCALE. */
                d__1 = 1. / scale;
                dgemm_("T", "N", pv, n, nv, &d__1, &bv[bv_offset], ldbv, &dwork[1], &ldw, &c_b18,
                    &dwork[kw], pv, 1L, 1L);
                dgemm_("N", "N", pv, n, n, &c_b24, &dwork[kw], pv, &a[a_offset], lda, &c_b24,
                    &c__[c_offset], ldc, 1L, 1L);
                /*              D <- D + BV'*X*B/SCALE. */
                dgemm_("N", "N", pv, m, n, &c_b24, &dwork[kw], pv, &b[b_offset], ldb, &c_b24,
                    &d__[d_offset], ldd, 1L, 1L);
            } else {
                /*              Compute X and SCALE satisfying */
                /*              AV'*X + X*A + SCALE*CV'*C = 0. */
                if (*n > 0) {
                    dtrsyl_("T", "N", &c__1, nv, n, &av[av_offset], ldav, &a[a_offset], lda,
                        &dwork[1], &ldw, &scale, &ierr, 1L, 1L);
                    if (ierr != 0) {
                        *info = 3;
                        return 0;
                    }
                }
                /*              Construct CS = DV'*C + BV'*X/SCALE, */
                /*                        DS = DV'*D. */
                /*              Additional workspace needed: MAX( PV*N, PV*M ). */
                /*              Construct C <- DV'*C + BV'*X/SCALE. */
                dgemm_("T", "N", pv, n, p, &c_b24, &dv[dv_offset], lddv, &c__[c_offset], ldc,
                    &c_b18, &dwork[kw], pv, 1L, 1L);
                dlacpy_("Full", pv, n, &dwork[kw], pv, &c__[c_offset], ldc, 4L);
                d__1 = 1. / scale;
                dgemm_("T", "N", pv, n, nv, &d__1, &bv[bv_offset], ldbv, &dwork[1], &ldw, &c_b24,
                    &c__[c_offset], ldc, 1L, 1L);
                /*              Construct D <- DV'*D. */
                dgemm_("T", "N", pv, m, p, &c_b24, &dv[dv_offset], lddv, &d__[d_offset], ldd,
                    &c_b18, &dwork[kw], pv, 1L, 1L);
                dlacpy_("Full", pv, m, &dwork[kw], pv, &d__[d_offset], ldd, 4L);
            }
        } else {
            /*           Compute the projection of V*G. */
            /*           Total workspace needed:  NV*N + MAX( PV*N, PV*M ). */
            /*           Compute -BV*C. */
            /*           Workspace needed: NV*N. */
            dgemm_("N", "N", nv, n, p, &c_b17, &bv[bv_offset], ldbv, &c__[c_offset], ldc, &c_b18,
                &dwork[1], &ldw, 1L, 1L);
            /*           Compute X and SCALE satisfying */
            /*           AV*X - X*A + SCALE*BV*C = 0. */
            if (*n > 0) {
                dtrsyl_("N", "N", &c_n1, nv, n, &av[av_offset], ldav, &a[a_offset], lda, &dwork[1],
                    &ldw, &scale, &ierr, 1L, 1L);
                if (ierr != 0) {
                    *info = 3;
                    return 0;
                }
            }
            /*           Construct CS = DV*C + CV*X/SCALE, */
            /*                     DS = DV*D. */
            /*           Additional workspace needed: MAX( PV*N, PV*M ). */
            /*           Construct C <- DV*C + CV*X/SCALE. */
            dgemm_("N", "N", pv, n, p, &c_b24, &dv[dv_offset], lddv, &c__[c_offset], ldc, &c_b18,
                &dwork[kw], pv, 1L, 1L);
            dlacpy_("Full", pv, n, &dwork[kw], pv, &c__[c_offset], ldc, 4L);
            d__1 = 1. / scale;
            dgemm_("N", "N", pv, n, nv, &d__1, &cv[cv_offset], ldcv, &dwork[1], &ldw, &c_b24,
                &c__[c_offset], ldc, 1L, 1L);
            /*           Construct D <- DV*D. */
            dgemm_("N", "N", pv, m, p, &c_b24, &dv[dv_offset], lddv, &d__[d_offset], ldd, &c_b18,
                &dwork[kw], pv, 1L, 1L);
            dlacpy_("Full", pv, m, &dwork[kw], pv, &d__[d_offset], ldd, 4L);
        }
    } else {
        /*        EV is a general matrix. */
        if (*nv > 0) {
            tolinf *= dlange_("1", nv, nv, &ev[ev_offset], ldev, &dwork[1], 1L);
            /*           Reduce (AV,EV), or (AV',EV') or (EV',AV') to a generalized */
            /*           real Schur form using an orthogonal equivalence */
            /*           transformation and apply the orthogonal transformation */
            /*           appropriately to BV and CV, or CV' and BV'. */
            /*           Workspace needed:  2*NV*NV + MAX( 11*NV+16, NV*P, NV*PV ); */
            /*                              prefer larger. */
            kq = 1;
            kz = kq + *nv * *nv;
            kar = kz + *nv * *nv;
            kai = kar + *nv;
            kb = kai + *nv;
            kw = kb + *nv;
            if (conjs) {
                *(unsigned char*)stdom = 'S';
                alpha += sqrt(tolinf);
                /*              Transpose AV and EV, if non-scalar. */
                i__1 = *nv - 1;
                for (i__ = 1; i__ <= i__1; ++i__) {
                    i__2 = *nv - i__;
                    dswap_(&i__2, &av[i__ + 1 + i__ * av_dim1], &c__1,
                        &av[i__ + (i__ + 1) * av_dim1], ldav);
                    i__2 = *nv - i__;
                    dswap_(&i__2, &ev[i__ + 1 + i__ * ev_dim1], &c__1,
                        &ev[i__ + (i__ + 1) * ev_dim1], ldev);
                    /* L10: */
                }
                if (discr) {
                    /*                 Reduce (EV',AV') to a generalized real Schur form */
                    /*                 using orthogonal transformation matrices Q and Z */
                    /*                 such that Q'*EV'*Z results in a quasi-triangular form */
                    /*                 and Q'*AV'*Z results upper triangular. */
                    /*                 Total workspace needed: 2*NV*NV + 11*NV + 16. */
                    *(unsigned char*)evtype = 'R';
                    i__1 = *ldwork - kw + 1;
                    dgges_("Vectors", "Vectors", "Not ordered", delctg_, nv, &ev[ev_offset], ldev,
                        &av[av_offset], ldav, &sdim, &dwork[kar], &dwork[kai], &dwork[kb],
                        &dwork[kq], &ldw, &dwork[kz], &ldw, &dwork[kw], &i__1, bwork, &ierr, 7L, 7L,
                        11L);
                } else {
                    /*                 Reduce (AV',EV') to a generalized real Schur form */
                    /*                 using orthogonal transformation matrices Q and Z */
                    /*                 such that Q'*AV'*Z results in a quasi-triangular form */
                    /*                 and Q'*EV'*Z results upper triangular. */
                    /*                 Total workspace needed: 2*NV*NV + 11*NV + 16. */
                    *(unsigned char*)evtype = 'G';
                    i__1 = *ldwork - kw + 1;
                    dgges_("Vectors", "Vectors", "Not ordered", delctg_, nv, &av[av_offset], ldav,
                        &ev[ev_offset], ldev, &sdim, &dwork[kar], &dwork[kai], &dwork[kb],
                        &dwork[kq], &ldw, &dwork[kz], &ldw, &dwork[kw], &i__1, bwork, &ierr, 7L, 7L,
                        11L);
                }
                if (ierr != 0) {
                    *info = 1;
                    return 0;
                }
                if (stabck) {
                    /*                 Check stability/antistability of generalized */
                    /*                 eigenvalues of the pair (AV,EV). */
                    ab09jx_(dico, stdom, evtype, nv, &alpha, &dwork[kar], &dwork[kai], &dwork[kb],
                        &tolinf, &ierr, 1L, 1L, 1L);
                    if (ierr != 0) {
                        *info = 4;
                        return 0;
                    }
                }
                /* Computing MAX */
                d__1 = work, d__2 = dwork[kw] + (doublereal)(kw - 1);
                work = max(d__1, d__2);
                /*              Compute Z'*BV and CV*Q. */
                /*              Total workspace needed: 2*NV*NV + NV*MAX(P,PV). */
                kw = kar;
                dlacpy_("Full", nv, pv, &bv[bv_offset], ldbv, &dwork[kw], &ldw, 4L);
                dgemm_("T", "N", nv, pv, nv, &c_b24, &dwork[kz], &ldw, &dwork[kw], &ldw, &c_b18,
                    &bv[bv_offset], ldbv, 1L, 1L);
                dlacpy_("Full", p, nv, &cv[cv_offset], ldcv, &dwork[kw], p, 4L);
                dgemm_("N", "N", p, nv, nv, &c_b24, &dwork[kw], p, &dwork[kq], &ldw, &c_b18,
                    &cv[cv_offset], ldcv, 1L, 1L);
            } else {
                /*              Reduce (AV,EV) to a generalized real Schur form */
                /*              using orthogonal transformation matrices Q and Z */
                /*              such that Q'*AV*Z results in a quasi-triangular form */
                /*              and Q'*EV*Z results upper triangular. */
                /*              Total workspace needed: 2*NV*NV + 11*NV + 16. */
                *(unsigned char*)stdom = 'U';
                *(unsigned char*)evtype = 'G';
                alpha -= sqrt(tolinf);
                i__1 = *ldwork - kw + 1;
                dgges_("Vectors", "Vectors", "Not ordered", delctg_, nv, &av[av_offset], ldav,
                    &ev[ev_offset], ldev, &sdim, &dwork[kar], &dwork[kai], &dwork[kb], &dwork[kq],
                    &ldw, &dwork[kz], &ldw, &dwork[kw], &i__1, bwork, &ierr, 7L, 7L, 11L);
                if (ierr != 0) {
                    *info = 1;
                    return 0;
                }
                if (stabck) {
                    /*                 Check stability/antistability of generalized */
                    /*                 eigenvalues of the pair (AV,EV). */
                    ab09jx_(dico, stdom, evtype, nv, &alpha, &dwork[kar], &dwork[kai], &dwork[kb],
                        &tolinf, &ierr, 1L, 1L, 1L);
                    if (ierr != 0) {
                        *info = 4;
                        return 0;
                    }
                }
                /* Computing MAX */
                d__1 = work, d__2 = dwork[kw] + (doublereal)(kw - 1);
                work = max(d__1, d__2);
                /*              Compute Q'*BV and CV*Z. */
                /*              Total workspace needed: 2*NV*NV + NV*MAX(P,PV). */
                kw = kar;
                dlacpy_("Full", nv, p, &bv[bv_offset], ldbv, &dwork[kw], &ldw, 4L);
                dgemm_("T", "N", nv, p, nv, &c_b24, &dwork[kq], &ldw, &dwork[kw], &ldw, &c_b18,
                    &bv[bv_offset], ldbv, 1L, 1L);
                dlacpy_("Full", pv, nv, &cv[cv_offset], ldcv, &dwork[kw], pv, 4L);
                dgemm_("N", "N", pv, nv, nv, &c_b24, &dwork[kw], pv, &dwork[kz], &ldw, &c_b18,
                    &cv[cv_offset], ldcv, 1L, 1L);
            }
            /* Computing MAX */
            d__1 = work, d__2 = (doublereal)((*nv << 1) * *nv + *nv * max(*p, *pv));
            work = max(d__1, d__2);
        }
        kc = 1;
        kf = kc + *nv * *n;
        ke = kf + *nv * *n;
        kw = ke + *n * *n;
        dlaset_("Full", nv, n, &c_b18, &c_b18, &dwork[kf], &ldw, 4L);
        if (conjs) {
            /*           Compute the projection of conj(V)*G. */
            /*           Total workspace needed: NV*N + MAX( NV*N+N*N, PV*N, PV*M ) */
            /*           Compute CV'*C. */
            /*           Workspace needed: NV*N. */
            dgemm_("T", "N", nv, n, p, &c_b24, &cv[cv_offset], ldcv, &c__[c_offset], ldc, &c_b18,
                &dwork[kc], &ldw, 1L, 1L);
            if (discr) {
                /*              Compute X and SCALE satisfying */
                /*              EV'*X - AV'*X*A = SCALE*CV'*C by solving equivalently */
                /*              EV'*X - Y*A = SCALE*CV'*C, */
                /*              AV'*X - Y   = 0. */
                /*              Additional workspace needed: */
                /*              real    NV*N + N*N; */
                /*              integer NV+N+6. */
                if (*n > 0) {
                    dlaset_("Full", n, n, &c_b18, &c_b24, &dwork[ke], &ldwn, 4L);
                    i__1 = *ldwork - kw + 1;
                    dtgsyl_("N", &c__0, nv, n, &ev[ev_offset], ldev, &a[a_offset], lda, &dwork[kc],
                        &ldw, &av[av_offset], ldav, &dwork[ke], &ldwn, &dwork[kf], &ldw, &scale,
                        &dif, &dwork[kw], &i__1, &iwork[1], &ierr, 1L);
                    if (ierr != 0) {
                        *info = 2;
                        return 0;
                    }
                }
                /*              Construct C <- DV'*C + BV'*X*A/SCALE, */
                /*                        D <- DV'*D + BV'*X*B/SCALE. */
                /*              Additional workspace needed: MAX( PV*N, PV*M ). */
                /*              C <- DV'*C. */
                kw = kf;
                dgemm_("T", "N", pv, n, p, &c_b24, &dv[dv_offset], lddv, &c__[c_offset], ldc,
                    &c_b18, &dwork[kw], pv, 1L, 1L);
                dlacpy_("Full", pv, n, &dwork[kw], pv, &c__[c_offset], ldc, 4L);
                /*              D <- DV'*D. */
                dgemm_("T", "N", pv, m, p, &c_b24, &dv[dv_offset], lddv, &d__[d_offset], ldd,
                    &c_b18, &dwork[kw], pv, 1L, 1L);
                dlacpy_("Full", pv, m, &dwork[kw], pv, &d__[d_offset], ldd, 4L);
                /*              C <- C + BV'*X*A/SCALE. */
                d__1 = 1. / scale;
                dgemm_("T", "N", pv, n, nv, &d__1, &bv[bv_offset], ldbv, &dwork[kc], &ldw, &c_b18,
                    &dwork[kw], pv, 1L, 1L);
                dgemm_("N", "N", pv, n, n, &c_b24, &dwork[kw], pv, &a[a_offset], lda, &c_b24,
                    &c__[c_offset], ldc, 1L, 1L);
                /*              D <- D + BV'*X*B/SCALE. */
                dgemm_("N", "N", pv, m, n, &c_b24, &dwork[kw], pv, &b[b_offset], ldb, &c_b24,
                    &d__[d_offset], ldd, 1L, 1L);
            } else {
                /*              Compute X and SCALE satisfying */
                /*              AV'*X + EV'*X*A + SCALE*CV'*C = 0 by solving equivalently */
                /*              AV'*X - Y*A    = -SCALE*CV'*C, */
                /*              EV'*X - Y*(-I) = 0. */
                /*              Additional workspace needed: */
                /*              real    NV*N+N*N; */
                /*              integer NV+N+6. */
                if (*n > 0) {
                    dlaset_("Full", n, n, &c_b18, &c_b17, &dwork[ke], &ldwn, 4L);
                    i__1 = *ldwork - kw + 1;
                    dtgsyl_("N", &c__0, nv, n, &av[av_offset], ldav, &a[a_offset], lda, &dwork[kc],
                        &ldw, &ev[ev_offset], ldev, &dwork[ke], &ldwn, &dwork[kf], &ldw, &scale,
                        &dif, &dwork[kw], &i__1, &iwork[1], &ierr, 1L);
                    /*                 Note that the computed solution in DWORK(KC) is -X. */
                    if (ierr != 0) {
                        *info = 2;
                        return 0;
                    }
                }
                /*              Construct C <- DV'*C + BV'*X/SCALE. */
                kw = kf;
                dgemm_("T", "N", pv, n, p, &c_b24, &dv[dv_offset], lddv, &c__[c_offset], ldc,
                    &c_b18, &dwork[kw], pv, 1L, 1L);
                dlacpy_("Full", pv, n, &dwork[kw], pv, &c__[c_offset], ldc, 4L);
                d__1 = -1. / scale;
                dgemm_("T", "N", pv, n, nv, &d__1, &bv[bv_offset], ldbv, &dwork[kc], &ldw, &c_b24,
                    &c__[c_offset], ldc, 1L, 1L);
                /*              Construct D <- DV'*D. */
                dgemm_("T", "N", pv, m, p, &c_b24, &dv[dv_offset], lddv, &d__[d_offset], ldd,
                    &c_b18, &dwork[kw], pv, 1L, 1L);
                dlacpy_("Full", pv, m, &dwork[kw], pv, &d__[d_offset], ldd, 4L);
            }
        } else {
            /*           Compute the projection of V*G. */
            /*           Total workspace needed: NV*N + MAX( NV*N+N*N, PV*N, PV*M ) */
            /*           Compute -BV*C. */
            /*           Workspace needed: NV*N. */
            dgemm_("N", "N", nv, n, p, &c_b17, &bv[bv_offset], ldbv, &c__[c_offset], ldc, &c_b18,
                &dwork[1], &ldw, 1L, 1L);
            /*           Compute X and SCALE satisfying */
            /*           AV*X - EV*X*A + SCALE*BV*C = 0 by solving equivalently */
            /*           AV*X - Y*A = -SCALE*BV*C, */
            /*           EV*X - Y   = 0. */
            /*           Additional workspace needed: */
            /*           real    NV*N + N*N; */
            /*           integer NV+N+6. */
            if (*n > 0) {
                dlaset_("Full", n, n, &c_b18, &c_b24, &dwork[ke], &ldwn, 4L);
                i__1 = *ldwork - kw + 1;
                dtgsyl_("N", &c__0, nv, n, &av[av_offset], ldav, &a[a_offset], lda, &dwork[kc],
                    &ldw, &ev[ev_offset], ldev, &dwork[ke], &ldwn, &dwork[kf], &ldw, &scale, &dif,
                    &dwork[kw], &i__1, &iwork[1], &ierr, 1L);
                if (ierr != 0) {
                    *info = 2;
                    return 0;
                }
            }
            /*           Construct C <- DV*C + CV*X/SCALE. */
            kw = kf;
            dgemm_("N", "N", pv, n, p, &c_b24, &dv[dv_offset], lddv, &c__[c_offset], ldc, &c_b18,
                &dwork[kw], pv, 1L, 1L);
            dlacpy_("Full", pv, n, &dwork[kw], pv, &c__[c_offset], ldc, 4L);
            d__1 = 1. / scale;
            dgemm_("N", "N", pv, n, nv, &d__1, &cv[cv_offset], ldcv, &dwork[1], &ldw, &c_b24,
                &c__[c_offset], ldc, 1L, 1L);
            /*           Construct D <- DV*D. */
            dgemm_("N", "N", pv, m, p, &c_b24, &dv[dv_offset], lddv, &d__[d_offset], ldd, &c_b18,
                &dwork[kw], pv, 1L, 1L);
            dlacpy_("Full", pv, m, &dwork[kw], pv, &d__[d_offset], ldd, 4L);
        }
    }
    /* Computing MAX */
    d__1 = work, d__2 = (doublereal)lw;
    dwork[1] = max(d__1, d__2);
    return 0;
    /* *** Last line of AB09JV *** */
} /* ab09jv_ */
