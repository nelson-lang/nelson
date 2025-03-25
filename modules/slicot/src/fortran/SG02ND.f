      SUBROUTINE SG02ND( DICO, JOBE, JOB, JOBX, FACT, UPLO, JOBL, TRANS,
     $                   N, M, P, A, LDA, E, LDE, B, LDB, R, LDR, IPIV,
     $                   L, LDL, X, LDX, RNORM, K, LDK, H, LDH, XE,
     $                   LDXE, OUFACT, IWORK, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To compute the optimal gain matrix K for the problem of optimal
C     control given by
C
C                        -1
C          K = (R + B'XB)  (B'Xop(A) + L')                           (1)
C
C     in the discrete-time case and
C
C               -1
C          K = R  (B'Xop(E) + L')                                    (2)
C
C     in the continuous-time case, where A, E, B and L are N-by-N,
C     N-by-N, N-by-M, and N-by-M matrices, respectively; R and X are
C     M-by-M and N-by-N symmetric matrices, respectively, and op(W) is
C     either W or W'. Matrix op(K) defines the feedback gain matrix, if
C     op(W) = W, and the estimator matrix, if op(W) = W'. The formulas
C     above are also useful in Newton's algorithms for solving algebraic
C     Riccati equations, when X is the current iterate.
C
C     Optionally, matrix R may be specified in a factored form, and L
C     may be zero. If R or R + B'XB (for DICO = 'C', or DICO = 'D',
C     respectively), is positive definite, let C be its Cholesky factor
C     (denoted, e.g., C = chol(R), for DICO = 'C'). Optionally, the
C     matrix H, defined by
C
C          H = op(E)'XB + L, if DICO = 'C', or
C          H = op(A)'XB + L, if DICO = 'D',                          (3)
C
C     is returned on exit, besides K; if C exists, the matrix F, defined
C     by FC = H may be optionally returned, instead of K and H. The
C     matrix F or the pair of matrices H and K may be used for computing
C     the residual matrix for an (approximate) solution of an algebraic
C     Riccati equation (see SLICOT Library routine SG02CW).
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     DICO    CHARACTER*1
C             Specifies the equation from which K is to be determined,
C             as follows:
C             = 'D':  Equation (1), discrete-time case;
C             = 'C':  Equation (2), continuous-time case.
C
C     JOBE    CHARACTER*1
C             Specifies whether E is a general or an identity matrix,
C             as follows:
C             = 'G':  The matrix E is general and is given;
C             = 'I':  The matrix E is assumed identity and is not given.
C             This parameter is not relevant for DICO = 'D'.
C
C     JOB     CHARACTER*1
C             Specifies what should be computed, as follows:
C             = 'K':  Compute and return the matrix K only;
C             = 'H':  Compute and return both matrices H and K;
C             = 'F':  Compute the matrix F, if possible; otherwise,
C                     compute and return H and K;
C             = 'D':  Compute and return both matrices H and K, when
C                     B and L have previously been transformed using
C                     SLICOT Library routines SB02MT or SB02MX, which
C                     returned OUFACT = 1. This is useful for computing
C                     K in (2), since then K is the solution of CK = H'.
C                     In this case, FACT should be set to 'C', and the
C                     array R must contain the Cholesky factor of
C                     R + B'XB, if DICO = 'D';
C             = 'C':  Compute and return the matrix F, when B and L have
C                     previously been transformed using SB02MT or
C                     SB02MX, which returned OUFACT = 1. In this case,
C                     FACT should be set to 'C', and the array R must
C                     contain the Cholesky factor of R + B'XB, if
C                     DICO = 'D'.
C             JOB should not be set to 'F' if FACT = 'U'.
C
C     JOBX    CHARACTER*1
C             Specifies whether the matrix op(Xop(E)), if DICO = 'C', or
C             op(Xop(A)), if DICO = 'D', must be computed, as follows:
C             = 'C':  Compute and return the coresponding matrix;
C             = 'N':  Do not compute that matrix.
C             This parameter is not relevant for DICO = 'C' and
C             JOBE = 'I'.
C
C     FACT    CHARACTER*1
C             Specifies how the matrix R is given (factored or not), as
C             follows:
C             = 'N':  Array R contains the matrix R;
C             = 'D':  Array R contains a P-by-M matrix D, where R = D'D;
C             = 'C':  Array R contains the Cholesky factor of R;
C             = 'U':  Array R contains the symmetric indefinite UdU' or
C                     LdL' factorization of R. This option is not
C                     available for DICO = 'D'.
C
C     UPLO    CHARACTER*1
C             Specifies which triangle of the possibly factored matrix R
C             (or R + B'XB, on exit) is or should be stored, as follows:
C             = 'U':  Upper triangle is stored;
C             = 'L':  Lower triangle is stored.
C
C     JOBL    CHARACTER*1
C             Specifies whether or not the matrix L is zero, as follows:
C             = 'Z':  L is zero;
C             = 'N':  L is nonzero.
C
C     TRANS   CHARACTER*1
C             Specifies the form of op(W) to be used in the matrix
C             multiplication, as follows:
C             = 'N':  op(W) = W;
C             = 'T':  op(W) = W';
C             = 'C':  op(W) = W'.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices A and X.  N >= 0.
C             No computations are performed if MIN(N,M) = 0.
C
C     M       (input) INTEGER
C             The order of the matrix R and the number of columns of the
C             matrices B and L.  M >= 0.
C
C     P       (input) INTEGER
C             The number of rows of the matrix D.
C             P >= M for DICO = 'C';
C             P >= 0 for DICO = 'D'.
C             This parameter is relevant only for FACT = 'D'.
C
C     A       (input) DOUBLE PRECISION array, dimension (LDA,N)
C             If DICO = 'D', the leading N-by-N part of this array must
C             contain the state matrix A of the system.
C             If DICO = 'C', this array is not referenced.
C
C     LDA     INTEGER
C             The leading dimension of array A.
C             LDA >= MAX(1,N) if DICO = 'D';
C             LDA >= 1        if DICO = 'C'.
C
C     E       (input) DOUBLE PRECISION array, dimension (LDE,*)
C             If JOBE = 'G' and DICO = 'C', the leading N-by-N part of
C             this array must contain the matrix E.
C             If JOBE = 'I' or DICO = 'D', this array is not referenced.
C
C     LDE     INTEGER
C             The leading dimension of array E.
C             LDE >= MAX(1,N), if JOBE = 'G' and DICO = 'C';
C             LDE >= 1,        if JOBE = 'I'  or DICO = 'D'.
C
C     B       (input/worksp.) DOUBLE PRECISION array, dimension (LDB,M)
C             The leading N-by-M part of this array must contain the
C             input matrix B of the system, transformed by SB02MT or
C             SB02MX, if JOB = 'D' or JOB = 'C'.
C             If DICO = 'D' and FACT = 'D' or 'C', the contents of this
C             array is destroyed. Specifically, if, on exit,
C             OUFACT(2) = 1, this array contains chol(X)*B, and if
C             OUFACT(2) = 2 and INFO < M+2, but INFO >= 0, its trailing
C             part (in the first N rows) contains the submatrix of
C             sqrt(V)*U'B corresponding to the non-negligible, positive
C             eigenvalues of X, where V and U are the matrices with the
C             eigenvalues and eigenvectors of X.
C             Otherwise, B is unchanged on exit.
C
C     LDB     INTEGER
C             The leading dimension of array B.  LDB >= MAX(1,N).
C
C     R       (input/output) DOUBLE PRECISION array, dimension (LDR,M)
C             On entry, if FACT = 'N', the leading M-by-M upper
C             triangular part (if UPLO = 'U') or lower triangular part
C             (if UPLO = 'L') of this array must contain the upper
C             triangular part or lower triangular part, respectively,
C             of the symmetric input weighting matrix R.
C             On entry, if FACT = 'D', the leading P-by-M part of this
C             array must contain the direct transmission matrix D of the
C             system.
C             On entry, if FACT = 'C', the leading M-by-M upper
C             triangular part (if UPLO = 'U') or lower triangular part
C             (if UPLO = 'L') of this array must contain the Cholesky
C             factor of the positive definite input weighting matrix R
C             (as produced by LAPACK routine DPOTRF).
C             On entry, if DICO = 'C' and FACT = 'U', the leading M-by-M
C             upper triangular part (if UPLO = 'U') or lower triangular
C             part (if UPLO = 'L') of this array must contain the
C             factors of the UdU' or LdL' factorization, respectively,
C             of the symmetric indefinite input weighting matrix R (as
C             produced by LAPACK routine DSYTRF).
C             The strictly lower triangular part (if UPLO = 'U') or
C             strictly upper triangular part (if UPLO = 'L') of this
C             array is used as workspace (filled in by symmetry with the
C             other strictly triangular part of R, of R+B'XB, or of the
C             result, if DICO = 'C', DICO = 'D' (if FACT = 'N', in both
C             cases), or (DICO = 'D' and (FACT = 'D' or FACT = 'C') and
C             UPLO = 'L'), respectively.
C             On exit, if OUFACT(1) = 1, and INFO = 0 (or INFO = M+1),
C             the leading M-by-M upper triangular part (if UPLO = 'U')
C             or lower triangular part (if UPLO = 'L') of this array
C             contains the Cholesky factor of the given input weighting
C             matrix R (for DICO = 'C'), or that of the matrix R + B'XB
C             (for DICO = 'D').
C             On exit, if OUFACT(1) = 2, and INFO = 0 (or INFO = M+1),
C             the leading M-by-M upper triangular part (if UPLO = 'U')
C             or lower triangular part (if UPLO = 'L') of this array
C             contains the factors of the UdU' or LdL' factorization,
C             respectively, of the given input weighting matrix
C             (for DICO = 'C'), or that of the matrix R + B'XB
C             (for DICO = 'D' and FACT = 'N').
C             On exit R is unchanged if FACT = 'U' or N = 0.
C
C     LDR     INTEGER.
C             The leading dimension of the array R.
C             LDR >= MAX(1,M)   if FACT <> 'D';
C             LDR >= MAX(1,M,P) if FACT =  'D'.
C
C     IPIV    (input/output) INTEGER array, dimension (M)
C             On entry, if FACT = 'U', this array must contain details
C             of the interchanges performed and the block structure of
C             the d factor in the UdU' or LdL' factorization of matrix R
C             (as produced by LAPACK routine DSYTRF).
C             On exit, if OUFACT(1) = 2, this array contains details of
C             the interchanges performed and the block structure of the
C             d factor in the UdU' or LdL' factorization of matrix R or
C             R + B'XB, as produced by LAPACK routine DSYTRF.
C             This array is not referenced if FACT = 'D', or FACT = 'C',
C             or N = 0.
C
C     L       (input) DOUBLE PRECISION array, dimension (LDL,M)
C             If JOBL = 'N', the leading N-by-M part of this array must
C             contain the cross weighting matrix L, transformed by
C             SB02MT or SB02MX, if JOB = 'D' or JOB = 'C'.
C             If JOBL = 'Z', this array is not referenced.
C
C     LDL     INTEGER
C             The leading dimension of array L.
C             LDL >= MAX(1,N) if JOBL = 'N';
C             LDL >= 1        if JOBL = 'Z'.
C
C     X       (input/output) DOUBLE PRECISION array, dimension (LDX,N)
C             On entry, the leading N-by-N part of this array must
C             contain the (approximate) solution matrix X of the
C             algebraic Riccati equation as produced by SLICOT Library
C             routines SB02MD or SB02OD (or SG02CD). Matrix X is assumed
C             non-negative definite if DICO = 'D', FACT <> 'N',
C             JOB <> 'D' and JOB <> 'C'. The full matrix X must be given
C             on input in this case.
C             For minimal workspace, full matrix X must also be given if
C             ((JOBX = 'C', DICO = 'D', FACT = 'N', and M > N), or
C              (JOBX = 'N', ((DICO = 'C' or FACT = 'N'), (DICO = 'D' or
C               JOBE = 'I') or N >= M, or LDWORK < N*N) and (DICO = 'D'
C               or JOBE = 'G' or JOB = 'K'))) and LDWORK < N*M.
C             (Simpler, but more demanding conditions are the following:
C             ((JOBX = 'C',  DICO = 'D', FACT = 'N', and M > N), or
C              (JOBX = 'N', (DICO = 'D' or ((DICO = 'C', JOBE = 'G') or
C               JOB = 'K'))),  LDWORK < N*N.)
C             For optimal workspace, full matrix X is not needed in any
C             of the cases described above for minimal workspace.
C             On exit, if DICO = 'D', FACT = 'D' or FACT = 'C', and
C             OUFACT(2) = 1, the N-by-N upper triangular part
C             (if UPLO = 'U') or lower triangular part (if UPLO = 'L')
C             of this array contains the Cholesky factor of the given
C             matrix X, which is found to be positive definite.
C             On exit, if DICO = 'D', FACT = 'D' or 'C', OUFACT(2) = 2,
C             and INFO <> M+2 (but INFO >= 0), the leading N-by-N part
C             of this array contains the matrix of orthonormal
C             eigenvectors of X.
C             On exit X is unchanged if DICO = 'C' or FACT = 'N'.
C
C     LDX     INTEGER
C             The leading dimension of array X.  LDX >= MAX(1,N).
C
C     RNORM   (input) DOUBLE PRECISION
C             If FACT = 'U', this parameter must contain the 1-norm of
C             the original matrix R (before factoring it).
C             Otherwise, this parameter is not used.
C
C     K       (output) DOUBLE PRECISION array, dimension (LDK,N)
C             If JOB = 'K' or JOB = 'H' or JOB = 'D' or OUFACT(1) = 2,
C             the leading M-by-N part of this array contains the gain
C             matrix K.
C
C     LDK     INTEGER
C             The leading dimension of array K.  LDK >= MAX(1,M).
C
C     H       (output) DOUBLE PRECISION array, dimension (LDH,*)
C             If JOB = 'H' or JOB = 'D' or (JOB = 'F' and
C             OUFACT(1) = 2), the leading N-by-M part of this array
C             contains the matrix H.
C             If JOB = 'C' or (JOB = 'F' and OUFACT(1) = 1), the leading
C             N-by-M part of this array contains the matrix F.
C             If JOB = 'K', this array is not referenced.
C
C     LDH    INTEGER
C             The leading dimension of array H.
C             LDH >= MAX(1,N), if JOB <> 'K';
C             LDH >= 1,        if JOB =  'K'.
C
C     XE      (output) DOUBLE PRECISION array, dimension (LDXE,*)
C             If JOBX = 'C', DICO = 'C', and JOBE = 'G', the leading
C             N-by-N part of this array contains the matrix product X*E,
C             if TRANS = 'N', or E*X, if TRANS = 'T' or TRANS = 'C'.
C             If JOBX = 'C' and DICO = 'D', the leading N-by-N part of
C             this array contains the matrix product X*A, if
C             TRANS = 'N', or A*X, if TRANS = 'T' or TRANS = 'C'.
C             These matrix products may be needed for computing the
C             residual matrix for an (approximate) solution of a Riccati
C             equation (see SLICOT Library routine SG02CW).
C             If JOBX = 'N' or (DICO = 'C' and JOBE = 'I'), this array
C             is not referenced.
C
C     LDXE    INTEGER
C             The leading dimension of array XE.
C             LDXE >= MAX(1,N), if JOBX = 'C', and either DICO = 'C' and
C                               JOBE = 'G', or DICO = 'D';
C             LDXE >= 1,        if JOBX = 'N' or (DICO = 'C' and
C                                                 JOBE = 'I').
C
C     OUFACT  (output) INTEGER array, dimension (2)
C             Information about the factorization finally used.
C             OUFACT(1) = 1:  Cholesky factorization of R (or R + B'XB)
C                             has been used;
C             OUFACT(1) = 2:  UdU' (if UPLO = 'U') or LdL' (if UPLO =
C                             'L') factorization of R (or R + B'XB)
C                             has been used;
C             OUFACT(2) = 1:  Cholesky factorization of X has been used;
C             OUFACT(2) = 2:  Spectral factorization of X has been used.
C             The value of OUFACT(2) is not set for DICO = 'C' or for
C             DICO = 'D' and FACT = 'N'.
C             This array is not set if N = 0 or M = 0.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (M)
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0 or LDWORK = -1, DWORK(1) returns the
C             optimal value of LDWORK, and for LDWORK set as specified
C             below, DWORK(2) contains the reciprocal condition number
C             of the matrix R (for DICO = 'C') or of R + B'XB (for
C             DICO = 'D'), if FACT = 'N' or FACT = 'U' or OUFACT(1) = 2,
C             or of its Cholesky factor, if FACT = 'C' or FACT = 'D' and
C             OUFACT(1) = 1; DWORK(2) is set to 1 if N = 0.
C             On exit, if LDWORK = -2 on input or INFO = -35, then
C             DWORK(1) returns the minimal value of LDWORK.
C             If on exit INFO = 0, and OUFACT(2) = 2, then DWORK(3),...,
C             DWORK(N+2) contain the eigenvalues of X, in ascending
C             order.
C
C     LDWORK  INTEGER
C             Dimension of working array DWORK.
C             Let a = N, if JOBX = 'N' and (DICO = 'D' or JOBE = 'G');
C                 a = 0, otherwise. Then
C             LDWORK >= max(2,2*M,a)       if FACT =  'U';
C             LDWORK >= max(2,3*M,4*N+1)   if FACT =  'D' or
C                                            (FACT =  'C' and JOB <> 'C'
C                                          and JOB <> 'D'), DICO = 'D';
C             LDWORK >= max(2,3*M,a)       otherwise.
C             For optimum performance LDWORK should be larger.
C
C             If LDWORK = -1, an optimal workspace query is assumed; the
C             routine only calculates the optimal size of the DWORK
C             array, returns this value as the first entry of the DWORK
C             array, and no error message is issued by XERBLA.
C
C             If LDWORK = -2, a minimal workspace query is assumed; the
C             routine only calculates the minimal size of the DWORK
C             array, returns this value as the first entry of the DWORK
C             array, and no error message is issued by XERBLA.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value;
C             = i:  if the i-th element of the d factor is exactly zero;
C                   the UdU' (or LdL') factorization has been completed,
C                   but the block diagonal matrix d is exactly singular;
C             = M+1:  if the matrix R (if DICO = 'C'), or R + B'XB
C                   (if DICO = 'D') is numerically singular (to working
C                   precision);
C             = M+2:  if one or more of the eigenvalues of X has not
C                   converged;
C             = M+3:  if the matrix X is indefinite and updating the
C                   triangular factorization failed.
C             If INFO > M+1, call the routine again with an appropriate,
C             unfactored matrix R.
C
C     METHOD
C
C     The (optimal) gain matrix K is obtained as the solution to the
C     system of linear equations
C
C        (R + B'XB) * K = B'Xop(A) + L'
C
C     in the discrete-time case and
C
C        R * K = B'Xop(E) + L'
C
C     in the continuous-time case, with R replaced by D'D if FACT = 'D'.
C     If FACT = 'N', Cholesky factorization is tried first, but
C     if the coefficient matrix is not positive definite, then UdU' (or
C     LdL') factorization is used. If FACT <> 'N', the factored form
C     of R is taken into account. The discrete-time case then involves
C     updating of a triangular factorization of R (or D'D); Cholesky or
C     symmetric spectral factorization of X is employed to avoid
C     squaring of the condition number of the matrix. When D is given,
C     its QR factorization is determined, and the triangular factor is
C     used as described above.
C
C     NUMERICAL ASPECTS
C
C     The algorithm consists of numerically stable steps.
C                                                   3     2
C     For DICO = 'C' and JOBE = 'I', it requires O(m  + mn ) floating
C                                            2
C     point operations if FACT = 'N' and O(mn ) floating point
C     operations, otherwise.
C     For DICO = 'D' or JOBE = 'G', the operation counts are similar,
C                       3
C     but additional O(n ) floating point operations may be needed in
C     the worst case.
C     These estimates assume that M <= N.
C
C     CONTRIBUTORS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Jan. 2014.
C
C     REVISIONS
C
C     V. Sima, Feb. 2014, July 2017, Dec. 2017.
C
C     KEYWORDS
C
C     Algebraic Riccati equation, closed loop system, continuous-time
C     system, discrete-time system, matrix algebra, optimal control,
C     optimal regulator.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE, TWO
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         DICO, FACT, JOB, JOBE, JOBL, JOBX, TRANS, UPLO
      INTEGER           INFO, LDA, LDB, LDE, LDH, LDK, LDL, LDR, LDWORK,
     $                  LDX, LDXE, M, N, P
      DOUBLE PRECISION  RNORM
C     .. Array Arguments ..
      INTEGER           IPIV(*), IWORK(*), OUFACT(2)
      DOUBLE PRECISION  A(LDA,*), B(LDB,*), DWORK(*), E(LDE,*),
     $                  H(LDH,*), K(LDK,*), L(LDL,*), R(LDR,*),
     $                  X(LDX,*), XE(LDXE,*)
C     .. Local Scalars ..
      LOGICAL           DISCR, LASTCS, LFACTA, LFACTC, LFACTD, LFACTU,
     $                  LJOBE, LNFACT, LTRANS, LUPLOU, SUFWRK, WITHC,
     $                  WITHCD, WITHD, WITHF, WITHH, WITHL, WITHXE
      CHARACTER         NT, NTRANS, NUPLO, SIDE, TR, TRL
      INTEGER           I, IFAIL, JW, JZ, MS, NM, NR, WRKMIN, WRKOPT
      DOUBLE PRECISION  EPS, RCOND, RNORMP, TEMP, TMP
C     .. Local Arrays ..
      DOUBLE PRECISION  DUMMY(1)
C     .. External Functions ..
      LOGICAL           LSAME
      DOUBLE PRECISION  DLAMCH, DLANSY
      EXTERNAL          DLAMCH, DLANSY, LSAME
C     .. External Subroutines ..
      EXTERNAL          DAXPY, DCOPY, DGEMM, DGEQRF, DLACPY, DLASET,
     $                  DPOCON, DPOTRF, DPOTRS, DSCAL, DSYCON, DSYEV,
     $                  DSYMM, DSYTRF, DSYTRS, DTRCON, DTRMM, DTRSM,
     $                  MA02AD, MA02ED, MB01RB, MB01RU, MB04KD, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DBLE, INT, MAX, MIN, SQRT
C     .. Executable Statements ..
C
      INFO   = 0
      DISCR  = LSAME( DICO,  'D' )
      LFACTC = LSAME( FACT,  'C' )
      LFACTD = LSAME( FACT,  'D' )
      LFACTU = LSAME( FACT,  'U' )
      LJOBE  = LSAME( JOBE,  'G' )
      LTRANS = LSAME( TRANS, 'T' ) .OR. LSAME( TRANS, 'C' )
      LUPLOU = LSAME( UPLO,  'U' )
      WITHC  = LSAME( JOB,   'C' )
      WITHD  = LSAME( JOB,   'D' )
      WITHF  = LSAME( JOB,   'F' )
      WITHH  = LSAME( JOB,   'H' )
      WITHL  = LSAME( JOBL,  'N' )
      WITHXE = LSAME( JOBX,  'C' )
      LFACTA = LFACTC .OR. LFACTD .OR. LFACTU
      WITHCD = WITHC  .OR. WITHD
      WITHH  = WITHH  .OR. WITHF .OR. WITHCD
      LNFACT = .NOT.LFACTA
C
C     Test the input scalar arguments.
C
      IF(       .NOT.DISCR .AND. .NOT.LSAME( DICO,  'C' ) ) THEN
         INFO = -1
      ELSE IF(  .NOT.LJOBE .AND. .NOT.LSAME( JOBE,  'I' ) .AND.
     $                           .NOT.DISCR               ) THEN
         INFO = -2
      ELSE IF( .NOT.WITHH  .AND. .NOT.LSAME( JOB,   'K' ) ) THEN
         INFO = -3
      ELSE IF( .NOT.WITHXE .AND. .NOT.LSAME( JOBX,  'N' ) ) THEN
         IF( DISCR .OR. LJOBE )
     $      INFO = -4
      ELSE IF( (    LNFACT .AND. .NOT.LSAME( FACT,  'N' ) ) .OR.
     $         (    DISCR  .AND. LFACTU ) .OR.
     $       ( .NOT.LFACTC .AND. WITHCD ) ) THEN
         INFO = -5
      ELSE IF( .NOT.LUPLOU .AND. .NOT.LSAME( UPLO,  'L' ) ) THEN
         INFO = -6
      ELSE IF( .NOT.WITHL  .AND. .NOT.LSAME( JOBL,  'Z' ) ) THEN
         INFO = -7
      ELSE IF( .NOT.LTRANS .AND. .NOT.LSAME( TRANS, 'N' ) ) THEN
         INFO = -8
      ELSE IF( N.LT.0 ) THEN
         INFO = -9
      ELSE IF( M.LT.0 ) THEN
         INFO = -10
      ELSE IF( LFACTD .AND. ( P.LT.0 .OR. ( .NOT.DISCR .AND. P.LT.M ) )
     $       ) THEN
         INFO = -11
      ELSE IF( LDA.LT.1 .OR. ( LDA.LT.N .AND.      DISCR ) ) THEN
         INFO = -13
      ELSE IF( LDE.LT.1 .OR. ( LDE.LT.N .AND. .NOT.DISCR
     $                                  .AND.      LJOBE ) ) THEN
         INFO = -15
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -17
      ELSE IF( LDR.LT.MAX( 1, M ) .OR. ( LFACTD .AND. LDR.LT.P ) ) THEN
         INFO = -19
      ELSE IF( LDL.LT.1 .OR. ( WITHL .AND. LDL.LT.N ) ) THEN
         INFO = -22
      ELSE IF( LDX.LT.MAX( 1, N ) ) THEN
         INFO = -24
      ELSE IF( LFACTU ) THEN
         IF( RNORM.LT.ZERO )
     $      INFO = -25
      END IF
      IF ( INFO.EQ.0 ) THEN
         IF( LDK.LT.MAX( 1, M ) ) THEN
            INFO = -27
         ELSE IF( LDH.LT.1  .OR. ( WITHH  .AND.  LDH.LT.N ) ) THEN
            INFO = -29
         ELSE IF( LDXE.LT.1 .OR. ( WITHXE .AND. ( DISCR .OR. LJOBE )
     $                                    .AND. LDXE.LT.N ) ) THEN
            INFO = -31
         ELSE
            IF ( LUPLOU ) THEN
               NUPLO = 'Lower'
            ELSE
               NUPLO = 'Upper'
            END IF
            NM = N*M
            IF ( DISCR .AND. LFACTA .AND. .NOT.WITHCD ) THEN
               WRKMIN = MAX( 2, 3*M, 4*N + 1 )
            ELSE
               IF ( .NOT.WITHXE .AND. ( DISCR .OR. LJOBE ) ) THEN
                  WRKMIN = MAX( 2, N )
               ELSE
                  WRKMIN = 2
               END IF
               IF( LFACTU ) THEN
                  WRKMIN = MAX( WRKMIN, 2*M )
               ELSE
                  WRKMIN = MAX( WRKMIN, 3*M )
               END IF
            END IF
            IF( LDWORK.EQ.-1 ) THEN
               IF ( WITHXE .AND. ( DISCR .OR. LJOBE ) ) THEN
                  IF ( DISCR .AND. LNFACT .AND. M.GT.N ) THEN
                     WRKOPT = NM
                  ELSE
                     WRKOPT = 0
                  END IF
               ELSE IF ( ( ( DISCR .AND. LFACTA ) .OR. ( .NOT.DISCR
     $                             .AND. LJOBE  ) ) .AND. N.LT.M ) THEN
                  WRKOPT = N*N
               ELSE IF ( .NOT.( DISCR .OR. LJOBE ) .AND. WITHH ) THEN
                  WRKOPT = 0
               ELSE
                  WRKOPT = NM
               END IF
C
               IF( LFACTA ) THEN
                  IF ( LFACTD ) THEN
                     CALL DGEQRF( P, M, R, LDR, DWORK, DWORK, -1, IFAIL)
                     WRKOPT = MAX( WRKOPT, INT( DWORK(1) )+MIN( P, M ) )
                  END IF
                  IF( DISCR .AND. .NOT.WITHCD ) THEN
                     CALL DSYEV( 'Vectors', NUPLO, N, X, LDX, DWORK,
     $                           DWORK, -1, IFAIL )
                     WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) + N + 2,
     $                             NM + N + 2 )
                  END IF
               ELSE
                  CALL DSYTRF( UPLO, M, R, LDR, IPIV, DWORK, -1, IFAIL )
                  WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) )
               END IF
               DWORK(1) = MAX( WRKMIN, WRKOPT )
               RETURN
            ELSE IF( LDWORK.EQ.-2 ) THEN
               DWORK(1) = WRKMIN
               RETURN
            ELSE IF( LDWORK.LT.WRKMIN ) THEN
               INFO = -35
               DWORK(1) = WRKMIN
               RETURN
            END IF
         END IF
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'SG02ND', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ( N.EQ.0 .OR. M.EQ.0 ) THEN
         DWORK(1) = TWO
         IF ( N.EQ.0 ) THEN
            DWORK(2) = ONE
         ELSE
            DWORK(2) = ZERO
         END IF
         RETURN
      END IF
C
      EPS = DLAMCH( 'Precision' )
C
C     Determine the right-hand side of the matrix equation, and R+B'XB,
C     if needed.
C     1. If JOBX = 'N' or (DICO = 'C' and JOBE = 'I'), compute  B'X
C     in K or XB either in H, if JOB <> 'K', or in the workspace, if
C     enough space. If JOB <> 'K', compute H in (3), otherwise compute
C     H' in K. The same formulas for H are used for JOB = 'D' or
C     JOB = 'C', but B and L on entry are transformed matrices; however,
C     the returned results correspond to the original matrices.
C     2. If JOBX = 'C', compute op(Xop(E)) or op(Xop(A)) in XE, for
C     DICO = 'C' (and JOBE = 'G') or DICO = 'D', respectively.
C     Then, use XE in computations similar to those described above.
C
C     (Note: Comments in the code beginning "Workspace:" describe the
C     minimal amount of real workspace needed at that point in the
C     code, as well as the preferred amount for good performance.
C     NB refers to the optimal block size for the immediately
C     following subroutine, as returned by ILAENV.)
C
C     Workspace: need   0;
C                prefer N*N, if M > N and ((DICO = 'D' and FACT <> 'N')
C                                      or  (DICO = 'C' and JOBE =  'G'))
C                                     and   JOBX = 'N';
C                       M*N, otherwise.
C     Only a triangle of X is needed when using the preferred length.
C
      SUFWRK = LDWORK.GE.NM
C
      NT = 'No transpose'
      TR = 'Transpose'
      IF ( LTRANS ) THEN
         NTRANS = NT
         SIDE   = 'Right'
      ELSE
         NTRANS = TR
         SIDE   = 'Left'
      END IF
C
      IF ( WITHL ) THEN
         TEMP = ONE
      ELSE
         TEMP = ZERO
      END IF
C
      IF ( WITHXE .AND. ( DISCR .OR. LJOBE ) ) THEN
         LASTCS = .FALSE.
         IF ( DISCR ) THEN
            IF ( LNFACT ) THEN
C
C              Discrete-time case. Compute a triangle of R + B'XB.
C
               IF ( M.LE.N ) THEN
                  CALL MB01RU( UPLO, TR, M, N, ONE, ONE, R, LDR, B, LDB,
     $                         X, LDX, XE, NM, INFO )
                  WRKOPT = 0
               ELSE IF ( SUFWRK ) THEN
                  CALL DSYMM(  'Left', UPLO, N, M, ONE, X, LDX, B, LDB,
     $                         ZERO, DWORK, N )
                  CALL MB01RB( 'Left', UPLO, TR, M, N, ONE, ONE, R, LDR,
     $                         B, LDB, DWORK, N, IFAIL )
                  WRKOPT = NM
               ELSE
C
C                 This case needs a full matrix X.
C
                  CALL DGEMM(  TR, NT, M, N, N, ONE, B, LDB, X, LDX,
     $                         ZERO, K, LDK )
                  CALL MB01RB( 'Left', UPLO, NT, M, N, ONE, ONE, R, LDR,
     $                         K, LDK, B, LDB, IFAIL )
                  WRKOPT = 0
               END IF
            END IF
            CALL DSYMM( SIDE, UPLO, N, N, ONE, X, LDX, A, LDA, ZERO, XE,
     $                  LDXE )
         ELSE
            CALL DSYMM( SIDE, UPLO, N, N, ONE, X, LDX, E, LDE, ZERO, XE,
     $                  LDXE )
            WRKOPT = 0
         END IF
         IF ( WITHH ) THEN
            IF ( WITHL )
     $         CALL DLACPY( 'All', N, M, L, LDL, H, LDH )
            CALL DGEMM(  NTRANS, NT, N, M, N, ONE, XE, LDXE, B, LDB,
     $                   TEMP, H, LDH )
            CALL MA02AD( 'All', N, M, H, LDH, K, LDK )
         ELSE
            IF ( WITHL )
     $         CALL MA02AD( 'All', N, M, L, LDL, K, LDK )
            CALL DGEMM( TR, TRANS, M, N, N, ONE, B, LDB, XE, LDXE, TEMP,
     $                  K, LDK )
         END IF
C
      ELSE IF ( (( DISCR .AND. LFACTA ) .OR. ( .NOT.DISCR .AND. LJOBE ))
     $                   .AND. N.LT.M  .AND. LDWORK.GE.N*N ) THEN
         LASTCS = .FALSE.
         IF ( DISCR ) THEN
            CALL DSYMM( SIDE, UPLO, N, N, ONE, X, LDX, A, LDA, ZERO,
     $                  DWORK, N )
         ELSE
            CALL DSYMM( SIDE, UPLO, N, N, ONE, X, LDX, E, LDE, ZERO,
     $                  DWORK, N )
         END IF
C
         IF ( WITHH ) THEN
            IF ( WITHL )
     $         CALL DLACPY( 'All', N, M, L, LDL, H, LDH )
            CALL DGEMM(  NTRANS, NT, N, M, N, ONE, DWORK, N, B, LDB,
     $                   TEMP, H, LDH )
            CALL MA02AD( 'All', N, M, H, LDH, K, LDK )
         ELSE
            IF ( WITHL )
     $         CALL MA02AD( 'All', N, M, L, LDL, K, LDK )
            CALL DGEMM( TR, TRANS, M, N, N, ONE, B, LDB, DWORK, N, TEMP,
     $                  K, LDK )
         END IF
         WRKOPT = N*N
C
      ELSE IF ( .NOT.( DISCR .OR. LJOBE ) .AND. WITHH ) THEN
C
C        Continuous-time case, with E identity, JOB <> 'K'.
C
         LASTCS = .FALSE.
C
         IF ( WITHH ) THEN
            IF ( WITHL )
     $         CALL DLACPY( 'All', N, M, L, LDL, H, LDH )
            CALL DSYMM(  'Left', UPLO, N, M, ONE, X, LDX, B, LDB, TEMP,
     $                   H, LDH )
            CALL MA02AD( 'All', N, M, H, LDH, K, LDK )
            WRKOPT = 0
         END IF
C
      ELSE IF ( SUFWRK ) THEN
         LASTCS = .FALSE.
         IF ( DISCR .OR. LJOBE ) THEN
            CALL DSYMM( 'Left', UPLO, N, M, ONE, X, LDX, B, LDB, ZERO,
     $                  DWORK, N )
            IF ( WITHH ) THEN
               IF ( WITHL )
     $            CALL DLACPY( 'All', N, M, L, LDL, H, LDH )
               IF ( DISCR ) THEN
                  CALL DGEMM( NTRANS, NT, N, M, N, ONE, A, LDA, DWORK,
     $                        N, TEMP, H, LDH )
               ELSE
                  CALL DGEMM( NTRANS, NT, N, M, N, ONE, E, LDE, DWORK,
     $                        N, TEMP, H, LDH )
               END IF
               CALL MA02AD( 'All', N, M, H, LDH, K, LDK )
            ELSE
               IF ( WITHL )
     $            CALL MA02AD( 'All', N, M, L, LDL, K, LDK )
               IF ( DISCR ) THEN
                  CALL DGEMM( TR, TRANS, M, N, N, ONE, DWORK, N, A, LDA,
     $                        TEMP, K, LDK )
               ELSE
                  CALL DGEMM( TR, TRANS, M, N, N, ONE, DWORK, N, E, LDE,
     $                        TEMP, K, LDK )
               END IF
            END IF
            WRKOPT = NM
C
        ELSE IF ( .NOT.WITHH .OR. LDWORK.GE.NM ) THEN
C
C           Continuous-time case, E identity.
C
            IF ( WITHH ) THEN
               IF ( WITHL )
     $            CALL DLACPY( 'All', N, M, L, LDL, H, LDH )
               CALL DSYMM(  'Left', UPLO, N, M, ONE, X, LDX, B, LDB,
     $                      TEMP, H, LDH )
               CALL MA02AD( 'All', N, M, H, LDH, K, LDK )
               WRKOPT = 0
            ELSE
               IF ( WITHL )
     $            CALL DLACPY( 'All', N, M, L, LDL, DWORK, N )
               CALL DSYMM(  'Left', UPLO, N, M, ONE, X, LDX, B, LDB,
     $                      TEMP, DWORK, N )
               CALL MA02AD( 'All', N, M, DWORK, N, K, LDK )
               WRKOPT = NM
            END IF
         END IF
C
      ELSE
C
C        This case needs a full matrix X.
C
         LASTCS = .TRUE.
C
         IF( WITHH ) THEN
            CALL DGEMM( NT, NT, N, M, N, ONE, X, LDX, B, LDB, ZERO, H,
     $                  LDH )
         ELSE
            CALL DGEMM( TR, NT, M, N, N, ONE, B, LDB, X, LDX, ZERO, K,
     $                  LDK )
         END IF
         WRKOPT = 0
      END IF
C
      IF ( LNFACT ) THEN
C
C        R not factored.
C
         IF ( DISCR .AND. .NOT.WITHXE ) THEN
C
C           Discrete-time case. Compute a triangle of R + B'XB.
C
            IF ( SUFWRK ) THEN
               CALL MB01RB( 'Left', UPLO, TR, M, N, ONE, ONE, R, LDR,
     $                      DWORK, N, B, LDB, IFAIL )
            ELSE IF( WITHH ) THEN
               CALL MB01RB( 'Left', UPLO, TR, M, N, ONE, ONE, R, LDR, B,
     $                      LDB, H, LDH, IFAIL )
            ELSE
               CALL MB01RB( 'Left', UPLO, NT, M, N, ONE, ONE, R, LDR, K,
     $                      LDK, B, LDB, IFAIL )
            END IF
         END IF
C
C        Compute the 1-norm of the matrix  R  or  R + B'XB.
C        Workspace: need M.
C
         RNORMP = DLANSY( '1-norm', UPLO, M, R, LDR, DWORK )
      END IF
C
      IF ( LASTCS ) THEN
         MS = MAX( INT( LDWORK/N ), 1 )
C
C        Workspace (for DICO = 'D' or JOBE = 'G'): need   N;
C                                                  prefer N*M.
         IF ( WITHH ) THEN
C
C           Premultiply XB by op(A)' or by op(E)' (on block-columns).
C           Add L if needed.
C
            IF ( DISCR ) THEN
C
               DO 10 I = 1, M, MS
                  NR = MIN( MS, M-I+1 )
                  CALL DLACPY( 'All', N, NR, H(1,I), LDH, DWORK, N )
                  IF ( WITHL )
     $               CALL DLACPY( 'All', N, NR, L(1,I), LDL, H(1,I),
     $                            LDH )
                  CALL DGEMM( NTRANS, NT, N, NR, N, ONE, A, LDA, DWORK,
     $                        N, TEMP, H(1,I), LDH )
   10          CONTINUE
C
            ELSE IF ( LJOBE ) THEN
C
               DO 20 I = 1, M, MS
                  NR = MIN( MS, M-I+1 )
                  CALL DLACPY( 'All', N, NR, H(1,I), LDH, DWORK, N )
                  IF ( WITHL )
     $               CALL DLACPY( 'All', N, NR, L(1,I), LDL, H(1,I),
     $                            LDH )
                  CALL DGEMM( NTRANS, NT, N, NR, N, ONE, E, LDE, DWORK, 
     $                        N, TEMP, H(1,I), LDH )
   20          CONTINUE
C
            ELSE IF ( WITHL ) THEN
C
               DO 30 I = 1, M
                  CALL DAXPY( N, ONE, L(1,I), 1, K(I,1), LDK )
   30          CONTINUE
C
            END IF
            CALL MA02AD( 'All', N, M, H, LDH, K, LDK )
C
         ELSE
C
C           Postmultiply B'X by op(A) or by op(E) (on block-rows).
C           Add L if needed.
C
            IF ( DISCR ) THEN
C
               DO 40 I = 1, M, MS
                  NR = MIN( MS, M-I+1 )
                  CALL DLACPY( 'All', NR, N, K(I,1), LDK, DWORK, NR )
                  IF ( WITHL )
     $               CALL MA02AD( 'All', N, NR, L(1,I), LDL, K(I,1),
     $                            LDK )
                  CALL DGEMM( NT, TRANS, NR, N, N, ONE, DWORK, NR, A,
     $                        LDA, TEMP, K(I,1), LDK )
   40          CONTINUE
C
            ELSE IF ( LJOBE ) THEN
C
               DO 50 I = 1, M, MS
                  NR = MIN( MS, M-I+1 )
                  CALL DLACPY( 'All', NR, N, K(I,1), LDK, DWORK, NR )
                  IF ( WITHL )
     $               CALL MA02AD( 'All', N, NR, L(1,I), LDL, K(I,1),
     $                            LDK )
                  CALL DGEMM( NT, TRANS, NR, N, N, ONE, DWORK, NR, E,
     $                        LDE, TEMP, K(I,1), LDK )
   50          CONTINUE
C
            ELSE IF ( WITHL ) THEN
C
               DO 60 I = 1, M
                  CALL DAXPY( N, ONE, L(1,I), 1, K(I,1), LDK )
   60          CONTINUE
C
            END IF
         END IF
C
      END IF
C
      WRKOPT = MAX( WRKMIN, WRKOPT )
C
C     Solve the matrix equation.
C
      IF ( LUPLOU ) THEN
         TRL = NT
      ELSE
         TRL = TR
      END IF
C
      IF ( LFACTA ) THEN
C
C        Case 1: Matrix R is given in a factored form.
C
         IF ( LFACTD ) THEN
C
C           Use QR factorization of D.
C           Workspace: need   min(P,M) + M,
C                      prefer min(P,M) + M*NB.
C
            JW = MIN( P, M ) + 1
            CALL DGEQRF( P, M, R, LDR, DWORK, DWORK(JW), LDWORK-JW+1,
     $                   IFAIL )
            WRKOPT = MAX( WRKOPT, INT( DWORK(JW) )+JW-1 )
            IF ( P.LT.M )
     $         CALL DLASET( 'Full', M-P, M, ZERO, ZERO, R(P+1,1), LDR )
C
C           Make positive the diagonal elements of the triangular
C           factor. Construct the strictly lower triangle, if requested.
C
            DO 70 I = 1, M
               IF ( .NOT.LUPLOU )
     $            CALL DCOPY( I-1, R(1,I), 1, R(I,1), LDR )
               IF ( R(I,I).LT.ZERO )
     $            CALL DSCAL( M-I+1, -ONE, R(I,I), LDR )
   70       CONTINUE
C
         END IF
C
         IF ( DISCR .AND. .NOT.WITHCD ) THEN
            JZ = 0
C
C           Discrete-time case. Update the factorization for B'XB.
C           Try first the Cholesky factorization of X, saving the
C           diagonal of X, in order to recover it, if X is not positive
C           definite. In the later case, use spectral factorization.
C           Workspace: need N.
C
            CALL DCOPY(  N, X, LDX+1, DWORK, 1 )
            CALL DPOTRF( UPLO, N, X, LDX, IFAIL )
C
            IF ( IFAIL.EQ.0 ) THEN
C
C              Use Cholesky factorization of X to compute chol(X)*B.
C
               OUFACT(2) = 1
               CALL DTRMM( 'Left', UPLO, TRL, 'Non unit', N, M, ONE, X,
     $                     LDX, B, LDB )
            ELSE
C
C              Use spectral factorization of X, X = UVU'.
C              Workspace: need   4*N+1,
C                         prefer N*(NB+2)+N+2.
C
               JW = N + 3
               OUFACT(2) = 2
               CALL DCOPY( N, DWORK, 1, X, LDX+1 )
               CALL DSYEV( 'Vectors', NUPLO, N, X, LDX, DWORK(3),
     $                     DWORK(JW), LDWORK-JW+1, IFAIL )
               IF ( IFAIL.GT.0 ) THEN
                  INFO = M + 2
                  RETURN
               END IF
               WRKOPT = MAX( WRKOPT, INT( DWORK(JW) )+JW-1 )
               TEMP   = ABS( DWORK(N+2) )*EPS*DBLE( N )
C
C              Check out the positive (semi-)definiteness of X.
C              First, count the negligible eigenvalues.
C
   80          CONTINUE
               IF ( ABS( DWORK(JZ+3) ).LE.TEMP ) THEN
                  JZ = JZ + 1
                  IF ( JZ.LT.N )
     $               GO TO 80
               END IF
C
               IF ( LFACTD .AND. N-JZ+P.LT.M ) THEN
C
C                 The coefficient matrix is (numerically) singular.
C
                  OUFACT(1) = 1
                  DWORK(2)  = ZERO
                  INFO = M + 1
                  RETURN
               END IF
C
               IF ( DWORK(JZ+3).LT.ZERO ) THEN
C
C                 X is not positive (semi-)definite. Updating fails.
C
                  INFO = M + 3
                  RETURN
               ELSE
C
C                 Compute sqrt(V)U'B.
C                 Workspace: need     2*N+2;
C                            prefer N*M+N+2.
C
                  WRKOPT = MAX( WRKOPT, NM + JW - 1 )
                  MS = MAX( INT( ( LDWORK - JW + 1 )/N ), 1 )
C
                  DO 90 I = 1, M, MS
                     NR = MIN( MS, M-I+1 )
                     CALL DLACPY( 'All', N, NR, B(1,I), LDB, DWORK(JW),
     $                            N )
                     CALL DGEMM(  TR, NT, N-JZ, NR, N, ONE, X(1,JZ+1),
     $                            LDX, DWORK(JW), N, ZERO, B(JZ+1,I),
     $                            LDB )
   90             CONTINUE
C
                  DO 100 I = JZ + 1, N
                     CALL DSCAL( M, SQRT( DWORK(I+2) ), B(I,1), LDB )
  100             CONTINUE
C
               END IF
C
            END IF
C
C           Update the triangular factorization.
C
            IF ( .NOT.LUPLOU )
C
C              Transpose the lower triangle for using MB04KD.
C
     $         CALL MA02ED( UPLO, M, R, LDR )
C
C           Workspace: need 2*M.
C
            CALL MB04KD( 'Full', M, 0, N-JZ, R, LDR, B(JZ+1,1), LDB,
     $                   DUMMY, N, DUMMY, M, DWORK, DWORK(M+1) )
C
C           Make positive the diagonal elements of the triangular
C           factor.
C
            DO 110 I = 1, M
               IF ( R(I,I).LT.ZERO )
     $            CALL DSCAL( M-I+1, -ONE, R(I,I), LDR )
  110       CONTINUE
C
            IF ( .NOT.LUPLOU )
C
C              Construct the lower triangle.
C
     $         CALL MA02ED( NUPLO, M, R, LDR )
C
         END IF
C
C        Compute the condition number of the coefficient matrix.
C
         IF ( .NOT.LFACTU ) THEN
C
C           Workspace: need 3*M.
C
            CALL DTRCON( '1-norm', UPLO, 'Non unit', M, R, LDR, RCOND,
     $                    DWORK, IWORK, IFAIL )
            OUFACT(1) = 1
         ELSE
C
C           Workspace: need 2*M.
C
            CALL DSYCON( UPLO, M, R, LDR, IPIV, RNORM, RCOND, DWORK,
     $                   IWORK, IFAIL )
            OUFACT(1) = 2
         END IF
C
      ELSE
C
C        Case 2: Matrix R is given in an unfactored form.
C
C        Save the given triangle of  R  or  R + B'XB  in the other
C        strict triangle and the diagonal in the workspace, and try
C        Cholesky factorization.
C        Workspace: need M.
C
         CALL DCOPY( M, R, LDR+1, DWORK, 1 )
         CALL MA02ED( UPLO, M, R, LDR )
         CALL DPOTRF( UPLO, M, R, LDR, IFAIL )
         IF( IFAIL.EQ.0 ) THEN
            OUFACT(1) = 1
C
C           Compute the reciprocal of the condition number of R
C           or R + B'XB.
C           Workspace: need 3*M.
C
            CALL DPOCON( UPLO, M, R, LDR, RNORMP, RCOND, DWORK, IWORK,
     $                   IFAIL )
         ELSE
            OUFACT(1) = 2
C
C           Use UdU' or LdL' factorization, first restoring the saved
C           triangle.
C
            CALL DCOPY( M, DWORK, 1, R, LDR+1 )
            CALL MA02ED( NUPLO, M, R, LDR )
C
C           Workspace: need   1,
C                      prefer M*NB.
C
            CALL DSYTRF( UPLO, M, R, LDR, IPIV, DWORK, LDWORK, INFO )
            IF( INFO.GT.0 )
     $         RETURN
            WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) )
C
C           Compute the reciprocal of the condition number of R.
C           Workspace: need   2*M.
C
            CALL DSYCON( UPLO, M, R, LDR, IPIV, RNORMP, RCOND, DWORK,
     $                   IWORK, IFAIL )
         END IF
      END IF
C
C     Return if the matrix is singular to working precision.
C
      DWORK(2) = RCOND
      IF( RCOND.LT.EPS ) THEN
         INFO = M + 1
         RETURN
      END IF
C
      IF ( OUFACT(1).EQ.1 ) THEN
C
C        Solve the positive definite linear system.
C
         IF ( WITHF ) THEN
            CALL DTRSM(  'Right', UPLO, TRL, 'Non-unit', N, M, ONE, R,
     $                   LDR, H, LDH )
         ELSE IF ( WITHD ) THEN
            CALL DTRMM(  'Right', UPLO, TRL, 'Non-unit', N, M, ONE, R,
     $                   LDR, H, LDH )
            CALL DTRSM(  'Left',  UPLO, TRL, 'Non-unit', M, N, ONE, R,
     $                   LDR, K, LDK )
         ELSE IF ( .NOT.WITHC ) THEN
            CALL DPOTRS( UPLO, M, N, R, LDR, K, LDK, IFAIL )
         END IF
      ELSE
C
C        Solve the indefinite linear system.
C
         CALL DSYTRS( UPLO, M, N, R, LDR, IPIV, K, LDK, IFAIL )
      END IF
C
C     Set the optimal workspace.
C
      DWORK(1) = WRKOPT
C
      RETURN
C *** Last line of SG02ND ***
      END
