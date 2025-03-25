      SUBROUTINE SB02ND( DICO, FACT, UPLO, JOBL, N, M, P, A, LDA, B,
     $                   LDB, R, LDR, IPIV, L, LDL, X, LDX, RNORM, F,
     $                   LDF, OUFACT, IWORK, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To compute the optimal feedback matrix F for the problem of
C     optimal control given by
C
C                        -1
C          F = (R + B'XB)  (B'XA + L')                           (1)
C
C     in the discrete-time case and
C
C               -1
C          F = R  (B'X + L')                                     (2)
C
C     in the continuous-time case, where A, B and L are N-by-N, N-by-M
C     and N-by-M matrices respectively; R and X are M-by-M and N-by-N
C     symmetric matrices respectively.
C
C     Optionally, matrix R may be specified in a factored form, and L
C     may be zero.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     DICO    CHARACTER*1
C             Specifies the equation from which F is to be determined,
C             as follows:
C             = 'D':  Equation (1), discrete-time case;
C             = 'C':  Equation (2), continuous-time case.
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
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices A and X.  N >= 0.
C             No computations are performed if MIN(N,M) = 0.
C
C     M       (input) INTEGER
C             The number of system inputs.  M >= 0.
C
C     P       (input) INTEGER
C             The number of rows of the matrix D.
C             P >= M for DICO = 'C';
C             P >= 0 for DICO = 'D'.
C             This parameter must be specified only for FACT = 'D'.
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
C     B       (input/worksp.) DOUBLE PRECISION array, dimension (LDB,M)
C             The leading N-by-M part of this array must contain the
C             input matrix B of the system.
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
C             result, if DICO = 'C', DICO = 'D', or (DICO = 'D' and
C             (FACT = 'D' or FACT = 'C') and UPLO = 'L'), respectively.
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
C             contain the cross weighting matrix L.
C             If JOBL = 'Z', this array is not referenced.
C
C     LDL     INTEGER
C             The leading dimension of array L.
C             LDL >= MAX(1,N) if JOBL = 'N';
C             LDL >= 1        if JOBL = 'Z'.
C
C     X       (input/output) DOUBLE PRECISION array, dimension (LDX,N)
C             On entry, the leading N-by-N part of this array must
C             contain the solution matrix X of the algebraic Riccati
C             equation as produced by SLICOT Library routines SB02MD or
C             SB02OD. Matrix X is assumed non-negative definite if
C             DICO = 'D' and (FACT = 'D' or FACT = 'C').
C             The full matrix X must be given on input if LDWORK < N*M
C             or if DICO = 'D' and (FACT = 'D' or FACT = 'C').
C             On exit, if DICO = 'D', FACT = 'D' or FACT = 'C', and
C             OUFACT(2) = 1, the N-by-N upper triangular part
C             (if UPLO = 'U') or lower triangular part (if UPLO = 'L')
C             of this array contains the Cholesky factor of the given
C             matrix X, which is found to be positive definite.
C             On exit, if DICO = 'D', FACT = 'D' or 'C', OUFACT(2) = 2,
C             and INFO < M+2 (but INFO >= 0), the leading N-by-N part of
C             this array contains the matrix of orthonormal eigenvectors
C             of X.
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
C     F       (output) DOUBLE PRECISION array, dimension (LDF,N)
C             The leading M-by-N part of this array contains the
C             optimal feedback matrix F.
C             This array is not referenced if DICO = 'C' and FACT = 'D'
C             and P < M.
C
C     LDF     INTEGER
C             The leading dimension of array F.  LDF >= MAX(1,M).
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
C             DICO = 'D'); DWORK(2) is set to 1 if N = 0.
C             On exit, if LDWORK = -2 on input or INFO = -25, then
C             DWORK(1) returns the minimal value of LDWORK.
C             If on exit INFO = 0, and OUFACT(2) = 2, then DWORK(3),...,
C             DWORK(N+2) contain the eigenvalues of X, in ascending
C             order.
C
C     LDWORK  INTEGER
C             Dimension of working array DWORK.
C             LDWORK >= max(2,2*M)           if FACT =  'U';
C             LDWORK >= max(2,3*M)           if FACT <> 'U', DICO = 'C';
C             LDWORK >= max(2,3*M,N)         if FACT =  'N', DICO = 'D';
C             LDWORK >= max(N+3*M+2,4*N+1)   if FACT <> 'N', DICO = 'D'.
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
C     The optimal feedback matrix F is obtained as the solution to the
C     system of linear equations
C
C        (R + B'XB) * F = B'XA + L'
C
C     in the discrete-time case and
C
C        R * F = B'X + L'
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
C                                    3     2
C     For DICO = 'C', it requires O(m  + mn ) floating point operations
C                           2
C     if FACT = 'N' and O(mn ) floating point operations, otherwise.
C     For DICO = 'D', the operation counts are similar, but additional
C        3
C     O(n ) floating point operations may be needed in the worst case.
C     These estimates assume that M <= N.
C
C     CONTRIBUTORS
C
C     Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Sep. 1997.
C     Supersedes Release 2.0 routine SB02BD by M. Vanbegin, and
C     P. Van Dooren, Philips Research Laboratory, Brussels, Belgium.
C
C     REVISIONS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Dec. 2013,
C     Jan. 2014.
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
      CHARACTER         DICO, FACT, JOBL, UPLO
      INTEGER           INFO, LDA, LDB, LDF, LDL, LDR, LDWORK, LDX, M,
     $                  N, P
      DOUBLE PRECISION  RNORM
C     .. Array Arguments ..
      INTEGER           IPIV(*), IWORK(*), OUFACT(2)
      DOUBLE PRECISION  A(LDA,*), B(LDB,*), DWORK(*), F(LDF,*),
     $                  L(LDL,*), R(LDR,*), X(LDX,*)
C     .. Local Scalars ..
      LOGICAL           DISCR, LFACTA, LFACTC, LFACTD, LFACTU, LNFACT,
     $                  LUPLOU, SUFWRK, WITHL
      CHARACTER         NT, NUPLO, TR, TRL
      INTEGER           I, IFAIL, JW, JZ, MS, NR, WRKMIN, WRKOPT
      DOUBLE PRECISION  EPS, RCOND, RNORMP, TEMP
C     .. Local Arrays ..
      DOUBLE PRECISION  DUMMY(1)
C     .. External Functions ..
      LOGICAL           LSAME
      DOUBLE PRECISION  DLAMCH, DLANSY
      EXTERNAL          DLAMCH, DLANSY, LSAME
C     .. External Subroutines ..
      EXTERNAL          DAXPY, DCOPY, DGEMM, DGEQRF, DLACPY, DLASET,
     $                  DPOCON, DPOTRF, DPOTRS, DSCAL, DSYCON, DSYEV,
     $                  DSYMM, DSYTRF, DSYTRS, DTRCON, DTRMM, MA02AD,
     $                  MA02ED, MB01RB, MB04KD, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DBLE, INT, MAX, MIN, SQRT
C     .. Executable Statements ..
C
      INFO   = 0
      DISCR  = LSAME( DICO, 'D' )
      LFACTC = LSAME( FACT, 'C' )
      LFACTD = LSAME( FACT, 'D' )
      LFACTU = LSAME( FACT, 'U' )
      LUPLOU = LSAME( UPLO, 'U' )
      WITHL  = LSAME( JOBL, 'N' )
      LFACTA = LFACTC .OR. LFACTD .OR. LFACTU
      LNFACT = .NOT.LFACTA
C
C     Test the input scalar arguments.
C
      IF( .NOT.DISCR .AND. .NOT.LSAME( DICO, 'C' ) ) THEN
         INFO = -1
      ELSE IF( ( LNFACT .AND. .NOT.LSAME( FACT, 'N' ) ) .OR.
     $         ( DISCR  .AND. LFACTU ) ) THEN
         INFO = -2
      ELSE IF( .NOT.LUPLOU .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -3
      ELSE IF( .NOT.WITHL  .AND. .NOT.LSAME( JOBL, 'Z' ) ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( M.LT.0 ) THEN
         INFO = -6
      ELSE IF( LFACTD .AND. ( P.LT.0 .OR. ( .NOT.DISCR .AND. P.LT.M ) )
     $       ) THEN
         INFO = -7
      ELSE IF( LDA.LT.1 .OR. ( DISCR .AND. LDA.LT.N ) ) THEN
         INFO = -9
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -11
      ELSE IF( LDR.LT.MAX( 1, M ) .OR. ( LFACTD .AND. 
     $         LDR.LT.MAX( 1, P ) ) ) THEN
         INFO = -13
      ELSE IF( LDL.LT.1 .OR. ( WITHL .AND. LDL.LT.N ) ) THEN
         INFO = -16
      ELSE IF( LDX.LT.MAX( 1, N ) ) THEN
         INFO = -18
      ELSE IF( LFACTU ) THEN
         IF( RNORM.LT.ZERO )
     $      INFO = -19
      END IF
      IF ( INFO.EQ.0 ) THEN
         IF( LDF.LT.MAX( 1, M ) ) THEN
            INFO = -21
         ELSE
            IF ( DISCR ) THEN
               IF( LNFACT ) THEN
                  WRKMIN = MAX( 2, 3*M, N )
               ELSE
                  WRKMIN = MAX( N + 3*M + 2, 4*N + 1 )
               END IF
            ELSE
               IF( LFACTU ) THEN
                  WRKMIN = MAX( 2, 2*M )
               ELSE
                  WRKMIN = MAX( 2, 3*M )
               END IF
            END IF
            IF( LDWORK.EQ.-1 ) THEN
               WRKOPT = MAX( WRKMIN, N*M )
               IF ( LFACTD ) THEN
                  CALL DGEQRF( P, M, R, LDR, DWORK, DWORK, -1, IFAIL )
                  WRKOPT = MAX( WRKOPT, INT( DWORK(1) )+MIN( P, M ) )
               END IF
               IF( LFACTA ) THEN
                  IF( DISCR ) THEN
                     CALL DSYEV( 'Vectors', 'Lower', N, X, LDX, DWORK,
     $                           DWORK, -1, IFAIL )
                     WRKOPT = MAX( WRKOPT, INT( DWORK(1) )+N+2,
     $                             N*M+2*N+2 )
                  END IF
               ELSE
                  CALL DSYTRF( UPLO, M, R, LDR, IPIV, DWORK, -1, IFAIL )
                  WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) )
               END IF
               DWORK(1) = WRKOPT
               RETURN
            ELSE IF( LDWORK.EQ.-2 ) THEN
               DWORK(1) = WRKMIN
               RETURN
            ELSE IF( LDWORK.LT.WRKMIN ) THEN
               INFO = -25
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
         CALL XERBLA( 'SB02ND', -INFO )
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
      NT = 'No transpose'
      TR = 'Transpose'
C
      EPS = DLAMCH( 'Precision' )
C
C     Determine the right-hand side of the matrix equation, and R+B'XB,
C     if needed.
C     Compute  B'X  in F or XB in the workspace, if enough space. In the
C     first case and for DICO = 'D' and FACT = 'N', compute R+B'XB in R.
C     Then, compute in F
C        B'XA + L', if DICO = 'D';
C        B'X  + L', if DICO = 'C'.
C     In the second case, reverse the order of the last two steps.
C
C     (Note: Comments in the code beginning "Workspace:" describe the
C     minimal amount of real workspace needed at that point in the
C     code, as well as the preferred amount for good performance.
C     NB refers to the optimal block size for the immediately
C     following subroutine, as returned by ILAENV.)
C
C     Workspace: need   0;
C                prefer M*N. This will need only a triangle of X.
C
      WRKOPT = MAX( WRKMIN, N*M )
      SUFWRK = LDWORK.GE.N*M
      IF ( SUFWRK ) THEN
         IF ( DISCR .OR. .NOT.WITHL ) THEN
            CALL DSYMM(  'Left', UPLO, N, M, ONE, X, LDX, B, LDB, ZERO,
     $                   DWORK, N )
            IF ( WITHL ) THEN
               CALL MA02AD( 'All', N, M, L, LDL, F, LDF )
               CALL DGEMM(  TR, NT, M, N, N, ONE, DWORK, N, A, LDA, ONE,
     $                      F, LDF )
            ELSE IF ( DISCR ) THEN
               CALL DGEMM(  TR, NT, M, N, N, ONE, DWORK, N, A, LDA,
     $                      ZERO, F, LDF )
            ELSE
               CALL MA02AD( 'All', N, M, DWORK, N, F, LDF )
            END IF
         ELSE
            CALL DLACPY( 'All', N, M, L, LDL, DWORK, N )
            CALL DSYMM(  'Left', UPLO, N, M, ONE, X, LDX, B, LDB, ONE,
     $                   DWORK, N )
            CALL MA02AD( 'All', N, M, DWORK, N, F, LDF )
         END IF
      ELSE
         CALL DGEMM( TR, NT, M, N, N, ONE, B, LDB, X, LDX, ZERO,
     $               F, LDF )
      END IF
C
      IF ( LNFACT ) THEN
C
C        R not factored.
C
         IF ( DISCR ) THEN
C
C           Discrete-time case. Compute a triangle of R + B'XB.
C
            IF ( SUFWRK ) THEN
               CALL MB01RB( 'Left', UPLO, TR, M, N, ONE, ONE, R, LDR,
     $                      DWORK, N, B, LDB, IFAIL )
            ELSE
               CALL MB01RB( 'Left', UPLO, NT, M, N, ONE, ONE, R, LDR, F,
     $                      LDF, B, LDB, IFAIL )
            END IF
         END IF
C
C        Compute the 1-norm of the matrix  R  or  R + B'XB.
C        Workspace: need M.
C
         RNORMP = DLANSY( '1-norm', UPLO, M, R, LDR, DWORK )
      END IF
C
      IF ( DISCR .AND. .NOT.SUFWRK ) THEN
         MS = MAX( LDWORK/N, 1 )
C
C        Postmultiply B'X by A.
C        Workspace: need   N;
C                   prefer N*M.
C
         DO 10 I = 1, M, MS
            NR = MIN( MS, M-I+1 )
            CALL DLACPY( 'All', NR, N, F(I,1), LDF, DWORK, NR )
            CALL DGEMM(  NT, NT, NR, N, N, ONE, DWORK, NR, A, LDA, ZERO,
     $                   F(I,1), LDF )
   10    CONTINUE
C
      END IF
C
      IF( WITHL .AND. .NOT.SUFWRK ) THEN
C
C        Add L'.
C
         DO 20 I = 1, M
            CALL DAXPY( N, ONE, L(1,I), 1, F(I,1), LDF )
   20    CONTINUE
C
      END IF
C
C     Solve the matrix equation.
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
            DO 30 I = 1, M
               IF ( R(I,I).LT.ZERO )
     $            CALL DSCAL( M-I+1, -ONE, R(I,I), LDR ) 
               IF ( .NOT.LUPLOU )
     $            CALL DCOPY( I-1, R(1,I), 1, R(I,1), LDR )
   30       CONTINUE
C
         END IF
C
         IF ( DISCR ) THEN
            JZ = 0
C
            IF ( LUPLOU ) THEN
               NUPLO = 'Lower'
            ELSE
               NUPLO = 'Upper'
            END IF
C
C           Discrete-time case. Update the factorization for B'XB.
C           Try first the Cholesky factorization of X, saving the
C           diagonal of X, in order to recover it, if X is not positive
C           definite. In the later case, use spectral factorization.
C           Workspace: need N.
C           Define     JW = 1   for Cholesky factorization of X,
C                      JW = N+3 for spectral factorization of X.
C
            CALL DCOPY(  N, X, LDX+1, DWORK, 1 )
            CALL DPOTRF( UPLO, N, X, LDX, IFAIL )
C
            IF ( IFAIL.EQ.0 ) THEN
C
C              Use Cholesky factorization of X to compute chol(X)*B.
C
               JW = 1
               OUFACT(2) = 1
               IF ( LUPLOU ) THEN
                  TRL = NT
               ELSE
                  TRL = TR
               END IF
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
   40          CONTINUE
               IF ( ABS( DWORK(JZ+3) ).LE.TEMP ) THEN
                  JZ = JZ + 1
                  IF ( JZ.LT.N ) GO TO 40
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
                  WRKOPT = MAX( WRKOPT, N*M + JW - 1 )
                  MS = MAX( ( LDWORK - JW + 1 )/N, 1 )
C
                  DO 50 I = 1, M, MS
                     NR = MIN( MS, M-I+1 )
                     CALL DLACPY( 'All', N, NR, B(1,I), LDB, DWORK(JW),
     $                            N )
                     CALL DGEMM(  TR, NT, N-JZ, NR, N, ONE, X(1,JZ+1),
     $                            LDX, DWORK(JW), N, ZERO, B(JZ+1,I),
     $                            LDB )
   50             CONTINUE
C
                  DO 60 I = JZ + 1, N
                     CALL DSCAL( M, SQRT( DWORK(I+2) ), B(I,1), LDB )
   60             CONTINUE
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
C           Workspace: need JW+2*M-1.
C
            CALL MB04KD( 'Full', M, 0, N-JZ, R, LDR, B(JZ+1,1), LDB,
     $                   DUMMY, N, DUMMY, M, DWORK(JW), DWORK(JW+M) )
C
C           Make positive the diagonal elements of the triangular
C           factor.
C
            DO 70 I = 1, M
               IF ( R(I,I).LT.ZERO )
     $            CALL DSCAL( M-I+1, -ONE, R(I,I), LDR )
   70       CONTINUE
C
            IF ( .NOT.LUPLOU )
C
C              Construct the lower triangle.
C
     $         CALL MA02ED( NUPLO, M, R, LDR )
C
         ELSE
            JW = 1
         END IF
C
C        Compute the condition number of the coefficient matrix.
C
         IF ( .NOT.LFACTU ) THEN
C
C           Workspace: need JW+3*M-1.
C
            CALL DTRCON( '1-norm', UPLO, 'Non unit', M, R, LDR, RCOND,
     $                    DWORK(JW), IWORK, IFAIL )
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
C           Compute the reciprocal of the condition number of R.
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
            IF ( LUPLOU ) THEN
               NUPLO = 'Lower'
            ELSE
               NUPLO = 'Upper'
            END IF
C
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
         CALL DPOTRS( UPLO, M, N, R, LDR, F, LDF, IFAIL )
      ELSE
C
C        Solve the indefinite linear system.
C
         CALL DSYTRS( UPLO, M, N, R, LDR, IPIV, F, LDF, IFAIL )
      END IF
C
C     Set the optimal workspace.
C
      DWORK(1) = WRKOPT
C
      RETURN
C *** Last line of SB02ND ***
      END
