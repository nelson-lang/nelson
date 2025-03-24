      SUBROUTINE SB02MX( JOBG, JOBL, FACT, UPLO, TRANS, FLAG, DEF, N, M,
     $                   A, LDA, B, LDB, Q, LDQ, R, LDR, L, LDL, IPIV,
     $                   OUFACT, G, LDG, IWORK, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To compute the following matrices
C
C                -1
C         G = B*R  *B',
C
C         -               -1
C         A = A +/- op(B*R  *L'),
C
C         -            -1
C         Q = Q +/- L*R  *L',
C
C     where A, B, Q, R, L, and G are N-by-N, N-by-M, N-by-N, M-by-M,
C     N-by-M, and N-by-N matrices, respectively, with Q, R and G
C     symmetric matrices, and op(W) is one of
C
C         op(W) = W  or  op(W) = W'.
C
C     When R is well-conditioned with respect to inversion, standard
C     algorithms for solving linear-quadratic optimization problems will
C     then also solve optimization problems with coupling weighting
C     matrix L. Moreover, a gain in efficiency is possible using matrix
C     G in the deflating subspace algorithms (see SLICOT Library routine
C     SB02OD) or in the Newton's algorithms (see SLICOT Library routine
C     SG02CD).
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOBG    CHARACTER*1
C             Specifies whether or not the matrix G is to be computed,
C             as follows:
C             = 'G':  Compute G;
C             = 'N':  Do not compute G.
C
C     JOBL    CHARACTER*1
C             Specifies whether or not the matrix L is zero, as follows:
C             = 'Z':  L is zero;
C             = 'N':  L is nonzero.
C
C     FACT    CHARACTER*1
C             Specifies how the matrix R is given (factored or not), as
C             follows:
C             = 'N':  Array R contains the matrix R;
C             = 'C':  Array R contains the Cholesky factor of R;
C             = 'U':  Array R contains the factors of the symmetric
C                     indefinite UdU' or LdL' factorization of R.
C
C     UPLO    CHARACTER*1
C             Specifies which triangle of the matrices R, Q (if
C             JOBL = 'N'), and G (if JOBG = 'G') is stored, as follows:
C             = 'U':  Upper triangle is stored;
C             = 'L':  Lower triangle is stored.
C
C     TRANS   CHARACTER*1
C             Specifies the form of op(W) to be used in the matrix
C             multiplication, as follows:
C             = 'N':  op(W) = W;
C             = 'T':  op(W) = W';
C             = 'C':  op(W) = W'.
C
C     FLAG    CHARACTER*1
C             Specifies which sign is used, as follows:
C             = 'P':  The plus  sign is used;
C             = 'M':  The minus sign is used.
C
C     DEF     CHARACTER*1
C             If FACT = 'N', specifies whether or not it is assumed that
C             matrix R is positive definite, as follows:
C             = 'D':  Matrix R is assumed positive definite;
C             = 'I':  Matrix R is assumed indefinite.
C             Both values can be used to perform the computations,
C             irrespective to the R definiteness, but using the adequate
C             value will save some computational effort (see FURTHER
C             COMMENTS).
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices A, Q, and G, and the number of
C             rows of the matrices B and L.  N >= 0.
C
C     M       (input) INTEGER
C             The order of the matrix R, and the number of columns of
C             the matrices B and L.  M >= 0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, if JOBL = 'N', the leading N-by-N part of this
C             array must contain the matrix A.
C             On exit, if JOBL = 'N', and INFO = 0, the leading N-by-N
C                                                    -
C             part of this array contains the matrix A.
C             If JOBL = 'Z', this array is not referenced.
C
C     LDA     INTEGER
C             The leading dimension of array A.
C             LDA >= MAX(1,N) if JOBL = 'N';
C             LDA >= 1        if JOBL = 'Z'.
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB,M)
C             On entry, the leading N-by-M part of this array must
C             contain the matrix B.
C             On exit, if OUFACT = 1, and INFO = 0, the leading N-by-M
C                                                             -1
C             part of this array contains the matrix B*chol(R)  .
C             On exit, B is unchanged if OUFACT <> 1 (hence also when
C             FACT = 'U').
C
C     LDB     INTEGER
C             The leading dimension of array B.  LDB >= MAX(1,N).
C
C     Q       (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
C             On entry, if JOBL = 'N', the leading N-by-N upper
C             triangular part (if UPLO = 'U') or lower triangular part
C             (if UPLO = 'L') of this array must contain the upper
C             triangular part or lower triangular part, respectively, of
C             the symmetric matrix Q. The strictly lower triangular part
C             (if UPLO = 'U') or strictly upper triangular part (if
C             UPLO = 'L') is not referenced.
C             On exit, if JOBL = 'N' and INFO = 0, the leading N-by-N
C             upper triangular part (if UPLO = 'U') or lower triangular
C             part (if UPLO = 'L') of this array contains the upper
C             triangular part or lower triangular part, respectively, of
C                                  -            -1
C             the symmetric matrix Q = Q +/- L*R  *L'.
C             If JOBL = 'Z', this array is not referenced.
C
C     LDQ     INTEGER
C             The leading dimension of array Q.
C             LDQ >= MAX(1,N) if JOBL = 'N';
C             LDQ >= 1        if JOBL = 'Z'.
C
C     R       (input/output) DOUBLE PRECISION array, dimension (LDR,M)
C             On entry, if FACT = 'N', the leading M-by-M upper
C             triangular part (if UPLO = 'U') or lower triangular part
C             (if UPLO = 'L') of this array must contain the upper
C             triangular part or lower triangular part, respectively,
C             of the symmetric input weighting matrix R.
C             On entry, if FACT = 'C', the leading M-by-M upper
C             triangular part (if UPLO = 'U') or lower triangular part
C             (if UPLO = 'L') of this array must contain the Cholesky
C             factor of the positive definite input weighting matrix R
C             (as produced by LAPACK routine DPOTRF).
C             On entry, if FACT = 'U', the leading M-by-M upper
C             triangular part (if UPLO = 'U') or lower triangular part
C             (if UPLO = 'L') of this array must contain the factors of
C             the UdU' or LdL' factorization, respectively, of the
C             symmetric indefinite input weighting matrix R (as produced
C             by LAPACK routine DSYTRF).
C             If FACT = 'N' and DEF = 'D', the strictly lower triangular
C             part (if UPLO = 'U') or strictly upper triangular part
C             (if UPLO = 'L') of this array is used as workspace (filled
C             in by symmetry). If FACT = 'N' and DEF = 'I', the strictly
C             lower triangular part (if UPLO = 'U') or strictly upper
C             triangular part (if UPLO = 'L') is unchanged.
C             On exit, if OUFACT = 1, and INFO = 0 (or INFO = M+1),
C             the leading M-by-M upper triangular part (if UPLO = 'U')
C             or lower triangular part (if UPLO = 'L') of this array
C             contains the Cholesky factor of the given input weighting
C             matrix.
C             On exit, if OUFACT = 2, and INFO = 0 (or INFO = M+1),
C             the leading M-by-M upper triangular part (if UPLO = 'U')
C             or lower triangular part (if UPLO = 'L') of this array
C             contains the factors of the UdU' or LdL' factorization,
C             respectively, of the given input weighting matrix.
C             On exit R is unchanged if FACT = 'C' or 'U'.
C
C     LDR     INTEGER
C             The leading dimension of array R.  LDR >= MAX(1,M).
C
C     L       (input/output) DOUBLE PRECISION array, dimension (LDL,M)
C             On entry, if JOBL = 'N', the leading N-by-M part of this
C             array must contain the matrix L.
C             On exit, if JOBL = 'N', OUFACT = 1, and INFO = 0, the
C             leading N-by-M part of this array contains the matrix
C                      -1
C             L*chol(R)  .
C             On exit, L is unchanged if OUFACT <> 1 (hence also when
C             FACT = 'U').
C             L is not referenced if JOBL = 'Z'.
C
C     LDL     INTEGER
C             The leading dimension of array L.
C             LDL >= MAX(1,N) if JOBL = 'N';
C             LDL >= 1        if JOBL = 'Z'.
C
C     IPIV    (input/output) INTEGER array, dimension (M)
C             On entry, if FACT = 'U', this array must contain details
C             of the interchanges performed and the block structure of
C             the d factor in the UdU' or LdL' factorization of matrix R
C             (as produced by LAPACK routine DSYTRF).
C             On exit, if OUFACT = 2, this array contains details of
C             the interchanges performed and the block structure of the
C             d factor in the UdU' or LdL' factorization of matrix R,
C             as produced by LAPACK routine DSYTRF.
C             This array is not referenced if FACT = 'C'.
C
C     OUFACT  (output) INTEGER
C             Information about the factorization finally used.
C             OUFACT = 0:  no factorization of R has been used (M = 0);
C             OUFACT = 1:  Cholesky factorization of R has been used;
C             OUFACT = 2:  UdU' (if UPLO = 'U') or LdL' (if UPLO = 'L')
C                          factorization of R has been used.
C
C     G       (output) DOUBLE PRECISION array, dimension (LDG,N)
C             If JOBG = 'G', and INFO = 0, the leading N-by-N upper
C             triangular part (if UPLO = 'U') or lower triangular part
C             (if UPLO = 'L') of this array contains the upper
C             triangular part (if UPLO = 'U') or lower triangular part
C                                                                 -1
C             (if UPLO = 'L'), respectively, of the matrix G = B*R  B'.
C             If JOBG = 'N', this array is not referenced.
C
C     LDG     INTEGER
C             The leading dimension of array G.
C             LDG >= MAX(1,N) if JOBG = 'G';
C             LDG >= 1        if JOBG = 'N'.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (M)
C             If FACT = 'C' or FACT = 'U', this array is not referenced.
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0 or LDWORK = -1, DWORK(1) returns the
C             optimal value of LDWORK; if FACT = 'N' and LDWORK is set
C             as specified below, DWORK(2) contains the reciprocal
C             condition number of the given matrix R. DWORK(2) is set to
C             zero if M = 0.
C             On exit, if LDWORK = -2 on input or INFO = -26, then
C             DWORK(1) returns the minimal value of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.
C             LDWORK >= 1              if FACT = 'C' or  (FACT = 'U' and
C                                         JOBG = 'N' and  JOBL = 'Z');
C             LDWORK >= MAX(2,3*M)     if FACT = 'N' and  JOBG = 'N' and
C                                                         JOBL = 'Z';
C             LDWORK >= MAX(2,3*M,N*M) if FACT = 'N' and (JOBG = 'G' or
C                                                         JOBL = 'N');
C             LDWORK >= MAX(1,N*M)     if FACT = 'U' and (JOBG = 'G' or
C                                                         JOBL = 'N').
C             For optimum performance LDWORK should be larger than 3*M,
C             if FACT = 'N'.
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
C             = i:  if the i-th element (1 <= i <= M) of the d factor is
C                   exactly zero; the UdU' (or LdL') factorization has
C                   been completed, but the block diagonal matrix d is
C                   exactly singular;
C             = M+1:  if the matrix R is numerically singular.
C
C     METHOD
C                            -     -
C     The matrices G, and/or A and Q are evaluated using the given or
C     computed symmetric factorization of R.
C
C     NUMERICAL ASPECTS
C
C     The routine should not be used when R is ill-conditioned.
C
C     FURTHER COMMENTS
C
C     Using argument TRANS allows to avoid the transposition of matrix A
C     needed to solve optimal filtering/estimation problems by the same
C     routines solving optimal control problems.
C     If DEF is set to 'D', but R is indefinite, the computational
C     effort for factorization will be approximately double, since
C     Cholesky factorization, tried first, will fail, and symmetric
C     indefinite factorization will then be used.
C     If DEF is set to 'I', but R is positive definite, the
C     computational effort will be slightly higher than that when using
C     Cholesky factorization. It is recommended to use DEF = 'D' also if
C     the definiteness is not known, but M is (much) smaller than N.
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Apr. 2014.
C     This is an extended version of the SLICOT Library routine SB02MT.
C
C     REVISIONS
C
C     V. Sima, May 2014, Aug. 2017, Oct. 2017.
C
C     KEYWORDS
C
C     Algebraic Riccati equation, closed loop system, continuous-time
C     system, discrete-time system, optimal regulator, Schur form.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         DEF, FACT, FLAG, JOBG, JOBL, TRANS, UPLO
      INTEGER           INFO, LDA, LDB, LDG, LDL, LDQ, LDR, LDWORK, M,
     $                  N, OUFACT
C     .. Array Arguments ..
      INTEGER           IPIV(*), IWORK(*)
      DOUBLE PRECISION  A(LDA,*), B(LDB,*), DWORK(*), G(LDG,*),
     $                  L(LDL,*), Q(LDQ,*), R(LDR,*)
C     .. Local Scalars ..
      LOGICAL           BNZER, LDEF, LFACTC, LFACTU, LFLAG, LJOBG,
     $                  LJOBL, LNFACT, LTRANS, LUPLOU
      CHARACTER         NT, TR, TRANSU
      INTEGER           J, WRKMIN, WRKOPT
      DOUBLE PRECISION  EPS, RCOND, RNORM, TEMP
C     .. External Functions ..
      LOGICAL           LSAME
      DOUBLE PRECISION  DLAMCH, DLANGE, DLANSY
      EXTERNAL          DLAMCH, DLANGE, DLANSY, LSAME
C     .. External Subroutines ..
      EXTERNAL          DCOPY, DGEMM, DLASET, DPOCON, DPOTRF, DSYCON,
     $                  DSYRK, DSYTRF, DSYTRS, DTRSM, MA02ED, MB01RB,
     $                  XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         INT, MAX
C     .. Executable Statements ..
C
      INFO   = 0
      LJOBG  = LSAME( JOBG,  'G' )
      LJOBL  = LSAME( JOBL,  'N' )
      LFACTC = LSAME( FACT,  'C' )
      LFACTU = LSAME( FACT,  'U' )
      LTRANS = LSAME( TRANS, 'N' )
      LUPLOU = LSAME( UPLO,  'U' )
      LFLAG  = LSAME( FLAG,  'M' )
      LDEF   = LSAME( DEF,   'D' )
      LNFACT = .NOT.( LFACTC .OR. LFACTU )
C
C     Test the input scalar arguments.
C
      IF(      .NOT.LJOBG  .AND. .NOT.LSAME( JOBG,  'N' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.LJOBL  .AND. .NOT.LSAME( JOBL,  'Z' ) ) THEN
         INFO = -2
      ELSE IF(      LNFACT .AND. .NOT.LSAME( FACT,  'N' ) ) THEN
         INFO = -3
      ELSE IF( .NOT.LUPLOU .AND. .NOT.LSAME( UPLO,  'L' ) ) THEN
         INFO = -4
      ELSE IF( .NOT.LTRANS .AND. .NOT.LSAME( TRANS, 'T' )
     $                     .AND. .NOT.LSAME( TRANS, 'C' ) ) THEN
         INFO = -5
      ELSE IF( .NOT.LFLAG  .AND. .NOT.LSAME( FLAG,  'P' ) ) THEN
         INFO = -6
      ELSE IF( .NOT.LDEF   .AND. .NOT.LSAME( DEF,   'I' )
     $                     .AND. LNFACT ) THEN
         INFO = -7
      ELSE IF( N.LT.0 ) THEN
         INFO = -8
      ELSE IF( M.LT.0 ) THEN
         INFO = -9
      ELSE IF( LDA.LT.1 .OR. ( LJOBL .AND. LDA.LT.N ) ) THEN
         INFO = -11
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -13
      ELSE IF( LDQ.LT.1 .OR. ( LJOBL .AND. LDQ.LT.N ) ) THEN
         INFO = -15
      ELSE IF( LDR.LT.MAX( 1, M ) ) THEN
         INFO = -17
      ELSE IF( LDL.LT.1 .OR. ( LJOBL .AND. LDL.LT.N ) ) THEN
         INFO = -19
      ELSE IF( LDG.LT.1 .OR. ( LJOBG .AND. LDG.LT.N ) ) THEN
         INFO = -23
      ELSE
         IF( LFACTC ) THEN
            WRKMIN = 1
         ELSE IF( LFACTU ) THEN
            IF( LJOBG .OR. LJOBL ) THEN
               WRKMIN = MAX( 1, N*M )
            ELSE
               WRKMIN = 1
            END IF
         ELSE
            IF( LJOBG .OR. LJOBL ) THEN
               WRKMIN = MAX( 2, 3*M, N*M )
            ELSE
               WRKMIN = MAX( 2, 3*M )
            END IF
         END IF
         IF( LDWORK.EQ.-1 ) THEN
            IF( LNFACT ) THEN
               CALL DSYTRF( UPLO, M, R, LDR, IPIV, DWORK, -1, INFO )
               WRKOPT = MAX( WRKMIN, INT( DWORK(1) ) )
            ELSE
               WRKOPT = WRKMIN
            END IF
            DWORK(1) = WRKOPT
            RETURN
         ELSE IF( LDWORK.EQ.-2 ) THEN
            DWORK(1) = WRKMIN
            RETURN
         ELSE IF( LDWORK.LT.WRKMIN ) THEN
            INFO = -26
            DWORK(1) = WRKMIN
         END IF
      END IF
C
      IF( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'SB02MX', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( M.EQ.0 ) THEN
         IF( LJOBG )
     $      CALL DLASET( UPLO, N, N, ZERO, ZERO, G, LDG )
         OUFACT   = 0
         DWORK(1) = WRKMIN
         IF( LNFACT )
     $      DWORK(2) = ZERO
         RETURN
      END IF
C
C     (Note: Comments in the code beginning "Workspace:" describe the
C     minimal amount of workspace needed at that point in the code,
C     as well as the preferred amount for good performance.
C     NB refers to the optimal block size for the immediately
C     following subroutine, as returned by ILAENV.)
C
      WRKOPT = 1
C
      IF( LNFACT ) THEN
C
C        Set relative machine precision.
C
         EPS = DLAMCH( 'Precision' )
C
C        Compute the norm of the matrix R, which is not factored.
C        Then, if DEF = 'D', save the given triangle of R in the other
C        strict triangle and the diagonal in the workspace, and try
C        Cholesky factorization.
C        Workspace: need M.
C
         RNORM = DLANSY( '1-norm', UPLO, M, R, LDR, DWORK )
C
         IF( LDEF ) THEN
            CALL DCOPY(  M, R, LDR+1, DWORK, 1 )
            CALL MA02ED( UPLO, M, R, LDR )
            CALL DPOTRF( UPLO, M, R, LDR, INFO )
            IF( INFO.EQ.0 ) THEN
C
C              Compute the reciprocal of the condition number of R.
C              Workspace: need 3*M.
C
               CALL DPOCON( UPLO, M, R, LDR, RNORM, RCOND, DWORK, IWORK,
     $                      INFO )
C
C              Return if the matrix is singular to working precision.
C
               OUFACT = 1
               IF( RCOND.LT.EPS ) THEN
                  INFO = M + 1
                  DWORK(2) = RCOND
                  RETURN
               END IF
               WRKOPT = MAX( WRKOPT, 3*M )
            ELSE
C
C              Restore the saved triangle, to use the UdU' or LdL'
C              factorization.
C
               CALL DCOPY( M, DWORK, 1, R, LDR+1 )
               IF( LUPLOU ) THEN
                  CALL MA02ED( 'Lower', M, R, LDR )
               ELSE
                  CALL MA02ED( 'Upper', M, R, LDR )
               END IF
            END IF
         END IF
C
         IF( .NOT.LDEF .OR. INFO.GT.0 ) THEN
C
C           Compute the UdU' or LdL' factorization.
C           Workspace: need   1,
C                      prefer M*NB.
C
            CALL DSYTRF( UPLO, M, R, LDR, IPIV, DWORK, LDWORK, INFO )
            OUFACT = 2
            IF( INFO.GT.0 ) THEN
               DWORK(2) = ZERO
               RETURN
            END IF
            WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) )
C
C           Compute the reciprocal of the condition number of R.
C           Workspace: need 2*M.
C
            CALL DSYCON( UPLO, M, R, LDR, IPIV, RNORM, RCOND, DWORK,
     $                   IWORK, INFO )
C
C           Return if the matrix is singular to working precision.
C
            IF( RCOND.LT.EPS ) THEN
               INFO = M + 1
               DWORK(2) = RCOND
               RETURN
            END IF
         END IF
      ELSE IF( LFACTC ) THEN
         OUFACT = 1
      ELSE
         OUFACT = 2
      END IF
C
      IF( N.GT.0 ) THEN
         NT = 'No transpose'
         TR = 'Transpose'
C
         IF( LJOBL ) THEN
            IF( LFLAG ) THEN
               TEMP = -ONE
            ELSE
               TEMP =  ONE
            END IF
         END IF
C
         BNZER = DLANGE( '1-norm', N, M, B, LDB, DWORK ).GT.ZERO
C
         IF( OUFACT.EQ.1 ) THEN
C
C           Solve positive definite linear system(s).
C
            IF( LUPLOU ) THEN
               TRANSU = NT
            ELSE
               TRANSU = TR
            END IF
C
            IF( BNZER ) THEN
C
C              Solve the system X*U = B, overwriting B with X.
C
               CALL DTRSM( 'Right', UPLO, TRANSU, 'Non-unit', N, M, ONE,
     $                     R, LDR, B, LDB )
C
               IF( LJOBG ) THEN
C                                            -1
C                 Compute the matrix  G = B*R  *B', multiplying X*X'.
C
                  CALL DSYRK( UPLO, NT, N, M, ONE, B, LDB, ZERO, G, LDG)
               END IF
            END IF
C
            IF( LJOBL ) THEN
C
C              Update matrices A (if B nonzero) and Q.
C
C              Solve the system Y*U = L, overwriting L with Y.
C
               CALL DTRSM( 'Right', UPLO, TRANSU, 'Non-unit', N, M, ONE,
     $                     R, LDR, L, LDL )
C
               IF( BNZER ) THEN
C
C                 Compute A <- A +/- op(X*Y').
C
                  IF( LTRANS ) THEN
                     CALL DGEMM( NT, TR, N, N, M, TEMP, B, LDB, L, LDL,
     $                           ONE, A, LDA )
                  ELSE
                     CALL DGEMM( NT, TR, N, N, M, TEMP, L, LDL, B, LDB,
     $                           ONE, A, LDA )
                  END IF
               END IF
C
C              Compute Q <- Q +/- Y*Y'.
C
               CALL DSYRK( UPLO, NT, N, M, TEMP, L, LDL, ONE, Q, LDQ )
            END IF
         ELSE
C
C           Solve indefinite linear system(s).
C
            IF( BNZER ) THEN
C
               IF( LJOBG .OR. .NOT.LTRANS ) THEN
C
C                 Solve the system UdU'*X = B' (or LdL'*X = B').
C                 Workspace: need N*M.
C
                  DO 10 J = 1, M
                     CALL DCOPY( N, B(1,J), 1, DWORK(J), M )
   10             CONTINUE
C
                  CALL DSYTRS( UPLO, M, N, R, LDR, IPIV, DWORK, M, INFO)
               END IF
C
               IF( LJOBG ) THEN
C                                                          -1
C                 Compute a triangle of the matrix  G = B*R  *B' = B*X.
C
                  CALL MB01RB( 'Left', UPLO, NT, N, M, ZERO, ONE, G,
     $                         LDG, B, LDB, DWORK, M, INFO )
               END IF
            END IF
C
            IF( LJOBL ) THEN
C
C              Update matrices A and Q.
C
               IF( .NOT.LTRANS .AND. BNZER ) THEN
C
C                 A <- A +/- L*X, if TRANS <> 'N'.
C
                  CALL DGEMM( NT, NT, N, N, M, TEMP, L, LDL, DWORK, M,
     $                        ONE, A, LDA )
               END IF
C
C              Solve the system UdU'*Y = L' (or LdL'*Y = L').
C
               DO 20 J = 1, M
                  CALL DCOPY( N, L(1,J), 1, DWORK(J), M )
   20          CONTINUE
C
               CALL DSYTRS( UPLO, M, N, R, LDR, IPIV, DWORK, M, INFO )
C
               IF( LTRANS .AND. BNZER ) THEN
C
C                 A <- A +/- B*Y, if TRANS =  'N'.
C
                  CALL DGEMM( NT, NT, N, N, M, TEMP, B, LDB, DWORK, M,
     $                        ONE, A, LDA )
               END IF
C                                               -            -1
C              Compute a triangle of the matrix Q = Q +/- L*R  *L'
C                                                 = Q +/- L*Y.
C
               CALL MB01RB( 'Left', UPLO, NT, N, M, ONE, TEMP, Q, LDQ,
     $                      L, LDL, DWORK, M, INFO )
            END IF
         END IF
      END IF
C
      DWORK(1) = WRKOPT
      IF( LNFACT )
     $   DWORK(2) = RCOND
C
C *** Last line of SB02MX ***
      RETURN
      END
