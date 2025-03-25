      SUBROUTINE MB01RB( SIDE, UPLO, TRANS, M, N, ALPHA, BETA, R, LDR,
     $                   A, LDA, B, LDB, INFO )
C
C     PURPOSE
C
C     To compute either the upper or lower triangular part of one of the
C     matrix formulas
C        _
C        R = alpha*R + beta*op( A )*B,                               (1)
C        _
C        R = alpha*R + beta*B*op( A ),                               (2)
C                                             _
C     where alpha and beta are scalars, R and R are m-by-m matrices,
C     op( A ) and B are m-by-n and n-by-m matrices for (1), or n-by-m
C     and m-by-n matrices for (2), respectively, and op( A ) is one of
C
C        op( A ) = A   or   op( A ) = A',  the transpose of A.
C
C     The result is overwritten on R.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     SIDE    CHARACTER*1
C             Specifies whether the matrix A appears on the left or
C             right in the matrix product as follows:
C                     _
C             = 'L':  R = alpha*R + beta*op( A )*B;
C                     _
C             = 'R':  R = alpha*R + beta*B*op( A ).
C
C     UPLO    CHARACTER*1                               _
C             Specifies which triangles of the matrices R and R are
C             computed and given, respectively, as follows:
C             = 'U':  the upper triangular part;
C             = 'L':  the lower triangular part.
C
C     TRANS   CHARACTER*1
C             Specifies the form of op( A ) to be used in the matrix
C             multiplication as follows:
C             = 'N':  op( A ) = A;
C             = 'T':  op( A ) = A';
C             = 'C':  op( A ) = A'.
C
C     Input/Output Parameters
C
C     M       (input) INTEGER           _
C             The order of the matrices R and R, the number of rows of
C             the matrix op( A ) and the number of columns of the
C             matrix B, for SIDE = 'L', or the number of rows of the
C             matrix B and the number of columns of the matrix op( A ),
C             for SIDE = 'R'.  M >= 0.
C
C     N       (input) INTEGER
C             The number of rows of the matrix B and the number of
C             columns of the matrix op( A ), for SIDE = 'L', or the
C             number of rows of the matrix op( A ) and the number of
C             columns of the matrix B, for SIDE = 'R'.  N >= 0.
C
C     ALPHA   (input) DOUBLE PRECISION
C             The scalar alpha. When alpha is zero then R need not be
C             set before entry.
C
C     BETA    (input) DOUBLE PRECISION
C             The scalar beta. When beta is zero then A and B are not
C             referenced.
C
C     R       (input/output) DOUBLE PRECISION array, dimension (LDR,M)
C             On entry with UPLO = 'U', the leading M-by-M upper
C             triangular part of this array must contain the upper
C             triangular part of the matrix R; the strictly lower
C             triangular part of the array is not referenced.
C             On entry with UPLO = 'L', the leading M-by-M lower
C             triangular part of this array must contain the lower
C             triangular part of the matrix R; the strictly upper
C             triangular part of the array is not referenced.
C             On exit, the leading M-by-M upper triangular part (if
C             UPLO = 'U'), or lower triangular part (if UPLO = 'L') of
C             this array contains the corresponding triangular part of
C                                 _
C             the computed matrix R.
C
C     LDR     INTEGER
C             The leading dimension of array R.  LDR >= MAX(1,M).
C
C     A       (input) DOUBLE PRECISION array, dimension (LDA,k), where
C             k = N  when  SIDE = 'L', and TRANS =  'N', or
C                          SIDE = 'R', and TRANS <> 'T';
C             k = M  when  SIDE = 'R', and TRANS = 'N', or
C                          SIDE = 'L', and TRANS <> 'T'.
C             On entry, if SIDE = 'L', and TRANS =  'N', or
C                          SIDE = 'R', and TRANS <> 'T',
C             the leading M-by-N part of this array must contain the
C             matrix A.
C             On entry, if SIDE = 'R', and TRANS =  'N', or
C                          SIDE = 'L', and TRANS <> 'T',
C             the leading N-by-M part of this array must contain the
C             matrix A.
C
C     LDA     INTEGER
C             The leading dimension of array A.  LDA >= MAX(1,l), where
C             l = M  when  SIDE = 'L', and TRANS =  'N', or
C                          SIDE = 'R', and TRANS <> 'T';
C             l = N  when  SIDE = 'R', and TRANS =  'N', or
C                          SIDE = 'L', and TRANS <> 'T'.
C
C     B       (input) DOUBLE PRECISION array, dimension (LDB,p), where
C             p = M  when  SIDE = 'L';
C             p = N  when  SIDE = 'R'.
C             On entry, the leading N-by-M part, if SIDE = 'L', or
C             M-by-N part, if SIDE = 'R', of this array must contain the
C             matrix B.
C
C     LDB     INTEGER
C             The leading dimension of array B.
C             LDB >= MAX(1,N), if SIDE = 'L';
C             LDB >= MAX(1,M), if SIDE = 'R'.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value.
C
C     METHOD
C
C     The matrix expression is evaluated taking the triangular
C     structure into account. A block algorithm is used.
C
C     FURTHER COMMENTS
C
C     The main application of this routine is when the result should
C     be a symmetric matrix, e.g., when B = X*op( A )', for (1), or
C     B = op( A )'*X, for (2), where B is already available and X = X'.
C     The required triangle only is computed and overwritten, contrary
C     to a general matrix multiplication operation.
C
C     This is a BLAS 3 version of the SLICOT Library routine MB01RX.
C
C     CONTRIBUTORS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Mar. 2013.
C
C     REVISIONS
C
C     V. Sima, Jul. 2013.
C
C     KEYWORDS
C
C     Elementary matrix operations, matrix algebra, matrix operations.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0 )
C                       NBS is a value used to switch from small to
C                       large block sizes. 
C                       N1P is a block size to be used for large
C                       matrices when SIDE = 'L' and TRANS = 'T', and
C                       N1L is a minimum block size N1 in this case.
C                       N2P and N2L are similar block sizes for N2,
C                       used when SIDE and TRANS have other values.
      INTEGER           N1L, N1P, N2L, N2P, NBS
      PARAMETER         ( N1L = 128, N1P = 512, N2L = 40, N2P = 128,
     $                    NBS = 48 )
C     .. Scalar Arguments ..
      CHARACTER         SIDE, TRANS, UPLO
      INTEGER           INFO, LDA, LDB, LDR, M, N
      DOUBLE PRECISION  ALPHA, BETA
C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), B(LDB,*), R(LDR,*)
C     .. Local Scalars ..
      LOGICAL           LSIDE, LTRANS, LUPLO
      INTEGER           I, IB, J, JB, MN, MX, N1, N2, NB, NBMIN, NX
C     .. Local Arrays ..
      DOUBLE PRECISION  D(1)
C     .. External Functions ..
      LOGICAL           LSAME
      INTEGER           ILAENV
      EXTERNAL          ILAENV, LSAME
C     .. External Subroutines ..
      EXTERNAL          DGEMM, DGEQRF, DLASCL, DLASET, MB01RX, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         INT, MAX, MIN
C     .. Executable Statements ..
C
C     Test the input scalar arguments.
C
      INFO   = 0
      LSIDE  = LSAME( SIDE,  'L' )
      LUPLO  = LSAME( UPLO,  'U' )
      LTRANS = LSAME( TRANS, 'T' ) .OR. LSAME( TRANS, 'C' )
C
      IF(      ( .NOT.LSIDE  ).AND.( .NOT.LSAME( SIDE,  'R' ) ) )THEN
         INFO = -1
      ELSE IF( ( .NOT.LUPLO  ).AND.( .NOT.LSAME( UPLO,  'L' ) ) )THEN
         INFO = -2
      ELSE IF( ( .NOT.LTRANS ).AND.( .NOT.LSAME( TRANS, 'N' ) ) )THEN
         INFO = -3
      ELSE IF( M.LT.0 ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDR.LT.MAX( 1, M ) ) THEN
         INFO = -9
      ELSE IF( LDA.LT.1 .OR.
     $   ( ( (      LSIDE .AND. .NOT.LTRANS ) .OR.
     $       ( .NOT.LSIDE .AND.      LTRANS ) ) .AND. LDA.LT.M ) .OR.
     $   ( ( (      LSIDE .AND.      LTRANS ) .OR.
     $       ( .NOT.LSIDE .AND. .NOT.LTRANS ) ) .AND. LDA.LT.N ) ) THEN
         INFO = -11
      ELSE IF( LDB.LT.1 .OR.
     $       (      LSIDE .AND. LDB.LT.N ) .OR.
     $       ( .NOT.LSIDE .AND. LDB.LT.M ) ) THEN
         INFO = -13
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'MB01RB', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ( M.EQ.0 )
     $   RETURN
C
      IF ( BETA.EQ.ZERO .OR. N.EQ.0 ) THEN
         IF ( ALPHA.EQ.ZERO ) THEN
C
C           Special case alpha = 0.
C
            CALL DLASET( UPLO, M, M, ZERO, ZERO, R, LDR )
         ELSE
C
C           Special case beta = 0 or N = 0.
C
            IF ( ALPHA.NE.ONE )
     $         CALL DLASCL( UPLO, 0, 0, ONE, ALPHA, M, M, R, LDR, INFO )
         END IF
         RETURN
      END IF
C
C     General case: beta <> 0.
C     Compute the required triangle of (1) or (2) using essentially
C     BLAS 3 operations.
C
C     Find the block size using DGEQRF.
C
      MX = MAX( M, N )
      MN = MIN( M, N )
      CALL DGEQRF( MX, MN, A, MX, D, D, -1, INFO )
      NB = INT( D(1) )/MN/8*8
C
      IF( NB.GT.1 .AND. NB.LT.M ) THEN
C
C        Determine when to cross over from blocked to unblocked code.
C
         NX = MAX( 0, ILAENV( 3, 'DGEQRF', ' ', MX, MN, -1, -1 ) )
         IF( NX.LT.M ) THEN
C
C           Determine the minimum value of NB.
C
            NBMIN = MAX( 2, ILAENV( 2, 'DGEQRF', ' ', MX, MN, -1, -1 ) )
         END IF
      ELSE
         NX    = 0
         NBMIN = 2
      END IF
C
      IF( NB.GE.NBMIN .AND. NB.LT.M .AND. NX.LT.M ) THEN
C
C        Use blocked code initially.
C
         IF( LTRANS .AND. LSIDE ) THEN
            IF( NB.LE.NBS ) THEN
               N1 = MIN( MAX( N1L, NB ), N )
            ELSE
               N1 = MIN( N1P, N )
            END IF
         ELSE
            IF( NB.LE.NBS ) THEN
               N2 = MIN( MAX( N2L, NB ), N )
            ELSE
               N2 = MIN( N2P, N )
            END IF
         END IF
C
         DO 50 I = 1, M - NX, NB
            IB = I + NB
            JB = MIN( M-I+1, NB )
C
C           Compute the current diagonal block and the needed off-
C           diagonal part of the current block row, if UPLO = 'U',
C           or block column, if UPLO = 'L'.
C
            IF( LTRANS ) THEN
               IF( LSIDE ) THEN
                  CALL MB01RX( SIDE, UPLO, TRANS, JB, N1, ALPHA, BETA,
     $                         R(I,I), LDR, A(1,I), LDA, B(1,I), LDB,
     $                         INFO )
                  DO 10 J = N1+1, N, N1
                     CALL MB01RX( SIDE, UPLO, TRANS, JB, MIN(N1, N-J+1),
     $                            ONE, BETA, R(I,I), LDR, A(J,I), LDA,
     $                            B(J,I), LDB, INFO )
   10             CONTINUE
                  IF( IB.LE.M ) THEN
                     IF( LUPLO ) THEN
                        CALL DGEMM( TRANS, 'No transpose', JB, M-IB+1,
     $                              N, BETA, A(1,I), LDA, B(1,IB), LDB,
     $                              ALPHA, R(I,IB), LDR )
                     ELSE
                        CALL DGEMM( TRANS, 'No transpose', M-IB+1, JB,
     $                              N, BETA, A(1,IB), LDA, B(1,I), LDB,
     $                              ALPHA, R(IB,I), LDR )
                     END IF
                  END IF
               ELSE
                  CALL MB01RX( SIDE, UPLO, TRANS, JB, N2, ALPHA, BETA,
     $                         R(I,I), LDR, A(I,1), LDA, B(I,1), LDB,
     $                         INFO )
                  DO 20 J = N2+1, N, N2
                     CALL MB01RX( SIDE, UPLO, TRANS, JB, MIN(N2, N-J+1),
     $                            ONE, BETA, R(I,I), LDR, A(I,J), LDA,
     $                            B(I,J), LDB, INFO )
   20             CONTINUE
                  IF( IB.LE.M ) THEN
                     IF( LUPLO ) THEN
                        CALL DGEMM( 'No transpose', TRANS, JB, M-IB+1,
     $                              N, BETA, B(I,1), LDB, A(IB,1), LDA,
     $                              ALPHA, R(I,IB), LDR )
                     ELSE
                        CALL DGEMM( 'No transpose', TRANS, M-IB+1, JB,
     $                              N, BETA, B(IB,1), LDB, A(I,1), LDA,
     $                              ALPHA, R(IB,I), LDR )
                     END IF
                  END IF
               END IF
            ELSE
               IF( LSIDE ) THEN
                  CALL MB01RX( SIDE, UPLO, TRANS, JB, N2, ALPHA, BETA,
     $                         R(I,I), LDR, A(I,1), LDA, B(1,I), LDB,
     $                         INFO )
                  DO 30 J = N2+1, N, N2
                     CALL MB01RX( SIDE, UPLO, TRANS, JB, MIN(N2, N-J+1),
     $                            ONE, BETA, R(I,I), LDR, A(I,J), LDA,
     $                            B(J,I), LDB, INFO )
   30             CONTINUE
                  IF( IB.LE.M ) THEN
                     IF( LUPLO ) THEN
                        CALL DGEMM( TRANS, 'No transpose', JB, M-IB+1,
     $                              N, BETA, A(I,1), LDA, B(1,IB), LDB,
     $                              ALPHA, R(I,IB), LDR )
                     ELSE
                        CALL DGEMM( TRANS, 'No transpose', M-IB+1, JB,
     $                              N, BETA, A(IB,1), LDA, B(1,I), LDB,
     $                              ALPHA, R(IB,I), LDR )
                     END IF
                  END IF
               ELSE
                  CALL MB01RX( SIDE, UPLO, TRANS, JB, N2, ALPHA, BETA,
     $                         R(I,I), LDR, A(1,I), LDA, B(I,1), LDB,
     $                         INFO )
                  DO 40 J = N2+1, N, N2
                     CALL MB01RX( SIDE, UPLO, TRANS, JB, MIN(N2, N-J+1),
     $                            ONE, BETA, R(I,I), LDR, A(J,I), LDA,
     $                            B(I,J), LDB, INFO )
   40             CONTINUE
                  IF( IB.LE.M ) THEN
                     IF( LUPLO ) THEN
                        CALL DGEMM( 'No transpose', TRANS, JB, M-IB+1,
     $                              N, BETA, B(I,1), LDB, A(1,IB), LDA,
     $                              ALPHA, R(I,IB), LDR )
                     ELSE
                        CALL DGEMM( 'No transpose', TRANS, M-IB+1, JB,
     $                              N, BETA, B(IB,1), LDB, A(1,I), LDA,
     $                              ALPHA, R(IB,I), LDR )
                     END IF
                  END IF
               END IF
            END IF
C
   50    CONTINUE
      ELSE
         I  = 1
         N1 = N
         N2 = N
      END IF
C
C     Use unblocked code to compute the last or only block.
C
      IF( I.LE.M ) THEN
         IF( LTRANS ) THEN
            IF( LSIDE ) THEN
               CALL MB01RX( SIDE, UPLO, TRANS, M-I+1, N1, ALPHA, BETA,
     $                      R(I,I), LDR, A(1,I), LDA, B(1,I), LDB,
     $                      INFO )
               DO 60 J = N1+1, N, N1
                  CALL MB01RX( SIDE, UPLO, TRANS, M-I+1, MIN(N1, N-J+1),
     $                         ONE, BETA, R(I,I), LDR, A(J,I), LDA,
     $                         B(J,I), LDB, INFO )
   60          CONTINUE
            ELSE
               CALL MB01RX( SIDE, UPLO, TRANS, M-I+1, N2, ALPHA, BETA,
     $                      R(I,I), LDR, A(I,1), LDA, B(I,1), LDB,
     $                      INFO )
               DO 70 J = N2+1, N, N2
                  CALL MB01RX( SIDE, UPLO, TRANS, M-I+1, MIN(N2, N-J+1),
     $                         ONE, BETA, R(I,I), LDR, A(I,J), LDA,
     $                         B(I,J), LDB, INFO )
   70          CONTINUE
            END IF
         ELSE
            IF( LSIDE ) THEN
               CALL MB01RX( SIDE, UPLO, TRANS, M-I+1, N2, ALPHA, BETA,
     $                      R(I,I), LDR, A(I,1), LDA, B(1,I), LDB,
     $                      INFO )
               DO 80 J = N2+1, N, N2
                  CALL MB01RX( SIDE, UPLO, TRANS, M-I+1, MIN(N2, N-J+1),
     $                         ONE, BETA, R(I,I), LDR, A(I,J), LDA,
     $                         B(J,I), LDB, INFO )
   80          CONTINUE
            ELSE
               CALL MB01RX( SIDE, UPLO, TRANS, M-I+1, N2, ALPHA, BETA,
     $                      R(I,I), LDR, A(1,I), LDA, B(I,1), LDB,
     $                      INFO )
               DO 90 J = N2+1, N, N2
                  CALL MB01RX( SIDE, UPLO, TRANS, M-I+1, MIN(N2, N-J+1),
     $                         ONE, BETA, R(I,I), LDR, A(J,I), LDA,
     $                         B(I,J), LDB, INFO )
   90          CONTINUE
            END IF
         END IF
      END IF
C
      RETURN
C *** Last line of MB01RB ***
      END
