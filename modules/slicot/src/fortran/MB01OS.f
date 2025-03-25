      SUBROUTINE MB01OS( UPLO, TRANS, N, H, LDH, X, LDX, P, LDP, INFO )
C
C     PURPOSE
C
C     To compute P = H*X or P = X*H, where H is an upper Hessenberg
C     matrix and X is a symmetric matrix.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     UPLO    CHARACTER*1
C             Specifies which triangle of the symmetric matrix X is
C             given as follows:
C             = 'U':  the upper triangular part is given;
C             = 'L':  the lower triangular part is given.
C
C     TRANS   CHARACTER*1
C             Specifies the operation to be performed as follows:
C             = 'N':         compute P = H*X;
C             = 'T' or 'C':  compute P = X*H.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices H, X, and P.  N >= 0.
C
C     H       (input) DOUBLE PRECISION array, dimension (LDH,N)
C             On entry, the leading N-by-N upper Hessenberg part of this
C             array must contain the upper Hessenberg matrix H.
C             The remaining part of this array is not referenced.
C
C     LDH     INTEGER
C             The leading dimension of the array H.  LDH >= MAX(1,N).
C
C     X       (input) DOUBLE PRECISION array, dimension (LDX,N)
C             On entry, if UPLO = 'U', the leading N-by-N upper
C             triangular part of this array must contain the upper
C             triangular part of the symmetric matrix X and the strictly
C             lower triangular part of the array is not referenced.
C             On entry, if UPLO = 'L', the leading N-by-N lower
C             triangular part of this array must contain the lower
C             triangular part of the symmetric matrix X and the strictly
C             upper triangular part of the array is not referenced.
C
C     LDX     INTEGER
C             The leading dimension of the array X.  LDX >= MAX(1,N).
C
C     P       (output) DOUBLE PRECISION array, dimension (LDP,N)
C             On exit, the leading N-by-N part of this array contains
C             the computed matrix P.
C
C     LDP     INTEGER
C             The leading dimension of the array P.  LDP >= MAX(1,N).
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -k, the k-th argument had an illegal
C                   value.
C
C     METHOD
C
C     The matrix expression is efficiently evaluated taking the
C     structure into account, and using inline code and BLAS routines.
C     Let X = U + sL, where U is upper triangular and sL is strictly
C     lower triangular. Then, P = H*X = H*U + H*sL = H*U + H*sU', where
C     sU is the strictly upper triangular part of X.
C     Similarly, P = X*H = L'*H + sL*H, where L is lower triangular, and
C     X = L + sL'. Note that H*U and L'*H are both upper Hessenberg.
C     However, when UPLO = 'L' and TRANS = 'N', or when UPLO = 'U' and
C     TRANS = 'T', then the matrix P is full. The computations are done
C     similarly.
C
C     NUMERICAL ASPECTS
C
C     The algorithm requires approximately N**3/2 operations.
C
C     CONTRIBUTORS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Apr. 2019.
C
C     REVISIONS
C
C     -
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
C     .. Scalar Arguments ..
      CHARACTER         TRANS, UPLO
      INTEGER           INFO, LDH, LDP, LDX, N
C     .. Array Arguments ..
      DOUBLE PRECISION  H(LDH,*), P(LDP,*), X(LDX,*)
C     .. Local Scalars ..
      DOUBLE PRECISION  TEMP
      INTEGER           I, J, J3
      LOGICAL           LTRANS, LUPLO
C     .. Local Arrays ..
      DOUBLE PRECISION  TMP(1)
C     .. External Functions ..
      DOUBLE PRECISION  DDOT
      LOGICAL           LSAME
      EXTERNAL          DDOT, LSAME
C     .. External Subroutines ..
      EXTERNAL          DAXPY, DCOPY, DGEMV, DTRMM, DTRMV, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         MAX, MIN
C     .. Executable Statements ..
C
C     Test the input scalar arguments.
C
      INFO   = 0
      LUPLO  = LSAME( UPLO, 'U' )
      LTRANS = LSAME( TRANS, 'T' ) .OR. LSAME( TRANS, 'C' )
C
      IF ( ( .NOT.LUPLO ).AND.( .NOT.LSAME( UPLO, 'L' ) ) ) THEN
         INFO = -1
      ELSE IF ( ( .NOT.LTRANS ).AND.( .NOT.LSAME( TRANS, 'N' ) ) ) THEN
         INFO = -2
      ELSE IF ( N.LT.0 ) THEN
         INFO = -3
      ELSE IF ( LDH.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF ( LDX.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF ( LDP.LT.MAX( 1, N ) ) THEN
         INFO = -9
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'MB01OS', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ( N.EQ.0 )
     $   RETURN
C
      IF ( .NOT.LTRANS ) THEN
C
         IF ( LUPLO ) THEN
            TMP(1) = ZERO
C
C           Compute  P := H*U + H*sU'.
C
            DO 30 J = 1, N - 1
               CALL DCOPY( J, X(1,J), 1, P(1,J), 1 )
               CALL DTRMV( UPLO, 'NoTran', 'NoDiag', J, H, LDH, P(1,J),
     $                     1 )
               CALL DCOPY( N-J, TMP, 0, P(J+1,J), 1 )
C
               DO 10 I = 2, J + 1
                  P(I,J) = P(I,J) + H(I,I-1)*X(I-1,J)
   10          CONTINUE
C
               DO 20 I = J + 2, N
                  CALL DAXPY( I, X(J,I-1), H(1,I-1), 1, P(1,J), 1 )
   20          CONTINUE
C
               CALL DAXPY( N, X(J,N), H(1,N), 1, P(1,J), 1 )
   30       CONTINUE
C
            CALL DCOPY( N, X(1,N), 1, P(1,N), 1 )
            CALL DTRMV( UPLO, 'NoTran', 'NoDiag', N, H, LDH, P(1,N), 1 )
C
            DO 40 I = 2, N
               P(I,N) = P(I,N) + H(I,I-1)*X(I-1,N)
   40       CONTINUE
C
         ELSE
C
C           Compute  P := H*L + H*sL'.
C           There is no contribution from sL' for the first column.
C
            CALL DCOPY( N, X, 1, P, 1 )
            CALL DTRMV( 'Upper', 'NoTran', 'NoDiag', N, H, LDH, P, 1 )
C
            DO 50 I = 2, N
               P(I,1) = P(I,1 ) + H(I,I-1)*X(I-1,1)
   50       CONTINUE
C
            DO 80 J = 2, N
C
C              Compute the contribution from H*sL'.
C
               CALL DCOPY( J-1, X(J,1), LDX, P(1,J), 1 )
               CALL DTRMV( 'Upper', 'NoTran', 'NoDiag', J-1, H, LDH,
     $                     P(1,J), 1 )
               P(J,J) = ZERO
C
               DO 60 I = 2, J
                  P(I,J) = P(I,J ) + H(I,I-1)*X(J,I-1)
   60          CONTINUE
C
C              Compute the contribution from H*L.
C
               TEMP = P(J,J)
               CALL DGEMV( 'NoTran', J-1, N-J+1, ONE, H(1,J), LDH,
     $                     X(J,J), 1, ONE, P(1,J), 1 )
               CALL DCOPY( N-J+1, X(J,J), 1, P(J,J), 1 )
               CALL DTRMV( 'Upper', 'NoTran', 'NoDiag', N-J+1, H(J,J),
     $                     LDH, P(J,J), 1 )
               P(J,J) = P(J,J) + TEMP
C
               DO 70 I = J+1, N
                  P(I,J) = P(I,J ) + H(I,I-1)*X(I-1,J)
   70          CONTINUE
C
   80       CONTINUE
C
         END IF
C
      ELSE
C
         IF ( LUPLO ) THEN
C
C           Compute  P := U*H + sU'*H.
C
            DO 90 J = 1, N - 2
               J3 = MIN( J+3, N )
               CALL DCOPY( J+1, H(1,J), 1, P(1,J), 1 )
               CALL DTRMV( UPLO, 'NoTran', 'NoDiag', J+1, X, LDX,
     $                     P(1,J), 1 )
               CALL DCOPY( J+1, H(1,J), 1, P(2,J+1), 1 )
               CALL DTRMV( UPLO, 'Tran', 'NoDiag', J+1, X(1,2), LDX,
     $                     P(2,J+1), 1 )
               CALL DAXPY( J, ONE, P(2,J+1), 1, P(2,J), 1 )
               P(J+2,J) = DDOT( J+1, X(1,J+2), 1, H(1,J), 1 )
               CALL DGEMV( 'Tran', J+1, N-J3, ONE, X(1,J3), LDX,
     $                     H(1,J), 1, ONE, P(J3,J), 1 )
               P(N,J) = DDOT( J+1, X(1,N), 1, H(1,J), 1 )
   90       CONTINUE
C
            IF ( N.EQ.1 ) THEN
               P(1,1) = X(1,1)*H(1,1)
            ELSE
               CALL DCOPY( N, H(1,N-1), 1, P(1,N-1), 1 )
               CALL DCOPY( N, H(1,N), 1, P(1,N), 1 )
               CALL DTRMM( 'Left', UPLO, 'NoTran', 'NoDiag', N, 2, ONE,
     $                     X, LDX, P(1,N-1), LDP )
C
               DO 100 I = 2, N
                  CALL DGEMV( 'Tran', I-1, 2, ONE, H(1,N-1), LDH,
     $                        X(1,I), 1, ONE, P(I,N-1), LDP )
  100          CONTINUE
C
            END IF
C
         ELSE
C
C           Compute  P := L*H + sL'*H.
C
            DO 110 J = 1, N - 1
               CALL DCOPY( J, H(1,J), 1, P(1,J), 1 )
               CALL DTRMV( UPLO, 'NoTran', 'NoDiag', J, X, LDX, P(1,J),
     $                     1 )
               CALL DCOPY( J, H(2,J), 1, P(1,J+1), 1 )
               CALL DTRMV( UPLO, 'Tran', 'NoDiag', J, X(2,1), LDX,
     $                     P(1,J+1), 1 )
               CALL DAXPY( J, ONE, P(1,J+1), 1, P(1,J), 1 )
               CALL DGEMV( 'NoTran', N-J, J+1, ONE, X(J+1,1), LDX,
     $                     H(1,J), 1, ZERO, P(J+1,J), 1 )
  110       CONTINUE
C
            CALL DCOPY( N, H(1,N), 1, P(1,N), 1 )
            CALL DTRMV( UPLO, 'NoTran', 'NoDiag', N, X, LDX, P(1,N), 1 )
C
            DO 120 I = 1, N - 1
               P(I,N) = P(I,N) + DDOT( N-I, X(I+1,I), 1, H(I+1,N), 1 )
  120       CONTINUE
C
         END IF
C
      END IF
C
      RETURN
C *** Last line of MB01OS ***
      END
