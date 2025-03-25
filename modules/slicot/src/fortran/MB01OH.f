      SUBROUTINE MB01OH( UPLO, TRANS, N, ALPHA, BETA, R, LDR, H, LDH, A,
     $                   LDA )
C
C     PURPOSE
C
C     To compute one of the symmetric rank 2k operations
C
C        R := alpha*R + beta*H*A' + beta*A*H',
C
C     or
C
C        R := alpha*R + beta*H'*A + beta*A'*H,
C
C     where alpha and beta are scalars, R, A, and H are N-by-N matrices,
C     with A and H upper Hessenberg.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     UPLO    CHARACTER*1
C             Specifies which triangle of the symmetric matrix R is
C             given as follows:
C             = 'U':  the upper triangular part is given;
C             = 'L':  the lower triangular part is given.
C
C     TRANS   CHARACTER*1
C             Specifies the form of H to be used in the matrix
C             multiplication as follows:
C             = 'N':  R := alpha*R + beta*H*A' + beta*A*H';
C             = 'T':  R := alpha*R + beta*H'*A + beta*A'*H;
C             = 'C':  R := alpha*R + beta*H'*A + beta*A'*H.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices R, A, and H.  N >= 0.
C
C     ALPHA   (input) DOUBLE PRECISION
C             The scalar alpha. When alpha is zero then R need not be
C             set before entry.
C
C     BETA    (input) DOUBLE PRECISION
C             The scalar beta. When beta is zero then A and H are not
C             referenced.
C
C     R       (input/output) DOUBLE PRECISION array, dimension (LDR,N)
C             On entry with UPLO = 'U', the leading N-by-N upper
C             triangular part of this array must contain the upper
C             triangular part of the symmetric matrix R.
C             On entry with UPLO = 'L', the leading N-by-N lower
C             triangular part of this array must contain the lower
C             triangular part of the symmetric matrix R.
C             In both cases, the other strictly triangular part is not
C             referenced.
C             On exit, the leading N-by-N upper triangular part (if
C             UPLO = 'U'), or lower triangular part (if UPLO = 'L'), of
C             this array contains the corresponding triangular part of
C             the computed matrix R.
C
C     LDR     INTEGER
C             The leading dimension of array R.  LDR >= MAX(1,N).
C
C     H       (input) DOUBLE PRECISION array, dimension (LDH,N)
C             On entry, the leading N-by-N upper Hessenberg part of this
C             array must contain the upper Hessenberg matrix H.
C             The remaining part of this array is not referenced.
C
C     LDH     INTEGER
C             The leading dimension of array H.  LDH >= MAX(1,N)
C
C     A       (input) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N upper Hessenberg part of this
C             array must contain the upper Hessenberg matrix A.
C             The remaining part of this array is not referenced.
C
C     LDA     INTEGER
C             The leading dimension of array A.  LDA >= MAX(1,N)
C
C     METHOD
C
C     A particularization of the algorithm used in the BLAS 3 routine
C     DSYR2K is used.
C
C     NUMERICAL ASPECTS
C
C     The algorithm requires approximately N**3/3 operations.
C
C     CONTRIBUTORS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Mar. 2019.
C
C     REVISIONS
C
C     V. Sima, Apr. 2019.
C
C     KEYWORDS
C
C     Elementary matrix operations, matrix algebra, matrix operations.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE, TWO
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION  ALPHA, BETA
      INTEGER           LDA, LDH, LDR, N
      CHARACTER         TRANS, UPLO
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), H(LDH,*), R(LDR,*)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION  BETA2, TEMP
      INTEGER           I, INFO, J, J1
      LOGICAL           LTRANS, UPPER
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION  TMP(1)
C     ..
C     .. External Functions ..
      DOUBLE PRECISION  DDOT
      LOGICAL           LSAME
      EXTERNAL          DDOT, LSAME
C     ..
C     .. External Subroutines ..
      EXTERNAL          DAXPY, DCOPY, DLASCL, DLASET, DSCAL, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC         MAX
C     ..
C     .. Executable Statements ..
C
C     Test the input parameters.
C
      UPPER  = LSAME( UPLO, 'U' )
      LTRANS = LSAME( TRANS, 'T' ) .OR. LSAME( TRANS, 'C' )
C
      INFO = 0
      IF ( .NOT.UPPER .AND. .NOT. LSAME( UPLO, 'L' ) ) THEN
         INFO = 1
      ELSE IF ( .NOT.LSAME( TRANS, 'N' ) .AND. .NOT.LTRANS ) THEN
         INFO = 2
      ELSE IF ( N.LT.0 ) THEN
         INFO = 3
      ELSE IF ( LDR.LT.MAX( 1,N ) ) THEN
         INFO = 7
      ELSE IF ( LDH.LT.MAX( 1,N ) ) THEN
         INFO = 9
      ELSE IF ( LDA.LT.MAX( 1, N ) ) THEN
         INFO = 11
      END IF
      IF ( INFO.NE.0) THEN
         CALL XERBLA( 'MB01OH', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ( N.EQ.0 .OR. ( BETA.EQ.ZERO .AND. ALPHA.EQ.ONE ) )
     $   RETURN
C
      IF ( BETA.EQ.ZERO ) THEN
         IF ( ALPHA.EQ.ZERO ) THEN
C
C           Special case alpha = 0.
C
            CALL DLASET( UPLO, N, N, ZERO, ZERO, R, LDR )
         ELSE
C
C           Special case beta = 0.
C
            IF ( ALPHA.NE.ONE )
     $         CALL DLASCL( UPLO, 0, 0, ONE, ALPHA, N, N, R, LDR, INFO )
         END IF
         RETURN
      END IF
C
C     Start the operations.
C
      IF ( .NOT.LTRANS ) THEN
C
C        Form  R := alpha*R + beta*H*A' + beta*A*H'.
C
         IF ( UPPER ) THEN
C
            DO 20 J = 1, N
               IF ( ALPHA.EQ.ZERO ) THEN
                  TMP(1) = ZERO
                  CALL DCOPY( J, TMP, 0, R(1,J), 1 )
               ELSE IF ( ALPHA.NE.ONE ) THEN
                  CALL DSCAL( J, ALPHA, R(1,J), 1 )
               END IF
C
               DO 10 I = MAX( 1, J-1 ), N
                  CALL DAXPY( J, BETA*A(J,I), H(1,I), 1, R(1,J), 1 )
                  CALL DAXPY( J, BETA*H(J,I), A(1,I), 1, R(1,J), 1 )
   10          CONTINUE
C
   20       CONTINUE
C
         ELSE
C
            DO 40 J = 1, N
               IF ( ALPHA.EQ.ZERO ) THEN
                  TMP(1) = ZERO
                  CALL DCOPY( N-J+1, TMP, 0, R(J,J), 1 )
               ELSE IF ( ALPHA.NE.ONE ) THEN
                  CALL DSCAL( N-J+1, ALPHA, R(J,J), 1 )
               END IF
C
               DO 30 I = MAX( 1, J-1 ), N - 1
                  CALL DAXPY( I-J+2, BETA*A(J,I), H(J,I), 1, R(J,J), 1 )
                  CALL DAXPY( I-J+2, BETA*H(J,I), A(J,I), 1, R(J,J), 1 )
   30          CONTINUE
C
               CALL DAXPY( N-J+1, BETA*A(J,N), H(J,N), 1, R(J,J), 1 )
               CALL DAXPY( N-J+1, BETA*H(J,N), A(J,N), 1, R(J,J), 1 )
   40       CONTINUE
C
          END IF
C
      ELSE
C
C        Form  R := alpha*R + beta*H'*A + beta*A'*H.
C
         BETA2 = TWO*BETA
C
         IF ( UPPER ) THEN
C
            DO 60 J = 1, N - 1
C
               DO 50 I = 1, J
                  TEMP = BETA*( DDOT( I+1, H(1,I), 1, A(1,J), 1 ) +
     $                          DDOT( I+1, A(1,I), 1, H(1,J), 1 ) )
                  IF ( ALPHA.EQ.ZERO ) THEN
                     R(I,J) = TEMP
                  ELSE
                     R(I,J) = ALPHA*R(I,J) + TEMP
                  END IF
   50          CONTINUE
C
   60       CONTINUE
C
            DO 70 I = 1, N - 1
               TEMP = BETA*( DDOT( I+1, H(1,I), 1, A(1,N), 1 ) +
     $                       DDOT( I+1, A(1,I), 1, H(1,N), 1 ) )
               IF ( ALPHA.EQ.ZERO ) THEN
                  R(I,N) = TEMP
               ELSE
                  R(I,N) = ALPHA*R(I,N) + TEMP
               END IF
   70       CONTINUE
C
            TEMP = BETA2*DDOT( N, H(1,N), 1, A(1,N), 1 )
            IF ( ALPHA.EQ.ZERO ) THEN
               R(N,N) = TEMP
            ELSE
               R(N,N) = ALPHA*R(N,N) + TEMP
            END IF
C
         ELSE
C
            DO 90 J = 1, N - 1
               J1 = J + 1
C
               DO 80 I = J, N
                  TEMP = BETA*( DDOT( J1, H(1,I), 1, A(1,J), 1 ) +
     $                          DDOT( J1, A(1,I), 1, H(1,J), 1 ) )
                  IF ( ALPHA.EQ.ZERO ) THEN
                     R(I,J) = TEMP
                  ELSE
                     R(I,J) = ALPHA*R(I,J) + TEMP
                  END IF
   80          CONTINUE
C
   90       CONTINUE
C
            TEMP = BETA2*DDOT( N, H(1,N), 1, A(1,N), 1 )
            IF ( ALPHA.EQ.ZERO ) THEN
               R(N,N) = TEMP
            ELSE
               R(N,N) = ALPHA*R(N,N) + TEMP
            END IF
C
         END IF
C
      END IF
C
      RETURN
C *** Last line of MB01OH ***
      END
