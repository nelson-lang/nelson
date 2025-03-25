      SUBROUTINE MB01OT( UPLO, TRANS, N, ALPHA, BETA, R, LDR, E, LDE, T,
     $                   LDT )
C
C     PURPOSE
C
C     To compute one of the symmetric rank 2k operations
C
C        R := alpha*R + beta*E*T' + beta*T*E',
C
C     or
C
C        R := alpha*R + beta*E'*T + beta*T'*E,
C
C     where alpha and beta are scalars, R, T, and E are N-by-N matrices,
C     with T and E upper triangular.
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
C             Specifies the form of E to be used in the matrix
C             multiplication as follows:
C             = 'N':  R := alpha*R + beta*E*T' + beta*T*E';
C             = 'T':  R := alpha*R + beta*E'*T + beta*T'*E;
C             = 'C':  R := alpha*R + beta*E'*T + beta*T'*E.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices R, T, and E.  N >= 0.
C
C     ALPHA   (input) DOUBLE PRECISION
C             The scalar alpha. When alpha is zero then R need not be
C             set before entry.
C
C     BETA    (input) DOUBLE PRECISION
C             The scalar beta. When beta is zero then T and E are not
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
C     E       (input) DOUBLE PRECISION array, dimension (LDE,N)
C             On entry, the leading N-by-N upper triangular part of this
C             array must contain the upper triangular matrix E.
C             The remaining part of this array is not referenced.
C
C     LDE     INTEGER
C             The leading dimension of array E.  LDE >= MAX(1,N).
C
C     T       (input) DOUBLE PRECISION array, dimension (LDT,N)
C             On entry, the leading N-by-N upper triangular part of this
C             array must contain the upper triangular matrix T.
C             The remaining part of this array is not referenced.
C
C     LDT     INTEGER
C             The leading dimension of array T.  LDT >= MAX(1,N).
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
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION  ALPHA, BETA
      INTEGER           LDE, LDR, LDT, N
      CHARACTER         TRANS, UPLO
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION  E(LDE,*), R(LDR,*), T(LDT,*)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION  TEMP
      INTEGER           I, INFO, J
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
      ELSE IF ( LDT.LT.MAX( 1,N ) ) THEN
         INFO = 9
      ELSE IF ( LDE.LT.MAX( 1, N ) ) THEN
         INFO = 11
      END IF
      IF ( INFO.NE.0) THEN
         CALL XERBLA( 'MB01OT', -INFO )
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
C        Form  R := alpha*R + beta*E*T' + beta*T*E'.
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
               DO 10 I = J, N
                  CALL DAXPY( J, BETA*T(J,I), E(1,I), 1, R(1,J), 1 )
                  CALL DAXPY( J, BETA*E(J,I), T(1,I), 1, R(1,J), 1 )
   10          CONTINUE
C
   20       CONTINUE
C
         ELSE
C
            DO 40 J = 1, N
               IF ( ALPHA.EQ.ZERO ) THEN
                  TMP(1) = ZERO
                  CALL DCOPY( J, TMP, 0, R(J,1), LDR )
               ELSE IF ( ALPHA.NE.ONE ) THEN
                  CALL DSCAL( J, ALPHA, R(J,1), LDR )
               END IF
C
               DO 30 I = 1, J
                  CALL DAXPY( I, BETA*T(I,J), E(1,J), 1, R(I,1), LDR )
                  CALL DAXPY( I, BETA*E(I,J), T(1,J), 1, R(I,1), LDR )
   30          CONTINUE
C
   40       CONTINUE
C
          END IF
C
      ELSE
C
C        Form  R := alpha*R + beta*E'*T + beta*T'*E.
C
         IF ( UPPER ) THEN
C
            DO 60 J = 1, N
C
               DO 50 I = 1, J
                  TEMP = BETA*( DDOT( I, E(1,I), 1, T(1,J), 1 ) +
     $                          DDOT( I, T(1,I), 1, E(1,J), 1 ) )
                  IF ( ALPHA.EQ.ZERO ) THEN
                     R(I,J) = TEMP
                  ELSE
                     R(I,J) = ALPHA*R(I,J) + TEMP
                  END IF
   50          CONTINUE
C
   60       CONTINUE
C
         ELSE
C
            DO 80 J = 1, N
C
               DO 70 I = J, N
                  TEMP = BETA*( DDOT( J, E(1,I), 1, T(1,J), 1 ) +
     $                          DDOT( J, T(1,I), 1, E(1,J), 1 ) )
                  IF ( ALPHA.EQ.ZERO ) THEN
                     R(I,J) = TEMP
                  ELSE
                     R(I,J) = ALPHA*R(I,J) + TEMP
                  END IF
   70          CONTINUE
C
   80       CONTINUE
C
         END IF
C
      END IF
C
      RETURN
C *** Last line of MB01OT ***
      END
