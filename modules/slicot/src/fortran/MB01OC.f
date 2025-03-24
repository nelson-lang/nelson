      SUBROUTINE MB01OC( UPLO, TRANS, N, ALPHA, BETA, R, LDR, H, LDH, X,
     $                   LDX, INFO )
C
C     PURPOSE
C
C     To perform one of the special symmetric rank 2k operations
C
C        R := alpha*R + beta*H*X + beta*X*H',
C     or
C        R := alpha*R + beta*H'*X + beta*X*H,
C
C     where alpha and beta are scalars, R and X are N-by-N symmetric
C     matrices, and H is an N-by-N upper Hessenberg matrix.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     UPLO    CHARACTER*1
C             Specifies which triangles of the symmetric matrices R
C             and X are given as follows:
C             = 'U':  the upper triangular part is given;
C             = 'L':  the lower triangular part is given.
C
C     TRANS   CHARACTER*1
C             Specifies the operation to be performed as follows:
C             = 'N':         R := alpha*R + beta*H*X  + beta*X*H';
C             = 'T' or 'C':  R := alpha*R + beta*H'*X + beta*X*H.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices R, H, and X.  N >= 0.
C
C     ALPHA   (input) DOUBLE PRECISION
C             The scalar alpha. When alpha is zero then R need not be
C             set before entry, except when R is identified with X in
C             the call.
C
C     BETA    (input) DOUBLE PRECISION
C             The scalar beta. When beta is zero then H and X are not
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
C             The leading dimension of the array R.  LDR >= MAX(1,N).
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
C     structure into account, and using inline code and BLAS1 routines.
C
C     NUMERICAL ASPECTS
C
C     The algorithm requires approximately N**3/2 operations.
C
C     FURTHER COMMENTS
C
C     This routine acts as a specialization of BLAS Library routine
C     DSYR2K.
C
C     CONTRIBUTORS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Feb. 2019.
C
C     REVISIONS
C
C     V. Sima, Mar. 2019.
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
      INTEGER           INFO, LDH, LDR, LDX, N
      DOUBLE PRECISION  ALPHA, BETA
C     .. Array Arguments ..
      DOUBLE PRECISION  H(LDH,*), R(LDR,*), X(LDX,*)
C     .. Local Scalars ..
      DOUBLE PRECISION  TEMP1, TEMP2
      INTEGER           I, J, L
      LOGICAL           LTRANS, LUPLO
C     .. Local Arrays ..
      DOUBLE PRECISION  TMP(1)
C     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
C     .. External Subroutines ..
      EXTERNAL          DAXPY, DCOPY, DLASCL, DLASET, DSCAL, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         MAX, MIN
C     .. Executable Statements ..
C
C     Test the input scalar arguments.
C
      INFO   = 0
      LUPLO  = LSAME( UPLO,  'U' )
      LTRANS = LSAME( TRANS, 'T' ) .OR. LSAME( TRANS, 'C' )
C
      IF (      ( .NOT.LUPLO  ).AND.( .NOT.LSAME( UPLO,  'L' ) ) ) THEN
         INFO = -1
      ELSE IF ( ( .NOT.LTRANS ).AND.( .NOT.LSAME( TRANS, 'N' ) ) ) THEN
         INFO = -2
      ELSE IF ( N.LT.0 ) THEN
         INFO = -3
      ELSE IF ( LDR.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF ( LDH.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF ( LDX.LT.MAX( 1, N ) ) THEN
         INFO = -11
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'MB01OC', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ( N.EQ.0 )
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
C     General case: beta <> 0.
C     Compute R = alpha*R + beta*( op( H )*X + X*op( H )' ), exploiting
C     the structure, where op( H ) is H, if UPLO = 'U', and H', if
C     UPLO = 'L'.
C
      IF ( .NOT.LTRANS ) THEN
C
C        Form  R := alpha*R + beta*( H*X + X*H' ).
C
         IF ( LUPLO ) THEN
            DO 20 J = 1, N
               IF ( ALPHA.EQ.ZERO ) THEN
                  TMP(1) = ZERO
                  CALL DCOPY( J, TMP, 0, R(1,J), 1 )
               ELSE IF ( ALPHA.NE.ONE ) THEN
                  CALL DSCAL( J, ALPHA, R(1,J), 1 )
               END IF
               I = MAX( 1, J-1 )
               DO 10 L = 1, N
                  IF ( L.LE.J ) THEN
                     TEMP1 = X(L,J)
                  ELSE
                     TEMP1 = X(J,L)
                  END IF
                  IF ( TEMP1.NE.ZERO )
     $               CALL DAXPY( MIN( L+1, J ), BETA*TEMP1, H(1,L), 1,
     $                           R(1,J), 1 )
                  IF ( L.GE.I ) THEN
                     TEMP2 = H(J,L)
                     IF ( TEMP2.NE.ZERO ) THEN
                        TEMP2 = BETA*TEMP2
                        CALL DAXPY( I, TEMP2, X(1,L), 1, R(1,J), 1 )
                        IF ( J.GT.1 )
     $                     R(J,J) = R(J,J) + TEMP1*TEMP2
                     END IF
                  END IF
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 40 J = 1, N
               IF ( ALPHA.EQ.ZERO ) THEN
                  TMP(1) = ZERO
                  CALL DCOPY( N-J+1, TMP, 0, R(J,J), 1 )
               ELSE IF ( ALPHA.NE.ONE ) THEN
                  CALL DSCAL( N-J+1, ALPHA, R(J,J), 1 )
               END IF
               DO 30 L = MAX( 1, J-1 ), N
                  I = MIN( L+1, N )
                  TEMP2 = BETA*H(J,L)
                  IF ( L.GE.J ) THEN
                     TEMP1 = BETA*X(L,J)
                  ELSE
                     TEMP1 = BETA*X(J,L)
                  END IF
                  CALL DAXPY( I-J+1, TEMP1, H(J,L), 1,   R(J,J), 1 )
                  CALL DAXPY( N-L+1, TEMP2, X(L,L), 1,   R(L,J), 1 )
                  IF ( L.GT.J )
     $              CALL DAXPY( L-J, TEMP2, X(L,J), LDX, R(J,J), 1 )
   30          CONTINUE
   40       CONTINUE
         END IF
      ELSE
C
C        Form  R := alpha*R + beta*( H'*X + X*H ).
C
         IF ( LUPLO ) THEN
            DO 70 J = 1, N
               IF ( ALPHA.EQ.ZERO ) THEN
                  TMP(1) = ZERO
                  CALL DCOPY( J, TMP, 0, R(1,J), 1 )
               ELSE IF ( ALPHA.NE.ONE ) THEN
                  CALL DSCAL( J, ALPHA, R(1,J), 1 )
               END IF
               DO 60 I = 1, J
                  DO 50 L = 1, MIN( J+1, N ) 
                     IF ( L.LE.J ) THEN
                        TEMP1 = X(L,J)
                        IF ( L.LE.I ) THEN
                           TEMP2 = X(L,I)
                        ELSE
                           TEMP2 = X(I,L)
                        END IF
                     ELSE
                        TEMP1 = X(J,L)
                        TEMP2 = X(I,L)
                     END IF
                     IF ( L.LE.MIN( I+1, N ) )
     $                  R(I,J) = R(I,J) + BETA*TEMP1*H(L,I)
                     R(I,J)    = R(I,J) + BETA*TEMP2*H(L,J)
   50             CONTINUE
   60          CONTINUE
   70       CONTINUE
         ELSE
            DO 100 J = 1, N
               IF ( ALPHA.EQ.ZERO ) THEN
                  TMP(1) = ZERO
                  CALL DCOPY( N-J+1, TMP, 0, R(J,J), 1 )
               ELSE IF ( ALPHA.NE.ONE ) THEN
                  CALL DSCAL( N-J+1, ALPHA, R(J,J), 1 )
               END IF
               DO 90 I = J, N
                  DO 80 L = 1, MIN( I+1, N )
                     IF ( L.GE.I ) THEN
                        TEMP1 = X(L,J)
                        TEMP2 = X(L,I)
                     ELSE
                        IF ( L.GE.J ) THEN
                           TEMP1 = X(L,J)
                        ELSE
                           TEMP1 = X(J,L)
                        END IF
                        TEMP2 = X(I,L)
                     END IF
                     R(I,J) = R(I,J) + BETA*TEMP1*H(L,I)
                     IF ( L.LE.MIN( J+1, N ) )
     $                  R(I,J) = R(I,J) + BETA*TEMP2*H(L,J)
   80             CONTINUE
   90          CONTINUE
  100       CONTINUE
         END IF
      END IF
C
      RETURN
C *** Last line of MB01OC ***
      END
