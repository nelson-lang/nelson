      DOUBLE PRECISION FUNCTION MA02MZ( NORM, UPLO, N, A, LDA, DWORK )
C
C     PURPOSE
C
C     To compute the value of the one norm, or the Frobenius norm, or
C     the infinity norm, or the element of largest absolute value
C     of a complex skew-Hermitian matrix.
C
C     Note that for this kind of matrices the infinity norm is equal
C     to the one norm.
C
C     FUNCTION VALUE
C
C     MA02MZ  DOUBLE PRECISION
C             The computed norm.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     NORM    CHARACTER*1
C             Specifies the value to be returned in MA02MZ:
C             = '1' or 'O':  one norm of A;
C             = 'F' or 'E':  Frobenius norm of A;
C             = 'I':         infinity norm of A;
C             = 'M':         max(abs(A(i,j)).
C
C     UPLO    CHARACTER*1
C             Specifies whether the upper or lower triangular part of
C             the skew-Hermitian matrix A is to be referenced.
C             = 'U':  Upper triangular part of A is referenced;
C             = 'L':  Lower triangular part of A is referenced.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrix A.  N >= 0.  When N = 0, MA02MZ is
C             set to zero.
C
C     A       (input) COMPLEX*16 array, dimension (LDA,N)
C             The skew-Hermitian matrix A.  If UPLO = 'U', the leading
C             N-by-N upper triangular part of A contains the upper
C             triangular part of the matrix A, and the strictly lower
C             triangular part of A is not referenced. If UPLO = 'L', the
C             leading N-by-N lower triangular part of A contains the
C             lower triangular part of the matrix A, and the strictly
C             upper triangular part of A is not referenced.
C             The real parts of the diagonal elements of A need not be
C             set and are assumed to be zero.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= max(1,N).
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (MAX(1,LDWORK)),
C             where LDWORK >= N when NORM = 'I' or '1' or 'O';
C             otherwise, DWORK is not referenced.
C
C     CONTRIBUTORS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Oct. 2015.
C     Based on LAPACK reference routine DLANSY.
C
C     REVISIONS
C
C     V. Sima, Jan. 2016.
C
C     KEYWORDS
C
C     Elementary matrix operations, skew-Hermitian matrix.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ONE, TWO, ZERO
      PARAMETER          ( ONE = 1.0D+0, TWO = 2.0D+0, ZERO = 0.0D+0 )
C     ..
C     .. Scalar Arguments ..
      CHARACTER          NORM, UPLO
      INTEGER            LDA, N
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION   DWORK( * )
      COMPLEX*16         A( LDA, * )
C     ..
C     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   ABSA, SCALE, SUM, VALUE
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     ..
C     .. External Subroutines ..
      EXTERNAL           ZLASSQ
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, DIMAG, MAX, SQRT
C     ..
C     .. Executable Statements ..
C
      IF( N.EQ.0 ) THEN
         VALUE = ZERO
      ELSE IF( LSAME( NORM, 'M' ) ) THEN
C
C        Find max(abs(A(i,j))).
C
         VALUE = ZERO
         IF( LSAME( UPLO, 'U' ) ) THEN
            DO 20 J = 1, N
               DO 10 I = 1, J-1
                  VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   10          CONTINUE
               VALUE = MAX( VALUE, ABS( DIMAG( A( J, J ) ) ) )
   20       CONTINUE
         ELSE
            DO 40 J = 1, N
               VALUE = MAX( VALUE, ABS( DIMAG( A( J, J ) ) ) )
               DO 30 I = J+1, N
                  VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   30          CONTINUE
   40       CONTINUE
         END IF
C
      ELSE IF( ( LSAME( NORM, 'I' ) ) .OR. ( LSAME( NORM, 'O' ) ) .OR.
     $         ( NORM.EQ.'1' ) ) THEN
C
C        Find normI(A) ( = norm1(A), since A is skew-Hermitian).
C
         VALUE = ZERO
         IF( LSAME( UPLO, 'U' ) ) THEN
            DO 60 J = 1, N
               SUM = ZERO
               DO 50 I = 1, J-1
                  ABSA = ABS( A( I, J ) )
                  SUM  = SUM + ABSA
                  DWORK( I ) = DWORK( I ) + ABSA
   50          CONTINUE
               DWORK( J ) = SUM +  ABS( DIMAG( A( J, J ) ) )
   60       CONTINUE
            DO 70 I = 1, N
               VALUE = MAX( VALUE, DWORK( I ) )
   70       CONTINUE
         ELSE
            DO 80 I = 1, N
               DWORK( I ) = ZERO
   80       CONTINUE
            DO 100 J = 1, N
               SUM = DWORK( J ) + ABS( DIMAG( A( J, J ) ) ) 
               DO 90 I = J+1, N
                  ABSA = ABS( A( I, J ) )
                  SUM  = SUM + ABSA
                  DWORK( I ) = DWORK( I ) + ABSA
   90          CONTINUE
               VALUE = MAX( VALUE, SUM )
  100       CONTINUE
            VALUE = MAX( VALUE, DWORK( N ) )
         END IF
C
      ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN
C
C        Find normF(A).
C
         SCALE = ZERO
         SUM   = ONE
         IF( LSAME( UPLO, 'U' ) ) THEN
            DO 110 J = 2, N
               CALL ZLASSQ( J-1, A( 1, J ), 1, SCALE, SUM )
  110       CONTINUE
         ELSE
            DO 120 J = 1, N-1
               CALL ZLASSQ( N-J, A( J+1, J ), 1, SCALE, SUM )
  120       CONTINUE
         END IF
         SUM = TWO*SUM
         DO 130 I = 1, N
            IF( DIMAG( A( I, I ) ).NE.ZERO ) THEN
               ABSA = ABS( DIMAG( A( I, I ) ) )
               IF( SCALE.LT.ABSA ) THEN
                  SUM   = ONE + SUM*( SCALE / ABSA )**2
                  SCALE = ABSA
               ELSE
                  SUM = SUM + ( ABSA / SCALE )**2
               END IF
            END IF
  130    CONTINUE
         VALUE = SCALE*SQRT( SUM )
      END IF
C
      MA02MZ = VALUE
      RETURN
C
C *** Last line of MA02MZ ***
      END
