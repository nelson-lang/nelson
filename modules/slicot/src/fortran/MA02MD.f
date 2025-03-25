      DOUBLE PRECISION FUNCTION MA02MD( NORM, UPLO, N, A, LDA, DWORK )
C
C     PURPOSE
C
C     To compute the value of the one norm, or the Frobenius norm, or
C     the infinity norm, or the element of largest absolute value
C     of a real skew-symmetric matrix.
C
C     Note that for this kind of matrices the infinity norm is equal
C     to the one norm.
C
C     FUNCTION VALUE
C
C     MA02MD  DOUBLE PRECISION
C             The computed norm.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     NORM    CHARACTER*1
C             Specifies the value to be returned in MA02MD:
C             = '1' or 'O':  one norm of A;
C             = 'F' or 'E':  Frobenius norm of A;
C             = 'I':         infinity norm of A;
C             = 'M':         max(abs(A(i,j)).
C
C     UPLO    CHARACTER*1
C             Specifies whether the upper or lower triangular part of
C             the skew-symmetric matrix A is to be referenced.
C             = 'U':  Upper triangular part of A is referenced;
C             = 'L':  Lower triangular part of A is referenced.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrix A.  N >= 0.  When N = 0, MA02MD is
C             set to zero.
C
C     A       (input) DOUBLE PRECISION array, dimension (LDA,N)
C             The skew-symmetric matrix A.  If UPLO = 'U', the leading
C             N-by-N strictly upper triangular part of A contains the
C             strictly upper triangular part of the matrix A, and the
C             lower triangular part of A is not referenced.
C             If UPLO = 'L', the leading N-by-N strictly lower
C             triangular part of A contains the strictly lower
C             triangular part of the matrix A, and the upper triangular
C             part of A is not referenced.
C             The diagonal of A need not be set to zero.
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
C     Elementary matrix operations, skew-symmetric matrix.
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
      DOUBLE PRECISION   A( LDA, * ), DWORK( * )
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
      EXTERNAL           DLASSQ
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
C     ..
C     .. Executable Statements ..
C
      IF( N.LE.1 ) THEN
         VALUE = ZERO
      ELSE IF( LSAME( NORM, 'M' ) ) THEN
C
C        Find max(abs(A(i,j))).
C
         VALUE = ZERO
         IF( LSAME( UPLO, 'U' ) ) THEN
            DO 20 J = 2, N
               DO 10 I = 1, J-1
                  VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 40 J = 1, N-1
               DO 30 I = J+1, N
                  VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   30          CONTINUE
   40       CONTINUE
         END IF
C
      ELSE IF( ( LSAME( NORM, 'I' ) ) .OR. ( LSAME( NORM, 'O' ) ) .OR.
     $         ( NORM.EQ.'1' ) ) THEN
C
C        Find normI(A) ( = norm1(A), since A is skew-symmetric).
C
         VALUE = ZERO
         IF( LSAME( UPLO, 'U' ) ) THEN
            DWORK( 1 ) = ZERO
            DO 60 J = 2, N
               SUM = ZERO
               DO 50 I = 1, J-1
                  ABSA = ABS( A( I, J ) )
                  SUM  = SUM + ABSA
                  DWORK( I ) = DWORK( I ) + ABSA
   50          CONTINUE
               DWORK( J ) = SUM
   60       CONTINUE
            DO 70 I = 1, N
               VALUE = MAX( VALUE, DWORK( I ) )
   70       CONTINUE
         ELSE
            DO 80 I = 1, N
               DWORK( I ) = ZERO
   80       CONTINUE
            DO 100 J = 1, N-1
               SUM = DWORK( J ) 
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
               CALL DLASSQ( J-1, A( 1, J ), 1, SCALE, SUM )
  110       CONTINUE
         ELSE
            DO 120 J = 1, N-1
               CALL DLASSQ( N-J, A( J+1, J ), 1, SCALE, SUM )
  120       CONTINUE
         END IF
         VALUE = SCALE*SQRT( TWO*SUM )
      END IF
C
      MA02MD = VALUE
      RETURN
C
C *** Last line of MA02MD ***
      END
