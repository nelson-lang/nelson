      DOUBLE PRECISION FUNCTION MA02SD( M, N, A, LDA )
C
C     Purpose
C
C     To compute the smallest nonzero absolute value of the elements of
C     a real matrix A.
C
C     FUNCTION VALUE
C
C     MA02SD  DOUBLE PRECISION
C             The smallest nonzero absolute value of the elements of the
C             matrix A.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     M       (input) INTEGER
C             The number of rows of the matrix A.  M >= 0.
C
C     N       (input) INTEGER
C             The number of columns of the matrix A.  N >= 0.
C
C     A       (input) DOUBLE PRECISION array, dimension (M,N)
C             On entry, this array must contain the matrix A.
C
C     LDA     DOUBLE PRECISION array, dimension (N)
C             The leading dimension of the array A.  LDA >= MAX(1,M).
C
C     METHOD
C
C     The smallest nonzero absolute value of the elements of the real
C     matrix A is found by direct comparison.
C
C     FURTHER COMMENTS
C
C     If M and/or N are/is zero, the returned result is 0.
C
C     CONTRIBUTORS
C
C     V. Sima, Oct. 2023.
C
C     REVISIONS
C
C     -
C
C     KEYWORDS
C
C     matrix operations.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
C
C     .. Scalar Arguments ..
      INTEGER            LDA, M, N
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
C     ..
C     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   AIJ, TMP
C     ..
C     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS
C     ..
C     .. Executable Statements ..
C
C     For efficiency, the input paramters are not tested.
C
C     Quick return if possible.
C
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
         MA02SD = ZERO
         RETURN
      END IF
C
      TMP = DLAMCH( 'Overflow' )
C
      DO 20 J = 1, N
C
         DO 10 I = 1, M
            AIJ = ABS( A( I, J ) )
            IF( AIJ.GT.ZERO ) THEN
               IF( AIJ.LT.TMP )
     $            TMP = AIJ
            END IF
   10    CONTINUE
C
   20 CONTINUE
C
      MA02SD = TMP
      RETURN
C
C *** Last line of MA02SD ***
      END
