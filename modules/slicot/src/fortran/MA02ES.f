      SUBROUTINE MA02ES( UPLO, N, A, LDA )
C
C     PURPOSE
C
C     To store by skew-symmetry the upper or lower triangle of a
C     skew-symmetric matrix, given the other triangle. The diagonal
C     entries are set to zero.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     UPLO    CHARACTER*1
C             Specifies which part of the matrix is given as follows:
C             = 'U':  Upper triangular part;
C             = 'L':  Lower triangular part.
C             For all other values, the array A is not referenced.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrix A.  N >= 0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N upper triangular part
C             (if UPLO = 'U'), or lower triangular part (if UPLO = 'L'),
C             of this array must contain the corresponding upper or
C             lower triangle of the skew-symmetric matrix A.
C             On exit, the leading N-by-N part of this array contains
C             the skew-symmetric matrix A with all elements stored.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= max(1,N).
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     Sep. 2012.
C
C     REVISIONS
C
C     -
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO
      PARAMETER         ( ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, N
C     .. Array Arguments ..
      DOUBLE PRECISION   A(LDA,*)
C     .. Local Scalars ..
      INTEGER            J
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C
C     .. Executable Statements ..
C
C     For efficiency reasons, the parameters are not checked for errors.
C
      IF( LSAME( UPLO, 'L' ) ) THEN
C
C        Construct the upper triangle of A.
C
         DO 20 I = 1, N
            A(I,I) = ZERO
            DO 10 J = 2, N
               A(I,J) = -A(J,I)
   10       CONTINUE
   20    CONTINUE
C
      ELSE IF( LSAME( UPLO, 'U' ) ) THEN
C
C        Construct the lower triangle of A.
C
         DO 40 I = 1, N
            A(I,I) = ZERO
            DO 30 J = 2, N
               A(J,I) = -A(I,J)
   30       CONTINUE
   40    CONTINUE
C
      END IF
      RETURN
C *** Last line of MA02ES ***
      END
