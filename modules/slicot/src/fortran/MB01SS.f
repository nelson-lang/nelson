      SUBROUTINE MB01SS( JOBS, UPLO, N, A, LDA, D )
C
C     PURPOSE
C
C     To scale a symmetric N-by-N matrix A using the row and column
C     scaling factors stored in the vector D.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOBS    CHARACTER*1
C             Specifies the scaling operation to be done, as follows:
C             = 'D':  row and column scaling with D, i.e., A will be
C                     transformed to diag(D)*A*diag(D);
C             = 'I':  row and column scaling with inv(D), i.e., A will
C                     be transformed to inv(diag(D))*A*inv(diag(D)).
C
C     UPLO    CHARACTER*1
C             Specifies which triangle of the matrix A is stored, as
C             follows:
C             = 'U':  Upper triangle is stored;
C             = 'L':  Lower triangle is stored.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrix A.  N >= 0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N upper triangular part
C             (if UPLO = 'U') or lower triangular part (if UPLO = 'L')
C             of this array must contain the upper triangular part or
C             lower triangular part, respectively, of the symmetric
C             matrix A.
C             On exit, the leading N-by-N upper triangular part
C             (if UPLO = 'U') or lower triangular part (if UPLO = 'L')
C             of this array contains the corresponding triangular part
C             of the matrix diag(D)*A*diag(D), if JOBS = 'D', or of the
C             matrix inv(diag(D))*A*inv(diag(D)), JOBS = 'I'.
C             The stricly lower triangular part (if UPLO = 'U') or
C             stricly upper triangular part (if UPLO = 'L') is not
C             referenced.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= max(1,M).
C
C     D       (input) DOUBLE PRECISION array, dimension (N)
C             The diagonal elements of the diagonal matrix D.
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, July 2019.
C
C    ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE   = 1.0D0 )
C     .. Scalar Arguments ..
      CHARACTER          JOBS, UPLO
      INTEGER            LDA, N
C     .. Array Arguments ..
      DOUBLE PRECISION   A(LDA,*), D(*)
C     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            I, J
      DOUBLE PRECISION   DJ
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. Executable Statements ..
C
C     Quick return if possible.
C
      IF( N.EQ.0 )
     $   RETURN
C
      UPPER = LSAME( UPLO, 'U' )
C
      IF( LSAME( JOBS, 'D' ) ) THEN
C
C        Row and column scaling with D.
C
         IF( UPPER ) THEN
C
            DO 20 J = 1, N
               DJ = D(J)
               DO 10 I = 1, J
                  A(I,J) = DJ*D(I)*A(I,J)
   10          CONTINUE
   20       CONTINUE
C
         ELSE
C
            DO 40 J = 1, N
               DJ = D(J)
               DO 30 I = J, N
                  A(I,J) = DJ*D(I)*A(I,J)
   30          CONTINUE
   40       CONTINUE
C
         END IF
C
      ELSE
C
C        Row and column scaling with inv(D).
C
         IF( UPPER ) THEN
C
            DO 60 J = 1, N
               DJ = ONE/D(J)
               DO 50 I = 1, J
                  A(I,J) = ( DJ/D(I) )*A(I,J)
   50          CONTINUE
   60       CONTINUE
C
         ELSE
C
            DO 80 J = 1, N
               DJ = D(J)
               DO 70 I = J, N
                  A(I,J) = ( DJ/D(I) )*A(I,J)
   70          CONTINUE
   80       CONTINUE
C
         END IF
C
      END IF
C
      RETURN
C *** Last line of MB01SS ***
      END
