      LOGICAL FUNCTION MA02HZ( JOB, M, N, DIAG, A, LDA )
C
C     PURPOSE
C
C     To check if A = DIAG*I, where I is an M-by-N matrix with ones on
C     the diagonal and zeros elsewhere, A is a complex matrix and DIAG
C     is a complex scalar.
C
C     FUNCTION VALUE
C
C     MA02HZ  LOGICAL
C             The function value is set to .TRUE. if A = DIAG*I, and to
C             .FALSE., otherwise. If min(M,N) = 0, the value is .FALSE.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOB     CHARACTER*1
C             Specifies the part of the matrix A to be checked out,
C             as follows:
C             = 'U': Upper triangular/trapezoidal part;
C             = 'L': Lower triangular/trapezoidal part.
C             Otherwise:  All of the matrix A.
C
C     Input/Output Parameters
C
C     M      (input) INTEGER
C            The number of rows of the matrix A.  M >= 0.
C
C     N      (input) INTEGER
C            The number of columns of the matrix A.  N >= 0.
C
C     DIAG   (input) COMPLEX*16
C            The scalar DIAG.
C
C     A      (input) COMPLEX*16 array, dimension (LDA,N)
C            The leading M-by-N part of this array must contain the
C            matrix A.  If JOB = 'U', only the upper triangle or
C            trapezoid is accessed; if JOB = 'L', only the lower
C            triangle or trapezoid is accessed.
C
C     LDA    INTEGER
C            The leading dimension of the array A.  LDA >= max(1,M).
C
C     METHOD
C
C     The routine returns immediately after detecting a diagonal element
C     which differs from DIAG, or a nonzero off-diagonal element in the
C     searched part of A.
C
C     CONTRIBUTORS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Dec. 2015.
C
C     REVISIONS
C
C     V. Sima, Jan. 2016.
C
C     KEYWORDS
C
C     Elementary operations.
C
C     ******************************************************************
C
C     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER          ( ZERO = ( 0.0D+0, 0.0D+0 ) )
C     .. Scalar Arguments ..
      CHARACTER          JOB
      INTEGER            LDA, M, N
      COMPLEX*16         DIAG
C     .. Array Arguments ..
      COMPLEX*16         A(LDA,*)
C     .. Local Scalars ..
      INTEGER            I, J
C     .. External Functions
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. Intrinsic Functions ..
      INTRINSIC          MIN
C
C     .. Executable Statements ..
C
C     Do not check parameters, for efficiency.
C     Quick return if possible.
C
      IF( MIN( M, N ).EQ.0 ) THEN
         MA02HZ = .FALSE.
         RETURN
      END IF
C
      IF( LSAME( JOB, 'U' ) ) THEN
C
         DO 20 J = 1, N
C
            DO 10 I = 1, MIN( J-1, M )
               IF( A(I,J).NE.ZERO ) THEN
                  MA02HZ = .FALSE.
                  RETURN
               END IF
   10       CONTINUE
C
            IF( J.LE.M ) THEN
               IF( A(J,J).NE.DIAG ) THEN
                  MA02HZ = .FALSE.
                  RETURN
               END IF
            END IF
   20    CONTINUE
C
      ELSE IF( LSAME( JOB, 'L' ) ) THEN
C
         DO 40 J = 1, MIN( M, N )
            IF( A(J,J).NE.DIAG ) THEN
               MA02HZ = .FALSE.
               RETURN
            END IF
C
            IF( J.LT.M ) THEN
C
              DO 30 I = J+1, M
                  IF( A(I,J).NE.ZERO ) THEN
                     MA02HZ = .FALSE.
                     RETURN
                  END IF
   30          CONTINUE
C
            END IF
   40    CONTINUE
C
      ELSE
C
         DO 70 J = 1, N
C
            DO 50 I = 1, MIN( J-1, M )
               IF( A(I,J).NE.ZERO ) THEN
                  MA02HZ = .FALSE.
                  RETURN
               END IF
   50       CONTINUE
C
            IF( J.LE.M ) THEN
               IF( A(J,J).NE.DIAG ) THEN
                  MA02HZ = .FALSE.
                  RETURN
               END IF
            END IF
C
            IF ( J.LT.M ) THEN
C
               DO 60 I = J+1, M
                  IF( A(I,J).NE.ZERO ) THEN
                     MA02HZ = .FALSE.
                     RETURN
                  END IF
   60          CONTINUE
C
            END IF
   70    CONTINUE
C
      END IF
C
      MA02HZ = .TRUE.
C
      RETURN
C *** Last line of MA02HZ ***
      END
