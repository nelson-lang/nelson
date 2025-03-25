      SUBROUTINE MA02PZ( M, N, A, LDA, NZR, NZC )
C
C     PURPOSE
C
C     To compute the number of zero rows and zero columns of a complex
C     matrix.
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
C     A       (input) COMPLEX*16 array, dimension (LDA,N)
C             The leading M-by-N part of this array must contain the
C             matrix A.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= max(1,M).
C
C     NZR     (output) INTEGER
C             The number of zero rows of the matrix A.
C
C     NZC     (output) INTEGER
C             The number of zero columns of the matrix A.
C
C     CONTRIBUTORS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Sep. 2016.
C
C     REVISIONS
C
C     -
C
C     KEYWORDS
C
C     Elementary matrix operations, complex matrix.
C
C     ******************************************************************
C
C     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER          ( ZERO = ( 0.0D+0, 0.0D+0 ) )
C     ..
C     .. Scalar Arguments ..
      INTEGER            LDA, M, N, NZC, NZR
C     ..
C     .. Array Arguments ..
      COMPLEX*16         A( LDA, * )
C     ..
C     .. Local Scalars ..
      INTEGER            I, J
C
C     ..Intrinsic Functions..
      INTRINSIC         MIN
C     ..
C     .. Executable Statements ..
C
C     For efficiency reasons, the parameters are not checked.
C
      NZC = 0
      NZR = 0
C
      IF( MIN( M, N ).GT.0 ) THEN
C
C        Scan columns 1 .. N.
C
         I = 0
C        WHILE ( I.LE.N ) DO
   10    CONTINUE
         I = I + 1
         IF( I.LE.N ) THEN
            DO 20  J = 1, M
               IF( A( J, I ).NE.ZERO )
     $            GO TO 10
   20       CONTINUE
            NZC = NZC + 1
            GO TO 10
C
C           END WHILE 10
         END IF
C
C        Scan rows 1 .. M.
C
         I = 0
C        WHILE ( I.LE.M ) DO
   30    CONTINUE
         I = I + 1
         IF( I.LE.M ) THEN
            DO 40  J = 1, N
               IF( A( I, J ).NE.ZERO )
     $            GO TO 30
   40       CONTINUE
            NZR = NZR + 1
            GO TO 30
C
         END IF
C        END WHILE 30
      END IF
      RETURN
C
C *** Last line of MA02PZ ***
      END
