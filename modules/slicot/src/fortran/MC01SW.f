      SUBROUTINE MC01SW( A, B, M, E )
C
C     PURPOSE
C
C     To find the mantissa M and the exponent E of a real number A such
C     that
C        A = M * B**E
C        1 <= ABS( M ) < B
C     if A is non-zero. If A is zero, then M and E are set to 0.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     A       (input) DOUBLE PRECISION
C             The number whose mantissa and exponent are required.
C
C     B       (input) INTEGER
C             The base of the floating-point arithmetic.
C
C     M       (output) DOUBLE PRECISION
C             The mantissa of the floating-point representation of A.
C
C     E       (output) INTEGER
C             The exponent of the floating-point representation of A.
C
C     NUMERICAL ASPECTS
C
C     None.
C
C     CONTRIBUTOR
C
C     Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Mar. 1997.
C     Supersedes Release 2.0 routine MC01GZ by A.J. Geurts.
C
C     REVISIONS
C
C     -
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Scalar Arguments ..
      INTEGER           B, E
      DOUBLE PRECISION  A, M
C     .. Local Scalars ..
      DOUBLE PRECISION  DB
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DBLE
C     .. Executable Statements ..
C
C     Quick return if possible.
C
      IF ( A.EQ.ZERO ) THEN
         M = ZERO
         E = 0
         RETURN
      END IF
C
C     A non-zero.
C
      DB = DBLE( B )
      M = ABS( A )
      E = 0
C     WHILE ( M >= B ) DO
   20 IF ( M.GE.DB ) THEN
         M = M/DB
         E = E + 1
         GO TO 20
      END IF
C     END WHILE 20
C     WHILE ( M < 1 ) DO
   40 IF ( M.LT.ONE ) THEN
         M = M*DB
         E = E - 1
         GO TO 40
      END IF
C     END WHILE 40
C
      IF ( A.LT.ZERO ) M = -M
C
      RETURN
C *** Last line of MC01SW ***
      END
