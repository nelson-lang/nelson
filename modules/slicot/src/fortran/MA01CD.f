      INTEGER FUNCTION MA01CD( A, IA, B, IB )
C
C     PURPOSE
C
C     To compute, without over- or underflow, the sign of the sum of two
C     real numbers represented using integer powers of a base (usually,
C     the machine base). Any base can be used, but it should the same
C     for both numbers. The result is an integer with value 1, 0, or -1,
C     depending on the sum being found as positive, zero, or negative,
C     respectively.
C
C     FUNCTION VALUE
C
C     MA01CD  INTEGER
C             The sign of the sum of the two numbers, which is usually
C             either 1, or -1. If both numbers are 0, or if they have
C             the same exponent and their sum is 0, the returned value
C             is 0.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     A       (input)  DOUBLE PRECISION
C             The first real scalar.
C
C     IA      (input)  INTEGER
C             Exponent of the base for the first real scalar. The scalar
C             is represented as A * BASE**(IA).
C
C     B       (input)  DOUBLE PRECISION
C             The first real scalar.
C
C     IB      (input)  INTEGER
C             Exponent of the base for the first real scalar. The scalar
C             is represented as B * BASE**(IB).
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     Feb. 2010.
C
C     REVISIONS
C
C     -
C
C     KEYWORDS
C
C     Computer arithmetic, overflow, underflow.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Scalar Arguments ..
      INTEGER           IA, IB
      DOUBLE PRECISION  A, B
C     .. Local Scalars ..
      DOUBLE PRECISION  S, SA, SB
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, LOG, SIGN
C
C     .. Executable Statements ..
C
      IF( A.EQ.ZERO .AND. B.EQ.ZERO ) THEN
         MA01CD = 0
      ELSE IF( A.EQ.ZERO ) THEN
         MA01CD = SIGN( ONE, B )
      ELSE IF( B.EQ.ZERO ) THEN
         MA01CD = SIGN( ONE, A )
      ELSE IF( IA.EQ.IB ) THEN
         S = A + B
         IF( S.EQ.ZERO ) THEN
            MA01CD = 0
         ELSE
            MA01CD = SIGN( ONE, S )
         END IF
      ELSE
         SA = SIGN( ONE, A )
         SB = SIGN( ONE, B )
         IF( SA.EQ.SB ) THEN
            MA01CD = SA
         ELSE IF( IA.GT.IB ) THEN
            IF( ( LOG( ABS( A ) ) + IA - IB ).GE.LOG( ABS( B ) ) ) THEN
               MA01CD = SA
            ELSE
               MA01CD = SB
            END IF
         ELSE
            IF( ( LOG( ABS( B ) ) + IB - IA ).GE.LOG( ABS( A ) ) ) THEN
               MA01CD = SB
            ELSE
               MA01CD = SA
            END IF
         END IF
      END IF
C
      RETURN
C *** Last line of MA01CD ***
      END
