      SUBROUTINE MA01DD( AR1, AI1, AR2, AI2, EPS, SAFEMN, D )
C
C     PURPOSE
C
C     To compute an approximate symmetric chordal metric for two complex
C     numbers A1 and A2, with Aj = ARj + i*AIj, j = 1, 2.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     AR1     (input) DOUBLE PRECISION
C     AI1     (input) DOUBLE PRECISION
C             These scalars define the real and imaginary parts of the
C             number A1.
C
C     AR2     (input) DOUBLE PRECISION
C     AI2     (input) DOUBLE PRECISION
C             These scalars define the real and imaginary parts of the
C             number A2.
C
C     EPS     (input) DOUBLE PRECISION
C             The relative machine precision. See the LAPACK Library
C             routine DLAMCH.
C
C     SAFEMN  (input) DOUBLE PRECISION
C             The "safe minimum", such that its reciprocal does not
C             overflow. See the LAPACK Library routine DLAMCH.
C
C     D       (output) DOUBLE PRECISION
C             The approximate symmetric chordal metric D.  D >= 0.
C
C     METHOD
C
C     The approximate symmetric chordal metric is evaluated using the
C     formula
C
C        D = MIN( | A1 - A2 |, |1/A1 - 1/A2| ).
C
C     The chordal metric is finite even if A1 and A2 are both infinite,
C     or if one of them is infinite and the other is finite, nonzero.
C
C     CONTRIBUTOR
C
C     V. Sima, Mar. 2023.
C
C     REVISIONS
C
C     -
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE, TWO, FOUR
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                    FOUR = 4.0D0 )
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION  AI1, AI2, AR1, AR2, D, EPS, SAFEMN
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION  AP1, AP2, BIG, D1, D2, MX, MX1, MX2, PAR
C     ..
C     .. External Functions ..
      DOUBLE PRECISION  DLAPY2
      EXTERNAL          DLAPY2
C
C     .. Intrinsic functions ..
      INTRINSIC         ABS, MAX, MIN
C     ..
C     .. Executable Statements ..
C
C     For efficiency reasons, this routine does not check the input
C     parameters for errors.
C
      PAR = FOUR - TWO*EPS
      BIG = PAR/SAFEMN
C
C     Ensure that BIG is representable.
C
      IF( BIG*SAFEMN.GT.PAR )
     $   BIG = ONE/SAFEMN
C
C     Quick return if possible.
C
      MX1 = MAX( ABS( AR1 ), ABS( AI1 ) )
      MX2 = MAX( ABS( AR2 ), ABS( AI2 ) )
      MX  = MAX( MX1, MX2  )
C
      IF( MX.EQ.ZERO ) THEN
         D = ZERO
         GO TO 10
      ELSE IF( MX.LT.BIG ) THEN
         IF( MX2.EQ.ZERO ) THEN
            D = DLAPY2( AR1, AI1 )
            GO TO 10
         ELSE IF( MX1.EQ.ZERO ) THEN
            D = DLAPY2( AR2, AI2 )
            GO TO 10
         ELSE
            D1 = DLAPY2( AR1 - AR2, AI1 - AI2 )
         END IF
      ELSE
         D1 = BIG
      END IF
C
      IF( MX.GT.ONE/BIG ) THEN
         AP1 = DLAPY2( AR1, AI1 )
         AP2 = DLAPY2( AR2, AI2 )
         IF( MX1.LE.BIG .AND. MX2.LE.BIG ) THEN
            D2 = DLAPY2( ( AR1/AP1 )/AP1 - ( AR2/AP2 )/AP2,
     $                   ( AI2/AP2 )/AP2 - ( AI1/AP1 )/AP1 )
         ELSE IF( MX1.LE.BIG ) THEN
            D2 = ONE/AP1
         ELSE IF( MX2.LE.BIG ) THEN
            D2 = ONE/AP2
         ELSE
            D2 = ZERO
         END IF
      ELSE
         D2 = BIG
      END IF
C
      D = MIN( D1, D2 )
C
   10 CONTINUE
      RETURN
C     *** Last line of MA01DD ***
      END
