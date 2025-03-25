      SUBROUTINE MA01DZ( AR1, AI1, B1, AR2, AI2, B2, EPS, SAFEMN, D1,
     $                   D2, IWARN )
C
C     PURPOSE
C
C     To compute an approximate symmetric chordal metric for two complex
C     numbers A1 and A2 in real arithmetic.  Each number Aj, j = 1, 2,
C     is represented as a rational number with numerator ARj + i*AIj,
C     and denominator Bj, with Bj >= 0.  A value Bj = 0, when ARj or AIj
C     is nonzero, means that the number Aj is infinite.  The case when
C     ARj = AIj = Bj = 0 means that the pair (Aj,Bj) is not a number.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     AR1     (input) DOUBLE PRECISION
C     AI1     (input) DOUBLE PRECISION
C             These scalars define the real and imaginary parts of the
C             numerator of A1.
C
C     B1      (input) DOUBLE PRECISION
C             The denominator of A1.  B1 >= 0.
C
C     AR2     (input) DOUBLE PRECISION
C     AI2     (input) DOUBLE PRECISION
C             These scalars define the real and imaginary parts of the
C             numerator of A2.
C
C     B2      (input) DOUBLE PRECISION
C             The denominator of A2.  B2 >= 0.
C
C     EPS     (input) DOUBLE PRECISION
C             The relative machine precision. See the LAPACK Library
C             routine DLAMCH.
C
C     SAFEMN  (input) DOUBLE PRECISION
C             The "safe minimum", such that its reciprocal does not
C             overflow. See the LAPACK Library routine DLAMCH.
C
C     D1      (output) DOUBLE PRECISION
C             The numerator of the chordal metric D.  D1 >= 0.
C
C     D2      (output) DOUBLE PRECISION
C             The denominator of the chordal metric D.  D2 is 0 or 1.
C             If D2 = 0, and D1 = 0, the chordal metric is undefined, so
C             either A1 and/or A2 are undefined.
C
C     Warning Indicator
C
C     IWARN   INTEGER
C             = 0:  no warning;
C             = 1:  A1 or A2 is not a number (NaN);  D1 and D2 are both
C                   set to 0.
C
C     METHOD
C
C     The approximate symmetric chordal metric is evaluated using the
C     formula
C
C        D = MIN( | A1 - A2 |, |1/A1 - 1/A2| ),
C
C     taking into account the special cases of infinite or NaN values.
C     The chordal metric is finite even if A1 and A2 are both infinite,
C     or if one of them is infinite and the other is finite, nonzero.
C
C     CONTRIBUTOR
C
C     V. Sima, Feb. 2023.
C
C     REVISIONS
C
C     V. Sima, Mar. 2023.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE, TWO, FOUR
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                    FOUR = 4.0D0 )
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION  AI1, AI2, AR1, AR2, B1, B2, D1, D2, EPS, SAFEMN
      INTEGER           IWARN
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION  AP1, AP2, BIG, MX1, MX2, PAR, PI1, PI2, PR1,
     $                  PR2
      LOGICAL           INF1, INF2, ZER1, ZER2
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
      IWARN = 0
C
      PAR = FOUR - TWO*EPS
      BIG = PAR/SAFEMN
C
C     Ensure that BIG is representable.
C
      IF( BIG*SAFEMN.GT.PAR )
     $   BIG = ONE/SAFEMN
C
      MX1 = MAX( ABS( AR1 ), ABS( AI1 ) )
      MX2 = MAX( ABS( AR2 ), ABS( AI2 ) )
C
      IF( B1.EQ.ZERO ) THEN
         IF( MX1.EQ.ZERO ) THEN
            D1 = ZERO
            D2 = ZERO
            IWARN = 1
         ELSE
            IF( B2.EQ.ZERO ) THEN
               D1 = ZERO
               IF( MX2.EQ.ZERO ) THEN
                  D2 = ZERO
                  IWARN = 1
               ELSE
                  D2 = ONE
               END IF
            ELSE IF( B2.GT.ONE ) THEN
               IF( MX2.GT.B2/BIG ) THEN
                  D1 = B2/DLAPY2( AR2, AI2 )
                  D2 = ONE
               ELSE
                  D1 = ONE
                  D2 = ZERO
               END IF
            ELSE IF( MX2.GT.ZERO ) THEN
               D1 = B2/DLAPY2( AR2, AI2 )
               D2 = ONE
            ELSE 
               D1 = ONE
               D2 = ZERO
            END IF
         END IF
C
      ELSE IF( B2.EQ.ZERO ) THEN
         IF( MX2.EQ.ZERO ) THEN
            D1 = ZERO
            D2 = ZERO
            IWARN = 1
         ELSE
            IF( B1.GT.ONE ) THEN
               IF( MX1.GT.B1/BIG ) THEN
                  D1 = B1/DLAPY2( AR1, AI1 )
                  D2 = ONE
               ELSE
                  D1 = ONE
                  D2 = ZERO
               END IF
            ELSE IF( MX1.GT.ZERO ) THEN
               D1 = B1/DLAPY2( AR1, AI1 )
               D2 = ONE
            ELSE 
               D1 = ONE
               D2 = ZERO
            END IF
         END IF
C
      ELSE
C
C        ZERj = .TRUE. means that Aj is practically 0.
C        INFj = .TRUE. means that Aj is infinite.
C
         IF( B1.GE.ONE ) THEN
            INF1 = .FALSE.
            AP1  = DLAPY2( AR1/B1, AI1/B1 )
            ZER1 = AP1.LT.ONE/BIG
         ELSE
            ZER1 = .FALSE.
            INF1 = MX1.GT.B1*BIG
            IF( .NOT.INF1 )
     $         AP1 = DLAPY2( AR1/B1, AI1/B1 )
         END IF
C
         IF( B2.GE.ONE ) THEN
            INF2 = .FALSE.
            AP2  = DLAPY2( AR2/B2, AI2/B2 )
            ZER2 = AP2.LT.ONE/BIG
         ELSE
            ZER2 = .FALSE.
            INF2 = MX2.GT.B2*BIG
            IF( .NOT.INF2 )
     $         AP2 = DLAPY2( AR2/B2, AI2/B2 )
         END IF
C
C        A1 and/or A2 are/is (practically) 0.
C
         D2 = ONE
         IF( ZER1 .AND. ZER2 ) THEN
            D1 = ZERO
         ELSE IF( ZER1 ) THEN
            IF( .NOT.INF2 ) THEN
               D1 = AP2
            ELSE
               D1 = ONE
               D2 = ZERO
            END IF
         ELSE IF( ZER2 ) THEN
            IF( .NOT.INF1 ) THEN
               D1 = AP1
            ELSE
               D1 = ONE
               D2 = ZERO
            END IF
C
         ELSE IF( INF1 ) THEN
C
C           A1 and possibly A2 is/are practically infinite.
C
            IF( INF2 ) THEN
               D1 = ZERO
            ELSE
               D1 = B2/DLAPY2( AR2, AI2 )
            END IF
         ELSE IF( INF2 ) THEN
C
C           A2 is practically infinite.
C
            D1 = B1/DLAPY2( AR1, AI1 )
C
         ELSE
C
C           A1 and A2 are finite, representable numbers.
C
            PR1 = AR1/B1
            PI1 = AI1/B1
            PR2 = AR2/B2
            PI2 = AI2/B2
            D1 = MIN( DLAPY2( PR1 - PR2, PI1 - PI2 ),
     $                DLAPY2( ( PR1/AP1 )/AP1 - ( PR2/AP2 )/AP2,
     $                        ( PI2/AP2 )/AP2 - ( PI1/AP1 )/AP1 ) )
         END IF
      END IF
C
      RETURN
C     *** Last line of MA01DZ ***
      END
