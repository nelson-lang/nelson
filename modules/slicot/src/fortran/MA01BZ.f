      SUBROUTINE MA01BZ( BASE, K, S, A, INCA, ALPHA, BETA, SCAL )
C
C     PURPOSE
C
C     To compute the general product of K complex scalars trying to
C     avoid over- and underflow.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     BASE    (input)  DOUBLE PRECISION
C             Machine base.
C
C     K       (input)  INTEGER
C             The number of scalars.  K >= 1.
C
C     S       (input)  INTEGER array, dimension (K)
C             The signature array. Each entry of S must be 1 or -1.
C
C     A       (input)  COMPLEX*16 array, dimension (K)
C             Vector of complex scalars.
C
C     INCA    (input)  INTEGER
C             Increment for the array A. INCA <> 0.
C
C     ALPHA   (output)  COMPLEX*16
C             ALPHA is a complex scalar with ABS(ALPHA) = 0, or
C             1.0 <= ABS(ALPHA) < BASE, such that
C
C                ALPHA / BETA * BASE**(SCAL)
C
C             is the general product of the scalars in the array A.
C
C     BETA    (output)  COMPLEX*16
C             BETA is either 0.0 or 1.0.
C             See also the description of ALPHA.
C
C     SCAL    (output)  INTEGER
C             Scaling factor exponent, see ALPHA.
C
C     CONTRIBUTOR
C
C     D. Kressner, Technical Univ. Berlin, Germany, Dec. 2002.
C
C     REVISIONS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     Aug. 2009, SLICOT Library version of the routine ZLAPR1.
C
C     KEYWORDS
C
C     Computer arithmetic, overflow, underflow.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ONE, ZERO
      PARAMETER         ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      COMPLEX*16        CONE, CZERO
      PARAMETER         ( CONE  = ( 1.0D+0, 0.0D+0 ),
     $                    CZERO = ( 0.0D+0, 0.0D+0 ) )
C     .. Scalar Arguments ..
      INTEGER           INCA, K, SCAL
      DOUBLE PRECISION  BASE
      COMPLEX*16        ALPHA, BETA
C     .. Array Arguments ..
      INTEGER           S(*)
      COMPLEX*16        A(*)
C     .. Local Scalars ..
      INTEGER           I, INDA
      COMPLEX*16        CBASE
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DCMPLX
C
C     .. Executable Statements ..
C
      CBASE = DCMPLX( BASE, ZERO )
      ALPHA = CONE
      BETA  = CONE
      SCAL = 0
      INDA = 1
C
      DO 40  I = 1, K
         IF ( S(I).EQ.1 ) THEN
            ALPHA = ALPHA * A(INDA)
         ELSE
            IF ( A(INDA).EQ.CZERO ) THEN
               BETA = CZERO
            ELSE
               ALPHA = ALPHA / A(INDA)
            END IF
         END IF
         IF ( ABS( ALPHA ).EQ.ZERO ) THEN
            ALPHA = CZERO
            SCAL  = 0
            IF ( ABS( BETA ).EQ.ZERO )
     $         RETURN
         ELSE
C           WHILE ( ABS( ALPHA ).LT.ONE )  DO
   10       CONTINUE
               IF ( ABS( ALPHA ).GE.ONE )
     $            GO TO 20
               ALPHA = ALPHA*CBASE
               SCAL  = SCAL - 1
            GO TO 10
C           END WHILE 10
C
C           WHILE ( ABS( ALPHA ).GE.BASE ) DO
   20       CONTINUE
               IF ( ABS( ALPHA ).LT.BASE )
     $            GO TO 30
               ALPHA = ALPHA / CBASE
               SCAL  = SCAL + 1
            GO TO 20
C           END WHILE 20
   30       CONTINUE
         END IF
         INDA = INDA + INCA
   40 CONTINUE
C
      RETURN
C *** Last line of MA01BZ ***
      END
