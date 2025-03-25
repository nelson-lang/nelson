      SUBROUTINE MC01SY( M, E, B, A, OVFLOW )
C
C     PURPOSE
C
C     To find a real number A from its mantissa M and its exponent E,
C     i.e.,
C        A = M * B**E.
C     M and E need not be the standard floating-point values.
C     If ABS(A) < B**(EMIN-1), i.e. the smallest positive model number,
C     then the routine returns A = 0.
C     If M = 0, then the routine returns A = 0 regardless of the value
C     of E.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     M       (input) DOUBLE PRECISION
C             The mantissa of the floating-point representation of A.
C
C     E       (input) INTEGER
C             The exponent of the floating-point representation of A.
C
C     B       (input) INTEGER
C             The base of the floating-point arithmetic.
C
C     A       (output) DOUBLE PRECISION
C             The value of M * B**E.
C
C     OVFLOW  (output) LOGICAL
C             The value .TRUE., if ABS(M) * B**E >= B**EMAX (where EMAX
C             is the largest possible exponent) and .FALSE. otherwise.
C             A is not defined if OVFLOW = .TRUE..
C
C     NUMERICAL ASPECTS
C
C     None.
C
C     CONTRIBUTOR
C
C     Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Mar. 1997.
C     Supersedes Release 2.0 routine MC01GY by A.J. Geurts.
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
      LOGICAL           OVFLOW
      INTEGER           B, E
      DOUBLE PRECISION  A, M
C     .. Local Scalars ..
      INTEGER           EMAX, EMIN, ET, EXPON
      DOUBLE PRECISION  BASE, MT
C     .. External Functions ..
      DOUBLE PRECISION  DLAMCH
      EXTERNAL          DLAMCH
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, MOD
C     .. Executable Statements ..
C
      OVFLOW = .FALSE.
C
      IF ( ( M.EQ.ZERO ) .OR. ( E.EQ.0 ) ) THEN
         A = M
         RETURN
      END IF
C
C     Determination of the mantissa MT and the exponent ET of the
C     standard floating-point representation.
C
      EMIN = DLAMCH( 'Minimum exponent' )
      EMAX = DLAMCH( 'Largest exponent' )
      MT = M
      ET = E
C     WHILE ( ABS( MT ) >= B ) DO
   20 IF ( ABS( MT ).GE.B ) THEN
         MT = MT/B
         ET = ET + 1
         GO TO 20
      END IF
C     END WHILE 20
C     WHILE ( ABS( MT ) < 1 ) DO
   40 IF ( ABS( MT ).LT.ONE ) THEN
         MT = MT*B
         ET = ET - 1
         GO TO 40
      END IF
C     END WHILE 40
C
      IF ( ET.LT.EMIN ) THEN
         A = ZERO
         RETURN
      END IF
C
      IF ( ET.GE.EMAX ) THEN
         OVFLOW = .TRUE.
         RETURN
      END IF
C
C     Computation of the value of A by the relation
C     M * B**E = A * (BASE)**EXPON
C
      EXPON = ABS( ET )
      A = MT
      BASE = B
      IF ( ET.LT.0 ) BASE = ONE/BASE
C     WHILE ( not EXPON = 0 ) DO
   60 IF ( EXPON.NE.0 ) THEN
         IF ( MOD( EXPON, 2 ).EQ.0 ) THEN
            BASE = BASE*BASE
            EXPON = EXPON/2
         ELSE
            A = A*BASE
            EXPON = EXPON - 1
         END IF
         GO TO 60
      END IF
C     END WHILE 60
C
      RETURN
C *** Last line of MC01SY ***
      END
