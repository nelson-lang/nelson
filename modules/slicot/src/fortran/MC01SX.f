      INTEGER FUNCTION MC01SX( LB, UB, E, MANT )
C
C     PURPOSE
C
C     To compute the variation V of the exponents of a series of
C     non-zero floating-point numbers: a(j) = MANT(j) * beta**(E(j)),
C     where beta is the base of the machine representation of
C     floating-point numbers, i.e.,
C     V = max(E(j)) - min(E(j)), j = LB,...,UB and MANT(j) non-zero.
C
C     CONTRIBUTOR
C
C     Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Mar. 1997.
C     Supersedes Release 2.0 routine MC01GX by A.J. Geurts.
C
C     REVISIONS
C
C     -
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION        ZERO
      PARAMETER               ( ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      INTEGER                 LB, UB
C     .. Array Arguments ..
      INTEGER                 E(*)
      DOUBLE PRECISION        MANT(*)
C     .. Local Scalars ..
      INTEGER                 J, MAXE, MINE
C     .. Intrinsic Functions ..
      INTRINSIC               MAX, MIN
C     .. Executable Statements ..
C
      MAXE = E(LB)
      MINE = MAXE
C
      DO 20 J = LB + 1, UB
         IF ( MANT(J).NE.ZERO ) THEN
            MAXE = MAX( MAXE, E(J) )
            MINE = MIN( MINE, E(J) )
         END IF
   20 CONTINUE
C
      MC01SX = MAXE - MINE
C
      RETURN
C *** Last line of MC01SX ***
      END
