      LOGICAL FUNCTION SB02CX( REIG, IEIG )
C
C     PURPOSE
C
C     To select the purely imaginary eigenvalues in computing the
C     H-infinity norm of a system.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     REIG    (input) DOUBLE PRECISION
C             The real part of the current eigenvalue considered.
C
C     IEIG    (input) DOUBLE PRECISION
C             The imaginary part of the current eigenvalue considered.
C
C     METHOD
C
C     The function value SB02CX is set to .TRUE. for a purely imaginary
C     eigenvalue and to .FALSE., otherwise.
C
C     REFERENCES
C
C     None.
C
C     NUMERICAL ASPECTS
C
C     None.
C
C     CONTRIBUTOR
C
C     P. Hr. Petkov, Technical University of Sofia, May, 1999.
C
C     REVISIONS
C
C     P. Hr. Petkov, Technical University of Sofia, Oct. 2000.
C
C     KEYWORDS
C
C     H-infinity norm, robust control.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  HUNDRD
      PARAMETER         ( HUNDRD = 100.0D+0 )
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION  IEIG, REIG
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION  EPS, TOL
C     ..
C     .. External Functions ..
      DOUBLE PRECISION  DLAMCH
      EXTERNAL          DLAMCH
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC         ABS
C     ..
C     .. Executable Statements ..
C
C     Get the machine precision.
C
      EPS = DLAMCH( 'Epsilon' )
C
C     Set the tolerance in the determination of the purely
C     imaginary eigenvalues.
C
      TOL = HUNDRD*EPS
      SB02CX = ABS( REIG ).LT.TOL
C
      RETURN
C *** Last line of SB02CX ***
      END
