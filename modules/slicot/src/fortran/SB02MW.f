      LOGICAL FUNCTION SB02MW( REIG, IEIG )
C
C     PURPOSE
C
C     To select the stable eigenvalues for solving the discrete-time
C     algebraic Riccati equation.
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
C     The function value SB02MW is set to .TRUE. for a stable
C     eigenvalue (i.e., with modulus less than one) and to .FALSE.,
C     otherwise.
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
C     V. Sima, Katholieke Univ. Leuven, Belgium, Aug. 1997.
C
C     REVISIONS
C
C     -
C
C     KEYWORDS
C
C     Algebraic Riccati equation, closed loop system, discrete-time
C     system, optimal regulator, Schur form.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ONE
      PARAMETER         ( ONE = 1.0D0 )
C     .. Scalar Arguments ..
      DOUBLE PRECISION  IEIG, REIG
C     .. External Functions ..
      DOUBLE PRECISION   DLAPY2
      EXTERNAL           DLAPY2
C     .. Executable Statements ..
C
      SB02MW = DLAPY2( REIG, IEIG ).LT.ONE
C
      RETURN
C *** Last line of SB02MW ***
      END
