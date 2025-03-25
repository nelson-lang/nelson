      SUBROUTINE SB08MY( DA, A, B, EPSB )
C
C     PURPOSE
C
C     To compute the coefficients of B(s) = A(s) * A(-s) and a norm
C     for the accuracy of the computed coefficients.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     DA      (input) INTEGER
C             The degree of the polynomials A(s) and B(s).  DA >= 0.
C
C     A       (input) DOUBLE PRECISION array, dimension (DA+1)
C             This array must contain the coefficients of the polynomial
C             A(s) in increasing powers of s.
C
C     B       (output) DOUBLE PRECISION array, dimension (DA+1)
C             This array contains the coefficients of the polynomial
C             B(s) in increasing powers of s**2.
C
C     EPSB    (input/output) DOUBLE PRECISION
C             On entry, EPSB must contain the machine precision (see
C             LAPACK Library routine DLAMCH).
C             On exit, EPSB contains an updated value, using a norm
C             for the accuracy of the computed coefficients.
C
C     CONTRIBUTOR
C
C     Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Aug. 1997.
C     Supersedes Release 2.0 routine SB08AZ by A.J. Geurts.
C
C     REVISIONS
C
C     -
C
C     KEYWORDS
C
C     Laplace transform, polynomial operations, spectral factorization.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE, TWO, THREE
      PARAMETER         ( ZERO  = 0.0D0, ONE = 1.0D0, TWO=2.0D0,
     $                    THREE = 3.0D0 )
C     .. Scalar Arguments ..
      INTEGER           DA
      DOUBLE PRECISION  EPSB
C     .. Array Arguments ..
      DOUBLE PRECISION  A(*), B(*)
C     .. Local Scalars ..
      INTEGER           I, K
      DOUBLE PRECISION  MAXSA, SA, SABS, SIGNI, SIGNK, TERM
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, MAX, MIN
C     .. Executable Statements ..
C
      SIGNI = ONE
      MAXSA = ZERO
C
      DO 40 I = 0, DA
         SABS = A(I+1)**2
         SA = SIGNI*SABS
         SIGNK = -TWO*SIGNI
C
         DO 20 K = 1, MIN( I, DA - I )
            TERM = SIGNK*A(I-K+1)*A(I+K+1)
            SA = SA + TERM
            SABS = SABS + ABS( TERM )
            SIGNK = -SIGNK
   20    CONTINUE
C
         B(I+1) = SA
         MAXSA = MAX( MAXSA, SABS )
         SIGNI = -SIGNI
   40 CONTINUE
C
      EPSB = THREE*MAXSA*EPSB
C
      RETURN
C *** Last line of SB08MY ***
      END
