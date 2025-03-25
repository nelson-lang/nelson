      SUBROUTINE MC01PD( K, REZ, IMZ, P, DWORK, INFO )
C
C     PURPOSE
C
C     To compute the coefficients of a real polynomial P(x) from its
C     zeros.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     K       (input) INTEGER
C             The number of zeros (and hence the degree) of P(x).
C             K >= 0.
C
C     REZ     (input) DOUBLE PRECISION array, dimension (K)
C     IMZ     (input) DOUBLE PRECISION array, dimension (K)
C             The real and imaginary parts of the i-th zero of P(x)
C             must be stored in REZ(i) and IMZ(i), respectively, where
C             i = 1, 2, ..., K. The zeros may be supplied in any order,
C             except that complex conjugate zeros must appear
C             consecutively.
C
C     P       (output) DOUBLE PRECISION array, dimension (K+1)
C             This array contains the coefficients of P(x) in increasing
C             powers of x. If K = 0, then P(1) is set to one.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (K+1)
C             If K = 0, this array is not referenced.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value;
C             > 0:  if INFO = i, (REZ(i),IMZ(i)) is a complex zero but
C                   (REZ(i-1),IMZ(i-1)) is not its conjugate.
C
C     METHOD
C
C     The routine computes the coefficients of the real K-th degree
C     polynomial P(x) as
C
C        P(x) = (x - r(1)) * (x - r(2)) * ... * (x - r(K))
C
C     where r(i) = (REZ(i),IMZ(i)).
C
C     Note that REZ(i) = REZ(j) and IMZ(i) = -IMZ(j) if r(i) and r(j)
C     form a complex conjugate pair (where i <> j), and that IMZ(i) = 0
C     if r(i) is real.
C
C     NUMERICAL ASPECTS
C
C     None.
C
C     CONTRIBUTOR
C
C     Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Mar. 1997.
C     Supersedes Release 2.0 routine MC01DD by A.J. Geurts.
C
C     REVISIONS
C
C     V. Sima, May 2002.
C
C     KEYWORDS
C
C     Elementary polynomial operations, polynomial operations.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Scalar Arguments ..
      INTEGER           INFO, K
C     .. Array Arguments ..
      DOUBLE PRECISION  DWORK(*), IMZ(*), P(*), REZ(*)
C     .. Local Scalars ..
      INTEGER           I
      DOUBLE PRECISION  U, V
C     .. External Subroutines ..
      EXTERNAL          DAXPY, DCOPY, XERBLA
C     .. Executable Statements ..
C
C     Test the input scalar arguments.
C
      IF( K.LT.0 ) THEN
         INFO = -1
C
C        Error return.
C
         CALL XERBLA( 'MC01PD', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      INFO = 0
      P(1) = ONE
      IF ( K.EQ.0 )
     $   RETURN
C
      I = 1
C     WHILE ( I <= K ) DO
   20 IF ( I.LE.K ) THEN
         U = REZ(I)
         V = IMZ(I)
         DWORK(1) = ZERO
C
         IF ( V.EQ.ZERO ) THEN
            CALL DCOPY( I, P, 1, DWORK(2), 1 )
            CALL DAXPY( I, -U, P, 1, DWORK, 1 )
            I = I + 1
C
         ELSE
            IF ( I.EQ.K ) THEN
               INFO = K
               RETURN
            ELSE IF ( ( U.NE.REZ(I+1) ) .OR. ( V.NE.-IMZ(I+1) ) ) THEN
               INFO = I + 1
               RETURN
            END IF
C
            DWORK(2) = ZERO
            CALL DCOPY( I, P, 1, DWORK(3), 1 )
            CALL DAXPY( I, -(U + U),  P, 1, DWORK(2), 1 )
            CALL DAXPY( I, U**2+V**2, P, 1, DWORK, 1 )
            I = I + 2
         END IF
C
         CALL DCOPY( I, DWORK, 1, P, 1 )
         GO TO 20
      END IF
C     END WHILE 20
C
      RETURN
C *** Last line of MC01PD ***
      END
