      SUBROUTINE MC01XD( ALPHA, BETA, GAMMA, DELTA, EVR, EVI, EVQ,
     $                   DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To compute the roots of the polynomial
C
C         P(t) = ALPHA + BETA*t + GAMMA*t^2 + DELTA*t^3 .
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     ALPHA   (input) DOUBLE PRECISION
C     BETA    (input) DOUBLE PRECISION
C     GAMMA   (input) DOUBLE PRECISION
C     DELTA   (input) DOUBLE PRECISION
C             The coefficients of the polynomial P.
C
C     EVR     (output) DOUBLE PRECISION array, DIMENSION at least 3
C     EVI     (output) DOUBLE PRECISION array, DIMENSION at least 3
C     EVQ     (output) DOUBLE PRECISION array, DIMENSION at least 3
C             On output, the kth root of P will be equal to
C             (EVR(K) + i*EVI(K))/EVQ(K) if EVQ(K) .NE. ZERO. Note that
C             the quotient may over- or underflow. If P has a degree d
C             less than 3, then 3-d computed roots will be infinite.
C             EVQ(K) >= 0.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, DIMENSION (LDWORK)
C             On exit, if LDWORK = -1 on input, then DWORK(1) returns
C             the optimal value of LDWORK.
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.  LDWORK >= 42.
C
C             If LDWORK = -1, an optimal workspace query is assumed; the
C             routine only calculates the optimal size of the DWORK
C             array, returns this value as the first entry of the DWORK
C             array, and no error message is issued by XERBLA.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value;
C             > 0:  if INFO = j, 1 <= j <= 6, an error occurred during
C                   the call to one of the LAPACK Library routines DGGEV
C                   or DGEEV (1 <= j <= 3). If INFO < 3, the values
C                   returned in EVR(K), EVI(K), and EVQ(K) should be
C                   correct for K = INFO+1,...,3.
C
C     METHOD
C
C     A matrix pencil is built, whose eigenvalues are the roots of the
C     given polynomial, and they are computed using the QZ algorithm.
C     However, when the ratio between the largest and smallest (in
C     magnitude) polynomial coefficients is relatively big, and either
C     ALPHA or DELTA has the largest magnitude, then a standard
C     eigenproblem is solved using the QR algorithm, and EVQ(I) are set
C     to 1, for I = 1,2,3.
C
C     NUMERICAL ASPECTS
C
C     The algorithm is numerically stable.
C
C     CONTRIBUTORS
C
C     M. Slowik, Institut fur Mathematik, TU Berlin, Dec. 2004.
C     P. Benner, Fakultat fur Mathematik, TU Chemnitz, Dec. 2004.
C     V. Sima, Research Institute for Informatics, Bucharest, Nov. 2005.
C
C     REVISIONS
C
C     V. Sima, Jan. 2013, Dec. 2013.
C
C     KEYWORDS
C
C     Eigenvalues, equivalence transformation, generalized real Schur
C     form, orthogonal transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE, TEN
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0, TEN = 1.0D1 )
C     .. Scalar Arguments ..
      INTEGER           INFO, LDWORK
      DOUBLE PRECISION  ALPHA, BETA, DELTA, GAMMA
C     .. Array Arguments ..
      DOUBLE PRECISION  DWORK(*), EVI(3), EVQ(3), EVR(3)
C     .. Local Scalars ..
      INTEGER           I, J, M2POS, NMIN, WRKPOS
      DOUBLE PRECISION  MAXC, MINC, VAR
C     .. External Subroutines ..
      EXTERNAL          DGEEV, DGGEV, DLADIV, DLASET, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, INT, MAX, MIN
C     .. Executable Statements ..
C
      INFO = 0
C
C     Test the input scalar arguments.
C
      NMIN = 42
      IF ( LDWORK.EQ.-1 ) THEN
         CALL DGEEV( 'N', 'N', 3, DWORK, 3, EVR, EVI, DWORK, 1, DWORK,
     $               1, DWORK, -1, INFO )
         CALL DGGEV( 'N', 'N', 3, DWORK, 3, DWORK, 3, EVR, EVI, EVQ,
     $               DWORK, 1, DWORK, 1, DWORK(2), -1, INFO )
         DWORK(1) = MAX( NMIN, 9+INT( DWORK(1) ), 18+INT( DWORK(2) ) )
         RETURN
      ELSE IF ( LDWORK.LT.NMIN ) THEN
         INFO = -9
C
C        Error return.
C
         CALL XERBLA( 'MC01XD', -INFO )
         RETURN
      END IF
C
C     Initialize DWORK positions.
C
      CALL DLASET( 'All', 18, 1, ZERO, ZERO, DWORK, 18 )
C
      M2POS  = 10
      WRKPOS = 19
C
C     Specify the different cases.
C
      IF ( ABS( ALPHA ).GT.ABS( BETA ) ) THEN
         I = 0
         EVR(1) = ALPHA
      ELSE
         I = 1
         EVR(1) = BETA
      END IF
C
      IF ( ABS( GAMMA ).GT.ABS( DELTA ) ) THEN
         J = 2
         EVR(2) = GAMMA
      ELSE
         J = 3
         EVR(2) = DELTA
      END IF
C
      IF ( ABS( EVR(2) ).GT.ABS( EVR(1) ) ) THEN
         I = J
         MAXC = ABS( EVR(2) )
      ELSE
         MAXC = ABS( EVR(1) )
      END IF
C
      MINC = MIN( ABS( ALPHA ), ABS( BETA ),
     $            ABS( GAMMA ), ABS( DELTA ) )
      IF ( MINC.GT.ZERO ) THEN
         VAR = MAXC/MINC
      ELSE
         VAR = MAXC
      END IF
C
      IF ( VAR.GT.TEN ) THEN
C
C        Large variation in the coefficients of the polynomial.
C        Generate the matrices M1 and M2 stored in DWORK.
C        If I = 0 or 3, a standard eigenvalue problem is solved,
C        since the current LAPACK generalized eigensolver does not
C        perform balancing.
C
         IF ( I.EQ.0 ) THEN
C
C           Elements of matrix M2.
C
            DWORK(1) = -BETA/ALPHA
            DWORK(2) =  ONE
            DWORK(4) = -GAMMA/ALPHA
            DWORK(6) =  ONE
            DWORK(7) = -DELTA/ALPHA
C
         ELSE IF ( I.EQ.1 ) THEN
C
C           Elements of matrix M1.
C
            DWORK(1) = -ALPHA/BETA
            DWORK(4) = -GAMMA/BETA
            DWORK(5) =  ONE
            DWORK(7) = -DELTA/BETA
            DWORK(9) =  ONE
C
C           Elements of matrix M2.
C
            DWORK(10) = ONE
            DWORK(11) = DWORK(1)
            DWORK(14) = DWORK(4)
            DWORK(15) = ONE
            DWORK(17) = DWORK(7)
C
         ELSE IF ( I.EQ.2 ) THEN
C
C           Elements of matrix M1.
C
            DWORK(2) = -ALPHA/GAMMA
            DWORK(4) =  ONE
            DWORK(5) = -BETA/GAMMA
            DWORK(8) = -DELTA/GAMMA
            DWORK(9) =  ONE
C
C           Elements of matrix M2.
C
            DWORK(10) = ONE
            DWORK(12) = DWORK(2)
            DWORK(14) = ONE
            DWORK(15) = DWORK(5)
            DWORK(18) = DWORK(8)
C
         ELSE
C
C           Elements of matrix M1.
C
            DWORK(3) = -ALPHA/DELTA
            DWORK(4) =  ONE
            DWORK(6) = -BETA/DELTA
            DWORK(8) =  ONE
            DWORK(9) = -GAMMA/DELTA
         END IF
C
C        Compute the roots of the polynomial by solving an eigenproblem
C        using the QR- or QZ-Algorithm.
C
         IF ( I.EQ.0 .OR. I.EQ.3 ) THEN
            CALL DGEEV( 'N', 'N', 3, DWORK, 3, EVR, EVI, DWORK(WRKPOS),
     $                  1, DWORK(WRKPOS), 1, DWORK(M2POS), LDWORK-9,
     $                  INFO )
            IF ( I.EQ.0 ) THEN
C
C              The roots are reciprocals of the computed eigenvalues.
C
               J = 1
C              WHILE J.LE.3-INFO
   10          CONTINUE
               IF ( J.LE.3 - INFO ) THEN
                  IF ( EVI(J).EQ.ZERO ) THEN
                     EVR(J) = ONE/EVR(J)
                     J = J + 1
                     GO TO 10
                  ELSE IF ( EVI(J).GT.ZERO ) THEN
                     CALL DLADIV( ONE, ZERO, EVR(J), EVI(J), EVR(J+1),
     $                            EVI(J+1) )
                     EVR(J) =  EVR(J+1)
                     EVI(J) = -EVI(J+1)
                     J = J + 2
                     GO TO 10
                  END IF
               END IF
C              END WHILE 10
            END IF
            EVQ(1) = ONE
            EVQ(2) = ONE
            EVQ(3) = ONE
         ELSE
            CALL DGGEV( 'N', 'N', 3, DWORK, 3, DWORK(M2POS), 3, EVR,
     $                  EVI, EVQ, DWORK(WRKPOS), 1, DWORK(WRKPOS), 1,
     $                  DWORK(WRKPOS), LDWORK-18, INFO )
         END IF
C
      ELSE
C
C        Small variation in the coefficients of the polynomial.
C        Generate the matrices M1 and M2 stored in DWORK.
C
         IF ( I.EQ.0 ) THEN
C
C           Elements of matrix M1.
C
            DWORK(1) = ALPHA
            DWORK(5) = ALPHA
            DWORK(9) = ALPHA
C
C           Elements of matrix M2.
C
            DWORK(10) = -BETA
            DWORK(11) =  ALPHA
            DWORK(13) = -GAMMA
            DWORK(15) =  ALPHA
            DWORK(16) = -DELTA
C
         ELSE IF ( I.EQ.1 ) THEN
C
C           Elements of matrix M1.
C
            DWORK(1) = -ALPHA
            DWORK(4) = -GAMMA
            DWORK(5) =  BETA
            DWORK(7) = -DELTA
            DWORK(9) =  BETA
C
C           Elements of matrix M2.
C
            DWORK(10) =  BETA
            DWORK(11) = -ALPHA
            DWORK(14) = -GAMMA
            DWORK(15) =  BETA
            DWORK(17) = -DELTA
C
         ELSE IF ( I.EQ.2 ) THEN
C
C           Elements of matrix M1.
C
            DWORK(2) = -ALPHA
            DWORK(4) =  GAMMA
            DWORK(5) = -BETA
            DWORK(8) = -DELTA
            DWORK(9) =  GAMMA
C
C           Elements of matrix M2.
C
            DWORK(10) =  GAMMA
            DWORK(12) = -ALPHA
            DWORK(14) =  GAMMA
            DWORK(15) = -BETA
            DWORK(18) = -DELTA
C
         ELSE
C
C           Elements of matrix M1.
C
            DWORK(3) = -ALPHA
            DWORK(4) =  DELTA
            DWORK(6) = -BETA
            DWORK(8) =  DELTA
            DWORK(9) = -GAMMA
C
C           Elements of matrix M2.
C
            DWORK(10) = DELTA
            DWORK(14) = DELTA
            DWORK(18) = DELTA
         END IF
C
C        Compute the roots of the polynomial by solving an eigenproblem
C        using the QZ-Algorithm.
C
         CALL DGGEV( 'N', 'N', 3, DWORK, 3, DWORK(M2POS), 3, EVR, EVI,
     $               EVQ, DWORK(WRKPOS), 1, DWORK(WRKPOS), 1,
     $               DWORK(WRKPOS), LDWORK-18, INFO )
      END IF
C
      RETURN
C *** Last line of MC01XD ***
      END
