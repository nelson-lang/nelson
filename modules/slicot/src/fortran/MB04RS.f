      SUBROUTINE MB04RS( M, N, PMAX, A, LDA, B, LDB, C, LDC, D, LDD, E,
     $                   LDE, F, LDF, SCALE, IWORK, INFO )
C
C     PURPOSE
C
C     To solve the generalized real Sylvester equation
C
C              A * R - L * B = scale * C,                            (1)
C              D * R - L * E = scale * F,
C
C     using Level 1 and 2 BLAS, where R and L are unknown real M-by-N
C     matrices, and (A, D), (B, E) and (C, F) are given matrix pairs of
C     size M-by-M, N-by-N and M-by-N, respectively. (A,D) and (B,E) must
C     be in generalized Schur canonical form, i.e., A, B are upper
C     quasi-triangular and D, E are upper triangular.
C
C     The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1 is an
C     output scaling factor chosen to avoid overflow.
C
C     This routine is intended to be called only by SLICOT Library
C     routine MB04RT. For efficiency purposes, the computations are
C     aborted when the absolute value of an element of R or L is greater
C     than a given value PMAX.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     M       (input) INTEGER
C             The order of the matrices A and D, and the row dimension
C             of C, F, R and L.  M >= 0.
C
C     N       (input) INTEGER
C             The order of the matrices B and E, and the column
C             dimension of C, F, R and L.  N >= 0.
C
C     PMAX    (input) DOUBLE PRECISION
C             An upper bound for the absolute value of the elements of
C             the solution (R, L).  PMAX >= 1.0D0.
C
C     A       (input) DOUBLE PRECISION array, dimension (LDA, M)
C             On entry, the leading M-by-M upper quasi-triangular part
C             of this array must contain the matrix A in the generalized
C             real Schur form, as returned by LAPACK routine DGGES.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= max(1, M).
C
C     B       (input) DOUBLE PRECISION array, dimension (LDB, N)
C             On entry, the leading N-by-N upper quasi-triangular part
C             of this array must contain the matrix B in the generalized
C             real Schur form.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= max(1, N).
C
C     C       (input/output) DOUBLE PRECISION array, dimension (LDC, N)
C             On entry, the leading M-by-N part of this array must
C             contain the right-hand-side of the first matrix equation
C             in (1).
C             On exit, if INFO = 0, the leading M-by-N part of this
C             array contains the solution R.
C
C     LDC     INTEGER
C             The leading dimension of the array C.  LDC >= max(1, M).
C
C     D       (input) DOUBLE PRECISION array, dimension (LDD, M)
C             On entry, the leading M-by-M upper triangular part of this
C             array must contain the matrix D in the generalized real
C             Schur form. The diagonal elements are non-negative.
C
C     LDD     INTEGER
C             The leading dimension of the array D.  LDD >= max(1, M).
C
C     E       (input) DOUBLE PRECISION array, dimension (LDE, N)
C             On entry, the leading N-by-N upper triangular part of this
C             array must contain the matrix E in the generalized real
C             Schur form. The diagonal elements are non-negative.
C
C     LDE     INTEGER
C             The leading dimension of the array E.  LDE >= max(1, N).
C
C     F       (input/output) DOUBLE PRECISION array, dimension (LDF, N)
C             On entry, the leading M-by-N part of this array must
C             contain the right-hand-side of the second matrix equation
C             in (1).
C             On exit, if INFO = 0, the leading M-by-N part of this
C             array contains the solution L.
C
C     LDF     INTEGER
C             The leading dimension of the array F.  LDF >= max(1, M).
C
C     SCALE   (output) DOUBLE PRECISION
C             On exit, 0 <= SCALE <= 1. If 0 < SCALE < 1, the solutions
C             R and L (C and F on entry) will hold the solutions to a
C             slightly perturbed system but the input matrices A, B, D
C             and E have not been changed. If SCALE = 0, R and L will
C             hold the solutions to the homogeneous system with C = 0
C             and F = 0. Normally, SCALE = 1.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (M+N+2)
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             = 1:  an element of R or L had the absolute value greater
C                   than the given value PMAX.
C             = 2:  the matrix pairs (A, D) and (B, E) have common or
C                   very close eigenvalues. The matrix Z in section
C                   METHOD is (almost) singular.
C
C     METHOD
C
C     The routine uses an adaptation of the method for solving
C     generalized Sylvester equations [1], which controls the magnitude
C     of the individual elements of the computed solution [2].
C
C     In matrix notation, solving equation (1) corresponds to solve
C     Zx = scale * b, where Z is defined as
C
C            Z = [ kron(In, A)  -kron(B', Im) ]                      (2)
C                [ kron(In, D)  -kron(E', Im) ],
C
C     Ik is the identity matrix of size k and X' is the transpose of X.
C     kron(X, Y) is the Kronecker product between the matrices X and Y.
C     The routine solves a number of systems (2) with n and m at most 2.
C
C     REFERENCES
C
C     [1] Kagstrom, B. and Westin, L.
C         Generalized Schur Methods with Condition Estimators for
C         Solving the Generalized Sylvester Equation.
C         IEEE Trans. Auto. Contr., 34, pp. 745-751, 1989.
C     [2] Kagstrom, B. and Westin, L.
C         GSYLV - Fortran Routines for the Generalized Schur Method with
C         Dif Estimators for Solving the Generalized Sylvester Equation.
C         Report UMINF-132.86, Institute of Information Processing,
C         Univ. of Umea, Sweden, July 1987.
C
C     CONTRIBUTOR
C
C     V. Sima, Nov. 2022.
C     This routine is a simplification and modification of the LAPACK
C     routine DTGSY2. Row scaling is applied to Z and b if it appears
C     that Z is (almost) singular, and a new attempt is made to solve
C     the system.
C
C     REVISIONS
C
C     V. Sima, Dec. 2022, Feb. 2023, Mar. 2023, Apr. 2023.
C
C     KEYWORDS
C
C     Diagonalization, orthogonal transformation, Schur form, Sylvester
C     equation.
C
C     ******************************************************************
C
C     .. Parameters ..
      INTEGER            LDZ
      PARAMETER          ( LDZ = 8 )
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
C     ..
C     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, LDC, LDD, LDE, LDF, M, N
      DOUBLE PRECISION   PMAX, SCALE
C     ..
C     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * ),
     $                   D( LDD, * ), E( LDE, * ), F( LDF, * )
C     ..
C     .. Local Scalars ..
      INTEGER            I, IE, IERR, II, IS, ISP1, IX, J, JE, JEP1, JJ,
     $                   JS, JSP1, K, L, MB, NB, P, Q, ZDIM
      DOUBLE PRECISION   ALPHA, SC, SCALOC
C     ..
C     .. Local Arrays ..
      INTEGER            IPIV( LDZ ), JPIV( LDZ )
      DOUBLE PRECISION   RHS(  LDZ ), Z( LDZ, LDZ ), ZS( LDZ, LDZ )
C     ..
C     .. External Functions ..
      INTEGER            IDAMAX
      EXTERNAL           IDAMAX
C     ..
C     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DGEMM, DGEMV, DGER, DGESC2,
     $                   DGETC2, DLACPY, DLASET, DSCAL
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
C     ..
C     .. Executable Statements ..
C
C     For efficiency reasons, this routine does not check the input
C     parameters for errors.
C
      INFO = 0
C
C     Determine block structure of A.
C
      P = 0
      I = 1
C
   10 CONTINUE
         IF( I.GT.M )
     $      GO TO 20
         P = P + 1
         IWORK( P ) = I
         IF( I.EQ.M )
     $      GO TO 20
         IF( A( I+1, I ).NE.ZERO ) THEN
            I = I + 2
         ELSE
            I = I + 1
         END IF
         GO TO 10
C
   20 CONTINUE
C
      IWORK( P+1 ) = M + 1
C
C     Determine block structure of B.
C
      Q = P + 1
      J = 1
C
   30 CONTINUE
         IF( J.GT.N )
     $      GO TO 40
         Q = Q + 1
         IWORK( Q ) = J
         IF( J.EQ.N )
     $      GO TO 40
         IF( B( J+1, J ).NE.ZERO ) THEN
            J = J + 2
         ELSE
            J = J + 1
         END IF
         GO TO 30
C
   40 CONTINUE
C
      IWORK( Q+1 ) = N + 1
C
C     Solve (I, J) - subsystem
C        A(I, I) * R(I, J) - L(I, J) * B(J, J) = C(I, J),
C        D(I, I) * R(I, J) - L(I, J) * E(J, J) = F(I, J),
C     for I = P, P - 1, ..., 1; J = 1, 2, ..., Q.
C
      SCALE  = ONE
      SCALOC = ONE
C
      DO 160 J = P + 2, Q
         JS   = IWORK( J )
         JSP1 = JS + 1
         JEP1 = IWORK( J+1 )
         JE   = JEP1 - 1
         NB   = JEP1 - JS
C
         DO 150 I = P, 1, -1
            IS   = IWORK( I )
            ISP1 = IS + 1
            IE   = IWORK( I+1 ) - 1
            MB   = IE - IS + 1
            ZDIM = MB*NB*2
C
            IF( ( MB.EQ.1 ) .AND. ( NB.EQ.1 ) ) THEN
C
C              Build a 2-by-2 system Z * x = RHS.
C
               Z( 1, 1 ) =  A( IS, IS )
               Z( 2, 1 ) =  D( IS, IS )
               Z( 1, 2 ) = -B( JS, JS )
               Z( 2, 2 ) = -E( JS, JS )
               CALL DLACPY( 'F', ZDIM, ZDIM, Z, LDZ, ZS, LDZ )
C
C              Set up right hand side(s).
C
               RHS( 1 ) = C( IS, JS )
               RHS( 2 ) = F( IS, JS )
C
C              Solve Z x = RHS.
C
               CALL DGETC2( ZDIM, Z, LDZ, IPIV, JPIV, IERR )
               IF( IERR.GT.0 ) THEN
                  INFO = 2
C
C                 (Almost) singular system, possibly badly scaled.
C
                  DO 50 L = 1, ZDIM
                     IX = IDAMAX( ZDIM, ZS( L, 1 ), LDZ )
                     SC = ABS( ZS( L, IX ) )
                     IF( SC.EQ.ZERO ) THEN
                        INFO = 1
                        RETURN
                     ELSE IF( SC.NE.ONE ) THEN
                        CALL DSCAL( ZDIM, ONE/SC, ZS( L, 1 ), LDZ )
                        RHS( L ) =  RHS( L )/SC
                     END IF
   50             CONTINUE
C
                  CALL DGETC2( ZDIM, ZS, LDZ, IPIV, JPIV, IERR )
                  IF( IERR.EQ.0 )
     $               INFO = 0
                  IF( INFO.GT.0 )
     $               RETURN
                  CALL DLACPY( 'F', ZDIM, ZDIM, ZS, LDZ, Z, LDZ )
               END IF
C
               CALL DGESC2( ZDIM, Z, LDZ, RHS, IPIV, JPIV, SCALOC )
C
C              Check the absolute values of the local solution.
C
               SCALE = SCALE*SCALOC
               IF( MAX( ABS( RHS( 1 ) ),
     $                  ABS( RHS( 2 ) ) )*SCALE.GT.PMAX ) THEN
                   INFO = 1
                   RETURN
               END IF
C
               IF( SCALOC.NE.ONE ) THEN
C
                  DO 60 K = 1, N
                     CALL DSCAL( M, SCALOC, C( 1, K ), 1 )
                     CALL DSCAL( M, SCALOC, F( 1, K ), 1 )
   60             CONTINUE
C
               END IF
C
C              Unpack solution vector(s).
C
               C( IS, JS ) = RHS( 1 )
               F( IS, JS ) = RHS( 2 )
C
C              Substitute R(I, J) and L(I, J) into remaining equation.
C
               IF( I.GT.1 ) THEN
                  ALPHA = -RHS( 1 )
                  CALL DAXPY( IS-1, ALPHA, A( 1, IS ), 1, C( 1, JS ),
     $                        1 )
                  CALL DAXPY( IS-1, ALPHA, D( 1, IS ), 1, F( 1, JS ),
     $                        1 )
               END IF
               IF( J.LT.Q ) THEN
                  CALL DAXPY( N-JE, RHS( 2 ), B( JS, JEP1 ), LDB,
     $                        C( IS, JEP1 ), LDC )
                  CALL DAXPY( N-JE, RHS( 2 ), E( JS, JEP1 ), LDE,
     $                        F( IS, JEP1 ), LDF )
               END IF
C
            ELSE IF( ( MB.EQ.1 ) .AND. ( NB.EQ.2 ) ) THEN
C
C              Build a 4-by-4 system Z * x = RHS.
C
               Z( 1, 1 ) = A( IS, IS )
               Z( 2, 1 ) = ZERO
               Z( 3, 1 ) = D( IS, IS )
               Z( 4, 1 ) = ZERO
C
               Z( 1, 2 ) = ZERO
               Z( 2, 2 ) = A( IS, IS )
               Z( 3, 2 ) = ZERO
               Z( 4, 2 ) = D( IS, IS )
C
               Z( 1, 3 ) = -B( JS, JS )
               Z( 2, 3 ) = -B( JS, JSP1 )
               Z( 3, 3 ) = -E( JS, JS )
               Z( 4, 3 ) = -E( JS, JSP1 )
C
               Z( 1, 4 ) = -B( JSP1, JS )
               Z( 2, 4 ) = -B( JSP1, JSP1 )
               Z( 3, 4 ) =  ZERO
               Z( 4, 4 ) = -E( JSP1, JSP1 )
               CALL DLACPY( 'F', ZDIM, ZDIM, Z, LDZ, ZS, LDZ )
C
C              Set up right hand side(s).
C
               RHS( 1 ) = C( IS, JS )
               RHS( 2 ) = C( IS, JSP1 )
               RHS( 3 ) = F( IS, JS )
               RHS( 4 ) = F( IS, JSP1 )
C
C              Solve Z x = RHS.
C
               CALL DGETC2( ZDIM, Z, LDZ, IPIV, JPIV, IERR )
               IF( IERR.GT.0 ) THEN
                  INFO = 2
C
C                 (Almost) singular system, possibly badly scaled.
C
                  DO 70 L = 1, ZDIM
                     IX = IDAMAX( ZDIM, ZS( L, 1 ), LDZ )
                     SC = ABS( ZS( L, IX ) )
                     IF( SC.EQ.ZERO ) THEN
                        INFO = 1
                        RETURN
                     ELSE IF( SC.NE.ONE ) THEN
                        CALL DSCAL( ZDIM, ONE/SC, ZS( L, 1 ), LDZ )
                        RHS( L ) =  RHS( L )/SC
                     END IF
   70             CONTINUE
C
                  CALL DGETC2( ZDIM, ZS, LDZ, IPIV, JPIV, IERR )
                  IF( IERR.EQ.0 )
     $               INFO = 0
                  IF( INFO.GT.0 )
     $               RETURN
                  CALL DLACPY( 'F', ZDIM, ZDIM, ZS, LDZ, Z, LDZ )
               END IF
C
               CALL DGESC2( ZDIM, Z, LDZ, RHS, IPIV, JPIV, SCALOC )
C
C              Check the absolute values of the local solution.
C
               SCALE = SCALE*SCALOC
               IF( MAX( ABS( RHS( 1 ) ), ABS( RHS( 2 ) ),
     $                  ABS( RHS( 3 ) ),
     $                  ABS( RHS( 4 ) ) )*SCALE.GT.PMAX ) THEN
                   INFO = 1
                   RETURN
               END IF
C
               IF( SCALOC.NE.ONE ) THEN
C
                  DO 80 K = 1, N
                     CALL DSCAL( M, SCALOC, C( 1, K ), 1 )
                     CALL DSCAL( M, SCALOC, F( 1, K ), 1 )
   80             CONTINUE
C
               END IF
C
C              Unpack solution vector(s).
C
               C( IS, JS )   = RHS( 1 )
               C( IS, JSP1 ) = RHS( 2 )
               F( IS, JS )   = RHS( 3 )
               F( IS, JSP1 ) = RHS( 4 )
C
C              Substitute R(I, J) and L(I, J) into remaining equation.
C
               IF( I.GT.1 ) THEN
                  CALL DGER( IS-1, NB, -ONE, A( 1, IS ), 1, RHS( 1 ),
     $                       1, C( 1, JS ), LDC )
                  CALL DGER( IS-1, NB, -ONE, D( 1, IS ), 1, RHS( 1 ),
     $                       1, F( 1, JS ), LDF )
               END IF
               IF( J.LT.Q ) THEN
                  CALL DAXPY( N-JE, RHS( 3 ), B( JS, JEP1 ), LDB,
     $                        C( IS, JEP1 ), LDC )
                  CALL DAXPY( N-JE, RHS( 3 ), E( JS, JEP1 ), LDE,
     $                        F( IS, JEP1 ), LDF )
                  CALL DAXPY( N-JE, RHS( 4 ), B( JSP1, JEP1 ), LDB,
     $                        C( IS, JEP1 ), LDC )
                  CALL DAXPY( N-JE, RHS( 4 ), E( JSP1, JEP1 ), LDE,
     $                        F( IS, JEP1 ), LDF )
               END IF
C
            ELSE IF( ( MB.EQ.2 ) .AND. ( NB.EQ.1 ) ) THEN
C
C              Build a 4-by-4 system Z * x = RHS.
C
               Z( 1, 1 ) = A( IS, IS )
               Z( 2, 1 ) = A( ISP1, IS )
               Z( 3, 1 ) = D( IS, IS )
               Z( 4, 1 ) = ZERO
C
               Z( 1, 2 ) = A( IS, ISP1 )
               Z( 2, 2 ) = A( ISP1, ISP1 )
               Z( 3, 2 ) = D( IS, ISP1 )
               Z( 4, 2 ) = D( ISP1, ISP1 )
C
               Z( 1, 3 ) = -B( JS, JS )
               Z( 2, 3 ) =  ZERO
               Z( 3, 3 ) = -E( JS, JS )
               Z( 4, 3 ) =  ZERO
C
               Z( 1, 4 ) =  ZERO
               Z( 2, 4 ) = -B( JS, JS )
               Z( 3, 4 ) =  ZERO
               Z( 4, 4 ) = -E( JS, JS )
               CALL DLACPY( 'F', ZDIM, ZDIM, Z, LDZ, ZS, LDZ )
C
C              Set up right hand side(s).
C
               RHS( 1 ) = C( IS, JS )
               RHS( 2 ) = C( ISP1, JS )
               RHS( 3 ) = F( IS, JS )
               RHS( 4 ) = F( ISP1, JS )
C
C              Solve Z * x = RHS.
C
               CALL DGETC2( ZDIM, Z, LDZ, IPIV, JPIV, IERR )
               IF( IERR.GT.0 ) THEN
                  INFO = 2
C
C                 (Almost) singular system, possibly badly scaled.
C
                  DO 90 L = 1, ZDIM
                     IX = IDAMAX( ZDIM, ZS( L, 1 ), LDZ )
                     SC = ABS( ZS( L, IX ) )
                     IF( SC.EQ.ZERO ) THEN
                        INFO = 1
                        RETURN
                     ELSE IF( SC.NE.ONE ) THEN
                        CALL DSCAL( ZDIM, ONE/SC, ZS( L, 1 ), LDZ )
                        RHS( L ) =  RHS( L )/SC
                     END IF
   90             CONTINUE
C
                  CALL DGETC2( ZDIM, ZS, LDZ, IPIV, JPIV, IERR )
                  IF( IERR.EQ.0 )
     $               INFO = 0
                  IF( INFO.GT.0 )
     $               RETURN
                  CALL DLACPY( 'F', ZDIM, ZDIM, ZS, LDZ, Z, LDZ )
               END IF
C
               CALL DGESC2( ZDIM, Z, LDZ, RHS, IPIV, JPIV, SCALOC )
C
C              Check the absolute values of the local solution.
C
               SCALE = SCALE*SCALOC
               IF( MAX( ABS( RHS( 1 ) ), ABS( RHS( 2 ) ),
     $                  ABS( RHS( 3 ) ),
     $                  ABS( RHS( 4 ) ) )*SCALE.GT.PMAX ) THEN
                   INFO = 1
                   RETURN
               END IF
C
               IF( SCALOC.NE.ONE ) THEN
C
                  DO 100 K = 1, N
                     CALL DSCAL( M, SCALOC, C( 1, K ), 1 )
                     CALL DSCAL( M, SCALOC, F( 1, K ), 1 )
  100             CONTINUE
C
               END IF
C
C              Unpack solution vector(s).
C
               C( IS, JS )   = RHS( 1 )
               C( ISP1, JS ) = RHS( 2 )
               F( IS, JS )   = RHS( 3 )
               F( ISP1, JS ) = RHS( 4 )
C
C              Substitute R(I, J) and L(I, J) into remaining equation.
C
               IF( I.GT.1 ) THEN
                  CALL DGEMV( 'N', IS-1, MB, -ONE, A( 1, IS ), LDA,
     $                        RHS( 1 ), 1, ONE, C( 1, JS ), 1 )
                  CALL DGEMV( 'N', IS-1, MB, -ONE, D( 1, IS ), LDD,
     $                        RHS( 1 ), 1, ONE, F( 1, JS ), 1 )
               END IF
               IF( J.LT.Q ) THEN
                  CALL DGER( MB, N-JE, ONE, RHS( 3 ), 1, B( JS, JEP1 ),
     $                       LDB, C( IS, JEP1 ), LDC )
                  CALL DGER( MB, N-JE, ONE, RHS( 3 ), 1, E( JS, JEP1 ),
     $                       LDB, F( IS, JEP1 ), LDC )
               END IF
C
            ELSE IF( ( MB.EQ.2 ) .AND. ( NB.EQ.2 ) ) THEN
C
C              Build an 8-by-8 system Z * x = RHS.
C
               CALL DLASET( 'F', LDZ, LDZ, ZERO, ZERO, Z, LDZ )
C
               Z( 1, 1 ) = A( IS, IS )
               Z( 2, 1 ) = A( ISP1, IS )
               Z( 5, 1 ) = D( IS, IS )
C
               Z( 1, 2 ) = A( IS, ISP1 )
               Z( 2, 2 ) = A( ISP1, ISP1 )
               Z( 5, 2 ) = D( IS, ISP1 )
               Z( 6, 2 ) = D( ISP1, ISP1 )
C
               Z( 3, 3 ) = A( IS, IS )
               Z( 4, 3 ) = A( ISP1, IS )
               Z( 7, 3 ) = D( IS, IS )
C
               Z( 3, 4 ) = A( IS, ISP1 )
               Z( 4, 4 ) = A( ISP1, ISP1 )
               Z( 7, 4 ) = D( IS, ISP1 )
               Z( 8, 4 ) = D( ISP1, ISP1 )
C
               Z( 1, 5 ) = -B( JS, JS )
               Z( 3, 5 ) = -B( JS, JSP1 )
               Z( 5, 5 ) = -E( JS, JS )
               Z( 7, 5 ) = -E( JS, JSP1 )
C
               Z( 2, 6 ) = -B( JS, JS )
               Z( 4, 6 ) = -B( JS, JSP1 )
               Z( 6, 6 ) = -E( JS, JS )
               Z( 8, 6 ) = -E( JS, JSP1 )
C
               Z( 1, 7 ) = -B( JSP1, JS )
               Z( 3, 7 ) = -B( JSP1, JSP1 )
               Z( 7, 7 ) = -E( JSP1, JSP1 )
C
               Z( 2, 8 ) = -B( JSP1, JS )
               Z( 4, 8 ) = -B( JSP1, JSP1 )
               Z( 8, 8 ) = -E( JSP1, JSP1 )
               CALL DLACPY( 'F', ZDIM, ZDIM, Z, LDZ, ZS, LDZ )
C
C              Set up right hand side(s).
C
               K  = 1
               II = MB*NB + 1
C
               DO 110 JJ = JS, JS + NB - 1
                  CALL DCOPY( MB, C( IS, JJ ), 1, RHS( K ), 1 )
                  CALL DCOPY( MB, F( IS, JJ ), 1, RHS( II ), 1 )
                  K  = K  + MB
                  II = II + MB
  110          CONTINUE
C
C              Solve Z * x = RHS.
C
               CALL DGETC2( ZDIM, Z, LDZ, IPIV, JPIV, IERR )
               IF( IERR.GT.0 ) THEN
                  INFO = 2
C
C                 (Almost) singular system, possibly badly scaled.
C
                  DO 120 L = 1, ZDIM
                     IX = IDAMAX( ZDIM, ZS( L, 1 ), LDZ )
                     SC = ABS( ZS( L, IX ) )
                     IF( SC.EQ.ZERO ) THEN
                        INFO = 1
                        RETURN
                     ELSE IF( SC.NE.ONE ) THEN
                        CALL DSCAL( ZDIM, ONE/SC, ZS( L, 1 ), LDZ )
                        RHS( L ) =  RHS( L )/SC
                     END IF
  120             CONTINUE
C
                  CALL DGETC2( ZDIM, ZS, LDZ, IPIV, JPIV, IERR )
                  IF( IERR.EQ.0 )
     $               INFO = 0
                  IF( INFO.GT.0 )
     $               RETURN
                  CALL DLACPY( 'F', ZDIM, ZDIM, ZS, LDZ, Z, LDZ )
               END IF
C
               CALL DGESC2( ZDIM, Z, LDZ, RHS, IPIV, JPIV, SCALOC )
C
C              Check the absolute values of the local solution.
C
               SCALE = SCALE*SCALOC
               IF( MAX( ABS( RHS( 1 ) ), ABS( RHS( 2 ) ),
     $                  ABS( RHS( 3 ) ), ABS( RHS( 4 ) ),
     $                  ABS( RHS( 5 ) ), ABS( RHS( 6 ) ),
     $                  ABS( RHS( 7 ) ),
     $                  ABS( RHS( 8 ) ) )*SCALE.GT.PMAX ) THEN
                   INFO = 1
                   RETURN
               END IF
C
               IF( SCALOC.NE.ONE ) THEN
C
                  DO 130 K = 1, N
                     CALL DSCAL( M, SCALOC, C( 1, K ), 1 )
                     CALL DSCAL( M, SCALOC, F( 1, K ), 1 )
  130             CONTINUE
C
               END IF
C
C              Unpack solution vector(s).
C
               K = 1
               II = MB*NB + 1
C
               DO 140 JJ = JS, JS + NB - 1
                  CALL DCOPY( MB, RHS( K ),  1, C( IS, JJ ), 1 )
                  CALL DCOPY( MB, RHS( II ), 1, F( IS, JJ ), 1 )
                  K  = K  + MB
                  II = II + MB
  140          CONTINUE
C
C              Substitute R(I, J) and L(I, J) into remaining equation.
C
               IF( I.GT.1 ) THEN
                  CALL DGEMM( 'N', 'N', IS-1, NB, MB, -ONE, A( 1, IS ),
     $                        LDA, RHS( 1 ), MB, ONE, C( 1, JS ), LDC )
                  CALL DGEMM( 'N', 'N', IS-1, NB, MB, -ONE, D( 1, IS ),
     $                        LDD, RHS( 1 ), MB, ONE, F( 1, JS ), LDF )
               END IF
               IF( J.LT.Q ) THEN
                  K = MB*NB + 1
                  CALL DGEMM( 'N', 'N', MB, N-JE, NB, ONE, RHS( K ), MB,
     $                        B( JS, JEP1 ), LDB, ONE, C( IS, JEP1 ),
     $                        LDC )
                  CALL DGEMM( 'N', 'N', MB, N-JE, NB, ONE, RHS( K ), MB,
     $                        E( JS, JEP1 ), LDE, ONE, F( IS, JEP1 ),
     $                        LDF )
               END IF
C
            END IF
C
  150    CONTINUE
C
  160 CONTINUE
C
      RETURN
C
C *** Last line of MB04RS ***
      END
