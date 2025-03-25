      SUBROUTINE MB04RV( M, N, PMAX, A, LDA, B, LDB, C, LDC, D, LDD, E,
     $                   LDE, F, LDF, SCALE, INFO )
C
C     PURPOSE
C
C     To solve the generalized complex Sylvester equation
C
C              A * R - L * B = scale * C,                            (1)
C              D * R - L * E = scale * F,
C
C     using Level 1 and 2 BLAS, where R and L are unknown M-by-N
C     matrices, and (A, D), (B, E) and (C, F) are given matrix pairs of
C     size M-by-M, N-by-N and M-by-N, respectively. A, B, D and E are
C     complex upper triangular (i.e., (A,D) and (B,E) are in generalized
C     Schur form).
C
C     The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1 is an
C     output scaling factor chosen to avoid overflow.
C
C     This routine is intended to be called only by SLICOT Library
C     routine MB04RW. For efficiency purposes, the computations are
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
C             An upper bound for the "absolute value" of the elements of
C             the solution (R, L). (See FURTHER COMMENTS.)
C             PMAX >= 1.0D0.
C
C     A       (input) COMPLEX*16 array, dimension (LDA, M)
C             On entry, the leading M-by-M upper triangular part of this
C             array must contain the matrix A in the generalized complex
C             Schur form, as returned by LAPACK routine ZGGES.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= max(1, M).
C
C     B       (input) COMPLEX*16 array, dimension (LDB, N)
C             On entry, the leading N-by-N upper triangular part of this
C             array must contain the matrix B in the generalized complex
C             Schur form.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= max(1, N).
C
C     C       (input/output) COMPLEX*16 array, dimension (LDC, N)
C             On entry, the leading M-by-N part of this array must
C             contain the right-hand-side of the first matrix equation
C             in (1).
C             On exit, if INFO = 0, the leading M-by-N part of this
C             array contains the solution R.
C
C     LDC     INTEGER
C             The leading dimension of the array C.  LDC >= max(1, M).
C
C     D       (input) COMPLEX*16 array, dimension (LDD, M)
C             On entry, the leading M-by-M upper triangular part of this
C             array must contain the matrix D in the generalized complex
C             Schur form. The diagonal elements are non-negative real.
C
C     LDD     INTEGER
C             The leading dimension of the array D.  LDD >= max(1, M).
C
C     E       (input) COMPLEX*16 array, dimension (LDE, N)
C             On entry, the leading N-by-N upper triangular part of this
C             array must contain the matrix E in the generalized complex
C             Schur form. The diagonal elements are non-negative real.
C
C     LDE     INTEGER
C             The leading dimension of the array E.  LDE >= max(1, N).
C
C     F       (input/output) COMPLEX*16 array, dimension (LDF, N)
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
C     FURTHER COMMENTS
C
C     For efficiency reasons, the "absolute value" of a complex number x
C     is computed as |real(x)| + |imag(x)|.
C
C     CONTRIBUTOR
C
C     V. Sima, Oct. 2022.
C     This routine is a simplification and modification of the LAPACK
C     routine ZTGSY2. Row scaling is applied to Z and b if it appears
C     that Z is (almost) singular, and a new attempt is made to solve
C     the system.
C
C     REVISIONS
C
C     V. Sima, Nov. 2022, Dec. 2022, Feb. 2023, Apr. 2023.
C
C     KEYWORDS
C
C     Diagonalization, unitary transformation, Schur form, Sylvester
C     equation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      INTEGER            LDZ
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, LDZ = 2 )
C
C     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, LDC, LDD, LDE, LDF, M, N
      DOUBLE PRECISION   PMAX, SCALE
C     ..
C     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), B( LDB, * ), C( LDC, * ),
     $                   D( LDD, * ), E( LDE, * ), F( LDF, * )
C     ..
C     .. Local Scalars ..
      INTEGER            I, IERR, IX, J, K, L
      DOUBLE PRECISION   ARHS1, ARHS2, SC, SCALOC
      COMPLEX*16         ALPHA
C     ..
C     .. Local Arrays ..
      INTEGER            IPIV( LDZ ), JPIV( LDZ )
      COMPLEX*16         RHS(  LDZ ), Z( LDZ, LDZ ), ZS( LDZ, LDZ )
C     ..
C     .. External Functions ..
      INTEGER            IZAMAX
      EXTERNAL           IZAMAX
C     ..
C     .. External Subroutines ..
      EXTERNAL           ZAXPY, ZDSCAL, ZGESC2, ZGETC2, ZLACPY
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCMPLX, DIMAG, MAX
C     ..
C     .. Executable Statements ..
C
C     For efficiency reasons, this routine does not check the input
C     parameters for errors.
C
      INFO = 0
      IERR = 0
C
C     Solve (I, J) - system
C        A(I, I) * R(I, J) - L(I, J) * B(J, J) = C(I, J),
C        D(I, I) * R(I, J) - L(I, J) * E(J, J) = F(I, J),
C     for I = M, M - 1, ..., 1; J = 1, 2, ..., N.
C
      SCALE  = ONE
      SCALOC = ONE
C
      DO 40 J = 1, N
C
         DO 30 I = M, 1, -1
C
C           Build the 2-by-2 system of algebraic equations.
C
            Z( 1, 1 ) =  A( I, I )
            Z( 2, 1 ) =  D( I, I )
            Z( 1, 2 ) = -B( J, J )
            Z( 2, 2 ) = -E( J, J )
            CALL ZLACPY( 'F', LDZ, LDZ, Z, LDZ, ZS, LDZ )
C
C           Set up the right hand side(s).
C
            RHS( 1 ) = C( I, J )
            RHS( 2 ) = F( I, J )
C
C           Solve Z * x = RHS.
C
            CALL ZGETC2( LDZ, Z, LDZ, IPIV, JPIV, IERR )
            IF( IERR.GT.0 ) THEN
               INFO = 2
C
C              (Almost) singular system, possibly badly scaled.
C
               DO 10 L = 1, LDZ
                  IX = IZAMAX( LDZ, ZS( L, 1 ), LDZ )
                  SC = ABS(  DBLE( ZS( L, IX ) ) ) +
     $                 ABS( DIMAG( ZS( L, IX ) ) )
                  IF( SC.EQ.ZERO ) THEN
                     INFO = 1
                     RETURN
                  ELSE IF( SC.NE.ONE ) THEN
                     CALL ZDSCAL( LDZ, ONE/SC, ZS( L, 1 ), LDZ )
                     RHS( L ) =  RHS( L )/DCMPLX( SC )
                  END IF
   10          CONTINUE
C
               CALL ZGETC2( LDZ, ZS, LDZ, IPIV, JPIV, IERR )
               IF( IERR.EQ.0 )
     $            INFO = 0
               IF( INFO.GT.0 )
     $            RETURN
               CALL ZLACPY( 'F', LDZ, LDZ, ZS, LDZ, Z, LDZ )
            END IF
C
            CALL ZGESC2( LDZ, Z, LDZ, RHS, IPIV, JPIV, SCALOC )
C
C           Check the "absolute values" of the local solution.
C
            ARHS1 = ABS( DBLE( RHS( 1 ) ) ) + ABS( DIMAG( RHS( 1 ) ) )
            ARHS2 = ABS( DBLE( RHS( 2 ) ) ) + ABS( DIMAG( RHS( 2 ) ) )
            SCALE = SCALE*SCALOC
            IF( MAX( ARHS1, ARHS2 )*SCALE.GT.PMAX ) THEN
                INFO = 1
                RETURN
            END IF
C
            IF( SCALOC.NE.ONE ) THEN
C
               DO 20 K = 1, N
                  CALL ZDSCAL( M, SCALOC, C( 1, K ), 1 )
                  CALL ZDSCAL( M, SCALOC, F( 1, K ), 1 )
   20          CONTINUE
C
            END IF
C
C           Unpack the solution vector(s).
C
            C( I, J ) = RHS( 1 )
            F( I, J ) = RHS( 2 )
C
C           Substitute R(I, J) and L(I, J) into the remaining equation.
C
            IF( I.GT.1 ) THEN
               ALPHA = -RHS( 1 )
               CALL ZAXPY( I-1, ALPHA, A( 1, I ), 1, C( 1, J ), 1 )
               CALL ZAXPY( I-1, ALPHA, D( 1, I ), 1, F( 1, J ), 1 )
            END IF
            IF( J.LT.N ) THEN
               CALL ZAXPY( N-J, RHS( 2 ), B( J, J+1 ), LDB, C( I, J+1 ),
     $                     LDC )
               CALL ZAXPY( N-J, RHS( 2 ), E( J, J+1 ), LDE, F( I, J+1 ),
     $                     LDF )
            END IF
C
   30    CONTINUE
C
   40 CONTINUE
C
      RETURN
C *** Last line of MB04RV ***
      END
