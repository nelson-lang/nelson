      SUBROUTINE MB04RW( M, N, PMAX, A, LDA, B, LDB, C, LDC, D, LDD, E,
     $                   LDE, F, LDF, SCALE, IWORK, INFO )
C
C     PURPOSE
C
C     To solve the generalized complex Sylvester equation
C
C              A * R - L * B = scale * C,                            (1)
C              D * R - L * E = scale * F,
C
C     using Level 3 BLAS, where R and L are unknown M-by-N matrices, and
C     (A, D), (B, E) and (C, F) are given matrix pairs of size M-by-M,
C     N-by-N and M-by-N, respectively. A, B, D and E are complex upper
C     triangular (i.e., (A,D) and (B,E) are in generalized Schur form).
C
C     The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1 is an
C     output scaling factor chosen to avoid overflow.
C
C     This routine is intended to be called only by SLICOT Library
C     routine MB04RZ. For efficiency purposes, the computations are
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
C     [3] Kagstrom, B.
C         A Perturbation Analysis of the Generalized Sylvester Equation
C         (AR - LB, DR - LE ) = (C, F).
C         SIAM J. Matrix Anal. Appl., 15(4), pp. 1045-1060, 1994.
C     [4] Kagstrom, B. and Poromaa, P.
C         LAPACK-Style Algorithms and Software for Solving the
C         Generalized Sylvester Equation and Estimating the Separation
C         between Regular Matrix Pairs.
C         ACM Trans. on Math. Software, 22(1), pp. 78–103, 1996.
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
C     routine ZTGSYL.
C
C     REVISIONS
C
C     V. Sima, Nov. 2022, Apr. 2023.
C
C     KEYWORDS
C
C     Diagonalization, unitary transformation, Schur form, Sylvester
C     equation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
      COMPLEX*16         CONE
      PARAMETER          ( CONE = (1.0D+0, 0.0D+0) )
C     ..
C     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, LDC, LDD, LDE, LDF, M, N
      DOUBLE PRECISION   PMAX, SCALE
C     ..
C     .. Array Arguments ..
      INTEGER            IWORK( * )
      COMPLEX*16         A( LDA, * ), B( LDB, * ), C( LDC, * ),
     $                   D( LDD, * ), E( LDE, * ), F( LDF, * )
C     ..
C     .. Local Scalars ..
      INTEGER            I, IE, IS, J, JE, JS, K, MB, NB, P, Q
      DOUBLE PRECISION   SCALOC
C     ..
C     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
C     ..
C     .. External Subroutines ..
      EXTERNAL           MB04RV, ZGEMM, ZLASCL
C     ..
C     .. Executable Statements ..
C
C     For efficiency reasons, this routine does not check the input
C     parameters for errors.
C
      INFO  = 0
      SCALE = ONE
C
C     Quick return if possible.
C
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
C
C     Determine optimal block sizes MB and NB using ZTGSYL.
C
      MB = ILAENV( 2, 'ZTGSYL', 'NoTran', M, N, -1, -1 )
      NB = ILAENV( 5, 'ZTGSYL', 'NoTran', M, N, -1, -1 )
C
      IF( ( MB.LE.1 .AND. NB.LE.1 ) .OR. ( MB.GE.M .AND. NB.GE.N ) )
     $      THEN
C
C        Use unblocked Level 2 BLAS solver.
C
         CALL MB04RV( M, N, PMAX, A, LDA, B, LDB, C, LDC, D, LDD, E,
     $                LDE, F, LDF, SCALE, INFO )
         RETURN
C
      END IF
C
C     Determine the block structure of A.
C
      P = 0
      I = 1
C
   10 CONTINUE
         IF( I.GT.M )
     $      GO TO 20
         P = P + 1
         IWORK( P ) = I
         I = I + MB
         IF( I.GE.M )
     $      GO TO 20
      GO TO 10
C
   20 CONTINUE
      IWORK( P+1 ) = M + 1
      IF( IWORK( P ).EQ.IWORK( P+1 ) )
     $   P = P - 1
C
C     Determine the block structure of B.
C
      Q = P + 1
      J = 1
C
   30 CONTINUE
         IF( J.GT.N )
     $      GO TO 40
         Q = Q + 1
         IWORK( Q ) = J
         J = J + NB
         IF( J.GE.N )
     $      GO TO 40
      GO TO 30
C
   40 CONTINUE
      IWORK( Q+1 ) = N + 1
      IF( IWORK( Q ).EQ.IWORK( Q+1 ) )
     $   Q = Q - 1
C
C     Solve the (I, J)-subsystem
C           A(I, I) * R(I, J) - L(I, J) * B(J, J) = C(I, J),
C           D(I, I) * R(I, J) - L(I, J) * E(J, J) = F(I, J),
C     for I = P, P - 1, ..., 1; J = 1, 2, ..., Q.
C
      DO 60 J = P + 2, Q
         JS = IWORK( J )
         JE = IWORK( J+1 ) - 1
         NB = JE - JS + 1
C
         DO 50 I = P, 1, -1
            IS = IWORK( I )
            IE = IWORK( I+1 ) - 1
            MB = IE - IS + 1
            CALL MB04RV( MB, NB, PMAX, A( IS, IS ), LDA, B( JS, JS ),
     $                   LDB, C( IS, JS ), LDC, D( IS, IS ), LDD,
     $                   E( JS, JS ), LDE, F( IS, JS ), LDF, SCALOC,
     $                   INFO )
            IF( INFO.GT.0 )
     $         RETURN
C
            IF( SCALOC.NE.ONE ) THEN
               CALL ZLASCL( 'G', 0, 0, ONE, SCALOC, M, JS-1, C, LDC, K )
               CALL ZLASCL( 'G', 0, 0, ONE, SCALOC, M, JS-1, F, LDF, K )
C
               CALL ZLASCL( 'G', 0, 0, ONE, SCALOC, IS-1, NB,
     $                      C( 1, JS ), LDC, K )
               CALL ZLASCL( 'G', 0, 0, ONE, SCALOC, IS-1, NB,
     $                      F( 1, JS ), LDF, K )
C
               CALL ZLASCL( 'G', 0, 0, ONE, SCALOC, M-IE, NB,
     $                      C( IE+1, JS ), LDC, K )
               CALL ZLASCL( 'G', 0, 0, ONE, SCALOC, M-IE, NB,
     $                      F( IE+1, JS ), LDF, K )
C
               CALL ZLASCL( 'G', 0, 0, ONE, SCALOC, M, N-JE,
     $                      C( 1, JE+1 ), LDC, K )
               CALL ZLASCL( 'G', 0, 0, ONE, SCALOC, M, N-JE,
     $                      F( 1, JE+1 ), LDF, K )
               SCALE = SCALE*SCALOC
            END IF
C
C           Substitute R(I, J) and L(I, J) into the remaining equation.
C
            IF( I.GT.1 ) THEN
               CALL ZGEMM( 'N', 'N', IS-1, NB, MB, -CONE, A( 1, IS ),
     $                     LDA, C( IS, JS ), LDC, CONE, C( 1, JS ),
     $                     LDC )
               CALL ZGEMM( 'N', 'N', IS-1, NB, MB, -CONE, D( 1, IS ),
     $                     LDD, C( IS, JS ), LDC, CONE, F( 1, JS ),
     $                     LDF )
            END IF
C
            IF( J.LT.Q ) THEN
               CALL ZGEMM( 'N', 'N', MB, N-JE, NB, CONE, F( IS, JS ),
     $                     LDF, B( JS, JE+1 ), LDB, CONE, C( IS, JE+1 ),
     $                     LDC )
               CALL ZGEMM( 'N', 'N', MB, N-JE, NB, CONE, F( IS, JS ),
     $                     LDF, E( JS, JE+1 ), LDE, CONE, F( IS, JE+1 ),
     $                     LDF )
            END IF
C
   50    CONTINUE
C
   60 CONTINUE
C
      RETURN
C
C *** Last line of MB04RW ***
      END
